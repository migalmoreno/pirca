(ns pirca.ssg.builders.github
  "Mirrors project repos to GitHub via the gh CLI."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [pirca.ssg.process :as process]))

(defn setup-git-auth!
  "Configure git to authenticate to GitHub via the gh CLI (GH_TOKEN)."
  []
  (process/shell "gh" "auth" "setup-git"))

(defn update-profile!
  "Set the authenticated GitHub account's display name and external link."
  [fullname domain]
  (process/shell "gh"
                 "api"
                 "--method"
                 "PATCH"
                 "user"
                 "-f"
                 (str "name=" fullname)
                 "-f"
                 (str "blog=https://" domain)))

(defn- repo-exists?
  [owner repo-name]
  (-> (process/capture "gh" "repo" "view" (str owner "/" repo-name))
      :exit
      zero?))

(defn- create-repo!
  [owner repo-name]
  (process/shell "gh" "repo" "create" (str owner "/" repo-name) "--public"))

(defn- sync-settings!
  [owner repo-name synopsis actions?]
  (process/shell "gh"
                 "repo"
                 "edit"
                 (str owner "/" repo-name)
                 "--description"
                 synopsis
                 "--enable-issues=false"
                 "--enable-wiki=false"
                 "--enable-projects=false")
  (process/shell "gh"
                 "api"
                 "--method"
                 "PUT"
                 (str "repos/" owner "/" repo-name "/actions/permissions")
                 "-F"
                 (str "enabled=" (boolean actions?)))
  (process/shell "gh"
                 "api"
                 "--method"
                 "PATCH"
                 (str "repos/" owner "/" repo-name)
                 "-F"
                 "has_pull_requests=false"))

(defn- current-topics
  [owner repo-name]
  (->> (process/capture "gh" "repo" "view" (str owner "/" repo-name)
                        "--json" "repositoryTopics"
                        "--jq" ".repositoryTopics[].name")
       :out
       str/split-lines
       (remove str/blank?)
       set))

(defn- sync-topics!
  [owner repo-name tags]
  (let [current   (current-topics owner repo-name)
        desired   (set tags)
        to-add    (set/difference desired current)
        to-remove (set/difference current desired)]
    (when (seq to-add)
      (process/shell "gh" "repo" "edit" (str owner "/" repo-name)
                     "--add-topic" (str/join "," to-add)))
    (when (seq to-remove)
      (process/shell "gh" "repo" "edit" (str owner "/" repo-name)
                     "--remove-topic" (str/join "," to-remove)))))

(defn ensure-repo!
  "Create owner/repo-name on GitHub if it doesn't exist, then bring its
  description, issues, wiki, projects, and Actions settings in sync.
  actions? controls whether GitHub Actions is enabled for the repo. When
  github-topics is non-nil, also reconciles the repo's GitHub topics with it
  (added/removed via a diff against the repo's current topics), since gh
  only exposes additive/subtractive topic edits, not a replace-all."
  [owner repo-name synopsis actions? github-topics]
  (when-not (repo-exists? owner repo-name)
    (create-repo! owner repo-name))
  (sync-settings! owner repo-name synopsis actions?)
  (when (some? github-topics)
    (sync-topics! owner repo-name github-topics)))

(defn- list-repos
  [owner]
  (->> (process/capture "gh" "repo" "list" owner "--limit" "1000"
                        "--json" "name" "--jq" ".[].name")
       :out
       str/split-lines
       (remove str/blank?)
       set))

(defn prune-repos!
  "Delete every repo owned by owner that isn't in KEEP (the set of repo-names
  declared in config.edn), so the GitHub account stays a pure mirror of the
  site. Requires a token with the delete_repo scope."
  [owner keep]
  (doseq [repo-name (set/difference (list-repos owner) (set keep))]
    (println "Deleting stale GitHub repo" repo-name)
    (process/shell "gh" "repo" "delete" (str owner "/" repo-name) "--yes")))

(defn push-branch!
  "Force-push git-dir's current branch to owner/repo-name on GitHub under
  that same branch name, rather than mirroring every local ref (e.g. the
  internal `public` bookkeeping branch used by the publish builder)."
  [git-dir owner repo-name]
  (let [branch (-> (process/capture "git" "-C" git-dir "symbolic-ref" "--short"
                                    "HEAD")
                   :out
                   str/trim)]
    (process/shell "git"
                   "-C"
                   git-dir
                   "push"
                   "--force"
                   (str "https://github.com/" owner "/" repo-name ".git")
                   (str "refs/heads/" branch ":refs/heads/" branch))))
