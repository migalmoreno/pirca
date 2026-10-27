(ns pirca.ssg.builders.projects
  "Project preparation: mirror configured repos, optionally publish a redated
  branch, extract each repo's source tree, and generate org post files from
  project metadata so the org reader can process them."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [integrant.core :as ig]
   [pirca.ssg.builders.github :as github]
   [pirca.ssg.builders.publish :as publish]
   [pirca.ssg.builders.repo :as repo]
   [pirca.ssg.config :as config]
   [pirca.ssg.post :as post]
   [pirca.ssg.process :as process]))

(defn extract-archive!
  [git-dir out-dir]
  (let [tar-file (str (fs/temp-dir) "/pirca-" (fs/file-name out-dir) ".tar")]
    (try
      (process/shell "git"
                     "-C"
                     git-dir
                     "archive"
                     "--format=tar"
                     "--output"
                     tar-file
                     "HEAD")
      (when (fs/exists? out-dir) (fs/delete-tree out-dir))
      (fs/create-dirs out-dir)
      (process/shell "tar" "-xf" tar-file "-C" (str out-dir))
      (finally (fs/delete-if-exists tar-file)))))

(defn find-readme
  [src-dir]
  (some #(let [f (str src-dir "/" %)]
           (when (fs/exists? f) (fs/slurp f)))
        ["README.org" "README"]))

(defn write-org-file!
  [posts-dir src-dir slug {:keys [repo-name synopsis tags license]}]
  (let [lines    (cond-> [(str "#+SOURCE-DIR: " (fs/absolutize src-dir))]
                   repo-name (conj (str "#+REPO-NAME: " repo-name))
                   synopsis  (conj (str "#+SYNOPSIS: " synopsis))
                   tags      (conj (str "#+TAGS: " (str/join " " tags)))
                   license   (conj (str "#+LICENSE: " license)))
        readme   (find-readme src-dir)
        content  (str (or readme "") "\n" (str/join "\n" lines) "\n")
        org-file (str posts-dir "/projects/" slug ".org")]
    (when (or (not (fs/exists? org-file))
              (not= content (fs/slurp org-file)))
      (fs/spit org-file content))))

(defn project-by-slug
  "Find a project in the projects vector by its derived slug."
  [projects slug]
  (some #(when (= (post/->slug (:repo-name %)) slug) %) projects))

(defn- prune-stale-posts!
  "Remove org posts under POSTS-DIR/projects whose slug is not in KEEP (the
  set of configured project slugs)."
  [posts-dir keep]
  (let [dir (str posts-dir "/projects")]
    (when (fs/exists? dir)
      (doseq [f    (fs/glob dir "*.org")
              :let [slug (str (fs/strip-ext (fs/file-name f)))]
              :when (not (contains? keep slug))]
        (println "Removing stale project post" (str f))
        (fs/delete-if-exists f)))))

(defn- head-sha
  "Return the resolved HEAD commit sha of the bare repo at GIT-DIR."
  [git-dir]
  (str/trim (:out (process/capture "git" "-C" git-dir "rev-parse" "HEAD"))))

(defn prepare-repos!
  "Mirror and prepare the project repos.

  OPTS (the :repo builder options):
    :forge-base-url - forge base URL
    :forgejo-token  - token; when present, publish a redated `public` branch
    :username       - forge username for authentication
    :cache-dir      - bare mirror directory
    :prefix         - project URL prefix used for served repos (unused here)

  Mirrors each repo, optionally publishes it, extracts its source tree, writes
  its org post to POSTS-DIR/projects, and removes posts for projects no longer
  listed."
  [opts posts-dir projects]
  (let [{:keys [forge-base-url forgejo-token username cache-dir]} opts
        publish? (boolean forgejo-token)]
    (fs/create-dirs (str posts-dir "/projects"))
    (when publish?
      (repo/setup-git-auth! forge-base-url username forgejo-token))
    (doseq [{:keys [repo-name] :as project} projects]
      (let [slug     (post/->slug repo-name)
            repo-url (str forge-base-url repo-name)
            git-dir  (str cache-dir "/" slug ".git")
            src-dir  (str cache-dir "/" slug "-src")
            marker   (str cache-dir "/" slug ".sha")]
        (try
          (repo/ensure-mirror! repo-url git-dir)
          (when publish?
            (publish/sync-public! git-dir repo-url))
          (let [sha (head-sha git-dir)
                old (when (fs/exists? marker) (str/trim (fs/slurp marker)))]
            (if (and (= sha old) (fs/exists? src-dir))
              nil
              (do
                (println "Preparing project" slug "...")
                (extract-archive! git-dir src-dir)
                (write-org-file! posts-dir src-dir slug project)
                (fs/spit marker sha))))
          (catch #?(:clj Exception :cljs js/Error) e
            (println "Warning: failed to prepare" slug
                     "-"                          (.getMessage e))))))
    (prune-stale-posts! posts-dir
                        (into #{} (map (comp str post/->slug :repo-name))
                              projects))))

(defn prepare-github!
  "Mirror the project repos to GitHub and sync the account profile/repo list.

  OPTS (the :github builder options):
    :owner - GitHub owner
    :token - GitHub token; when present, the mirroring runs
    :fullname, :domain - used to update the account profile
    :cache-dir - bare mirror directory"
  [opts posts-dir projects]
  (let [{:keys [owner token fullname domain cache-dir]} opts]
    (when token
      (github/setup-git-auth!)
      (github/update-profile! fullname domain)
      (github/prune-repos! owner (map :repo-name projects))
      (doseq [{:keys [repo-name synopsis github-actions? github-topics]}
              projects]
        (let [slug    (post/->slug repo-name)
              git-dir (str cache-dir "/" slug ".git")]
          (try
            (github/ensure-repo! owner
                                 repo-name
                                 synopsis
                                 github-actions?
                                 github-topics)
            (github/push-branch! git-dir owner repo-name)
            (catch #?(:clj Exception :cljs js/Error) e
              (println "Warning: failed to mirror" slug
                       "-"                          (.getMessage e)))))))))

(defmethod ig/init-key ::repos
  [_ {:keys [config]}]
  (when (config/enabled? config :builders :repo)
    (prepare-repos! (config/opts config :builders :repo)
                    (get config :posts-dir "posts")
                    (get config :projects [])))
  nil)

(defmethod ig/init-key ::github
  [_ {:keys [config]}]
  (let [opts (config/opts config :builders :github)]
    (when (and (config/enabled? config :builders :github) (:token opts))
      (prepare-github!
       (merge opts
              {:cache-dir (or (:cache-dir (config/opts config :builders :repo))
                              ".git-cache")
               :fullname  (get-in config [:site :fullname])
               :domain    (get-in config [:site :domain])})
       (get config :posts-dir "posts")
       (get config :projects []))))
  nil)
