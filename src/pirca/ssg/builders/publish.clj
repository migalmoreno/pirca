(ns pirca.ssg.builders.publish
  "Publish a redated copy of a repo's branch for public mirroring.

  Rewrites commits made since the last publish onto a dedicated `public`
  branch (same tree/message/author, dates reset to now) and force-pushes it
  back to the origin, leaving the branch you develop on untouched."
  (:require
   [clojure.string :as str]
   [pirca.ssg.process :as process]))

(def ^:private public-branch "public")
(def ^:private source-tag "public-source")

(defn- git-out
  [git-dir & args]
  (let [{:keys [exit out]} (apply process/capture "git" "-C" git-dir args)]
    (when (zero? exit) (str/trim out))))

(defn- ancestor?
  [git-dir ancestor descendant]
  (-> (process/capture "git" "-C" git-dir "merge-base" "--is-ancestor"
                       ancestor descendant)
      :exit
      zero?))

(defn- redate-commit!
  "Build a copy of sha as a new commit with the same tree, message, and
  author/committer identity, but with author/committer date set to now."
  [git-dir sha parent now]
  (let [[tree author-name author-email committer-name committer-email]
        (str/split (git-out git-dir
                            "log"                                     "-1"
                            "--format=%T%x1f%an%x1f%ae%x1f%cn%x1f%ce" sha)
                   #"\x1f")
        message (git-out git-dir "log" "-1" "--format=%B" sha)
        args    (cond-> ["git" "-C" git-dir "commit-tree" tree]
                  parent (into ["-p" parent])
                  true   (into ["-m" message]))]
    (str/trim
     (:out (apply process/capture
                  {:extra-env {"GIT_AUTHOR_NAME"     author-name
                               "GIT_AUTHOR_EMAIL"    author-email
                               "GIT_COMMITTER_NAME"  committer-name
                               "GIT_COMMITTER_EMAIL" committer-email
                               "GIT_AUTHOR_DATE"     now
                               "GIT_COMMITTER_DATE"  now}}
                  args)))))

(defn- reset-public!
  "(Re)establish `public` and public-source as an exact, unrewritten copy
  of branch's current state; used both for a repo's first-ever sync and
  to recover after branch has been rebased past the previously recorded
  source commit."
  [git-dir repo-url branch branch-head]
  (process/shell "git"
                 "-C"
                 git-dir
                 "update-ref"
                 (str "refs/heads/" public-branch)
                 branch-head)
  (process/shell "git" "-C" git-dir "tag" "-f" source-tag branch-head)
  (process/shell "git"
                 "-C"
                 git-dir
                 "push"
                 "--force"
                 repo-url
                 (str "refs/heads/" public-branch ":refs/heads/" public-branch)
                 (str "refs/tags/" source-tag ":refs/tags/" source-tag))
  (process/shell "git"
                 "-C"
                 git-dir
                 "update-ref"
                 (str "refs/heads/" branch)
                 branch-head))

(defn sync-public!
  [git-dir repo-url]
  (let [branch      (git-out git-dir "symbolic-ref" "--short" "HEAD")
        branch-head (git-out git-dir "rev-parse" branch)
        public-tip  (git-out git-dir
                             "rev-parse" "--verify"
                             "-q"
                             (str "refs/heads/" public-branch))
        source-tip  (git-out git-dir
                             "rev-parse" "--verify"
                             "-q"
                             (str "refs/tags/" source-tag))]
    (cond
      (or (nil? public-tip) (nil? source-tip))
      (reset-public! git-dir repo-url branch branch-head)

      (not (ancestor? git-dir source-tip branch))
      (do (println "public-source no longer reachable from"
                   branch
                   "- branch was likely rebased, resetting `public` baseline")
          (reset-public! git-dir repo-url branch branch-head))

      :else
      (let [new-shas (remove str/blank?
                             (str/split-lines
                              (git-out git-dir
                                       "rev-list"
                                       "--reverse"
                                       (str source-tip ".." branch))))]
        (if (empty? new-shas)
          (process/shell "git"
                         "-C"
                         git-dir
                         "update-ref"
                         (str "refs/heads/" branch)
                         public-tip)
          (let [now (str (java.time.OffsetDateTime/now))
                tip (reduce (fn [parent sha]
                              (redate-commit! git-dir sha parent now))
                            public-tip
                            new-shas)]
            (process/shell "git"
                           "-C"
                           git-dir
                           "update-ref"
                           (str "refs/heads/" public-branch)
                           tip)
            (process/shell "git" "-C" git-dir "tag" "-f" source-tag branch-head)
            (process/shell "git"
                           "-C"
                           git-dir
                           "push"
                           "--force"
                           repo-url
                           (str "refs/heads/"  public-branch
                                ":refs/heads/" public-branch)
                           (str "refs/tags/" source-tag ":refs/tags/" source-tag))
            (process/shell "git"
                           "-C"
                           git-dir
                           "update-ref"
                           (str "refs/heads/" branch)
                           tip)))))))
