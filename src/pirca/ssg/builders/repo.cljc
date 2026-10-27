(ns pirca.ssg.builders.repo
  "Git repo HTTP serving: mirrors repos and exposes them via dumb HTTP protocol."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [integrant.core :as ig]
   [pirca.ssg.config :as config]
   [pirca.ssg.core :as core]
   [pirca.ssg.post :as post]
   [pirca.ssg.process :as process]))

(def ^:private mirrored (atom #{}))

(defn setup-git-auth!
  "Configure git to authenticate HTTP(S) operations under forge-base-url
  with a Forgejo access token."
  [forge-base-url username token]
  (let [authed (str/replace-first forge-base-url
                                  #"^https?://"
                                  #(str % username ":" token "@"))]
    (process/shell "git"
                   "config"
                   "--global"
                   (str "url." authed ".insteadOf")
                   forge-base-url)))

(defn ensure-mirror!
  "Clone url as a bare repo to git-dir if not present, or fetch updates if it
  is. Skips if already called for this git-dir in the current session."
  [url git-dir]
  (when-not (contains? @mirrored git-dir)
    (if (fs/exists? (fs/file git-dir "config"))
      (process/shell "git" "-C" git-dir "fetch")
      (process/shell "git" "clone" "--bare" url git-dir))
    (swap! mirrored conj git-dir)))

(defn- read-packed-refs
  [git-dir]
  (let [f (fs/file git-dir "packed-refs")]
    (when (fs/exists? f)
      (->> (str/split-lines (fs/slurp f))
           (remove #(or (str/blank? %)
                        (str/starts-with? % "#")
                        (str/starts-with? % "^")))
           (map #(let [[sha ref] (str/split % #" " 2)] [ref sha]))
           (into {})))))

(defn- read-loose-refs
  "Read loose refs under GIT-DIR/refs, returning {\"refs/heads/x\" sha} with
  paths relative to GIT-DIR."
  [git-dir]
  (let [refs-dir (fs/file git-dir "refs")]
    (when (fs/exists? refs-dir)
      (let [root (fs/absolutize git-dir)]
        (->> (fs/glob refs-dir "**")
             (filter fs/regular-file?)
             (map (fn [f]
                    [(str (fs/relativize root (fs/absolutize f)))
                     (str/trim (fs/slurp f))]))
             (into {}))))))

(defn- write-info-refs!
  [git-dir]
  (let [refs (merge (read-packed-refs git-dir) (read-loose-refs git-dir))
        info (fs/file git-dir "info")]
    (fs/create-dirs info)
    (fs/spit (fs/file info "refs")
             (str (str/join "\n" (map (fn [[ref sha]] (str sha "\t" ref))
                                      refs))
                  "\n"))))

(defn- write-info-packs!
  [git-dir]
  (let [pack-dir (fs/file git-dir "objects" "pack")
        packs    (->> (when (fs/exists? pack-dir) (fs/list-dir pack-dir))
                      (filter #(str/ends-with? (fs/file-name %) ".pack"))
                      (map #(str "P " (fs/file-name %))))
        info     (fs/file git-dir "objects" "info")]
    (fs/create-dirs info)
    (fs/spit (fs/file info "packs") (str (str/join "\n" packs) "\n"))))

(defn prism-css
  "Return a builder that emits Prism's CSS (the theme plus the line-numbers
  plugin) so the git file viewer can highlight code.

  Options:
    :output-path - artifact path (default \"assets/css/highlight.css\")
    :theme       - Prism theme file under node_modules (default prism-tomorrow)
    :plugins     - seq of plugin CSS files under node_modules"
  [&
   {:keys [output-path theme plugins]
    :or   {output-path "assets/css/highlight.css"
           theme       "node_modules/prismjs/themes/prism-tomorrow.css"
           plugins     ["node_modules/prismjs/plugins/line-numbers/prism-line-numbers.css"]}}]
  (fn [_site _posts]
    [{:path    output-path
      :content (str/join "\n" (map fs/slurp (cons theme plugins)))}]))

(defn repo-dumb-http
  "Return a builder that serves each project's git repo via the dumb HTTP
  protocol. Generates info/refs and objects/info/packs index files, then
  copies each bare repo into the site output so users can clone with:
    git clone https://<domain>/<prefix>/<slug>.git

  Repos are mirrored via ensure-mirror! (no-op if already done this session
  by projects/prepare!).

  Options:
    :prefix         - URL prefix (default \"/projects\")
    :cache-dir      - bare repo cache directory (default \".git-cache\")
    :forge-base-url - base URL for cloning (e.g. \"ssh://forgejo@host/org/\")
    :projects       - seq of project maps (each with :repo-name)"
  [&
   {:keys [prefix cache-dir forge-base-url projects]
    :or   {prefix    "/projects"
           cache-dir ".git-cache"}}]
  (fn [_site _posts]
    (mapcat (fn [{:keys [repo-name]}]
              (let [slug    (post/->slug repo-name)
                    git-dir (str cache-dir "/" slug ".git")]
                (try
                  (when forge-base-url
                    (ensure-mirror! (str forge-base-url repo-name) git-dir))
                  (process/shell "git"
                                 "-C"     git-dir
                                 "repack" "-a"
                                 "-d"     "--max-pack-size=20m")
                  (write-info-refs! git-dir)
                  (write-info-packs! git-dir)
                  [{:path       (str (subs prefix 1) "/" slug ".git")
                    :copy-from  git-dir
                    :directory? true}
                   {:path       (str (subs prefix 1) "/" slug)
                    :copy-from  git-dir
                    :directory? true}]
                  (catch #?(:clj Exception :cljs js/Error) e
                    (println "Warning: failed to serve" slug
                             "-"                        (.getMessage e))
                    nil))))
            (or projects []))))

(defn repo-last-commit
  "Return {:sha :message :date} for HEAD of git-dir, or nil if unavailable."
  [git-dir]
  (try
    (let [{:keys [exit out]}
          (process/capture "git" "-C" git-dir "log" "-1"
                           "--format=%H%x1f%s%x1f%cI")]
      (when (zero? exit)
        (when-let [line (first (filter seq (str/split-lines out)))]
          (zipmap [:sha :message :date]
                  (str/split (str/trim line) #"\x1f" 3)))))
    (catch #?(:clj Exception :cljs js/Error) _ nil)))

(defn repo-commits
  "Return seq of {:sha :message :date} for git-dir, newest first."
  [git-dir & {:keys [max-count] :or {max-count 50}}]
  (try
    (let [{:keys [exit out]}
          (process/capture "git" "-C" git-dir "log"
                           (str "--max-count=" max-count)
                           "--format=%H%x1f%s%x1f%cI")]
      (when (zero? exit)
        (->> (str/split-lines out)
             (filter seq)
             (map #(zipmap [:sha :message :date]
                           (str/split (str/trim %) #"\x1f" 3))))))
    (catch #?(:clj Exception :cljs js/Error) _ [])))

(defmethod ig/init-key ::builder
  [_ {:keys [config output]}]
  (when (config/enabled? config :builders :repo)
    (let [{:keys [forge-base-url cache-dir prefix]
           :or   {cache-dir ".git-cache"}} (config/opts config :builders :repo)
          builders (cond-> [(prism-css)]
                     forge-base-url
                     (conj (repo-dumb-http
                            :prefix         (or prefix "/projects")
                            :cache-dir      cache-dir
                            :forge-base-url forge-base-url
                            :projects       (:projects config))))]
      (core/write-artifacts! output (map #(% nil nil) builders))
      nil)))
