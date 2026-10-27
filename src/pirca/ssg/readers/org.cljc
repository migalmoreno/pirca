(ns pirca.ssg.readers.org
  "Org-mode reader: renders .org files to HTML via an Emacs subprocess.

  Requires an `emacs` (or `emacsclient`) binary on PATH.

  Options (passed to `org-reader`):
    :cache-dir   - cache directory (default: .org-mode-reader-cache)
    :cache?      - enable caching (default: true)
    :preamble    - path to an elisp file loaded before export
    :emacsclient?- use emacsclient instead of emacs --batch
    :daemon-name - emacsclient daemon socket name (default: blog-build)"
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [integrant.core :as ig]
   [pirca.ssg.platform :as platform]
   [pirca.ssg.post :as post]
   [pirca.ssg.process :as process]
   [pirca.ssg.reader :as reader]))

(def ^:private default-opts
  {:cache-dir   ".org-mode-reader-cache"
   :cache?      true
   :daemon-name "blog-build"})

(defn- escape-path
  [path]
  (-> path
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn- with-preamble
  [opts form]
  (if-let [p (:preamble opts)]
    (str "(progn (load-file \"" (escape-path p) "\") " form ")")
    form))

(defn- render-form
  [opts file-path tmp-path]
  (with-preamble
    opts
    (str "(save-excursion"
         " (require 'htmlize)"
         " (setq org-html-htmlize-output-type 'css)"
         " (let ((enable-local-variables :all))"
         "   (set-buffer (find-file-noselect \""
         (escape-path file-path)
         "\")))"
         " (setq-local org-export-filter-latex-fragment-functions"
         "   (list (lambda (data backend channel)"
         "           (org-html-encode-plain-text data))))"
         " (let ((result (org-export-as 'html nil nil t)))"
         "   (with-temp-buffer"
         "     (insert result)"
         "     (write-region (point-min) (point-max) \""
         (escape-path tmp-path)
         "\"))))")))

(defn- elisp-string-list
  [strings]
  (str "(" (str/join " " (map #(str "\"" % "\"") strings)) ")"))

(defn- metadata-form
  [opts file-path keys]
  (with-preamble
    opts
    (str "(save-excursion"
         " (let ((enable-local-variables :all))"
         "   (set-buffer (find-file-noselect \""
         (escape-path file-path)
         "\")))"
         " (org-collect-keywords '"
         (elisp-string-list keys)
         "))")))

(defn- daemon-running?
  [opts]
  (and (:emacsclient? opts)
       (zero? (:exit (process/capture "emacsclient" "--alternate-editor" "false"
                                      "-s" (:daemon-name opts) "-e" "t")))))

(defn stop-daemon!
  [opts]
  (when (daemon-running? opts)
    (process/capture "emacsclient" "-s" (:daemon-name opts) "-e"
                     "(kill-emacs)")))

(defn- ensure-daemon!
  [opts]
  (when (and (:emacsclient? opts) (not (daemon-running? opts)))
    (process/shell "emacs" (str "--daemon=" (:daemon-name opts)) "-Q")
    (loop [remaining 20]
      (process/sleep 500)
      (when (and (pos? remaining) (not (daemon-running? opts)))
        (recur (dec remaining))))))

(def ^:private emacs-timeout-ms 60000)

(defn- run-emacs
  [opts form-str]
  (let [cmd    (if (:emacsclient? opts)
                 ["emacsclient" "--alternate-editor" "false"
                  "-s" (:daemon-name opts) "-e" form-str]
                 ["emacs" "--batch" "--eval" form-str])
        {:keys [out err exit]} (apply process/capture-timeout
                                      emacs-timeout-ms
                                      cmd)]
    (when-not (zero? exit)
      (throw (ex-info (str "Emacs subprocess failed\n" err)
                      {:exit exit :stderr err})))
    out))

(defn- cache-file
  [opts hash suffix]
  (fs/file (:cache-dir opts) (str hash "-" suffix)))

(defn- read-cache
  "Return [raw-metadata-map html-string] from cache, or nil on miss."
  [opts hash]
  (let [meta-f (cache-file opts hash "metadata")
        html-f (cache-file opts hash "content")]
    (when (and (fs/exists? meta-f) (fs/exists? html-f))
      [(read-string (fs/slurp meta-f)) (fs/slurp html-f)])))

(defn- write-cache
  [opts hash metadata-raw html]
  (fs/create-dirs (:cache-dir opts))
  (fs/spit (cache-file opts hash "metadata") (pr-str metadata-raw))
  (fs/spit (cache-file opts hash "content") html))

(def ^:private default-extra-keys
  ["CROSSPOST" "SCRIPTS" "META-TAGS" "LICENSE" "SYNOPSIS" "SOURCE-DIR"
   "REPO-NAME"])

(defn- collect-metadata-raw
  "Call Emacs to collect org keywords; returns a map of keyword->string."
  [opts file extra-keys]
  (let [all-keys    (into ["TITLE" "DATE" "TAGS"] extra-keys)
        form        (metadata-form opts (str (fs/absolutize file)) all-keys)
        form-to-run (if (:emacsclient? opts) form (str "(print " form ")"))
        result      (str/trim (run-emacs opts form-to-run))
        pairs       (read-string result)]
    (into {} (map (fn [[k v]] [(keyword (str/lower-case k)) v]) pairs))))

(defn- parse-metadata
  "Convert raw string metadata into typed values (dates, tag lists)."
  [raw]
  (cond-> raw
    (:date raw) (update :date post/parse-org-date)
    (:tags raw) (update :tags #(str/split % #"\s+"))))

(defn- render-org
  [opts file]
  (let [tmp (fs/create-temp-file {:prefix "ssg-org-" :suffix ".html"})]
    (try
      (run-emacs opts (render-form opts (str (fs/absolutize file))
                                   (str (fs/absolutize tmp))))
      (str "<div>" (fs/slurp tmp) "</div>")
      (finally (fs/delete-if-exists tmp)))))

(defn read-org-post
  "Read an org-mode FILE and return [metadata content]. Results are
  optionally cached by file content hash."
  [opts file _posts-root]
  (ensure-daemon! opts)
  (let [hash   (platform/md5 file)
        cached (when (:cache? opts) (read-cache opts hash))]
    (if cached
      [(parse-metadata (first cached)) (second cached)]
      (let [metadata-raw (collect-metadata-raw opts file default-extra-keys)
            html         (render-org opts file)]
        (when (:cache? opts)
          (write-cache opts hash metadata-raw html))
        [(parse-metadata metadata-raw) html]))))

(defn org-reader
  "Return a reader map for .org files. OPTS is merged over the defaults."
  [& [opts]]
  (let [opts (merge default-opts opts)]
    {:extensions #{"org"}
     :read-fn    (fn [file posts-root] (read-org-post opts file posts-root))}))

(defmethod ig/init-key ::daemon
  [_ {:keys [config]}]
  (when-let [opts (:org (:readers config))]
    (stop-daemon! (merge default-opts opts)))
  nil)

(defmethod ig/init-key ::reader
  [_ {:keys [config]}]
  (let [opts (:org (:readers config))
        dir  (get config :posts-dir "posts")
        defaults (let [{:keys [fullname email]} (:site config)]
                   (cond-> {}
                     fullname (assoc :author fullname)
                     email    (assoc :email email)))
        posts (doall (reader/read-posts dir [(org-reader opts)]))]
    (if (seq defaults)
      (map #(update % :metadata (fn [m] (merge defaults m))) posts)
      posts)))
