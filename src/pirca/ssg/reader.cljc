(ns pirca.ssg.reader
  "Reader abstraction and post-directory scanning.

  A reader is a map:
    {:extensions  #{\"org\" \"md\"}
     :read-fn     (fn [file posts-root] -> [metadata-map html-string])

  metadata-map is a parsed map (date as LocalDateTime, tags as vector of strings).
  html-stirng is a raw HTML string wrapped in a <div>.

  The posts-root argument lets dir-tagging readers compute relative paths.
  Simple readers can ignore it by using (fn [file & _] ...)."
  (:require
   [babashka.fs :as fs]
   [pirca.ssg.post :as post]))

(defn- find-reader
  [readers file]
  (let [ext (fs/extension file)]
    (some (fn [r] (when (contains? (:extensions r) ext) r)) readers)))

(defn- file->slug [file] (fs/strip-ext (fs/file-name file)))

(defn read-posts
  "Scan POSTS-DIR recursively and read every file matched by any reader.
  Returns a sequence of post maps."
  [posts-dir readers]
  (let [root (fs/file posts-dir)]
    (->> (fs/glob root "**")
         (filter fs/regular-file?)
         (keep (fn [file]
                 (when-let [reader (find-reader readers file)]
                   (try (let [[metadata content] ((:read-fn reader) file root)
                              slug               (file->slug file)]
                          (assoc (post/make-post slug metadata content)
                                 :path (str (fs/relativize root file))))
                        (catch #?(:clj Exception :cljs js/Error) e
                          (println "Warning: failed to read" (str file)
                                   "-"                       (.getMessage e))
                          nil))))))))
