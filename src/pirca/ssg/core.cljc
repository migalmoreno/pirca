(ns pirca.ssg.core
  (:require
   [babashka.fs :as fs]))

(defn- write-bytes!
  "Write a byte array to FILE on either platform."
  [file ^bytes content]
  #?(:clj  (with-open [out (java.io.FileOutputStream. file)]
             (.write out content))
     :cljs (.writeFileSync (js/require "fs") file (js/Buffer.from content))))

(defn- write-artifact
  [output-dir {:keys [path content copy-from directory?]}]
  (let [dest (fs/path output-dir path)]
    (if directory?
      (fs/copy-tree copy-from dest {:replace-existing true})
      (do
        (fs/create-dirs (fs/parent dest))
        (if (bytes? content)
          (write-bytes! (fs/file dest) content)
          (fs/spit dest content))))))

(defn- normalize-artifacts
  "Coerce the return value of a builder into a flat seq of artifact maps."
  [result]
  (cond (nil? result) []
        (map? result) [result]
        :else         (filter map? (flatten result))))

(defn prepare-output!
  "Remove OUTPUT-DIR if present and recreate it, so stale files from previous
  builds do not linger."
  [output-dir]
  (when (fs/exists? output-dir)
    (fs/delete-tree output-dir))
  (fs/create-dirs output-dir))

(defn write-artifacts!
  "Write ARTIFACTS (builder return values) to OUTPUT-DIR."
  [output-dir artifacts]
  (doseq [artifact (normalize-artifacts artifacts)]
    (write-artifact output-dir artifact)))
