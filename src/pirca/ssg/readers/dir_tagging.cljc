(ns pirca.ssg.readers.dir-tagging
  "Adds directory names from each post's path as metadata. A post at
  posts/projects/foo.org gains {:projects true}."
  (:require
   [clojure.string :as str]))

(defn tag-posts
  [posts]
  (map (fn [{:keys [path] :as p}]
         (let [dirs (->> (some-> path (str/split #"/") butlast)
                         (remove str/blank?))]
           (update p :metadata merge
                   (into {} (map (fn [d] [(keyword d) true]) dirs)))))
       posts))
