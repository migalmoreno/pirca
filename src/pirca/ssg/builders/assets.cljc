(ns pirca.ssg.builders.assets
  "Static assets builder: copies directory trees into the output."
  (:require
   [integrant.core :as ig]
   [pirca.ssg.config :as config]
   [pirca.ssg.core :as core]))

(defn static-directory
  "Return a builder that copies SOURCE-DIR into the output directory.

  The directory is copied as-is: assets/ -> <output>/assets/"
  [source-dir]
  (fn [_site _posts]
    [{:path source-dir :copy-from source-dir :directory? true}]))

(defmethod ig/init-key ::builder
  [_ {:keys [config output]}]
  (when (config/enabled? config :builders :assets)
    (let [builder (static-directory (get (config/opts config :builders :assets)
                                         :dir "assets"))]
      (core/write-artifacts! output (builder nil nil))
      nil)))
