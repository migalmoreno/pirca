(ns pirca.ssg.builders.frontend
  (:require
   [integrant.core :as ig]
   [pirca.ssg.config :as config]
   [pirca.ssg.core :as core]))

(defn- release-frontend!
  [build-id]
  #?(:clj
     (do
       (require 'shadow.cljs.devtools.api
                'shadow.cljs.devtools.server)
       (let [start!  (resolve 'shadow.cljs.devtools.server/start!)
             stop!   (resolve 'shadow.cljs.devtools.server/stop!)
             release (resolve 'shadow.cljs.devtools.api/release)]
         (start!)
         (try
           (release build-id)
           (finally (stop!)))))
     :cljs
     (throw (ex-info "The frontend builder requires the JVM (shadow-cljs)"
                     {:build-id build-id}))))

(defn cljs-bundle
  [{:keys [build-id output-path build-dir]
    :or   {build-id    :frontend
           output-path "assets/js"
           build-dir   "dist"}}]
  (fn [_site _posts]
    (release-frontend! build-id)
    [{:path       output-path
      :copy-from  build-dir
      :directory? true}]))

(defmethod ig/init-key ::builder
  [_ {:keys [config output]}]
  (when (config/enabled? config :builders :frontend)
    (let [builder (cljs-bundle (config/opts config :builders :frontend))]
      (core/write-artifacts! output (builder nil nil))
      nil)))
