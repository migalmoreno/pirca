(ns pirca.ssg.serve.jvm
  "JVM serve backend: Jetty static file server + java.nio WatchService."
  (:require
   [babashka.fs :as fs]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.file :refer [wrap-file]]
   [ring.middleware.not-modified :refer [wrap-not-modified]]
   [ring.util.mime-type :as mime]
   [pirca.ssg.serve :as serve])
  (:import
   [java.nio.file FileSystems StandardWatchEventKinds]))

(defn- wrap-file-content-type
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (and (instance? java.io.File (:body response))
               (nil? (get-in response [:headers "Content-Type"])))
        (let [ct (or (mime/ext-mime-type (.getName ^java.io.File
                                                   (:body response)))
                     "text/plain")]
          (assoc-in response [:headers "Content-Type"] ct))
        response))))

(defn- create-handler
  [dir]
  (-> (constantly
       {:status 404 :headers {"Content-Type" "text/plain"} :body "Not found"})
      (wrap-file dir {:index-files? true})
      wrap-file-content-type
      wrap-not-modified))

(defn- watch-kinds
  []
  (into-array [StandardWatchEventKinds/ENTRY_CREATE
               StandardWatchEventKinds/ENTRY_MODIFY
               StandardWatchEventKinds/ENTRY_DELETE]))

(defn- register-tree
  [watcher root]
  (doseq [f     (fs/glob root "**")
          :when (fs/directory? f)]
    (.register (fs/path f) watcher (watch-kinds)))
  (.register (fs/path root) watcher (watch-kinds)))

(defn- watch!
  [dirs rebuild-fn]
  (let [watcher (.newWatchService (FileSystems/getDefault))]
    (doseq [dir dirs :when (fs/exists? dir)] (register-tree watcher dir))
    (future (try (loop []
                   (when-let [k (.take watcher)]
                     (doseq [event (.pollEvents k)
                             :when (= (.kind event)
                                      StandardWatchEventKinds/ENTRY_CREATE)]
                       (let [child (fs/file (str (.watchable k))
                                            (str (.context event)))]
                         (when (fs/directory? child)
                           (register-tree watcher (str child)))))
                     (try (rebuild-fn)
                          (catch Exception e
                            (println "Build error:" (.getMessage e))))
                     (.reset k)
                     (recur)))
                 (finally (.close watcher))))))

(defn backend
  "Create a fresh JVM backend. Constructed on demand (rather than cached in a
  def) so namespace reloads that redefine the Backend protocol do not leave a
  stale reified instance behind."
  []
  (reify serve/Backend
    (start-server! [_ dir port]
      (jetty/run-jetty (create-handler dir) {:port port :join? false}))
    (stop-server! [_ handle]
      (.stop ^org.eclipse.jetty.server.Server handle))
    (watch-dirs! [_ dirs on-change]
      (watch! dirs on-change))
    (cancel-watch! [_ handle]
      (future-cancel handle))))
