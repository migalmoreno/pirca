(ns pirca.ssg.serve
  "Cross-platform static dev server.

  The HTTP engine and file watcher are provided by a platform backend
  implementing the `Backend` protocol. Use `serve!` for the common case; it
  starts the default backend, serves DIR, watches DIRS, and calls BUILD-FN on
  every change.

  Platform backends:
    - pirca.ssg.serve.jvm  (Jetty + java.nio WatchService) - JVM
    - pirca.ssg.serve.node (http + fs.watch)               - Node"
  (:require
   [clojure.string :as str]))

(defprotocol Backend
  "A static file server plus a directory watcher."
  (start-server! [_ dir port]
    "Start serving DIR on PORT. Returns an opaque server handle.")
  (stop-server! [_ handle]
    "Stop the server handle returned by start-server!.")
  (watch-dirs! [_ dirs on-change]
    "Watch DIRS recursively, calling ON-CHANGE on every change.
     Returns a cancellable handle.")
  (cancel-watch! [_ handle]
    "Stop the watcher handle returned by watch-dirs!."))

(defonce ^:private state
  (atom {:server nil :watcher nil :backend nil}))

(defn stop!
  "Stop any running server and file watcher."
  []
  (let [{:keys [server watcher backend]} @state]
    (when (and backend watcher) (cancel-watch! backend watcher))
    (when (and backend server) (stop-server! backend server))
    (reset! state {:server nil :watcher nil :backend nil})))

#?(:clj
   (do
     (defn- default-backend []
       (@(requiring-resolve 'pirca.ssg.serve.jvm/backend)))
     (defonce ^:private _shutdown-hook
       (let [hook (Thread. stop!)]
         (.addShutdownHook (Runtime/getRuntime) hook)
         hook))))

#?(:cljs
   (defn- default-backend []
     (when-let [r (try (js/require "./serve/node") (catch :default _ nil))]
       (.-backend r))))

(defn start!
  "Start the default platform backend. Returns the server handle."
  [& [dir port]]
  (let [dir      (or dir "site")
        port     (Integer/parseInt (str (or port "8080")))
        backend  (default-backend)
        existing (:watcher @state)]
    (when existing
      (cancel-watch! backend existing))
    (println (str "Serving " dir "/ at http://localhost:" port))
    (let [server
          (try
            (start-server! backend dir port)
            (catch #?(:clj Exception :cljs js/Error) e
              (let [msg (or (.getMessage e) "")]
                (if (re-find #"(?i)address already in use|EADDRINUSE" msg)
                  (do (println (str "Port " port " is already in use; "
                                    "stop the other server or choose a "
                                    "different :port."))
                      (throw (ex-info (str "Port " port " already in use")
                                      {:port port :dir dir})))
                  (throw e)))))]
      (reset! state {:backend backend :server server :watcher nil}))))

(defn- debounce
  "Return a fn that coalesces rapid calls, invoking F once after MS of quiet."
  [f ms]
  #?(:clj
     (let [pending (atom nil)]
       (fn []
         (when-let [p @pending] (future-cancel p))
         (reset! pending
                 (future
                   (try
                     (Thread/sleep ms)
                     (f)
                     (catch java.util.concurrent.CancellationException _)
                     (catch Exception e
                       (println "Build error:" (.getMessage e))))))))
     :cljs
     (let [pending (atom nil)]
       (fn []
         (when-let [t @pending] (js/clearTimeout t))
         (reset! pending
                 (js/setTimeout
                  (fn []
                    (try
                      (f)
                      (catch :default e
                        (println "Build error:" (.-message e)))))
                  ms))))))

(defn serve!
  "Build, start the file server, and watch DIRS for changes.

  Options:
    :dir         - directory to serve (default \"site\")
    :port        - port to listen on (default 8080)
    :dirs        - directories to watch (default [\"posts\" \"assets\"])
    :debounce-ms - coalesce watcher events over this window (default 200)"
  [build-fn &
   [{:keys [dir port dirs debounce-ms]
     :or   {dir "site" port "8080" dirs ["posts" "assets"]
            debounce-ms 200}}]]
  (build-fn)
  (start! dir port)
  (let [{:keys [backend]} @state]
    (swap! state assoc :watcher
           (watch-dirs! backend dirs (debounce build-fn debounce-ms)))))

#?(:clj
   (defn -main [& [dir port]]
     (start! dir port)
     (.join ^org.eclipse.jetty.server.Server (:server @state))))
