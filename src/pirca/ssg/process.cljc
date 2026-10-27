(ns pirca.ssg.process
  "Cross-platform subprocess shim.

  On the JVM this delegates to babashka.process; on Node it delegates to
  child_process. Only the subset needed by the SSG builders is exposed.

  `shell` runs a command with inherited stdio and throws on non-zero exit.
  `capture` runs a command and returns {:exit :out :err} as strings.

  Recognized options (first arg, map): :dir (working directory) and
  :extra-env (environment variables merged over the current environment)."
  (:require
   #?(:clj [babashka.process :as proc])))

#?(:cljs
   (defn- spawn-sync
     "Run ARGS (an (opts? cmd & rest) seq) with child_process.spawnSync.
     EXTRA-OPTIONS are merged into the spawn options."
     [args extra-options]
     (let [[opts cmd & argv] (if (map? (first args))
                               args
                               (cons {} args))
           options          (cond-> (merge {:cwd (when-let [d (:dir opts)] d)}
                                           extra-options)
                              (:extra-env opts)
                              (assoc :env (js/Object.assign #js {}
                                                            (.-env js/process)
                                                            (clj->js (:extra-env opts)))))]
       (.spawnSync (js/require "child_process")
                   (clj->js (into [cmd] argv))
                   (clj->js options)))))

(defn shell
  "Run COMMAND with ARGS, streaming output, throwing on non-zero exit.
  Extra option maps may be passed as the first argument."
  [& args]
  #?(:clj  (apply proc/shell args)
     :cljs (let [res (spawn-sync args {:stdio "inherit"})]
             (when-not (zero? (.-status res))
               (throw (ex-info (str "Command failed: " (pr-str args))
                               {:exit (.-status res)})))
             res)))

(defn capture
  "Run COMMAND with ARGS and return {:exit :out :err} (all strings).
  Extra option maps may be passed as the first argument."
  [& args]
  #?(:clj  (let [[opts args'] (if (map? (first args))
                                [(first args) (rest args)]
                                [{} args])
                 {:keys [exit out err]} @(proc/process (vec args')
                                                       (merge {:out :string
                                                               :err :string}
                                                              opts))]
             {:exit exit :out out :err err})
     :cljs (let [res (spawn-sync args {:encoding "utf8"})]
             {:exit (or (.-status res) 0)
              :out  (or (.-stdout res) "")
              :err  (or (.-stderr res) "")})))

(defn capture-timeout
  "Like `capture`, but kill the process and throw if it does not finish
  within TIMEOUT-MS. Returns {:exit :out :err}."
  [timeout-ms & args]
  #?(:clj  (let [[opts args'] (if (map? (first args))
                                [(first args) (rest args)]
                                [{} args])
                 p      (proc/process (vec args')
                                      (merge {:out :string :err :string} opts))
                 result (deref p timeout-ms ::timeout)]
             (when (= result ::timeout)
               (.destroyForcibly ^java.lang.Process (:proc p))
               (throw (ex-info (str "Process timed out after "
                                    (/ timeout-ms 1000) "s")
                               {:timeout true})))
             (let [{:keys [exit out err]} result]
               {:exit exit :out out :err err}))
     :cljs (let [res (spawn-sync args {:encoding "utf8"
                                       :timeout  timeout-ms})]
             (when (.-error res)
               (throw (ex-info (str "Process timed out after "
                                    (/ timeout-ms 1000) "s")
                               {:timeout true})))
             {:exit (or (.-status res) 0)
              :out  (or (.-stdout res) "")
              :err  (or (.-stderr res) "")})))

(defn sleep
  "Block the current thread for MS milliseconds."
  [ms]
  #?(:clj  (Thread/sleep ms)
     :cljs (let [end (+ (js/Date.now) ms)]
             (while (< (js/Date.now) end) nil))))
