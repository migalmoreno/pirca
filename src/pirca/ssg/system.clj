(ns pirca.ssg.system
  (:require
   [clojure.java.io :as io]
   [integrant.core :as ig]
   [pirca.ssg.config :as config]
   [pirca.ssg.core :as core]
   [pirca.ssg.reader]
   [pirca.ssg.readers.org]
   [pirca.ssg.builders.projects]
   [pirca.ssg.builders.atom]
   [pirca.ssg.builders.repo]
   [pirca.ssg.builders.assets]))

(defmethod ig/init-key ::output
  [_ {:keys [config]}]
  (let [dir (get config :output-dir "site")]
    (core/prepare-output! dir)
    dir))

(def default-system
  {:pirca.ssg.config/config                 {}
   :pirca.ssg.readers.org/daemon            {:config (ig/ref :pirca.ssg.config/config)}
   :pirca.ssg.readers.org/reader            {:config (ig/ref :pirca.ssg.config/config)}
   :pirca.ssg.reader/posts                  {:config  (ig/ref :pirca.ssg.config/config)
                                             :readers [(ig/ref :pirca.ssg.readers.org/reader)]
                                             :repos   (ig/ref :pirca.ssg.builders.projects/repos)}
   :pirca.ssg.builders.projects/repos       {:config (ig/ref :pirca.ssg.config/config)}
   :pirca.ssg.builders.projects/github      {:config (ig/ref :pirca.ssg.config/config)
                                             :repos  (ig/ref :pirca.ssg.builders.projects/repos)}
   ::output                                 {:config (ig/ref :pirca.ssg.config/config)}
   :pirca.ssg.builders.atom/builder         {:config (ig/ref :pirca.ssg.config/config)
                                             :output (ig/ref ::output)
                                             :posts  (ig/ref :pirca.ssg.reader/posts)}
   :pirca.ssg.builders.repo/builder         {:config (ig/ref :pirca.ssg.config/config)
                                             :output (ig/ref ::output)}
   :pirca.ssg.builders.assets/builder       {:config (ig/ref :pirca.ssg.config/config)
                                             :output (ig/ref ::output)}})

(defn config
  "The merged system map: pirca defaults overlaid with the :system section of
  CONFIG-FILE (defaults to the classpath config.edn). Pass it to ig/init."
  ([] (config (io/resource "config.edn")))
  ([config-file]
   (merge default-system (config/read-system config-file))))
