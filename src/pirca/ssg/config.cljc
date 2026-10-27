(ns pirca.ssg.config
  "Library-owned configuration.

  The schemas below are the single source of default values: `load-config`
  validates a config map and fills in any missing keys from their schema
  defaults. A site supplies environment-derived values (e.g. #env) and picks
  which builders/readers to enable; keys are gated by presence, so defaults
  do not enable anything."
  (:require
   [aero.core :as aero]
   [integrant.core :as ig]
   [malli.core :as m]
   [malli.error :as me]
   [malli.transform :as mt]
   [babashka.fs :as fs]
   #?(:clj [clojure.java.io :as io])))

(def ^:private atom-schema
  [:map
   [:prefix {:optional true} [:string {:default "/blog"}]]
   [:project-prefix {:optional true} [:string {:default "/projects"}]]
   [:blog? {:optional true} :boolean]
   [:projects? {:optional true} :boolean]])

(def ^:private repo-schema
  [:map
   [:forge-base-url {:optional true} [:maybe :string]]
   [:forgejo-token {:optional true} [:maybe :string]]
   [:username {:optional true} [:maybe :string]]
   [:cache-dir {:optional true} [:string {:default ".git-cache"}]]
   [:prefix {:optional true} [:string {:default "/projects"}]]])

(def ^:private assets-schema
  [:map
   [:dir {:optional true} [:string {:default "assets"}]]])

(def ^:private frontend-schema
  [:map
   [:build-id {:optional true} :keyword]
   [:output-path {:optional true} :string]
   [:build-dir {:optional true} :string]])

(def ^:private github-schema
  [:map
   [:owner {:optional true} [:maybe :string]]
   [:token {:optional true} [:maybe :string]]])

(def ^:private org-reader-schema
  [:map
   [:cache-dir {:optional true} :string]
   [:cache? {:optional true} :boolean]
   [:preamble {:optional true} [:maybe :string]]
   [:emacsclient? {:optional true} :boolean]
   [:daemon-name {:optional true} :string]])

(def schema
  [:map
   [:site [:map
           [:title :string]
           [:domain {:optional true} [:maybe :string]]
           [:fullname {:optional true} [:maybe :string]]
           [:email {:optional true} [:maybe :string]]]]
   [:posts-dir {:optional true} [:string {:default "posts"}]]
   [:output-dir {:optional true} [:string {:default "site"}]]
   [:projects {:optional true} [:vector {:default []} :map]]
   [:readers
    {:optional true}
    [:map {:default {}}
     [:org {:optional true} org-reader-schema]]]
   [:builders
    {:optional true}
    [:map {:default {}}
     [:atom     {:optional true} atom-schema]
     [:repo     {:optional true} repo-schema]
     [:assets   {:optional true} assets-schema]
     [:frontend {:optional true} frontend-schema]
     [:github   {:optional true} github-schema]]]])

(def ^:private defaults-transformer
  (mt/default-value-transformer {::mt/add-optional-keys true}))

(defn load-config
  "Validate SITE-CONFIG against the schema, filling missing keys from their
  schema defaults. Throws ex-info with a human-readable explanation when the
  result does not conform."
  [site-config]
  (if (m/validate schema site-config)
    (m/decode schema site-config defaults-transformer)
    (throw (ex-info (str "Invalid pirca configuration:\n"
                         (me/humanize (m/explain schema site-config)))
                    {:config site-config
                     :errors (me/humanize (m/explain schema site-config))}))))

(defmethod aero/reader 'ig/ref
  [_ _ value]
  (ig/ref value))

(defn read-file
  "Read a config file from a filesystem path (JVM) or EDN string (CLJS)."
  [file]
  #?(:clj  (aero/read-config (io/file file))
     :cljs (aero/read-config (fs/slurp file))))

(defn read-system
  "Read the :system section (an integrant system map) from CONFIG-FILE."
  [config-file]
  (when config-file
    (:system (read-file config-file))))

(defmethod ig/init-key ::config
  [_ {:keys [config-file overrides]}]
  (let [file (or config-file
                 #?(:clj (io/resource "config.edn")
                    :cljs "config.edn"))]
    (load-config (merge (if file (dissoc (read-file file) :system) {})
                        overrides))))

(defn enabled?
  "True when the :builders/:readers key K is present in CONFIG."
  [config section k]
  (contains? (get config section) k))

(defn opts
  "Options map for CONFIG's section/key K (empty map when none were given)."
  [config section k]
  (let [v (get-in config [section k])]
    (if (map? v) v {})))
