(ns pirca.ssg.builders.atom
  "Atom feed builder."
  (:require
   [integrant.core :as ig]
   [pirca.ssg.builders.repo :as repo]
   [pirca.ssg.config :as config]
   [pirca.ssg.core :as core]
   [pirca.ssg.post :as post]
   [clojure.data.xml :as xml]
   [tick.core :as t]))

(def ^:private rfc3339
  (t/formatter "yyyy-MM-dd'T'HH:mm:ssXXX"))

(defn- format-rfc3339
  [date]
  (t/format rfc3339 (t/in date "UTC")))

(defn- post-url
  [domain prefix p]
  (str "https://" domain prefix "/" (post/post-slug p) ".html"))

(defn- entry-xml
  [domain prefix post]
  {:tag     :entry
   :content [{:tag :title :content [(post/post-title post)]}
             {:tag   :link
              :attrs {:href (post-url domain prefix post) :rel "alternate"}}
             {:tag :id :content [(post-url domain prefix post)]}
             {:tag     :updated
              :content [(format-rfc3339 (post/post-date post))]}
             {:tag     :content
              :attrs   {:type "html"}
              :content [(post/post-content post)]}]})

(defn- feed-xml
  [site posts prefix feed-url]
  (let [domain (get site :domain "localhost")
        title  (get site :title "Blog")]
    {:tag     :feed
     :attrs   {:xmlns "http://www.w3.org/2005/Atom"}
     :content [{:tag :title :content [title]}
               {:tag :link :attrs {:href feed-url :rel "self"}}
               {:tag   :link
                :attrs {:href (str "https://" domain) :rel "alternate"}}
               {:tag :id :attrs {} :content [feed-url]}
               {:tag     :updated
                :content [(format-rfc3339 (post/post-date (first posts)))]}
               (map (partial entry-xml domain prefix) posts)]}))

(defn atom-feed
  "Return a builder that generates an Atom feed for all posts.

  Options:
    :prefix  - URL prefix for post links, e.g. \"/blog\" (default \"/blog\")
    :path    - output path, e.g. \"feed.xml\" (default \"feed.xml\")"
  [& {:keys [prefix path] :or {prefix "/blog" path "feed.xml"}}]
  (fn [site posts]
    (when (seq posts)
      (let [sorted (post/posts-reverse-chronological posts)]
        [{:path    path
          :content (xml/emit-str
                    (feed-xml site sorted prefix
                              (str "https://" (get site :domain "localhost")
                                   prefix "/feed.xml")))}]))))

(defn atom-feeds-by-tag
  "Return a builder that generates one Atom feed per tag.

  Options:
    :prefix      - URL prefix for post links (default \"/blog\")
    :tags-prefix - output path prefix (default \"feeds/tags\")"
  [& {:keys [prefix tags-prefix] :or {prefix "/blog" tags-prefix "feeds/tags"}}]
  (fn [site posts]
    (let [tags (distinct (mapcat post/post-tags posts))]
      (for [tag tags]
        (let [tagged (filter #(some #{tag} (post/post-tags %)) posts)
              sorted (post/posts-reverse-chronological tagged)]
          {:path    (str tags-prefix "/" tag ".xml")
           :content (xml/emit-str
                     (feed-xml site sorted prefix
                               (str "https://" (get site :domain "localhost")
                                    prefix "/feeds/tags/" tag ".xml")))})))))

;;; Commit-based project feeds
;;
;; These builders are pure with respect to git: callers supply a
;; COMMIT-PROVIDER (fn [post] -> seq of {:sha :message :date}), where :date is
;; an ISO-8601 string. This keeps feed generation decoupled from any particular
;; git implementation.

(defn- parse-git-date
  [s]
  (t/parse-offset-date-time s (:iso-offset-date-time t/predefined-formatters)))

(defn- format-odt
  [odt]
  (t/format rfc3339 odt))

(defn- project-page-url
  [domain prefix slug]
  (str "https://" domain prefix "/" slug ".html"))

(defn- commit-entry-xml
  [domain prefix slug {:keys [sha message date]}]
  {:tag     :entry
   :content [{:tag :title :content [message]}
             {:tag   :link
              :attrs {:href (project-page-url domain prefix slug)
                      :rel  "alternate"}}
             {:tag     :id
              :content [(str (project-page-url domain prefix slug) "#" sha)]}
             {:tag :updated :content [(format-odt (parse-git-date date))]}]})

(defn- project-summary-entry-xml
  [domain prefix repo-name slug {:keys [message date]}]
  {:tag     :entry
   :content [{:tag :title :content [repo-name]}
             {:tag   :link
              :attrs {:href (project-page-url domain prefix slug)
                      :rel  "alternate"}}
             {:tag :id :content [(project-page-url domain prefix slug)]}
             {:tag :updated :content [(format-odt (parse-git-date date))]}
             {:tag :content :attrs {:type "text"} :content [message]}]})

(defn- project-feed-xml
  [site entry-xmls most-recent feed-path title]
  (let [domain   (get site :domain "localhost")
        feed-url (str "https://" domain "/" feed-path)]
    {:tag     :feed
     :attrs   {:xmlns "http://www.w3.org/2005/Atom"}
     :content [{:tag :title :content [title]}
               {:tag :link :attrs {:href feed-url :rel "self"}}
               {:tag   :link
                :attrs {:href (str "https://" domain) :rel "alternate"}}
               {:tag :id :content [feed-url]}
               {:tag :updated :content [most-recent]}
               entry-xmls]}))

(defn- project-posts
  [posts]
  (filter #(boolean (post/post-ref % :projects)) posts))

(defn project-atom-feed
  "Return a builder that generates a combined Atom feed for all projects,
  with one entry per project keyed on the last commit date.

  Options:
    :prefix          - URL prefix for project links (default \"/projects\")
    :path            - output path (default \"projects/feed.xml\")
    :commit-provider - (fn [post] -> seq of {:sha :message :date})"
  [&
   {:keys [prefix path commit-provider]
    :or   {prefix "/projects" path "projects/feed.xml"}}]
  (fn [site posts]
    (let [domain  (get site :domain "localhost")
          entries (->> (project-posts posts)
                       (keep (fn [p]
                               (let [slug    (post/post-slug p)
                                     commits (commit-provider p)]
                                 (when-let [commit (first commits)]
                                   {:date (parse-git-date (:date commit))
                                    :xml  (project-summary-entry-xml
                                           domain
                                           prefix
                                           (or (post/post-ref p :repo-name)
                                               slug)
                                           slug
                                           commit)}))))
                       (sort-by :date #(compare %2 %1)))]
      (when (seq entries)
        [{:path    path
          :content (xml/emit-str
                    (project-feed-xml site
                                      (map :xml entries)
                                      (format-odt (:date (first entries)))
                                      path
                                      (str (get site :title "Site")
                                           " — Projects")))}]))))

(defn project-atom-feeds
  "Return a builder that generates one Atom feed per project with individual
  commits as entries.

  Options:
    :prefix          - URL prefix for project links (default \"/projects\")
    :path-fn         - fn of slug -> output path (default \"projects/<slug>/feed.xml\")
    :commit-provider - (fn [post] -> seq of {:sha :message :date})"
  [&
   {:keys [prefix path-fn commit-provider]
    :or   {prefix  "/projects"
           path-fn #(str "projects/" % "/feed.xml")}}]
  (fn [site posts]
    (let [domain (get site :domain "localhost")]
      (keep
       (fn [p]
         (let [slug      (post/post-slug p)
               repo-name (or (post/post-ref p :repo-name) slug)
               commits   (commit-provider p)]
           (when (seq commits)
             (let [path (path-fn slug)]
               {:path    path
                :content (xml/emit-str
                          (project-feed-xml
                           site
                           (map (partial commit-entry-xml domain prefix slug)
                                commits)
                           (format-odt (parse-git-date (:date (first commits))))
                           path
                           (str (get site :title "Site") " — " repo-name)))}))))
       (project-posts posts)))))

(defn project-atom-feeds-by-tag
  "Return a builder that generates one Atom feed per project tag, each with
  one entry per matching project keyed on last commit date.

  Options:
    :prefix          - URL prefix for project links (default \"/projects\")
    :tags-prefix     - output path prefix (default \"projects/tags\")
    :commit-provider - (fn [post] -> seq of {:sha :message :date})"
  [&
   {:keys [prefix tags-prefix commit-provider]
    :or   {prefix "/projects" tags-prefix "projects/tags"}}]
  (fn [site posts]
    (let [domain (get site :domain "localhost")
          tagged (->> (project-posts posts)
                      (keep (fn [p]
                              (let [slug    (post/post-slug p)
                                    commits (commit-provider p)]
                                (when-let [commit (first commits)]
                                  {:slug      slug
                                   :repo-name (or (post/post-ref p :repo-name)
                                                  slug)
                                   :tags      (post/post-tags p)
                                   :date      (parse-git-date (:date commit))
                                   :commit    commit})))))]
      (for [tag (distinct (mapcat :tags tagged))]
        (let [matching (->> tagged
                            (filter #(some #{tag} (:tags %)))
                            (sort-by :date #(compare %2 %1)))
              path     (str tags-prefix "/" tag ".xml")]
          {:path    path
           :content (xml/emit-str
                     (project-feed-xml
                      site
                      (map (fn [{:keys [repo-name slug commit]}]
                             (project-summary-entry-xml domain
                                                        prefix
                                                        repo-name
                                                        slug
                                                        commit))
                           matching)
                      (format-odt (:date (first matching)))
                      path
                      (str (get site :title "Site") " — Projects: " tag)))})))))

(defn- blog-posts? [p] (not (post/post-ref p :projects)))

(defn- cache-dir
  [cfg]
  (or (:cache-dir (config/opts cfg :builders :repo)) ".git-cache"))

(defn- commit-provider
  [dir]
  (fn [p] (repo/repo-commits (str dir "/" (post/post-slug p) ".git"))))

(defn- last-commit-provider
  [dir]
  (fn [p] [(repo/repo-last-commit (str dir "/" (post/post-slug p) ".git"))]))

(defmethod ig/init-key ::builder
  [_ {:keys [config output posts]}]
  (when (config/enabled? config :builders :atom)
    (let [{:keys [prefix project-prefix blog? projects?]
           :or   {prefix "/blog" project-prefix "/projects"
                  blog? true projects? true}} (config/opts config :builders :atom)
          dir  (cache-dir config)
          site (assoc (:site config)
                      :default-metadata (cond-> {}
                                          (get-in config [:site :fullname])
                                          (assoc :author (get-in config [:site :fullname]))
                                          (get-in config [:site :email])
                                          (assoc :email (get-in config [:site :email]))))
          builders (cond-> []
                     blog?
                     (into [(fn [site posts]
                              ((atom-feed :prefix prefix :path "feed.xml")
                               site
                               (filter blog-posts? posts)))
                            (fn [site posts]
                              ((atom-feeds-by-tag :prefix prefix)
                               site
                               (filter blog-posts? posts)))])
                     projects?
                     (into [(project-atom-feed
                             :prefix          project-prefix
                             :commit-provider (last-commit-provider dir))
                            (project-atom-feeds
                             :prefix          project-prefix
                             :commit-provider (commit-provider dir))
                            (project-atom-feeds-by-tag
                             :prefix          project-prefix
                             :commit-provider (last-commit-provider dir))]))]
      (core/write-artifacts! output (map #(% site posts) builders))
      nil)))
