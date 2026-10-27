(ns pirca.ssg.post
  (:require
   [clojure.string :as str]
   [tick.core :as t]))

(def ^:private fmt-full
  (t/formatter "yyyy-MM-dd EEE HH:mm"))

(def ^:private fmt-short
  (t/formatter "yyyy-MM-dd EEE"))

(def ^:private fmt-display
  (t/formatter "MMMM d, yyyy"))

(defn parse-org-date
  "Parse an org-mode date string like <2023-01-22 Sun 19:13> or <2023-01-22 Sun>."
  [s]
  (let [s (-> s
              (str/replace #"[<>]" "")
              str/trim)]
    (try
      (t/parse-date-time s fmt-full)
      (catch #?(:clj Exception :cljs js/Error) _
        (t/midnight (t/parse-date s fmt-short))))))

(defn format-date
  "Format a date-time for human display."
  [date]
  (t/format fmt-display date))

(defn ->slug [s] (str/replace s "." "-"))

(defn post-title [post] (:title post))
(defn post-date [post] (:date post))
(defn post-slug [post] (:slug post))
(defn post-content [post] (:content post))
(defn post-ref [post key] (get (:metadata post) key))
(defn post-tags [post] (get (:metadata post) :tags []))

(defn make-post
  "Construct a post map from a slug, parsed metadata map, and raw HTML content string."
  [slug metadata content]
  {:slug     slug
   :title    (:title metadata "Untitled")
   :date     (:date metadata)
   :content  content
   :metadata (dissoc metadata :title :date)})

(defn posts-reverse-chronological
  "Sort posts newest-first."
  [posts]
  (sort-by :date #(compare %2 %1) posts))
