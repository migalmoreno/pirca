(ns pirca.ssg.builders.blog
  "Blog builder: generates individual post pages and collection index pages."
  (:require
   [pirca.ssg.post :as post]))

(defn post-uri
  [prefix p]
  (str prefix "/" (post/post-slug p) ".html"))

(defn- render-post-page
  "Render a single post page using LAYOUT-FN and TEMPLATE-FN."
  [site post layout-fn template-fn]
  (layout-fn site (post/post-title post) (template-fn post)))

(defn- render-collection-page
  "Render a collection index page."
  [site title sorted-posts prefix layout-fn collection-template-fn]
  (layout-fn site
             title
             (collection-template-fn site title sorted-posts prefix)))

(defn blog
  "Return a builder that generates individual post pages and collection indexes.

  Options:
    :prefix              - URL/path prefix, e.g. \"/blog\"
    :layout-fn           - (fn [site title body-hiccup] -> html-string)
    :post-template-fn    - (fn [post] -> hiccup)
    :collection-template-fn - (fn [site title posts prefix] -> hiccup)
    :collections         - seq of {:name \"...\" :path \"...\" :sort-fn f}
                           path is relative to the output dir, e.g. \"blog/index.html\""
  [&
   {:keys [prefix layout-fn post-template-fn collection-template-fn collections]
    :or   {prefix "/blog" collections []}}]
  (fn [site posts]
    (concat
     (map (fn [p]
            {:path    (str (subs prefix 1) "/" (post/post-slug p) ".html")
             :content (render-post-page site p layout-fn post-template-fn)})
          posts)
     (map (fn [{:keys [name path sort-fn]}]
            {:path    path
             :content (render-collection-page site
                                              name
                                              (sort-fn posts)
                                              prefix
                                              layout-fn
                                              collection-template-fn)})
          collections))))

(defn static-page
  "Return a builder that generates a single static page.

  RENDER-FN is (fn [site posts] -> hiccup), LAYOUT-FN wraps it,
  and the result is written to OUTPUT-PATH."
  [output-path title layout-fn render-fn]
  (fn [site posts]
    [{:path    output-path
      :content (layout-fn site title (render-fn site posts))}]))
