(ns pirca.ssg.frontend.repo
  (:require
   ["prismjs" :as Prism]
   ["prismjs/plugins/line-numbers/prism-line-numbers"]
   ["prismjs/components/prism-clojure"]
   ["prismjs/components/prism-nix"]
   ["prismjs/components/prism-bash"]
   ["prismjs/components/prism-python"]
   ["prismjs/components/prism-ruby"]
   ["prismjs/components/prism-rust"]
   ["prismjs/components/prism-go"]
   ["prismjs/components/prism-yaml"]
   ["prismjs/components/prism-json"]
   ["prismjs/components/prism-typescript"]
   ["prismjs/components/prism-jsx"]
   ["prismjs/components/prism-tsx"]
   ["prismjs/components/prism-lisp"]
   ["prismjs/components/prism-scheme"]
   ["isomorphic-git" :as git]
   ["diff" :as Diff]
   ["@isomorphic-git/lightning-fs" :as LightningFS]
   [clojure.string :as str]
   [reagent.core :as r]
   [reagent.dom.client :as rdc]))

(def ^:private default-config
  {:lang-aliases {"clj"  "clojure"
                  "cljs" "clojure"
                  "cljc" "clojure"
                  "edn"  "clojure"
                  "sh"   "bash"
                  "yml"  "yaml"
                  "el"   "lisp"
                  "scm"  "scheme"
                  "ts"   "typescript"}})

(defn ^:private config
  []
  (merge default-config
         (when-let [c (.-pircaConfig js/window)] (js->clj c))))

(defn- set-view!
  "Announce the viewer's current view to the host page. Sets
  data-pirca-view on <html> and dispatches a `pirca:view-change` event whose
  detail is {:view VIEW}. The host decides what to do with it (e.g. hide a
  navbar); pirca itself has no navigation behaviour."
  [view]
  (set! (.. js/document -documentElement -dataset -pircaView) (name view))
  (.dispatchEvent js/document
                  (js/CustomEvent. "pirca:view-change"
                                   #js {:detail #js {:view (name view)}})))

(defonce _aliases
  (let [langs (.-languages Prism)]
    (doseq [[alias target] (:lang-aliases (config))]
      (aset langs alias (aget langs target)))))

(defn- clone-url
  [display-url copy-url]
  (let [copied? (r/atom false)]
    (fn []
      [:div.clone-url
       [:span.clone-url__protocol "HTTPS"]
       [:input.clone-url__input
        {:type      "text"
         :read-only true
         :value     display-url
         :size      (count display-url)}]
       [:button.clone-url__copy
        {:on-click (fn []
                     (-> js/navigator
                         .-clipboard
                         (.writeText copy-url))
                     (reset! copied? true)
                     (js/setTimeout #(reset! copied? false) 2000))}
        (if @copied? "Copied!" "Copy")]])))

(defn- file-ext
  [path]
  (let [parts (str/split path #"\.")]
    (if (> (count parts) 1) (last parts) "text")))

(defn- paths->tree
  [paths]
  (reduce (fn [tree path]
            (assoc-in tree (str/split path #"/") nil))
          {}
          paths))

(defn- code-block
  [content ext]
  [:pre.line-numbers
   [:code
    {:class (str "language-" ext)
     :ref   #(when % (.highlightElement Prism %))}
    content]])

(defn- file-tree-node
  [node-name node-children full-path on-select open-dirs toggle-dir!]
  (if node-children
    [:li.file-tree__dir
     [:details
      {:open      (contains? open-dirs full-path)
       :on-toggle (fn [e] (toggle-dir! full-path (.-open (.-currentTarget e))))}
      [:summary node-name]
      [:ul.file-tree
       (for [[child-name child-children] (sort-by (fn [[k v]] [(if v 0 1) k])
                                                  node-children)]
         ^{:key child-name}
         [file-tree-node child-name child-children
          (str full-path "/" child-name) on-select open-dirs toggle-dir!])]]]
    [:li.file-tree__file
     [:a
      {:href     (str "#" full-path)
       :on-click (fn [e]
                   (.preventDefault e)
                   (on-select full-path))}
      node-name]]))

(defn- file-tree
  [tree on-select open-dirs toggle-dir!]
  [:ul.file-tree
   (for [[node-name node-children] (sort-by (fn [[k v]] [(if v 0 1) k]) tree)]
     ^{:key node-name}
     [file-tree-node node-name node-children node-name on-select open-dirs
      toggle-dir!])])

(defn- fetch-text!
  ([url] (fetch-text! url nil))
  ([url opts]
   (-> (js/fetch url (or opts #js {}))
       (.then #(.text %)))))

(defn- fetch-bytes!
  [url]
  (-> (js/fetch url)
      (.then #(.arrayBuffer %))
      (.then #(js/Uint8Array. %))))

(defn- populate-repo!
  [fs dir url]
  (let [pfs    (.-promises fs)
        mkdir! #(-> (.mkdir pfs %)
                    (.catch (constantly nil)))
        nc     #js {:cache "no-store"}
        t      (js/Date.now)]
    (->
     (mkdir! dir)
     (.then #(mkdir! (str dir "/refs")))
     (.then #(mkdir! (str dir "/info")))
     (.then #(mkdir! (str dir "/objects")))
     (.then #(js/Promise.all
              #js [(mkdir! (str dir "/refs/heads"))
                   (mkdir! (str dir "/objects/info"))
                   (mkdir! (str dir "/objects/pack"))]))
     (.then #(js/Promise.all
              #js [(fetch-text! (str url "/HEAD?t=" t) nc)
                   (fetch-text! (str url "/info/refs?t=" t) nc)
                   (fetch-text! (str url "/objects/info/packs?t=" t) nc)]))
     (.then
      (fn [[head refs-text packs-text]]
        (let [branch  (-> head
                          str/trim
                          (str/replace #"^ref: refs/heads/" ""))
              ref-map (->> (str/split refs-text #"\r?\n")
                           (filter seq)
                           (keep (fn [line]
                                   (let [[sha ref] (str/split line #"\t" 2)]
                                     (when (and sha ref)
                                       [(str/trim ref) (str/trim sha)]))))
                           (into {}))
              sha     (or (get ref-map (str "refs/heads/" branch))
                          (get ref-map "HEAD"))
              packs   (->> (str/split packs-text #"\n")
                           (filter #(str/starts-with? % "P "))
                           (map #(str/trim (subs % 2))))
              dl!     (fn [path remote]
                        (-> (fetch-bytes! remote)
                            (.then #(.writeFile pfs path %))))]
          (js/Promise.all
           (clj->js
            (concat
             [(.writeFile pfs (str dir "/HEAD") head)
              (.writeFile pfs (str dir "/refs/heads/" branch) (str sha "\n"))]
             (mapcat (fn [pack]
                       (let [idx  (str/replace pack #"\.pack$" ".idx")
                             base (str url "/objects/pack/")]
                         [(dl! (str dir "/objects/pack/" pack) (str base pack))
                          (dl! (str dir "/objects/pack/" idx)
                               (str base idx))]))
                     packs))))))))))

(defn- format-bytes
  [n]
  (when n
    (cond
      (< n 1024)          (str n " B")
      (< n (* 1024 1024)) (str (Math/round (/ n 1024)) " KB")
      :else               (str (.toFixed (/ n (* 1024 1024)) 1) " MB"))))

(defn- relative-time
  [timestamp-secs]
  (let [diff (/ (- (js/Date.now) (* timestamp-secs 1000)) 1000)
        mins (/ diff 60)
        hrs  (/ mins 60)
        days (/ hrs 24)]
    (cond
      (< mins 1)   "just now"
      (< hrs 1)    (str (Math/floor mins) " minutes ago")
      (< days 1)   (str (Math/floor hrs) " hours ago")
      (< days 30)  (str (Math/floor days) " days ago")
      (< days 365) (str (Math/floor (/ days 30)) " months ago")
      :else        (str (Math/floor (/ days 365)) " years ago"))))

(defn- commit-bar
  [commits commit-count on-show-history]
  (let [c (first commits)]
    [:div.commit-bar
     (if c
       (let [sha (subs (.-oid c) 0 7)
             msg (first (str/split-lines (.. c -commit -message)))
             ts  (.. c -commit -author -timestamp)]
         [:<>
          [:code.commit-bar__sha sha]
          [:span.commit-bar__message msg]
          [:span.commit-bar__time (relative-time ts)]])
       [:span.commit-bar__message "\u2026"])
     [:button.commit-bar__history
      {:on-click on-show-history
       :disabled (nil? commits)}
      (if commit-count (str commit-count " commits") "\u2026")]]))

(defn- load-more-sentinel
  [on-load-more]
  (let [observer (atom nil)]
    (r/create-class
     {:component-will-unmount #(when @observer (.disconnect @observer))
      :reagent-render
      (fn []
        [:div.commit-list__sentinel
         {:ref (fn [el]
                 (when @observer (.disconnect @observer))
                 (when el
                   (let [obs (js/IntersectionObserver.
                              (fn [entries]
                                (when (.-isIntersecting (aget entries 0))
                                  (on-load-more)))
                              #js {:rootMargin "200px"})]
                     (.observe obs el)
                     (reset! observer obs))))}])})))

(defn- commit-list
  [commits commit-count loading-more? on-select on-back on-load-more]
  (let [has-more (< (count commits) commit-count)]
    [:div.commit-list
     [:div.commit-list__header
      [:a {:href "#" :on-click (fn [e] (.preventDefault e) (on-back))}
       "\u2190 Files"]]
     [:ul.commit-list__entries
      (for [c commits]
        (let [sha (subs (.-oid c) 0 7)
              msg (first (str/split-lines (.. c -commit -message)))
              ts  (.. c -commit -author -timestamp)]
          ^{:key sha}
          [:li.commit-list__entry
           {:on-click #(on-select (.-oid c))}
           [:code.commit-list__sha sha]
           [:span.commit-list__message msg]
           [:span.commit-list__time (relative-time ts)]]))]
     (when has-more
       (if loading-more?
         [:p.commit-list__loading "\u2026"]
         [load-more-sentinel on-load-more]))]))

(defn- diff-chunk-view
  [chunks]
  [:div.diff-view
   (map-indexed
    (fn [ci chunk]
      (let [added?   (.-added chunk)
            removed? (.-removed chunk)
            lines    (str/split (.-value chunk) #"\n")]
        ^{:key ci}
        [:div
         (map-indexed
          (fn [li line]
            (when-not (and (= line "") (= li (dec (count lines))))
              ^{:key li}
              [:div
               {:class (cond added?   "diff-line diff-line--added"
                             removed? "diff-line diff-line--removed"
                             :else    "diff-line diff-line--context")}
               [:span.diff-line__prefix
                (cond added?   "+"
                      removed? "-"
                      :else    " ")]
               [:code.diff-line__content line]]))
          lines)]))
    chunks)])

(defn- commit-detail
  [commits selected-oid file-diffs expanded-files on-back on-toggle-file]
  (let [commit  (first (filter #(= (.-oid %) selected-oid) commits))
        sha     (when commit (subs selected-oid 0 7))
        msg     (when commit (str/trim (.. commit -commit -message)))
        changes (when commit
                  (some-> (.. commit -commit -changes)
                          array-seq))]
    [:div.commit-detail
     [:div.commit-detail__header
      [:a {:href "#" :on-click (fn [e] (.preventDefault e) (on-back))}
       "\u2190 History"]
      (when sha [:code.commit-detail__sha sha])
      (when msg [:span.commit-detail__message msg])]
     (if (seq changes)
       [:ul.commit-detail__files
        (for [ch   changes
              :let [b-oid    (aget ch 0)
                    a-oid    (aget ch 1)
                    path     (aget ch 2)
                    status   (cond (nil? b-oid) "deleted"
                                   (nil? a-oid) "added"
                                   :else        "modified")
                    diff     (get file-diffs path)
                    expanded (contains? expanded-files path)]]
          ^{:key path}
          [:li.commit-detail__file
           [:div.commit-detail__file-header
            {:on-click #(on-toggle-file path)}
            [:span {:class (str "commit-detail__file-status--" status)} status]
            [:code.commit-detail__file-path path]
            (cond
              (= diff :loading)
              [:span.commit-detail__diff-loading "\u2026"]
              (:error diff)
              [:span.commit-detail__diff-error "error"]
              (:binary diff)
              (when (or (:old-size diff) (:new-size diff))
                [:span.commit-detail__diff-stat
                 (format-bytes (:old-size diff)) " \u2192 "
                 (format-bytes (:new-size diff))])
              diff
              [:span.commit-detail__diff-stat
               [:span.diff-stat__added (str "+" (:added diff))]
               " "
               [:span.diff-stat__removed (str "-" (:removed diff))]])]
           (when expanded
             (if (:binary diff)
               [:p.commit-detail__empty "Binary file not shown"]
               (when (:chunks diff)
                 [diff-chunk-view (:chunks diff)])))])]
       [:p.commit-detail__empty "No file changes"])]))

(defn- file-viewer
  [git-url]
  (let [fs-name (-> git-url
                    (str/split #"/")
                    last
                    (str/replace #"\.git$" ""))
        fs      (LightningFS. fs-name)
        dir     "/repo"
        state   (r/atom
                 {:status         :loading
                  :tree           nil
                  :commits        nil
                  :commit-count   nil
                  :page-depth     50
                  :loading-more   false
                  :branch         "HEAD"
                  :view           :tree
                  :selected-oid   nil
                  :file-diffs     {}
                  :expanded-files #{}
                  :path           nil
                  :content        nil
                  :error          nil
                  :open-dirs      #{}})]
    (-> (populate-repo! fs dir git-url)
        (.then
         (fn [_]
           (-> (git/currentBranch #js {:fs fs :dir dir :gitdir dir})
               (.then #(when % (swap! state assoc :branch %)))
               (.catch (constantly nil)))
           (-> (git/log #js {:fs fs :dir dir :gitdir dir :ref "HEAD"})
               (.then #(swap! state assoc :commit-count (.-length %)))
               (.catch (constantly nil)))
           (-> (git/log #js {:fs             fs
                             :dir            dir
                             :gitdir         dir
                             :ref            "HEAD"
                             :depth          50
                             :includeChanges true})
               (.then #(swap! state assoc :commits (array-seq %)))
               (.catch (constantly nil)))
           (-> (git/listFiles #js {:fs fs :dir dir :gitdir dir :ref "HEAD"})
               (.then (fn [files]
                        (swap! state assoc
                               :status :ready
                               :tree   (paths->tree files)))))))
        (.catch (fn [e]
                  (swap! state assoc :status :error :error (.-message e)))))
    (letfn
     [(load-more-commits! []
        (when-not (:loading-more @state)
          (let [new-depth (+ (:page-depth @state) 50)]
            (swap! state assoc :loading-more true)
            (-> (git/log #js {:fs             fs
                              :dir            dir
                              :gitdir         dir
                              :ref            "HEAD"
                              :depth          new-depth
                              :includeChanges true})
                (.then (fn [result]
                         (swap! state assoc
                                :commits      (array-seq result)
                                :page-depth   new-depth
                                :loading-more false)))
                (.catch (fn [_]
                          (swap! state assoc :loading-more false)))))))
      (select-file! [path]
        (when (nil? (:path @state))
          (.pushState js/history
                      nil
                      ""
                      (str "#" (:branch @state) "/tree/" path))
          (when-let [el (.querySelector js/document "[data-git-viewer]")]
            (.scrollIntoView el #js {:block "start" :behavior "instant"}))
          (set-view! :file)
          (when-let [container (.querySelector js/document
                                               "[data-page-content]")]
            (set! (.-display (.-style container)) "none")))
        (swap! state assoc :path path :content nil :error nil)
        (-> (git/resolveRef #js {:fs fs :dir dir :gitdir dir :ref "HEAD"})
            (.then
             (fn [oid]
               (git/readObject
                #js
                 {:fs fs :dir dir :gitdir dir :oid oid :filepath path})))
            (.then (fn [result]
                     (swap! state assoc
                            :content
                            (.decode (js/TextDecoder.) (.-object result)))))
            (.catch (fn [e]
                      (swap! state assoc :error (.-message e))))))
      (toggle-dir! [dir-path open?]
        (swap! state update :open-dirs (if open? conj disj) dir-path))
      (load-file-diff! [path b-oid a-oid]
        (swap! state assoc-in [:file-diffs path] :loading)
        (let [read-blob!
              (fn [oid]
                (if oid
                  (-> (git/readBlob #js {:fs fs :dir dir :gitdir dir :oid oid})
                      (.then (fn [result]
                               (let [blob (.-blob result)
                                     size (.-length blob)]
                                 (if (or (> size 500000)
                                         (not= -1 (.indexOf blob 0)))
                                   {:binary true :size size}
                                   (.decode (js/TextDecoder.) blob))))))
                  (js/Promise.resolve "")))]
          (-> (js/Promise.all #js [(read-blob! a-oid) (read-blob! b-oid)])
              (.then
               (fn [[old-str new-str]]
                 (if (or (map? old-str) (map? new-str))
                   (swap! state assoc-in
                          [:file-diffs path]
                          {:binary   true
                           :old-size (when (map? old-str) (:size old-str))
                           :new-size (when (map? new-str) (:size new-str))})
                   (let [chunks  (array-seq (.diffLines Diff old-str new-str))
                         added   (reduce +
                                         (keep #(when (.-added %) (.-count %))
                                               chunks))
                         removed (reduce +
                                         (keep #(when (.-removed %)
                                                  (.-count %))
                                               chunks))]
                     (swap! state assoc-in
                            [:file-diffs path]
                            {:chunks chunks :added added :removed removed})))))
              (.catch (fn [e]
                        (swap! state assoc-in
                               [:file-diffs path]
                               {:error (.-message e)}))))))
      (toggle-file-diff! [path]
        (swap! state update
               :expanded-files
               (fn [s] (if (contains? s path) (disj s path) (conj s path)))))
      (set-container-visible! [visible?]
        (when-let [container (.querySelector js/document
                                             "[data-page-content]")]
          (set! (.-display (.-style container)) (if visible? "" "none"))))
      (show-history! []
        (set-container-visible! false)
        (.pushState js/history nil "" (str "#commits/" (:branch @state)))
        (swap! state assoc :view :commits))
      (show-tree! []
        (.back js/history))
      (show-commit! [oid]
        (let [commit  (first (filter #(= (.-oid %) oid) (:commits @state)))
              changes (some-> (.. commit -commit -changes)
                              array-seq)
              paths   (set (map #(aget % 2) changes))]
          (.pushState js/history nil "" (str "#commit/" oid))
          (swap! state assoc
                 :view           :commit-detail
                 :selected-oid   oid
                 :file-diffs     {}
                 :expanded-files paths)
          (doseq [ch   changes
                  :let [b-oid (aget ch 0)
                        a-oid (aget ch 1)
                        path  (aget ch 2)]]
            (load-file-diff! path b-oid a-oid))))
      (handle-popstate! []
        (let [hash (.-hash js/location)]
          (cond
            (str/starts-with? hash "#commits/")
            (do (set-container-visible! false)
                (swap! state assoc
                       :view    :commits
                       :path    nil
                       :content nil
                       :error   nil)
                (set-view! :commits))

            (str/starts-with? hash "#commit/")
            (let [oid     (subs hash (count "#commit/"))
                  commit  (first (filter #(= (.-oid %) oid) (:commits @state)))
                  changes (some-> (.. commit -commit -changes)
                                  array-seq)
                  paths   (set (map #(aget % 2) changes))]
              (set-container-visible! false)
              (swap! state assoc
                     :view           :commit-detail
                     :selected-oid   oid
                     :file-diffs     {}
                     :expanded-files paths
                     :path           nil
                     :content        nil
                     :error          nil)
              (set-view! :commit-detail)
              (doseq [ch   changes
                      :let [b-oid (aget ch 0)
                            a-oid (aget ch 1)
                            path  (aget ch 2)]]
                (load-file-diff! path b-oid a-oid)))

            :else
            (do
              (set-container-visible! true)
              (swap! state assoc :view :tree :path nil :content nil :error nil)
              (set-view! :tree)))))]
      (.addEventListener js/window "popstate" handle-popstate!)
      (fn []
        (let [{:keys [status tree commits commit-count loading-more branch view
                      selected-oid file-diffs expanded-files
                      path content error open-dirs]}
              @state]
          [:div.file-viewer
           (case status
             :loading [:p "Loading\u2026"]
             :error [:p "Error: " error]
             :ready
             (if path
               [:div.file-view
                [:div.file-view__header
                 [:a
                  {:href     "#"
                   :on-click (fn [e]
                               (.preventDefault e)
                               (.back js/history))}
                  "\u2190"]
                 [:span " " path]]
                (cond
                  error   [:p "Error: " error]
                  content [code-block content (file-ext path)]
                  :else   [:p "Loading\u2026"])]
               [:div
                [commit-bar commits commit-count show-history!]
                (case view
                  :commits
                  [commit-list commits commit-count loading-more
                   show-commit! show-tree! load-more-commits!]
                  :commit-detail
                  [commit-detail commits selected-oid file-diffs expanded-files
                   #(.back js/history) toggle-file-diff!]
                  [file-tree tree select-file! open-dirs toggle-dir!])]))])))))

(defonce clone-url-root
  (when-let [el (.querySelector js/document "[data-clone-url]")]
    (rdc/create-root el)))

(defonce clone-url-data
  (when-let [el (.querySelector js/document "[data-clone-url]")]
    {:display (.getAttribute el "data-display-url")
     :copy    (.getAttribute el "data-clone-url")}))

(defonce file-viewer-root
  (when-let [el (.querySelector js/document "[data-git-viewer]")]
    (rdc/create-root el)))

(defonce git-url
  (some-> (.querySelector js/document "[data-git-viewer]")
          (.getAttribute "data-git-url")
          (->> (str (.-origin js/location)))))

(defn ^:dev/after-load mount-root
  []
  (.highlightAll Prism)
  (when clone-url-root
    (rdc/render clone-url-root
                [clone-url (:display clone-url-data) (:copy clone-url-data)]))
  (when file-viewer-root
    (rdc/render file-viewer-root [file-viewer git-url])
    (set-view! :tree)))
