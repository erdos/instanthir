(require '[babashka.classpath :refer [add-classpath]]
         '[clojure.string :as str]
         '[babashka.curl :as curl]
         )

(defn rss-item [item]
  (let [title (some #(when (= :title (:tag %)) (first (:content %)))
                    (:content item))
        url   (some #(when (= :link (:tag %)) (first (:content %)))
                    (:content item))
        categories (keep #(when (= :category (:tag %)) (first (:content %)))
                          (:content item))]
    {:title title
     :url url
     :tags (vec categories)}))

(defn fetch-items [url]
  (->
   url
   (curl/get)
   :body
   (xml/parse-str)
   :content
   (->> (some :content))
   (->> (filter (comp #{:item} :tag)))
   (->> (map rss-item))))

(defn render-html [elem]
  (cond
    (vector? elem)
    (str "<" (name (first elem)) ">" (apply str (map render-html (next elem))) "</" (name (first elem)) ">")

    (sequential? elem)
    (apply str (map render-html elem))

    :else
    (str elem)))

(defn template [items]
  [:html
   [:body
    [:h1 "Hello"]
    [:table
     (for [item items]
       [:tr
        [:td (:title item)]
        ]
       )]
    ]])



(->
 (concat
  (fetch-items "https://24.hu/feed/")
  (fetch-items "https://444.hu/feed/")
  (fetch-items "https://hvg.hu/rss"))
 (template)
 (render-html)
 (->> (spit "docs/index.html"))
 )
