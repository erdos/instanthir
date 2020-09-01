(require '[babashka.classpath :refer [add-classpath]]
         '[clojure.string :as str]
         '[babashka.curl :as curl]
         )

(def tag-blacklist #{"_444_adomany"})

(defn parse-time [s]
  (some-> s (java.time.ZonedDateTime/parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(defn rss-item [item]
  (let [title (some #(when (= :title (:tag %)) (first (:content %)))
                    (:content item))
        url   (some #(when (= :link (:tag %)) (first (:content %)))
                    (:content item))
        categories (keep #(when (= :category (:tag %)) (first (:content %)))
                          (:content item))]
    {:title title
     :link url
     :published (some-> item :content
                         (->> (some #(when (= :pubDate (:tag %)) (first (:content %)))))
                         (parse-time))
     :tags (vec (remove tag-blacklist categories))}))

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
    (if (map? (second elem))
      (str "<" (name (first elem))
           (apply str (for [[k v] (second elem)]
                        (str \space (name k) \= \" (str v) \")))
           ">"
           (render-html (nnext elem))
           "</" (name (first elem)) ">")
      (str "<" (name (first elem))
           (if (next elem)
             (str ">" (render-html (next elem)) "</" (name (first elem)) ">")
             (str "/>")
             )))

    (sequential? elem)
    (apply str (map render-html elem))

    :else
    (str elem)))


(defmacro defsynonyms [& words])

(defsynonyms "COVID-19" "covid-19" "koronavírus")

(defn template [items]
  [:html
   [:head
    [:title "hirek."]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:style "i {font-size: 0.9em; color: #222}"]]
   [:body
    [:h1 "Hello"]
    [:table
     (for [item (reverse (sort-by :published items))]
       [:tr
        [:td
         [:a {:href (:link item)} (:title item)]
         [:br]
         (when-let [t (:published item)]
           [:span (-> (java.time.format.DateTimeFormatter/ofPattern "HH.mm") (.format t))])
         (for [tag (:tags item)]
           [:i (str tag) ", "] )]
        ]
       )]
    ]])


(->> ["https://24.hu/feed/"
      "https://444.hu/feed/"
      "https://hvg.hu/rss"
      "https://www.valaszonline.hu/feed/"
      "https://hang.hu/feed/"
      "https://kolozsvaros.com/feed/"
      "https://www.napi.hu/feed/mindencikk.xml"
      "https://www.portfolio.hu/rss/all.xml"
      "https://magyarnarancs.hu/rss/"
      ]
     (pmap fetch-items)
     (flatten)
     (template)
     (render-html)
     (spit (doto "docs/index.html" io/make-parents)))
