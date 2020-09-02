(require '[babashka.classpath :refer [add-classpath]]
         '[clojure.string :as str]
         '[babashka.curl :as curl]
         )

(def tag-blacklist #{"_444_adomany"})

(defn parse-time [s]
  (some-> s
          (java.time.ZonedDateTime/parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
          (.withZoneSameInstant (java.time.ZoneId/of "Europe/Budapest"))
          ))

;; set of sets
(def synonyms
  (zipmap
   (repeatedly #(java.util.UUID/randomUUID))
   (map #(vec (.split % ",")) (line-seq (io/reader "synonyms.csv")))))

(defn rss-item [item]
  (let [title (some #(when (= :title (:tag %)) (first (:content %)))
                    (:content item))
        url   (some #(when (= :link (:tag %)) (first (:content %)))
                    (:content item))
        categories (keep #(when (= :category (:tag %)) (first (:content %)))
                         (:content item))
        super-tags (set
                    (some (fn [c] (seq (for [[k sr] synonyms, s sr :when (= s c)] k)))
                          categories))
        super-tags (into super-tags
                         (for [[uuid sr] synonyms, s sr
                               :when (.contains (.toLowerCase title) s)]
                           uuid))]
    {:title title
     :link url
     :published (some-> item :content
                         (->> (some #(when (= :pubDate (:tag %)) (first (:content %)))))
                         (parse-time))
     :tags (vec (remove tag-blacklist categories))
     :super-tags super-tags}))

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
    (str elem)
    ))


(defmacro defsynonyms [& words])

(defsynonyms "COVID-19" "covid-19" "koronavírus")

(defn template [items]
  [:html
   [:head
    [:title "hirek."]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:style
     "i {font-size: 0.9em; color: #222}"
     "label.cat {display:inline-block; margin: 1em; user-select: none; font-family: sans-serif}"
     "label.cat div {padding: 0.5em; background: #eee; border: 1px solid #aaa; text-transform:uppercase}"

     "label.cat input {display:none}"
     "label.cat input + div {background: white}"
     "label.cat input:checked + div {background:#ad4}"
     "label.cat input + div::before {content:'❌ '}"
     "label.cat input:checked + div::before {content:'✓ '}"
     ".hide {display:none}"
     ]]
   [:script
    "function toggle() {
       var id= event.target.parentElement.getAttribute('data-id');
       document.querySelectorAll('td.id-' + id).forEach(function(t) {t.classList.toggle('hide')});

    }"]
   [:body
    (let [cats (into (sorted-set) (mapcat :super-tags items))]
      [:div
       (for [c (sort-by (comp first synonyms) cats)
             :let [label (first (get synonyms c))
                   cnt   (count (filter #(some #{c} (:super-tags %)) items))]]
         [:label {:class "cat" :data-id (str c)}
          [:input {:type "checkbox" :onclick "javascript:toggle()" :checked ""}]
          [:div {:class "button"} (str label) " (" cnt ")"]
          ])
       ]
      )
    [:table
     (for [item (reverse (sort-by :published items))]
       [:tr
        [:td
         (if (:super-tags item)
           {:class (str/join " " (map (fn [s] (str "id-" s)) (:super-tags item)))}
           {})
         [:a {:href (:link item)} (:title item)]
         [:br]
         (when-let [t (:published item)]
           [:span (-> (java.time.format.DateTimeFormatter/ofPattern "HH:mm") (.format t))])
         [:i
          (for [tag (:tags item)]
            (str ", " tag))]
         (when (seq (:super-tags item))
           [:i [:b (for [tag (:super-tags item)
                         :let [[tag-name] (synonyms tag)]]
                     (str ", " tag-name)
                     )]]
           )]])]
    [:div "-"] ;; footer
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
