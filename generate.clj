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

(->
 "https://24.hu/feed/"
 (curl/get)
 :body
 (xml/parse-str)
 :content
 (->> (some :content))
 (->> (filter (comp #{:item} :tag)))
 (->> (map rss-item))
 (pr-str)

 (->> (spit "docs/index.html")))
