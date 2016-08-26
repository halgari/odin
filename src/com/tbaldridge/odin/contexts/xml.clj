(ns com.tbaldridge.odin.contexts.xml
  (:require [com.tbaldridge.odin.contexts.data :as d]
            [com.tbaldridge.odin :as o]
            [clojure.data.xml :as xml]))


(xml/parse-str "<h1><div>42</div></h1>")


(defn tag [src path t]
  (d/query src path :tag t))

(defn content [src path content]
  (d/query-in src path [:content 0] content))


(defn tag-content [src path t c]
  (o/and
    (tag src path t)
    (content src path c)))


(o/defrule tag-content-child [?src ?p ?tag ?c]
  (o/and
    (d/parent-of ?src ?p ?child)
    (tag-content ?src ?child ?tag ?c)))


(def data (xml/parse-str (time (slurp "https://api.eve-central.com/api/quicklook?typeid=34"))))





(let [
      ]
  (count (time (transduce
                 identity
                 conj
                 (o/for-query
                   (o/and
                     (tag data ?order :order)
                     (tag-content-child data ?order :station ?station-id)
                     (tag-content-child data ?order :station_name ?station-name)
                     (tag-content-child data ?order :vol_remain ?vol-remain))
                   {:station-id   (Long/parseLong ?station-id)
                    :station-name ?station-name
                    :vol-remain   (Long/parseLong ?vol-remain)})))))

