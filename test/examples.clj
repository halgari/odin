 (ns examples
   (:require [com.tbaldridge.odin :as o]
             [com.tbaldridge.odin.unification :as u]
             [com.tbaldridge.odin.contexts.data :as d]
             [clojure.test :refer [deftest is testing]]))


(deftest basic-example
    (let [orders [{:customer/name "Sam"
                   :customer/id   3411}
                  {:customer/id 3411
                   :order/items {1212 3}}
                  {:customer/id 3411
                   :order/items {2232 2 4242 3}}
                  {:item/id    1212
                   :item/price 40.0}
                  {:item/id    2232
                   :item/price 100}
                  {:item/id    4242
                   :item/price 1.99}]]
      (->> (o/for-query
             (o/and
               (d/query orders ?customer :customer/name "Sam")
               (d/query orders ?customer :customer/id ?id)
               (d/query orders ?order :customer/id ?id)
               (d/query-in orders ?order [:order/items ?item-id] ?qty)
               (d/query orders ?item :item/id ?item-id)
               (d/query orders ?item :item/price ?price))
             (* ?qty ?price))
           (reduce + 0))))

;; => 325.97