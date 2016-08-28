(ns com.tbaldridge.odin-test
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.contexts.data :as d]
            [clojure.test :refer :all]))


(deftest basic-query-test
  (let [data {:a {:b {:c 1}}
                 :d {:e {:f 2}
                     :g 3}}]
    ;; Test with both pre-indexed and non-pre-indexed data
    (doseq [data [data (d/index-data data)]]
      (is (= (set (o/for-query
                    (d/query data _ :c ?v)
                    ?v))
             #{1}))

      (is (= (set (o/for-query
                    (d/query data _ ?a 1)
                    ?a))
             #{:c}))

      (is (= (set (o/for-query
                    (o/and (d/query data _ _ ?v)
                           (o/pass (integer? ?v)))
                    #{1 2 3}))))

      (is (= (set (o/for-query
                    (d/query data _ ?a _)
                    #{:a :b :c :d :e :f :g})))))))