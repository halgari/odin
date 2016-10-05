(ns com.tbaldridge.odin-test
  (:refer-clojure :exclude [ancestors])
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.contexts.data :as d]
            [clojure.test :refer :all]))


(deftest basic-query-test
  (let [data {:a {:b {:c 1}}
                 :d {:e {:f 2}
                     :g 3}}]
    ;; Test with both pre-indexed and non-pre-indexed data
    (doseq [data [#_data (d/index-data data)]]
      (binding [u/*query-ctx* {::u/fn u/println-tracing-reporter}]
        (is (= (set (o/for-query
                      (d/query data _ :c ?v)
                      ?v))
               #{1})))

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

(o/defrule parent [data ?parent ?child]
  (o/and
    (d/query data ?cid :name ?child)
    (d/query-in data ?cid [:parents _] ?parent)))

(o/defrule ancestors [data ?ancestor ?child]
  (o/or
    (parent data ?ancestor ?child)

    (o/and
      (parent data ?tp ?child)
      (o/lazy-rule (ancestors data ?ancestor ?tp)))))

(deftest rules-test
  (let [data [{:name :Bill :parents [:Sam :Jane]}
              {:name :Tom :parents [:Jane :Sam]}
              {:name :Sam :parents [:Edward :Erin]}
              {:name :Jane :parents [:Jack :Rose]}]]

    (is (= (set (o/for-query
                  (parent data ?parent :Bill)
                  ?parent))
           #{:Sam :Jane}))

    (is (= (set (o/for-query
                  (parent data ?parent :Sam)
                  ?parent))
           #{:Edward :Erin}))


    (is (= (set (o/for-query
                  (ancestors data ?a :Bill)
                  ?a))
           #{:Edward :Erin :Jack :Rose :Sam :Jane}))))






(o/defrule link [data ?from ?to]
  (o/and
    (o/log "Link " ?from ?to)
    (d/query data ?node 0 ?from)
    (d/query data ?node 1 ?to)))

(o/defrule ^:tabled calls [data ?from ?to]
  (o/and
    (o/or
      (link data ?from ?to)

      (o/and
          (link data ?from ?n)
          (o/lazy-rule (calls data ?n ?to))))))




(deftest tabling-test
  (let [link-data [[:a :b]
                   [:b :c]
                   [:c :d]
                   [:d :a]]]
    (is (= (set (o/for-query
                  (link link-data :a ?to)
                  ?to))
           #{:b}))

    (is (= (set (o/for-query
                  (calls link-data :a ?calls)
                  ?calls))
           #{:a :b :c :d}))



    (testing "tabled results triggered by other results are fully processed"
      (o/with-query-ctx
        (doseq [start [:c :d :a :b]]
          (is (= (set (o/for-query
                        (calls link-data start ?calls)
                        ?calls))
                 #{:a :b :c :d}))))))

  (testing "complex graphs"
    (let [link-data [[:a :b]
                     [:b :c]
                     [:c :d]
                     [:d :b]
                     [:d :e]
                     [:e :c]
                     [:f :g]
                     [:g :h]
                     [:h :f]]]
      (is (= (set (o/for-query
                    (calls link-data :a ?calls)
                    ?calls))
             #{:b :c :d :e}))

      (is (= (set (o/for-query
                    (calls link-data :c ?calls)
                    ?calls))
             #{:b :c :d :e}))

      (is (= (set (o/for-query
                    (calls link-data :f ?calls)
                    ?calls))
             #{:g :h :f}))

      (testing "tabled results work for separate graphs"
        ;; If we marked all calls to 'calls' tabled after the first query
        ;; the second here would not complete
        (o/with-query-ctx
          (is (= (set (o/for-query
                        (calls link-data :c ?calls)
                        ?calls))
                 #{:b :c :d :e}))

          (is (= (set (o/for-query
                        (calls link-data :f ?calls)
                        ?calls))
                 #{:g :h :f}))))



      )))