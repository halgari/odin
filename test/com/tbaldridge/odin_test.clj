(ns com.tbaldridge.odin-test
  (:refer-clojure :exclude [ancestors])
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.contexts.data :as d]
            [clojure.test :refer [deftest is testing]]))


(deftest basic-query-test
  (let [data {:a {:b {:c 1}}
              :d {:e {:f 2}
                  :g 3}}]
    ;; Test with both pre-indexed and non-pre-indexed data
    (doseq [data [data]]
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
                           (o/when (integer? ?v)))
                    ?v))
             #{1 2 3}))

      (is (= (set (o/for-query
                    (d/query data _ ?a _)
                    ?a))
             #{:a :b :c :d :e :f :g})))))

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


(deftest transform-tests
  (let [data {:a 1 :b 2}]
    (is (= {:a 2 :b 3}
           (o/transform data
             (o/and
               (d/query data ?p ?a ?v)
               (o/when (integer? ?v))
               (o/update ?p ?a))
             inc))))

  (let [data [{:name :Bill :parents [:Sam :Jane]}
              {:name :Tom :parents [:Jane :Sam]}
              {:name :Sam :parents [:Edward :Erin]}
              {:name :Jane :parents [:Jack :Rose]}]]
    (is (= (o/transform
             data
             (o/and
               (d/query data ?person :name ?name)
               (d/query-in data ?child [:parents _] ?name)
               (d/query data ?child :name ?child-name)
               (o/update ?person :children))
             (fnil conj #{})
             ?child-name)

           [{:name    :Bill
             :parents [:Sam :Jane]}
            {:name    :Tom
             :parents [:Jane :Sam]}
            {:name     :Sam
             :children #{:Bill :Tom}
             :parents  [:Edward :Erin]}
            {:name     :Jane
             :children #{:Bill :Tom}
             :parents  [:Jack :Rose]}]))))


(deftest projection-tests
  (is (= (set (o/for-query
                (o/project
                  1 ?b)
                ?b))
         #{1}))

  (is (= (set (o/for-query
                (o/project
                  (range 10) [?x ...])
                ?x))
         (set (range 10))))

  (is (= (set (o/for-query
                (o/project
                  [1 2 3] [?v ...]
                  (* ?v ?v) ?r)
                ?r))
         #{1 4 9}))

  (testing "binding destructuring with simple binding"
    (is (= (set (o/for-query
                  (o/project
                    [1 2] [?a ?b]
                    (- ?b ?a) ?c)
                  ?c))
           #{1})))

  (testing "binding destructuring with cat"
    (is (= (set (o/for-query
                  (o/project
                    (eduction
                      (map (juxt dec inc))
                      (range 4)) [[?a ?b] ...]
                    (- ?b ?a) ?c)
                  ?c))
           #{2})))
  )

(o/defrule add-dispatch [?a ?b ?c]
  (o/switch
    [?a ?b _] (o/project
                (+ ?a ?b) ?c)
    [?a _ ?c] (o/project
                (- ?c ?a) ?b)
    [_ ?b ?c] (o/project
                (- ?c ?b) ?a)
    [?a ?b ?c] (o/when
                 (= (+ ?a ?b) ?c))))

(deftest test-switch
  (is (= (set (o/for-query
                (o/and
                  (o/project
                    (range 3) [?x ...])
                  (add-dispatch ?x 1 ?y))
                ?y))
         #{1 2 3}))

  (is (= (set (o/for-query
                (o/and
                  (o/project
                    (range 3) [?y ...])
                  (add-dispatch ?x 1 ?y))
                ?x))
         #{-1 0 1}))

  (is (= (set (o/for-query
                (o/and
                  (o/project
                    (range 1 4) [?y ...])
                  (o/project
                    (range 0 3) [?x ...])
                  (add-dispatch ?x 1 ?y))
                ?x))
         #{0 1 2}))

  (is (= (set (o/for-query
                (o/and
                  (o/= ?x 1)
                  (o/project
                    (range 0 3) [?y ...])
                  (add-dispatch ?x ?z ?y))
                ?z))
         #{0 1 -1})))


(deftest parent-of-tests
  (let [data {:depth 1
              :a {:depth 2
                  :b {:depth 3
                      :c {:depth 4}}}}]
    (is (= (set (o/for-query
                  (o/and
                    (d/query data ?c :depth 4)
                    (d/parent-of data ?p ?c)
                    (d/query data ?p :depth ?pd))
                  ?pd))
           #{1 2 3}))

    (is (= (set (o/for-query
                  (o/and
                    (d/query data ?c :depth 2)
                    (d/parent-of data ?p ?c)
                    (d/query data ?p :depth ?pd))
                  ?pd))
           #{1}))

    (is (= (set (o/for-query
                  (o/and
                    (d/query data ?p :depth 2)
                    (d/parent-of data ?p ?c)
                    (d/query data ?c :depth ?pd))
                  ?pd))
           #{3 4}))

    (is (= (set (o/for-query
                  (o/and
                    (d/query data ?p :depth 2)
                    (d/query data ?c :depth 3)
                    (d/parent-of data ?p ?c)
                    (o/= ?pd 42))
                  ?pd))
           #{42}))))



(deftest context-tests
  (testing "basic context usage"
    (is (= (set (o/for-query
                  (o/and
                    (o/update-local-cache ::test conj 1)
                    (o/update-local-cache ::test conj 2)
                    (o/get-local-cache ::test ?result))
                  ?result))
           #{'(2 1)})))

  (testing "values can be walked"
    (is (= (set (o/for-query
                  (o/and
                    (o/project
                      (range 4) [?data ...])
                    (o/update-local-cache ::test conj ?data)
                    (o/get-local-cache ::test ?result))
                  ?result))
           #{'(0) '(1) '(2) '(3)})))

  (testing "results are isolated"
    (is (= (set (o/for-query
                  (o/and
                    (o/or
                      (o/update-local-cache ::test conj 1)
                      (o/update-local-cache ::test conj 2))
                    (o/get-local-cache ::test ?result))
                  ?result))
           #{'(2) '(1)})))

  (testing "backtracking removes cached results"
    (is (= (set (o/for-query
                  (o/and
                    (o/or
                      (o/and
                        (o/update-local-cache ::test conj 1)
                        o/fail)
                      (o/update-local-cache ::test conj 2))
                    (o/get-local-cache ::test ?result))
                  ?result))
           #{'(2)})))


  )