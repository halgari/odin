(ns odin.unification-test
  (:refer-clojure :exclude [==])
  (:require [odin.unification :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.spec :as s]
            [clojure.test :refer :all]
            [odin.unification :as u]
            [odin.contexts.data :as d]))


(deftest basic-unification
  (are [q result] (= (into [] (for-query q {:x ?x :y ?y})) result)
    (conjunction
      (== ?y 11)
      (== ?x ?y)) [{:x 11 :y 11}]

    (conjunction
      (== ?x 42)
      (== ?y 0)) [{:x 42 :y 0}]

    (disjunction
      (conjunction
        (== ?x 42)
        (== ?y 11))
      (conjunction
        (== ?x 11)
        (== ?y 42))) [{:x 42 :y 11}
                      {:x 11 :y 42}]))


(s/def ::name string?)
(s/def ::id integer?)

(s/def ::person (s/keys :req [::name ::id]))

(s/def ::pet-name string?)
(s/def ::pet (s/keys :req [::id ::pet-name]))

(s/def ::basic-items (s/coll-of (s/or :pet ::pet :person ::person)))

(s/exercise ::basic-items)

(defspec data-tests 100
         (for-all [itms (s/gen ::basic-items)]
                  (into [] (for [[?e ?nm] (into [] (u/for-query
                                                     (d/query itms ?e ::name ?nm)
                                                     [?e ?nm]))]
                             (let [found (get-in itms ?e)]
                               (is (s/valid? ::person found))
                               (is (= (::name found) ?nm)))))))