(ns com.tbaldridge.contexts.data-test
  (:require [clojure.test :refer :all]
            [com.tbaldridge.odin.contexts.data :as d]
            [clojure.spec :as s]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.tbaldridge.odin :as o]))


(s/def ::data
  (s/or
    :int integer?
    :kw keyword?
    :string string?
    :map (s/map-of keyword? ::data :min-count 1)
    :sequence (s/coll-of ::data :into '() :min-count 1)
    :vector (s/coll-of ::data :into [] :min-count 1)))

(s/def ::coll-data
  (s/or
    :map (s/map-of keyword? ::data :min-count 1)
    :sequence (s/coll-of ::data :into '() :min-count 1)
    :vector (s/coll-of ::data :into [] :min-count 1)))

(defrecord SimpleIndexer [data]
  d/IDataIndexer
  (add-datom [this p a v]
    (swap! data conj [p a v]))
  (remove-datom [this p a v]
    (swap! data disj [p a v])))

(defn simple-indexer []
  (->SimpleIndexer (atom #{})))

(defspec test-nil-to-indexed 10
  (binding [s/*recursion-limit* 2]
    (prop/for-all [v (s/gen ::coll-data)]
      (let [idx (simple-indexer)]
        (time (d/diff-data idx nil v))
        (println " datoms " (count @(:data idx)))
        (is (pos? (count @(:data idx))))))))

(defspec test-a-to-b 10
  (binding [s/*recursion-limit* 2]
    (prop/for-all [v1 (s/gen ::coll-data)
                   v2 (s/gen ::coll-data)]
      (let [idx1 (simple-indexer)
            idx2 (simple-indexer)]
        (time (d/diff-data idx1 nil v1))
        (time (do (d/diff-data idx2 nil v2)
                  (d/diff-data idx2 v2 v1)))
        (println " datoms " (count @(:data idx1)))
        (is (pos? (count @(:data idx1))))
        (is (= @(:data idx1) @(:data idx2)))))))

(defspec test-a-to-nil 10
  (binding [s/*recursion-limit* 2]
    (prop/for-all [v1 (s/gen ::coll-data)]
      (let [idx1 (simple-indexer)]
        (time (d/diff-data idx1 nil v1))
        (time (d/diff-data idx1 v1 nil))
        (println " datoms " (count @(:data idx1)))
        (is (= @(:data idx1) #{}))))))

(deftest test-assoc
  (let [idx (d/index-data {:a 42})]
    (is (= idx {:a 42}))
    (is (= (assoc idx :a 0) {:a 0}))
    (is (= (count idx) 1))

    (is (= (set (o/for-query
                  (d/query idx _ :a ?i)
                  ?i))
           #{42}))

    (let [idx (assoc idx :a 0)]
      (is (= (set (o/for-query
                    (d/query idx _ :a ?i)
                    ?i))
             #{0})))))

(comment
  (let [db  [[:a :b]
             [:b :c]
             [:c :d]
             [:d :a]]
        da  [[:a :b]
             [:b :c]
             [:c :d]
             [:d :a]]
        si1 (simple-indexer)
        si2 (simple-indexer)]
    (d/diff-data si1 nil da)
    (d/diff-data si1 da db)
    (d/diff-data si2 nil db)
    (println @(:data si1))
    (println @(:data si2)))

  (s/exercise any?)
  )