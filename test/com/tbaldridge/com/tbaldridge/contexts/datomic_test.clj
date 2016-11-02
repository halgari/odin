(ns com.tbaldridge.com.tbaldridge.contexts.datomic-test
  (:require [datomic.api :as d]
            [com.tbaldridge.odin.contexts.datomic :refer [datoms]]
            [com.tbaldridge.odin :as o]
            [clojure.test :refer :all])
  (:import (java.io Writer)))

(set! *warn-on-reflection* true)


(def schema [{:db/id                 (d/tempid :db.part/db)
              :db/ident              :person/name
              :db/valueType          :db.type/string
              :db/cardinality        :db.cardinality/one
              :db/index              true
              :db.install/_attribute :db.part/db}

             {:db/id                 (d/tempid :db.part/db)
              :db/ident              :person/age
              :db/valueType          :db.type/long
              :db/index              true
              :db/cardinality        :db.cardinality/one
              :db.install/_attribute :db.part/db}

             {:db/id                 (d/tempid :db.part/db)
              :db/ident              :person/parents
              :db/valueType          :db.type/ref
              :db/index              true
              :db/cardinality        :db.cardinality/many
              :db.install/_attribute :db.part/db}

             {:db/id                 (d/tempid :db.part/db)
              :db/ident              :parent/children
              :db/valueType          :db.type/ref
              :db/index              true
              :db/cardinality        :db.cardinality/many
              :db.install/_attribute :db.part/db}])

(def data [{:db/id       (d/tempid :db.part/user -1)
            :person/name "Bill"
            :person/age  60}

           {:db/id       (d/tempid :db.part/user -2)
            :person/name "Jane"
            :person/age  60}


           {:db/id          (d/tempid :db.part/user -3)
            :person/name    "Sam"
            :person/age     40
            :person/parents #{(d/tempid :db.part/user -1)
                              (d/tempid :db.part/user -2)}}

           {:db/id          (d/tempid :db.part/user -4)
            :person/name    "June"
            :person/age     42
            :person/parents #{(d/tempid :db.part/user -1)
                              (d/tempid :db.part/user -2)}}])


(def test-db (delay
               (let [uri  (str "datomic:mem://" (name (gensym "")))
                     _    (d/create-database uri)
                     conn (d/connect uri)]
                 @(d/transact conn schema)
                 @(d/transact conn data)
                 conn)))

(deftest datomic-tests
  (let [db (d/db @test-db)]
    (is (= (set (o/for-query
                  (o/and
                    (datoms db ?parent :person/name "Bill")
                    (datoms db ?child :person/parents ?parent)
                    (datoms db ?child :person/name "June")
                    (datoms db ?child :person/age ?age))
                  ?age))
           #{42}))

    (is (= (set (o/for-query
                  (o/and
                    (datoms db ?parent :person/name ?parent-name)
                    (datoms db ?child :person/parents ?parent)
                    (datoms db ?child :person/name "June"))
                  ?parent-name))
           #{"Bill" "Jane"}))

    (is (= (set (o/for-query
                  (o/and
                    (datoms db ?person ?attr "Bill"))
                  ?attr))
           #{}))))

(comment
  (let [db (d/db @test-db)]
    @(d/transact @test-db (vec (o/for-query
                                 (datoms db ?child :person/parents ?parent)
                                 [:db/add ?parent :parent/children ?child]))))

  (sort (for [x (range 10)
              y (range 10)]
          [x y]))
  (set! *unchecked-math* :warn-on-boxed)

  (deftype Datom [e a v])

  (deftype Blank []
    Comparable
    (compareTo [this other]
      -1))

  (defmethod print-method Datom
    [^Datom d ^Writer w]
    (.write w (pr-str [(.-e d) (.-a d) (.-v d)])))

  #_(defn compare [^Comparable a ^Comparable b])


  (defn sort-fn [format]
    (case format
      :eav (fn [^Datom a ^Datom b]
             (case (compare (.-e a) (.-e b))
               -1 -1
               1 1
               0 (case (compare (.-a a) (.-a b))
                   -1 -1
                   1 1
                   0 (compare (.-v a) (.-v b)))))
      :ave (fn [^Datom a ^Datom b]
             (case (compare (.-a a) (.-a b))
               -1 -1
               1 1
               0 (case (compare (.-v a) (.-v b))
                   -1 -1
                   1 1
                   0 (compare (.-e a) (.-e b)))))
      :vea (fn [^Datom a ^Datom b]
             (case (compare (.-v a) (.-v b))
               -1 -1
               1 1
               0 (case (compare (.-e a) (.-e b))
                   -1 -1
                   1 1
                   0 (compare (.-a a) (.-a b)))))))

  (dotimes [x 100]
    (time ()))

  (defn bsearch [^objects arr ^Datom search]
    (let [f (sort-fn :eav)]
      (loop [min 0
             max (dec (alength arr))
             c   0]
        #_(println min max)
        (if (< c 30)
          (if (= min max)
            min
            (let [idx (int (+ (int (/ (- max min) 2)) min))
                  val (aget arr idx)]
              #_(println val search (f search val) idx)
              (case (f search val)
                0 (recur min idx (inc c))
                -1 (recur min idx (inc c))
                1 (if (= (inc idx) max)
                    (recur max max (inc c))
                    (recur idx max (inc c))))))))))

  (def dtoms (into-array (vec (for [x (range 100)
                                    y (range 10)
                                    z (range 10)]
                                (->Datom x y z)))))
  (def arr
    (let []
      (time [(do (java.util.Arrays/sort
                   dtoms
                   (comparator (sort-fn :eav)))
                 dtoms)
             (sort (sort-fn :ave)
                   dtoms)
             (sort (sort-fn :vea)
                   dtoms)])))
  (let [arr    (first arr)
        search (->Datom 2 1 (->Blank))
        f      (sort-fn :eav)]
    (dotimes [x 100]
      (time (bsearch arr search))))





  #_(let [data (vec (for [x (range 100)
                          y (range 100)
                          z (range 100)]
                      [x y z]))]
      (dotimes [x 10]
        (time
          (count
            (sort data)
            #_(reduce
                (fn [acc [x y z]]
                  (assoc-in acc [x y] z))
                {}
                data))))
      )



  (let [a [:foo :bar :baz 1]
        b [:foo :bar :baz 2]

        c (hash-map a 44 b 41)]
    (time (dotimes [x 100000]
            (get c b))))

  (require '[com.tbaldridge.odin.util :as util])
  (require '[com.tbaldridge.odin.contexts.data :as data])
  (let [datoms (vec (for [x (range 100)
                          y (range 10)
                          z (range 10)]
                      [x y z]))]
    (println (count datoms))
    (let [v (time (->>
                    (:eav (reduce
                            (fn [acc [p a v]]
                              (let [acc (-> acc
                                            (util/assoc-in! [:eav p a] v)
                                            (util/update-in! [:ave a v] data/conj-list p)
                                            (util/update-in! [:vea v p] data/conj-list a))]
                                acc))
                            nil

                            datoms))))]
      (dotimes [x 5]
        (println (time (vec (get v 50)))))))


  (import 'com.tbaldridge.odin.TupleStore)
  (import 'com.tbaldridge.odin.TupleStore$Tuple)
  (import 'java.util.ArrayList)

  (require '[com.tbaldridge.odin.contexts.data :as d])

  (require '[clojure.data.xml :as xml])
  (xml/parse-str "<h1><div>42</div></h1>")
  (def data (xml/parse-str (time (slurp "https://api.eve-central.com/api/quicklook?typeid=34"))))

  (count (vec (com.tbaldridge.odin.contexts.data/map-path data)))


  (compare [:f] :b)

  (sort (comparator
          (fn [a b]
            (compare (hash a) (hash b)))) [:f :b [11]])

data
  (dotimes [x 100]
    (time (let [al    (ArrayList.)
                objs  (.toArray ^ArrayList (reduce
                                             (fn [^ArrayList al [e a v]]
                                               (.add al e)
                                               (.add al a)
                                               (.add al v)
                                               al)
                                             al
                                             (eduction
                                               (take 1000)
                                               (com.tbaldridge.odin.contexts.data/map-path data))))
                _     (println (alength objs) (.size al))
                store (.sort (TupleStore. objs) 0)]
            (println (alength objs) "->>" (count data))
            (vec (eduction
                   (map
                     (fn [^TupleStore$Tuple t]
                       [(.V1 t) (.V2 t) (.V3 t)]))
                   (take 20)
                   store))
            )))

  )