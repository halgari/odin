(ns com.tbaldridge.com.tbaldridge.contexts.datomic-test
  (:require [datomic.api :as d]
            [com.tbaldridge.odin.contexts.datomic :refer [datoms]]
            [com.tbaldridge.odin :as o]
            [clojure.test :refer :all]))

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