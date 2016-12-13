(ns com.tbaldridge.contexts.datomic-test
  (:require [datomic.api :as d]
            [com.tbaldridge.odin.contexts.data :as dc]
            [com.tbaldridge.odin.contexts.datomic :refer [datoms] :as datomic]
            [com.tbaldridge.odin :as o]
            [clojure.test :refer :all]))

(set! *warn-on-reflection* true)


(def schema [{:db/ident              :person/name
              :db/valueType          :db.type/string
              :db/cardinality        :db.cardinality/one
              :db/index              true}

             {:db/ident              :person/age
              :db/valueType          :db.type/long
              :db/index              true
              :db/cardinality        :db.cardinality/one}

             {:db/ident              :person/parents
              :db/valueType          :db.type/ref
              :db/index              true
              :db/cardinality        :db.cardinality/many}

             {:db/ident              :parent/children
              :db/valueType          :db.type/ref
              :db/index              true
              :db/cardinality        :db.cardinality/many}])

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

(defn new-db []
  (let [uri  (str "datomic:mem://" (name (gensym "")))
        _    (d/create-database uri)
        conn (d/connect uri)]
    @(d/transact conn schema)
    @(d/transact conn data)
    conn))

(def test-db (delay
               (new-db)))

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
           #{"Bill" "Jane"}))))


(deftest transact-tests
  (testing "asserting single datoms"
    (let [conn (new-db)]
      (datomic/transact-query conn
                               (o/and
                                 (datomic/add {:person/name "Zeb"
                                               :person/age 44})
                                 (datomic/add {:person/name "Zeek"
                                               :person/age 43})))
      (let [db (d/db conn)]
        (is (= (set (o/for-query
                      (datoms db _ :person/name ?name)
                      ?name))
               #{"Sam" "Bill" "Zeek" "Jane" "Zeb" "June"})))))

  (testing "asserting multiple datoms"
    (let [conn (new-db)
          data {"Zeb"  44
                "Zeek" 43}]
      (datomic/transact-query conn
                               (o/and
                                 (o/project
                                   data [[?name ?age] ...]
                                   {:person/name ?name
                                    :person/age  ?age} ?data)
                                 (datomic/add ?data)))
      (let [db (d/db conn)]
        (is (= (set (o/for-query
                      (datoms db _ :person/name ?name)
                      ?name))
               #{"Sam" "Bill" "Zeek" "Jane" "Zeb" "June"})))))

  (testing "multiple contexts"
    (let [conn1 (new-db)
          conn2 (new-db)
          data  {:conn1 {"Zeb" 44}
                 :conn2 {"Zeek" 43}}]
      (datomic/transact-query {:conn1 conn1
                               :conn2 conn2}
                              (o/and
                                (dc/query-in data [] [?conn ?name] ?age)
                                (o/project
                                  {:person/name ?name
                                   :person/age ?age} ?data)
                                (datomic/add ?conn ?data)))
      (let [db (d/db conn1)]
        (is (= (set (o/for-query
                      (datoms db _ :person/name ?name)
                      ?name))
               #{"Sam" "Bill" "Jane" "Zeb" "June"})))

      (let [db (d/db conn2)]
        (is (= (set (o/for-query
                      (datoms db _ :person/name ?name)
                      ?name))
               #{"Sam" "Bill" "Zeek" "Jane" "June"}))))))
