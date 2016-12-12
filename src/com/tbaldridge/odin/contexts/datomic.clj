(ns com.tbaldridge.odin.contexts.datomic
  (:require [datomic.api :as d]
            [com.tbaldridge.odin :as o]))


(o/defrule datoms [?db ?e ?a ?v]
  (o/switch
    [?e ?a ?v] (o/when
                 (first (d/datoms ?db :eavt ?e ?a ?v)))

    [?e ?a _] (o/project
                (d/datoms ?db :eavt ?e ?a) [[_ _ ?v] ...])

    [?e _ _] (o/project
               (d/datoms ?db :eavt ?e) [[_ ?a ?v] ...])

    [_ ?a ?v] (o/project
                (d/datoms ?db :avet ?a ?v) [[?e] ...])

    [_ ?a _] (o/project
                (d/datoms ?db :avet ?a) [[?e _ ?v] ...])

    [_ _ ?v] (o/project
               (d/datoms ?db :vaet ?v) [[?e ?a] ...])))


;; Transaction helpers

(let [vconj (fnil conj [])]
  (defn add
    ([map]
      (o/update-local-cache ::tx-data vconj map))
    ([e a v]
     (o/update-local-cache ::tx-data vconj [:db/add e a v])))

  )


(defmacro transact-query [conn query]
  (let [sym-name (gensym "?result")]
    `(d/transact ~conn
                 (into []
                   cat
                   (o/for-query
                     (o/and
                       ~query
                       (o/get-local-cache ::tx-data ~sym-name))
                     ~sym-name)))))