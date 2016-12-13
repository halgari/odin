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
     (o/update-local-cache ::tx-data update ::default vconj map))
    ([conn map]
     (o/update-local-cache ::tx-data update conn vconj map))
    ([e a v]
     (o/update-local-cache ::tx-data update vconj ::default [:db/add e a v]))
    ([conn e a v]
     (o/update-local-cache ::tx-data update vconj conn [:db/add e a v])))

  )


(defn transact-all [conns data]
  (into {}
        (map (fn [[conn data]]
               [conn @(d/transact (get conns conn conn) data)]))
        data))

(defmacro transact-query [conn query]
  (let [sym-name (gensym "?result")]
    `(let [conns# ~conn
           conns# (if (map? conns#) conns# {::default conns#})
           vdata# (vec (o/for-query
                         (o/and
                           ~query
                           (o/get-local-cache ::tx-data ~sym-name))
                         ~sym-name))
           data# (apply merge-with concat vdata#)]
       (transact-all conns# data#))))
