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