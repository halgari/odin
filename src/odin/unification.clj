(ns odin.unification
  (:refer-clojure :exclude [==])
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec :as s])
  (:import (java.io Writer)))

(def ^:dynamic *query-ctx* nil)

(deftype LVar [])

(defn lvar []
  (->LVar))

(defn lvar? [x]
  (instance? LVar x))

(defn walk [env a]
  (if-let [a (get env a)]
    (recur env a)
    a))

(defmulti -unify (fn [env a b]
                   [(type a) (type b)]))

(defmethod -unify [Object Object]
  [env a b]
  (when (= a b)
    env))

(defmethod -unify [LVar Object]
  [env a b]
  (assoc env a b))

(defmethod -unify [Object LVar]
  [env a b]
  (assoc env b a))


(defn unify [env a b]
  (-unify env (walk env a) (walk env b)))


(defmethod print-method LVar
  [v ^Writer w]
  (.write w (str "LVar@" (System/identityHashCode v))))

(defn == [a b]
  (keep
    (fn [env]
      (unify env a b))))

(def conjunction comp)

(defn disjunction [& exprs]
  (fn [xf]
    (let [fs (mapv (fn [expr]
                     (#'clojure.core/preserving-reduced (expr xf)))
                   exprs)]
      (fn
        ([] (xf))
        ([acc] (xf acc))
        ([acc itm]
         (reduce #(%2 %1 itm) acc fs))))))

(defn range-table [c i]
  (mapcat
    (fn [env]
      (let [c' (walk env c)]
        (assert (not (lvar? c')) "c must be bound")
        (eduction
          (keep (partial unify env i))
          (range c'))))))

(defn body-lvars [form]
  (let [lvars (atom #{})
        form (walk/postwalk
               (fn [v]
                 (cond
                   (and (symbol? v)
                        (not (namespace v))
                        (str/starts-with? (name v) "?"))
                   (do (swap! lvars conj v)
                       v)

                   (= v '_)
                   `(lvar)

                   :else v))
               form)]


    [@lvars form]))

(defn with-env [reducible]
  (println "ENV")
  (let [ctx *query-ctx*]
    (reify
      clojure.lang.IReduceInit
      (reduce [this f init]
        (if *query-ctx*
          (reduce f init reducible)
          (binding [*query-ctx* ctx]
            (reduce f init reducible)))))))

(defmacro for-query [query projection]
  (let [[query-lvars query-form] (body-lvars query)
        [proj-lvars proj-form] (body-lvars projection)
        lvars (set/union query-lvars proj-lvars)
        env-sym (gensym "env_")]
    `(let [~@(interleave lvars (repeat `(lvar)))]
       (binding [*query-ctx* (or *query-ctx* {})]
         (with-env
           (eduction
             (comp
               ~query-form
               (keep
                 (fn [~env-sym]
                   (let [~@(interleave
                             lvars
                             (map (fn [lvar]
                                    `(walk ~env-sym ~lvar))
                                  lvars))]
                     ~proj-form))))
             [{}]))))))

(defmacro project [expr bind]
  (let [[lvars expr] (body-lvars expr)
        env-sym (gensym "env_")]
    `(keep
       (fn [~env-sym]
         (let [~@(interleave
                   lvars
                   (map (fn [lvar]
                          `(walk ~env-sym ~lvar))
                        lvars))]
           (unify ~env-sym ~bind ~expr))))))

(defmacro pass [expr]
  (let [[lvars expr] (body-lvars expr)
        env-sym (gensym "env_")]
    `(keep
       (fn [~env-sym]
         (let [~@(interleave
                   lvars
                   (map (fn [lvar]
                          `(walk ~env-sym ~lvar))
                        lvars))]
           (when ~expr
             ~env-sym))))))








