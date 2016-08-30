(ns com.tbaldridge.odin.unification
  (:refer-clojure :exclude [==])
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec :as s]
            [com.tbaldridge.odin.util :refer [body-lvars]])
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

(defmethod -unify [LVar LVar]
  [env a b]
  (assoc env a b ))


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


(defn with-env [reducible]
  (let [ctx *query-ctx*]
    (reify
      clojure.lang.IReduceInit
      (reduce [this f init]
        (if *query-ctx*
          (reduce f init reducible)
          (binding [*query-ctx* ctx]
            (reduce f init reducible)))))))

(defn for-query-impl [query projection]
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

(defn project-impl [expr bind]
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

(defn pass-impl [expr]
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



(defn defrule-impl [name args body]
  (let [body `(conjunction
                ~@body)
        [lvars body] (body-lvars body)
        lvars (set/difference lvars (set args))]
    `(defn ~name ~args
       (let [~@(interleave
                 lvars
                 (map (fn [lvar]
                        `(lvar))
                      lvars))]
         ~@body))))


(defn just [x]
  (reify
    clojure.lang.IReduceInit
    (reduce [this f init]
      (unreduced (f init x)))))


(defn lazy-rule-impl [expr]
  `(mapcat
     (fn recursive-call [env#]
       (eduction
         ~expr
         (just env#)))))










