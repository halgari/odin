
(ns odin.unification
  (:import (java.io Writer)))


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
          (range c))))))


(let [a (lvar)]
  (transduce
    (conjunction
      (range-table 10 a)
      (== a 9)
      (keep
            (fn [env]
              (walk env a))))
    conj
    [{}]))



































