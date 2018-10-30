(ns ^:skip-wiki com.tbaldridge.odin.unification
  (:refer-clojure :exclude [==])
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [com.tbaldridge.odin.util :refer [body-lvars]]
            [com.tbaldridge.odin.util :as util])
    (:import (java.io Writer)))


(def ^:dynamic *query-ctx* nil)

(defn cache-in-context-impl [k body]
  `(if-let [v# (get *query-ctx* ~k)]
     v#
     (let [b# ~body]
       (set! *query-ctx* (assoc *query-ctx* ~k b#))
       b#)))

(declare tracing-impl)

(defn with-tracing [prefix lvars & clauses]
  (apply comp (tracing-impl prefix lvars) clauses))


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
  (assoc env a b))


(defn unify
  ([env a b]
   (when env
     (-unify env (walk env a) (walk env b))))
  ([env a b c d]
   (-> env
       (unify a b)
       (unify c d)))
  ([env a b c d e f]
   (-> env
       (unify a b)
       (unify c d)
       (unify e f)))
  ([env a b c d e f & more]
   (apply unify (-> env
                    (unify a b)
                    (unify c d)
                    (unify e f)) more)))

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
        lvars   (set/union query-lvars proj-lvars)
        env-sym (gensym "env_")]
    `(let [~@(interleave lvars (repeat `(lvar)))]
       (binding [*query-ctx* (or *query-ctx* {} #_{::fn println-tracing-reporter})]
         (with-env
           (eduction
             (comp
               ~query-form
               (with-tracing "for-query projection: " [~@lvars]
                 (keep
                   (fn [~env-sym]
                     (let [~@(interleave
                               lvars
                               (map (fn [lvar]
                                      `(walk ~env-sym ~lvar))
                                    lvars))]
                       ~proj-form)))))
             [(hash-map)]))))))




(s/def ::bind-projection any?)
(s/def ::cat-projection (s/spec (s/cat :bind any?
                                       :cat #{'...})))

(s/def ::projection (s/cat :expr any?
                           :projection (s/alt :cat ::cat-projection
                                              :bind ::bind-projection)))

(s/def ::projections (s/cat :goals (s/* ::projection)))

(defn clean-projection [body]
  (walk/postwalk
    (fn [form]
      (if (= form `(lvar))
        '_
        form))
    body))

(defn project-impl [{:keys [expr projection] :as clause}]
  (let [[type proj] projection
        proj (clean-projection proj)]
    (let [[lvars expr] (body-lvars expr)
          env-sym (gensym "env_")]
      (case type
        :bind (let [[proj-lvars] (body-lvars proj)
                    ; ^^ Don't use the xformed expr since that will mess with Clojure's destructuring macro
                    new-proj-names (map #(gensym (name %)) proj-lvars)]
                `(keep
                   (fn [~env-sym]
                     (let [~@(interleave
                               lvars
                               (map (fn [lvar]
                                      `(walk ~env-sym ~lvar))
                                    lvars))
                           ~@(interleave new-proj-names proj-lvars)
                           ~proj ~expr]
                       (unify ~env-sym ~@(interleave new-proj-names proj-lvars))))))
        :cat (let [{:keys [bind]} proj
                   ;; Don't use the xformed expr since that will mess with Clojure's destructuring macro
                   ;; Since '_ will be transformed into `(lvar)
                   [proj-lvars] (body-lvars bind)
                   new-proj-names (map #(gensym (name %)) proj-lvars)]
               `(mapcat
                  (fn [~env-sym]
                    (let [~@(interleave
                              lvars
                              (map (fn [lvar]
                                     `(walk ~env-sym ~lvar))
                                   lvars))
                          ~@(interleave new-proj-names proj-lvars)]
                      (eduction
                        (keep
                          (fn [~bind]
                            (unify ~env-sym ~@(interleave new-proj-names proj-lvars))))
                        ~expr)))))))))

(defn project-impls [clauses]
  (let [conf (s/conform ::projections clauses)
        _    (assert (not= conf ::s/invalid) (s/explain ::projections clauses))]
    (list* `conjunction
           (map project-impl (:goals conf)))))


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
  (let [body  `(conjunction
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
     (fn ~'recursive-call [env#]
       (eduction
         ~expr
         (just env#)))))

(defmacro lazy-rule [expr]
  (lazy-rule-impl expr))



(defn tracing-impl [prefix lvars]
  (if-let [f (get *query-ctx* ::fn)]
    (map
      (fn [env]
        (f prefix lvars (mapv #(walk env %) lvars))
        env))
    identity))

(defn println-tracing-reporter [prefix lvars vals]
  (print prefix)
  (mapv
    (fn [lvar val]
      (if (lvar? val)
        (print lvar "(unbound) ")
        (print lvar "(" val ") ")))
    lvars vals)
  (println ""))

(def transform-data-lvar (lvar))
(def transform-fns-lvar (lvar))

(defn transform [location f args]
  (map
    (fn [env]
      (let [data (walk env transform-data-lvar)
            fns  (walk env transform-fns-lvar)]
        (assert (not (lvar? data)))
        (assoc env transform-fns-lvar
                   (if (lvar? fns)
                     [[f location args]]
                     (conj fns [f location args])))))))

(defn walk-all [env data]
  (walk/postwalk
    (fn [form]
      (if (lvar? form)
        (walk env form)
        form))
    data))

(def xform-path (lvar))
(def xform-attr (lvar))

(defn transform-data [data envs f]
  (reduce
    (fn [acc env]
      (let [path (walk env xform-path)
            attr (walk env xform-attr)]
        (let [p (if (lvar? attr)
                  (vec path)
                  (conj (vec path) attr))]
          (update-in acc p f env))))
    data
    envs))

(defn transform-query-impl [data body f args]
  (let [[lvars query-form] (body-lvars body)
        env-sym (gensym "env")
        [args-lvars args-form] (body-lvars args)]
    `(let [~@(interleave lvars (repeat `(lvar)))]
       (binding [*query-ctx* (or *query-ctx* {})]
         (transform-data
           ~data
           (with-env
             (eduction
               ~query-form
               [(hash-map)]))
           (fn [old# ~env-sym]
             (let [~@(interleave args-lvars
                                 (map (fn [lvar]
                                        `(walk ~env-sym ~lvar)) args-lvars))]
               (~f old# ~@args-form))))))))




(defn switch-impl [bodies]
  (let [columns  (->> bodies
                      keys
                      (apply map hash-set)
                      (map #(disj % '_ `(lvar)))
                      (map (fn [column]
                             (assert (= (count column) 1)
                                     (str "Switch statement can only have a single lvar or _ for each column, got " (pr-str column)))
                             (first column)))
                      vec)
        body-map (zipmap
                   (->> (keys bodies)
                        (map
                          (fn [a]
                            (mapv #(contains? #{'_ `(lvar)} %) a))))
                   (vals bodies))
        fns-sym  (gensym "fns")
        acc-sym  (gensym "acc")
        itm-sym  (gensym "itm")]
    `(fn [xf#]
       (let [~fns-sym (mapv
                        (fn [f#]
                          (f# xf#))
                        [~@(vals body-map)])]
         (fn
           ([] (xf#))
           ([acc#] (xf# acc#))
           ([~acc-sym ~itm-sym]
            (util/truth-table ~(vec (for [column columns]
                                      `(lvar? (walk ~itm-sym ~column))))
              ~@(apply concat
                       (map-indexed
                         (fn [idx [k v]]
                           [k `((get ~fns-sym ~idx) ~acc-sym ~itm-sym)])
                         body-map)))))))))


(defn update-local-cache [k f args]
  (map
    (fn [env]
      (apply update-in env [::context k] f (map (partial walk env) args)))))

(defn get-local-cache [k lvar]
  (keep
    (fn [env]
      (unify env (get-in env [::context k])  lvar))))
