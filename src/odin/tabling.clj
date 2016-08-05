(ns odin.tabling
  (:refer-clojure :exclude [==])
  (:require [odin.unification :refer [lvar? lvar == disjunction conjunction unify walk] :as u]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as str])
  (:import [odin.unification LVar]))



(defprotocol ITableEntry
  (completed? [this])
  (results [this])
  (suspended [this])
  (add-result! [this result])
  (suspend [this f seen])
  (add-seen [this f seen])
  (complete [this]))

(defrecord SuspendedEntry [f xf processed]
  )


(deftype TableEntry [^:volatile-mutable status ^:volatile-mutable suspended ^:volatile-mutable results]
  ITableEntry
  (completed? [this]
    (= status :completed))
  (results [this]
    results)
  (suspended [this]
    suspended)
  (add-result! [this result]
    (set! results (conj results result)))
  (suspend [this f seen]
    (set! suspended (assoc suspended f seen)))
  (add-seen [this f seen]
    (set! suspended (update-in suspended [f] conj seen)))
  (complete [this]
    (set! status :completed)
    (set! suspended nil)))




(def ^:dynamic *tables*)
(def ^:dynamic *building* false)

(defn make-key [env args]
  (mapv (fn [arg]
          (let [v (u/walk env arg)]
            (if (lvar? v)
              LVar
              v)))
        args))

(defn unify-row [env args row]
  (reduce-kv
    (fn [env idx arg]
      (if (lvar? arg)
        (unify env arg (nth row idx))
        env))
    env
    args))

(defn emit-results [xf acc env table outer-args]
  (let [walked-args (mapv (partial walk env) outer-args)]
    (transduce
      (map (partial unify-row env walked-args))
      xf
      acc
      (if (instance? TableEntry table)
        (results table)
        table))))

(defn bind-args [args vals]
  (reduce-kv
    (fn [acc i arg]
      (let [v (nth vals i)]
        (if (identical? LVar v)
          acc
          (assoc acc arg v))))
    {}
    args))

(defmacro doreduce [[b coll & rest] & body]
  `(reduce
     (fn [_# ~b]
       ~(if rest
          `(doreduce ~rest ~@body)
          `(do ~@body))
       nil)
     nil
     ~coll))

(defn continue-suspended [rule-name]
  (let [emitted (volatile! false)]
    (doreduce [[k table] (*tables* rule-name)
               [f seen] (suspended table)
               to-emit (set/difference (results table) seen)]
              (vreset! emitted false)
              (f to-emit))
    (if @emitted
      (recur rule-name)
      rule-name)))

(defn tabulate [rule-name inner-args body-expr]
  (fn [outer-args]
    (fn [xf]
      (fn tabulate-inner
        ([] (xf))
        ([acc] (xf acc))
        ([acc env]
         (let [key (make-key env outer-args)]
           (if-let [^TableEntry table (get-in *tables* [rule-name key])]
             (if (completed? table)
               ;; The table is completed, so just unify the results
               (do
                 (println "Prebuilt")
                 (emit-results xf acc env table outer-args))
               ;; Table is not completed, so we must be in a sub node somewhere. So we'll emit all resuls
               ;; we have in the table so far, then create a suspension and attach it to the table.
               (let [to-emit (results table)]
                 (suspend table (fn suspension [vals]
                                  (add-seen table suspension vals)
                                  (emit-results xf nil env [vals] outer-args))
                          to-emit)
                 (emit-results xf acc env to-emit outer-args)))
             ;; No entry exists for this rule-key, so we need to create one
             (let [table   (let [table (->TableEntry :building {} #{})]
                             (set! *tables* (assoc-in *tables* [rule-name key] table))
                             table)
                   new-env (bind-args inner-args key)]
               (let [rf (fn
                          ([] nil)
                          ([acc] nil)
                          ([acc itm]
                           (add-result! table (make-key itm inner-args))
                           nil))]
                 (binding [*building* true]
                   (transduce body-expr rf [new-env]))
                 (continue-suspended rule-name))
               (let [to-emit (results table)
                     result (emit-results xf acc env to-emit outer-args)]
                 (if *building*
                   (suspend table (fn suspension [vals]
                                    (add-seen table suspension vals)
                                    (emit-results xf nil env [vals] outer-args))
                            to-emit)
                   (doreduce [[_ table] (*tables* rule-name)]
                             (complete table)))
                 result)))))))))

(def graph [[:a :b]
            [:b :c]
            [:c :d]
            [:d :a]
            [:d :f]
            [:f :a]
            [:c :f]
            [:c :g]
            [:g :z]])

(defn linked [?a ?b]
  (apply disjunction
         (map (fn [[a b]]
                (disjunction
                  (conjunction
                    (== ?a a)
                    (== ?b b))
                  (conjunction
                    (== ?a b)
                    (== ?b a)))) graph)))

(defmacro delay-rule [expr]
  `(fn [xf#]
     (fn [] (xf#))
     (fn [acc#] (xf# acc#))
     (fn [acc# itm#]
       ((~expr xf#) acc# itm#))))

(declare path)
(let [inner-a (lvar)
      inner-b (lvar)
      s (lvar)
      table (tabulate ::path [inner-a inner-b]
                      (disjunction
                        (linked inner-a inner-b)
                        (conjunction
                          (linked inner-a s)
                          (delay-rule (path s inner-b)))))]
  (defn path [a b]
    (table [a b])))


(defn body-args [form]
  (let [args     (atom #{})
        out-form (walk/postwalk
                   (fn [itm]
                     (if (and (symbol? itm)
                              (not (namespace itm))
                              (str/starts-with? (name itm) "?")
                              (not (::created (meta itm))))
                       (do (swap! args conj itm)
                           (vary-meta itm assoc ::created true))
                       itm))
                   form)]
    [@args out-form]))

(defmacro defrule [name args & body]
  (let [nm (keyword (clojure.core/name (gensym "rule-")))
        [found-vars body] (body-args body)
        all-vars (set/union (set args) found-vars)]
    `(do
       (declare ~name)
       (let [~@(interleave all-vars (repeat `(lvar)))
               table-fn# (tabulate ~nm [~@args] (delay-rule (conjunction ~@body)))]
           (defn ~name [~@args]
             (table-fn# [~@args]))))))



(defrule path [?a ?b]
         (disjunction
           (linked ?a ?b)
           (conjunction
             (linked ?a ?s)
             (path ?s ?b))))

(macroexpand '(defrule path [?a ?b]
                       (disjunction
                         (linked ?a ?b)
                         (conjunction
                           (linked ?a ?s)
                           (path ?s ?b)))))

(binding [*tables* {} #_ {:foo {[LVar 42] (->TableEntry :completed [] [[1 42]
                                                                       [2 42]])}}]
  (let [inner-a (lvar)
        inner-b (lvar)
        outer-a (lvar)
        result  (last (repeatedly 2 #(transduce
                                      (comp #_((tabulate :foo [inner-a inner-b] (range-table inner-a inner-b))
                                                [42 outer-a])
                                        (path :a outer-a)
                                        #_((tabulate ::path [inner-a inner-b]
                                                     (range-table inner-a inner-b))
                                            [42 outer-a])
                                        (keep (fn [env]
                                                (u/walk env outer-a))))
                                      conj
                                      [{}])))]
    (doseq [[k v] (::path *tables*)]
      (println k (results v)))
    result))


(let [a (lvar)
      b (lvar)]
  (make-key {a 42} [a b]))
























