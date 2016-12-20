(ns com.tbaldridge.odin.contexts.data
  (:refer-clojure :exclude [==])
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.util :as util])
  (:import (clojure.lang Associative ILookup IPersistentCollection Seqable)))

(set! *warn-on-reflection* true)

(declare diff-data)

(defprotocol IDataIndexer
  (add-datom [this p a v])
  (remove-datom [this p a v]))

(defprotocol IIndex
  (force-index [this])
  (get-index [this type]))

(defprotocol IUpdatableDB
  (update-data [this data]))

(defn prefix [itm rc]
  (reify
    clojure.lang.IReduceInit
    (reduce [this f init]
      (let [acc (f init itm)]
        (if (reduced? acc)
          @acc
          (reduce f acc rc))))))

(defn path? [p]
  (and (vector? p)
       (::path (meta p))))

(defn parent-vectors [v]
  (assert (vector? v) "Path must be a vector")
  (when (seq v)
    (reify
      clojure.lang.IReduceInit
      (reduce [this f init]
        (loop [acc init
               v   (pop v)]
          (let [result (f acc v)]
            (if (reduced? result)
              @result
              (if (empty? v)
                result
                (recur result (pop v))))))))))

(deftype IndexedData [^clojure.lang.Associative coll
                      ^:volatile-mutable indexed?
                      ^:volatile-mutable prev-coll
                      ^:volatile-mutable eav
                      ^:volatile-mutable ave
                      ^:volatile-mutable vea]
  IDataIndexer
  (add-datom [this p a v]
    (set! eav (util/massoc-in! eav [p a] v))
    (set! ave (util/massoc-in! ave [a v p] p))
    (set! vea (util/massoc-in! vea [v p a] a)))

  (remove-datom [this p a v]
    (set! eav (util/mdissoc-in! eav [p a]))
    (set! ave (util/mdissoc-in! ave [a v p]))
    (set! vea (util/mdissoc-in! vea [v p a])))

  IIndex
  (get-index [this type]
    (when-not indexed?
      (force-index this))
    (case type
      :eav eav
      :ave ave
      :vea vea))

  (force-index [this]
    (locking this
      (when-not indexed?
        (diff-data this prev-coll coll)
        (set! indexed? true)
        (set! prev-coll nil))
      this))

  clojure.lang.Associative
  (containsKey [this o]
    (.containsKey coll o))
  (entryAt [this o]
    (.entryAt coll o))
  (assoc [this k v]
    (IndexedData. (.assoc coll k v)
                   false
                   (if indexed?
                     coll prev-coll)
                   nil nil nil))

  Seqable
  (seq [this]
    (.seq coll))

  IPersistentCollection
  (count [this]
    (.count coll))
  (cons [this o]
    (.cons coll o))
  (empty [this]
    ;;TODO, return new
    )
  (equiv [this o]
    (.equiv coll o))
  ILookup
  (valAt [this o]
    (.valAt coll o))
  (valAt [this o o1]
    (.valAt coll o o1)))

(comment
  (let [idx (->IndexedData {} nil)]
    (diff-data idx nil {:a {:b {:c 42}}})
    (diff-data idx {:a {:b {:c 42}}} {:a 42})
    (get-index idx)
    )
  )

(declare diff-data)

(defn index-data
  "Indexes a collection for use in a call to `query`. Greater performance
    can be found by indexing a collection once and re-using the index for
    many successive queries. `query` automatically indexes all collections,
    however, so this is not absoultely required. "
  ^IndexedData [coll]
  (->IndexedData coll false nil nil nil nil))

(defn coll-index
  "Like `index-data` but looks in the query context to see if this collection
  was previously indexed. Not super useful for end-users but, kept public
  incase it is found useful by someone. "
  [coll]
  (if (instance? IndexedData coll)
    coll
    (if-let [v (get-in u/*query-ctx* [::indicies coll])]
      v
      (let [indexed (index-data coll)]
        (set! u/*query-ctx* (assoc-in u/*query-ctx* [::indicies coll] indexed))
        indexed))))


(defn query
  "Given a Clojure datastructure, query it, binding p to the `get-in` path
  of each value. `v` is bound to the values in the collection. `a` is bound
  to the attribute that connects a `p` and a `v`. `coll` must be bound,
  but `p`, `a` and `v` are fully relational. "
  [coll p a v]
  (let [index (coll-index coll)]
    (u/with-tracing "Data Query" [p a v]
      (mapcat
        (fn [env]
          (let [index (if (u/lvar? coll)
                        (coll-index (u/walk env coll))
                        index)
                p'    (u/walk env p)
                a'    (u/walk env a)
                v'    (u/walk env v)]
            (util/truth-table [(u/lvar? p') (u/lvar? a') (u/lvar? v')]

              [true false true] (util/efor [[v es] (get (get-index index :ave) a')
                                            [e] es]
                                  (-> env
                                      (u/unify p' e)
                                      (u/unify v' v)))
              [false false true] (when-some [v (get-in (get-index index :eav) [p' a'])]
                                   (u/just (assoc env v' v)))

              [false true true] (util/efor [[a v] (get (get-index index :eav) p')]
                                  (assoc env a' a v' v))

              [true false false] (util/efor [[e] (get-in (get-index index :ave) [a' v'])]
                                   (assoc env p' e))

              [false true false] (util/efor [[a] (get-in (get-index index :vea) [v' p'])]
                                   (u/unify env a' a))

              [false false false] (when (= v' (get-in (get-index index :eav) [p' a']))
                                    (u/just env))

              [true true false] (util/efor [[e as] (get (get-index index :vea) v')
                                            [a] as]
                                  (-> env
                                      (u/unify p' e)
                                      (u/unify a' a)))

              [true true true] (util/efor [[e avs] (get-index index :eav)
                                           [a v] avs]
                                 (-> env
                                     (u/unify p' e)
                                     (u/unify a' a)
                                     (u/unify v' v))))))))))


(defn query-in [coll p [h & t] v]
  (if (seq t)
    (let [cvar (u/lvar)]
      (u/conjunction
        (query coll p h cvar)
        (query-in coll cvar t v)))
    (query coll p h v)))

(defn parent-of [data ?p ?c]
  (mapcat
    (fn [env]
      (let [p'    (u/walk env ?p)
            c'    (u/walk env ?c)]
        (util/truth-table [(u/lvar? p') (u/lvar? c')]
          [false true] (let [eav (get-index (coll-index (u/walk env data)) :eav)
                             ts  (drop 1 (tree-seq path? (fn [p]
                                                           (for [[a v] (get eav p)
                                                                 :when (path? v)]
                                                             v)) p'))]
                         (for [child ts]
                           (assoc env ?c child)))
          [true false] (do
                         (assert (vector? c') "Child value in parent-of must be a vector")
                         (when (seq c')
                           (eduction
                             (map #(assoc env p' %))
                             (parent-vectors c'))))
          [false false] (when (transduce
                                (filter (partial = p'))
                                util/first-rf
                                (parent-vectors c'))
                          [env])
          )))))


(defn get-type [v]
  (cond
    (nil? v) :nil
    (map? v) :map
    (vector? v) :map
    (and (sequential? v)
         (not (string? v))) :seq
    :else :val))

(defn nil-reduce [xf coll]
  (transduce xf (fn
                  ([] nil)
                  ([x] nil)
                  ([x y] nil)) nil coll))

(defn vreduce-kv [f init coll]
  (if (instance? clojure.lang.PersistentVector coll)
    (reduce-kv #(f %1 (long %2) %3) init coll)
    (reduce-kv f init coll)))

(defn diff-val [di path attr a b]
  (when-not (identical? a b)
    (condp = [(get-type a) (get-type b)]
      [:val :val] (if (not= a b)
                    (do
                      (remove-datom di path attr a)
                      (add-datom di path attr b)))
      [:map :nil] (let [new-path (conj path attr)]
                    (remove-datom di path attr new-path)
                    (vreduce-kv
                      (fn [_ k v]
                        (remove-datom di path k new-path)
                        (diff-val di new-path k v nil))
                      nil
                      a))
      [:val :nil] (remove-datom di path attr a)

      [:val :map] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))
      [:val :seq] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))
      [:seq :val] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))
      [:map :seq] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))
      [:map :val] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))
      [:seq :map] (do (diff-val di path attr a nil)
                      (diff-val di path attr nil b))

      [:nil :map] (let [new-path (conj path attr)]
                    (add-datom di path attr new-path)
                    (vreduce-kv
                      (fn [_ k v]
                        (diff-val di new-path k nil v))
                      nil
                      b))
      [:nil :val] (add-datom di path attr b)

      [:nil :seq] (let [new-path (conj path attr)]
                    (add-datom di path attr new-path)
                    (nil-reduce
                      (map-indexed
                        (fn [i v]
                          (diff-val di new-path i nil v)))
                      b))
      [:seq :nil] (let [new-path (conj path attr)]
                    (remove-datom di path attr new-path)
                    (nil-reduce
                      (map-indexed
                        (fn [i v]
                          (diff-val di new-path i v nil)))
                      a))
      [:map :map] (let [new-path (conj path attr)]
                    (vreduce-kv
                      (fn [_ ka va]
                        (if-some [vb (get b ka)]
                          (diff-val di new-path ka va vb)
                          (diff-val di new-path ka va nil)))
                      nil
                      a)
                    (vreduce-kv
                      (fn [_ kb vb]
                        (if-not (contains? a kb)
                          (diff-val di new-path kb nil vb)))
                      nil
                      b))
      [:seq :seq] (let [new-path (conj path attr)]
                    (loop [[a & an :as as] (seq a)
                           [b & bn :as bs] (seq b)
                           idx 0]
                      (when (or as bs)
                        (diff-val di new-path idx a b)
                        (recur an bn (inc idx))))))))

(defn diff-data [di a b]
  (let [path ^::path []]
    (condp = [(get-type a) (get-type b)]
      [:nil :map] (vreduce-kv
                    (fn [_ k v]
                      (diff-val di path k nil v))
                    nil
                    b)

      [:nil :seq] (nil-reduce
                    (map-indexed
                      (fn [i v]
                        (diff-val di path i nil v)))
                    b)
      [:map :map] (do (vreduce-kv
                        (fn [_ ka va]
                          (if-some [vb (get b ka)]
                            (diff-val di path ka va vb)
                            (do (remove-datom di path ka va)
                                (diff-val di path ka va nil))))
                        nil
                        a)
                      (vreduce-kv
                        (fn [_ kb vb]
                          (if-not (contains? a kb)
                            (diff-val di path kb nil vb)))
                        nil
                        b))
      [:seq :seq] (loop [[a & an :as as] (seq a)
                         [b & bn :as bs] (seq b)
                         idx 0]
                    (when (or as bs)
                      (diff-val di path idx a b)
                      (recur an bn (inc idx))))
      [:map :seq] (do (diff-data di a nil)
                      (diff-data di nil b))
      [:seq :map] (do (diff-data di a nil)
                      (diff-data di nil b))
      [:map :nil] (vreduce-kv
                    (fn [_ k v]
                      (diff-val di path k v nil))
                    nil
                    a)

      [:seq :nil] (nil-reduce
                    (map-indexed
                      (fn [i v]
                        (diff-val di path i v nil)))
                    a)
      [:nil :val] nil)))





