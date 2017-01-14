(ns com.tbaldridge.odin.contexts.data
  (:refer-clojure :exclude [==])
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.util :as util]))

(set! *warn-on-reflection* true)

(defn prefix [itm rc]
  (reify
    clojure.lang.IReduceInit
    (reduce [this f init]
      (let [acc (f init itm)]
        (if (reduced? acc)
          @acc
          (reduce f acc rc))))))



(defn map-value [p k v]
  (cond
    (map? v)
    (let [next-path (conj p k)]
      (prefix [p k next-path]
              (eduction
                (mapcat (fn [[k v]]
                          (map-value next-path k v)))
                v)))

    (and (sequential? v)
         (not (string? v)))

    (let [next-path (conj p k)]
      (prefix [p k next-path]
              (eduction
                (map-indexed
                  (partial map-value next-path))
                cat
                v)))

    :else
    [[p k v]]))


(defn map-path [v]
  (let [p ^::path []]
    (cond

      (map? v)
      (eduction
        (mapcat
          (fn [[k v]]
            (map-value p k v)))
        v)

      (and (sequential? v)
           (not (string? v)))

      (eduction
        (map-indexed
          (partial map-value p))
        cat
        v))))

(defn path? [p]
  (and (vector? p)
       (::path (meta p))))

(deftype IndexedData [coll index])

(def conj-list (fnil conj '()))

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

{:eav   {}
 :ave   {}
 :vea   {}
 :paths {[]      [[:a] [:a :b] [:a :b :c]]
         [:a]    [[:a :b] [:a :b :c]]
         [:a :b] [[:a :b :c]]}}

(defn index-data
  "Indexes a collection for use in a call to `query`. Greater performance
    can be found by indexing a collection once and re-using the index for
    many successive queries. `query` automatically indexes all collections,
    however, so this is not absoultely required. "
  ^IndexedData [coll]
  (->>
    (reduce
      (fn [acc [p a v]]
        (let [acc (-> acc
                      (util/assoc-in! [:eav p a] v)
                      (util/update-in! [:ave a v] conj-list p)
                      (util/update-in! [:vea v p] conj-list a))]
          (if (path? v)
            (reduce
              (fn [acc p]
                (util/update-in! acc [:paths p] conj-list v))
              acc
              (prefix p
                      (parent-vectors p)))
            acc)))
      nil
      (map-path coll))
    (->IndexedData coll)))



(defn coll-index
  "Like `index-data` but looks in the query context to see if this collection
  was previously indexed. Not super useful for end-users but, kept public
  incase it is found useful by someone. "
  [coll]
  (if (instance? IndexedData coll)
    (.-index ^IndexedData coll)
    (if-let [v (get-in u/*query-ctx* [::indicies coll])]
      v
      (let [indexed (.-index (index-data coll))]
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

                              [true false true] (util/efor [[v es] (get-in index [:ave a'])
                                                            e es]
                                                  (-> env
                                                      (u/unify p' e)
                                                      (u/unify v' v)))
                              [false false true] (when-some [v (get-in index [:eav p' a'])]
                                                   (u/just (assoc env v' v)))

                              [false true true] (util/efor [[a v] (get-in index [:eav p'])]
                                                  (assoc env a' a v' v))

                              [true false false] (util/efor [e (get-in index [:ave a' v'])]
                                                   (assoc env p' e))

                              [false true false] (util/efor [a (get-in index [:vea v' p'])]
                                                   (u/unify env a' a))

                              [false false false] (when (= v' (get-in index [:eav p' a']))
                                                    (u/just env))

                              [true true false] (util/efor [[e as] (get-in index [:vea v'])
                                                            a as]
                                                  (-> env
                                                      (u/unify p' e)
                                                      (u/unify a' a)))

                              [true true true] (util/efor [[e avs] (get index :eav)
                                                           [a v] avs]
                                                 (-> env
                                                     (u/unify p' e)
                                                     (u/unify a' a)
                                                     (u/unify v' v))))))))))


(defn query-in
  ([coll path]
    (query-in coll (butlast path) (last path)))
  ([coll path v]
    (query-in coll [] path v))
  ([coll p [h & t] v]
   (if (seq t)
     (let [cvar (u/lvar)]
       (u/conjunction
         (query coll p h cvar)
         (query-in coll cvar t v)))
     (query coll p h v))))

(defn parent-of [data ?p ?c]
  (mapcat
    (fn [env]
      (let [data' (u/walk env data)
            p' (u/walk env ?p)
            c' (u/walk env ?c)]
        (util/truth-table [(u/lvar? p') (u/lvar? c')]
          [false true] (let [index (:paths (coll-index data'))]
                         (util/efor [c (get index p')]
                           (assoc env ?c c)))
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

(defn == [a b]
  (keep
    (fn [env]
      (u/unify env a b))))



