(ns com.tbaldridge.odin.contexts.data
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.util :as util])
  (:import (java.io Writer)))

(set! *warn-on-reflection* true)

(defprotocol IPath
  (add-to-path [this val]))

(deftype Path [v nxt]
  IPath
  (add-to-path [this val]
    (Path. val this))
  clojure.lang.IReduceInit
  (reduce [this f init]
    (loop [^Path p this
           acc init
           i 0]
      (if p
        (let [result (f acc p)]
          (if (reduced? result)
            @result
            (recur (.-nxt p) result (inc i))))
        acc))))

(extend-protocol IPath
  nil
  (add-to-path [this val]
    (->Path val this)))


(def empty-path (->Path nil nil))

(defmethod print-method Path
  [^Path p ^Writer w]
  (reduce
    (fn [_ ^Path p]
      (.write w (str (.-v p)))
      (.write w " "))
    nil
    p))

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
    (let [next-path (add-to-path p k)]
      (prefix [p k next-path]
              (eduction
                (mapcat (fn [[k v]]
                          (map-value next-path k v)))
                v)))

    (and (sequential? v)
         (not (string? v)))

    (let [next-path (add-to-path p k)]
      (prefix [p k next-path]
              (eduction
                (map-indexed
                  (partial map-value next-path))
                cat
                v)))

    :else
    [[p k v]]))


(defn map-path [v]
  (let [p empty-path]
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

(deftype IndexedData [coll index])

(defn path-seq [^Path path]
  (when path
    (cons path (lazy-seq (path-seq (.-nxt path))))))

(def conj-list (fnil conj '()))

(defn index-data ^IndexedData [coll]
  (->>
    (reduce
      (fn [acc [p a v]]
        (let [acc (-> acc
                      (util/assoc-in! [:eav p a] v)
                      (util/update-in! [:ave a v] conj-list p)
                      (util/update-in! [:vea v p] conj-list a))]
          (if (instance? Path v)
            (reduce
              (fn [acc p]
                (util/update-in! acc [:paths p] conj-list v))
              acc
              p)
            acc)))
      nil
      (map-path coll))
    (->IndexedData coll)))


(defn coll-index [coll]
  (if (instance? IndexedData coll)
    (.-index ^IndexedData coll)
    (if-let [v (get-in u/*query-ctx* [::indicies coll])]
      v
      (let [indexed (.-index (index-data coll))]
        (set! u/*query-ctx* (assoc-in u/*query-ctx* [::indicies coll] indexed))
        indexed))))


(defn query [coll p a v]
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


(defn query-in [coll p [h & t] v]
  (if (seq t)
    (let [cvar (u/lvar)]
      (u/conjunction
        (query coll p h cvar)
        (query-in coll cvar t v)))
    (query coll p h v)))



#_(o/defrule parent-of [data ?p ?c]
  (o/or
    (o/= ?p ?c)
    (o/and
      (query data ?p _ ?ic)
      (u/lazy-rule (parent-of data ?ic ?c)))))

(defn children-paths [data ?p]
  (let [index (:paths (coll-index data))
        inner-fn (fn inner-fn [index parent]
                   (mapcat
                     (fn [v]
                       (cons v (inner-fn index v)))
                     (get index parent)))]
    (inner-fn index ?p)))

(defn parent-of [data ?p ?c]
  (mapcat
    (fn [env]
      (let [data' (u/walk env data)
            p' (u/walk env ?p)
            c' (u/walk env ?c)]
        (util/truth-table [(u/lvar? p') (u/lvar? c')]
                          [false true] (let [index (:paths (coll-index data'))]
                                         (util/efor [c (get index p')]
                                           (assoc env ?c c))))))))






