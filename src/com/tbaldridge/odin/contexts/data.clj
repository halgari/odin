(ns com.tbaldridge.odin.contexts.data
  (:require [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.util :as util]))

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

(defn index-data [coll]
  (reduce
    (fn [acc [p a v]]
      (-> acc
          (assoc-in [:eav p a] v)
          (update-in [:ave a v] conj p)
          (update-in [:vea v p] conj a)))
    {}
    (map-path coll)))


(defn coll-index [coll]
  (if-let [v (get (u/*query-ctx* ::indicies) coll)]
    v
    (let [indexed (time (index-data coll))]
      (set! u/*query-ctx* (assoc-in u/*query-ctx* [::indicies coll] indexed))
      indexed)))

(defn just [x]
  (reify
    clojure.lang.IReduceInit
    (reduce [this f init]
      (unreduced (f init x)))))


(defn query [coll p a v]
  (let [index (coll-index coll)]
    (mapcat
      (fn [env]
        (let [p' (u/walk env p)
              a' (u/walk env a)
              v' (u/walk env v)]
          (condp = [(u/lvar? p') (u/lvar? a') (u/lvar? v')]

            [true false true] (util/efor [[v es] (get-in index [:ave a'])
                                          e es]
                                         (-> env
                                             (u/unify p' e)
                                             (u/unify v' v)))
            [false false true] (when-some [v (get-in index [:eav p' a'])]
                                 (just (assoc env v' v)))

            [false true true] (util/efor [[a v] (get-in index [:eav p'])]
                                         (assoc env a' a v' v))

            [true false false] (util/efor [e (get-in index [:ave a' v'])]
                                          (u/unify env p' e))

            [false true false] (util/efor [a (get-in index [:vea v' p'])]
                                          (u/unify env a' a))

            [false false false] (when (= v' (get-in index [:eav p' a']))
                                  (just env))

            [true true true] (util/efor [[e avs] (get index :eav)
                                         [a v] avs]
                                        (-> env
                                            (u/unify p' e)
                                            (u/unify a' a)
                                            (u/unify v' v)))))))))


(defn query-in [coll p [h & t] v]
  (if (seq t)
    (let [cvar (u/lvar)]
      (u/conjunction
        (query coll p h cvar)
        (query-in coll cvar t v)))
    (query coll p h v)))

(defmacro lazy-rule [expr]
  `(mapcat
     (fn recursive-call [env#]
       (eduction
         ~expr
         (just env#)))))

(o/defrule parent-of [data ?p ?c]
  (o/or
    (o/= ?p ?c)
    (o/and
      (query data ?p _ ?ic)
      (lazy-rule (parent-of data ?ic ?c)))))






