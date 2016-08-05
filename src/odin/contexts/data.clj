(ns odin.contexts.data
  (:require [odin.unification :as u]
            [odin.util :as util]))

(defn prefix [itm rc]
  (reify
    clojure.lang.IReduceInit
    (reduce [this f init]
      (let [acc (f init itm)]
        (println "Reduced? " (reduced? acc))
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
  (println "CTX -> " (pr-str u/*query-ctx*))
  (if-let [v (get (u/*query-ctx* ::indicies) coll)]
    v
    (let [indexed (index-data coll)]
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
          (println "LVARs-> " [(u/lvar? p') (u/lvar? a') (u/lvar? v')])
          (println "vals" p' a' v')

          (condp = [(u/lvar? p') (u/lvar? a') (u/lvar? v')]

            [true false true] (util/efor [[v es] (get-in index [:ave a'])
                                          e es]
                                         (do (println "v e" v e)
                                             (-> env
                                                 (u/unify p' e)
                                                 (u/unify v' v))))
            [false false true]  (just (assoc env v' (get-in index [:eav p' a'])))

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
  (println "->DAT " p h t v)
  (if (seq t)
    (let [cvar (u/lvar)]
      (u/conjunction
        (query coll p h cvar)
        (query-in coll cvar t v)))
    (query coll p h v)))



(def test-data
  [{:name "Bill"
    :gender :male
    :children ["Sally" "Sam" "Jake"]}
   {:name "Jane"
    :gender :female
    :children ["Sally" "Sam"]}
   {:name "Beth"
    :gender :female
    :children ["Jake"]}
   {:name "Horace"
    :gender :male
    :children ["Bill"]}
   {:name     "Judith"
    :gender :female
    :children ["Bill"]}])

(:vea (index-data test-data))

(= (set (u/for-query
          (query test-data ?e ?a ?v)
          [?e ?a ?v]))
   (set (map-path test-data)))

(defn parents [d ?p1 ?p2 ?c]
  (let [idx1 (u/lvar)
        idx2 (u/lvar)
        ?pp1 (u/lvar)
        ?pp2 (u/lvar)]
    (u/conjunction
      (query-in d ?pp1 [:children idx1] ?c)
      (query d ?pp1 :gender :male)
      (query d ?pp1 :name ?p1)
      (query-in d ?pp2 [:children idx2] ?c)
      (query d ?pp2 :gender :female)
      (query d ?pp2 :name ?p2))))

(defn procreated [d ?p1 ?p2]
  (parents d ?p1 ?p2 (u/lvar)))

(vec (u/for-query
       (parents test-data ?p1 ?p2 ?c)
       {:father ?p1
        :mother ?p2
        :child ?c}))

(vec (u/for-query
       (u/conjunction
         (query test-data ?pp :name ?pn)
         (query-in test-data ?pp [:children ?idx] ?cn))
       {:parent ?pn
        :child  ?cn
        :nth    (inc ?idx)}
       ))