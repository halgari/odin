(ns com.tbaldridge.odin.contexts.random
  (:require [clojure.data.generators :as g]
            [clojure.test.check.random :as r]
            [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.unification :as u]))

(defn rngs
  ([]
    (rngs (r/make-random)))
  ([seed-or-rng]
   (let [initial (if (integer? seed-or-rng)
                   (r/make-random seed-or-rng)
                   seed-or-rng)]
     (reify
       clojure.lang.IReduceInit
       (reduce [this f init]
         (loop [acc init
                rng initial]
           (let [[curr next] (r/split rng)
                 result (f acc curr)]
             (if (reduced? result)
               @result
               (recur result next)))))))))

(transduce
  (comp
    (map r/rand-long)
    (take 3))
  conj
  (rngs))

(o/defrule or-split [?rnd ?rnd-out & ?clauses]
  (o/and
    (o/project
      (map vector
           (r/split-n ?rnd (count ?clauses))
           ?clauses) [[?rnd-out ?clause] ...])
    (o/invoke ?clause)))

(o/defrule split-rngs [?rng & ?rngs]
  (o/project
    (r/split-n ?rng (count ?rngs)) ?rngs))

(o/defrule one-of [?options ?out]
  (o/project
    (g/rand-nth ?options) ?out))

(o/defrule accumulate-when [?data pred]
  (let [acc (atom #{})]
    (o/and
      (o/project
        (do (println "d " ?data) 4) ?v)
      (o/when (pred @acc ?data))
      (o/project
        (do
          (println "->d" @acc ?data)
          (swap! acc conj ?data)) ?s))))

(comment

  (vec
    (eduction
      (take 1)
      (o/for-query
        (o/and
          (o/project
            (rngs 3) [?rnd ...])
          (or-split ?rnd ?rnd-out
                    (o/project
                      (r/rand-long ?rnd-out) ?val)
                    (o/project
                      (r/rand-double ?rnd-out) ?val)))
        ?val)))

  (macroexpand '(o/defrule split-rngs [?rng & ?rngs]
                  (o/project
                    (do (println "h")
                        true) ?v)
                  (o/project
                    (do
                      (println ")> " ?rngs)
                      (do
                        (println ?rngs)
                        (r/split-n ?rng (count ?rngs)))) ?gens)
                  (o/= ?gens ?rngs)))
  (vec
    (eduction
      (take 10)
      (o/for-query
        (o/and
          (o/project
            (rngs 32) [?rng ...])
          (split-rngs ?rng ?a ?b)
          (o/project
            (int (* 10 (r/rand-double ?a))) ?d)
          (accumulate-when ?d (complement contains?)))
        ?d))))

