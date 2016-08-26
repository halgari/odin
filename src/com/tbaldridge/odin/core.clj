(ns com.tbaldridge.odin.core)

(defn extract-vars [env vars new-vars]
  (zipmap
    new-vars
    (into [] (map (partial walk env) vars))))

(defn inject-vars [env vars new-vars]
  (let [v (into [] (map (partial walk env) new-vars))]
    (reduce-kv
      (fn [env i itm]
        (assoc env (nth vars i) itm))
      env
      v)))

(defn memoized-impl [inner-args inner-xf]
  (let [a (atom {})]
    (fn [& args]
      (mapcat
        (fn [env]
          (let [key     (into []
                              (map-indexed
                                (fn [i v]
                                  (let [v (walk env v)]
                                    (if (lvar? v)
                                      (nth inner-args i)
                                      v))))
                              args)
                new-env (reduce-kv
                          (fn [acc i v]
                            (if (not (lvar? v))
                              (assoc acc (nth inner-args i) v)
                              acc))
                          {}
                          key)
                results (transduce inner-xf conj [new-env])]
            (println inner-args key new-env results)
            nil))))))

(let [inner-x (lvar)
      inner-y (lvar)
      outer-x (lvar)]
  (println inner-x inner-y)
  (transduce
    ((memoized-impl [inner-x inner-y]
                    (mapcat
                      (fn [env]
                        (println "r" (range (walk env inner-y)))
                        (eduction
                          (map (partial unify env inner-x))
                          (range (walk env inner-y))))))
      outer-x 42)
    conj
    [{}]))


(def test-data
  [{:name "Bill"}
   ])