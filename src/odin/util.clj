(ns odin.util)



(defmacro efor [[bind coll & rest] body]
  (if rest
    (cond
      (= :let bind)
      `(let [~@coll]
         (efor ~rest ~body))

      (= :when bind)
      `(when ~coll
         (efor ~rest ~body))

      :else
      `(eduction
         (mapcat (fn [~bind]
                   (efor ~rest ~body)))
         ~coll))

    (cond
      (= :let bind)
      `(let [~@coll]
         (cons ~body nil))

      (= :when bind)
      `(when ~coll
         (cons ~body nil))

      :else
      `(eduction
         (map
           (fn [~bind]
             ~body))
         ~coll))))

