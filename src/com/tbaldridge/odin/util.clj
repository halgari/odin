(ns com.tbaldridge.odin.util
  (:require [clojure.walk :as walk]
            [clojure.string :as str])
  (:import (java.util Map HashMap Set HashSet)))


(defn first-rf
  ([] nil)
  ([acc] acc)
  ([acc itm]
   (reduced itm)))

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

(defn query-var? [v]
  (and (symbol? v)
       (not (namespace v))
       (str/starts-with? (name v) "?")))


(defn body-lvars [form]
  (let [lvars (atom #{})
        form  (walk/postwalk
                (fn [v]
                  (cond
                    (query-var? v)
                    (do (swap! lvars conj v)
                        v)

                    (= v '_)
                    `(com.tbaldridge.odin.unification/lvar)

                    :else v))
                form)]


    [@lvars form]))


(defn transform [data pth f & args]
  (let [inner (fn transform-inner [data [h & t :as path] f args]
                (if path
                  (if-let [sub-data (get data h)]
                    (let [result (transform-inner sub-data t f args)]
                      (if (identical? sub-data result)
                        data
                        (assoc data h result)))
                    data)
                  (apply f data args)))]
    (inner data pth f args)))

(defn assoc-in! [^Map coll [h & t] v]
  (let [^Map coll (or coll (HashMap.))]
    (if t
      (.put coll h (assoc-in! (get coll h) t v))
      (.put coll h v))
    coll))

(defmacro massoc-in! [coll [h & t] v]
  (let [coll-sym (with-meta (gensym "coll") {:tag 'java.util.Map})]
    `(let [~coll-sym (or ~coll {} #_(HashMap. 64))]
       ~(if t
          `(assoc ~coll-sym ~h (massoc-in! (get ~coll-sym ~h) ~t ~v))
          `(assoc ~coll-sym ~h ~v))
       #_~coll-sym)))

(defmacro mdissoc-in! [coll [h & t]]
  (let [coll-sym (gensym "coll")]
    `(when-let [~coll-sym ~coll]
       ~(if t
          `(if-let [c# (mdissoc-in! (get ~coll-sym ~h) ~t)]
             (assoc ~coll-sym ~h c#)
             (not-empty (dissoc ~coll-sym ~h)))
          `(not-empty (dissoc ~coll ~h))))))


(defn dissoc-in! [^Map coll [h & t] v]
  (when coll
    (if t
      (if (dissoc-in! (get coll h) t v)
        coll
        (do (.remove coll h)
            (not-empty coll)))
      (do (.remove coll h)
          (not-empty coll)))))



(comment
  (let [m (assoc-in! nil [:a :b :c] :d)]
    (dissoc-in! m [:a :b] :c)
    )

  (meta (first (second (clojure.walk/macroexpand-all '(massoc-in! nil [:a :b :c] :d)))))

  (meta ^Map [])


  (massoc-in! nil [:a :b :c] :d)

  )

(defn update-in! [coll path f & args]
  (let [pfn (fn inner [^Map coll [h & t] f args]
              (println coll path h t )

              (let [^Map coll (or coll (HashMap.))]
                (if t
                  (let [prev   (get coll h)
                        result (inner prev t f args)]
                    (println "RES " h result)
                    (if result
                      (if (identical? prev result)
                        coll
                        (.put coll h result))
                      (do (.remove coll h)
                          (when (pos? (count coll))
                            coll))))
                  (do (.put coll h (apply f (get coll h) args))
                      (when (pos? (count coll))
                        coll)))
                coll))]
    (pfn coll path f args)))


(defn conj-set! [^Set coll itm]
  (let [^Set s (if (nil? coll)
                 (HashSet.)
                 coll)]
    (.add s itm)
    s))

(defn disj-set! [^Set coll itm]
  (.remove coll itm))


(defn truth-table-impl [path vars forms]
  (if (= (count path) (count vars))
    (or (forms path)
        `(throw (IllegalArgumentException. ~(str "No matching clause: " path))))
    `(if ~(nth vars (count path))
       ~(truth-table-impl (conj path true) vars forms)
       ~(truth-table-impl (conj path false) vars forms))))

(defmacro truth-table [vars & {:as bodies}]
  (truth-table-impl [] vars bodies))

