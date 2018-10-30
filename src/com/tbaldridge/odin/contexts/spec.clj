(ns com.tbaldridge.odin.contexts.spec
  (:require [clojure.spec.alpha :as s]
            [com.tbaldridge.odin.contexts.data :as d]
            [com.tbaldridge.odin :as o]))


(s/def ::name-spec-pair (s/cat
                          ::name keyword?
                          ::spec any?))
(s/def ::or-spec (s/cat
                   ::name #{`s/or}
                   ::pairs (s/+ ::name-spec-pair)))

(s/def ::spec-form (s/or
                     ::or ::or-spec
                     ::symbol symbol?))


(s/def ::named-specs (s/+ (s/cat ::name keyword?
                                 ::spec ::spec)))

(s/def ::or (s/cat ::fn #{`s/or}
                   ::clauses ::named-specs))

(s/def ::and (s/cat ::fn #{`s/and}
                    ::clauses (s/+ ::spec)))

(s/def ::keys-args (s/& (s/alt ::req (s/cat ::arg-name #{:req}
                                            ::spec-args (s/coll-of qualified-keyword?))
                               ::opt (s/cat ::arg-name #{:opt}
                                            ::spec-args (s/coll-of qualified-keyword?))
                               ::req-un (s/cat ::arg-name #{:opt}
                                               ::spec-args (s/coll-of qualified-keyword?))
                               ::opt-un (s/cat ::arg-name #{:opt}
                                               ::spec-args (s/coll-of qualified-keyword?)))
                        ::type-conformer))

(s/def ::keys (s/cat ::fn #{`s/keys}
                     ::clauses (s/+ ::keys-args)))

(s/def ::type-conformer (s/conformer
                          (fn [[type data]]
                            (cond
                              (= type ::keyword)
                              {::type    ::keyword
                               ::keyword data}

                              (= type ::symbol)
                              {::type   ::symbol
                               ::symbol data}

                              (map? data)
                              (assoc data ::type type)

                              :else
                              {::type      ::spec-form
                               ::orig-type type
                               ::form      data}))
                          (fn [data]
                            (let [type (::type data)]
                              (case type
                                ::keyword [type (::keyword data)]
                                ::symbol [type (::symbol data)]
                                ::spec-form [(::orig-type data)
                                             (::form data)]
                                [type data])))))

(s/def ::spec (s/and (s/or ::or ::or
                           ::and ::and
                           ::keys ::keys
                           ::symbol symbol?
                           ::keyword qualified-keyword?)
                     ::type-conformer))

(clojure.pprint/pprint (s/conform ::spec `(s/keys :req [::foo ::bar])))

(s/explain ::spec #_`[:req [::foo]] `(s/keys :req [::foo ::bar]))
(s/unform ::spec (s/conform ::spec `(s/keys :req [::foo ::bar])))
(s/conform ::spec `(s/keys :req [::foo ::bar]))

;;(def frm
;; (clojure.spec.alpha/and clojure.core/simple-symbol?
;;    (clojure.core/fn [%] (clojure.core/not= (quote &) %)))))

;;(s/conform :com.tbaldridge.odin.contexts.spec/spec  frm)
;;this fails us

;; Testing com.tbaldridge.contexts.spec-test
;; :clojure.core.specs.alpha/local-name ->  :clojure.spec.alpha/invalid

;; Couldn't conform  :clojure.core.specs.alpha/local-name
;; (clojure.spec.alpha/and
;;  clojure.core/simple-symbol? (clojure.core/fn [%]
;;                                (clojure.core/not= (quote &) %)))


;;this is just scanning through the registry, looking for
;;specs that we've defined as compatible with
;;odin's notion of specs.  Many if of the specs will
;;print out nonconformity errors, so you get a lot of
;;noise.  During testing it looks like things fail,
;;but they don't.
(defn spec-forms []
  (o/cache-in-context ::specs
    (let [registry (s/registry)]
      (zipmap (keys registry)
              (->> registry
                   (map
                     (fn [[k spec]]
                       (let [result (->> spec
                                         s/form
                                         (s/conform ::spec))]
                         (println k "-> " result)
                         (if (s/invalid? result)
                           ;;recommend changing this to be optional or silent.
                           ;;many registered specs don't conform, it's okay.
                           (println "Couldn't conform " k " " (s/form spec))
                           result)))))))))

(defn spec-query [e a v]
  (d/query (spec-forms) e a v))

(defn spec-query-in [e a v]
  (d/query-in (spec-forms) e a v))

(defn spec-or [path]
  (spec-query path 0 `s/or))

(o/defrule spec-clause [?path ?name ?pred]
  (o/and
    (spec-or ?path)
    (spec-query ?path ?idx ?name)
    (o/when (odd? ?idx))
    (o/project (inc ?idx) ?pred-idx)
    (spec-query ?path ?pred-idx ?pred)))

(defn spec-data [?spec ?data]
  (o/project
    (do (println "SPEC " ?spec)
        (if (keyword? ?spec)
          (let [[type data] (->> (s/registry)
                                 ?spec
                                 s/form
                                 (s/conform ::spec-form))]
            (case type
              ::or (assoc data ::type type ::name ?spec)
              ::symbol {::type ::symbol ::name ?spec ::symbol data}))
          ?spec)) ?data))

(o/defrule component-spec [?spec ?sub-spec]
  (o/and
    (spec-query ?spec ::type ?type)
    (o/or
      (o/and
        (o/when (= ?type ::or))
        (spec-query ?spec ::clauses ?ss)
        (spec-query-in ?ss [_ ::spec] ?sub-spec))

      (o/and
        (o/when (= ?type ::and))
        (spec-query-in ?spec [::clauses _] ?sub-spec))

      (o/and
        (o/when (= ?type ::keyword))
        (spec-query ?spec ::keyword ?kw)
        (spec-query [] ?kw ?sub-spec)))))

(o/defrule non-component-spec [?spec ?non-component]
  (o/and
    (spec-query ?spec ::type ?type)

    (o/or
      (o/and
        (o/when (= ?type ::keys))
        (spec-query-in ?spec [::clauses _ ::spec-args _] ?non-component)))))

(o/defrule component-specs [?spec ?sub-spec]
  (o/or
    (component-spec ?spec ?sub-spec)

    (o/and
      (component-spec ?spec ?ss)
      (o/lazy-rule (component-spec ?ss ?sub-spec)))))

#_(o/defrule sub-spec [?spec ?path ?sub-spec]
    (o/or
      (o/and
        (o/when (keyword? ?spec))
        (o/project
          (-> (s/registry)
              ?spec
              s/form) ?sub-spec)
        (o/= ?spec ?path))

      (o/and
        (o/when (seq? ?spec))
        (o/project
          (s/conform ::spec-form ?spec) ?parsed)
        (map-subspecs ?parsed ?path ?sub-spec))))

#_(o/defrule sub-specs [?spec ?path-in ?path-out ?sub-specs]
  (o/or
    (o/= ?path-in ?path-out)
    (sub-spec ?spec ?path-in ?sub-specs)

    (o/and
      (sub-spec ?spec ?sub ?msp)
      (o/project
        (conj ?path-in ?sub) ?new-path)
      (o/lazy-rule (sub-specs ?msp ?new-path ?path-out ?sub-specs)))))

#_(defn query-spec [data spec path]
  (o/for-query
    (o/and
      (spec-query-in ?spec [::clauses _ ::spec-args _] ?arg))))
