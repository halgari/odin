(ns com.tbaldridge.odin
  (:refer-clojure :exclude [or and = update when])
  (:require [com.tbaldridge.odin.unification :as u]
            [com.tbaldridge.odin.tabling :as tabling]
            [com.tbaldridge.odin.util :as util]
            [clojure.core :as clj]))

(defn lvar
  "Creates a new logic variable"
  []
  (u/lvar))

(defn lvar?
  "Returns truthy if x is a logic variable"
  [x]
  (u/lvar? x))

(defn and
  "Creates a conjunction between one or more query clauses"
  [& clauses]
  (apply u/conjunction clauses))

(defn or
  "Creates a disjunction between one or more query clauses"
  [& clauses]
  (apply u/disjunction clauses))

(defn =
  ([a] a)
  ([a b] (u/== a b))
  ([a b & c]
    (apply = (= a b) c)))


(defmacro for-query
  "Takes a query and projection and returns a reducible collection
  of the results of the query projected through the projection. Any
  symbols in the body of the query or the projection that start with ?
  will be automatically created in-scope as an lvar."
  [query projection]
  (u/for-query-impl query projection))


(defmacro when
  "Filters out any results were pred-clause is not truthy. Any symbols
  that start with ? in the pred-clause will be considered lvars and will
  be automatically walked from each envrionment."
  [pred-clause]
  (u/pass-impl pred-clause))

(defmacro project
  "Evaluates an expression and unifies it to the given lvar. Any symbols
  that start with ? in the expr will be considered lvars and will be
  automatically walked from each environment."
  [& clauses]
  (u/project-impls clauses))

(defmacro defrule
  "Creates a rule function with additional syntactic sugar. Any symbols
  that start with ? in the body that are not also arguments are considered
  fresh lvars and will be created and kept in scope in the body. If the
  name's metadata contains {:tabled true} then the rule will be tabled. "
  [name args & body]
  (if (:tabled (meta name))
    (tabling/defrule-impl name args body)
    (u/defrule-impl name args body)))

(defn =
  "Creates a query that unifies two more more values or lvars."
  ([a b]
   (u/== a b))
  ([a b & rest]
   (apply and (= a b) rest)))

(defmacro lazy-rule
  "Rules in Odin are eagerly created. This means that recursive rules
  cause stack exceptions when they are instantiated. Wrap any goal in
  this macro to stop the eager instantiation and keep the stack overflow
  from happening. "
  [expr]
  (u/lazy-rule-impl expr))

(defn log
  [prefix & args]
  (map
    (fn [env]
      (apply println prefix (map (partial u/walk env) args))
      env)))

(defmacro with-query-ctx
  "There are several bits of information (indicies and the like) that are
  created during query execution. This data is stored in the query context.
  This context is thrown away after the query completes. Wrap one or more
  query executions with a single with-query-ctx to reuse a query contex
  from one execution to the other. "
  [& body]
  `(binding [u/*query-ctx* (clj/or u/*query-ctx* {})]
     ~@body))



(def ^{:doc "A goal that always fails."}
    fail (filter (constantly false)))

#_(defmacro transform-query [data body]
  (u/transform-query-impl data body))

#_(defn transform [location f & args]
  (u/transform location f args))

(defmacro transform [data q f & args]
  (u/transform-query-impl data q f args))

(defn update
  ([?path]
    (==
      ?path u/xform-path))
  ([?path ?attr]
    (and
      (= ?path u/xform-path)
      (= ?attr u/xform-attr))))

