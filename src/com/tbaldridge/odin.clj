(ns com.tbaldridge.odin
  (:refer-clojure :exclude [or and =])
  (:require [com.tbaldridge.odin.unification :as u]))

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


(defmacro for-query
  "Takes a query and projection and returns a reducible collection
  of the results of the query projected through the projection. Any
  symbols in the body of the query or the projection that start with ?
  will be automatically created in-scope as an lvar."
  [query projection]
  (u/for-query-impl query projection))

(defmacro transform-query [query [f p & args]]
  (let [vars (util/filter query-var? args)])
  `(reduce

     (for-query
       ~@query
       ~@(filter ))))

(transform-query
  (query data ?p :c 2)
  (inc ?p :c 2))

(defmacro pass
  "Filters out any results were pred-clause is not truthy. Any symbols
  that start with ? in the pred-clause will be considered lvars and will
  be automatically walked from each envrionment."
  [pred-clause]
  (u/pass-impl pred-clause))

(defmacro project
  "Evaluates an expression and unifies it to the given lvar. Any symbols
  that start with ? in the expr will be considered lvars and will be
  automatically walked from each environment."
  [expr lvar]
  (u/project-impl expr lvar))

(defmacro defrule
  "Creates a rule function with additional syntactic sugar. Any symbols
  that start with ? in the body that are not also arguments are considered
  fresh lvars and will be created and kept in scope in the body."
  [name args & body]
  (u/defrule-impl name args body))

(defn =
  "Creates a query that unifies two more more values or lvars."
  ([a b]
   (u/== a b))
  ([a b & rest]
   (apply and (= a b) rest)))