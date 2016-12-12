# Odin

An embedded extensible logic programming DSL for CLojure


# Rationale 

Clojure programmers tend to prefer data-structures over objects for transferring and storing information. Often
manipulations of these structures consists of collecting information from one set of data using reduce, walk, or recursive functions,
and then these results are projected into output collections to be processed by other functions before finally being written
to an output data store or perhaps transmitted to some other client. 

These operations of reduce, walk and processing functions could be viewed as ad-hoc, hard coded query languages.
Odin aims to simplify these operations by providing a generic, logic based, query language for Clojure data sources. These sources could be raw Clojure data structures, XML,
databases, etc. 

Other logic languages may have other trade-offs and benefits, but Odin attempts to hit the "sweet spot" between performance,
 extesibility and ease of use. Other languages will be faster, but Odin attepts to be a "drop in and use" solution where
  the total run time of a query is not the top priority. 


# Tutorial 

First of all we need to import the proper namespaces. Most of the code involved in querying data is found in `com.tbaldridge.odin`.
In addition getting any sort of work done with Odin will require the use of a data context. A good starting context is the data
context found in `com.tbaldridge.odin.contexts.data`. 

     (ns example
       (:require [com.tbaldridge.odin :as o]
                 [com.tbaldridge.odin.contexts.data :as d]))
                 
                 
The most common way to query data is the macro known as `for-query`. This macro acts somewhat like `clojure.core.for` except that
the datasource is a query instead of a set of sequences. The syntax of `for-query` is thus:

     (for-query
       <query>
       <projection>)
       
Let's say we have this collection of data:

     (def data {:val 40
                     :sub {:val 3
                           :sub {:val -1}}})
                      
Using this data we can query for all the values found under a :val key.

    (o/for-query
      (d/query data ?path :val ?val)
      ?val)

For optimization and performance reasons, `for-query` returns a opaque object that implements Clojure's reduce interfaces, but
we can simply hand this result to a function like `clojure.core.set` to realize the results:

    (set (o/for-query
           (d/query data ?path :val ?val)
           ?val))   
           
    ;=> #{40 3 -1}
    
As we can see, `d/query` presents a tuple interface on Clojure data collections. This tuple consists of a path (much like
a path that would be handed to `get-in`), an attribute, and a value. The second part of the `for-query` is the projection
and it's simply a form that describes what value should be returned for each value found.

For more advanced queries, Odin supports conjunctions, and predicate guards:


    (set (o/for-query
           (o/and
             (d/query data ?path :val ?val)
             (o/when (odd? ?val)))
           ?val))
           
    ;=> #{3 -1}
           
`o/and` provides a way to combine multiple queries via a conjunction (or a "and"). `o/when` takes a single argument form. This form should return false if a given expression should be filtered out of the result set. 

Projections (the second form in the `for-query`) are simply Clojure expressions where the query values (symbols prefixed
with `?`) are bound to query results. We can use arbitrary Clojure logic in these forms. In addition, it should be mentioned
that any query parameter that is specified using `_` is interpreted as a wildcard. 
 
    (def data {:a 1 :b 2 :c 3})
    
    (into {}
      (for-query
        (o/query data _ _ ?val)
        [?val (* ?val ?val)]))
        
    ;; => {1 1
           2 4
           3 9}
           

Relationships between query clauses can be defined by using `o/and`. For example, let's find out the balance of all 
bank accounts:

    (def accounts {:fred {:credits 1000 :debits 500}
                   :sam {:credits 220 :debits 300}
                   :sue {:credits 3300 :debits 100}
                   :jane {:credits 2000 :debits 1000}})
                   
    (into {}
      (o/for-query
        (o/and
          (d/query accounts ?account :credits ?credits)
          (d/query accounts ?account :debits ?debits)
          (d/query accounts _ ?name ?account))
        [?name (- ?credits ?debits)]))
        
    ;=> {:fred 500
         :sam -80
         :sue 3200
         :jane 1000}
         
### Transforming data

Querying a data structure is fairly useful, but often the results of a query will simply be used to drive the transformation
of another data structure. This is why Odin also provides the `transform` macro, let's write a transformation query
that looks for overdrawn bank accounts and updates the accounts by adding a attribute called `:overdrawn/balance` that
specifies the negative balance of the account. 



    (o/transform
        (o/and
          (d/query data ?account :credits ?credits)
          (d/query data ?account :debits ?debits)
          (o/project
            (- ?credits ?debits) ?balance
          (o/when (neg? ?balance))
          (o/update ?account))
        assoc :overdrawn/balance ?balance)

    ;=> {:fred {:credits 1000 :debits 500}
         :sam {:credits 220 :debits 300 :overdrawn/amount -80}
         :sue {:credits 3300 :debits 100}
         :jane {:credits 2000 :debits 1000}}
    
        
There's a few new constructs in this query. First of all we see the use of `o/project`, this macro provides a way
of injecting arbitrary Clojure code into the middle of a query. The first form of `o/project` specifies a clojure expression, 
and the second form specifies how to bind the expression's result in the query execution. Here we are calculating
the balance of an account and storing it in `?balance`. 

The next construct we see is `o/update`. This clause works in conjunction with `o/transform` and provides context to the
transformation point. When using `clojure.core/update-in` we provide a path that specifies the part of the data structure
to modify, so `o/transform` uses the location specified by `o/update` to specifiy where to run `assoc`. The syntax of 
`o/transform` is then:

    (o/transform query-that-contains-update-clause
       f & args-for-f)
           
           

### More on projections

The `o/projection` construct provides a very powerful way to create new data sources in Odin.

    (set (o/for-query
            (o/project
              (range 3) [?i ...]
              (* ?i ?i) ?squared)
            ?squared))
            
    ;=> #{0 1 4}
    
The binding form `[?var ...]` specifies that the result of the projection is a reducible collection and that each value
found in the reducing operation should be bound to `?val`. Using `o/project` in conjunction with `o/switch` allows for new
data contexts to be created quickly and easily. For example, this is the entire sourcecode listing for Odin's integration
with Datomic. 


    (ns com.tbaldridge.odin.contexts.datomic
      (:require [datomic.api :as d]
                [com.tbaldridge.odin :as o]))
    
    
    (o/defrule datoms [?db ?e ?a ?v]
      (o/switch
        [?e ?a ?v] (o/when
                     (first (d/datoms ?db :eavt ?e ?a ?v)))
    
        [?e ?a _] (o/project
                    (d/datoms ?db :eavt ?e ?a) [[_ _ ?v] ...])
    
        [?e _ _] (o/project
                   (d/datoms ?db :eavt ?e) [[_ ?a ?v] ...])
    
        [_ ?a ?v] (o/project
                    (d/datoms ?db :avet ?a ?v) [[?e] ...])
    
        [_ ?a _] (o/project
                    (d/datoms ?db :avet ?a) [[?e _ ?v] ...])
    
        [_ _ ?v] (o/project
                   (d/datoms ?db :vaet ?v) [[?e ?a] ...])))


The `o/switch` macro provides a sort of `case` or `switch` construct in which each branch specifies a query to execute
if the provided vars are bound. 
  
## Q/A

### Q: Why would I use this over Datomic Datalog?
A: Datalog is set-based, therefore you always get all the answers. Odin's query language is lazy, you can get one answer, 100, or 
all the answers to a query, only as many answers as are requested will be processed. Odin also supports querying efficiently
over Clojure data. This can also be done with Datomic Datalog, but it's not as streamlined. Datomic's approach does have 
benifits, the set-based approach will vastly outperform Odin's lazy approach when all results are required.

### Q: Why would I use this over core.logic?
A: Core.Logic is a more general purpose logic language. Odin is aimed to be a query language that is easy to extend. Core.Logic's
use of monads may carry less restrictions, but they pose a challenge to anyone looking to integrate with the library. Odin uses
transducers and as such is extensible with Clojure primitives like `mapcat` and `keep`.

### Q: Why shouldn't I use Odin?
A: Odin is fairly generic, and as such will probably not out perform more optimized tailor-made solutions. 


## Prior Work

### muKanren
The core of this engine is a highly modified variant of muKanren. The paper found here,
is invaluable: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

## License

Copyright © 2016 Timothy Baldridge

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
