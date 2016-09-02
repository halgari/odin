# odin

An embedded extensible logic programming DSL for CLojure


# Rationale 

Clojure programmers tend to prefer data-structures over objects for transferring and storing information. Often
manipulations of these structures consists of collecting data from one set of data using reduce, walk, or recursive functions,
and then these results are projected into output collections to be processed by other functions before finally being written
to an output data store or perhaps transmitted to some other client. 

These collections of reduce, walk and processing functions could be viewed as ad-hoc, hard coded query languages. Odin aims to
be a generic, logic based, query language for Clojure data sources. These sources could be raw Clojure data structures, XML,
databases, etc. 

Other logic languages may have other trade-offs and benefits, but Odin attempts to hit the "sweet spot" between performance,
 extesibility and ease of use. Other languages will be faster, but Odin attepts to be a "drop in and use" solution where
  the total run time of a query is not the top priority. 
  
  
## Q/A

### Q: Why would I use this over Datomic Datalog?
A: Datalog is set-based. You always get all the answers. Odin's query language is lazy, you can get one answer, 100, or 
all the answers to a query, only as many answers as are requested will be processed. 

### Q: Why would I use this over core.logic?
A: Core.Logic is a more general purpose logic language. Odin is aimed to be a query language that is easy to extend. Core.Logic's
use of monads may carry less restrictions, but they pose a challenge to anyone looking to integrate with the library. Odin uses
transducers and as such is extensible with Clojure primitives like `mapcat` and `keep`.



## License

Copyright © 2016 Timothy Baldridge

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
