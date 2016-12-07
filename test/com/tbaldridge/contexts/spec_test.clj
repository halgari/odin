(ns com.tbaldridge.contexts.spec-test
  (:require [clojure.test :refer [deftest is]]
            [com.tbaldridge.odin :as o]
            [com.tbaldridge.odin.contexts.spec :as sc]
            [clojure.spec :as s]))


(s/def ::some-int integer?)
(s/def ::some-float float?)
(s/def ::some-number (s/or ::int ::some-int
                           ::float ::some-float))

(s/def ::pos-int (s/and ::some-int pos?))

(s/def ::map (s/keys :req [::some-int ::some-float]))


(deftest basic-resolution-tests
  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::some-int ?s)
                  (sc/spec-query ?s ::sc/type ::sc/symbol)
                  (sc/spec-query ?s ::sc/symbol ?sym))
                ?sym))
         #{`integer?}))

  (is (= (set (o/for-query
                (sc/spec-data ::some-int ?s)
                ?s))
         #{{::sc/type ::sc/symbol
            ::sc/name ::some-int
            ::sc/symbol `integer?}}))

  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::some-number ?spec)
                  (sc/component-spec ?spec ?s)
                  (sc/spec-query-in ?s [::sc/keyword] ?kw))
                ?kw))
         #{::some-float ::some-int}))

  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::some-number ?spec)
                  (sc/component-specs ?spec ?s)
                  (sc/spec-query ?s ::sc/symbol ?kw))
                ?kw))
         #{`float? `integer?}))

  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::some-number ?spec)
                  (sc/component-specs ?spec ?s)
                  (o/or
                    (sc/spec-query ?s ::sc/keyword ?val)
                    (sc/spec-query ?s ::sc/symbol ?val)))
                ?val))
         #{`float? `integer?
           ::some-float ::some-int}))

  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::pos-int ?spec)
                  (sc/component-specs ?spec ?s)
                  (sc/spec-query ?s ::sc/symbol ?kw))
                ?kw))
         #{`integer? `pos?}))

  (is (= (set (o/for-query
                (o/and
                  (sc/spec-query [] ::map ?spec)
                  (sc/non-component-spec ?spec ?s))
                ?s))
         #{::some-float ::some-int}))



  )
