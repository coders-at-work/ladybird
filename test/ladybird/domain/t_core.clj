(ns ladybird.domain.t-core
    (:require [midje.sweet :refer :all]
              [ladybird.domain.core :refer :all]
              [ladybird.data.enum :refer (defenum)]
              [ladybird.data.db-converter :refer (BOOL DATETIME)]
              [ladybird.data.build-in-validator :refer (not-nil is-number)]))

(facts "about defdomain: meta map"
       (fact "will contain domain name automatically if it is not specified"
             (defdomain ADomain [:field])
             ADomain => (contains {:domain-name "ADomain"}))
       (fact "will define domain according to the domain name"
             (defdomain ADomain [:field] {:domain-name "BDomain"})
             BDomain => (contains {:domain-name "BDomain"}))
       (fact "can specify one field as primary key"
             (defdomain ADomain [:id :b] {:primary-key :b})
             ADomain => (contains {:primary-key :b}))
       (fact "will automatically set :id as primary key if primary key is not specified but :id is included in fields"
             (defdomain ADomain [:id :b])
             ADomain => (contains {:primary-key :id}))
       (fact "can specify fields which are completely maintained by database"
             (defdomain ADomain [:a :b :c] {:db-maintain-fields [:b :c]})
             ADomain => (contains {:db-maintain-fields [:b :c]}))
       (fact "can specify fields and their values. These fields will be set to corresponding values when a record is inserted."
             (defdomain ADomain [:a :b] {:add-fixed {:b nil}})
             ADomain => (contains {:add-fixed {:b nil}}))
       (fact "can specify fields which are immutable once created"
             (defdomain ADomain [:a :b] {:immutable-fields [:a :b]})
             ADomain => (contains {:immutable-fields [:a :b]}))
       (fact "can specify fields used by optimistic locking"
             (defdomain ADomain [:a :b] {:optimistic-locking-fields [:a :b]})
             ADomain => (contains {:optimistic-locking-fields [:a :b]}))
       (fact "can specify field converters"
             (defdomain ADomain [:field] {:converters {:field BOOL}})
             ADomain => (contains {:converters {:field BOOL}}))
       (fact "can specify field validators"
             (defdomain ADomain [:field] {:validate {:field not-nil}})
             ADomain => (contains {:validate {:field not-nil}}))
       (fact "can specify type hints"
             (defdomain ADomain [:field] {:type-hints {:field Integer}})
             ADomain => (contains {:type-hints {:field java.lang.Integer}})))

(facts "about defdomain: parsing fields definition"
       (fact "can specify fields solely"
             (defdomain ADomain [:a :b])
             ADomain => (contains {:fields [:a :b]})
             (keys ADomain) =not=> (contains #{:converters :validate}))
       (fact "can specify converters"
             (defdomain ADomain [:field BOOL])
             ADomain => (contains {:converters {:field BOOL}})) 
       (fact "can specify validators"
             (defdomain ADomain [:field BOOL not-nil])
             ADomain => (contains {:validate {:field not-nil}})
             (defdomain ADomain [:field BOOL [not-nil is-number]])
             ADomain => (contains {:validate {:field [not-nil is-number]}}))
       (fact "can ignore some content by specifying '_"
             (defdomain ADomain [:field _ not-nil])
             ADomain => (contains {:validate {:field not-nil} :fields [:field]})
             (keys ADomain) =not=> (contains #{:converters})
             (defdomain ADomain [:field BOOL _])
             ADomain => (contains {:converters {:field BOOL} :fields [:field]})
             (keys ADomain) =not=> (contains #{:validate}))
       (fact "a comprehensive example"
             (defdomain ADomain [:a :b BOOL :c BOOL not-nil :d _ not-nil :e _ [not-nil is-number] :f])
             ADomain => (contains {:fields [:a :b :c :d :e :f] :converters {:b BOOL :c BOOL} :validate {:c not-nil :d not-nil :e [not-nil is-number]}})))

(facts "about defdomain: contents in meta map take precedence over those in fields definition"
       (fact "converters"
             (defdomain ADomain [:a BOOL] {:converters {:a DATETIME}})
             ADomain => (contains {:converters {:a DATETIME}}))
       (fact "validators"
             (defdomain ADomain [:a _ not-nil] {:validate {:a [not-nil is-number]}})
             ADomain => (contains {:validate {:a [not-nil is-number]}})))

(future-facts "about defdomain: domain function names")
(future-facts "about defdomain: domain function implementation")

(facts "about def-typed-domain"
       (fact "can specify fields"
             (def-typed-domain TypedDomain [:field])
             TypedDomain => (contains {:fields [:field]}))
       (fact "can specify type hints"
             (def-typed-domain TypedDomain [:field Integer])
             TypedDomain => (contains {:type-hints {:field java.lang.Integer}})
             (keys TypedDomain) =not=> (contains #{:converters :validate}))
       (fact "can specify converters"
             (def-typed-domain TypedDomain [:field Integer BOOL])
             TypedDomain => (contains {:converters {:field BOOL} :type-hints {:field java.lang.Integer}})
             (keys TypedDomain) =not=> (contains #{:validate}))
       (fact "can specify validators"
             (def-typed-domain TypedDomain [:field Integer BOOL not-nil])
             TypedDomain => (contains {:validate {:field not-nil} :converters {:field BOOL} :type-hints {:field java.lang.Integer}}))
       (fact "'_ can be used to ignore type hint"
             (def-typed-domain TypedDomain [:field _])
             TypedDomain => (contains {:fields [:field]}) 
             (keys TypedDomain) =not=> (contains #{:type-hints :converters :validate}) 
             
             (def-typed-domain TypedDomain [:field _ BOOL])
             TypedDomain => (contains {:converters {:field BOOL}})
             (keys TypedDomain) =not=> (contains #{:type-hints}) 

             (def-typed-domain TypedDomain [:field _ BOOL not-nil])
             TypedDomain => (contains {:validate {:field not-nil} :converters {:field BOOL}}) 
             (keys TypedDomain) =not=> (contains #{:type-hints}) )
       (fact "'_ can be used to ignore converter"
             (def-typed-domain TypedDomain [:field _ _])
             TypedDomain => (contains {:fields [:field]}) 
             (keys TypedDomain) =not=> (contains #{:type-hints :converters :validate}) 
             
             (def-typed-domain TypedDomain [:field Integer _])
             TypedDomain => (contains {:type-hints {:field java.lang.Integer}})
             (keys TypedDomain) =not=> (contains #{:converters :validate})

             (def-typed-domain TypedDomain [:field Integer _ not-nil])
             TypedDomain => (contains {:validate {:field not-nil} :type-hints {:field Integer}}) 
             (keys TypedDomain) =not=> (contains #{:converters}))
       (fact "'_ can be used to ignore validator"
             (def-typed-domain TypedDomain [:field _ _ _])
             TypedDomain => (contains {:fields [:field]}) 
             (keys TypedDomain) =not=> (contains #{:type-hints :converters :validate}) 
             
             (def-typed-domain TypedDomain [:field Integer _ _])
             TypedDomain => (contains {:type-hints {:field java.lang.Integer}})
             (keys TypedDomain) =not=> (contains #{:converters :validate})

             (def-typed-domain TypedDomain [:field Integer BOOL _])
             TypedDomain => (contains {:type-hints {:field java.lang.Integer} :converters {:field BOOL}})
             (keys TypedDomain) =not=> (contains #{:validate}))
       (fact "contents in meta map take precedence over those in fields definition"
             (def-typed-domain TypedDomain [:field Integer BOOL not-nil] {:type-hints {:field Long} :converters {:field DATETIME} :validate {:field [not-nil is-number]}})
             TypedDomain => (contains {:type-hints {:field java.lang.Long} :converters {:field DATETIME} :validate {:field [not-nil is-number]}}))
       (fact "a comprehensive example"
             (def-typed-domain TypedDomain [:a :b Integer :c Integer BOOL :d Integer BOOL [not-nil is-number]
                                            :e _ BOOL :f _ _ not-nil :g Integer _ not-nil :h])
             TypedDomain => (contains {:fields [:a :b :c :d :e :f :g :h]
                                       :type-hints {:b Integer :c Integer :d Integer :g Integer}
                                       :converters {:c BOOL :d BOOL :e BOOL}
                                       :validate {:d [not-nil is-number] :f not-nil :g not-nil}
                                       }))
       (fact "a comprehensive expansion"
             (def-typed-domain TypedDomain [:a :b Integer :c Integer BOOL :d Integer BOOL [not-nil is-number]
                                            :e _ BOOL :f _ _ not-nil :g Integer _ not-nil :h] {:type-hints {:c Long}})
                       =expands-to=>
                       (ladybird.domain.core/defdomain TypedDomain [:a :b _ _ :c BOOL _ :d BOOL [not-nil is-number] :e BOOL _ :f _ not-nil :g _ not-nil :h _ _] {:type-hints {:c Long}})))

(facts "about def-enum-predicates"
       (future-fact "If the domain is the capitalized camel case of the last section of the containing namespace name, def-enum-predicates can define predicate functions named by field and enum key if the enum field is provided"
             (defenum STATUS :active "A" :inactive "I")
             (defdomain Member [:status STATUS])
             (def-enum-predicates :status))
       (fact "can define predicate functions named by caller for an enum field in the specified domain if domain, field and functions name mapping are provided"
             (defenum STATUS :active "A" :inactive "I")
             (defdomain Member [:status STATUS])
             (def-enum-predicates Member :status {:active status-active? :inactive status-inactive?})
             (status-active? nil) => false
             (status-active? {}) => false
             (status-active? {:status :active}) => true
             (status-inactive? {:status :inactive}) => true
             (status-active? {:status :inactive}) => false))
