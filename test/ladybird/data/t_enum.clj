(ns ladybird.data.t-enum
    (:require [midje.sweet :refer :all]
              [ladybird.data.enum :refer :all]
              [ladybird.data.converter-core :refer (in-fn out-fn)]
     ))

(facts "about defenum"
       (fact "will define a converter named by the first argument"
             (defenum E :a 1 :b 2)
             (in-fn E) => ifn?
             (out-fn E) => ifn?)
       (fact "will define a validate fn named by prefixing 'enum:' to the first argument"
             (defenum E :a 1 :b 2)
             enum:E => fn?)
       (fact "the in-fn of converter converts values to keys, and the out-fn converts keys to values"
             (defenum E :a 1 :b 2)
             ((in-fn E) 1) => :a
             ((out-fn E) :b) => 2)
       (fact "the validate fn validates if the argument is one of the enum keys"
             (defenum E :a 1 :b 2)
             (enum:E :a) => true
             (enum:E 1) => (just [::enum:E string?]))
       (fact "the enum converter contains [:type :ladybird.data.enum/enum] map entry"
             (defenum E :a 1 :b 2)
             E => (contains {:type :ladybird.data.enum/enum})))

(facts "about enum utilities"
       (fact "can tell whether a data structure is an enum"
             (defenum E :a 1 :b 2)
             (enum? E) => true
             (enum? 1) => false
             (enum? {}) => false
             (enum? nil) => false
             (enum? ()) => false))
