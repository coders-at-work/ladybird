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
             (defenum E :a 1 :b 2 c 3 4 5)
             ((in-fn E) 1) => :a
             ((in-fn E) 3) => 'c
             ((in-fn E) 5) => 4
             ((out-fn E) :b) => 2
             ((out-fn E) 'c) => 3
             ((out-fn E) 4) => 5)
       (fact "if a key is an instance of clojure.lang.Named, the out-fn of converter will also map (name key) to key's value"
             (defenum E "a" 1 :b 2 c 3)
             ((out-fn E) "a") => 1
             ((out-fn E) "b") => 2
             ((out-fn E) "c") => 3)
       (fact "the validate fn validates if the argument is one of the enum keys"
             (defenum E :a 1 :b 2)
             (enum:E :a) => true
             (enum:E 1) => (just [::enum:E string?]))
       (fact "the enum converter contains [:type :ladybird.data.enum/enum] map entry"
             (defenum E :a 1 :b 2)
             E => (contains {:type :ladybird.data.enum/enum}))
       (fact "the enum converter contains :ladybird.data.enum/spec-kvs slot"
             (defenum E :a 1 b 2)
             E => (contains {:ladybird.data.enum/spec-kvs [:a 1 'b 2]})))

(facts "about enum utilities"
       (fact "can tell whether a data structure is an enum"
             (defenum E :a 1 :b 2)
             (enum? E) => true
             (enum? 1) => false
             (enum? {}) => false
             (enum? nil) => false
             (enum? ()) => false)
       (fact "can get the keys of out-fn"
             (defenum E :a 1 b 2)
             (enum-keys E) => (just [:a 'b "a" "b"] :in-any-order))
       (fact "can get the vals of out-fn"
             (defenum E :a 1 b 2)
             (enum-vals E) => (just [1 2] :in-any-order))
       (fact "can get the keys in the original enum spec"
             (defenum E :a 1 b 2)
             (spec-keys E) => '(:a b))
       (fact "can get the values in the original enum spec"
             (defenum E :a 1 b 2)
             (spec-vals E) => '(1 2)))
