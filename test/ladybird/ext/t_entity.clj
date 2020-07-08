(ns ladybird.ext.t-entity
    (:require [midje.sweet :refer :all]
              [ladybird.ext.entity :refer :all]
              )
    )

(facts "Can define an entity"
       (fact "Can define an entity which is just a domain"
             (defentity E [:a :b])
             E => (contains {:table-name "e" :fields [:a :b]})
             )
       )
