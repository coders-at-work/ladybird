(ns ladybird.ext.t-dto
    (:require [midje.sweet :refer :all]
              [ladybird.ext.dto :refer :all]
              [ladybird.ext.entity :refer (defentity)]
              [ladybird.domain.core :refer (defdomain)]
              [schema.core :as s]
              )
    )

(facts "Can generate dto from entity or domain"
       (fact "The generated field schem will be nullable string if it has no converter or validator"
             (defdomain Dom [:id])
             (defdto-from-entity D Dom)
             (s/check D {:id nil}) => nil
             (s/check D {:id "a"}) => nil

             (defentity E [:id])
             (defdto-from-entity D E)
             (s/check D {:id nil}) => nil
             (s/check D {:id "a"}) => nil
             )
       (fact "Can define dto for multi entities"
             (defdomain Dom [:a])
             (defentity E [:b :c])
             (defentity F [:d])
             (defdto-from-entity D Dom E {:includes [:b]} F)
             D => {:a (s/maybe s/Str) :b (s/maybe s/Str) :d (s/maybe s/Str)}
             )
       )
