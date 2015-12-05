(ns ladybird.core-test
  (:require [ladybird.core :refer :all]
            [midje.sweet :refer :all]))

(facts "about chain-proc"
       (fact "should process data by chaining functions, left most first"
             (chain-proc 3 #(* % 2) inc) => 7)
       (fact "should unmodify data if proc-fns is empty"
             (chain-proc 3) => 3))

(facts "about chain-gen"
       (fact "should unmodify meta-m if gen-fns is empty"
             (chain-gen {:a 1} ) => {:a 1})
       (fact "can generate form by chaining functions"
             (chain-gen {:v 3} #(assoc % :f `(inc ~(:v %))) (fn [m] `(* 2 ~(:f m)))) => 8))
