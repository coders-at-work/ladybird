(ns ladybird.svc.t-core
    (:require [midje.sweet :refer :all]
              [ladybird.svc.core :refer :all]
              [ladybird.data.build-in-validator :as v]
     ))

(facts "about SVC"
       (fact "should generate content according to *generate-fns*"
             (binding [*generate-fns* [(fn [m] 1)]]
                      (SVC a "a" [] {} 2) => 1
                      ))
       (fact "should generate meta correctly"
             (binding [*generate-fns* [(fn [{:keys [svc prototype] :as m}] (assoc m :svc `'~svc :prototype `'~prototype))]]
                      (SVC a "a" [x] {:b 1}) => {:svc 'a :doc-string "a" :prototype '[x] :options {:b 1} :body nil}))
       (fact "should chain content generating functions in the order of *generate-fns*"
             (binding [*generate-fns* [(fn [{:keys [svc] :as m}] (assoc m :svc `'~svc))
                                       (fn [{:keys [prototype] :as m}] (assoc m :prototype `'~prototype))
                                       ]]
                      (SVC a "a" [x] {:b 1}) => {:svc 'a :doc-string "a" :prototype '[x] :options {:b 1} :body nil}))
       (fact "put all together"
             (binding [*generate-fns* [encapsule-body transform check-and-bind
                                       (fn [{:keys [prototype body-form]}] `(fn ~prototype ~body-form))]]
                      (let [a (SVC a "a" [a b c d] {:check-and-bind [a [v/not-nil v/is-boolean]
                                                                     b v/not-nil
                                                                     c [:to inc]
                                                                     d [v/not-nil :to dec]]
                                                    :to str}
                                   [a b c d])]
                        (a false 2 3 4) => "[false 2 4 3]")))
       )
