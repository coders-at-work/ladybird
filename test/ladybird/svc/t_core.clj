(ns ladybird.svc.t-core
    (:require [midje.sweet :refer :all]
              [ladybird.svc.core :refer :all]
              [ladybird.data.build-in-validator :as v]))

(facts "about svc"
       (fact "should put all parameters except for gen-fns into meta map."
             (svc [identity] :a "a" [] {}) => {:svc :a :doc-string "a" :prototype [] :options {} :body nil})
       (fact "should expand to call with ladybird.core/chain-gen"
             (svc [identity] a "a" [] {}) =expands-to=> (ladybird.core/chain-gen {:svc 'a :doc-string "a" :prototype '[] :options '{} :body 'nil} identity))
       (fact "should accept body"
             (svc [(fn [m] `(+ ~@(:body m)))] a "a" [] {} 1 2) => 3)
       (fact "a comprehensive example"
             (let [f (svc
                       [encapsule-body transform check-and-bind (fn [{:keys [prototype body-form]}] `(fn ~prototype ~body-form))]
                       a "a" [a b c d] {:check-and-bind [a [v/not-nil v/is-boolean]
                                                         b v/not-nil
                                                         c [:to inc]
                                                         d [v/not-nil :to dec]]
                                        :to str}
                       [a b c d])]
               (f false 2 3 4) => "[false 2 4 3]")))

(facts "about s-svc"
       (fact "should work as ladybird.svc.core/svc if all parameters are provided"
             (s-svc [:body first] a "a" [] {} 1) => 1
             (s-svc [:body first] a "a" [] {} 1) =expands-to=> (ladybird.svc.core/svc [:body first] a "a" [] {} 1))
       (fact "should make options nil if only 5 arguments passed into it, and the last argument will be body"
             (s-svc [:body first] a "a" [] (str "a")) => "a"
             (s-svc [:body first] a "a" [] {:a 1}) => {:a 1}
             (s-svc [:body first] a "a" [] {:a 1}) =expands-to=> (ladybird.svc.core/svc [:body first] a "a" [] nil {:a 1}))
       (fact "should make options nil if more than 5 arguments passed into it, but the first argument after prototype is not a map"
             (s-svc [:body last] a "a" [] 1 2) =expands-to=> (ladybird.svc.core/svc [:body last] a "a" [] nil 1 2) )
       (fact "a comprehensive example"
             (let [f (s-svc
                       [encapsule-body transform check-and-bind (fn [{:keys [prototype body-form]}] `(fn ~prototype ~body-form))]
                       a "a" [a b c d] {:check-and-bind [a [v/not-nil v/is-boolean]
                                                         b v/not-nil
                                                         c [:to inc]
                                                         d [v/not-nil :to dec]]
                                        :to str}
                       [a b c d])]
               (f false 2 3 4) => "[false 2 4 3]")))


(defsvc a "a" [x] {} 1)
(defsvc b "b" [x] 1)
(defsvc c "c" [a b c d] {:check-and-bind [a [v/not-nil v/is-boolean]
                                          b v/not-nil
                                          c [:to inc]
                                          d [v/not-nil :to dec]]
                         :to str}
        [a b c d])
(facts "about defsvc"
       (fact "should define a function"
             (a 2) => 1)
       (fact "can ignore options like ladybird.svc.core/s-svc"
             (b 2) => 1)
       (fact "a comprehensive example"
             (c false 2 3 4) => "[false 2 4 3]"))

(facts "about t-svc"
       (fact "options map is prior to prototype"
             (t-svc [encapsule-body transform gen-defn] a "a" {:to #(str %)} [])
             (a) => "")
       (fact "options map can be ignored"
             (t-svc [encapsule-body transform gen-defn] a "a" [])
             (a) => nil)
       (fact "unlike s-svc, options map will not be ignored implicitly"
             (t-svc [encapsule-body transform gen-defn] a "a" [] {})
             (a) => {})
       (fact "a comprehensive example"
             (let [f (t-svc
                       [encapsule-body transform check-and-bind (fn [{:keys [prototype body-form]}] `(fn ~prototype ~body-form))]
                       a "a" {:check-and-bind [a [v/not-nil v/is-boolean]
                                               b v/not-nil
                                               c [:to inc]
                                               d [v/not-nil :to dec]]
                              :to str}
                       [a b c d]
                       [a b c d])]
               (f false 2 3 4) => "[false 2 4 3]")))
