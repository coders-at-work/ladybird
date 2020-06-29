(ns ladybird.ext.t-svc
    (:require [midje.sweet :refer :all]
              [ladybird.ext.svc :refer :all]
              [schema.core :as s]
              )
    )

(declare s)
(facts "Can define a svc"
       (fact "Can define an svc just like a function"
             (defsvc s "" [] 1 2)
             (s) => 2
             )
       (facts "Can define an svc with permission checking information"
              (fact "Can specify how to check permission"
                    (defsvc s ""
                      :permission ..permissions..
                      :permission-check {:check-fn (constantly true) :fail-fn #(throw (ex-info "fail" {:a 1}))}
                      []
                      1
                      )
                    (s) => 1 
                    )
              (fact "Can specify what to do if the permission checking failed"
                    (defsvc s ""
                      :permission ..permissions..
                      :permission-check {:check-fn (constantly false) :fail-fn #(throw (ex-info "fail" {:permission %}))}
                      []
                      1
                      )
                    (s) => (throws clojure.lang.ExceptionInfo) 
                    )
              )
       (facts "Can specify how to transform the structure of returned data when defining a svc"
              (facts "Can specify what structure the output data should be converted to"
                     (fact "Can select fields"
                           (defsvc s
                             ""
                             :out-struct {:a s/Int :b {:c _}}
                             []
                             {:a 1 :b {:c 2 :d 3}})
                           (s) => {:a 1 :b {:c 2}}
                           )
                     (fact "Can rename a list of fields"
                           (defsvc s
                             ""
                             :out-struct {* [:a :aa :b :bb] :c {* [:d :dd]}}
                             []
                             {:a 1 :b 2 :c {:d 3 :e 4}}
                             )
                           (s) => {:aa 1 :bb 2 :c {:dd 3}}
                           )
                     (fact "Can select and rename a list of fields"
                           (defsvc s
                             ""
                             :out-struct {+ [:a [[:c :d] :dd]]}
                             []
                             {:a 1 :b 2 :c {:d 3 :e 4}}
                             )
                           (s) => {:a 1 :dd 3}
                           )
                     (fact "Can remove a list of fields"
                           (defsvc s
                             ""
                             :out-struct {- [:a :b] :c {- [:d]}}
                             []
                             {:a 1 :b 2 :c {:d 3 :e 4}}
                             )
                           (s) => {:c {:e 4}}
                           )
                     )
              (fact "Can specify how to convert the output data"
                    (defsvc s
                      ""
                      :out-fn #(select-keys % [:a])
                      []
                      {:a 1 :b {:c 2 :d 3}})
                    (s) => {:a 1})
              (fact "Can specify how to convert the output data which is a seq"
                    (defsvc s
                      ""
                      :out-seq-fn #(select-keys % [:a])
                      []
                      [{:a 1 :b 2} {:a 2 :c 4}])
                    (s) => '({:a 1} {:a 2}))
              ))
