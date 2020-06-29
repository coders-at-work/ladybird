(ns ladybird.ext.t-svc
    (:require [midje.sweet :refer :all]
              [ladybird.ext.svc :refer :all]
              ; [glp-host-app-api.infrastructure.session :refer (with-session)]
              [schema.core :as s]
              )
    )

(facts "Can alter structure of data"
       (fact "If target structure is nil, the source data will be untouched"
             (structure-as nil ..source-data..) => ..source-data..
             )
       (facts "Can transform structure of map"
              (fact "Retruns empty map if target structure is empty"
                    (structure-as {} ..source-data..) => {}
                    )
              (fact "Can select required keys of the target structure"
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:a 1 :b 2 :c 3}) => {:a 1 :b 2}
                    )
              (fact "Can select a list of required keys"
                    (structure-as {ADD [:a :b]} {:b "b" :c 1}) => {:a nil :b "b"}
                    )
              (fact "Can transform correctly for nested maps"
                    (structure-as {:a ..schema-a.. :b {:c ..schema-c..}} {:a 1 :b {:c 3 :d 4} :e 5}) => {:a 1 :b {:c 3}}
                    )
              (fact "Can transfrom correctly for optional keys"
                    (structure-as {:a ..schema-a.. (s/optional-key :b) {:c ..schema-c..}} {:id 1 :a 2 :b {:c 4 :d 5}}) => {:a 2 :b {:c 4}}
                    )
              (fact "Will add required key if it is absent in the source data"
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:a 3}) => {:a 3 :b nil}
                    )
              (fact "Will not add optional key if it is absent in the source data"
                    (structure-as {:a ..schema-a.. (s/optional-key :b) ..schema-b..} {:a 3}) => {:a 3}
                    )
              (fact "Can rename a key by using a vector as structure key"
                    (structure-as {[:a :aa] ..schema-a.. :b ..schema-b..} {:a 3 :b 4 :c 5}) => {:aa 3 :b 4}
                    (structure-as {[:a :aa] ..schema-a.. :b ..schema-b..} {:b 4 :c 5}) => {:aa nil :b 4}
                    (structure-as {[:a :aa] ..schema-a.. :b ..schema-b.. :c {:d ..schema-d.. [:e :ee] ..schema-e..}}
                                  {:a 3 :b 4 :c {:d 5 :f 7}}
                                  ) => {:aa 3 :b 4 :c {:d 5 :ee nil}}
                    )
              (fact "Can rename a source key to multiple different keys"
                    (structure-as {[:a :aa] ..schema-a.. [:a :aaa] ..schema-a..} {:a 3 :b 4 :c 5}) => {:aa 3 :aaa 3}
                    )
              (fact "Can rename key and make it optional by using a vector with meta data as structure key"
                    (structure-as {[:a :aa] ..schema-a.. :b ..schema-b..} {:b 4 :c 5}) => {:aa nil :b 4}
                    (structure-as {^:optional [:a :aa] ..schema-a.. :b ..schema-b..} {:b 4 :c 5}) => {:b 4}
                    )
              (fact "Can make a key optional by renaming it as itself with optional meta"
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b 2}) => {:a nil :b 2}
                    (structure-as {^:optional [:a :a] ..schema-a.. :b ..schema-b..} {:b 2}) => {:b 2}
                    )
              (fact "Can rename a list of keys"
                    (structure-as {RENAME [:a :aa :b :bb] :c :_} {:b "b" :c 1}) => {:aa nil :bb "b" :c 1}
                    (structure-as {:a {RENAME [:b :bb]}} {:a {:b "b" :c 1}} ) => {:a {:bb "b"}}
                    )
              (fact "Can remove keys"
                    (structure-as {(remove-key :b) :_} {:b "b" :c 1}) => {:c 1}
                    (structure-as {(remove-key :a) :_ (remove-key :b) :_} {:a 0 :b "b" :c 1}) => {:c 1}
                    )
              (fact "If both remove keys and non remove keys are specified, it means first removes the remove keys then apply non-remove structure template"
                    (structure-as {(remove-key :b) :_ [:c :cc] :_} {:b "b" :c 1 :d 2}) => {:cc 1}
                    (structure-as {(remove-key :a) :_ (remove-key :b) :_ [:c :cc] :_} {:a 0 :b "b" :c 1 :d 2}) => {:cc 1}
                    )
              (fact "Can remove a list of keys"
                    (structure-as {REMOVE [:a :b]} {:a 0 :b "b" :c 1}) => {:c 1}
                    (structure-as {REMOVE [:a :b] [:c :cc] :_} {:a 0 :b "b" :c 1}) => {:cc 1}
                    )
              (facts "Can rename a path"
                     (facts "Can rename a path as a required key"
                            (fact "Will add the required key if the path exists"
                                  (structure-as {[[:a :b] :d] s/Str :c s/Str} {:a {:b "b"} :c 1}) => {:d "b" :c 1}
                                  (structure-as {:a {:b s/Int, [[:d :e] :dd] s/Int} :c s/Int} {:a {:b 2 :d {:e 3}} :c 1}) => {:a {:b 2 :dd 3} :c 1}
                                  )
                            (fact "Will add the required key even if the path doesn't exist"
                                  (structure-as {[[:a :b] :d] s/Str :c s/Str} {:b "b" :c 1}) => {:d nil :c 1}
                                  )
                           )
                     (facts "Can rename a path as an optional key"
                            (fact "Will add the optional key if the path exists"
                                  (structure-as {^:optional [[:a :b] :d] s/Str :c s/Str} {:a {:b "b"} :c 1}) => {:d "b" :c 1}
                                  )
                            (fact "Will not add the optional key if the path doesn't exist"
                                  (structure-as {^:optional [[:a :b] :d] s/Str :c s/Str} {:b "b" :c 1}) => {:c 1}
                                  )
                           )
                    )
              (fact "Can rename a path by specifying it within a list"
                    (structure-as {RENAME [[:a :b] :d] :c :_} {:a {:b "b"} :c 1}) => {:d "b" :c 1}
                    )
              (fact "Can select and rename a list of keys"
                    (structure-as {ADD [[:a :aa] :b [[:c :d] :dd]]} {:b "b" :c {:d 1}}) => {:aa nil :b "b" :dd 1}
                    (structure-as {:a {ADD [:b]}} {:a {:b "b" :c 1}}) => {:a {:b "b"}}
                    )
              (fact "Can use an option map to rename keys"
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {[:a :aa] true}) => {:aa nil :b "b"}
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {^:optional [:a :aa] true}) => {:b "b"}
                    (structure-as {(s/optional-key :a) ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {[:a :aa] true}) => {:aa nil :b "b"}
                    )
              (fact "Can use an option map to rename a list of keys"
                    (structure-as {:a s/Str :b s/Str} {:b "b" :c 1} {RENAME [:a :aa :b :bb] :c :_}) => {:aa nil :bb "b" :c 1}
                    (structure-as {:a {:b :_ :c :_}} {:a {:b "b" :c 1}} {:a {RENAME [:b :bb]}}) => {:a {:bb "b"}}
                    )
              (fact "Can use an option map to remove a list of keys"
                    (structure-as {:a s/Str :b s/Str} {:b "b" :c 1} {REMOVE [:a]}) => {:b "b"}
                    (structure-as {:a {:b :_ :c :_}} {:a {:b "b" :c 1 :d 2}} {:a {REMOVE [:b]}}) => {:a {:c 1 :d 2}}
                    )
              (fact "If there is a same top level key in both schema and option map, the option map wins"
                    (structure-as {[:a :AA] ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {[:a :aa] true}) => {:aa nil :b "b"}
                    (structure-as {[:a :AA] ..schema-a.. [:a :AAA] ..schema-a..} {:b "b" :c 1} {[:a :aa] true}) => {:aa nil}
                    (structure-as {:a {:b :_ [:c :CC] :_}} {:a {:b 1 :c 2}} {:a {[:c :cc] :_}}) => {:a {:cc 2}}
                    )
              (fact "If both schema and option map want to take a same target key, the option map wins"
                    (structure-as {:AA s/Str :b s/Str} {:b "b" :c 1} {[:c :AA] true}) => {:AA 1 :b "b"}
                    (structure-as {[:a :AA] s/Str :b s/Str :c :_ :d :_ :e :_ :f :_ :g :_ :h :_ :i :_}
                                  {:b "b" :c 1}
                                  {[:c :AA] true}) => {:AA 1 :b "b" :d nil :e nil :f nil :g nil :h nil :i nil}
                    (structure-as {[:a :AA] s/Str :b s/Str :c :_ :d :_ :e :_ :f :_ :g :_ :h :_ :i :_}
                                  {:a "1" :b "b" :c 1}
                                  {:AA true}) => {:AA nil :b "b" :c 1 :d nil :e nil :f nil :g nil :h nil :i nil}
                    (structure-as {[:a :AA] {[:b :BB] :_}} {:c {:d 1}} {[:c :AA] {[:d :BB] :_}}) => {:AA {:BB 1}}
                    )
              (fact "All different keys in schema and option map/vector will be included in the result"
                    (structure-as {:a ..schema-a.. :b ..schema-b..}
                                  {:b "b" :c 1}
                                  {[:a :aa] true, [:c :cc] true}) => {:aa nil :b "b" :cc 1}
                    (structure-as {:b ..schema-b..}
                                  {:b 1 :a [{:c 2} {:d 3}]}
                                  {[:a :aa] [{^:optional [:c :cc] true}]}) => {:b 1 :aa '({:cc 2} {})}
                    (structure-as {:a :_} {:a 1 :b 2 :c 3} {:b :_}) => {:a 1 :b 2}
                    )
              (fact "If option map contains no renaming information, the schema takes effect"
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} nil) => {:a nil :b "b"}
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {}) => {:a nil :b "b"}
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} nil) => {:a nil :b "b"}
                    (structure-as {:a ..schema-a.. :b ..schema-b..} {:b "b" :c 1} {}) => {:a nil :b "b"}
                    )
              (fact "Can use meta data to rename keys"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :aa] true, [:c :cc] true}})
                                  {:b "b" :c 1}) => {:aa nil :b "b" :cc 1}
                    )
              (fact "Can use meta data to rename a list of keys"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {RENAME [:a :aa :b :bb]}}) {:b "b" :c 1}) => {:aa nil :bb "b"}
                    (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {RENAME [:b :bb]}}}) {:a {:b "b" :c 1}}) => {:a {:bb "b"}}
                    )
              (fact "Can use meta data to remove a list of keys"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {REMOVE [:a]}}) {:b "b" :c 1}) => {:b "b"}
                    (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {REMOVE [:b]}}}) {:a {:b "b" :c 1 :d 2}}) => {:a {:c 1 :d 2}}
                    )
              (fact "If both meta data and option map are specified, only option map takes effect"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}})
                                  {:b "b" :c 1}
                                  {[:a :aa] true, [:c :cc] true}) => {:aa nil :b "b" :cc 1}
                    )
              (fact "If option map is nil, then meta data takes effect"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}})
                                  {:b "b" :c 1}
                                  nil) => {:AA nil :b "b" :cc 1}
                    )
              (fact "But empty option map will disable the meta data"
                    (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}})
                                  {:b "b" :c 1}
                                  {}) => {:a nil :b "b"}
                    )
              )
       (facts "Can transform structure of vector as sequence"
              (fact "Retruns empty vector if target structure is an empty vector"
                    (structure-as [] ..source-data..) => []
                    )
              (fact "Can transform vector of map"
                    (structure-as [{:a ..schema-a..}] [{:a 1 :b 2} {:a 3 :b 4}]) => '({:a 1} {:a 3})
                    )
              (fact "Can transform nested vector correctly"
                    (structure-as {:a ..schema-a.. :b [..schema-b..]} {:b ["b" "bb"] :c 1}) => {:a nil :b '("b" "bb")}
                    (structure-as {:a ..schema-a.. :b [{:c [{:d ..schema-d..}]}]}
                                  {:a 2 :aa 3 
                                   :b '({:c [{:d 4 :dd 4} {:d 5 :dd 5}] :cc 10}
                                        {:c [{:d 6 :dd 6} {:d 7 :dd 7}] :cc 20}
                                        )}
                                  ) => {:a 2 :b '({:c ({:d 4} {:d 5})}, {:c ({:d 6} {:d 7})})}
                    )
              (fact "Can use an option vector to rename keys"
                    (structure-as [{:a ..schema-a.. :b ..schema-b..}]
                                  [{:a 1 :b 2} {:b 3}]
                                  [{^:optional [:a :aa] true}]) => '({:aa 1 :b 2} {:b 3})
                    )
              (fact "Can use an option vector to rename a list of keys"
                    (structure-as [{:a s/Str :b s/Str}] [{:b "b" :c 1}] [{RENAME [:a :aa :b :bb] :c :_}]) => '({:aa nil :bb "b" :c 1})
                    )
              (fact "Can use an option vector to remove a list of keys"
                    (structure-as [{:a s/Str :b s/Str}] [{:b "b" :c 1}] [{REMOVE [:a]}]) => '({:b "b"})
                    (structure-as [{:a {:b :_ :c :_}}] [{:a {:b "b" :c 1 :d 2}}] [{:a {REMOVE [:b]}}]) => '({:a {:c 1 :d 2}})
                    )
              (fact "Can use meta data to rename keys"
                    (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :aa] true}]})
                                  [{:a 1 :b 2} {:b 3}]) => '({:aa 1 :b 2} {:b 3})
                    )
              (fact "Can use meta data to rename a list of keys"
                    (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{RENAME [:a :aa :b :bb]}]}) [{:b "b" :c 1}]) => '({:aa nil :bb "b"})
                    (structure-as (with-meta [{:a {:b :_ :c :_}}] {ALTER-STRUCT [{:a {RENAME [:b :bb]}}]}) [{:a {:b "b" :c 1}}]) => '({:a {:bb "b"}})
                    )
              (fact "Can use meta data to remove a list of keys"
                    (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{REMOVE [:a]}]}) [{:b "b" :c 1}]) => '({:b "b"})
                    (structure-as (with-meta [{:a {:b :_ :c :_}}] {ALTER-STRUCT [{:a {REMOVE [:b]}}]}) [{:a {:b "b" :c 1 :d 2}}]) => '({:a {:c 1 :d 2}})
                    )
              (fact "If both meta data and option vector are specified, only option vector takes effect"
                    (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :AA] true}]})
                                  [{:a 1 :b 2} {:b 3}]
                                  [{^:optional [:a :aa] true}]) => '({:aa 1 :b 2} {:b 3})
                    )
              (fact "If option vector is nil, then meta data takes effect"
                    (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :AA] true}]})
                                  [{:a 1 :b 2} {:b 3}]
                                  nil) => '({:AA 1 :b 2} {:b 3})
                    )
              (fact "But empty option vector will disable the meta data"
                    (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :AA] true}]})
                                  [{:a 1 :b 2} {:b 3}]
                                  []) => '({:a 1 :b 2} {:a nil :b 3})
                    )
              )
       (fact "Can put all together"
             (structure-as [{:a {:b [{:c ..schema-c..}] :d ..schema-d..}, (s/optional-key :e) [{:f ..schema-f.. (s/optional-key :g) ..schema-g..}]}]
                           '({:a {:b [{:c 1} {:c 11}]}}
                             {:a {:b [{:c 2 :cc 2} {:c 22}], :d 3}, :e [{:f 4} {:f 5 :g 5}]})
                           ) => '({:a {:b ({:c 1} {:c 11}), :d nil}}
                                  {:a {:b ({:c 2} {:c 22}), :d 3}, :e ({:f 4} {:f 5 :g 5})}
                                  )
             (structure-as [{^:optional [:a :aa] ..schema-a.., :b {:c ..schema-c..}}]
                           '({:b {:c 1 :d 2}}
                             {:a 3}
                             )
                           ) => '({:b {:c 1}} {:aa 3 :b {:c nil}})
             (structure-as (with-meta {:a s/Int :b {:c s/Int :d s/Int}} {ALTER-STRUCT {[:a :aa] true, [:b :bb] {^:optional [:d :dd] true}}})
                           {:a 1 :b {:c 2}}
                           ) => {:aa 1 :bb {}}
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
