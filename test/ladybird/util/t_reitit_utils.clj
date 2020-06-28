(ns ladybird.util.t-reitit-utils
    (:require [midje.sweet :refer :all]
              [ladybird.util.reitit-utils :refer :all :as rutil]
              [schema.core :as s]
              )
    )

(facts "Can define a reitit endpoint"
       (fact "The simplest endpoint only includes method, path and a default qualified name"
             (def-endpoint E :post "path")
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?})})
                         ])
             )
       (fact "Can specify swagger summary and description of the endpoint"
             (def-endpoint E :post "path"
               :summary "summary"
               :description "description"
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :summary "summary"
                                             :description "description"
                                             }) })
                         ])
             )
       (fact "Can specify response schema for status 200"
             (def-endpoint E :post "path"
               :return ..schema..
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :responses {200 {:body ..schema..}}
                                             })
                                })])
             )
       (fact "Can specify query params with schema and description"
             (def-endpoint E :post "path"
               :query-params [x s/Str "x"
                              ^:optional y s/Int "y"
                              ]
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :parameters {:query {:x s/Str
                                                                  (s/optional-key :y) s/Int
                                                                  }}
                                             :param-desc {:x "x"
                                                          :y "y"
                                                          }
                                             :handler-parameters {:query {:x s/Str
                                                                          :y s/Int
                                                                          }}
                                             })
                                })])
             )
       (fact "Can specify path params with schema and description"
             (def-endpoint E :post "path"
               :path-params [x s/Str "x"
                             ^:optional y s/Int "y"
                             ]
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :parameters {:path {:x s/Str
                                                                 (s/optional-key :y) s/Int
                                                                 }}
                                             :param-desc {:x "x"
                                                          :y "y"
                                                          }
                                             :handler-parameters {:path {:x s/Str
                                                                         :y s/Int
                                                                         }}
                                             })
                                })])
             )
       (fact "Can specify form params with schema and description"
             (def-endpoint E :post "path"
               :form-params [x s/Str "x"
                             ^:optional y s/Int "y"
                             ]
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :parameters {:form {:x s/Str
                                                                 (s/optional-key :y) s/Int
                                                                 }}
                                             :param-desc {:x "x"
                                                          :y "y"
                                                          }
                                             :handler-parameters {:form {:x s/Str
                                                                         :y s/Int
                                                                         }}
                                             })
                                })])
             )
       (fact "Can specify header params with schema and description"
             (def-endpoint E :post "path"
               :header-params [x s/Str "x"
                               ^:optional y s/Int "y"
                               ]
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :parameters {:header {:x s/Str
                                                                   (s/optional-key :y) s/Int
                                                                   }}
                                             :param-desc {:x "x"
                                                          :y "y"
                                                          }
                                             :handler-parameters {:header {:x s/Str
                                                                           :y s/Int
                                                                           }}
                                             })
                                })])
             )
       (fact "Can specify multipart params with schema and description"
             (def-endpoint E :post "path"
               :multipart-params [x ..multipart-spec.. "x"
                                  ^:optional y ..multipart-spec.. "y"
                                  ]
               )
             E => (just ["path"
                         (just {:name :ladybird.util.t-reitit-utils/E
                                :post (just {:handler fn?
                                             :parameters {:multipart {:x ..multipart-spec..
                                                                      (s/optional-key :y) ..multipart-spec..
                                                                      }}
                                             :param-desc {:x "x"
                                                          :y "y"
                                                          }
                                             :handler-parameters {:multipart {:x ..multipart-spec..
                                                                              :y ..multipart-spec..
                                                                              }}
                                             })
                                })])
             )
       (facts "Can specify body params with schema and description"
              (fact "Can specify one body parameter"
                    (def-endpoint E :post "path"
                      :body-params [x {:a s/Int :b s/Int} "x"
                                    ]
                      )
                    E => (just ["path"
                                (just {:name :ladybird.util.t-reitit-utils/E
                                       :post (just {:handler fn?
                                                    :parameters {:body {:a s/Int
                                                                        :b s/Int
                                                                        }}
                                                    :handler-parameters {:body {:x {:a s/Int
                                                                                    :b s/Int
                                                                                    }}}
                                                    })
                                       })])
                    )
              (fact "It will throw exception if you specify more than one body parameter"
                    ;; Is there a way to assert exception thrown when a macro is expanding? Both the following code and =expands-to=> don't work.
                    ; (try
                    ;   (macroexpand
                    ;   '(def-endpoint E :post "path"
                    ;   :body-params [x {:a s/Int :b s/Int} "x"
                    ;                 y s/Int "y"
                    ;                 ]
                    ;   ))
                    ;   (catch IllegalArgumentException e
                    ;          (println " ===================================== catch it")
                    ;          e)) => IllegalArgumentException
                    ;
                    (#'rutil/def-endpoint* 'E :post "path"
                                    [
                                     :body-params ['x {:a s/Int :b s/Int} "x"
                                                   'y s/Int "y"]
                                     ]
                                    ) => (throws IllegalArgumentException)
                    )
              )
       (facts "Can wrap the rest of arguments as the body of handler"
              (fact "Can generate handler if there is no rest argument"
                    (def-endpoint E :post "path"
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Can generate handler if there are more than one rest arguments"
                    (def-endpoint E :post "path"
                      1
                      2
                      3
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 2 3)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              )
       (facts "Can destruct params for handlers"
              (fact "Can destruct params for query-params"
                    (def-endpoint E :post "path"
                      :query-params [a s/Int "a"
                                     ^:optional b s/Int "b"]
                      1
                      (+ a b)
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:query {:a s/Int :b s/Int}}
                                                                          :param-desc {:a "a" :b "b"}
                                                                          :parameters {:query {:a s/Int (schema.core/optional-key :b) s/Int}}}
                                                                         (clojure.core/fn [{{{:keys [a b] :as =query-params=} :query =path-params= :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 (+ a b))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil)))
              (fact "Can destruct params for path-params"
                    (def-endpoint E :post "path"
                      :path-params [a s/Int "a"
                                     ^:optional b s/Int "b"]
                      1
                      (+ a b)
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:path {:a s/Int :b s/Int}}
                                                                          :param-desc {:a "a" :b "b"}
                                                                          :parameters {:path {:a s/Int (schema.core/optional-key :b) s/Int}}}
                                                                         (clojure.core/fn [{{=query-params= :query {:keys [a b] :as =path-params=} :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 (+ a b))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil)))
              (fact "Can destruct params for form-params"
                    (def-endpoint E :post "path"
                      :form-params [a s/Int "a"
                                    ^:optional b s/Int "b"]
                      1
                      (+ a b)
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:form {:a s/Int :b s/Int}}
                                                                          :param-desc {:a "a" :b "b"}
                                                                          :parameters {:form {:a s/Int (schema.core/optional-key :b) s/Int}}}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             {:keys [a b] :as =form-params=} :form =header-params= :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 (+ a b))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil)))
              (fact "Can destruct params for header-params"
                    (def-endpoint E :post "path"
                      :header-params [a s/Int "a"
                                     ^:optional b s/Int "b"]
                      1
                      (+ a b)
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:header {:a s/Int :b s/Int}}
                                                                          :param-desc {:a "a" :b "b"}
                                                                          :parameters {:header {:a s/Int (schema.core/optional-key :b) s/Int}}}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             =form-params= :form {:keys [a b] :as =header-params=} :header
                                                                                             =body-params= :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 (+ a b))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil)))
              (fact "Can destruct params for multipart-params"
                    (def-endpoint E :post "path"
                      :multipart-params [a ..multipart-spec.. "a"
                                         ^:optional b ..multipart-spec.. "b"]
                      1
                      (do-something a b)
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:multipart {:a ..multipart-spec.. :b ..multipart-spec..}}
                                                                          :param-desc {:a "a" :b "b"}
                                                                          :parameters {:multipart {:a ..multipart-spec..
                                                                                                   (schema.core/optional-key :b) ..multipart-spec..}}}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             =body-params= :body {:keys [a b] :as =multipart-params=} :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do 1 (do-something a b))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil)))
              (fact "The whole body will be bound to the body-params parameter"
                    (def-endpoint E :post "path"
                      :body-params [x {:a s/Int :b s/Int} "x"
                                    ]
                      [(:a x) (:b x)]
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {:handler-parameters {:body {:x {:a s/Int :b s/Int}}}
                                                                          :parameters {:body {:a s/Int :b s/Int}}}
                                                                         (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                                             =form-params= :form =header-params= :header
                                                                                             x :body =multipart-params= :multipart
                                                                                             } :parameters}]
                                                                                          (clojure.core/let [=body-params= {:x x}
                                                                                                             =all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do [(:a x) (:b x)])]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))))
       (facts "Can also package params for handler"
              (fact "Can package all params"
                    (def-endpoint E :post "path"
                      :body-params [x {:a s/Int :b s/Int} "x"
                                    ]
                      :query-params [y s/Int "y"]
                      :form-params [z s/Int "z"]
                      :path-params [i s/Int "i"]
                      :header-params [j s/Int "j"]
                      :multipart-params [k ..multipart-spec.. "k"]
                      (let [{:keys [x y z i j k]} =all-params=]
                        [x y z i j k
                         =query-params= =form-params= =path-params= =header-params= =body-params= =multipart-params=])
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E
                                                                         :post "path"
                                                                         {:handler-parameters {:body {:x {:a s/Int :b s/Int}}
                                                                                               :query {:y s/Int}
                                                                                               :form {:z s/Int}
                                                                                               :path {:i s/Int}
                                                                                               :header {:j s/Int}
                                                                                               :multipart {:k ..multipart-spec..}
                                                                                               }
                                                                          :parameters {:body {:a s/Int :b s/Int}
                                                                                       :query {:y s/Int}
                                                                                       :form {:z s/Int}
                                                                                       :path {:i s/Int}
                                                                                       :header {:j s/Int}
                                                                                       :multipart {:k ..multipart-spec..}
                                                                                       }
                                                                          :param-desc {:y "y" :z "z" :i "i" :j "j" :k "k"}}
                                                                         (clojure.core/fn [{{x :body
                                                                                             {:keys [y] :as =query-params=} :query
                                                                                             {:keys [z] :as =form-params=} :form
                                                                                             {:keys [i] :as =path-params=} :path
                                                                                             {:keys [j] :as =header-params=} :header
                                                                                             {:keys [k] :as =multipart-params=} :multipart
                                                                                             }       :parameters}]
                                                                                          (clojure.core/let [=body-params= {:x x}
                                                                                                             =all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do (let [{:keys [x y z i j k]} =all-params=]
                                                                                                                       [x y z i j k
                                                                                                                        =query-params= =form-params= =path-params= =header-params= =body-params= =multipart-params=]))]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined all params symbol"
                    (def-endpoint E :post "path"
                      =all-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =all-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined query params symbol"
                    (def-endpoint E :post "path"
                      =query-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =query-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined path params symbol"
                    (def-endpoint E :post "path"
                      =path-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =path-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined form params symbol"
                    (def-endpoint E :post "path"
                      =form-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =form-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined header params symbol"
                    (def-endpoint E :post "path"
                      =header-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =header-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined body params symbol"
                    (def-endpoint E :post "path"
                      =body-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =body-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )
              (fact "Handler body can be the predefined multipart params symbol"
                    (def-endpoint E :post "path"
                      =multipart-params=
                      ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                         {}
                                                                         (clojure.core/fn [{{=body-params= :body
                                                                                             =query-params= :query
                                                                                             =form-params= :form
                                                                                             =path-params= :path
                                                                                             =header-params= :header
                                                                                             =multipart-params= :multipart} :parameters}]
                                                                                          (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                             ret (do =multipart-params=)]
                                                                                                            {:status 200 :body ret}))
                                                                         nil))
                    )

              )
       (fact "Handler body can be the destructed local symbol of parameters"
             (def-endpoint E :post "path"
               :query-params [x s/Int "x"]
               x
               ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                  {:param-desc {:x "x"}
                                                                   :parameters {:query {:x s/Int}}
                                                                   :handler-parameters {:query {:x s/Int}}}
                                                                  (clojure.core/fn [{{=body-params= :body
                                                                                      {:keys [x] :as =query-params=} :query
                                                                                      =form-params= :form
                                                                                      =path-params= :path
                                                                                      =header-params= :header
                                                                                      =multipart-params= :multipart} :parameters}]
                                                                                   (clojure.core/let [=all-params=  (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                                      ret (do x)]
                                                                                                     {:status 200 :body ret}))
                                                                  nil))
             )
       (fact "A single unknown symbol will be treated as the handler itself instead of the handler body"
             (def-endpoint E :post "path"
               x
               ) =expands-to=> (def E (ladybird.util.reitit-utils/endpoint :ladybird.util.t-reitit-utils/E :post "path"
                                                                  {}
                                                                  x
                                                                  nil))
             )
       (facts "Can specify other route data in :config slot"
              (fact "Can specify the name of endpoint"
                    (def-endpoint E :post "path"
                      :config {:name :E})
                    E => (just ["path"
                                (just {:name :E
                                       :post (just {:handler fn?})})
                                ])
                    )
              (fact "Can put other route data in :config slot"
                    (def-endpoint E :post "path"
                      :config {:name :E
                               :swagger {:tags ["math"]}})
                    E => (just ["path"
                                (just {:name :E
                                       :post (just {:handler fn?})
                                       :swagger {:tags ["math"]}})
                                ])
                    )
              (fact "Top level :no-doc, :handler and :middleware slots will be put under the http method key"
                    (def-endpoint E :post "path"
                      :config {:name :E
                               :no-doc true
                               :handler ..handler..
                               :middleware ..middleware..})
                    E => (just ["path"
                                {:name :E
                                 :post {:no-doc true
                                        :handler ..handler..
                                        :middleware ..middleware..
                                        }}
                                ])
                    )
              )
)

(facts "Can define a reitit endpoint from a endpoint template"
       (fact "Can define body parameter with predifned schema in the template"
             (def Sch {:x ..schema-x.. :y ..schema-y..})
             (def-endpoint T :post "path"
               :body-params [:l Sch "l"]
               )
             (def-endpoint-by-template E T
               [l]
               ) =expands-to=> (def E ["path"
                                       {:name :ladybird.util.t-reitit-utils/E
                                        :post {:parameters {:body {:x ..schema-x.. :y ..schema-y..}}
                                               :handler-parameters {:body {:l {:x ..schema-x.. :y ..schema-y..}}}
                                               :handler (clojure.core/fn [{{=query-params= :query =path-params= :path
                                                                            =form-params= :form =header-params= :header
                                                                            l :body =multipart-params= :multipart
                                                                            } :parameters}]
                                                                         (clojure.core/let [=body-params= {:l l}
                                                                                            =all-params= (clojure.core/merge =query-params= =path-params= =form-params= =header-params= =body-params= =multipart-params=)
                                                                                            ret (do [l])]
                                                                                           {:status 200 :body ret}))
                                               }
                                        }
                                       ])
             )
       )
