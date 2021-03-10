(ns ladybird.util.reitit
    (:require [reitit.swagger :as rswagger]
              [reitit.core :as r]
              [muuntaja.core :as m]
              [meta-merge.core :refer (meta-merge)]
              [schema-tools.swagger.core :refer (transform-pred transform-type)]
              [schema.core :as s]
              )
    (:import [com.fasterxml.jackson.datatype.joda JodaModule]
             [com.fasterxml.jackson.databind.module SimpleModule]
             [jsonista.jackson FunctionalSerializer]
             org.joda.time.DateTime
             )
    ; (:import org.joda.time.DateTime)
    )

;; json formater
(def default-muuntaja
  (m/create
    (-> m/default-options
        (assoc-in [:formats "application/json" :opts] {:modules [(JodaModule.)]})
        (assoc-in [:formats "application/json" :decoder-opts :bigdecimals] true)
        (assoc-in [:formats "application/json" :encoder-opts] {:modules [(doto
                                                                   (SimpleModule. "Instant")
                                                                   (.addSerializer
                                                                     java.time.Instant
                                                                     (FunctionalSerializer.
                                                                       (fn [t gen]
                                                                           (let [formatter (.toFormatter
                                                                                             (.appendInstant (java.time.format.DateTimeFormatterBuilder. ) 3))]
                                                                             (.writeString gen (.format formatter t)))))))]})
        #_(assoc-in [:formats "application/json" :encoder-opts :date-format] "yyyy-MM-dd"))))

;; swagger schema
(defmethod transform-pred
  :schema-tools.swagger.core/default
  [this _]
  (let [m (meta this)
        t (:type m)
        ]
    (if t
      (select-keys m [:type :format :example])
      {:type "string"})))

(defmethod transform-type
  org.joda.time.DateTime
  [_ _]
  {:type "string" :format "date-time"}
  )

;; swagger handler
(def ^:const Parameter-Description-Key :param-desc)

(defn- add-param-desc-to-parameters
  [route-data swagger-parameters]
  (mapv (fn [{:keys [name] :as parameter}]
            (let [name-key (keyword name)]
              (update-in parameter [:description] #(or (get-in route-data [Parameter-Description-Key name-key]) %))
              )
            )
        swagger-parameters)
  )

(defn- add-param-desc-to-end-points
  [route-end-points swagger-end-points]
  (->>
    (map (fn [[method end-point]]
             (let [route-data (get-in route-end-points [method :data])]
               [method (update-in end-point [:parameters] (partial add-param-desc-to-parameters route-data))])
             )
         swagger-end-points)
    (into {})))

(defn- add-param-desc-to-paths
  [path-routes-m paths-m]
  (->>
    (map (fn [[p swagger-end-points]]
             [
              p
              (let [[_ _ route-end-points] (-> (get path-routes-m p) first)]
                (add-param-desc-to-end-points route-end-points swagger-end-points))
              ]
             )
         paths-m)
    (into {})
    )
  )

(defn add-param-desc-for-spec
  ""
  [router {:keys [paths] :as swagger-spec}]
  (let [routes (r/compiled-routes router)
        path-routes-m (group-by first routes)
        ]
    (update-in swagger-spec [:paths] (partial add-param-desc-to-paths path-routes-m))
    )
  )

(defn create-swagger-handler
  ""
  []
  (let [origin-handler (rswagger/create-swagger-handler)]
    (fn create-swagger
        ([{::r/keys [router match] :keys [request-method] :as req}]
         (let [origin-resp (origin-handler req)
               origin-spec (:body origin-resp)
               ]
           (assoc origin-resp :body (add-param-desc-for-spec router origin-spec))
           )
         )
        ([req res raise]
         (try
           (res (create-swagger req))
           (catch Exception e
                  (raise e))
           ))))
  )

;; endpoint definition
(defn endpoint
  "
  "
  [name method path endpoint-spec handler config]
  [path (meta-merge {:name name method (merge {:handler handler} endpoint-spec)} config)])

(defn- generate-response-spec
  [return-spec]
  {:responses {200 {:body return-spec}}}
  )

(defn- generate-parameter-spec
  [parameter-type param-spec]
  (let [params (partition 3 param-spec)]
    (reduce
      (fn [parameters-spec [param-sym coercion description]]
          (let [param-key (keyword param-sym)
                param-spec-key (if (:optional (meta param-sym)) `(s/optional-key ~param-key) param-key)
                ]
            (-> parameters-spec
                (assoc-in [:parameters parameter-type param-spec-key] coercion)
                (assoc-in [Parameter-Description-Key param-key] description)
                (assoc-in [:handler-parameters parameter-type param-key] coercion)
                )
            )
          )
      {:parameters {parameter-type nil}
       Parameter-Description-Key nil
       }
      params
      )
    )
  )

(defmulti generate-spec-for (fn [k spec] k))
(defmethod generate-spec-for
  :return
  [k spec]
  (generate-response-spec spec)
  )
(defmethod generate-spec-for
  :form-params
  [k spec]
  (generate-parameter-spec :form spec)
  )
(defmethod generate-spec-for
  :query-params
  [k spec]
  (generate-parameter-spec :query spec)
  )
(defmethod generate-spec-for
  :path-params
  [k spec]
  (generate-parameter-spec :path spec)
  )
(defmethod generate-spec-for
  :body-params
  [k spec]
  (if (= 3 (count spec))
    (let [body-params (second spec)]
      {:parameters {:body body-params}
       :handler-parameters {:body {(keyword (first spec)) body-params}}})
    (throw (IllegalArgumentException. ":body-params can only contain one paramter of which the schema is used as the structure of body"))
    #_(generate-parameter-spec :body spec))
  )
(defmethod generate-spec-for
  :header-params
  [k spec]
  (generate-parameter-spec :header spec)
  )
(defmethod generate-spec-for
  :multipart-params
  [k spec]
  (generate-parameter-spec :multipart spec)
  )
(defmethod generate-spec-for
  :summary
  [k summary]
  {:summary summary}
  )
(defmethod generate-spec-for
  :description
  [k description]
  {:description description}
  )

(defn- group-route-data
  [route-data spec-key-set]
  (let [route-data-pairs (partition 2 2 nil route-data)]
    (reduce
      (fn [grouped-data [k v :as pair]]
          (cond
            (spec-key-set k) (update-in grouped-data [:spec-data] #(conj % [k v]))
            (= :config k) (assoc grouped-data :config v)
            :otherwise (update-in grouped-data [:handler-body] #(if (= 1 (count pair)) (conj % k) (conj % k v)))
            )
          )
      {:spec-data [] :config nil :handler-body []}
      route-data-pairs
      )
    )
  )

(defn- group-route-data-for-endpoint
  [route-data]
  (group-route-data route-data #{:return :form-params :query-params :path-params :header-params :body-params :multipart-params :summary :description})
  )

(def ^:const All-Params-Local-Sym '=all-params=)
(def ^:const Query-Params-Local-Sym '=query-params=)
(def ^:const Path-Params-Local-Sym '=path-params=)
(def ^:const Form-Params-Local-Sym '=form-params=)
(def ^:const Header-Params-Local-Sym '=header-params=)
(def ^:const Body-Params-Local-Sym '=body-params=)
(def ^:const Multipart-Params-Local-Sym '=multipart-params=)

(defn- whole-param-key
  [param-type param-m]
  (when (= :body param-type)
    (ffirst param-m)
    )
  )

(defn- generate-handler
  [parameters handler-body]
  (if (and (= 1 (count handler-body))
           (symbol? (first handler-body))
           (not
             (or
               (#{All-Params-Local-Sym Query-Params-Local-Sym Path-Params-Local-Sym
                  Form-Params-Local-Sym Header-Params-Local-Sym Body-Params-Local-Sym Multipart-Params-Local-Sym} (first handler-body))
               (get (->> (vals parameters) (apply merge) keys (map #(symbol (name %))) set) (first handler-body))
               ))
           )
    (first handler-body)
    (let [param-type-basic-destructs {:query Query-Params-Local-Sym :path Path-Params-Local-Sym :form Form-Params-Local-Sym
                                      :header Header-Params-Local-Sym :body Body-Params-Local-Sym :multipart Multipart-Params-Local-Sym
                                      }
          [handler-param-destrutct let-binding] (reduce
                                                  (fn [[param-destruct let-binding] [param-type param-m]]
                                                      (let [basic-destruct (param-type-basic-destructs param-type)
                                                            param-destruct (dissoc param-destruct basic-destruct)
                                                            [destruct-key let-binding] (if-let [whole-param-as (whole-param-key param-type param-m)]
                                                                                               (let [whole-param-as-sym (symbol (name whole-param-as))]
                                                                                                 [whole-param-as-sym
                                                                                                  (conj let-binding basic-destruct {whole-param-as whole-param-as-sym})])
                                                                                               [(assoc {:keys (mapv #(symbol (name %)) (keys param-m))} :as basic-destruct)
                                                                                                let-binding])
                                                            ]
                                                        [(assoc param-destruct destruct-key param-type) let-binding]
                                                        )
                                                      )
                                                  [(zipmap (vals param-type-basic-destructs) (keys param-type-basic-destructs)) []]
                                                  parameters
                                                  )
          handler-param-destrutct {handler-param-destrutct :parameters}
          ]
      `(fn [~handler-param-destrutct]
           (let [~@let-binding
                 ~All-Params-Local-Sym (merge ~Query-Params-Local-Sym ~Path-Params-Local-Sym ~Form-Params-Local-Sym
                                              ~Header-Params-Local-Sym ~Body-Params-Local-Sym ~Multipart-Params-Local-Sym)
                 ~'ret (do ~@handler-body)]
             {:status 200 :body ~'ret}
             )))))

(defn- alter-config
  ""
  [method config]
  (reduce
    (fn [ret k]
        (if (contains? ret k)
          (-> (assoc-in ret [method k] (ret k)) (dissoc ret k))
          ret)
        )
    config
    [:no-doc :handler :middleware]
    )
  )

(defn- to-qualified-keyword
  [sym]
  (->> (name sym) (str (ns-name *ns*) "/") keyword))

(defn- def-endpoint*
  [endpoint-var-sym method path route-data]
  (let [{:keys [spec-data config handler-body]} (group-route-data-for-endpoint route-data)
        ; _ (println " ==== spec-data: " spec-data ", handler-body: " handler-body)
        endpoint-spec (->> (map #(apply generate-spec-for %) spec-data) (apply meta-merge))
        ; _ (println " ==== endpoint-spec: " endpoint-spec)
        handler (generate-handler (:handler-parameters endpoint-spec) handler-body)
        ; _ (println " ==== handler: " handler)
        endpoint-name (to-qualified-keyword endpoint-var-sym)
        ]
    `(def ~endpoint-var-sym (endpoint ~endpoint-name ~method ~path ~endpoint-spec ~handler ~(alter-config method config)))
    )
  )
(defmacro def-endpoint
  "
  Example:
  (def-endpoint create-prize-category
                :post \"/create-prize-category\"
                :summary \"swagger summary\"
                :description \"swagger description\"
                ; like compojure-api, you can use :form-params, :query-params, :path-params, :header-params, :body-params and :multipart-params to specify the parameters coercion
                :form-params [prize-category-name s/Str \"The category name of the new prize category\"
                              x s/Int \"The x variable\"
                              ^:optional y s/Int \"The y variable\"
                              ]
                :return PrizeCategoryOutDto ; The schema of success response.
                :config nil ; The value of :config is a map. If it is nil it can be ignored.
                (svc/create-prize-category prize-category-name)
  )
  "
  [endpoint-var-sym method path & route-data]
  (def-endpoint* endpoint-var-sym method path route-data)
  )

(defmacro def-endpoints-of
  ""
  [path & endpoint-data]
  )

;; define endpoint by template
(defn- extract-method-map
  [route-data-m]
  (let [all-methods #{:get :head :patch :delete :options :post :put :trace}
        route-method (->> (keys route-data-m) (filter all-methods) first)
        ]
    {:method route-method :method-data (route-method route-data-m)}
    )
  )

(defn- group-route-data-for-template
  ""
  [route-data]
  (group-route-data route-data #{:name :summary :description :handler :no-doc :middleware})
  )

(defn- set-spec-based-on-template
  ""
  [template-data method spec-data]
  (if-let [[k v] (first spec-data)]
          (let [t-data (if (= :name k)
                         (assoc template-data k v)
                         (assoc-in template-data [method k] v)
                         )
                ]
            (recur t-data method (next spec-data))
            )
          template-data
          )
  )

(defn- endpoint-from-template
  ""
  [[path template-data :as template] route-data]
  (let [{:keys [spec-data handler-body]} (group-route-data-for-template route-data)
        ; _ (println " ==== spec-data: " spec-data ", handler-body: " handler-body)
        {:keys [method method-data]} (extract-method-map template-data)
        endpoint-data (set-spec-based-on-template (dissoc template-data :name) method spec-data)
        endpoint-data (if (empty? handler-body)
                        endpoint-data
                        (let [{:keys [handler-parameters]} method-data]
                          (->> (generate-handler handler-parameters handler-body)
                               (assoc-in endpoint-data [method :handler])
                               ))
                        )
        ]
    [path endpoint-data]
    )
  )

(defmacro def-endpoint-by-template
  "
  Example:
  (def-endpoint T
                :post \"/t\"
                :body-params [body-params {:u String :p String} \"all body params\"]
                (let [{:keys [u p]} body-params] {:u p :p u})
  )
  (def-endpoint-by-template E T
                            :name :e
                            :summary \"e\"
                            :description \"e\"
                            :no-doc true
                            :middleware [m1 m2]
                            ; you can specify handler by - :handler handler-fn if you like
                            (let [{:keys [u p]} body-params] (str u \"/\" p))
  )
  "
  [endpoint-var-sym template-sym & route-data]
  (let [endpoint (endpoint-from-template (eval template-sym) route-data)
        endpoint (update-in endpoint [1 :name] #(or % (to-qualified-keyword endpoint-var-sym)))
        ]
    `(def ~endpoint-var-sym ~endpoint))
  )
