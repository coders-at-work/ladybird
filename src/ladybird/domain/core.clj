(ns ladybird.domain.core
    (:require [ladybird.util.string :as str]
              [ladybird.domain.impl :as impl]
              ))

;; domain meta preparing functions
(defn to-clj-name [domain-name]
  (str/camel-case-to-clj-case domain-name))

(defn prepare-table-name [{:keys [domain-name table-name] :as domain-meta}]
  (assoc domain-meta
         :table-name
         (or table-name
             (-> (to-clj-name domain-name) str/hyphen-to-underscore))))

(defn prepare-crud-fn-names [{:keys [primary-key domain-name table-name] :as domain-meta}]
  (let [clj-name (to-clj-name domain-name)
        query-fn-name (str "query-" clj-name)
        query-fn-doc-string (str "query " clj-name " by condition ")
        get-by-fn-name (str "get-" clj-name "-by")
        get-by-fn-doc-string (str "get one " clj-name " by condition ")
        get-fn-name (str "get-" clj-name)
        get-fn-doc-string (str "get one " clj-name " by primary key ")
        add-fn-name (str "add-" clj-name)
        add-fn-doc-string (str "add " clj-name)
        ]
    (assoc domain-meta :query-fn-meta [query-fn-name query-fn-doc-string]
                       :get-by-fn-meta [get-by-fn-name get-by-fn-doc-string]
                       :get-fn-meta (when primary-key [get-fn-name get-fn-doc-string])
                       :add-fn-meta [add-fn-name add-fn-doc-string]
                       )))

;; domain generating functions
(defn generate-domain [{:keys [domain-name] :as domain-meta}]
  `(def ~(symbol domain-name) ~domain-meta)
  )

(defn generate-query-fn [{:keys [table-name fields query-fn-meta] :as domain-meta}]
  (let [[query-fn-name query-fn-doc-string] query-fn-meta
        query-fn (symbol query-fn-name)
        query-spec {:fields fields}
        ]
    `(defn ~query-fn ~query-fn-doc-string [condition# ]
       (impl/query ~table-name condition# ~query-spec))))

(defn generate-get-by-fn [{:keys [query-fn-meta get-by-fn-meta] :as domain-meta}]
  (let [[query-fn-name] query-fn-meta
        [get-by-fn-name get-by-fn-doc-string] get-by-fn-meta
        query-fn (symbol query-fn-name)
        get-by-fn (symbol get-by-fn-name)]
    `(defn ~get-by-fn ~get-by-fn-doc-string [condition#]
       (first (~query-fn condition#))
       )))

(defn generate-get-fn [{:keys [primary-key get-by-fn-meta get-fn-meta] :as domain-meta}]
  (when primary-key
    (let [[get-by-fn-name] get-by-fn-meta
          [get-fn-name get-fn-doc-string] get-fn-meta
          get-by-fn (symbol get-by-fn-name)
          get-fn (symbol get-fn-name)
          ]
      `(defn ~get-fn ~get-fn-doc-string [pk#]
         (~get-by-fn (list '~'= ~primary-key pk#))
         ))))

(defn generate-add-fn [{:keys [domain-name add-fn-meta] :as domain-meta}]
  (let [[add-fn-name add-fn-doc-string] add-fn-meta
        add-fn (symbol add-fn-name)
        param-sym (-> (to-clj-name domain-name) symbol)
        ]
    `(defn ~add-fn ~add-fn-doc-string [~param-sym])
    )
  )

;; define domain
(def default-prepare-fns [prepare-table-name prepare-crud-fn-names])

(def default-generate-fns [generate-domain generate-query-fn generate-get-by-fn generate-get-fn generate-add-fn])

(def ^:dynamic *prepare-fns* default-prepare-fns)

(def ^:dynamic *generate-fns* default-generate-fns)

(defn create-meta
  "Create basic meta data from arguments of defdomain. Default meta keys include:
   :domain-name
   :fields
   :primary-key
   :auto-fields -- fields generated automatically by database
   "
  [domain-name fields meta-data]
  (let [primary-key (when (some #{:id} fields) :id)]
    (merge {:domain-name (name domain-name)
            :fields fields
            :primary-key primary-key
            :auto-fields (when (= primary-key :id) [:id])
            }
           meta-data)))

(def ^:dynamic *create-meta* create-meta)

(defmacro defdomain
  ([domain-name fields]
   `(defdomain ~domain-name ~fields {}))
  ([domain-name fields meta-data]
   (let [domain-meta (*create-meta* domain-name fields meta-data)
         prepare-fn (->> (reverse *prepare-fns*) (apply comp))
         domain-meta (prepare-fn domain-meta)
         body (map #(% domain-meta) *generate-fns*)
         ]
     `(do
        ~@body
        ;~(generate-domain domain-meta)
        #_(def ~domain-name ~domain-meta)))
   )
  )
