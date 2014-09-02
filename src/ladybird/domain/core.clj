(ns ladybird.domain.core
    (:require [ladybird.util.string :as str]
              [ladybird.data.core :as dc]
              ))

;; domain meta preparing functions
(defn to-clj-name [domain-name]
  (str/clj-case domain-name))

(defn create-meta
  "Create basic meta data from arguments of defdomain. Default meta keys include:
   :domain-name
   :fields
   :primary-key
   :db-maintain-fields -- fields maintained automatically by database, needn't be inserted or updated
   :create-fix -- a map contains fields and their insert values, when a record is being inserted, these fields will be set to those values
   :immutable-fields -- fields shouldn't be changed once created, but are not db maintaining fields

   Ex.
      {:domain-name \"Pro\"
       :fields [:id :name :age :create-time :last-update]
       :primary-key [:id]
       :db-maintain-fields [:id :last-update]
       :create-fix {:create-time nil}
       :immutable-fields [:create-time]
      }
   "
  [domain-name fields meta-data]
  (let [primary-key (when (some #{:id} fields) :id)]
    (merge {:domain-name (name domain-name)
            :fields fields
            :primary-key primary-key
            :db-maintain-fields (when (= primary-key :id) [:id])
            }
           meta-data)))

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

(defn generate-query-fn [{:keys [table-name fields query-fn-meta converters] :as domain-meta}]
  (let [[query-fn-name query-fn-doc-string] query-fn-meta
        query-fn (symbol query-fn-name)
        query-spec {:fields fields :converters converters}
        ]
    ;; TODO generate query-fn with meta param
    `(defn ~query-fn ~query-fn-doc-string
         ([condition#]
          (~query-fn ~query-spec condition#))
         ([query-spec# condition#]
          (dc/query ~table-name query-spec# condition#)))))

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

(defn add-record! [{:keys [table-name db-maintain-fields create-fix converters] :as spec} & recs]
  (let [rec (apply dissoc rec db-maintain-fields)
        rec (merge rec create-fix)
        ]
    (apply dc/add! table-name {:converters converters} recs)))

(defn generate-add-fn [{:keys [domain-name add-fn-meta] :as domain-meta}]
  (let [[add-fn-name add-fn-doc-string] add-fn-meta
        add-fn (symbol add-fn-name)
        ]
    `(defn ~add-fn ~add-fn-doc-string [& recs#]
       (apply add-record! ~(symbol domain-name) recs#))))

;; define domain
(def default-prepare-fns [create-meta prepare-table-name prepare-crud-fn-names])

(def default-generate-fns [generate-domain generate-query-fn generate-get-by-fn generate-get-fn generate-add-fn])

(def ^:dynamic *prepare-fns* default-prepare-fns)

(def ^:dynamic *generate-fns* default-generate-fns)

(defmacro defdomain
  ([domain-name fields]
   `(defdomain ~domain-name ~fields {}))
  ([domain-name fields meta-data]
   (let [prepare-fn (->> (reverse *prepare-fns*) (apply comp))
         domain-meta (prepare-fn domain-name fields meta-data)
         body (map #(% domain-meta) *generate-fns*)
         ]
     `(do
        ~@body
        #_(def ~domain-name ~domain-meta)))
   )
  )
