(ns ladybird.domain.core
    (:require [ladybird.util.string :as str]
              [ladybird.data.core :as dc]
              [ladybird.data.cond :as c]
              [ladybird.data.validate-core :as v]
              ))

;; domain meta preparing functions
(defn- to-clj-name [domain-name]
  (str/clj-case domain-name))

(defn- optimistic-locking-fields
  "Returns real fields used by optimistic locking, which is specified by meta"
  [{:keys [fields optimistic-locking-fields] :as meta}]
  (cond (some #{:*} optimistic-locking-fields) fields
        (seq optimistic-locking-fields) optimistic-locking-fields
        (some #{:version} fields) [:version]
        (some #{:last-update} fields) [:last-update]
        :default optimistic-locking-fields)
  )

(defn- create-field-meta [field [converter validate]]
       (let [field (if (vector? field) (second field) field)
             ret (when (and converter
                            (not= '_ converter))
                   {:converters {field converter}})
             ret (if (and validate
                          (not= '_ validate))
                   (assoc ret :validate {field validate})
                   ret)
             ]
         ret))

(defn- field-name-def? [x]
       (or (keyword? x)
           (and (vector? x)
                (= 2 (count x))
                (keyword? (first x))
                (keyword? (second x)))))

(defn- parse-fields-def [fields-def]
       (let [fields-def (partition-by field-name-def? fields-def)
             fields-def (partition 2 2 nil fields-def)
             ]
         (reduce (fn [ret [fields f-def]]
                     (let [ret (apply update-in ret [:fields] conj fields)
                           f (last fields)
                           {:keys [converters validate]} (create-field-meta f f-def)
                           ret (if converters (update-in ret [:converters] merge converters) ret)
                           ret (if validate (update-in ret [:validate] merge validate) ret)
                           ]
                       ret))
                 {:fields []}
                 fields-def)))

;; TODO add :update-fix key
(defn- create-meta
  "Create basic meta data from arguments of defdomain. Default meta keys include:
   :domain-name
   :fields
   :primary-key -- if not specified and fields contains :id, then the primary key will be set to :id by default
   :db-maintain-fields -- fields maintained automatically by database, needn't be inserted or updated
   :add-fixed -- a map contains fields and their insert values, when a record is being inserted, these fields will be set to those values
   :immutable-fields -- fields shouldn't be changed once created, but are not db maintaining fields
   :optimistic-locking-fields -- fields used by optimistic locking, if not specified and fields contains :version or :last-update,
                                 :optimistic-locking-fields will be set to it, if both contained, :version takes precedence. If
                                 :optimistic-locking-fields contains :*, then all fields will be used by optimistic locking.
   :converters -- a map specifying fields converters.

   In addition to the keys above, all keys used by ladybird.data.core/query can be added to domain meta.

   Ex.
      {:domain-name \"Pro\"
       :fields [:id :name :age :create-time :last-update]
       :primary-key :id
       :db-maintain-fields [:id :last-update]
       :add-fixed {:create-time nil}
       :immutable-fields [:create-time]
      }
   "
  [domain-name fields-def meta-data]
  (let [{:keys [fields] :as org-meta} (parse-fields-def fields-def)
        primary-key (when (some #{:id} fields) :id)]
    (merge org-meta
           {:domain-name (name domain-name)
            :primary-key primary-key
            :db-maintain-fields (when (= primary-key :id) [:id])
            }
           meta-data)))

(defn- prepare-table-name [{:keys [domain-name table-name] :as domain-meta}]
  (assoc domain-meta
         :table-name
         (or table-name
             (-> (to-clj-name domain-name) str/hyphen-to-underscore))))

(defn- prepare-crud-fn-names [{:keys [primary-key domain-name table-name] :as domain-meta}]
  (let [clj-name (to-clj-name domain-name)
        query-fn-name (str "query-" clj-name)
        query-fn-doc-string (str "query " clj-name " by condition ")
        query-from-fn-name (str "query-" clj-name "-from")
        query-from-fn-doc-string (str "query " clj-name " by condition and order, returns results from specified offset and up to limit rows ")
        get-by-fn-name (str "get-" clj-name "-by")
        get-by-fn-doc-string (str "get one " clj-name " by condition ")
        get-fn-name (str "get-" clj-name)
        get-fn-doc-string (str "get one " clj-name " by primary key ")
        add-fn-name (str "add-" clj-name "!")
        add-fn-doc-string (str "add " clj-name)
        update-fn-name (str "update-" clj-name "!")
        update-fn-doc-string (str "update " clj-name)
        save-fn-name (str "save-" clj-name "!")
        save-fn-doc-string (str "save " clj-name)
        delete-by-fn-name (str "delete-" clj-name "-by!")
        delete-by-fn-doc-string (str "delete " clj-name " by condition")
        delete-fn-name (str "delete-" clj-name "!")
        delete-fn-doc-string (str "delete " clj-name " by primary key\nReturn:\n    1 -- success to delete the record\n    0 -- fail to delete the record because the record has been modified")
        ]
    (assoc domain-meta :query-fn-meta [query-fn-name query-fn-doc-string]
                       :get-by-fn-meta [get-by-fn-name get-by-fn-doc-string]
                       :get-fn-meta (when primary-key [get-fn-name get-fn-doc-string])
                       :query-from-fn-meta [query-from-fn-name query-from-fn-doc-string]
                       :add-fn-meta [add-fn-name add-fn-doc-string]
                       :update-fn-meta [update-fn-name update-fn-doc-string]
                       :save-fn-meta (when primary-key [save-fn-name save-fn-doc-string])
                       :delete-by-fn-meta [delete-by-fn-name delete-by-fn-doc-string]
                       :delete-fn-meta (when primary-key [delete-fn-name delete-fn-doc-string])
                       )))

(defn- prepare-validate-fn-names [{:keys [domain-name] :as domain-meta}]
  (let [clj-name (to-clj-name domain-name)
        validator-name (str clj-name "-validator")
        validate-fn-name (str "validate-" clj-name)
        validate-fn-doc-string (str "validate a " clj-name " record")
        check-fn-name (str "check-" clj-name)
        check-fn-doc-string (str "validate a " clj-name " record and throw exception if validating failed")
        ]
    (assoc domain-meta :validate-fn-meta [validator-name validate-fn-name validate-fn-doc-string check-fn-name check-fn-doc-string])))

;; domain generating functions
(defn- generate-domain [{:keys [domain-name] :as domain-meta}]
  `(def ~(symbol domain-name) ~domain-meta)
  )

(defn- generate-query-fn [{:keys [table-name fields query-fn-meta converters] :as domain-meta}]
  (let [[query-fn-name query-fn-doc-string] query-fn-meta
        query-fn (symbol query-fn-name)
        query-spec {:fields fields :converters converters}
        ]
    `(defn ~query-fn ~query-fn-doc-string
         ([condition#]
          (~query-fn ~query-spec condition#))
         ([query-spec# condition#]
          (dc/query ~table-name query-spec# condition#)))))

(defn- generate-query-from-fn [{:keys [fields converters query-fn-meta query-from-fn-meta] :as domain-meta}]
  (let [[query-fn-name] query-fn-meta
        query-fn (symbol query-fn-name)
        [query-from-fn-name query-from-fn-doc-string] query-from-fn-meta
        query-from-fn (symbol query-from-fn-name)
        ]
    `(defn ~query-from-fn ~query-from-fn-doc-string
       ([condition# order# offset# limit#]
        (~query-from-fn ~{:fields fields :converters converters} condition# order# offset# limit#))
       ([query-spec# condition# order# offset# limit#]
        (~query-fn (assoc query-spec# :order order# :offset offset# :limit limit#) condition#)))))

(defn- generate-get-by-fn [{:keys [query-fn-meta get-by-fn-meta] :as domain-meta}]
  (let [[query-fn-name] query-fn-meta
        [get-by-fn-name get-by-fn-doc-string] get-by-fn-meta
        query-fn (symbol query-fn-name)
        get-by-fn (symbol get-by-fn-name)]
    `(defn ~get-by-fn ~get-by-fn-doc-string [condition#]
       (first (~query-fn condition#))
       )))

(defn- generate-get-fn [{:keys [primary-key get-by-fn-meta get-fn-meta] :as domain-meta}]
  (when primary-key
    (let [[get-by-fn-name] get-by-fn-meta
          [get-fn-name get-fn-doc-string] get-fn-meta
          get-by-fn (symbol get-by-fn-name)
          get-fn (symbol get-fn-name)
          ]
      `(defn ~get-fn ~get-fn-doc-string [pk#]
         (~get-by-fn (list '~'= ~primary-key pk#))
         ))))

(defn- data-field [[_ df :as field-def]]
       (if (c/raw? df)
         (keyword (second df))
         df))

(defn- retain-fields [fields-def rec]
       (->> (map #(if (vector? %) (data-field %) %) fields-def) (select-keys rec)))

(defn add-record! [{:keys [table-name fields db-maintain-fields add-fixed converters] :as spec} & recs]
  (let [recs (map #(-> (apply dissoc (retain-fields fields %) db-maintain-fields) (merge add-fixed)) recs)
        ]
    (apply dc/add! table-name {:fields fields :converters converters} recs)))

(defn- generate-add-fn [{:keys [domain-name add-fn-meta] :as domain-meta}]
  (let [[add-fn-name add-fn-doc-string] add-fn-meta
        add-fn (symbol add-fn-name)
        ]
    `(defn ~add-fn ~add-fn-doc-string [& recs#]
       (apply add-record! ~(symbol domain-name) recs#))))

(defn update-record! [{:keys [table-name fields db-maintain-fields immutable-fields converters] :as spec} condition datum]
  (let [datum (retain-fields fields datum)
        datum (apply dissoc datum db-maintain-fields)
        datum (apply dissoc datum immutable-fields)
        ]
    (if (empty? datum)
      0
      (dc/modify! table-name {:fields fields :converters converters} condition datum))))

(defn- generate-update-fn [{:keys [domain-name update-fn-meta] :as domain-meta}]
  (let [[update-fn-name update-fn-doc-string] update-fn-meta
        update-fn (symbol update-fn-name)
        ]
    `(defn ~update-fn ~update-fn-doc-string [condition# datum#]
       (update-record! ~(symbol domain-name) condition# datum#))))

(defn- pk-and-lock-condition [{:keys [primary-key] :as domain-meta}]
       (let [lock-fields (optimistic-locking-fields domain-meta)
             lock-clauses (map (fn [field] `(list '~'= ~field (~field ~'rec))) lock-fields)
             pk-cond `(list '~'= ~primary-key (~primary-key ~'rec))
             ]
         (if (empty? lock-clauses)
           pk-cond
           `(list '~'and ~pk-cond ~@lock-clauses))))

(defn- generate-save-fn [{:keys [primary-key save-fn-meta update-fn-meta] :as domain-meta}]
  (when primary-key
    (let [[update-fn-name] update-fn-meta
          update-fn (symbol update-fn-name)
          [save-fn-name save-fn-doc-string] save-fn-meta
          save-fn (symbol save-fn-name)
          condition (pk-and-lock-condition domain-meta)
          ]
      `(defn ~save-fn ~save-fn-doc-string [~'rec]
          (~update-fn ~condition ~'rec)))))

(defn delete-record! [{:keys [table-name converters] :as spec} condition]
  (when (empty? condition) (throw (IllegalArgumentException. "condition is empty in delete statement")))
  (dc/remove! table-name {:converters converters} condition))

(defn- generate-delete-by-fn [{:keys [domain-name delete-by-fn-meta] :as domain-meta}]
  (let [[delete-by-fn-name delete-by-fn-doc-string] delete-by-fn-meta
        delete-by-fn (symbol delete-by-fn-name)
        ]
    `(defn ~delete-by-fn ~delete-by-fn-doc-string [condition#]
       (delete-record! ~(symbol domain-name) condition#))))

(defn- delete-fn-impl [get-fn delete-by-fn pk condition]
       (if (get-fn pk)
         (do
           (delete-by-fn condition)
           (if (get-fn pk) 0 1))
         0))

(defn- generate-delete-fn [{:keys [primary-key delete-fn-meta delete-by-fn-meta get-fn-meta] :as domain-meta}]
  (when primary-key
    (let [[get-fn-name] get-fn-meta
          get-fn (symbol get-fn-name)
          [delete-by-fn-name] delete-by-fn-meta
          delete-by-fn (symbol delete-by-fn-name)
          [delete-fn-name delete-fn-doc-string] delete-fn-meta
          delete-fn (symbol delete-fn-name)
          condition (pk-and-lock-condition domain-meta)
          ]
      `(defn ~delete-fn ~delete-fn-doc-string [~'rec]
         (if (empty? ~'rec)
           0
           (#'ladybird.domain.core/delete-fn-impl ~get-fn ~delete-by-fn (~primary-key ~'rec) ~condition))))))

(defn- generate-validator [{:keys [domain-name validate-fn-meta] :as domain-meta}]
  (let [[validator-name] validate-fn-meta]
    `(def ~(symbol validator-name) (v/m-validator (:validate ~(symbol domain-name))))))

(defn- generate-validate-fn [{:keys [validate-fn-meta] :as domain-meta}]
  (let [[validator-name validate-fn-name validate-fn-doc-string] validate-fn-meta]
    `(defn ~(symbol validate-fn-name) ~validate-fn-doc-string [rec#]
       (~(symbol validator-name) rec#))))

(defn check-validate-result [validate-result]
  (let [msgs (v/err-msgs validate-result)]
    (when msgs (throw (RuntimeException. (clojure.string/join ", " msgs))))))

;; TODO: i18n 
(defn- generate-check-fn [{:keys [validate-fn-meta] :as domain-meta}]
  (let [[_ validate-fn-name _ check-fn-name check-fn-doc-string] validate-fn-meta]
    `(defn ~(symbol check-fn-name) ~check-fn-doc-string [rec#]
       (check-validate-result (~(symbol validate-fn-name) rec#)))))

;; define domain
(def ^:private prepare-fns [create-meta prepare-table-name prepare-crud-fn-names prepare-validate-fn-names])

(def ^:private generate-fns [generate-domain generate-query-fn generate-get-by-fn generate-get-fn generate-add-fn generate-update-fn
                             generate-save-fn generate-delete-by-fn generate-delete-fn generate-query-from-fn generate-validator
                             generate-validate-fn generate-check-fn])


(defmacro defdomain
  "Define the data structure of the domain object.

   Params:
       domain-name -- the name of the domain, you can refer the data structure by the var named by it after defined the domain 
       fields -- the fields definition. See also ladybird.domain.core/create-meta for the simplest format accepted by the default implementation.
                 The definition can contain field converter and validators. The form is: :field, :field converter, :field converter validator-or-validators. Converter and validator-or-validators can be _, means ignoring it.
                 Ex.
                    [:a :b :c]
                    [:a BOOL :b _ not-nil :c :d BOOL [not-nil is-boolean]]
       meta-data -- other meta data to define the domain. See ladybird.domain.core/create-meta for which meta data is accepted by the default implementation.
   
   Ex.
      (use 'ladybird.data.db-converter)
      (use 'ladybird.data.build-in-validator)
      (defdomain Tmp [:id :create-time :last-update :valid]
                     {:db-maintain-fields [:id :last-update]
                      :add-fixed {:create-time nil}
                      :immutable-fields [:create-time]
                      :converters {:valid BOOL}
                      :validate {:valid [not-nil is-boolean]}})
      same as above:
      (defdomain Tmp [:id
                      :create-time
                      :last-update
                      :valid BOOL [not-nil is-boolean]]
                     {:db-maintain-fields [:id :last-update]
                      :add-fixed {:create-time nil}
                      :immutable-fields [:create-time]})
  "
  ([domain-name fields-def]
   `(defdomain ~domain-name ~fields-def {}))
  ([domain-name fields-def meta-data]
   (let [prepare-fn (->> (reverse prepare-fns) (apply comp))
         domain-meta (prepare-fn domain-name fields-def meta-data)
         body (map #(% domain-meta) generate-fns)
         ]
     `(do
        ~@body))))
