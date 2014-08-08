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

(defn prepare-crud-fn-names [{:keys [domain-name table-name] :as domain-meta}]
  (let [clj-name (to-clj-name domain-name)
        query-fn-name (str "query-" clj-name)
        query-fn-doc-string (str "query " clj-name " by condition ")
        get-by-fn-name (str "get-" clj-name "-by")
        get-by-fn-doc-string (str "get one " clj-name " by condition ")
        get-fn-name (str "get-" clj-name)
        get-fn-doc-string (str "get one " clj-name " by primary key ")
        ]
    (assoc domain-meta :query-fn-name query-fn-name :query-fn-doc-string query-fn-doc-string
                       :get-by-fn-name get-by-fn-name :get-by-fn-doc-string get-by-fn-doc-string
                       :get-fn-name get-fn-name :get-fn-doc-string get-fn-doc-string))
  )

;; domain generating functions
(defn generate-domain [{:keys [domain-name] :as domain-meta}]
  `(def ~(symbol domain-name) ~domain-meta)
  )

(defn generate-query-fn [{:keys [table-name fields query-fn-name query-fn-doc-string] :as domain-meta}]
  (let [query-fn (symbol query-fn-name)
        query-spec {:fields fields}
        ]
    `(defn ~query-fn ~query-fn-doc-string [condition# ]
       (impl/query ~table-name condition# ~query-spec))))

(defn generate-get-by-fn [{:keys [query-fn-name get-by-fn-name get-by-fn-doc-string] :as domain-meta}]
  (let [query-fn (symbol query-fn-name)
        get-by-fn (symbol get-by-fn-name)]
    `(defn ~get-by-fn ~get-by-fn-doc-string [condition#]
       (first (~query-fn condition#))
       ))
  )

;; define domain
(def default-prepare-fns [prepare-table-name prepare-crud-fn-names])

(def default-generate-fns [generate-domain generate-query-fn generate-get-by-fn])

(def ^:dynamic *prepare-fns* default-prepare-fns)

(def ^:dynamic *generate-fns* default-generate-fns)

(defmacro defdomain
  ([domain-name fields]
   `(defdomain ~domain-name ~fields {}))
  ([domain-name fields meta-data]
   (let [domain-meta (merge {:domain-name (name domain-name) :fields fields} meta-data)
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
