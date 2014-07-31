(ns ladybird.db.patch.korma
    (:require [korma.core :as kc]
              [korma.db :as kdb]
              [korma.sql.engine :as eng]
              [korma.sql.fns :as fns]))

;; update patch
(defmacro ^{:private true} make-query
  "a copy of korma.core/make-query. The only reason it is here is because korma.core/make-query is private, and it is hard to call a private macro in other namespace"
  [ent m]
  `(let [ent# ~ent]
     (if (:type ent#)
       ent#
       (let [~'this-query (kc/empty-query ent#)]
         (merge ~'this-query ~m)))))

(defn update*
  "same as korma.core/update*, except that (:results returned-value) is nil, which will cause update operation returning affected rows count"
  [ent]
  (make-query ent {:type :update
              :fields {}
              :where []}))

(defmacro update
  "a repalcement of korma.core/update, to return affected rows count instead"
  [ent & body]
  (#'kc/make-query-then-exec #'update* body ent))

;; where patch
(defn- where-or-having-form [where*-or-having* query form]
       (let [primitive-form? (or (map? form) (sequential? form))
             parsing-expansion (if primitive-form? (eng/parse-where form) `(eval (eng/parse-where ~form)))]
         `(let [q# ~query]
            (~where*-or-having* q#
                                (eng/bind-query q#
                                                (eng/pred-map ~parsing-expansion))))))

(defmacro where [query form]
  (where-or-having-form #'kc/where* query form))

;; interface
(defn init-db [korma-db-def-map]
  (let [conn (->> korma-db-def-map kdb/create-db)]
    {:conn-def conn :db-conn (kdb/get-connection conn)}))

(def ^:private aggregates {"count" fns/agg-count
                           "min" fns/agg-min
                           "max" fns/agg-max
                           "first" fns/agg-first
                           "last" fns/agg-last
                           "avg" fns/agg-avg
                           "stdev" fns/agg-stdev
                           "sum" fns/agg-sum})

(defn- parse-aggregate [[[function-name field] alias]]
       #(kc/fields % [((aggregates (name function-name)) % field) alias]))

(defn- make-db-fns [db]
       (let [add-db-fn #(if db (assoc % :db db) %)
             db-options (:options db)
             add-options-fn #(if db-options (assoc % :options db-options) %)]
         [add-db-fn add-options-fn]))

(defn select [ent where-clause {:keys [fields converters join aggregate db] :as spec}]
  (let [where-fn (if-not (empty? where-clause) #(where % where-clause) identity)
        fields-fn (if fields #(apply kc/fields % fields) identity)
        aggregate-fn (if aggregate (parse-aggregate aggregate) identity)
        complete-query-fn (comp fields-fn where-fn aggregate-fn)
        [add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/select* ent) add-db-fn add-options-fn complete-query-fn kc/exec)))

(defn insert! [ent data {:keys [fields converters db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/insert* ent) add-db-fn add-options-fn (kc/values data) kc/exec :GENERATED_KEY)))

(defn update! [ent datum where-clause {:keys [fields converters db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (update* ent) add-db-fn add-options-fn (kc/set-fields datum) (where where-clause) kc/exec first)))
