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

(defn- make-order-fn [order]
       (let [order (map #(if (keyword? %) [% :asc] %) order)
             order (flatten order)
             ]
         (fn [query]
             (loop [q query
                    o order
                    ]
                   (let [[f dir] o
                         qu (kc/order q f dir)
                         ord (drop 2 o)
                         ]
                     (if (empty? ord)
                       qu
                       (recur qu ord)))))))

(defn- do-join [query join-type table on]
       (kc/join* query join-type table (eng/pred-map (eval (eng/parse-where on)))))

(defn- create-single-join-fn [alias joins]
       (let [[join-type table _ on] (alias joins)
              join-type (if (= :inner join-type) "" join-type)
             ]
         #(do-join % join-type [table (name alias)] on)))

(defn- create-join-fn [join-with joins]
       (let [do-join-fns (map #(create-single-join-fn % joins) join-with)]
         (apply comp (reverse do-join-fns))))

(defn- gather-fields [fields join-with joins]
       (let [origin-fields (if fields fields [])]
         (reduce (fn [ret a]
                     (let [[_ _ join-fields] (a joins)]
                       (apply conj ret join-fields)))
                 origin-fields join-with)))

(defn select
  "Params:
      spec -- a map contains select specification, can contain the following keys:
          :modifier -- see 'ladybird.db.dml/select'
          :order -- see 'ladybird.db.dml/select'
          :db -- database connection configuration
          :join-with -- same as 'ladybird.db.dml/select'
          :joins -- same as 'ladybird.db.dml/select'
   "
  [ent where-clause {:keys [fields join-with joins aggregate modifier order offset limit db] :as spec}]
  (let [where-fn (if-not (empty? where-clause) #(where % where-clause) identity)
        fields (gather-fields fields join-with joins)
        fields-fn (if fields #(apply kc/fields % fields) identity)
        aggregate-fn (if aggregate (parse-aggregate aggregate) identity)
        modifier-fn (if modifier #(kc/modifier % modifier) identity)
        join-fn (create-join-fn join-with joins)
        order-fn (if (empty? order) identity (make-order-fn order))
        offset-fn (if offset #(kc/offset % offset) identity)
        limit-fn (if limit #(kc/limit % limit) identity)
        complete-query-fn (comp fields-fn where-fn aggregate-fn modifier-fn join-fn order-fn offset-fn limit-fn)
        [add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/select* ent) add-db-fn add-options-fn complete-query-fn kc/exec)))

(defn insert! [ent data {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/insert* ent) add-db-fn add-options-fn (kc/values data) kc/exec :GENERATED_KEY)))

(defn update! [ent datum where-clause {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (update* ent) add-db-fn add-options-fn (kc/set-fields datum) (where where-clause) kc/exec first)))

(defn delete! [ent where-clause {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/delete* ent) add-db-fn add-options-fn (where where-clause) kc/exec)))
