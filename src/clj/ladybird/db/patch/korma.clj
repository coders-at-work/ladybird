(ns ladybird.db.patch.korma
    (:require [korma.core :as kc]
              [korma.db :as kdb]
              [korma.sql.engine :as eng]
              [korma.sql.fns :as fns]
              [korma.sql.utils :as ku]))

;; debugging
(def korma-exec-sql @#'kdb/exec-sql)

(def ^:private debug-result* (atom nil))
(defn debug-result
  "
   Gets the last information kept in debug mode.
  "
  [] @debug-result*)

(defn- debug-exec-sql [{:keys [sql-str params] :as query}]
       (println "sql execution info:")
       (println sql-str "   " params)
       (println)
       (reset! debug-result* [sql-str params])
       (korma-exec-sql query))

(defn enter-debug-mode!
  "
   Alters the root binding of #'korma.db/exec-sql to print and kepp the information of sql execution. Mustn't be called in production code.
  "
  []
  (alter-var-root #'kdb/exec-sql (fn [_] debug-exec-sql)))

(defn exit-debug-mode!
  "
   Restores the root binding of #'korma.db/exec-sql.
  "
  []
  (alter-var-root #'kdb/exec-sql (fn [_] korma-exec-sql)))

;; limit offset patch
(def korma-sql-limit-offset eng/sql-limit-offset)

(defn- sqlserver-limit-offset [{:keys [limit offset] :as query}]
       (let [limit-sql (when limit
                         (format " fetch next %d rows only" limit))
             offset (or offset (and limit 0))
             offset-sql (when offset
                          (format " offset %d rows" offset))]
             (update-in query [:sql-str] str offset-sql limit-sql)))

(defn sql-limit-offset [{:keys [limit offset] :as query}]
  (if (= "sqlserver" (:subprotocol eng/*bound-options*))
    (sqlserver-limit-offset query)
    (korma-sql-limit-offset query)))

(alter-var-root #'eng/sql-limit-offset  (fn [_] sql-limit-offset))

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

(defn- parse-aggregate [[[function-name field] alias group-by]]
       #_(kc/fields % [((aggregates (name function-name)) % field) alias]) 
       #(let [q (kc/fields % [((aggregates (name function-name)) % field) alias])]
          (if group-by
            (kc/group q group-by)
            q)))

(defn- make-db-fns [db]
       (let [add-db-fn #(if db (assoc % :db db) %)
             db-options (:options db)
             add-options-fn #(if db-options (assoc % :options db-options) %)]
         [add-db-fn add-options-fn]))

(defn to-korma-order [order]
  (let [order (partition-by #(#{:asc :desc} %) order)
        order (partition-all 2 2 order)
        field-order-fn (fn [fields] (mapcat #(if (vector? %) % (vector % :asc)) fields))
        order (mapcat
                (fn [[fields [dir]]]
                    (if dir
                      (-> (drop-last fields) field-order-fn (concat [(last fields) dir]))
                      (field-order-fn fields)))
                order)
        ]
    (flatten order)))

(defn- make-order-fn [order]
       (let [order (to-korma-order order)]
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

(defn- gather-fields [fields join-with joins]
       (let [origin-fields (if fields fields [:korma.core/*])]
         (reduce (fn [ret a]
                     (let [[_ _ join-fields] (a joins)]
                       (if (empty? join-fields)
                         ret
                         (apply conj ret join-fields))))
                 origin-fields join-with)))

(defn- make-where-fn [where-clause]
       (if (seq where-clause) #(where % where-clause) identity))

(declare create-join-fn)
(defn- construct-query [ent where-clause {:keys [fields join-with joins aggregate modifier order offset limit group-by db] :as spec}]
       (let [where-fn (make-where-fn where-clause)
             fields (gather-fields fields join-with joins)
             fields-fn (if fields #(apply kc/fields % fields) identity)
             aggregate-fn (if aggregate (parse-aggregate aggregate) identity)
             modifier-fn (if modifier #(kc/modifier % modifier) identity)
             join-fn (create-join-fn join-with joins db)
             order-fn (if (empty? order) identity (make-order-fn order))
             offset-fn (if offset #(kc/offset % offset) identity)
             limit-fn (if limit #(kc/limit % limit) identity)
             group-fn (if group-by #(apply kc/group % group-by) identity)
             complete-query-fn (comp fields-fn where-fn aggregate-fn modifier-fn join-fn order-fn offset-fn limit-fn group-fn)
             [add-db-fn add-options-fn] (make-db-fns db)
             ]
         (-> (kc/select* ent) add-db-fn add-options-fn complete-query-fn)))

(defn subselect-entity [ent alias where-clause {:keys [fields join-with joins aggregate modifier order offset limit db] :as spec}]
  (let [subquery (-> (construct-query ent where-clause spec) kc/exec kc/query-only ku/sub-query)]
    (kc/table (kc/create-entity (name alias)) subquery alias)))

(defn m-to-subselect [{:keys [as-table condition] :as ent} alias db]
  (subselect-entity as-table alias condition (assoc ent :db db)))

(defn nested-entity [ent-def alias-prefix db]
  (if (string? ent-def)
    ent-def
    (let [alias-order (java.util.concurrent.atomic.AtomicLong. 2)]
      (clojure.walk/postwalk #(if (and (map? %) (contains? % :as-table))
                                (m-to-subselect % (->> (.getAndIncrement alias-order) (str alias-prefix "_") keyword) db)
                                %)
                             ent-def))))

(defn construct-korma-entity [ent-def alias db]
  (if (string? ent-def)
    ent-def
    (let [{:keys [as-table condition]} ent-def
          table-ent (nested-entity as-table alias db)
          ]
      (subselect-entity table-ent alias condition (assoc ent-def :db db))
      )
    #_(let [alias-order (java.util.concurrent.atomic.AtomicLong. 0)]
      (clojure.walk/postwalk #(if (and (map? %) (contains? % :as-table))
                                (m-to-subselect % (->> (.getAndIncrement alias-order) (str alias "_") keyword) db)
                                %)
                             ent))))

(defn- do-join [query join-type table on]
       (kc/join* query join-type table (eng/pred-map (eval (eng/parse-where on)))))

(defn- create-single-join-fn [alias joins db]
       (let [[join-type table _ on] (alias joins)
             join-type (if (= :inner join-type) "" join-type)
             table (construct-korma-entity table alias db)
             ]
         #(do-join % join-type [table (name alias)] on)))

(defn- create-join-fn [join-with joins db]
       (let [do-join-fns (map #(create-single-join-fn % joins db) join-with)]
         (apply comp (reverse do-join-fns))))

(defn select
  "Params:
      spec -- a map contains select specification, can contain the following keys:
          :fields -- same as korma.core
          :aggregate -- same as 'ladybird.db.dml/select'
          :group-by -- same as 'ladybird.db.dml/select'
          :modifier -- see 'ladybird.db.dml/select'
          :order -- see 'ladybird.db.dml/select'
          :offset -- same as 'ladybird.db.dml/select'
          :limit -- same as 'ladybird.db.dml/select'
          :db -- database connection configuration
          :join-with -- same as 'ladybird.db.dml/select'
          :joins -- same as 'ladybird.db.dml/select'
   "
  [ent where-clause {:keys [fields join-with joins aggregate modifier order offset limit group-by db] :as spec}]
  (let [ent (construct-korma-entity ent :main db)]
    (kc/exec (construct-query ent where-clause spec)))
  #_(let [where-fn (if-not (empty? where-clause) #(where % where-clause) identity)
        fields (gather-fields fields join-with joins)
        fields-fn (if fields #(apply kc/fields % fields) identity)
        aggregate-fn (if aggregate (parse-aggregate aggregate) identity)
        modifier-fn (if modifier #(kc/modifier % modifier) identity)
        join-fn (create-join-fn join-with joins)
        order-fn (if (empty? order) identity (make-order-fn order))
        offset-fn (if offset #(kc/offset % offset) identity)
        limit-fn (if limit #(kc/limit % limit) identity)
        complete-query-fn (comp fields-fn where-fn aggregate-fn modifier-fn join-fn order-fn offset-fn limit-fn)
        [add-db-fn add-options-fn] (make-db-fns db)
        ;q (kc/select* "tmp")
        ;q (assoc q :options options)
        ;q (kc/where q {:valid "T"})
        ;q (kc/query-only (kc/exec q))
        ;subq (korma.sql.utils/sub-query q)
        ;t (kc/table (kc/create-entity "tmp") subq :tmp)
        ;ent (if (string? ent) ent (clojure.walk/postwalk #(if (and (map? %) (contains? % :db)) (add-options-fn %) %) ent))
        ;_ (println "ent: " ent)
        ]
    (-> (kc/select* ent) add-db-fn add-options-fn complete-query-fn kc/exec)))

(defn insert! [ent data {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)]
    (-> (kc/insert* ent) add-db-fn add-options-fn (kc/values data) kc/exec)))

(defn update! [ent datum where-clause {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)
        where-fn (make-where-fn where-clause)
        ]
    (-> (kc/update* ent) add-db-fn add-options-fn (kc/set-fields datum) where-fn kc/exec)))

(defn delete! [ent where-clause {:keys [db] :as spec}]
  (let [[add-db-fn add-options-fn] (make-db-fns db)
        where-fn (make-where-fn where-clause)
        ]
    (-> (kc/delete* ent) add-db-fn add-options-fn where-fn kc/exec)))
