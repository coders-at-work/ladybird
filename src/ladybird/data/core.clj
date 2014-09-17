(ns ladybird.data.core
    (:use [clojure.walk :only (postwalk prewalk)])
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.util.keyword :as key]
              [ladybird.db.dml :as dml]
              [ladybird.data.cond :as c]
              ))

;; convert db field related
(defn- domain-field-to-db-field [field]
       (-> (name field) str/clj-case-to-db-case keyword))

(defn- alias-field [table field]
       (key/str-keyword table "." field))

(defn- field-def-for-single-field [table field]
       (if (c/raw? field)
         field
         [(alias-field table (domain-field-to-db-field field)) field]))

(defn- field-def-for-field-alias [table field alias]
       (if (c/raw? field)
         [field alias]
         [(alias-field table field) alias]))

(defn- aliased-field? [field]
       (re-find #"\." (name field)))

(defn- split-aliased-field [aliased-field]
       (->> (clojure.string/split (name aliased-field) #"\.") (map keyword)))

;; raw value processing
(defn- to-raw-result [x]
       (postwalk #(if (c/raw? %)
                    (dml/raw (second %))
                    %)
                 x))

(defn make-select-fields
  "Translate domain fields definition to sql select [db-field alias] pairs. A field is a keyword or a [db-field alias] vector."
  [table & fields]
  (map #(if (vector? %)
          (field-def-for-field-alias table (first %) (second %))
          (field-def-for-single-field table %))
       fields))

;; meta data
(defn prepare-select-spec [table {:keys [fields aggregate modifier join-with joins order offset limit] :as query-spec}]
  (let [ret {:aggregate aggregate :modifier modifier :order order :offset offset :limit limit}
        fields (apply make-select-fields table fields)
        ret (if-not (empty? fields) (assoc ret :fields fields) ret)
        ]
    ret))

(defn- original-field-def [fields field]
       (let [g (group-by keyword? fields)
             single-fields (g true)
             field-pairs (g false)
             ]
         (or ((set single-fields) field)
             (some (fn [[f a :as field-def]] (when (= a field) field-def)) field-pairs)
             field)))

(defn- find-db-field-for [data-fields-def data-field]
       (let [field-def (original-field-def data-fields-def data-field)
             ]
         (if (vector? field-def)
           (first field-def)
           (if (c/raw? field-def)
             field-def
             (domain-field-to-db-field field-def)))))

(defn- join-fields-for-field-alias-pair [fields field alias]
       [(find-db-field-for fields field) alias])

(defn- data-model-join-fields [{:keys [fields] :as data-model} join-fields]
       (map (fn [j-field]
                (if (vector? j-field)
                  (join-fields-for-field-alias-pair fields (first j-field) (second j-field)) 
                  (original-field-def fields j-field)))
            join-fields))

(defn- data-model? [table-or-data-model]
       (map? table-or-data-model))

(declare condition-to-where)

(defn prepare-joins [{:keys [converters table table-fields] :as convert-spec} join-with joins]
  (when join-with
    (let [spec (assoc convert-spec :joins joins)]
      (reduce (fn [ret a]
                (let [[join-type table-or-data-model join-fields on] (a joins)
                      is-data-model (data-model? table-or-data-model)
                      table (if is-data-model (:table-name table-or-data-model) table-or-data-model)
                      join-fields (if is-data-model (data-model-join-fields table-or-data-model join-fields) join-fields)
                      join-fields (apply make-select-fields a join-fields)
                      join-fields (to-raw-result join-fields)
                      on (condition-to-where spec on)
                      ]
                  (assoc ret a [join-type table join-fields on])))
            {} join-with))))

;; convert
(defn convert-value [c-type converters k v]
  (if-let [c (get-in converters [k c-type])]
          (c v)
          v))

(defn convert-record-in [{:keys [converters] :as spec} rec]
  (reduce (fn [m [k v]]
              (assoc m k (convert-value :in converters k v)))
          {} rec))

(defn convert-record-out [{:keys [fields converters] :as spec} rec]
  (reduce (fn [m [k v]]
              (let [v (if (c/raw? v)
                        (dml/raw (second v))
                        (convert-value :out converters k v))
                    db-k (find-db-field-for fields k)
                    ]
                (assoc m db-k v)))
          {} rec))

(defn- converters-from-join-data-model [{:keys [converters] :as data-model} join-fields]
       (reduce (fn [ret join-field-def]
                   (let [[old-field new-field] (if (vector? join-field-def) join-field-def [join-field-def join-field-def])
                         old-field (if (c/raw? old-field) (keyword (second old-field)) old-field)
                         new-field (if (c/raw? new-field) (keyword (second new-field)) new-field)
                         ]
                     (if-let [c (old-field converters)]
                       (assoc ret new-field c)
                       ret)))
               {} join-fields))

(defn converters-from-joins [join-with joins]
  (reduce (fn [ret a]
              (let [[_ table-or-data-model join-fields] (a joins)]
                (if (data-model? table-or-data-model)
                  (merge ret (converters-from-join-data-model table-or-data-model join-fields))
                  ret)))
          {} join-with))

;; prepare sql structure
(defn- convert-condition-field-value [converters joins field val]
       (if-not (aliased-field? field)
               (convert-value :out converters field val)
               (let [[alias data-field] (split-aliased-field field)
                     [_ table-or-data-model] (alias joins)
                     ]
                 (if-not (data-model? table-or-data-model)
                         val
                         (convert-value :out (:converters table-or-data-model) data-field val)))))

(defn- convert-pred-expr [converters joins [pred field val :as pred-expr]]
       (cond (= 'nil? pred) (list '= field nil)
             (and (keyword? field) (keyword? val)) (list pred field val)
             (or (c/raw? field) (c/raw? val)) (list pred field val)
             (= 'in  pred) (list pred field (mapv #(if (c/raw? %) % (convert-condition-field-value converters joins field %)) val))
             (keyword? field) (list pred field (convert-condition-field-value converters joins field val))
             (keyword? val) (list pred val (convert-condition-field-value converters joins val field))
             :default (list pred field val)))

(defn- alias-found-db-field [table table-field-def data-field]
       (->> (find-db-field-for table-field-def data-field) (alias-field table)))

(defn- convert-aliased-data-field-to-db-field [joins aliased-data-field]
       (let [[alias data-field] (split-aliased-field aliased-data-field)
             [_ table-or-data-model] (alias joins)
             ]
         (if (data-model? table-or-data-model)
           (let [data-model table-or-data-model
                 data-field-def (:fields data-model)]
             (alias-found-db-field alias data-field-def data-field))
           aliased-data-field)))

(defn- convert-data-field-to-db-field-in-condition [table table-fields joins data-field]
       (if (aliased-field? data-field)
         (convert-aliased-data-field-to-db-field joins data-field)
         (alias-found-db-field table table-fields data-field)))

(defn- condition-to-where [{:keys [converters table table-fields joins] :as spec} condition]
       (let [pred? #'c/pred?]
         (prewalk #(cond (pred? %) (convert-pred-expr converters joins %)
                         (c/raw? %) (list dml/raw (second %))
                         (keyword? %) (convert-data-field-to-db-field-in-condition table table-fields joins %)
                         :default %)
                  condition)))

;; crud
(defn query
  "query data
   Params:
       table -- a string of table name
       condition -- a list represent the query criteria, ex. '(and (< :user-age 35) (> :user-age 20))
                    don't prefix by table alias for fields in the from-table
                    see also ladybird.data.cond
       spec -- query specification, contains information about data model and sql options
           build-in keys as following:
               :fields -- same as ladybird.db.dml/select
               :converters - A map contains fields as keys and their converters as values. 
               :aggregate -- same as ladybird.db.dml/select
               :modifier -- same as ladybird.db.dml/select
               :joins -- a map of join specs, each key will be used as the alias of the joined table, and will be used in :join-with.
                         Each value has the following forms:
                             [join-type table-or-data-model fields on-clause]
                                 join-type -- can be :inner, :left or :right
                                 table-or-data-model -- the table name string. Or a data model map.
                                 fields -- same as :fields above, must not prefixed by table aliases
                                 on-clause -- on condition, its form is same as :condition above
                             Ex. 
                                {:joins {:p [:inner \"person\" [:name :age] '(= :p.id :person-id)]
                                         :e [:left Email [[:address :addr]] '(= :p.id :e.person-id)]}}
               :join-with -- a vector of join names, each name is a key in :joins. Only these join specs will be used in query.
                             Ex. :join-with [:p :e]
               :order -- see also ladybird.db.dml/select, the difference is that it accepts raw field names here
                       Ex.
                          :order [[(ladybird.data.cond/raw \"valid\") :desc :id :desc]]
               :offset -- same as ladybird.db.dml/select
               :limit -- same as ladybird.db.dml/select
   Return:
       a seq of data"
  ([table condition]
   (query table {} condition))
  ;; TODO support aggregate
  ([table {:keys [fields converters aggregate join-with joins modifier order offset limit] :as spec} condition]
         (let [cond-convert-spec {:table table :table-fields fields :joins joins :converters converters}
               {:keys [fields order] :as spec} (prepare-select-spec table spec)
               fields (to-raw-result fields)
               order (to-raw-result order)
               joins-converters (converters-from-joins join-with joins)
               rec-convert-spec {:converters (merge converters joins-converters)}
               joins (prepare-joins cond-convert-spec join-with joins)
               spec (assoc spec :fields fields :order order)
               spec (if (empty? joins) spec (assoc spec :join-with join-with :joins joins))
               where (condition-to-where cond-convert-spec condition)
               ]
     (->> (dml/select table where spec) (map #(convert-record-in rec-convert-spec %))))))

(defn add!
  "add data
   Params:
       table -- same as 'query' 
       rec -- a map
       recs -- a seq of maps(recs)
       spec -- see also 'query' 
   Return:"
  ([table rec]
   (add! table {} rec))
  ([table {:keys [fields converters] :as spec} & recs]
   (dml/insert! table
                (map #(convert-record-out spec %) recs)
                spec)))

(defn modify!
  "modify data
   Params:
       table -- same as 'query' 
       datum -- a map specifying which fields to be changed
       condition -- same as 'query' 
       spec -- see also 'query' 
   Return:
       count of affected rows" 
  ([table condition datum]
   (modify! table {} condition datum))
  ([table {:keys [fields converters] :as spec} condition datum]
   (let [where (condition-to-where (assoc spec :table table :table-fields fields) condition)
         datum (convert-record-out spec datum )]
     (dml/update! table datum where spec))))

(defn remove!
  "remove data
   Params:
       table -- same as 'query'
       condition -- same as 'query'
       spec -- see also 'query'
   Return:
       "
  [table {:keys [fields converters] :as spec} condition]
  (let [where (condition-to-where (assoc spec :table table :table-fields fields) condition)]
    (dml/delete! table where spec)))
