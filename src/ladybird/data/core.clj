(ns ladybird.data.core
    (:use [clojure.walk :only (postwalk prewalk)])
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.util.keyword :as key]
              [ladybird.db.dml :as dml]
              [ladybird.data.cond :as c]
              ))

;; convert db record related
(defn- domain-field-to-db-field [field]
       (-> (name field) str/clj-case-to-db-case keyword))

(defn- alias-field [table field]
       (key/str-keyword (name table) "." field))

(defn- field-def-for-single-field [table field]
       (if (c/raw? field)
         field
         [(alias-field table (domain-field-to-db-field field)) field]))

(defn- field-def-for-field-alias [table field alias]
       (if (c/raw? field)
         [field alias]
         [(alias-field table field) alias]))

;; raw value processing
(defn- to-raw-result [x]
       (postwalk #(if (c/raw? %)
                    (dml/raw (second %))
                    %)
                 x))

;; TODO deal with raw fields correctly in table fields and join fields
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
             (some (fn [[f a]] (= field a)) field-pairs)
             field)))

(defn- join-fields-for-field-alias-pair [fields field alias]
       (let [field-def (original-field-def fields field)
             o-field (if (vector? field-def)
                       (first field-def)
                       (if (c/raw? field-def)
                         field-def
                         (domain-field-to-db-field field-def)))
             ]
         [o-field alias]))

(defn- data-model-join-fields [{:keys [fields] :as data-model} join-fields]
       (map (fn [j-field]
                (if (vector? j-field)
                  (join-fields-for-field-alias-pair fields (first j-field) (second j-field)) 
                  (original-field-def fields j-field)))
            join-fields))

(defn prepare-joins [join-with joins]
  (when join-with
    (reduce (fn [ret a]
                (let [[join-type table-or-data-model join-fields on] (a joins)
                      is-data-model (map? table-or-data-model)
                      table (if is-data-model (:table-name table-or-data-model) table-or-data-model)
                      join-fields (if is-data-model (data-model-join-fields table-or-data-model join-fields) join-fields)
                      join-fields (apply make-select-fields a join-fields)
                      join-fields (to-raw-result join-fields)
                      ]
                  (assoc ret a [join-type table join-fields on])))
            {} join-with)))

;; convert
(defn convert-value [c-type converters k v]
  (if-let [c (get-in converters [k c-type])]
          (c v)
          v))

(defn convert-record-in [{:keys [converters] :as spec} rec]
  (reduce (fn [m [k v]]
              (assoc m k (convert-value :in converters k v)))
          {} rec))

(defn convert-record-out [{:keys [converters] :as spec} rec]
  (reduce (fn [m [k v]]
              (let [v (if (c/raw? v)
                        (dml/raw (second v))
                        (convert-value :out converters k v))
                    ]
                (assoc m (domain-field-to-db-field k) v)))
          {} rec))

;; prepare sql structure
#_(defn- to-construct-raw [x]
       (postwalk #(if (c/raw? %)
                    (list dml/raw (second %))
                    %)
                 x))

(defn- convert-pred-expr [converters [pred field val :as pred-expr]]
       (let [db-field (domain-field-to-db-field field)]
         (cond (= 'nil? pred) (list '= db-field nil)
               (c/raw? val) (list pred db-field val)
               (= 'in  pred) (list pred db-field (mapv #(if (c/raw? %) % (convert-value :out converters field %)) val))
               :default (list pred db-field (convert-value :out converters field val)))))

(defn- condition-to-where [{:keys [converters] :as spec} condition]
       (let [pred? #'c/pred?]
         (prewalk #(cond (pred? %) (convert-pred-expr converters %)
                         (c/raw? %) (list dml/raw (second %))
                         :default %)
                condition)))

;; crud
(defn query
  "query data
   Params:
       table -- a string of table name
       condition -- a list represent the query criteria, ex. '(and (< :user-age 35) (> :user-age 20))
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
  ;; TODO use converters to translate condition, ex. for boolean values
  ([table {:keys [fields converters aggregate join-with joins modifier order offset limit] :as spec} condition]
         (let [{:keys [fields order] :as spec} (prepare-select-spec table spec)
               fields (to-raw-result fields)
               order (to-raw-result order)
               joins (prepare-joins join-with joins)
               spec (assoc spec :fields fields :order order)
               spec (if (empty? joins) spec (assoc spec :join-with join-with :joins joins))
               convert-spec {:converters converters}
               where (condition-to-where convert-spec condition)
               ]
     (->> (dml/select table where spec) (map #(convert-record-in convert-spec %))))))

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
  ([table {:keys [converters] :as spec} & recs]
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
  ([table {:keys [converters] :as spec} condition datum]
   (let [where (condition-to-where spec condition)
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
  [table {:keys [converters] :as spec} condition]
  (let [where (condition-to-where spec condition)]
    (dml/delete! table where spec)))
