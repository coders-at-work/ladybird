(ns ladybird.data.core
    (:use [clojure.walk :only (postwalk prewalk)])
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.db.dml :as dml]
              [ladybird.data.cond :as c]
              ))

;; convert db record related
(defn- domain-field-to-db-field [field]
       (-> (name field) str/clj-case-to-db-case keyword))

(defn make-select-fields
  "Translate domain fields definition to sql select [db-field alias] pairs. A field is a keyword or a [db-field alias] vector."
  [& fields]
  (map #(if (vector? %)
          %
          (vector (domain-field-to-db-field %) %))
       fields))

;; meta data
(defn create-select-spec [{:keys [fields aggregate modifier order offset limit] :as query-spec}]
  (let [ret {:aggregate aggregate :modifier modifier :order order :offset offset :limit limit}
        fields (apply make-select-fields fields)
        ret (if-not (empty? fields) (assoc ret :fields fields) ret)
        ]
    ret))

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

(defn- to-raw-result [x]
       (postwalk #(if (c/raw? %)
                    (dml/raw (second %))
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
  ([table {:keys [fields converters aggregate join modifier order offset limit] :as spec} condition]
         (let [{:keys [fields order] :as spec} (create-select-spec spec)
               fields (to-raw-result fields)
               order (to-raw-result order)
               spec (assoc spec :fields fields :order order)
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
