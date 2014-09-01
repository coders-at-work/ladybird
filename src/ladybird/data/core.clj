(ns ladybird.data.core
    (:use [clojure.walk :only (postwalk)])
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.db.dml :as dml]
              [ladybird.data.cond :as c]
              ))

;; convert db record related
(defn- domain-field-to-db-field [field]
       (-> (name field) str/clj-case-to-db-case keyword))

(defn make-select-fields
  "Translate domain fields definition to sql select [db-field alias] pairs. A field is a keyword."
  [& fields]
  (map #(vector (domain-field-to-db-field %) %) fields))

;; meta data
(defn create-select-spec [{:keys [fields ] :as query-spec}]
  (let [ret {}
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
              (assoc m (domain-field-to-db-field k) (convert-value :out converters k v)))
          {} rec))

;; prepare sql structure
(defn- make-raw [x]
       (postwalk #(if (c/raw? %)
                    (list dml/raw (second %))
                    %)
                 x))

;; crud
(defn query
  "query data
   Args:
       table -- a string of table name
       condition -- a list represent the query criteria, ex. '(and (< :user-age 35) (> :user-age 20))
                    see also ladybird.data.cond
       spec -- data model spec
           build-in keys as following:
               :fields -- see also ladybird.db.dml/select
               :converters - A map contains fields as keys and their converters as values. 
               :aggregate -- see also ladybird.db.dml/select
   Return:
       a seq of data"
  ([table condition]
   (query table {} condition))
  ;; TODO use converters to translate condition, ex. for boolean values
  ([table {:keys [fields converters aggregate join] :as spec} condition]
         (let [spec (create-select-spec spec)
               [where] (map make-raw [condition])]
     (->> (dml/select table where spec) (map #(convert-record-in spec %))))
   #_(->> (select table (condition-to-where condition) spec)
        (map #(convert-datum-in % spec)))) 
  )

(defn add!
  "add data
   Args:
       table -- see also 'query' 
       rec -- a map
       recs -- a seq of maps
       spec -- see also 'query' 
   Return:"
  ([table rec]
   (add! table {} rec))
  ([table {:keys [converters] :as spec} & recs]
   (dml/insert! table
                (map #(convert-record-out spec %) recs)
                spec)
   #_(let [tr-fn #(reduce
                  (fn [ret [k v]] (assoc ret (domain-field-to-db-field k) v))
                  {} %)]
     (dml/insert! table (map tr-fn data) spec))
   #_(let [add-data (if-not (sequential? data) [data] data)]
     (insert! table (map #(convert-datum-out % spec) add-data) spec))))
