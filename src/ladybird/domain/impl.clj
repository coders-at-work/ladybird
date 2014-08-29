(ns ladybird.domain.impl
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.db.dml :as dml]
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

;; crud
(defn query
  ""
  ([table condition]
   (query table {} condition))
  ;; TODO use converters to translate condition, ex. for boolean values
  ([table {:keys [fields converters aggregate join] :as query-spec} condition]
         (let [spec (create-select-spec query-spec)
               ]
     (->> (dml/select table condition spec) (map #(convert-record-in query-spec %))))
   #_(->> (select table (condition-to-where condition) spec)
        (map #(convert-datum-in % spec)))) 
  )

(defn add!
  "add data
   Args:
       table -- see also 'query' 
       data -- can be a map, or a seq of maps
       spec -- see also 'query' 
   Return:"
  ([table data]
   (add! table {} data))
  ([table {:keys [converters] :as spec} & data]
   (dml/insert! table
                (map #(convert-record-out spec %) data)
                spec)
   #_(let [tr-fn #(reduce
                  (fn [ret [k v]] (assoc ret (domain-field-to-db-field k) v))
                  {} %)]
     (dml/insert! table (map tr-fn data) spec))
   #_(let [add-data (if-not (sequential? data) [data] data)]
     (insert! table (map #(convert-datum-out % spec) add-data) spec))))
