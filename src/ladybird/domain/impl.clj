(ns ladybird.domain.impl
    (:require [clojure.string :as string]
              [ladybird.util.string :as str]
              [ladybird.db.dml :as dml]
              ))

;; convert db record related
(defn- clj-case-to-db-case [s]
       (-> s string/lower-case str/hyphen-to-underscore))

(defn- domain-field-to-db-field [field]
       (-> (name field) clj-case-to-db-case keyword))

(defn make-select-fields
  "Translate domain fields definition to sql select [db-field alias] pairs. A field is a keyword."
  [& fields]
  (reduce (fn [ret domain-field]
              (conj ret [(domain-field-to-db-field domain-field) domain-field]))
          [] fields))

;; meta data
(defn create-select-spec [{:keys [fields ] :as query-spec}]
  (let [ret {}
        fields (apply make-select-fields fields)
        ret (if-not (empty? fields) (assoc ret :fields fields) ret)
        ]
    ret))

;; crud
(defn query
  ""
  ([table condition]
   (query table condition {}))
  ;; TODO use converters to translate condition, ex. for boolean values
  ([table condition {:keys [fields converters aggregate join] :as query-spec}]
         (let [spec (create-select-spec query-spec) #_(if-not (empty? fields)
                                                            (assoc spec :fields (apply make-select-fields fields))
                                                            fields)
               ]
     (dml/select table condition spec))
   #_(->> (select table (condition-to-where condition) spec)
        (map #(convert-datum-in % spec)))) 
  )
