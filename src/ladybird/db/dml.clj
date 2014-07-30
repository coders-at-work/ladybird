(ns ladybird.db.dml
    (:require [ladybird.db.core :as dbc]
              [ladybird.db.patch.korma :as dbk]))

;; access db
(defn select
  "database select operation
   Args:
       table -- a string of table name, or a table object which is determined by the lower sql abstraction library
       where -- a list represent the sql where clause, ex. '(and (< :user_age 35) (> :user_age 20))
       spec -- a map of spec about database operation, lower sql adapter layer must translate this spec to forms used by the specified sql abstraction library 
               build-in keys as following:
                   :aggregate -- spec of aggregation, should be of form as: [[function-name field] alias]. function-name can be a string, a keyword or a symbol. field and alias should be a keyword.
                                 Ex. [[:count :id] :cnt], [[\"max\" :price] :most_expensive]
   Return:
       database records"
  ([table where]
    (select table where {}))
  ([table where {:keys [fields join aggregate] :as spec}]
    (let [db (->> (dbc/get-cur-conn) :conn-def)
          spec (assoc spec :db db)]
      (dbk/select table where spec))))

(defn insert!
  "insert database record 
   Args:
       table -- see also 'select' 
       data -- can be a map, or a seq of maps
       spec -- see also 'select'
   Return:"
  ([table data]
   (insert! table data {}))
  ([table data {:keys [fields] :as spec}]
    (dbk/insert! table data spec)))

(defn update!
  "update database record
   Args:
       table -- see also 'select' 
       datum -- a map specifying which fields to be changed
       where -- see also 'select' 
       spec -- see also 'select'
   Return:
       count of affected rows" 
  ([table datum where]
   (update! table datum where {}))
  ([table datum where {:keys [fields] :as spec}]
    (dbk/update! table datum where spec)))
