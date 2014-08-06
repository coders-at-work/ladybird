(ns ladybird.db.dml
    (:require [ladybird.db.core :as dbc]
              [ladybird.db.patch.korma :as dbk]))

(defn- assoc-spec-with-db [spec]
       (->> (dbc/get-cur-conn) :conn-def (assoc spec :db)))

;; access db
(defn select
  "database select operation
   Args:
       table -- a string of table name, or a table object which is determined by the lower sql abstraction library
       where -- a list represent the sql where clause, ex. '(and (< :user_age 35) (> :user_age 20))
       spec -- a map of spec about database operation, lower sql adapter layer must translate this spec to forms used by the specified sql abstraction library 
               build-in keys as following:
                   :fields -- a vector of field specs specifying which fields will be selected from the table. A field spec can be:
                                field  -- field name, can be string or keyword
                                [field field-alias] -- field name and its' alias, both can be string or keyword
                   :aggregate -- spec of aggregation, should be of form as: [[function-name field] alias]. function-name can be a string, a keyword or a symbol. field and alias should be a keyword.
                                 Ex. [[:count :id] :cnt], [[\"max\" :price] :most_expensive]
                   :joins -- a vector of join specs. Each join spec has one of following forms:
                                   [join-type table fields on-clause]
                                   [join-type [table alias] fields on-clause]
                             join-type -- can be :inner, :left or :right
                             table -- the table name, can be string or keyword
                             alias -- the alias name, can be string or keyword
                             fields -- same as :fields above 
                             on-clause -- on condition, its form is same as where
                     Ex. 
                         {:joins [[:inner :person [:name :age] '(= :person.id :person_id)]
                                  [:left [:email :e] [[:address :addr]] '(= :person.id :e.person_id)]]}
                   ;
   Return:
       database records"
  ([table where]
    (select table where {}))
  ([table where {:keys [fields joins aggregate] :as spec}]
    (dbk/select table where (assoc-spec-with-db spec))))

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
    (dbk/insert! table data (assoc-spec-with-db spec))))

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
    (dbk/update! table datum where (assoc-spec-with-db spec))))

(defn delete!
  "delete database record
   Args:
       table -- see also 'select' 
       where -- see also 'select' 
       spec -- see also 'select'
   Return:
       nil" 
  ([table where]
   (delete! table where {}))
  ([table where {:keys [fields] :as spec}]
   (dbk/delete! table where (assoc-spec-with-db spec))))
