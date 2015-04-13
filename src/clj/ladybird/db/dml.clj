(ns ladybird.db.dml
    (:require [ladybird.db.core :as dbc]
              [ladybird.db.patch.korma :as dbk]
              [korma.core :as kc]))

(defn- assoc-spec-with-db [spec]
       (->> (dbc/get-cur-conn) :conn-def (assoc spec :db)))

;; construct sql
(def raw kc/raw)

;; access db
(defn select
  "database select operation
   Params:
       table -- a string of table name, or a table object which is determined by the lower sql abstraction library
       where -- a list represent the sql where clause, fields should be keywords, ex. '(and (< :user_age 35) (> :user_age 20))
       spec -- a map of spec about database operation, lower sql adapter layer must translate this spec to forms used by the specified sql abstraction library 
               build-in keys as following:
                   :fields -- a vector of field specs specifying which fields will be selected from the table. A field spec can be:
                                field  -- field name, must be a keyword
                                [field field-alias] -- field name and its' alias, both must be keywords
                   :aggregate -- spec of aggregation, should be of form as: [[function-name field] alias] or [[function-name field] alias group-by]. function-name can be a string, a keyword or a symbol. field, alias and group-by should be a keyword.
                                 Ex. [[:count :id] :cnt], [[\"max\" :price] :most_expensive :category_id]
                   :joins -- a map of join specs, each key will be used as the alias of the joined table, and will be used in :join-with.
                             Each value has the following forms:
                                 [join-type table fields on-clause]
                                     join-type -- can be :inner, :left or :right
                                     table -- the table name string
                                     fields -- like :fields above, but the field name should prefixed by the table alias
                                     on-clause -- on clause, its form is same as :where above
                                 Ex. 
                                    {:joins {:p [:inner \"person\" [:p.name :p.age] '(= :p.id :person_id)]
                                             :e [:left \"email\" [[:e.address :addr]] '(= :p.id :e.person_id)]}}
                   :join-with -- a vector of join names, each name is a key in :joins. Only these join specs will be used in query.
                                 Ex. :join-with [:p :e]
                   :modifier -- a string means a sql modifier in a select statement, like 'distinct'
                                Ex.
                                   :modifier \"distinct\"
                   :order -- a vector of order-by specs. Each element can be one of the following forms:
                                   :field-name -- means 'order by field-name asc'
                                   [:field-name :dir :another-field :another-dir ...] -- means 'order by field-name dir, another-field another-dir...', direction can be asc or desc
                                   Ex.
                                      :order [:age [:start-time :desc :end-time :asc] :create-time]
                   :offset -- a number means from which position the selecting results begin to return
                   :limit -- a number means the maximum rows will be returned
                   :group-by -- a collection of group-by fields
                                Example:
                                :group-by [:gender :age]
   Return:
       database records"
  ([table where]
    (select table where {}))
  ([table where {:keys [fields join-with joins aggregate group-by modifier order offset limit] :as spec}]
    (dbk/select table where (assoc-spec-with-db spec))))

(defn insert!
  "insert database record 
   Params:
       table -- same as 'select' 
       data -- can be a map, or a seq of maps
       spec -- see also 'select'
   Return:
       the id of the first inserted record"
  ([table data]
   (insert! table data {}))
  ([table data {:keys [fields] :as spec}]
    (dbk/insert! table data (assoc-spec-with-db spec))))

(defn update!
  "update database record
   Params:
       table -- same as 'select' 
       datum -- a map specifying which fields to be changed
       where -- same as 'select' 
       spec -- see also 'select'
   Return:
       count of affected rows" 
  ([table datum where]
   (update! table datum where {}))
  ([table datum where {:keys [fields] :as spec}]
    (dbk/update! table datum where (assoc-spec-with-db spec))))

(defn delete!
  "delete database record
   Params:
       table -- same as 'select' 
       where -- same as 'select' 
       spec -- see also 'select'
   Return:
       nil" 
  ([table where]
   (delete! table where {}))
  ([table where {:keys [fields] :as spec}]
   (dbk/delete! table where (assoc-spec-with-db spec))))
