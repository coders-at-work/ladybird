(ns ladybird.db.core
    (:use [ladybird.util.core :only (def-bindable)])
    (:require [ladybird.db.patch.korma :as dbk]))


;; connection map
(def ^:private conn-map
     "Each key is a name, the value is a map containing following keys:
         :original-conn-def   --    the original connection definition map, which is specified by db users
         :conn-def            --    the connection definition object, which is implemented by the underground db access framwork
         :db-conn             --    the jdbc connection"
     (atom {}))

(defn- add-to-conn-map [name value]
       (reset! conn-map (assoc @conn-map name value)))

(defn get-conn
  "Get the db connection related objects specified by the name."
  [name]
  (@conn-map name))

(defn get-db-conn
  "Get the jdbc connection specified by the name."
  [name]
  (get-in @conn-map [name :db-conn]))

;; current connection
(def ^:dynamic *cur-conn-name* nil)

(defn cur-conn-name
  "Get the current db connection name. If *cur-conn-name* has been bound, return it; otherwise, return the main db connection name(:main)."
  []
  (or *cur-conn-name* :main))

(defn get-cur-conn
  "Get the current db connection related objects."
  []
  (get-conn (cur-conn-name)))

(defn get-cur-db-conn
  "Get the current jdbc connection."
  []
  (get-db-conn (cur-conn-name)))

;; db preparation
(defn init-db
  "Init a db connection and name it."
  [name db-def-map]
  (->> (dbk/init-db db-def-map) (merge {:original-conn-def db-def-map}) (add-to-conn-map name)))

(defn init-main-db
  "Init the main db connection(the db connection named :main)."
  [db-def-map]
  (init-db :main db-def-map))
