(ns ladybird.db.core
    (:use [clojure.java.jdbc.deprecated :only (find-connection with-connection transaction)]
          [ladybird.util.core :only (def-bindable)])
    (:require [ladybird.db.bridge :as dbb]))


;; transaction
(defmacro do-tx [connection & body]
    `(if-not (find-connection)
       (with-connection ~connection
                        (transaction ~@body))
       (transaction ~@body)))

;; connection map
(def ^:private conn-map (atom {}))

(defn- add-to-conn-map [name value]
       (reset! conn-map (assoc @conn-map name value)))

(defn get-conn [name]
  (@conn-map name))

;; current connection
(def-bindable cur-conn nil)

(defmacro with-cur-conn-as [name & body]
  `(with-cur-conn (get-conn ~name)
                  ~@body))

;; db preparation
(defn init-db [name db-def-map]
  (->> (dbb/init-db db-def-map) (add-to-conn-map name)))
