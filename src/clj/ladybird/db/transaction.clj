(ns ladybird.db.transaction
    (:use ladybird.db.core)
    (:require [korma.db :as kdb]))

;; transaction
(defmacro do-tx [connection & body]
  `(if-not kdb/*current-conn*
     (binding [kdb/*current-conn* ~connection]
              (kdb/transaction ~@body))
     (kdb/transaction ~@body)))

(defmacro tx [db-name & body]
  ; TODO: checks whether *cur-conn-name* is bound and whether the bound value equals to db-name and throws exception if they are not the same
  `(binding [*cur-conn-name* ~db-name]
            (do-tx (get-cur-db-conn)
                   ~@body)))

;; non-transaction
(defmacro non-tx [db-name & body]
  `(binding [*cur-conn-name* ~db-name]
            ~@body))
