(ns ladybird.db.transaction
    (:use [clojure.java.jdbc.deprecated :only (find-connection with-connection transaction)]
          ladybird.db.core)
    )

;; transaction
(defmacro do-tx [connection & body]
    `(if-not (find-connection)
       (with-connection ~connection
                        (transaction ~@body))
       (transaction ~@body)))

(defmacro tx [db-name & body]
  `(binding [*cur-conn-name* ~db-name]
            (do-tx (get-cur-db-conn)
                   ~@body)))
