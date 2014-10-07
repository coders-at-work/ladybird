(ns ladybird.db.load
    (:require [korma.db :as kdb]
              [ladybird.db.core :as c]
     ))

;; load db
(defn jndi [db-def]
  (kdb/create-db db-def))

(def db-helpers {:mssql kdb/mssql
                 :oracle kdb/oracle
                 :mysql kdb/mysql
                 :msaccess kdb/msaccess
                 :jndi jndi
                 })

(defn parse-db-def [{:keys [dbms-type password decrypter] :as db-def}]
  (let [db-helper (db-helpers dbms-type)]
    (cond
      (nil? db-helper) (throw (RuntimeException. "Unknown dbms type."))
      (= :jndi dbms-type) (dissoc db-def :dbms-type)
      :others (let [passowrd (if decrypter (decrypter password) password)]
                (-> (dissoc db-def :dbms-type :decrypter) (assoc :password password) db-helper)))))

(defn load-db-file [fpath]
  (let [db-defs-m (load-file fpath)
        db-defs-m (if (:dbms-type db-defs-m) {:main db-defs-m} db-defs-m)
        ]
    (doseq [[def-name db-def] db-defs-m]
           (c/init-db def-name (parse-db-def db-def)))))
