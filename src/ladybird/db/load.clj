(ns ladybird.db.load
    (:require [korma.db :as kdb]
              [ladybird.db.core :as c]
     ))

;; load db
(def db-helpers {:mssql kdb/mssql
                 :oracle kdb/oracle
                 :mysql kdb/mysql
                 :msaccess kdb/msaccess
                 })

(defn parse-db-def [{:keys [dbms-type password decrypter] :as db-def}]
  (let [db-helper (db-helpers dbms-type)]
    (cond
      (= :jndi dbms-type) (dissoc db-def :dbms-type)
      (nil? db-helper) (throw (RuntimeException. "Unknown dbms type."))
      :others (let [passowrd (if decrypter (decrypter password) password)]
                (-> (dissoc db-def :dbms-type :decrypter) (assoc :password password) db-helper)))))

(defn load-db-file
  "
  Ex.
      =========== file default-main-db.def ===========  
      {:dbms-type :mysql
       :host \"localhost\"
       :port 3306
       :db \"cookbook\"
       :user \"xzj\"
       :password \"xzjamjzx\"
      }

      =========== file named-db.def =============
      {:qq {:dbms-type :mysql
            :host \"localhost\"
            :port 3306
            :db \"cookbook\"
            :user \"xzj\"
            :password \"xzjamjzx\"
           }
       :main {.......}
      }
  "
  [fpath]
  (let [db-defs-m (load-file fpath)
        db-defs-m (if (:dbms-type db-defs-m) {:main db-defs-m} db-defs-m)
        ]
    (doseq [[def-name db-def] db-defs-m]
           (c/init-db def-name (parse-db-def db-def)))))
