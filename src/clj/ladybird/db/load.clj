(ns ladybird.db.load
    (:require [korma.db :as kdb]
              [ladybird.db.core :as c]
              [ladybird.misc.exception :as e]
              [clojure.edn :as edn]
              [ladybird.db.patch.teradata :as teradata]
     ))

;; load db
(def db-helpers {:mssql kdb/mssql
                 :oracle kdb/oracle
                 :mysql kdb/mysql
                 :msaccess kdb/msaccess
                 :teradata teradata/teradata
                 })

(defn parse-db-def [{:keys [dbms-type password decrypter] :as db-def}]
  (let [db-helper (db-helpers dbms-type)]
    (cond
      ;; TODO: Since korma 0.4.2 sets delimiters for mysql automatically, it is fine to remove :jndi-mysql in future.
      (= :jndi-mysql dbms-type) (-> (dissoc db-def :dbms-type) (assoc :delimiters "`"))
      (= :jndi dbms-type) (dissoc db-def :dbms-type)
      (nil? db-helper) (throw (e/sys-error :load-db-def-failed "Unknown dbms type" dbms-type))
      :others (let [passwrd (cond (and password decrypter) (decrypter password)
                                  password password
                                  )]
                (-> (if passwrd
                      (assoc db-def :password passwrd)
                      (dissoc db-def :password)
                      )
                    (dissoc :dbms-type :decrypter)
                    db-helper
                    )
                )
      )
    )
  )

(defn load-db-file
  "
  Paras:
    fpath  --  a file path string or any object which can be coerced to a java.io.Reader object by clojure.java.io/reader.

  Ex.
      =========== file default-main-db.def ===========  
      {:dbms-type :mysql
       :host \"localhost\"
       :port 3306
       :db \"cookbook\"
       :user \"username\"
       :password \"password\"
      }

      =========== file named-db.def =============
      {:qq {:dbms-type :mysql
            :host \"localhost\"
            :port 3306
            :db \"cookbook\"
            :user \"username\"
            :password \"password\"
           }
       :main {.......}
      }
  "
  [fpath]
  (let [db-defs-m (-> fpath slurp edn/read-string)
        db-defs-m (if (:dbms-type db-defs-m) {:main db-defs-m} db-defs-m)
        ]
    (doseq [[def-name db-def] db-defs-m]
           (c/init-db def-name (parse-db-def db-def)))))
