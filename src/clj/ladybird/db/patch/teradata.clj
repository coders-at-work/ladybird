(ns ladybird.db.patch.teradata)

(defn teradata
  "Create a database specification for a teradata database.
  Opts should include keys for :host, :user, and :password.
  You can also optionally set
  :db, :port
  Please refer to http://developer.teradata.com/doc/connectivity/jdbc/reference/current/frameset.html
  "
  [{:keys [host user password db port make-pool?]
    :or {host "localhost", user "", password "", make-pool? true}
    :as opts}]
  (let [subname (cond-> (str "//" host "/USER=" user ",PASSWORD=" password)
                  db (str ",DATABASE=" db)
                  port (str ",DBS_PORT=" port))]
    (merge {:classname "com.teradata.jdbc.TeraDriver" ; must be in classpath
            :subprotocol "teradata"
            :subname subname
            :make-pool? make-pool?}
           (dissoc opts :host :user :password :db :port))
    )
  )

