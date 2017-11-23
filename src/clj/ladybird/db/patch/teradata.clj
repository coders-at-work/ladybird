(ns ladybird.db.patch.teradata)

(defn teradata
  "Create a database specification for a teradata database. Opts should include keys
  for :db, :user, and :password. You can also optionally set host.
  Please refer to http://developer.teradata.com/doc/connectivity/jdbc/reference/current/frameset.html
  "
  [{:keys [host user password make-pool?]
    :or {host "localhost", user "", password "", make-pool? true}
    :as opts}]
  ; lein localrepo install path-to/terajdbc4.jar local.repo/terajdbc 16.00.00.33
  ; lein localrepo install path-to/tdgssconfig.jar local.repo/teratdgss 16.00.00.33
  (merge {:classname "com.teradata.jdbc.TeraDriver" ; must be in classpath
          :subprotocol "teradata"
          :subname (str "//" host)
          :make-pool? make-pool?}
         (dissoc opts :host))
  )

