(ns ladybird.db.patch.t-korma
    (:require [midje.sweet :refer :all]
              [ladybird.db.patch.korma :refer :all :as pk]
              [korma.db :as kdb]
              [korma.sql.engine :as eng]
     ))

(facts "about debugging"
       (fact "original korma exec-sql function is kept"
             korma-exec-sql => (exactly @#'kdb/exec-sql))
       (fact "enter-debug-mode! change the root binding of #'korma.db/exec-sql"
             (enter-debug-mode!)
             @#'kdb/exec-sql => (exactly @#'pk/debug-exec-sql))
       (fact "exit-debug-mode! restore the root binding of #'korma.db/exec-sql"
             (exit-debug-mode!)
             @#'kdb/exec-sql => (exactly korma-exec-sql)))

(facts "about limit offset patch"
       (fact "alter root bingding of #'eng/sql-limit-offset and keep the original eng/sql-limit-offset function"
             sql-limit-offset => (exactly @#'eng/sql-limit-offset)
             korma-sql-limit-offset =not=> (exactly @#'eng/sql-limit-offset))
       (fact "will generate ' limit x offset y ' style sql string if subprotocol is not sqlserver"
             (binding [eng/*bound-options* {:subprotocol "mysql"}]
                      (sql-limit-offset {:limit 10 :offset 9}) => {:limit 10 :offset 9 :sql-str " LIMIT 10 OFFSET 9"}))
       (fact "will generate sqlserver 2012 style sql string if subprotocol is sqlserver"
             (binding [eng/*bound-options* {:subprotocol "sqlserver"}]
                      (sql-limit-offset {:limit 10 :offset 9}) => {:limit 10 :offset 9 :sql-str " offset 9 rows fetch next 10 rows only"}))
       (fact "offset will be 0 if subprotocol is sqlserver and only limit is specified"
             (binding [eng/*bound-options* {:subprotocol "sqlserver"}]
                      (sql-limit-offset {:limit 10}) => {:limit 10 :sql-str " offset 0 rows fetch next 10 rows only"}))
       (fact "offset limit string will be empty string if both are not specified"
             (binding [eng/*bound-options* {:subprotocol "sqlserver"}]
                      (sql-limit-offset {}) => {:sql-str ""})
             (binding [eng/*bound-options* {:subprotocol "mysql"}]
                      (sql-limit-offset {}) => {:sql-str ""})))

(facts "about transforming order spec to korma style"
       (fact "will be empty if order spec is empty"
             (to-korma-order nil) => ()
             (to-korma-order ()) => ()
             (to-korma-order []) => ())
       (fact "will be ordered by asc if direction is not specified"
             (to-korma-order [:id]) => '(:id :asc))
       (fact "can specify direction"
             (to-korma-order [:id#desc]) => '(:id :desc))
       (fact "can mix the two flavor above"
             (to-korma-order [:id :name#desc :age]) => '(:id :asc :name :desc :age :asc))
       (fact "can put :field :dir pairs in a vector"
             (to-korma-order [:id [:name :desc :age :asc]]) => '(:id :asc :name :desc :age :asc))
       (fact "a comprehensive example"
             (to-korma-order [:id :a :b#desc :c#asc [:name :desc :age :asc] :d#desc :e]) => '(:id :asc :a :asc :b :desc :c :asc :name :desc :age :asc :d :desc :e :asc)))
