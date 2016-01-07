(ns ladybird.db.patch.t-korma
    (:require [midje.sweet :refer :all]
              [ladybird.db.patch.korma :refer :all :as pk]
              [korma.db :as kdb]
     ))

(facts "about debugging"
       (fact "original korma exec-sql function is kept"
             korma-exec-sql => (exactly @#'kdb/exec-sql))
       (fact "enter-debug-mode! change the root binding of #'korma.db/exec-sql"
             (enter-debug-mode!)
             @#'kdb/exec-sql => (exactly @#'pk/debug-exec-sql))
       (fact "exit-debug-mode! restore the root binding of #'korma.db/exec-sql"
             (exit-debug-mode!)
             @#'kdb/exec-sql => (exactly korma-exec-sql))
       )
