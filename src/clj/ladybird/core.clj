(ns ladybird.core
  (require [ladybird.db.load :as load])
  (require [korma.core :refer (exec-raw)])
  (require [ladybird.db.transaction :as tx])
  (require [ladybird.db.core :as db])
  (:gen-class)
  )

; chaining generation
(defn chain-proc
  "
  Process data through chaining functions in proc-fns. The left most function will process data first, then the second function, and so on.

  Ex:
     (chain-proc 3 #(* % 2) inc) => 7
     (chain-proc 3) => 3
  "
  [data & proc-fns]
  (let [proc-fn (->> (reverse proc-fns) (apply comp))]
    (proc-fn data)))

(defmacro chain-gen
  "
  Expand to process meta-m(a map) through chaining functions in gen-fns, then evaluate the result. Can be used to generate code.
  Params:
     meta-m   --  a map of meta data
     gen-fns  --  functions to generate some code

  Ex:
     (chain-gen {:a 1} ) => {:a 1}
     (chain-gen {:v 3} #(assoc % :f `(inc ~(:v %))) (fn [m] `(* 2 ~(:f m)))) => 8
  "
  [meta-m & gen-fns]
  `(eval (chain-proc ~meta-m ~@gen-fns)))

(defn -main [db-file & args]
  (println "Hello world!")
  (load/load-db-file db-file)
  (let [results (tx/do-tx (db/get-db-conn :main) (exec-raw "select * from dbcinfo" :results))]
    (println :results results)
    )
  )
