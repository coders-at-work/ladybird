(ns ladybird.story.core
    (:use [ladybird.util.symbol :only (str-symbol)]
          [ladybird.db.transaction :only (tx non-tx)]))

(def ^:private DEFAULT-DB-NAME :main)

(def ^:dynamic *default-db-name* DEFAULT-DB-NAME)


(defn- make-macro
  [tx-op db-name name & body]
  (let [inner-fn-name (str-symbol name "-inner-fn")
        inner-fn-name (with-meta inner-fn-name {:private true :db-name db-name})
        name (with-meta name {:db-name db-name})]
    `(do
       (defn- ~inner-fn-name ~@body)
       (defn ~name [& args#] (~tx-op ~db-name (apply ~inner-fn-name args#))))))

;; transactional story
(defmacro defstory-on-db
  "Define a transactional story on the specified db connection.
   Parameters:
      db-name   --    The name of the db connection, same as the 'name' parameter in ladybird.db.core/init-db
      name      --    The name of the story function. The macro will define a private inner function, and a public function named by this parameter.
      body      --    The body of the story function."
  [db-name name & body]
  (apply make-macro `tx db-name name body))

(defmacro defstory
  "Define a transactional story on the db connection which is specified by the current value of *default-db-name*."
  [name & body]
  `(defstory-on-db ~*default-db-name* ~name ~@body))

;; non-transactional story
(defmacro deFn-on-db
  "Define a non-transactional function like story on the specified db connection. In the generated story, you can access the specified db."
  [db-name name & body]
  (apply make-macro `non-tx db-name name body))

(defmacro deFn
  "Define a non-transactional function like story on the db connection which is specified by the current value of *default-db-name*."
  [name & body]
  `(deFn-on-db ~*default-db-name* ~name ~@body))

;; load stories
(defn load-stories-on-db
  "Load story namespaces on the specified db connecton. Require the story namespaces with db connection binding to the specified db name."
  [db-name story-ns & story-nses]
  (binding [*default-db-name* db-name]
           (apply require story-ns story-nses)))

;([name doc-string? attr-map? [params*] body] [name doc-string? attr-map? ([params*] body) + attr-map?])
