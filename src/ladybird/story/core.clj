(ns ladybird.story.core
    (:use [ladybird.util.symbol :only (str-symbol)]
          [ladybird.db.transaction :only (tx)]
     )
    )

(def ^:private DEFAULT-DB-NAME :main)

(def ^:dynamic *default-db-name* DEFAULT-DB-NAME)

(defmacro defstory-on-db [db-name name & fdecl]
  (let [inner-fn-name (str-symbol name "-inner-fn")
        inner-fn-name (with-meta inner-fn-name {:private true :db-name db-name})
        name (with-meta name {:db-name db-name})]
    `(do
       (defn- ~inner-fn-name ~@fdecl)
       (defn ~name [& args#] (tx ~db-name (apply ~inner-fn-name args#))))))

(defmacro defstory [name & body]
  `(defstory-on-db ~*default-db-name* ~name ~@body))

(defn load-stories-on-db [db-name story-ns & story-nses]
  (binding [*default-db-name* db-name]
           (apply require story-ns story-nses)))

;([name doc-string? attr-map? [params*] body] [name doc-string? attr-map? ([params*] body) + attr-map?])
