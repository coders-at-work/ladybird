(ns ladybird.ext.entity
    (:require [ladybird.domain.core :refer (def-enum-predicates defdomain)]
              [ladybird.data.enum :as enum]
              [ladybird.util.string :as lstr]
              )
    )

(defn change-add-fn [add-fn]
  (fn [& args]
      (:generated_keys (apply add-fn args))
      )
  )

(defn- gen-field-fns
  ""
  [field]
  (let [field-name (name field)
        assoc-field-name (str "assoc-" field-name)
        ]
    `(do
       (defn ~(symbol field-name)
         [~'ent]
         (~field ~'ent)
         )
       (defn ~(symbol assoc-field-name)
         [~'ent ~'v]
         (assoc ~'ent ~field ~'v)
         )
       )
    )
  )

(defn- gen-enum-predicates
  ""
  [entity entity-converters]
  (let [enum-fields (->>
                      (filter (fn [[f c]] (enum/enum? c)) entity-converters)
                      (map first)
                      )]
    (map
      (fn [f]
          `(def-enum-predicates ~entity ~f)
          )
      enum-fields
      )
    )
  )

(defn- gen-create-fn
  ""
  [entity-sym fields db-maintain-fields add-fixed]
  (let [ignore-fields (set db-maintain-fields)
        ignore-fields (apply conj ignore-fields (keys add-fixed))
        init-params (->> (filter #(not (ignore-fields %)) fields)
                         (mapv symbol)
                         )
        function-name (str "create-" (-> (name entity-sym) lstr/clj-case))
        function-body (->> (map #(vector (keyword %) %) init-params) (into {}))
        ]
    `(defn ~(symbol function-name)
       ~[{:keys init-params}]
       ~function-body
       )
    )
  )

(defmacro def-entity-fns
  [entity]
  (let [{:keys [converters fields db-maintain-fields add-fixed]} (eval entity)
        ]
    `(do
       ~@(map gen-field-fns fields)
       ~@(gen-enum-predicates entity converters)
       ~(gen-create-fn entity fields db-maintain-fields add-fixed)
       )
    )
  )

(defmacro defentity
  "A thin wrapper of ladybird.domain.core/defdomain. Accepts same arguments as defdomain. But it changes the implementation of add record function so that the function returns the generated id directly instead of a map. It also generates enum predicates for the domain. At last, it generates a function to create a new instance of the domain. This instance contains all fields except for db-maintain-fields and add-fixed fields. You can pass a map to this function. All fields you don't specify in the argument map will have a nil value.
   Another major difference between defentity and defdomain is that defentity will not use :last-update field as optimistic-locking-fields automatically.(This feature isn't implemented yet.)"
  [domain-name & args]
  `(do
     (defdomain ~domain-name ~@args)
     (->
       (ns-resolve ~'*ns* (-> (:add-fn-meta ~domain-name) first symbol))
       (alter-var-root change-add-fn)
       )
     (def-entity-fns ~domain-name)
     )
  )
