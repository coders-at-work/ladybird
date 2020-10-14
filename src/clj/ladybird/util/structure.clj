(ns ladybird.util.structure
    (:require [schema.core :as s]
      )
    (:import schema.core.Schema)
    )

(def ^:const ALTER-STRUCT ::alter-struct)
(def ^:const ADD ::add)
(def ^:const RENAME ::rename)
(def ^:const REMOVE-KEY ::remove-key)
(def ^:const REMOVE ::remove-key-list)

(defn remove-key
  "Tags k as a key to be removed."
  [k]
  [REMOVE-KEY k]
  )

(defn- src-target-keys
  [template-key]
  (if (sequential? template-key)
    template-key
    (let [real-k (if (s/optional-key? template-key) (:k template-key) template-key)]
      [real-k real-k])))

(defn- source-key
  [structure-key]
  (-> (src-target-keys structure-key) first)
  )

(defn- target-key
  [structure-key]
  (-> (src-target-keys structure-key) second)
  )

(defn- filter-struct-key-by-opts
  [struct-template opts]
  (let [src-key-grouped-opts (group-by #(source-key (first %)) opts)
        target-key-grouped-opts (group-by #(target-key (first %)) opts)
        src-key-in-opts? #(->> (first %) source-key (contains? src-key-grouped-opts))
        target-key-in-opts? #(->> (first %) target-key (contains? target-key-grouped-opts))
        ]
    (->>
      (filter
        (complement (some-fn src-key-in-opts? target-key-in-opts?))
        struct-template)
      (into {})
      )
    )
  )

(declare prepare-struct)
(defn- merge-struct-and-options
  [struct-schema options]
  (cond
    (instance? schema.core.Schema struct-schema) struct-schema
    (sequential? struct-schema) [(prepare-struct (first struct-schema) (first options))]
    (map? struct-schema) (let [prepared-struct (filter-struct-key-by-opts struct-schema options)
                               ]
                           (merge prepared-struct options))
    :otherwise struct-schema
    ))

(defn- normalize-keys-renaming-spec [renaming-keys-spec]
  (let [m (meta renaming-keys-spec)
        v renaming-keys-spec
        ]
    (->>
      (partition 2 2 v)
      (map
        (fn [rename-spec]
            (let [rename-spec (vec rename-spec)
                  rename-spec (if m (with-meta rename-spec m) rename-spec)
                  ]
              (vector rename-spec :_))))
      (into {}))
    )
  )

(defn- normalize-struct-spec
  [struct-spec]
  (cond
    (instance? schema.core.Schema struct-spec) struct-spec
    (sequential? struct-spec) [(normalize-struct-spec (first struct-spec))]
    (map? struct-spec) (reduce
                         (fn [ret [k v]]
                             (condp = k
                               RENAME (merge ret (normalize-keys-renaming-spec v))
                               REMOVE (reduce (fn [r remove-k] (assoc r (remove-key remove-k) :_)) ret v)
                               ADD (reduce (fn [r k] (assoc r k :_)) ret v)
                               (assoc ret k (normalize-struct-spec v))
                               )
                             )
                         {}
                         struct-spec
                         )
    :otherwise struct-spec
    )
  )

(defn- prepare-struct
  [struct-schema options]
  (merge-struct-and-options (normalize-struct-spec struct-schema) (normalize-struct-spec options))
  )

(defn- get-data-by-key-or-path
  [m k]
  (if (sequential? k)
    (when (seq k)
      (get-in m k))
    (get m k))
  )

(defn- contains-key-or-path?
  [m k]
  (if (sequential? k)
    (loop [current-m m
           k-seq (seq k)
           result false
           ]
          (if-let [[current-k] k-seq]
                  (if (contains? current-m current-k)
                    (recur (get current-m current-k) (next k-seq) true)
                    false
                    )
                  result
            )
          )
    (contains? m k)))

(defn- remove-key?
  [k]
  (and (vector? k) (= 2 (count k)) (= REMOVE-KEY (first k)))
  )

(defn- key-to-remove
  [remove-key]
  (second remove-key)
  )

(declare do-structure-as)
(defn- process-non-remove-key
  [structure-schema from ret k]
  (when from
    (let [is-optional-key (or (s/optional-key? k) (-> (meta k) :optional))
          [src-k target-k] (src-target-keys k)
          ]
      (if (or (contains-key-or-path? from src-k) (not is-optional-key))
        (assoc ret target-k (do-structure-as (structure-schema k) (get-data-by-key-or-path from src-k)))
        ret
        )
      )))

(defn- process-remove-keys
  [from removal-keys]
  (if (seq removal-keys)
    (apply dissoc from (map key-to-remove removal-keys))
    from
    )
  )

(defn- do-structure-as
  [structure-schema from]
  (cond
    (instance? schema.core.Schema structure-schema) from
    (map? structure-schema) (if (seq structure-schema)
                              (let [remove-keys (->> (keys structure-schema) (filter remove-key?))
                                    struct-sch (apply dissoc structure-schema remove-keys)
                                    from (process-remove-keys from remove-keys)
                                    ]
                                (if (seq struct-sch)
                                  (reduce
                                    (fn [ret k]
                                        (process-non-remove-key struct-sch from ret k)
                                        )
                                    {}
                                    (keys struct-sch)
                                    )
                                  from
                                  )
                                )
                              {}
                              )
    (sequential? structure-schema) (map #(do-structure-as (first structure-schema) %) from)
    :otherwise from
    ))

(defn structure-as
  "
  Example:
  1. select required keys - will add required keys with value nil if they are absent in a map
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1}) => {:a nil :b \"b\"}
  2. select a list of required keys
      (structure-as {ADD [:a :b]} {:b \"b\" :c 1}) => {:a nil :b \"b\"}
  3. select optional keys - will not add optional keys if they are absent
      (structure-as {(s/optional-key :a) s/Str :b s/Str} {:b \"b\" :c 1}) => {:b \"b\"}
  4. rename required keys
      (structure-as {[:a :aa] s/Str :b s/Str} {:b \"b\" :c 1}) => {:aa nil :b \"b\"}
  5. rename required keys and make them optional
      (structure-as {^:optional [:a :aa] s/Str :b s/Str} {:b \"b\" :c 1}) => {:b \"b\"}
  6. rename a list of keys
      (structure-as {RENAME [:a :aa :b :bb]} {:b \"b\" :c 1}) => {:aa nil :bb \"b\"}
      (structure-as {:a {RENAME [:b :bb]}} {:a {:b \"b\" :c 1}} ) => {:a {:bb \"b\"}}
      (structure-as {RENAME ^:optional [:a :aa :b :bb]} {:b 2}) => {:bb 2}
  7. select or rename keys(required or optional) from nil always returns nil
      (structure-as {:a :_ :b {:c :_} ADD [:d :e]} nil) => nil
      (structure-as {(s/optional-key :a) :_} nil) => nil
      (structure-as {^:optional [:a :a] :_} nil) => nil
      (structure-as {RENAME [:a :aa :b :bb]} nil) => nil
      (structure-as {RENAME ^:optional [:a :aa :b :bb]} nil) => nil
  8. select or rename required keys with value nil returns a map containing the keys with value nil
      (structure-as {:a :_ :b {:c :_} ADD [:d :e]} {}) => {:a nil :b nil :d nil :e nil}
      (structure-as {:a :_ :b {:c :_} ADD [:d :e]} {:a nil :b nil :d nil :e nil}) => {:a nil :b nil :d nil :e nil}
      (structure-as {RENAME [:a :aa :b :bb]} {}) => {:aa nil :bb nil}
      (structure-as {RENAME [:a :aa :b :bb]} {:a nil :b nil}) => {:aa nil :bb nil}
  9. remove keys
      (structure-as {(remove-key :b) :_} {:b \"b\" :c 1}) => {:c 1}
      (structure-as {(remove-key :a) :_ (remove-key :b) :_} {:a 0 :b \"b\" :c 1}) => {:c 1}
  10. if both remove keys and non-remove keys are specified, it means first removes the specified remove keys then apply other non-remove structure template
      (structure-as {(remove-key :b) :_ [:c :cc] :_} {:b \"b\" :c 1 :d 2}) => {:cc 1}
  11. remove a list of keys
      (structure-as {REMOVE [:a :b]} {:a 0 :b \"b\" :c 1}) => {:c 1}
      (structure-as {REMOVE [:a :b] [:c :cc] :_} {:a 0 :b \"b\" :c 1}) => {:cc 1}
  12. rename a path
      (structure-as {[[:a :b] :d] s/Str :c s/Int} {:a {:b \"b\"} :c 1}) => {:d \"b\" :c 1}
      (structure-as {^:optional [[:a :b] :d] s/Str :c s/Str} {:b \"b\" :c 1}) => {:c 1}
      (structure-as {:a {:b s/Int, [[:d :e] :dd] s/Int} :c s/Str} {:a {:b 2 :d {:e 3}} :c 1}) => {:a {:b 2 :dd 3} :c 1}
  13. rename a path by specifying it within a list
      (structure-as {RENAME [[:a :b] :d] :c :_} {:a {:b \"b\"} :c 1}) => {:d \"b\" :c 1}
  14. select and rename a list of keys
      (structure-as {ADD [[:a :aa] :b [[:c :d] :dd]]} {:b \"b\" :c {:d 1}}) => {:aa nil :b \"b\" :dd 1}
      (structure-as {:a {ADD [:b]}} {:a {:b \"b\" :c 1}}) => {:a {:b \"b\"}}
  15. treat vector schema as a sequence
      (structure-as {:a s/Str :b [s/Str]} {:b [\"b\" \"bb\"] :c 1}) => {:a nil :b '(\"b\" \"bb\")}
  16. can use an option map/vector to rename keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true}) => {:aa nil :b \"b\"}
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {^:optional [:a :aa] true}) => {:b \"b\"}
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c {:d 1}} {[:a :aa] true [[:c :d] :dd] true}) => {:aa nil :b \"b\" :dd 1}
  17. can use an option map/vector to rename a list of keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {RENAME [:a :aa :b :bb]}) => {:aa nil :bb \"b\"}
      (structure-as {:a {:b :_ :c :_}} {:a {:b \"b\" :c 1}} {:a {RENAME [:b :bb]}}) => {:a {:bb \"b\"}}
      (structure-as [{:a s/Str :b s/Str}] [{:b \"b\" :c 1}] [{RENAME [:a :aa :b :bb]}]) => '({:aa nil :bb \"b\"})
  18. can use an option map/vector to remove a list of keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {REMOVE [:a]}) => {:b \"b\"}
      (structure-as {:a {:b :_ :c :_}} {:a {:b \"b\" :c 1 :d 2}} {:a {REMOVE [:b]}}) => {:a {:c 1 :d 2}}
      (structure-as [{:a s/Str :b s/Str}] [{:b \"b\" :c 1}] [{REMOVE [:a]}]) => '({:b \"b\"})
  19. if there is a same top level key in both schema and option map, the option map wins
      (structure-as {[:a :AA] s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true}) => {:aa nil :b \"b\"}
      (structure-as {:a {:b :_ [:c :CC] :_}} {:a {:b 1 :c 2}} {:a {[:c :cc] :_}}) => {:a {:cc 2}}
  20. if both schema and option map/vector want to take a same target key, the option map/vector wins
      (structure-as {:AA s/Str :b s/Str} {:b \"b\" :c 1} {[:c :AA] true}) => {:AA 1 :b \"b\"}
      (structure-as {[:a :AA] s/Str :b s/Str} {:b \"b\" :c 1} {[:c :AA] true}) => {:AA 1 :b \"b\"}
  21. option map/vector can contain extra field, all different keys in schema and option map/vector will be included in the result
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true, [:c :cc] true}) => {:aa nil :b \"b\" :cc 1}
      (structure-as {:a :_} {:a 1 :b 2 :c 3} {:b :_}) => {:a 1 :b 2}
  22. can use meta data to rename keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :aa] true, [:c :cc] true}}) {:b \"b\" :c 1}) => {:aa nil :b \"b\" :cc 1}
      (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :aa] true}]}) [{:a 1 :b 2} {:b 3}]) => '({:aa 1 :b 2} {:b 3})
  23. can use meta data to rename a list of keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {RENAME [:a :aa :b :bb]}}) {:b \"b\" :c 1}) => {:aa nil :bb \"b\"}
      (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {RENAME [:b :bb]}}}) {:a {:b \"b\" :c 1}}) => {:a {:bb \"b\"}}
      (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{RENAME [:a :aa :b :bb]}]}) [{:b \"b\" :c 1}]) => '({:aa nil :bb \"b\"})
  24. can use meta data to remove a list of keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {REMOVE [:a]}}) {:b \"b\" :c 1}) => {:b \"b\"}
      (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {REMOVE [:b]}}}) {:a {:b \"b\" :c 1 :d 2}}) => {:a {:c 1 :d 2}}
      (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{REMOVE [:a]}]}) [{:b \"b\" :c 1}]) => '({:b \"b\"})
  25. if both meta data and option map/vector are specified, only option map/vector takes effect
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}}) {:b \"b\" :c 1} {[:a :aa] true, [:c :cc] true}) => {:aa nil :b \"b\" :cc 1}
      (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :AA] true}]}) [{:a 1 :b 2} {:b 3}] [{^:optional [:a :aa] true}]) => '({:aa 1 :b 2} {:b 3})
  26. if option map/vector is nil, then meta data takes effect
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}}) {:b \"b\" :c 1} nil) => {:AA nil :b \"b\" :cc 1}
  27. but empty option map/vector will disable the meta data
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}}) {:b \"b\" :c 1} {}) => {:a nil :b \"b\"}
  "
  ([structure-schema from opts]
   (let [m (if opts
             {ALTER-STRUCT opts}
             (meta structure-schema))]
     (structure-as (with-meta structure-schema m) from))
   )
  ([structure-schema from]
   (let [m (meta structure-schema)
         structure-schema (prepare-struct structure-schema (ALTER-STRUCT m))
         ]
     (do-structure-as structure-schema from)
     )))
