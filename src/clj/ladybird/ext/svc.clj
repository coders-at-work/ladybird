(ns ladybird.ext.svc
    (:require ;[glp-host-app-api.user-context :as usr-ctxt]
              ;[glp-host-app-api.middleware.exception :refer (forbidden-ex)]
              [ladybird.svc.core :refer (encapsule-body check-and-bind gen-defn t-svc) :as ladybird-svc]
              [schema.core :as s]
              [clojure.walk :refer (postwalk)]
              )
    (:import schema.core.Schema)
    )

;; svc result transformation
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

(defn- normalize-struct-spec
  [struct-spec]
  (cond
    (instance? schema.core.Schema struct-spec) struct-spec
    (sequential? struct-spec) [(normalize-struct-spec (first struct-spec))]
    (map? struct-spec) (reduce
                         (fn [ret [k v]]
                             (condp = k
                               RENAME (->> (partition 2 2 v) (map #(vector (vec %) :_)) (into {}) (merge ret))
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
  (let [is-optional-key (or (s/optional-key? k) (-> (meta k) :optional))
        [src-k target-k] (src-target-keys k)
        ]
    (if (or (contains-key-or-path? from src-k) (not is-optional-key))
      (assoc ret target-k (do-structure-as (structure-schema k) (get-data-by-key-or-path from src-k)))
      ret
      )
    ))

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
  1. select required keys - will add required keys if they are absent
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
  7. remove keys
      (structure-as {(remove-key :b) :_} {:b \"b\" :c 1}) => {:c 1}
      (structure-as {(remove-key :a) :_ (remove-key :b) :_} {:a 0 :b \"b\" :c 1}) => {:c 1}
  8. if both remove keys and non-remove keys are specified, it means first removes the specified remove keys then apply other non-remove structure template
      (structure-as {(remove-key :b) :_ [:c :cc] :_} {:b \"b\" :c 1 :d 2}) => {:cc 1}
  9. remove a list of keys
      (structure-as {REMOVE [:a :b]} {:a 0 :b \"b\" :c 1}) => {:c 1}
      (structure-as {REMOVE [:a :b] [:c :cc] :_} {:a 0 :b \"b\" :c 1}) => {:cc 1}
  10. rename a path
      (structure-as {[[:a :b] :d] s/Str :c s/Int} {:a {:b \"b\"} :c 1}) => {:d \"b\" :c 1}
      (structure-as {^:optional [[:a :b] :d] s/Str :c s/Str} {:b \"b\" :c 1}) => {:c 1}
      (structure-as {:a {:b s/Int, [[:d :e] :dd] s/Int} :c s/Str} {:a {:b 2 :d {:e 3}} :c 1}) => {:a {:b 2 :dd 3} :c 1}
  11. rename a path by specifying it within a list
      (structure-as {RENAME [[:a :b] :d] :c :_} {:a {:b \"b\"} :c 1}) => {:d \"b\" :c 1}
  12. select and rename a list of keys
      (structure-as {ADD [[:a :aa] :b [[:c :d] :dd]]} {:b \"b\" :c {:d 1}}) => {:aa nil :b \"b\" :dd 1}
      (structure-as {:a {ADD [:b]}} {:a {:b \"b\" :c 1}}) => {:a {:b \"b\"}}
  13. treat vector schema as a sequence
      (structure-as {:a s/Str :b [s/Str]} {:b [\"b\" \"bb\"] :c 1}) => {:a nil :b '(\"b\" \"bb\")}
  14. can use an option map/vector to rename keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true}) => {:aa nil :b \"b\"}
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {^:optional [:a :aa] true}) => {:b \"b\"}
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c {:d 1}} {[:a :aa] true [[:c :d] :dd] true}) => {:aa nil :b \"b\" :dd 1}
  15. can use an option map/vector to rename a list of keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {RENAME [:a :aa :b :bb]}) => {:aa nil :bb \"b\"}
      (structure-as {:a {:b :_ :c :_}} {:a {:b \"b\" :c 1}} {:a {RENAME [:b :bb]}}) => {:a {:bb \"b\"}}
      (structure-as [{:a s/Str :b s/Str}] [{:b \"b\" :c 1}] [{RENAME [:a :aa :b :bb]}]) => '({:aa nil :bb \"b\"})
  16. can use an option map/vector to remove a list of keys
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {REMOVE [:a]}) => {:b \"b\"}
      (structure-as {:a {:b :_ :c :_}} {:a {:b \"b\" :c 1 :d 2}} {:a {REMOVE [:b]}}) => {:a {:c 1 :d 2}}
      (structure-as [{:a s/Str :b s/Str}] [{:b \"b\" :c 1}] [{REMOVE [:a]}]) => '({:b \"b\"})
  17. if there is a same top level key in both schema and option map, the option map wins
      (structure-as {[:a :AA] s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true}) => {:aa nil :b \"b\"}
      (structure-as {:a {:b :_ [:c :CC] :_}} {:a {:b 1 :c 2}} {:a {[:c :cc] :_}}) => {:a {:cc 2}}
  18. if both schema and option map/vector want to take a same target key, the option map/vector wins
      (structure-as {:AA s/Str :b s/Str} {:b \"b\" :c 1} {[:c :AA] true}) => {:AA 1 :b \"b\"}
      (structure-as {[:a :AA] s/Str :b s/Str} {:b \"b\" :c 1} {[:c :AA] true}) => {:AA 1 :b \"b\"}
  19. option map/vector can contain extra field, all different keys in schema and option map/vector will be included in the result
      (structure-as {:a s/Str :b s/Str} {:b \"b\" :c 1} {[:a :aa] true, [:c :cc] true}) => {:aa nil :b \"b\" :cc 1}
      (structure-as {:a :_} {:a 1 :b 2 :c 3} {:b :_}) => {:a 1 :b 2}
  20. can use meta data to rename keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :aa] true, [:c :cc] true}}) {:b \"b\" :c 1}) => {:aa nil :b \"b\" :cc 1}
      (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :aa] true}]}) [{:a 1 :b 2} {:b 3}]) => '({:aa 1 :b 2} {:b 3})
  21. can use meta data to rename a list of keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {RENAME [:a :aa :b :bb]}}) {:b \"b\" :c 1}) => {:aa nil :bb \"b\"}
      (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {RENAME [:b :bb]}}}) {:a {:b \"b\" :c 1}}) => {:a {:bb \"b\"}}
      (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{RENAME [:a :aa :b :bb]}]}) [{:b \"b\" :c 1}]) => '({:aa nil :bb \"b\"})
  22. can use meta data to remove a list of keys
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {REMOVE [:a]}}) {:b \"b\" :c 1}) => {:b \"b\"}
      (structure-as (with-meta {:a {:b :_ :c :_}} {ALTER-STRUCT {:a {REMOVE [:b]}}}) {:a {:b \"b\" :c 1 :d 2}}) => {:a {:c 1 :d 2}}
      (structure-as (with-meta [{:a s/Str :b s/Str}] {ALTER-STRUCT [{REMOVE [:a]}]}) [{:b \"b\" :c 1}]) => '({:b \"b\"})
  23. if both meta data and option map/vector are specified, only option map/vector takes effect
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}}) {:b \"b\" :c 1} {[:a :aa] true, [:c :cc] true}) => {:aa nil :b \"b\" :cc 1}
      (structure-as (with-meta [{:a ..schema-a.. :b ..schema-b..}] {ALTER-STRUCT [{^:optional [:a :AA] true}]}) [{:a 1 :b 2} {:b 3}] [{^:optional [:a :aa] true}]) => '({:aa 1 :b 2} {:b 3})
  24. if option map/vector is nil, then meta data takes effect
      (structure-as (with-meta {:a s/Str :b s/Str} {ALTER-STRUCT {[:a :AA] true, [:c :cc] true}}) {:b \"b\" :c 1} nil) => {:AA nil :b \"b\" :cc 1}
  25. but empty option map/vector will disable the meta data
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

(defn transform
  "
   Adds the ability to declare transformation function for the body returned value when defining a service.
   Supporting keys are:
       :=to=> tr-fn                =>        (tr-fn returned-value)
       :=map=> tr-fn               =>        (map tr-fn returned-value)
       :=struct=> schema           =>        (structure-as schema returned-value), 用法如下：
                                              1. select required keys 会补上缺失的 required keys，值为 nil
                                                  :=struct=> {:a _ :b s/Str}
                                                  {:b \"b\"} => {:a nil :b \"b\"}
                                              2. rename keys
                                                  :=struct=> {:a _ :b {[:c :cc] s/Int, [:e :ee] s/Int}}
                                                  {:a 1 :b {:c 2 :d 3}} => {:a 1 :b {:cc 2 :ee nil}}
                                              3. rename keys and make them optional, 转换结果不会补上缺失的 optioanl keys
                                                  :=struct=> [{:a _, ^:optional [:b :bb] s/Int}]
                                                  [{:a 1} {:a 2 :b 3}] => ({:a 1} {:a 2 :bb 3})
                                              4. rename a list of keys
                                                 :=struct=> {* [:a :aa :b :bb] :c {* [:d :dd]}}
                                                 {:a 1 :b 2 :c {:d 3 :e 4}} => {:aa 1 :bb 2 :c {:dd 3}}
                                              5. select a list of required keys and rename them if needed
                                                 :=struct=> {+ [:a [[:c :d] :dd]]}
                                                 {:a 1 :b 2 :c {:d 3 :e 4}} => {:a 1 :dd 3}
                                              6. remove a list of keys
                                                 :=struct=> {- [:a :b] :c {- [:d]}}
                                                 {:a 1 :b 2 :c {:d 3 :e 4}} => {:c {:e 4}}
                                              7. using meta data to alter structure of existed schema
                                                  (def Sch {:a s/Int :b {:c s/Int :d s/Int}})
                                                  :=struct=> ^{:alter-struct {[:a :aa] _, [:b :bb] {^:optional [:d :dd] _}}} Sch
                                                  {:a 1 :b {:c 2}} => {:aa 1 :bb {:c 2}}
                                              8. or you can define schema with meta information
                                                  (def Sch ^{:alter-struct {[:a :aa] _, [:b :bb] {^:optional [:d :dd] _}}} {:a s/Int :b {:c s/Int :d s/Int}})
                                                  :=struct=> Sch
                                                  {:a 1 :b {:c 2}} => {:aa 1 :bb {:c 2}}
                                              9. you can use schema.core/optional-key to specify optional keys
                                                  :=struct=> {(s/optional-key :a) s/Int :b s/Int}
                                                  {:b 2 :c 3} => {:b 2}
                                              See also structure-as.
  "
  [{:keys [options body-form] :as meta-data}]
  (let [{:keys [=to=> =map=> =struct=>]} options
        _ (when (< 1 (-> (keep identity [=to=> =map=> =struct=>]) count))
            (throw (ex-info "only one of :=to=>, :=map=> and :=struct=> can be specified" options)))
        =struct=> (when =struct=>
                    (postwalk
                      #(case %
                         * RENAME
                         - REMOVE
                         + ADD
                         _ :_
                         %)
                      =struct=>))
        body-form (cond
                    =to=> `(~=to=> ~body-form)
                    =map=> `(map ~=map=> ~body-form)
                    =struct=> `(structure-as ~=struct=> ~body-form ~(some-> (meta =struct=>) :alter-struct))
                    :default body-form)
        ]
    (assoc meta-data :body-form body-form)))

(defn with-permission
  ""
  [{:keys [options body-form] :as meta-data}]
  (let [{:keys [permission permission-check]} options]
    (if permission
      (let [{:keys [check-fn fail-fn]} permission-check
            body-form `(if (~check-fn ~permission) ~body-form (~fail-fn ~permission))
            ]
        (assoc meta-data :body-form body-form)
        )
      meta-data
      )
    )
  )

;; defsvc
; (def ^:const svc-cfg-keys #{:out-fn :out-seq-fn :out-struct :session :permission :permission-check :check-and-bind})
(defn- group-svc-data
  [svc-data]
  (let [svc-data-pairs (partition 2 2 nil svc-data)]
    (reduce
      (fn [{:keys [svc-body] :as grouped-data} [k v :as pair]]
          (cond
            (and (empty? svc-body) (keyword? k)) (update-in grouped-data [:svc-cfg] #(conj % [k v]))
            :otherwise (update-in grouped-data [:svc-body] #(if (= 1 (count pair)) (conj % k) (conj % k v)))
            )
          )
      {:svc-cfg [] :svc-body []}
      svc-data-pairs
      )
    )
  )

(defmulti join-svc-meta-of (fn [svc-meta k v] k))
(defmethod join-svc-meta-of
  :out-fn
  [svc-meta k v]
  (assoc svc-meta :=to=> v)
  )
(defmethod join-svc-meta-of
  :out-seq-fn
  [svc-meta k v]
  (assoc svc-meta :=map=> v)
  )
(defmethod join-svc-meta-of
  :out-struct
  [svc-meta k v]
  (assoc svc-meta :=struct=> v)
  )
(defmethod join-svc-meta-of
  :check-and-bind
  [svc-meta k v]
  (assoc svc-meta k v)
  )
(defmethod join-svc-meta-of
  :permission
  [svc-meta k v]
  (assoc svc-meta k v)
  )
(defmethod join-svc-meta-of
  :default
  [svc-meta k v]
  (assoc svc-meta k v)
  )

(defn- make-svc-meta
  [svc-cfg]
  (loop [svc-cfg svc-cfg
         svc-meta {}
         ]
        (if-let [[k v] (first svc-cfg)]
          (recur (next svc-cfg) (join-svc-meta-of svc-meta k v) )
          svc-meta
          )
        )
  )

(defmacro defsvc
  "
  Example:
  (defsvc s
    \"doc-string\"
    :session true ; if :session config isn't specified, it is equal to :session true
    :out-fn out-function ; a function to convert output data returned from body
    :out-seq-fn map-function ; a function to convert output data which is a seq returned from body
    :out-struct {:a _ :b {:c _}} ; a structure template
    :permission [\"p1\" \"p2\"] ; all \"p1\", \"p2\" are required
    :permission #{\"p1\", \"p2\"} ; any one of \"p1\", \"p2\" is required
    :permission-check {
                        :check-fn user-context/has-permissoins?  ; A unary function to check if the permission specified by :permission key is satisfied.
                                                                 ; It should return logical true if the check passed, otherwise return logical false.
                                                                 ; The argument passed to this function is the permission specified by :permission key.
                        :fail-fn exception/forbidden             ; A unary function which is called when the permission check function returns logical false.
                                                                 ; The argument passed to this function is the permission specified by :permission key.
                      }
  [args]
  body
  )

  See also transform.
  "
  [svc-name doc-string & svc-data]
  (let [{:keys [svc-cfg svc-body]} (group-svc-data svc-data)
        svc-meta (make-svc-meta svc-cfg)
        ]
  `(t-svc [encapsule-body transform check-and-bind with-permission gen-defn] ~svc-name ~doc-string ~svc-meta ~@svc-body))
  ; `(s-svc [encapsule-body transform check-and-bind gen-defn] ~svc-name ~doc-string ~@args)
  )
