(ns ladybird.ext.svc
    (:require [ladybird.svc.core :refer (encapsule-body check-and-bind gen-defn t-svc) :as ladybird-svc]
              [ladybird.util.structure :refer (structure-as RENAME REMOVE ADD)]
              [clojure.walk :refer (postwalk)]
              )
    )

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
                                              See also ladybird.util.structure/structure-as.
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
