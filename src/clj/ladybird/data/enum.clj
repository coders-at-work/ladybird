(ns ladybird.data.enum
    (:use ladybird.data.converter-core
          [ladybird.data.order :only (make-order)]
          [ladybird.util.symbol :only (str-symbol)]
          [ladybird.util.string :only (qualify-name)]
          [ladybird.util.coll :only (mapcatv)])
    (:require [ladybird.data.validate-core :as v]
              [ladybird.data.build-in-validator :as b]
              [clojure.walk :refer (postwalk)]))

(defn- quote-symbol [x]
       (if (symbol? x) `'~x x))

(defn- quote-all-symbols [kvs]
       (->> kvs
            (map quote-symbol)
            vec))

(defn- enum-converter [is-strict kvs]
       (let [stored-kvs (quote-all-symbols kvs)
             in-vks (reverse stored-kvs)
             out-kvs (partition-all 2 kvs)
             out-kvs (mapcat
                       (fn [[k v]]
                           (let [v (quote-symbol v)]
                             (if (and
                                   (instance? clojure.lang.Named k)
                                   (not (string? k)))
                               (if is-strict
                                 [(quote-symbol k) v]
                                 [(quote-symbol k) v (clojure.core/name k) v])
                               [k v])))
                       out-kvs)
             ]
         {:in (apply hash-map in-vks) :out (apply hash-map out-kvs) ::spec-kvs (vec stored-kvs)}))

(defn enum-body [name kvs]
  (let [is-strict (-> (meta name) :strict)
        validator (str-symbol "enum:" name)
        i18n-msg-key (keyword (qualify-name validator))
        ]
    `(do
       (def ~name (assoc ~(enum-converter is-strict kvs) :type ::enum))
       (def ~validator (b/enum-of ~name ~i18n-msg-key)))))

(defmacro defenum
  "
   Defines a converter and a validator for an enum.

   e.g.
       [ladybird.data.converter-core :refer (in-fn out-fn)]
       (defenum E :a 1 :b 2 c 3 4 5)
       ((in-fn E) 1) => :a
       ((in-fn E) 3) => 'c
       ((in-fn E) 5) => 4
       ((out-fn E) :b) => 2
       ((out-fn E) 'c) => 3
       ((out-fn E) 4) => 5)
       (enum:E :a) => true

   In normal, if a key is an instance of clojure.lang.Named, the out-fn of converter will also map (name key) to key's value.
   e.g.
       (defenum E \"a\" 1 :b 2 c 3)
       ((out-fn E) \"a\") => 1
       ((out-fn E) \"b\") => 2
       ((out-fn E) \"c\") => 3)
       (enum:E \"a\") => true

   You can forbid this behavior with {:strict true} meta.
   e.g.
       (defenum ^:strict E \"a\" 1 :b 2 c 3)
       ((out-fn E) \"a\") => 1
       ((out-fn E) \"b\") => nil
       ((out-fn E) \"c\") => nil
  "
  [name k1 v1 & kvs]
  (let [kvs (apply vector k1 v1 kvs)]
    (enum-body name kvs)))

(defn- quote-ks-in-order-partition [kvs]
       (->>
         kvs
         (partition-all 2)
         (map (fn [[k v]]
                  (let [quoted-k (quote-symbol k)
                        v (quote-symbol v)]
                    (if (instance? clojure.lang.Named k) [quoted-k (name k) v] [quoted-k v]))))))

(defn ordered-kvs-to-order-es [ordered-kvs]
  (let [parts (partition-by vector? ordered-kvs)]
    (-> (mapcat (fn [[e :as col]]
                    (if (vector? e)
                      (map #(->> % quote-ks-in-order-partition (apply concat) vec) col)
                      (quote-ks-in-order-partition col)))
                parts)
        vec)))

(defmacro def-ordered-enum
  "
   Defines an enum with ordering.
  "
  [name & ordered-kvs]
  (let [kvs (flatten ordered-kvs)
        ]
    `(do
       (defenum ~name ~@kvs)
       (def ~name (->> ~(ordered-kvs-to-order-es ordered-kvs) make-order (merge ~name))))))

(defmacro defenum-for-names
  "
   Defines enum for a seq of keys in which each key is an instance of clojure.lang.Named and maps a key to (name key).

   e.g.
       [ladybird.data.converter-core :refer (in-fn out-fn)]
       (defenum-for-names E :a \"b\" c)
       ((in-fn E) \"a\") => :a
       ((in-fn E) \"b\") => \"b\"
       ((in-fn E) \"c\") => 'c
       ((out-fn E) :a) => \"a\"
       ((out-fn E) \"b\") => \"b\"
       ((out-fn E) 'c) => \"c\"


   Normally, if a key is an instance of clojure.lang.Named, the out-fn of converter will also map (name key) to key's value.
   e.g.
       (defenum-for-names E :a \"b\" c)
       ((out-fn E) \"a\") => \"a\"
       ((out-fn E) \"c\") => \"c\")

   Of course, {:strict true} meta still takes effect.
   e.g.
       (defenum-for-names ^:strict E :a \"b\" c)
       ((out-fn E) :a) => \"a\"
       ((out-fn E) \"b\") => \"b\"
       ((out-fn E) 'c) => \"c\"
       ((out-fn E) \"a\") => nil
       ((out-fn E) \"c\") => nil
  "
  [ename k & ks]
  (let [ks (cons k ks)
        kvs (mapcat list ks (map name ks))]
    `(defenum ~ename ~@kvs)))

;; enum utilities
(defn enum? [x]
  (= ::enum (:type x)))

(defn enum-keys [enum]
  (-> enum out-fn keys))

(defn enum-vals [enum]
  (-> enum in-fn keys))

(defn kv-pairs
  "
   Get a seq of [key value] pairs used by the enum converter.
  "
  [enum]
  (map identity (out-fn enum)))

(defn spec-kvs [enum]
  (::spec-kvs enum))

(defn spec-keys
  "
   Get the keys in the original enum spec.
  "
  [enum]
  (->> enum ::spec-kvs (take-nth 2)))

(defn spec-vals
  "
   Get the values in the original enum spec.
  "
  [enum]
  (->> enum ::spec-kvs (drop 1) (take-nth 2)))

(defn value-of
  "
   Convert the key to its value.
  "
  [enum k]
  (tr-out enum k))

(defn key-of
  "
   Convert the value to its key.
  "
  [enum v]
  (tr-in enum v))
