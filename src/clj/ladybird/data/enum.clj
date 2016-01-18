(ns ladybird.data.enum
    (:use ladybird.data.converter-core
          [ladybird.data.order :only (make-order)]
          [ladybird.util.symbol :only (str-symbol)]
          [ladybird.util.string :only (qualify-name)]
          [ladybird.util.coll :only (mapcatv)])
    (:require [ladybird.data.validate-core :as v]
              [ladybird.data.build-in-validator :as b]))

(defn enum-body [name kvs]
  (let [validator (str-symbol "enum:" name)
        i18n-msg-key (keyword (qualify-name validator))
        ks (take-nth 2 kvs)
        vs (take-nth 2 (drop 1 kvs))
        k-strs (map clojure.core/name ks)
        c {:in (apply hash-map (reverse kvs)) :out (zipmap (concat ks k-strs) (concat vs vs))}
        ]
    `(do
       (def ~name (assoc ~c :type ::enum))
       (def ~validator (b/enum-of ~name ~i18n-msg-key)))))

(defmacro defenum [name k1 v1 & kvs]
  (let [kvs (apply vector k1 v1 kvs)]
    (enum-body name kvs)))

(defn- ordered-kvs-to-order-es [ordered-kvs]
       (let [parts (partition-by vector? ordered-kvs)]
         (-> (mapcat (fn [[e :as col]]
                         (if (vector? e)
                           col
                           (->> col (partition 2) (map vec))))
                     parts)
             vec)))

(defmacro def-ordered-enum [name & ordered-kvs]
  (let [kvs (flatten ordered-kvs)
        ]
    `(do
       (defenum ~name ~@kvs)
       (def ~name (->> ~(ordered-kvs-to-order-es ordered-kvs) make-order (merge ~name))))))

(defmacro defenum-for-keys 
  ([name keys]
   `(defenum-for-keys ~name name ~keys))
  ([name str-fn keys]
   `(let [kvs# (mapcatv list ~keys (map ~str-fn ~keys))]
      (eval (enum-body '~name kvs#)))))

;; enum utilities
(defn enum? [x]
  (= ::enum (:type x)))

(defn enum-keys [enum]
  (-> enum out-fn keys))

(defn enum-vals [enum]
  (-> enum in-fn keys))
