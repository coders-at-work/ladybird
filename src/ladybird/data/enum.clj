(ns ladybird.data.enum
    (:use ladybird.data.converter-core
          [ladybird.data.order :only (make-order)]
          [ladybird.util.symbol :only (str-symbol)]
          [ladybird.util.string :only (qualify-name)])
    (:require [ladybird.data.validate-core :as v]
              [ladybird.data.build-in-validator :as b]))

(defmacro defenum [name k1 v1 & kvs]
  (let [kvs (apply vector k1 v1 kvs)
        validator (str-symbol "enum:" name)
        i18n-msg-key (keyword (qualify-name name))
        ]
    `(do
       (def ~name (value-converter ~kvs))
       (def ~validator (b/enum-of ~name ~i18n-msg-key)))))

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
