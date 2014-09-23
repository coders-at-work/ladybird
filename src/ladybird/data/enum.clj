(ns ladybird.data.enum
    (:use ladybird.data.converter-core
          [ladybird.data.order :only (make-order)]
          ))

(defmacro defenum [name k1 v1 & kvs]
  (let [kvs (apply vector k1 v1 kvs)]
    `(def ~name (value-converter ~kvs))))

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
