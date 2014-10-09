(ns ladybird.data.order)

(defn- gen-order-map [es]
       (->> (mapcat (fn [e idx]
                        (if (vector? e)
                          (for [x e y [idx]] [x y])
                          [[e idx]]))
                    es (range))
            (into {})))

(defn make-order
  "construct an order definition of a groups of elements
   An order definition is a map containing following keys:
       ::es -- the same as the param
       ::max -- a set of the max values
       ::min -- a set of the min values
       ::compare-fn -- a function like clojure.core/compare to compare two elements
   Ex.
       (make-order [:a :b :c])  <-->  :a < :b < :c
       (make-order [:a [:b :c] :d])  <-->  :a < :b = :c < :d"
  [es]
  (let [order-map (gen-order-map es)
        max-vals (-> [(last es)] flatten set)
        min-vals (-> [(first es)] flatten set)
        compare-fn #(compare (order-map %1) (order-map %2))]
    {::es es ::max max-vals ::min min-vals ::compare-fn compare-fn}))

(defn compare-order [order-def v1 v2]
  ((::compare-fn order-def) v1 v2))

(defn order>? [order-def v1 v2]
  (pos? (compare-order order-def v1 v2)))

(defn order<? [order-def v1 v2]
  (neg? (compare-order order-def v1 v2)))

(defn order>=? [order-def v1 v2]
  (not (neg? (compare-order order-def v1 v2))))

(defn order<=? [order-def v1 v2]
  (not (pos? (compare-order order-def v1 v2))))

(defn order=? [order-def v1 v2]
  (zero? (compare-order order-def v1 v2)))

(defn order-max? [order-def v]
  (-> ((::max order-def) v) nil? not))

(defn order-min? [order-def v]
  (-> ((::min order-def) v) nil? not))
