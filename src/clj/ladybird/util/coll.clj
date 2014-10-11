(ns ladybird.util.coll)

(defn consv
  "
  Returns a new vector where x is the first element and seq is the rest.
  "
  [x seq]
  (vec (cons x seq)))


(defn concatv
  "
  Returns a vector representing the concatenation of the elements in the supplied colls.
  "
  [& args]
  (vec (apply concat args)))

(defn mapcatv
  "
  Like mapcat, but return a vector instead.
  "
  [& args]
  (vec (apply mapcat args)))
