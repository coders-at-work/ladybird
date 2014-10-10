(ns ladybird.util.symbol)

(defn str-symbol
  "Construct a symbol from the string which is the concatenation of all the args' name.
   
   Example:
      (str-symbol \"a\" :b 'cd)   ;;=> abcd"
  [x & ys]
  (->> (cons x ys) (map name) (apply str) symbol))

(defn meta-flag-symbol
  "
  Creates a symbole named by x with the given keywords being set to true in its metadata.

  Example:
      (meta-flag-symbol \"a\" :dynamic :private)
      (meta *1) -> {:dynamic true :private true}
  "
  [x kw & kws]
  (->> (repeat true) (zipmap (cons kw kws)) (with-meta (-> x name symbol))))
