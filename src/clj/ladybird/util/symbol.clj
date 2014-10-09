(ns ladybird.util.symbol)

(defn str-symbol
  "Construct a symbol from the string which is the concatenation of all the args' name.
   
   Example:
      (str-symbol \"a\" :b 'cd)   ;;=> abcd"
  [x & ys]
  (->> (cons x ys) (map name) (apply str) symbol))
