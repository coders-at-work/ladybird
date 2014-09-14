(ns ladybird.util.keyword
    (:require [ladybird.util.string :as str]))

(defn clj-case-keyword [x]
  (keyword (str/clj-case x)))

(defn str-keyword [x & ys]
  (keyword (apply str (map name (cons x ys)))))
