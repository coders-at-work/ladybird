(ns ladybird.util.keyword
    (:require [ladybird.util.string :as str]))

(defn clj-case-keyword [x]
  (keyword (str/clj-case x)))
