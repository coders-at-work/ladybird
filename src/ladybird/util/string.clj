(ns ladybird.util.string
    (:require [clojure.string :as string])
    )

(defn hyphen-to-underscore [s]
  (string/replace s "-" "_"))

(defn underscore-to-hyphen [s]
  (string/replace s "_" "-"))
