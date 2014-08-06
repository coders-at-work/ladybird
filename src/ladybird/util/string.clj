(ns ladybird.util.string
    (:require [clojure.string :as string])
    )

(defn hyphen-to-underscore [s]
  (string/replace s "-" "_"))

(defn underscore-to-hyphen [s]
  (string/replace s "_" "-"))

(defn camel-case-to-clj-case [x]
  (-> (name x)
    (string/replace #"[A-Z]" #(str "-" (string/lower-case %)))
    (string/replace #"^([^a-zA-Z]*)-([a-z])" #(str (second %) (last %)))
    underscore-to-hyphen))
