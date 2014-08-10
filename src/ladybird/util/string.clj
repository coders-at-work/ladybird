(ns ladybird.util.string
    (:require [clojure.string :as string])
    )

(defn hyphen-to-underscore [s]
  (string/replace s "-" "_"))

(defn underscore-to-hyphen [s]
  (string/replace s "_" "-"))

(defn clj-case [x]
  (-> (name x)
    (string/replace #"[A-Z]" #(str "-" (string/lower-case %)))
    (string/replace #"^([^a-zA-Z]*)-([a-z])" #(str (second %) (last %)))
    underscore-to-hyphen))

(defn clj-case-to-db-case [s]
  (-> s string/lower-case hyphen-to-underscore))

(defn db-case-to-clj-case [s]
  (-> s string/lower-case underscore-to-hyphen))

