(ns ladybird.util.string
    (:require [clojure.string :as string])
    (:import [java.util Date]
             [java.text SimpleDateFormat]))

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

;; convert between Date and String
(def datetime-format-str "yyyy-MM-dd HH:mm:ss")
(def ^{:private true} datetime-format (SimpleDateFormat. datetime-format-str))
(def date-format-str "yyyy-MM-dd")
(def ^{:private true} date-format (SimpleDateFormat. date-format-str))
(def time-format-str "HH:mm:ss")
(def ^{:private true} time-format (SimpleDateFormat. time-format-str))

(defn date-to-datetime-str [^Date d]
  (.format datetime-format d))

(defn datetime-str-to-date [str]
  (.parse datetime-format str))

