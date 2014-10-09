(ns ladybird.data.db-converter
    (:use ladybird.data.converter-core
          [ladybird.util.string :only (datetime-str-to-date)])
    (:import (java.util Date Calendar)))

(defn- truncate-time [^java.util.Date d]
       (let [c (Calendar/getInstance)]
         (doto c
               (.setTime d)
               (.set Calendar/HOUR_OF_DAY 0)
               (.set Calendar/MINUTE 0)
               (.set Calendar/SECOND 0))
         (.getTime c)))

(def BOOL (value-converter [true "T" false "F"]))

(def DATETIME (nullable-converter #(java.util.Date. (.getTime %)) #(-> (.getTime %) java.sql.Timestamp. (doto (.setNanos 0)))))

(def DATE (composite-converter {:in identity :out truncate-time} DATETIME))

(def DECIMAL (nullable-converter bigdec))

(def STR->DATETIME (composite-converter (when-converter string? datetime-str-to-date) DATETIME))

;; TODO should be STRING-OUT-INT-IN
(def INT->STRING (when-converter integer? str))

;; datetime, encryption, simplfied/traditional chinese
