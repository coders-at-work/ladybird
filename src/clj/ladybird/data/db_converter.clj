(ns ladybird.data.db-converter
    (:require [ladybird.data.converter-core :refer :all]
              [ladybird.util.string :refer (datetime-str-to-date)]
              [clojure.edn :as edn]
              )
    (:import (java.util Date Calendar)
             java.nio.ByteBuffer
             java.sql.Timestamp
             ))

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

(def JAVA-TIME-INSTANT
  {:in #(some-> % (.toInstant))
   :out #(some-> % (Timestamp/from))
   }
  )

(def EDN
  {:in edn/read-string
   :out pr-str
   }
  )

(def MSSQL-ROWVERSION
     {:in #(-> % ByteBuffer/wrap .getLong)
     :out identity})

;; encryption
