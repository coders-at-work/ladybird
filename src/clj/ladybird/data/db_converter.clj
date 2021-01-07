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

(defn- read-from-edn [s]
  (letfn [(is-java-time-instant? [t v]
            ; (prn-str java.time.Instant) will generate a string like "#object[java.time.Instant 0x70d987ac \"2021-01-07T08:55:04.352Z\"]\n"
            (and (= t 'object)
                 (vector? v)
                 (= (first v) 'java.time.Instant)
                 )
            )
          ]
    (edn/read-string {:default (fn [t v]
                                 (if (is-java-time-instant? t v)
                                   ; parse to java.time.Instant
                                   (java.time.Instant/parse (nth v 2))
                                   (throw (RuntimeException. (format "No reader function for tag %s %s" t v)))
                                   )
                                 )
                      } s)
    )
  )

(def EDN
  {:in read-from-edn
   :out prn-str
   }
  )

(def MSSQL-ROWVERSION
     {:in #(-> % ByteBuffer/wrap .getLong)
     :out identity})

;; encryption
