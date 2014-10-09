(ns ladybird.data.build-in-validator
  (:use [clojure.string :only (blank?)]
        ;[util.core-utils :only (defn_)]
        ;[util.str-utils :only (is-datetime-str? is-date-str?)]
        ladybird.data.validate-core)
  (:require [ladybird.data.converter-core :as c])
  )

(defn value-in
  "
  Create a value-in validator.
  "
  [i18n-msg-key v1 & vs]
  (let [vals (into [v1] vs)
        val-set (set vals)
        ]
        (validator #(contains? val-set %) i18n-msg-key (str "%s value should be in " vals))))

(defn nil-or-satisfied
  "
  Create a function which tests its argument, if the argument is nil, returns true, otherwise returns (pred-or-validator arg).
  "
  [pred-or-validator]
  #(if (nil? %) true (pred-or-validator %)))

(def-validator not-nil (complement nil?) "%s cannot be null")

(def-validator not-blank #(and (string? %) ((complement blank?) %)) "%s should be non-blank string")

(def-validator not-zero (nil-or-satisfied (complement zero?)) "%s cannot be zero")

(def-validator is-int (nil-or-satisfied integer?) "%s should be integer")

(def-validator is-int-str (nil-or-satisfied #(re-matches #"^[+-]?\d+$" %)) "%s should be integer string")

(def-validator is-non-negative-int-str (nil-or-satisfied #(re-matches #"^[+]?\d+$" %)) "%s should be non-negative integer string")

(def-validator is-number (nil-or-satisfied number?) "%s should be number")

(defn enum-of [e-converter i18n-msg-key]
  (->> (c/out-fn e-converter) keys (apply value-in i18n-msg-key) nil-or-satisfied))

(def is-boolean (nil-or-satisfied (value-in ::is-boolean true false)))

(def is-boolean-str (nil-or-satisfied (value-in ::is-boolean-str "true" "false")))

(def-validator is-email (nil-or-satisfied #(re-matches #"\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*" %)) "%s should be email address")

;(def is-datetime-str (nil-or-satisfied #(is-datetime-str? %) "{0} should be datetime format yyyy-MM-dd HH:mm:ss")) 

;(def is-date-str (nil-or-satisfied #(is-date-str? %) "{0} should be date format yyyy-MM-dd")) 
