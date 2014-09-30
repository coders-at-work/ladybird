(ns ladybird.data.validate-core
    (:require [ladybird.misc.i18n :as i]
              [ladybird.util.string :as str])
    )

(defn validator
  "
   Create a simple validator. A simple validator is a function accepting one argument. It tests the argument with a predicate. If the argument passes the test, it returns true, otherwise it returns a vector containing an i18n key and a stirng of error message.
  "
  [pred i18n-msg-key err-msg]
    #(if (pred %) true [i18n-msg-key err-msg]))

(defmacro def-validator
  "
   Define a simple validator with qualified validator name as i18n message key.
  "
  [vfn pred err-msg]
  (let [i18n-msg-key (keyword (str/qualify-name vfn))]
    `(def ~vfn (validator ~pred ~i18n-msg-key ~err-msg))))

(defn validate
  "
   Validate a value by a sequence of validators.
  "
  [val & validators]
  (map #(% val) validators))

(defn validate-m
  "
   Validate a data map according to a validator map.
  "
  [vmap m]
  (->> (select-keys m (keys vmap))
       (map (fn [[k v]] (->> (vmap k) list flatten
                             (apply validate v)
                             (vector k))))
       (into {})))

(defn m-validator
  "
   Create a validator to validate a data map according to a validator map.
  "
  [vmap]
  (partial validate-m vmap))

(defn get-err-msg
  "
   Format a validation error message and i18n it.
  "
  [locale a-name [i18n-msg-key err-msg]]
  (let [i18n-msg (i/get-resource i18n-msg-key locale)
        i18n-msg (if (= i18n-msg-key i18n-msg) err-msg i18n-msg)
        ]
    (format i18n-msg a-name)))

(defn err-msgs
  "
   Format and i18n error messages of the validate result. Return nil when result is all true, otherwise return a seq of messages.
  "
  ([validate-result]
   (err-msgs {} validate-result))
  ([kmap validate-result]
   (err-msgs (i/locale) kmap validate-result))
  ([locale kmap validate-result]
   (let [msgs (map (fn [[k rs]]
                       (let [kname (kmap k k)]
                         (->> (filter (complement true?) rs) (map (partial get-err-msg locale kname)))))
                   validate-result)
         msgs (apply concat msgs)
        ]
    (if (empty? msgs) nil msgs))))
