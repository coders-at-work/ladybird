(ns ladybird.ext.schema
    (:require [schema.core :as s]
              [ladybird.data.enum :refer (spec-keys)]
              [clojure.string :as str]
              [ladybird.util.string :refer (date-str-to-date)]
              )
    )

(def not-blank (s/pred (with-meta
                         (every-pred string? (complement str/blank?)) {:type "string" :example " a not blank string "})
                       "must be a non-blank string"))

(defn enum-to-schema [enum]
  (->> (spec-keys enum) (apply s/enum)))

(def date-str (s/pred #(and (string? %)
                            (re-find #"^\d{4}-\d{2}-\d{2}$" %)
                            (date-str-to-date %)) "Date format should be \"yyyy-MM-dd\""))
