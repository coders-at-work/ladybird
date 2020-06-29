(ns ladybird.ext.schema
    (:require [schema.core :as s]
              [ladybird.data.enum :refer (spec-keys)]
              [clojure.string :as str]
              )
    )

(def not-blank (s/pred (with-meta
                         (every-pred string? (complement str/blank?)) {:type "string" :example " a not blank string "})
                       "must be a non-blank string"))

(defn enum-to-schema [enum]
  (->> (spec-keys enum) (apply s/enum)))
