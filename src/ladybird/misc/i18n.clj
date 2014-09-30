(ns ladybird.misc.i18n
  (:use [ladybird.util.core :only (def-bindable)]))

(def ^:private i18n-resource (atom nil))
(defn load-resource [resource-map]
  (reset! i18n-resource resource-map))

(def-bindable locale "zh-MO")

(defn get-resource
  ([i18n-key]
   (get-resource i18n-key (locale)))
  ([i18n-key locale]
   (or (get-in @i18n-resource [locale i18n-key])
       (get-in @i18n-resource ["en-US" i18n-key])
       i18n-key)))

