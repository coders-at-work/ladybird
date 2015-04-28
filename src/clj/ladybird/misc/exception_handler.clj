(ns ladybird.misc.exception-handler
    (:use ladybird.misc.exception
          [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.misc.monitor :as mon]
              [clojure.tools.logging :as log]
              [clojure.string :as str])
    )

;; exception handler
(defn adaptive-ex-handler
  ([ex]
   (adaptive-ex-handler ex #(log/error (str/join "\n" [(type %) (get-stack-trace-str %)]))))
  ([ex others-fn]
   (cond
     (no-data? ex) nil 
     (or (no-priv? ex)
         (unauthorized? ex)) (log/warn (ex-msg ex))
     (sys-error? ex) (log/error (ex-msg ex))
     :others (others-fn ex))))

(defn default-ex-handler [ex]
  (adaptive-ex-handler ex))

(defn mon-stack-error-str [e]
  (str/join "\n" [(type e)
                  (mon/call-stack-str)
                  (get-stack-trace-str e)]))

(defn default-mon-ex-handler [ex]
  (adaptive-ex-handler ex
                       #(do 
                          (mon/set-error-occured!)
                          (log/error (mon-stack-error-str %))
                          (throw %))))

(def ^:private ex-handler-container (atom default-ex-handler))

(defn unified-ex-handler []
  @ex-handler-container)

(defn set-unified-ex-handler! [f]
  (reset! ex-handler-container f))
