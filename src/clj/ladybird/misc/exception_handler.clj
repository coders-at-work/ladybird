(ns ladybird.misc.exception-handler
    (:use ladybird.misc.exception
          [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.misc.monitor :as mon]
              [clojure.tools.logging :as log]
              [clojure.string :as str])
    )

;; exception handler
(defn adaptive-ex-handler [ex others-fn]
  (letfn [(thr [] (throw (RuntimeException. (ex-msg ex) ex)))]
         (cond
           (no-data? ex) (thr)
           (or (no-priv? ex)
               (unauthorized? ex)) (do (log/warn (ex-msg ex))
                                       (thr))
           (sys-error? ex) (do (log/error (ex-msg ex))
                               (thr))
           :others (others-fn ex))))

(defn default-ex-handler [ex]
  (adaptive-ex-handler ex
                       #(do
                          (log/error (str/join "\n" [(type %) (get-stack-trace-str %)]))
                          (throw %)))
  #_(letfn [(thr [] (throw (RuntimeException. (ex-msg ex) ex)))]
         (cond
           (no-data? ex) (thr)
           (or (no-priv? ex)
               (unauthorized? ex)) (do (log/warn (ex-msg ex))
                                       (thr))
           (sys-error? ex) (do (log/error (ex-msg ex))
                               (thr))
           :others (do (log/error (str/join "\n" [(type ex) (get-stack-trace-str ex)]))
                       (throw ex)))))

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
