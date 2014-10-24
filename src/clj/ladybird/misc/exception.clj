(ns ladybird.misc.exception
    (:use [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.misc.i18n :as i18n]
              [clojure.tools.logging :as log]
              [clojure.string :as str])
    )

;; exception
(defn ex-msg [ex]
  (let [msg (.getMessage ex)]
    (if (not (str/blank? msg))
      msg
      (let [{:keys [ex-key msg-args]} (ex-data ex)
            res (i18n/get-resource ex-key)]
        (if (= ex-key res)
          (str "**-" res "-** " (str/join ", " msg-args))
          (apply format res msg-args))))))

(defn create-ex [ex-type ex-key msg-args]
  (ex-info "" {:ex-type ex-type :ex-key ex-key :msg-args msg-args}))

(defn sys-error [ex-key & args]
  (create-ex :sys-error ex-key args))

(defn no-priv 
  ([]
   (no-priv ""))
  ([priv]
   (create-ex :no-priv :no-priv [priv])))

(defn no-data
  ([]
   (no-data ""))
  ([msg]
   (create-ex :no-data :no-data [msg])))

(defn unauthorized []
  (create-ex :unauthorized :unauthorized [""]))

(defn is-ex-type? [ex ex-type]
  (and (instance? clojure.lang.ExceptionInfo ex)
       (= ex-type (:ex-type (ex-data ex)))))

(defn sys-error? [ex]
  (is-ex-type? ex :sys-error))

(defn no-priv? [ex]
  (is-ex-type? ex :no-priv))

(defn no-data? [ex]
  (is-ex-type? ex :no-data))

(defn unauthorized? [ex]
  (is-ex-type? ex :unauthorized))


;; exception handler
(defn default-ex-handler [ex]
  (cond
    (or (sys-error? ex)
        (no-priv? ex)
        (no-data? ex)
        (unauthorized? ex)) (throw (RuntimeException. (ex-msg ex) ex))
    :others (do
              (log/error (str/join "\n" [(type ex) (get-stack-trace-str ex)]))
              (throw ex))))

(def ^:private ex-handler-container (atom default-ex-handler))

(defn unified-ex-handler []
  @ex-handler-container)

(defn set-unified-ex-handler! [f]
  (reset! ex-handler-container f))
