(ns ladybird.misc.exception
    (:require [ladybird.misc.i18n :as i18n]
              [clojure.string :as str])
    )

;; build in exceptions
(defn ex-msg [ex]
  (let [{:keys [ex-type ex-key msg-args]} (ex-data ex) 
        msg (.getMessage ex)]
    (if (not (str/blank? msg))
      (str "**-" ex-type "-** " msg)
      (let [res (i18n/get-resource ex-key)]
        (if (= ex-key res)
          (str "**-" ex-type "/" res "-** " (str/join ", " msg-args))
          (apply format res msg-args))))))

(defn create-ex
  ([ex-type msg]
   (ex-info msg {:ex-type ex-type}))
  ([ex-type ex-key msg-args]
   (create-ex ex-type ex-key msg-args nil))
  ([ex-type ex-key msg-args m]
   (ex-info "" (merge {:ex-type ex-type :ex-key ex-key :msg-args msg-args} m))))

(defn sys-error
  ([msg]
   (create-ex :sys-error msg))
  ([ex-key arg-1 & args]
   (create-ex :sys-error ex-key (cons arg-1 args))))

(defn no-priv 
  ([]
   (no-priv nil))
  ([m & privs]
   (create-ex :no-priv :no-priv privs m)))

(defn no-data
  ([]
   (no-data ""))
  ([msg]
   (create-ex :no-data :no-data [msg])))

(defn unauthorized
  ([]
   (unauthorized nil))
  ([m]
   (create-ex :unauthorized :unauthorized [""] m)))

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
