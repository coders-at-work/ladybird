(ns ladybird.svc.core
    (:use [ladybird.util.core :only (get-stack-trace-str)])
    (:require [clojure.tools.logging :as log])
    )

(defn encapsule-body [{:keys [body] :as meta}]
  (let [body-form `(do ~@body)]
    (-> meta (dissoc :body) (assoc :body-form body-form))))

(defn catch-forms [meta]
  (let [catch-forms `((catch Exception ~'e (log/error (get-stack-trace-str ~'e)) (ex-info "Oops!" {:ex-type :other-exception})))]
    (assoc meta :catch-forms catch-forms)))

(defn gen-defn-with-catch-point [{:keys [svc doc-string prototype body-form catch-forms] :as meta}]
  (let [args prototype
        ]
    `(defn ~svc ~doc-string ~args
       (try ~body-form ~@catch-forms))))

(def ^:dynamic *generate-fns* [encapsule-body catch-forms gen-defn-with-catch-point] )

(defmacro defsvc
  ""
  [svc-name doc-string prototype options & body]
  (let [generate-fn (->> (reverse *generate-fns*) (apply comp))
        meta {:svc svc-name :doc-string doc-string :prototype prototype :options options :body body}
        ]
    `~(generate-fn meta)))
