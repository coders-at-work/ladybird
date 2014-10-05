(ns ladybird.svc.core
    (:use [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.data.validate-core :as v]
              [clojure.tools.logging :as log])
    )

(defn encapsule-body [{:keys [body] :as meta}]
  (let [body-form `(do ~@body)]
    (-> meta (dissoc :body) (assoc :body-form body-form))))

(defn- construct-check-and-bind-body [specs-m body-form]
       (let [{:keys [validate vars-m] :as m} (reduce (fn [ret [var-sym var-spec]]
                                                         (-> (update-in ret [:validate] assoc `'~var-sym var-spec)
                                                             (update-in [:vars-m] assoc `'~var-sym var-sym)
                                                             )
                                                         )
                                                     {} specs-m)
             ]
         `(do
            (-> ((v/m-validator ~validate) ~vars-m) v/check-validate-result)
            ~body-form)
         ;m
         )
       )

(defn check-and-bind [{:keys [options body-form] :as meta}]
  (let [{:keys [check-and-bind]} options]
    (if check-and-bind
      (assoc meta :body-form (construct-check-and-bind-body check-and-bind body-form))
      meta)))

(defn catch-forms [meta]
  (let [catch-forms `((catch Exception ~'e (log/error (get-stack-trace-str ~'e)) (ex-info "Oops!" {:ex-type :other-exception})))]
    (assoc meta :catch-forms catch-forms)))

(defn gen-defn-with-catch-point [{:keys [svc doc-string prototype body-form catch-forms] :as meta}]
  (let [args prototype
        ]
    `(defn ~svc ~doc-string ~args
       (try ~body-form ~@catch-forms))))

(def ^:dynamic *generate-fns* [encapsule-body catch-forms gen-defn-with-catch-point] )

#_(defmacro defsvc
  ""
  [svc-name doc-string prototype options & body]
  (let [generate-fn (->> (reverse *generate-fns*) (apply comp))
        meta {:svc svc-name :doc-string doc-string :prototype prototype :options options :body body}
        ]
    `~(generate-fn meta)))

(defmacro defsvc
  "You can create your own version of defsvc by bingding *generate-fns*."
  [svc-name doc-string prototype options & body]
  `(let [~'generate-fn (->> (reverse *generate-fns*) (apply comp))
         ~'meta {:svc '~svc-name :doc-string '~doc-string :prototype '~prototype :options '~options :body '~body}
         ]
     (eval (~'generate-fn ~'meta))))
