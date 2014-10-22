(ns ladybird.svc.generate
    (:use [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.misc.monitor :as mon]
              [clojure.tools.logging :as log])
    )

(defn with-stack [{:keys [svc prototype body-form] :as meta}]
  (let [svc-info `(apply list '~svc ~prototype)
        body-form `(mon/monitor-exec-state ~(ns-name *ns*) ~svc ~prototype ~body-form)
        ]
    (assoc meta :body-form body-form)))

(defn stack-error-str [e]
  (clojure.string/join "\n" [(type e)
                             (mon/call-stack-str)
                             (get-stack-trace-str e)]))

(defn catch-stack [{:keys [body-form] :as meta}]
  (let [catch-forms `((catch Exception e#
                             (mon/set-error-occured!)
                             (log/error (stack-error-str e#))
                             (ex-info "Oops!" {:ex-type :other-exception})))
        body-form `(try ~body-form ~@catch-forms)
        ]
    (assoc meta :body-form body-form)))
