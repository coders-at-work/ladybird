(ns ladybird.svc.generate
    (:require [ladybird.misc.monitor :as mon]
              [ladybird.misc.exception-handler :as exh]
              [clojure.tools.logging :as log])
    )

(defn with-stack [{:keys [svc prototype body-form] :as meta}]
  (let [body-form `(mon/monitor-exec-state ~(ns-name *ns*) ~svc ~prototype ~body-form)
        ]
    (assoc meta :body-form body-form)))

(defn catch-stack [{:keys [body-form] :as meta}]
  (let [catch-forms `((catch Exception e# (exh/default-mon-ex-handler e#)))
        body-form `(try ~body-form ~@catch-forms)
        ]
    (assoc meta :body-form body-form)))
