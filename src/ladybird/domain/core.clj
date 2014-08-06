(ns ladybird.domain.core
    (:require [ladybird.util.string :as str])
    )

;; domain meta preparing functions
(defn prepare-table-name [{:keys [domain-name table-name] :as domain-meta}]
  (assoc domain-meta
         :table-name
         (or table-name
             (-> (str/camel-case-to-clj-case domain-name) str/hyphen-to-underscore))))


;; define domain
(def default-prepare-fns [prepare-table-name])

(def ^:dynamic *prepare-fns* default-prepare-fns)

(defmacro defdomain
  ([domain-name fields]
   `(defdomain ~domain-name ~fields {}))
  ([domain-name fields meta-data]
   (let [domain-meta (merge {:domain-name (name domain-name) :fields fields} meta-data)
         prepare-fn (->> (reverse *prepare-fns*) (apply comp))
         domain-meta (prepare-fn domain-meta)
         ]
     `(do
        (def ~domain-name ~domain-meta)))
   )
  )
