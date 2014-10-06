(ns ladybird.svc.core
    (:use [ladybird.util.core :only (get-stack-trace-str)])
    (:require [ladybird.data.validate-core :as v]
              [clojure.tools.logging :as log])
    )

(defn encapsule-body [{:keys [body] :as meta}]
  (let [body-form `(do ~@body)]
    (-> meta (dissoc :body) (assoc :body-form body-form))))

(defn- parse-check-and-bind-specs [specs]
       (reduce (fn [ret [var-sym var-spec]]
                   (let [contains-converter? (and (vector? var-spec) (= :to (-> var-spec drop-last last)))
                         validate-spec (if contains-converter? (-> (drop-last 2 var-spec) vec) var-spec)
                         converter (when contains-converter? (last var-spec))
                         ret (if (and (vector? validate-spec) (empty? validate-spec))
                               ret
                               (-> (update-in ret [:validate] assoc `'~var-sym validate-spec)
                                   (update-in [:vars-m] assoc `'~var-sym var-sym)))
                         ]
                     (if converter
                       (update-in ret [:converters] #(let [converters (if %1 %1 [])]
                                                       (conj converters %2 %3))
                                  var-sym `(~converter ~var-sym))
                       ret)))
               {} specs))

(defn- construct-check-and-bind-body [specs body-form]
       (let [{:keys [validate converters vars-m] :as m} (parse-check-and-bind-specs (partition 2 specs))
             body-form (if converters `(let ~converters ~body-form) body-form)
             ]
         (if validate
           `(do
              (-> ((v/m-validator ~validate) ~vars-m) v/check-validate-result)
              ~body-form)
           body-form)))

(defn check-and-bind
  "
  Specs should be like: [a [not-nil is-boolean] b not-nil c [:to inc] d [not-nil :to dec]]
  "
  [{:keys [options body-form] :as meta}]
  (let [{:keys [check-and-bind]} options]
    (if check-and-bind
      (assoc meta :body-form (construct-check-and-bind-body check-and-bind body-form))
      meta)))

(defn catch-forms [meta]
  (let [catch-forms `((catch Exception ~'e
                             (log/error (clojure.string/join "\n" [(type ~'e) (get-stack-trace-str ~'e)]))
                             (ex-info "Oops!" {:ex-type :other-exception})))]
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

(defmacro binding-svc
  "
  First binding *generate-fns* with binding-val, then call defsvc with args.
  "
  [binding-val & args]
  `(binding [*generate-fns* ~binding-val]
            (defsvc ~@args)))
