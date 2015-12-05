(ns ladybird.svc.core
    (:require [ladybird.data.validate-core :as v]
              [ladybird.misc.exception-handler :as exh]
              [ladybird.core :refer (chain-gen)]
              )
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

(defn transform [{:keys [options body-form] :as meta}]
  (let [{:keys [to to-list-all to-vector-all]} options
        body-form (cond
                    to `(~to ~body-form)
                    to-list-all `(map ~to-list-all ~body-form)
                    to-vector-all `(mapv ~to-vector-all ~body-form)
                    :default body-form)
        ]
    (assoc meta :body-form body-form)))

(defn catch-forms [{:keys [body-form] :as meta}]
  (let [catch-forms `((catch Exception ~'e ((exh/unified-ex-handler) ~'e)))
        body-form `(try ~body-form ~@catch-forms)
        ]
    (assoc meta :body-form body-form)))

(defn gen-defn [{:keys [svc doc-string prototype body-form] :as meta}]
  (let [args prototype
        ]
    `(defn ~svc ~doc-string ~args ~body-form)))

;; service generation functions
(def ^:dynamic *generate-fns* [encapsule-body transform check-and-bind catch-forms gen-defn] )
(def ^:const default-gen-fns [encapsule-body transform check-and-bind catch-forms gen-defn] )

;; service macro
(defmacro svc
  "
  Generate service code. Parameters svc-name, doc-string, prototype, options and body will be put into a map at first. Then the map will be processed through chaining functions in gen-fns to generate service code.
  Params:
     gen-fns      --   functions to generate service code, the left most function will be called first, and it will be passed a argument of map. All arguments except for gen-fns will be put into the map with keywordized keys. See also ladybird.core/chain-gen.
     svc-name     --   a symbol, the service name
     doc-string   --   a doc string
     prototype    --   biding form, like similar parts in defn, fn, loop, for, etc., the actual data structure is determined by the implementation of the service
     options      --   normally is a map, can be any initial meta data
     body         --   code body

  Ex:
     (svc [encapsule-body transform check-and-bind catch-forms gen-defn] a \"a\" [a b c d] {} (str [a b c d]))
  "
  [gen-fns svc-name doc-string prototype options & body]
  (let [meta `{:svc '~svc-name :doc-string ~doc-string :prototype '~prototype :options '~options :body '~body}
         ]
     `(chain-gen ~meta ~@gen-fns)))

(defmacro s-svc
  "
  Generate service code. It is the same as svc, except that you can ignore the options if there is no content in it. Can be used to define your own version of service generating. See also ladybird.svc.core/defsvc.
  Params:
     See also ladybird.svc.core/svc

  Examples:
      (s-svc [encapsule-body transform check-and-bind catch-forms gen-defn] a \"a\" [a b c d] (str [a b c d]))
  "
  [gen-fns svc-name doc-string prototype & options-body]
  (let [[form] options-body
        [options body] (if (and (map? form) (> (count options-body) 1)) [form (rest options-body)] [nil options-body])
        ]
    `(svc ~gen-fns ~svc-name ~doc-string ~prototype ~options ~@body)))

(defmacro defsvc
  "
  Defines a default service. It is the same as SVC, except that you can ignore the options if there is no content in it.

  Examples:
      (defsvc a \"a\" [a b c d] (str [a b c d]))
  "
  [svc-name doc-string prototype & options-body]
  `(s-svc ~default-gen-fns ~svc-name ~doc-string ~prototype ~@options-body))


;; TODO: remove all the following code
(defmacro SVC
  "
  Creates a service. The default implementation will generate a function with prototype and body, and catching and logging all exceptions thrown by body as error. And you can specify local validation and conversion, returned value convertion in options.
  You can create your own version of defsvc by bingding *generate-fns*. See binding-svc.

  Examples:
      (SVC a \"a\" [a b c d] {:check-and-bind [a [not-nil is-boolean]
                                               b not-nil
                                               c [:to inc]
                                               d [not-nil :to dec]]
                              :to str}
        [a b c d])
      (a false 2 3 4) -> \"[false 2 4 3]\"
  "
  [svc-name doc-string prototype options & body]
  `(let [~'generate-fn (->> (reverse *generate-fns*) (apply comp))
         ~'meta {:svc '~svc-name :doc-string '~doc-string :prototype '~prototype :options '~options :body '~body}
         ]
     (eval (~'generate-fn ~'meta))))

#_(defmacro defsvc
  "
  Defines a service. It is the same as SVC, except that you can ignore the options if there is no content in it.

  Examples:
      (defsvc a \"a\" [a b c d] (str [a b c d]))
  "
  [svc-name doc-string prototype & body]
  (let [[form] body
        [options body] (if (and (map? form) (> (count body) 1)) [form (rest body)] [{} body])
        ]
    `(SVC ~svc-name ~doc-string ~prototype ~options ~@body)))

(defmacro binding-svc
  "
  First binding *generate-fns* with binding-val, then call defsvc with args.
  "
  [binding-val & args]
  `(binding [*generate-fns* ~binding-val]
            (defsvc ~@args)))
