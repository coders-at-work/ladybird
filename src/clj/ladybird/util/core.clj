(ns ladybird.util.core
    (:require [ladybird.util.string :as str]
              [ladybird.util.symbol :as sym])
    )

(defmacro def-bindable
  "Define a bindable var and other tools. Include a function for getting the var's value, a macro to dynamically binding the var and call the body.
   
   Example:
      (def-bindable aa 3)
      *aa*      ;;=> 3
      (aa)      ;;=> 3
      (with-aa 4 (inc (aa)))    ;;=> 5"
  [name init-form]
  (let [var-name (sym/str-symbol "*" name "*")
        var-sym (with-meta var-name {:dynamic true :private true})
        qualified-var-sym (symbol (str/qualify-name var-name))
        ]
    `(do
       (def ~var-sym ~init-form)
       (defn ~name [] ~var-name)
       (defmacro ~(sym/str-symbol "with-" name) [~'name & ~'body]
         `(binding [~'~qualified-var-sym ~~'name]
                   ~@~'body)))))

(defn get-stack-trace-str [e]
  (apply str (.getMessage e) "\n" (map #(str % "\n") (.getStackTrace e))))

(defn get-system-property 
  "Get a system property."
  ([s]
   (System/getProperty s))
  ([s default]
   (System/getProperty s default)))
