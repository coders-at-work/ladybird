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
        var-sym (with-meta var-name {:dynamic true :private true})]
    `(do
       (def ~var-sym ~init-form)
       (defn ~name [] ~var-name)
       (defmacro ~(sym/str-symbol "with-" name) [~'name & ~'body]
         `(binding [~(sym/str-symbol '~(str *ns*) "/" '~var-name) ~~'name]
            ~@~'body)))))
