(ns ladybird.misc.conf
    (:require [ladybird.util.symbol :as sym]
              [ladybird.misc.exception :as e])
    )

;; config
(def ^:private conf (atom nil))

(defn get-config [] @conf)

(defn load-config [config-map & cfms]
  (reset! conf (apply merge config-map cfms)))

(defn get-config-value [config-key]
  (config-key @conf))

(defmacro def-confn
  "
  Defines a function named by the configuration key to get the converted value of it.
      (def-confn fname) <=> (def-confn fname nil? identity)
      (def-confn fname convert-fn) <=> (def-confn fname nil? convert-fn)
  Params:
      name -- the name of the function, and the configuration key will be (keyword name)
      null?-fn -- a function to test if the config value is a null value. If the value is recognized as null, and you call the generated function without args, it will throw exception. The same situation when you call the generated function with a default value, it will return that default value.
      convert-fn -- a function to convert the value of the configuration key
  "
  ([name]
   `(def-confn ~name identity))
  ([name convert-fn]
   `(def-confn ~name nil? ~convert-fn))
  ([name null?-fn convert-fn]
   (let [k (keyword name)]
     `(defn ~name 
        ([]
         (let [v# (get-config-value ~k)]
           (when (~null?-fn v#) (throw (e/sys-error :invalid-config-value (format "configuration of %s is incorrect" ~k) (format "the invalid value is %s" v#))))
           (~convert-fn v#)))
        ([~'default-value]
         (let [v# (get-config-value ~k)]
           (if (~null?-fn v#) ~'default-value (~convert-fn v#))))))))

;; switcher
(defmacro def-switcher
  "
  Defines a two status switcher and functions to manipulate it.
      (def-switcher sms) <=> (def-switcher sms true false true)
      (def-switcher sms false) <=> (def-switcher sms true false false)

  Params:
      name -- the name of the switcher, it is a private var whose root bingding is an atom
      enabled-val -- the value which is recognized as enabled status
      disabled-val --the value which is recognized as disabled status 
      enable-by-default? -- it is true if the initial status of the switcher should be enabled; otherwise, it is false

  Ex.
      (def-switcher sms :t :f false)

      (sms-enabled?) -> false
      (enable-sms true)
      (sms-enabled?) -> true
      (set-sms-val :f)
      (sms-enabled?) -> false
  "
  ([name]
   `(def-switcher ~name ~true))
  ([name enable-by-default?]
   `(def-switcher ~name ~true ~false ~enable-by-default?))
  ([name enabled-val disabled-val enable-by-default?]
   (let [sw-sym (sym/meta-flag-symbol name :private)
         default-val (if enable-by-default? enabled-val disabled-val)
         enabled?-fn (sym/str-symbol name "-enabled?")
         set-fn (sym/str-symbol "set-" name "-val")
         enable-fn (sym/str-symbol "enable-" name)
         ]
     `(do
        (def ~sw-sym (atom ~default-val))
        (defn ~enabled?-fn [] (= ~enabled-val (deref ~sw-sym)))
        (defn ~set-fn [val#] (reset! ~sw-sym val#))
        (defn ~enable-fn [true?#]
          (if true?# (~set-fn ~enabled-val) (~set-fn ~disabled-val)))))))
