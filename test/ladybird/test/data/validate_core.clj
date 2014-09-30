(ns ladybird.test.data.validate-core)


(use 'ladybird.misc.i18n :reload)
(use 'ladybird.data.validate-core :reload)

(validator (complement nil?) :not-nil "%s can not be null")
(def not-nil *1)
(not-nil 1)
(not-nil false)
(not-nil nil)
(def r *1)
(get-err-msg "" :it r)
(load-resource {"xzj" {:not-nil "%s cannot be a null value"}})
(get-err-msg "xzj" :it r)

(validator integer? :is-int "%s should be an integer")
(def is-int *1)

(validate nil not-nil is-int)
(validate "" not-nil is-int)
(validate 1 not-nil is-int)

(def vm {:a [not-nil is-int] :b not-nil :c is-int})
(validate-m vm {:a 1 :b 2 :c 3})
(validate-m vm {:a nil :b nil :c ""})

(def vmv (m-validator vm))
(vmv {:a 1 :b 2 :c 3})
(def r *1)
(vmv {:a nil :b nil :c ""})
(def r2 *1)

(err-msgs "" {} r)
(err-msgs "" {} r2)
(err-msgs "xzj" {} r2)
(err-msgs "xzj" {:a "Ta"} r2)
(err-msgs r2)
(err-msgs {:a "Ta"} r2)

(def-validator not-nil2 #(not (nil? %)) "%s must not null")
(not-nil2 2)
(not-nil2 nil)
