(ns ladybird.svc.build-in-svc
    (:use ladybird.svc.core
          ladybird.svc.generate)
    )

(defmacro def-mon-svc
  "
  Defines a monitor svc with catch point.
  "
  [& args]
  `(s-svc [encapsule-body transform check-and-bind catch-stack with-stack gen-defn] ~@args)
  )

(defmacro def-mon-internal
  "
  Defines a monitor svc without catch point.
  "
  [& args]
  `(s-svc [encapsule-body with-stack gen-defn] ~@args)
  )
