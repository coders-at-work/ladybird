(ns ladybird.domain.fn
    (:use [ladybird.util.symbol :only (str-symbol)]
          [ladybird.util.string :only (qualify-name)])
    (:require [ladybird.data.order :as o]))

(defmacro def-field-value-pred
  "
   Define a macro to check whether the value of the specified field of a domain object equals the specified vlaue.
   Params:
       field -- the specified field, the name of the generated macro will be 'is-field?'
   
   Ex.    
      (def-field-value-preds :status)

      (is-status=? :active a-domain-object)
  "
  [field]
  `(defmacro ~(str-symbol "is-" (name field) "=?") [value# domain-obj#]
     `(= ~value# (~~field ~domain-obj#))))

(defn compare-order-by [order-def op v1 v2]
  (let [op-m {'> o/order>? '>= o/order>=? '< o/order<? '<= o/order<=? '= o/order=?
              '>? o/order>? '>=? o/order>=? '<? o/order<? '<=? o/order<=? '=? o/order=?}
        pred (op-m op)
        ]
    (pred order-def v1 v2)))

(defmacro def-field-order-preds
  "
   Define functions to compare the order of a domain object field with a value, or the order of another domain objcect field.
   Ex.
      (def-field-order-preds :trade-progress trade-progress-order)

      (is-trade-progress-order-of? a-domain-obj >=? :paid)
      (is-trade-progress-order-of? a-domain-obj < :finished ) ;; < same as <?

      (are-trade-progress-orders-of? a-domain-obj >? another-domain-obj)
  "
  [field order-def]
  (let [compare-order-fn (str-symbol "compare-order-by-" field "-as-" order-def)
        qualified-compare-order-fn (symbol (qualify-name compare-order-fn))
        ]
    `(do
       (def ~compare-order-fn (partial compare-order-by ~order-def))
       (defmacro ~(str-symbol "is-" (name field) "-order-of?") [domain-obj# op# val#]
         `(~'~qualified-compare-order-fn  '~op# (~~field ~domain-obj#) ~val#))
       (defmacro ~(str-symbol "are-" (name field) "-orders-of?") [domain-obj-a# op# domain-obj-b#]
         `(~'~qualified-compare-order-fn '~op# (~~field ~domain-obj-a#) (~~field ~domain-obj-b#))
         ))))

(defmacro def-field-value-&-order-preds
  "
   Same as the following code:
                       (do
                         (def-field-value-pred field)
                         (def-field-order-preds field order-def))
  "
  [field order-def]
  `(do
     (def-field-value-pred ~field)
     (def-field-order-preds ~field ~order-def)
     ))
