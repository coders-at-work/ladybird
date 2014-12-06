(ns ladybird.data.cond
    (:use [clojure.walk :only (postwalk)]))

(defn- convert-op-value[op value]
       (condp = op
         '<> ['!= value]
         'contain ['like (str "%" value "%")]
         'start-with ['like (str value "%")]
         'end-with ['like (str "%" value)]
         'is-null ['nil?]
         [op value]))

(defn- pred [[op field value :as p]]
       (let [[op value] (convert-op-value op value)]
         (if (= 2 (count p))
           `(list '~op ~field)
           `(list '~op ~field ~value))))

(defn- logical [[op & args]]
  `(list '~op ~@args))

(defn- pred? [x]
  (and (seq? x) (#{'= '!= '< '> '<= '>= 'like 'nil? 'in '<> 'contain 'start-with 'end-with 'is-null} (first x))))

(defn- logical? [x]
  (and (seq? x) (#{'and 'or 'not} (first x))))

;; build-in operators used by condition
;;  =    
;;  !=   
;;  <    
;;  >    
;;  <=   
;;  >=   
;;  and  
;;  or   
;;  not  
;;  like 
;;  nil? 
;;  in   
;; we add the following operator for convenience
;;  <>
;;  contain
;;  start-with
;;  end-with
;;  is-null
(defmacro make
  "generate condition which is a list made of build-in operators and values, ex. '(= :id 1)
   lower adapter of sql abstraction library must transalte this condition to where clause"
  [conds]
  (postwalk #(cond (pred? %) (pred %)
                   (logical? %) (logical %)
                   :default %)
            conds))

(def RAW :<raw>)

(defn raw
  "construct a list which means a raw string in a sql statement"
  [str]
  (list RAW str))

(defn raw? [x]
  (and (list? x)
       (= RAW (first x))))

(defn and-cond [clause1 & clauses]
  (let [conds (filter (complement nil?) (cons clause1 clauses))]
    (if-not (empty? conds)
      (apply list 'and conds))))

(defn or-cond [clause1 & clauses]
  (let [conds (filter (complement nil?) (cons clause1 clauses))]
    (if-not (empty? conds)
      (apply list 'or conds))))
