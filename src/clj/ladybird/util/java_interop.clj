(ns ladybird.util.java-interop
    (:require [clojure.string :as string]
              [ladybird.util.string :as str])
    )

(defn new-instance
  "create an instance of klass"
  [klass & args]
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))

(defn dynamic-call
  "dynamically call a method of the java obj, accepting variant args"
  [obj method & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj (name method) (into-array Object args)))

(defn bean-by-map [m o]
  (doseq [k (keys m)]
         (let [method (->> (name k) (str "set-") str/camel-case)]
           (dynamic-call o method (k m))))
  o)

(defn bean-from-map [m clz & args]
  (let [o (apply new-instance clz args)]
    (bean-by-map m o)))
