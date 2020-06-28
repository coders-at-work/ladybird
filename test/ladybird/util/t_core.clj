(ns ladybird.util.t-core
    (:require [midje.sweet :refer :all]
              [ladybird.util.core :refer :all]
              )
    )

(facts "Can construct a string containing the class name, message, exception data and stack trace of an exception"
       (fact "Can construct the string if exception data exists"
             (get-stack-trace-str (ex-info "a" {:a 1})) => #"clojure.lang.ExceptionInfo: a \{:a 1\}\n.+"
             )
       (fact "Can construct the string if exception data doesn't exist"
             (get-stack-trace-str (Exception. "a")) => #"java.lang.Exception: a\n.+"
             ))
