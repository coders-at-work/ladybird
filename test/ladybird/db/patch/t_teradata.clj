(ns ladybird.db.patch.t-teradata
  (:require [midje.sweet :refer :all]
            [ladybird.db.patch.teradata :as t]
            ))

(def classname "com.teradata.jdbc.TeraDriver")
(def subprotocol "teradata")
(def host "test-host")
(def user "test-user")
(def password "test-password")
(def port 8888)
(def other-param "other-param")

(facts "create database specification for teradata"
       (fact "no opts"
             (t/teradata {}) => {:classname classname
                                 :subprotocol subprotocol
                                 :subname "//localhost/USER=,PASSWORD="
                                 :make-pool? true
                                 })
       (fact "opts with host"
             (t/teradata {:host host}) => {:classname classname
                                           :subprotocol subprotocol
                                           :subname (format "//%s/USER=,PASSWORD=" host)
                                           :make-pool? true
                                           })
       (fact "opts with host, user, password"
             (t/teradata {:host host :user user :password password}) => {:classname classname
                                                                         :subprotocol subprotocol
                                                                         :subname (format "//%s/USER=%s,PASSWORD=%s" host user password)
                                                                         :make-pool? true
                                                                         })
       (fact "opts with host, port"
             (t/teradata {:host host :port port}) => {:classname classname
                                                      :subprotocol subprotocol
                                                      :subname (format "//%s/USER=,PASSWORD=,DBS_PORT=%s" host port)
                                                      :make-pool? true
                                                      })
       (fact "opts with host, user, password, other-param"
             (t/teradata {:host host :user user :password password :other-param other-param}) => {:classname classname
                                                                                      :subprotocol subprotocol
                                                                                      :subname (format "//%s/USER=%s,PASSWORD=%s" host user password)
                                                                                      :make-pool? true
                                                                                      :other-param other-param
                                                                                      })
       )

