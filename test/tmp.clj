(ns tmp
    (:use [ladybird.story.core :only (defstory deFn)]
          ladybird.db.dml))

(defstory tm [] (select "limbs" ()))

(defn tt [] (select "limbs" ()))

(deFn ta [] (select "limbs" ()))

(defstory tmi [] (insert! "limbs" {:arms 9 :legs 9 :thing "abc"}))
