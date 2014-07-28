(ns ladybird.db.bridge
    (:require [ladybird.db.patch.korma :as dbk]))

(def select dbk/select)

(def insert! dbk/insert!)

(def update! dbk/update!)
