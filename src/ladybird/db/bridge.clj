(ns ladybird.db.bridge
    (:require [ladybird.db.patch.korma :as dbk]))

(def init-db dbk/init-db)

(def select dbk/select)

(def insert! dbk/insert!)

(def update! dbk/update!)
