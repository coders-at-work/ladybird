(ns ladybird.test.domain.core
    (:use ladybird.test.data.core)
    )

(use 'ladybird.data.enum :reload)

(defenum COLOR :blue "blue" :red "red" :green "green" :brown "brown" :black "black" :white "white")

(defdomain Profile [:id :name :birth :color :foods :cats] {:db-maintain-fields [:id] :converters {:color COLOR}})

(query-profile (make (= :color :green)))

(query-profile (merge Profile {:join-with [:p :t] :joins joins11}) (make (= :p.last-name "e")))

(query-profile (merge Profile {:join-with [:p :t] :joins joins11}) (make (= :color :black )))

(def p-joins {:pr [:inner Profile [[:id :profile-id] :color] (make (= :pr.id :id))] :t [:inner Tmp [[:id :tid] [:last-update :l-u] [:valid :v]] (make (and (= :pr.id :t.id) ))]})

(query-p (merge P {:join-with [:pr :t] :joins p-joins}) (make (= :pr.color :black)))

(def p-joins2 {:pr [:inner Profile [[:id :profile-id] :color] (make (and (= :pr.id :id) (= :pr.color :black)))] :t [:inner Tmp [[:id :tid] [:last-update :l-u] [:valid :v]] (make (and (= :pr.id :t.id) ))]})

(query-p (merge P {:join-with [:pr :t] :joins p-joins2}) ())
