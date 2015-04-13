(ns ladybird.test.data.core)

;; init
(use 'korma.db)

(def db (mysql {:subname "//127.0.0.1:3306/cookbook" :user "xzj" :password "xzjamjzx"}))

(require '[ladybird.db.core :as dbc] :reload)

(dbc/init-main-db db)

(use 'ladybird.domain.core :reload)

(use 'ladybird.data.cond :reload)

(use 'ladybird.data.db-converter :reload-all)

(defdomain P [:id :name :age :first-name [:lastName :last-name]] {:db-maintain-fields [:id]})

(defdomain Tmp [:id :create-time :last-update :valid]
               {:db-maintain-fields [:id :last-update]
                :add-fixed {:create-time nil}
                :immutable-fields [:create-time]
                :converters {:valid BOOL}})

(require '[ladybird.data.core :as dc] :reload)

;; test db field mapping
(add-p! {:name "abc" :age 12 :first-name "a" :last-name "c"})
; 91

(update-p! (make (= :id 91)) {:last-name "d"})
; 1

(save-p! {:id 91 :last-name "e"})
; 1

(query-p (make (= :last-name "e")))


;; test raw
(query-tmp (make (in :valid [true (raw "'F'")])))
(map :id *1)

(query-tmp (assoc Tmp :fields (conj (:fields Tmp) (raw "id+1"))) (make (= :id 1)))


;; test generating condition dynamically
(get-tmp 1)
(def lu (:last-update *1))
(query-tmp (make (= :last-update lu)))
; should return id 1 record


;; test join
(def joins11 {:p [:inner P [[:id :pid] [:last-name :ln] (make (= :p.id :id))]
              :t [:inner Tmp [[:id :tid] [:last-update :l-u] [:valid :v]] (make (and (= :p.id :t.id) (in :t.valid [true])))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins11 :fields [:id]} (make (= :p.last-name "e")))

(def joins12 {:p [:inner P [[:id :pid] :last-name] (make (and (= :p.id :id) (= :p.last-name "e")))]
              :t [:inner Tmp [[:id :tid] [:last-update :l-u] [:valid :v]] (make (and (= :p.id :t.id) (in :t.valid [true])))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins12 :fields [:id]} ())
; the result should be same as last query

(def joins10 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
              :t [:inner Tmp [[:id :tid] [:last-update :l-u] [:valid :v]] (make (and (= :p.id :t.id) (in :t.valid [true])))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins10 :fields [:id]} ())
(map :id *1)

(def joins9 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
             :t [:inner Tmp [[:id :tid] [:last-update :l-u] :valid] (make (and (= :p.id :t.id) (= :t.valid true)))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins9 :fields [:id]} ())
(map :id *1)

(def joins8 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
             :t [:inner Tmp [[:id :tid] [:last-update :l-u] :valid] (make (and (= :p.id :t.id) (= :t.last-update lu)))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins8 :fields [:id]} ())
; should return id 1 record

(def joins7 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]})
(dc/query  "profile" {:join-with [:p] :joins joins7 :fields [:id]} ())

(def joins6 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
             :t [:inner Tmp [[:id :tid] (raw "last_update") :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins6 :fields [:id]} ())

(def joins5 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
             :t [:inner Tmp [[:id :tid] [(raw "last_update") :la-u] :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins5 :fields [:id]} ())

(def joins4 {:p [:left P [[:id :pid]] (make (= :p.id :id))]
             :t [:left "tmp" [[:id :tid] :last_update :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins4 :fields [:id]} ())

(def joins3 {:p [:left P [[:id :pid]] (make (= :p.id :id))]
             :t [:left Tmp [[:id :tid] :last-update :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins3 :fields [:id]} ())

(def joins2 {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
             :t [:inner Tmp [[:id :tid] :last-update :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins2 :fields [:id]} ())

(def joins {:p [:inner P [[:id :pid]] (make (= :p.id :id))]
            :t [:inner Tmp [[:id :tid] [:last-update :l-u] :valid] (make (= :p.id :t.id))]})
(dc/query  "profile" {:join-with [:p :t] :joins joins :fields [:id]} ())
