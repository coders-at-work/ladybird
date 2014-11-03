(ns ladybird.test.svc.build-in-svc)

(require '[ladybird.misc.monitor :as mon] :reload)
(use 'ladybird.svc.build-in-svc :reload)

(def-mon-internal a21 "a21" [x y] 1)
(def-mon-internal a22 "a22" [x y] 1)
(def-mon-internal a11 "a11" [x y] (a21 x y) (a22 x y))
(def-mon-internal a23 "a23" [x y] 1)
(def-mon-internal a24 "a24" [x y] (a23 x y))
(def-mon-internal a12 "a12" [x y] (a24 x y))
(def-mon-internal a "a" [x y] (a11 x y) (a12 x y))
(def-mon-internal b "b" [x y] 1)

(def-mon-svc d "d" [x y] (a x y) (b x y) (/ 0))
