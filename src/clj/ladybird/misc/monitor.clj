(ns ladybird.misc.monitor
    (:use [ladybird.util.core :only (def-bindable)]
          [ladybird.misc.conf :only (def-switcher)])
    (:require [ladybird.util.symbol :as sym]
              [ladybird.util.coll :as coll]
              [clojure.tools.logging :as log]))

;; As a developer, we usually need to monitor the execution of our programs at some time. Normally we accomplish this task with logging(or something similar, like pritnln etc.) directly. But the model of logging doesn't match monitoring perfectly. The various levels of logging(trace, debug, info, warn, error) don't have so much business with monitoring. And the logging statements spreading in our codes make them in a mess.
;;
;; So let's think about what we need when we monitor. The one of the two things we most likely need when monitoring is to monitor the execution path of the code and the arguments' value passed to the functions on the path when errors occured. The other thing is to monitor the elapsed time of some portions of the execution path when performance problems occured.
;;
;; Maybe the best way to help programers to code monitoring is automatically generating code to push the execution path information in a stack and to calculate the elapsed time of the execution when defining a function. And the automatically genenrating code also includes a catch point to catch all exceptions thrown in the function and then to log the infomation. This can be implemented via macro.
;;
;; This namespace provides infrastructure to recod info in a thread and provides macros to wrap this functionality. Then programmers can accomplish code generation metioned above based on it.



;; monitor config
(def-switcher root-monitor-cfg :exec-path false true)

(def ^:private monitor-cfg (atom nil))

(defn set-monitor-cfg!
  "
  Notes: Setting a value of a key to false doesn't mean to disable monitoring on that level, just means falling through to its' parent level, up to root.
  "
  ([ns-sym val] {:pre [(contains? #{:time :exec-path false} val)]} 
   (swap! monitor-cfg assoc ns-sym val))
  ([ns-sym var-sym val] {:pre [(contains? #{:time :exec-path false} val)]} 
   (swap! monitor-cfg assoc (sym/str-symbol ns-sym "/" var-sym) val)))

(defn get-monitor-cfg
  ([ns-sym]
   (or (get-in @monitor-cfg [ns-sym])
       @root-monitor-cfg))
  ([ns-sym var-sym]
   (or (get-in @monitor-cfg [(sym/str-symbol ns-sym "/" var-sym)])
       (get-monitor-cfg ns-sym))))


;; monitor infrastructure
(def-bindable mon-info nil)

(defn assoc-in-mon-info! [ks v]
  (swap! (mon-info) assoc-in ks v))

(defn assoc-mon-info! [k v]
  (assoc-in-mon-info! [k] v))

(defn get-in-mon-info [& ks]
  (when-let [mi (mon-info)] (get-in @mi ks)))

(defn call-stack-info []
  (get-in-mon-info :call-stack-info))

(defn stack-at [indices]
  (get-in (call-stack-info) indices))

(defn v-stack
  "
  Constructs a vector representing a call path.
  "
  [& sts]
  (apply vector ::v sts))

(defn h-stack
  "
  Constructs a vector representing serial function calls of the same level in the call path.
  "
  [& sts]
  (apply vector ::h sts))

(defn- first-index [indices]
  (assoc indices (dec (count indices)) 0))

(defn- first-element [indices]
  (-> indices first-index stack-at))

(defn in-v-stack? [indices]
  (= ::v (first-element indices)))

(defn in-h-stack? [indices]
  (= ::h (first-element indices)))

(defn v-stack? [st]
  (and (vector? st) (= ::v (first st))))

(defn h-stack? [st]
  (and (vector? st) (= ::h (first st))))

(defn call-stack-str []
  (let [call-stack (call-stack-info)
        f #(cond
             (v-stack? %) (->> (rest %) (interpose '->) vec)
             (h-stack? %) (->> (rest %) (interpose '|) )
             :others %)
        call-stack (clojure.walk/postwalk f call-stack)
        ]
    (str "  ===  current call stack: " call-stack)))

(defn inc-last-index [indices]
  (->> (last indices) inc (assoc indices (dec (count indices)))))

(defn assoc-in-stack! [is st]
  (assoc-in-mon-info! (apply conj [:call-stack-info] is) st))

(defn set-call-stack! [st]
  (assoc-in-mon-info! [:call-stack-info] st))

(defn- adjust-v-stack [parent-indices]
  (let [info (call-stack-info)
        parent-container-indices (drop-last parent-indices)
        parent-container (get-in info parent-container-indices)
        parent-index (last parent-indices)
        [first-part second-part] (split-at (inc parent-index) parent-container)
        first-part (vec first-part)
        second-part (if (= 1 (count second-part)) (first second-part) (apply v-stack second-part))
        second-part (h-stack second-part)
        parent-container (conj first-part second-part)
        ]
    (if (empty? parent-container-indices) parent-container (assoc-in info parent-container-indices parent-container))))

(defn- adjust-parent-child-indices! [parent-indices]
  (if (in-v-stack? parent-indices)
    (let [is (inc-last-index parent-indices)
          st (stack-at is)
          is (if st
               (if (h-stack? st)
                 (->> st count (conj is))
                 (do
                   (set-call-stack! (adjust-v-stack parent-indices))
                   (conj is 2)))
               is)
          ]
      [parent-indices is])
    (do
      (assoc-in-stack! parent-indices (v-stack (stack-at parent-indices)))
      [(conj parent-indices 1) (conj parent-indices 2)])))

(defn parent-stack-indices []
  (get-in-mon-info :parent-stack-indices))

(defn set-parent-stack-indices! [indices]
  (assoc-mon-info! :parent-stack-indices indices))

(defn append-stack! [m]
  (let [parent-indices (parent-stack-indices)
        [parent-indices current-indices] (adjust-parent-child-indices! parent-indices)
        ]
    (assoc-in-stack! current-indices m)
    (set-parent-stack-indices! current-indices)
    parent-indices))

(defn top-time-monitor-existed? []
  (:top-time-monitor-existed? @(mon-info)))

(defn set-top-time-monitor! []
  (assoc-mon-info! :top-time-monitor-existed? true))

(defn clean-top-time-monitor! []
  (swap! (mon-info) dissoc :top-time-monitor-existed?))

(defn set-error-occured! []
  (assoc-mon-info! :error-occured? true))

(defn error-occured? []
  (:error-occured? @(mon-info)))

(defn initial-mon-info [stack-info]
  (atom {:call-stack-info (v-stack stack-info) :parent-stack-indices [1]}))

;; monitor functionality 
(defmacro begin-stack [stack-info & body]
  `(with-mon-info (initial-mon-info ~stack-info) ~@body))

(defmacro enter-stack [tag-completed? stack-info & body]
  (let [complete-body (when tag-completed?
                        `(assoc-in-stack! (parent-stack-indices) {~stack-info :completed})
                        )
        body `(let [parent-indices# (append-stack! ~stack-info)
                    r# (do ~@body)
                    ]
                ~complete-body
                (set-parent-stack-indices! parent-indices#)
                r#
                )]
    body))

(defmacro enter-mon-time [stack-info & body]
  (let [body `(let [start# (System/nanoTime)
                    parent-indices# (append-stack! ~stack-info)
                    ret# (do ~@body)
                    current-indices# (parent-stack-indices)
                    ]
                (let [end# (System/nanoTime)
                      elapsed# (-> (/ (double (- end# start#)) 1000000.0) (str " ms"))
                      ]
                  (assoc-in-stack! current-indices# {~stack-info elapsed#})
                  (set-parent-stack-indices! parent-indices#)
                  ret#)
                )]
    `(if-let [top-time-monitor-existed?# (top-time-monitor-existed?)]
             ~body
             (do
               (set-top-time-monitor!)
               (let [r# ~body]
                 (when-not (error-occured?) (log/info (call-stack-str)))
                 (clean-top-time-monitor!)
                 r#)))))

(defmacro begin-mon-time [stack-info & body]
  `(with-mon-info (atom {:call-stack-info (v-stack) :parent-stack-indices [0]}) (enter-mon-time ~stack-info ~@body))
  )

(defmacro start-monitor [mon-type stack-info & body]
  (if (= mon-type :time)
    `(begin-mon-time ~stack-info ~@body)
    `(begin-stack ~stack-info ~@body)))

(defmacro join-monitor [mon-type stack-info & body]
  (if (= mon-type :time)
    `(enter-mon-time ~stack-info ~@body)
    `(enter-stack true ~stack-info ~@body)))

(defmacro monitor [mon-type stack-info & body]
  `(if (mon-info)
     (join-monitor ~mon-type ~stack-info ~@body)
     (start-monitor ~mon-type ~stack-info ~@body)))

(defmacro monitor-exec-state
  "
  Usually you only need to call this macro. The other macros above don't tend to be called directly by programers.
  "
  [ns-sym fn-sym args & body]
  (if-let [mon-type (get-monitor-cfg ns-sym fn-sym)]
          `(monitor ~mon-type (apply list (sym/str-symbol '~ns-sym "/" '~fn-sym) ~args) ~@body)
          `(do ~@body)))
