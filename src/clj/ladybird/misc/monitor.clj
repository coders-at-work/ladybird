(ns ladybird.misc.monitor
    (:use [ladybird.util.core :only (def-bindable)]
          [ladybird.misc.conf :only (def-switcher)])
    (:require [ladybird.util.symbol :as sym]
              [ladybird.util.coll :as coll]
              [clojure.tools.logging :as log]))

;; We usually need to monitor the execution of our programs at some time. Normally we accomplish this task with logging(or something similar, like pritnln etc.) directly. But the model of logging doesn't match monitoring perfectly. The various levels of logging(trace, debug, info, warn, error) don't have so much business with monitoring. And the logging statements spreading in our codes make them in a mess.
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
  ;; TODO: change key from vector to qualified symbol
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

(defn call-stack-info []
  (when-let [mi (mon-info)] (:call-stack-info @mi)))

(defn call-stack-str []
  (->> (call-stack-info) (interpose '->) vec (str "  ===  current call stack: "))
  )

(defn append-call-stack! [m & ms]
  (swap! (mon-info) #(apply update-in % [:call-stack-info] conj m ms)))

(defn reset-call-stack! [st]
  (swap! (mon-info) assoc-in [:call-stack-info] st))

(defn replace-a-stack! [i st]
  (let [st-info (call-stack-info)
        [first-half second-half] (split-at i st-info)
        st-info (coll/concatv first-half [st] (rest second-half))
        ] 
    (reset-call-stack! st-info)))

(defn top-time-monitor-existed? []
  (:top-time-monitor-existed? @(mon-info)))

(defn- assoc-mon-info! [k v]
       (swap! (mon-info) assoc k v))

(defn set-top-time-monitor! []
  (assoc-mon-info! :top-time-monitor-existed? true))

(defn set-error-occured! []
  (assoc-mon-info! :error-occured? true))

(defn error-occured? []
  (:error-occured? @(mon-info)))

(defn initial-mon-info [stack-info]
  (atom {:call-stack-info [stack-info]}))

;; monitor functionality 
(defmacro begin-stack [stack-info & body]
  `(with-mon-info (initial-mon-info ~stack-info) ~@body)
  )

(defmacro enter-stack [tag-completed? stack-info & body]
  (let [body `(do (append-call-stack! ~stack-info) ~@body)]
    (if tag-completed?
      `(let [i# (count (call-stack-info))
             r# ~body]
         (replace-a-stack! i# {~stack-info :completed})
         r#)
      body)))

(defmacro enter-mon-time [stack-info & body]
  (let [body `(let [start# (System/nanoTime)
                    i# (count (call-stack-info))
                    ret# (enter-stack false ~stack-info ~@body) 
                    ]
                (let [end# (System/nanoTime)
                      elapsed# (-> (/ (double (- end# start#)) 1000000.0) (str " ms"))
                      ]
                  (replace-a-stack! i# {~stack-info elapsed#})
                  ret#))
        ]
    `(if-let [top-time-monitor-existed?# (top-time-monitor-existed?)]
             ~body
             (do
               (set-top-time-monitor!)
               (let [r# ~body]
                 (when-not (error-occured?) (log/info (call-stack-str)))
                 r#)))))

(defmacro begin-mon-time [stack-info & body]
  `(with-mon-info (atom {:call-stack-info []}) (enter-mon-time ~stack-info ~@body))
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
