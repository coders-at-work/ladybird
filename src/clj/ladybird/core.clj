(ns ladybird.core)

; chaining generation
(defn chain-proc
  "
  Process data through chaining functions in proc-fns. The left most function will process data first, then the second function, and so on.

  Ex:
     (chain-proc 3 #(* % 2) inc) => 7
     (chain-proc 3) => 3
  "
  [data & proc-fns]
  (let [proc-fn (->> (reverse proc-fns) (apply comp))]
    (proc-fn data)))

(defmacro chain-gen
  "
  Expand to process meta-m(a map) through chaining functions in gen-fns, then evaluate the result. Can be used to generate code.
  Params:
     meta-m   --  a map of meta data
     gen-fns  --  functions to generate some code 

  Ex:
     (chain-gen {:a 1} ) => {:a 1}
     (chain-gen {:v 3} #(assoc % :f `(inc ~(:v %))) (fn [m] `(* 2 ~(:f m)))) => 8
  "
  [meta-m & gen-fns]
  `(eval (chain-proc ~meta-m ~@gen-fns))
  ;; TODO: remove the following code
  #_(let [gen-fns (vec gen-fns)]
    `(let [gen-fn# (->> (reverse ~gen-fns) (apply comp))]
       (eval (gen-fn# ~meta-m)))))
