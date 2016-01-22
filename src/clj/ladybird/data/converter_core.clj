(ns ladybird.data.converter-core)

(defn convert-fn [c c-type] {:pre [(#{:in :out} c-type)]}
  (c-type c))

(defn in-fn [c] (convert-fn c :in))

(defn out-fn [c] (convert-fn c :out))

(defn tr-in [c v] ((in-fn c) v))

(defn tr-out [c v] ((out-fn c) v))

(defn value-converter [keyvals]
  {:pre [(not (empty? keyvals)) (even? (count keyvals)) (every? #(not (nil? %)) keyvals)]}
  {:in (apply hash-map (reverse keyvals)) :out (apply hash-map keyvals)})

(defn composite-converter [conv & convs]
  (let [convs (apply list conv convs)
        r-convs (reverse convs)
        composite-conv-fn (fn [in-out convs] (apply comp (map in-out convs)))]
    {:in (composite-conv-fn :in convs) :out (composite-conv-fn :out r-convs)}))

;; TODO should change to [pred-in fin pred-out fount]
(defn when-not-converter
  "converter based upon the logical truth of a predicate. If it is true, the object will not be converted.
   If it is false, in or out function will be called to convert the object.
   Args:
   Return:
   a converted obj"
  ([pred f]
   (when-not-converter pred f f))
  ([pred fin fout]
   (let [convert-when-not (fn [f] #(if (pred %) % (f %)))]
     {:in (convert-when-not fin) :out (convert-when-not fout)})))

(defn when-converter
  ([pred f]
   (when-converter pred f f))
  ([pred fin fout]
   (when-not-converter (complement pred) fin fout)))

(defn nullable-converter
  ([f]
   (nullable-converter f f))
  ([fin fout]
   (when-not-converter nil? fin fout)))
