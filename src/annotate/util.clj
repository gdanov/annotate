(ns annotate.util)

(defn lookup
  "Lookup system property, returning a default value
  if the property cannot be found."
  [prop default]
  (or (System/getProperty prop) default))

(defn array? [x]
  (-> x .getClass .isArray))

(defn fq-ns [v]
  (let [{:keys [ns name]} (meta v)]
    (if (= (ns-name ns) 'clojure.core)
      name
      (symbol (str ns "/" name)))))

(declare truncate)

(defn truncate-coll
  "Recursively truncate a collection to a certain maximum length.
  Adds an ellipsis where truncation occurs."
  [coll]
  (let [max-length 5
        max-length-exceeded (> (count coll) max-length)
        coll' (if max-length-exceeded (take max-length coll) coll)]
    (cond (map? coll)
          (let [s (map (fn [[k v]] [(truncate k) (truncate v)]) coll')]
            (into {} (if max-length-exceeded
                       (concat s '([... ...]))
                       s)))
          (seq? coll)
          (let [s (map truncate coll')]
            (apply list (if max-length-exceeded
                          (concat s '(...))
                          s)))
          :else
          (let [s (map truncate coll')]
            (into (empty coll) (if max-length-exceeded
                                 (concat s '(...))
                                 s))))))

(defn truncate-str [s]
  (let [max-length 20]
    (if (> (.length s) max-length)
      (str (subs s 0 max-length) "...")
      s)))

(defn truncate [x]
  (cond (coll? x) (truncate-coll x)
        (string? x) (truncate-str x)
        :else x))

(defn parse-arglist
  "Takes a vector with inputs and output types
  separated by =>.  For example,

  [String => String]

  Returns a pair of the inputs and output."
  [t]
  (let [pred (partial not= '=>)
        inputs (take-while pred t)
        outputs (rest (drop-while pred t))
        output (when (seq outputs) (first outputs))]
    [inputs output]))

(defn parse-doc-attr-map
  "Parse the doc-string and attr-map."
  [[fst snd & rst :as args]]
  (cond (and (string? fst) (map? snd)) args
        (string? fst) (list* fst nil snd rst)
        (map? fst) (list* nil fst snd rst)
        :else (list* nil nil args)))

(defn remove-pre-post
  "Remove pre/post conditions from the body."
  [[fst & rst :as body]]
  (if (and (> (count body) 1) (map? fst)
           (or (contains? fst :pre) (contains? fst :post)))
    rst
    body))

(defn rest-args? [args]
  (= (first (drop (- (count args) 2) args)) '&))

(defmacro and-not
  [& forms]
  `(and ~@(map (fn [form] `(not ~form)) forms)))

(defn arity-match?
  "Do the arities of the arglists and types match?"
  [arglists ts]
  (let [ts (if (vector? ts) (list ts) ts)]
    (and (= (count arglists) (count ts))
         (->> (map (fn [a t]
                     (let [inputs (first (parse-arglist t))]
                       (and (= (count a) (count inputs))
                            (or (and (rest-args? a) (rest-args? inputs))
                                (and-not (rest-args? a) (rest-args? inputs))))))
                   arglists ts)
              (every? true?)))))

(defmacro assert-arity-match
  [sym arglists ts]
  `(assert (arity-match? ~arglists ~ts) (str ~sym " arity mismatch")))

(defn fix-arglists
  "Restore arglists metadata to the expected value."
  [v arglists]
  (alter-meta! v (fn [m]
                   (->> (if (vector? arglists)
                          (list arglists)
                          arglists)
                        (assoc m :arglists)))))

(defn remove-amp [xs]
  (remove #(= '& %) xs))

(defmacro lookup-arglists [sym]
  `(-> (var ~sym) meta :arglists))

(defn typecheck?
  "Is type checking enabled?"
  []
  (= (lookup "annotate.typecheck" "off") "on"))

(defn parse-fn-args [n-or-t args]
  (if (symbol? n-or-t)
    [n-or-t (first args) (rest args)]
    [nil n-or-t args]))

(defn quote-special [form]
  (cond (list? form) (apply list (map quote-special form))
        (vector? form) (mapv quote-special form)
        (= form '=>) ''=>
        (= form '&) ''&
        :else form))
