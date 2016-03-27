(ns annotate.util-macro)

(defmacro and-not
  [& forms]
  `(and ~@(map (fn [form] `(not ~form)) forms)))

(defmacro assert-arity-match
  [sym arglists ts]
  `(assert (arity-match? ~arglists ~ts) (str ~sym " arity mismatch")))

(defmacro lookup-arglists [sym]
  `(-> (var ~sym) meta :arglists))
