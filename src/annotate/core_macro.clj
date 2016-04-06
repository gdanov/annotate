(ns annotate.core-macro)

(defmacro annotation
  "Returns the simple type annotation for the var of the given symbol."
  [sym]
  `(-> (var ~sym) meta :annotate.core/simple))

(defmacro ppann
  "Pretty print the simple type annotation for the var of the given
  symbol."
  [sym]
  `(pprint (annotation ~sym)))

(defmacro canonical
  "Returns the canonical type annotation for the var of the given
  symbol."
  [sym]
  `(-> (var ~sym) meta :annotate.core/canonical))

(defmacro ppcan
  "Pretty print the canonical type annotation for the var of the given
  symbol."
  [sym]
  `(pprint (canonical ~sym)))

(defmacro with-canonical
  "Set the canonical flag to true within the scope of body."
  [& body]
  `(binding [annotate.core/*canonical-name* true]
     ~@body))

(defn- arity-expand
  "If the type annotation denotes a multi arity fn, make sure the
  individual arity annotations are wrapped in a proper list."
  [t]
  (if (and (list? t) (every? vector? t))
    `(list ~@t)
    t))

(defmacro ann
  "Annotate var referenced by sym with type t. Prepends the type
  annotation to the var’s doc string and adds two metadata keys. Must be
  called after defing the var."
  [sym t]
  `(let [t# ~(arity-expand t)
         f# (fn [{doc# :doc, :as m#}]
              (assoc m# :doc (str (display-type t#)
                               (when doc# (str "\n\n" doc#)))
                :annotate.core/simple (display-type t#)
                :annotate.core/canonical (with-canonical
                              (display-type t#))))]
     (alter-meta! (var ~sym) f#)))

(defmacro with-checking
  "Execute body with type checking enabled."
  [& body]
  `(binding [annotate.core/*checking-enabled* true]
     ~@body))
