(ns annotate.core
  "Annotate and type check."
  #?(:clj  (:use [clojure.pprint :only [pprint]])
     :cljs (:use [cljs.pprint :only [pprint]]))
  (:require [clojure.string :as string])
  #?(:cljs (:require-macros [annotate.core :refer [with-canonical]])))

(def ^:dynamic *canonical-name* false)
(def ^:dynamic *checking-enabled* false)

(defprotocol Typeable
  (display-type [this])
  (valid-type? [this])
  (check [this that]))

#?(:clj
   (do
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
     ))

(defn- throwe [msg errors]
  (throw (ex-info msg {:errors errors})))

(defn check*
  "Type check type/value pairs. Throws an exception if one or more values
  fail to type check. Used internally in defn', defnv, etc.

  For example:
  (check* \"user\" Keyword :age Int 10.0)

  Will throw an exception."
  [label & args]
  (assert (even? (count args)))
  (let [res (->> (partition 2 args)
                 (map #(apply check %))
                 (remove nil?))]
    (when (seq res)
      (let [invalid (->> (map pr-str res)
                         (string/join ", "))]
        (throwe (str "Failed to type check " label ": " invalid) res)))))

(defmacro assert*
  [left right & msg]
  `(let [err# (check ~left ~right)
         ~'valid? (nil? err#)]
     (assert ~'valid? (str ~@msg " \ndetails:\n" err#))))

(defn lazy-check
  "Lazily check that each member of a sequence s is of type t. Throws
  an exception if a realized value is not of type t. Returns a lazy
  seq."
  [t s]
  (map (fn [x] (check* "sequence" t x) x) s))

(defn display-canonical
  "Returns the canonical type annotation for the given type."
  [t]
  (with-canonical (display-type t)))
