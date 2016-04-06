(ns annotate.core
  "Annotate and type check."
  #?(:clj  (:use [clojure.pprint :only [pprint]])
     :cljs (:use [cljs.pprint :only [pprint]]))
  (:require [clojure.string :as string])
  #?(:cljs (:require-macros [annotate.core-macro :refer [with-canonical]])
     :clj (:require [annotate.core-macro :refer :all])))

(def ^:dynamic *canonical-name* false)
(def ^:dynamic *checking-enabled* false)

(defprotocol Typeable
  (display-type [this])
  (valid-type? [this])
  (check [this that]))

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
