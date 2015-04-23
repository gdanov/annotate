(ns annotate.test.midje
  "Helper functions for testing with Midje."
  (:use [annotate.core :only [validate with-validation]]
        midje.sweet))

(defn- gen-validating-fact
  [arrow doc-string test]
  `(fact ~doc-string
     (with-validation ~test) ~arrow (throws clojure.lang.ExceptionInfo)))

(defmacro valid
  "Given a doc string and one or more tests, run the tests with
  with-validation enabled. The fact checks if no runtime exceptions are
  thrown."
  [doc-string test]
  (gen-validating-fact '=not=> doc-string test))

(defmacro invalid
  "Given a doc string and one or more tests, run the tests with
  with-validation enabled. The fact checks if a runtime exception is
  thrown."
  [doc-string test]
  (gen-validating-fact '=> doc-string test))

(defmacro typecheck
  "Validate the output type of a function. Additionally, code is run
  with with-validation enabled. Prefer valid/invalid where possible."
  [doc-string test type]
  `(fact ~doc-string
     (with-validation (validate ~type ~test)) '=> nil))
