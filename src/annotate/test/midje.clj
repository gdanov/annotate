(ns annotate.test.midje
  "Helper functions for testing with Midje.

  Note: You must add Midje to your project's dependency vector."
  (:use midje.sweet))

(defn- gen-checking-fact
  [arrow doc-string test]
  `(fact ~doc-string
     ~test ~arrow (throws clojure.lang.ExceptionInfo)))

(defmacro valid
  "Given a doc string and one or more tests, run the tests. The fact
  checks if no runtime exceptions are thrown."
  [doc-string test]
  (gen-checking-fact '=not=> doc-string test))

(defmacro invalid
  "Given a doc string and one or more tests, run the tests. The fact
  checks if a runtime exception is thrown."
  [doc-string test]
  (gen-checking-fact '=> doc-string test))
