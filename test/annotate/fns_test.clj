(ns annotate.fns-test
  (:use [annotate types fns] midje.sweet))

(facts "fn"
  (fact ((fnv [=> String] [] "Hello, world")) => "Hello, world")
  (fact ((fnv [String => String] [x] x) "Bob") => "Bob")
  (fact ((fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob") =>
    "BillyBob")
  (fact ((fnv cat ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob") =>
    "BillyBob")
  (fact ((fnv id [String => String] [x] x) "Bob") => "Bob")
  (fact ((fnv id [String => String] [x] x) 42) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check id input(s): (not (instance? String 42))"))
  (fact ((fnv [String => String] [x] x) 10) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check anonymous input(s): (not (instance? String 10))"))
  (fact ((fnv [String => String] [x] 20) "Bob") =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check anonymous output: (not (instance? String 20))"))
  (fact ((fna [String => String] [x] x) "Bob") => "Bob")
  (fact ((fn$ [String => String] [x] x) "Bob") => "Bob"))
