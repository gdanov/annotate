(ns annotate.fns-test
  (:use [annotate types fns]
        ;;midje.sweet
        clojure.test
        ))

(deftest t-fn
  (is (= "Hello, world" ((fnv [=> String] [] "Hello, world"))))
  (is (= "Bob" ((fnv [String => String] [x] x) "Bob")))
  (is (= "BillyBob"
        ((fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "BillyBob"
        ((fnv cat ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "Bob" ((fnv id [String => String] [x] x) "Bob")))
  (is  (thrown-with-msg? clojure.lang.ExceptionInfo
         #"Failed to type check id input"
         ((fnv id [String => String] [x] x) 42)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Failed to type check anonymous input"
        ((fnv [String => String] [x] x) 10)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Failed to type check anonymous output"
        ((fnv [String => String] [x] 20) "Bob")))
  (is (= "Bob" ((fna [String => String] [x] x) "Bob")))
  (is (= "Bob" ((fn$ [String => String] [x] x) "Bob"))))
