(ns annotate.fns-test
  (:require [annotate.types :as types #?@(:cljs [:refer [String]])]
            [annotate.fns :as fns :include-macros true]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is]])))

(deftest t-fn
  (is (= "Hello, world" ((fns/fnv [=> String] [] "Hello, world"))))
  (is (= "Bob" ((fns/fnv [String => String] [x] x) "Bob")))
  (is (= "BillyBob"
        ((fns/fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "BillyBob"
        ((fns/fnv cat ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "Bob" ((fns/fnv id [String => String] [x] x) "Bob")))
  (is  (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Object)
         #"Failed to type check id input"
         ((fns/fnv id [String => String] [x] x) 42)))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Object)
        #"Failed to type check anonymous input"
        ((fns/fnv [String => String] [x] x) 10)))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Object)
        #"Failed to type check anonymous output"
        ((fns/fnv [String => String] [x] 20) "Bob")))
  (is (= "Bob" ((fns/fna [String => String] [x] x) "Bob")))
  (is (= "Bob" ((fns/fn$ [String => String] [x] x) "Bob")))
  ;; TODO arity match for fn
  (is (thrown? #?(:clj java.lang.AssertionError :cljs js/Object)
        (fns/fnv [String String => String] [a] true)))
  #?(:clj
     (is (thrown? #?(:clj java.lang.AssertionError :cljs js/Object)
           (fns/defnv xyz [String String => String] [a] true))))
  ;; TODO &&&& ?
  ;; TODO [=>] -> what args are valid? (see above?)
  ;; TODO destructuring
  )

(deftest t-match-arity
  (is (false? "todo")))

#?(:clj
   (deftest t-parse-arg
     (testing "single arity"
       (is (= [[['String] 'String]] (fns/parse-args+ '[String => String])))
       (is (= [[['String 'types/Int] 'String]] (fns/parse-args+ '[String types/Int => String])))
       (is (= [[[] 'types/Int]] (fns/parse-args+ '[=> types/Int])))
       (is (= [[["test"] 'Boolean]] (fns/parse-args+ '["test" => Boolean])))
       (is (thrown? java.lang.AssertionError (fns/parse-args+ '[String =>])))
       (is (thrown? java.lang.AssertionError (fns/parse-args+ '[String types/Int])))
       (is (thrown? java.lang.AssertionError (fns/parse-args+ '[=>]))))
     (testing "1+ arity"
       (is (= [[['String 'types/Int] 'String]]
             (fns/parse-args+ '([String types/Int => String]))))
       (is (= [[['String] 'String] [['String 'types/Int] 'String]]
             (fns/parse-args+ '([String => String] [String types/Int => String])))))))

(deftest t-fn-fn-checking
  ;; testing higher-order arguments and return types
  (testing "fns are properly annotated"
    (is (= [[`String `types/Int]]
          (-> (fns/fnv [String => types/Int] [_])
            meta :annotate.types/contract)))
    (is (= [[`String `types/Int] [`String `types/Int `String]]
          (-> (fns/fnv ([String => types/Int] [String types/Int => String]) ([_]) ([_ _]))
            meta :annotate.types/contract))))
  ;;(fnv [])
  ;; annotated input & output values are checked
  ;; not annotated input & output values are *not* checked
  )
