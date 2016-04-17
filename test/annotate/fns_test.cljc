(ns annotate.fns-test
  (:require [annotate.types :as types #?@(:cljs [:refer [String]])]
            [annotate.fns :as fns :include-macros true]
            [annotate.core :as core]
            #?(:clj [clojure.test :as test :refer :all]
               :cljs [cljs.test :as test :refer-macros [deftest testing is]])))

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

(defmethod test/assert-expr 'valid? [msg form]
  `(let [res# (core/check ~(nth form 1) ~(nth form 2))]
     (if (nil? res#)
       (test/do-report {:type :pass})
       (test/do-report {:type :fail :message "contract check failed"
                        :expected 'nil :actual res#}))))

(deftest t-a
  (is (valid? nil nil)))

#?(:clj
   (do
     (deftest t-parse-fn-decl*
       (testing "anonymous fn"
         (is (thrown? java.lang.AssertionError
               (fns/parse-fn-decl* '(:fn "alabala" []))))
         (is (thrown? AssertionError
               (fns/parse-fn-decl* '(:fn [] true))))
         (is (thrown? AssertionError
               (fns/parse-fn-decl* '(:fn [] [] true))))

         ;; in order to have lower maint of tests, the params contract
         ;; is parsed later. don't want to rewrite all tests once I
         ;; change how I represent contracts
         ;; keep data raw as long as possible, compute as late as possible
         (is (valid? {:name 'a-name :anonymous true
                      :bodies [{:contract ['_ '_ '_ '=> '_]
                                :params ['x 'y '& 'z]
                                :body '(xyz)}]}
               (fns/parse-fn-decl* '(:fn a-name [_ _ _ => _] [x y & z] xyz))))

         (is (valid? {:anonymous true
                      :bodies [{:contract ['=>] :params [] :body '(nil)}]}
               (fns/parse-fn-decl* '(:fn [=>] [] nil))))

         (is (valid? {:bodies [{:body '((+ 1 2) true)}]}
               (fns/parse-fn-decl* '(:fn [=>] [] (+ 1 2) true))))

         (is (valid? {:anonymous true
                      :bodies [{:contract '[=>] :params ['x 'y '& 'z] :body '(xyz)}
                               {:contract '[=>] :params [] :body '()}]}
               (fns/parse-fn-decl* '(:fn a-name ([ => ] [x y & z] xyz) ([=>] []))))))

       ;; TODO check that key is NOT present
       ;;(is (nil? (core/check {:anonymous nil} {})))
       (testing "defn"
         (is (thrown-with-msg? AssertionError #"Assert failed: defn needs a name"
               (fns/parse-fn-decl* '(:defn [=> String] [_]))))

         (is (valid? (types/I (types/Pred #(nil? (:anonymous %)))
                       {:name 'a-name :doc-string "some-doc"
                        :bodies [{:contract ['=> 'String] :params '[_] :body '(xyz)}]})
               (fns/parse-fn-decl* '(:defn a-name "some-doc" ^{} [=> String] ^{}
                                           [_] {:pre [true]} xyz))))
         ;; NOTE multiarity syntax is different from the original
         (is (valid? {:name 'name
                      :bodies [{:params ['x]} {:params ['_ '_]}]}
               (fns/parse-fn-decl* '(:defn name "blaa" ([=>] [x]) ([=>] [_ _])))))
         ))

     (deftest t-parse-arg
       (testing "single arity"
         (is (= [['String] 'String] (fns/parse-args+ '[String => String])))
         (is (= [['String 'types/Int] 'String] (fns/parse-args+ '[String types/Int => String])))
         (is (= [[] 'types/Int] (fns/parse-args+ '[=> types/Int])))
         (is (= [["test"] 'Boolean] (fns/parse-args+ '["test" => Boolean])))
         (is (thrown? java.lang.AssertionError (fns/parse-args+ '[String =>])))
         (is (thrown? java.lang.AssertionError (fns/parse-args+ '[String types/Int])))
         (is (thrown? java.lang.AssertionError (fns/parse-args+ '[=>]))))
       )

     (deftest t-defn
       ;; TODO varargs?????????????
       )
     ))

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
