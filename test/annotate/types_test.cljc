(ns annotate.types-test
  (:require [annotate.core :refer [display-type check valid-type? display-canonical]]
            [annotate.util :refer [truncate]]
            [annotate.types
             :refer [U I fixed-key? optional-key required-key Eq Symbol
                     Date Keyword Var NonEmpty Empty Vec Option Nilable Count Member
                     Coll Num Int Seq LazySeq Seqable NilableColl CanSeq SortedMap KwA
                     Named Pairs Subset ExMsg #?@(:cljs [String Number]) Date Atom Regex]]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :as test :refer-macros [deftest testing is]]))
  #?(:cljs (:require-macros [annotate.types-macro :refer [Pred IFn]])
     :clj (:require [annotate.types-macro :refer [Pred IFn]]))
  #?(:clj (:import (annotate.types PredicateType IFnType) ;;needed by the Pred macro. can't skip it?
                   )))

(defn core-sym [sym]
  #?(:clj (symbol (str "clojure.lang." (name sym)))
     :cljs (symbol "cljs.core" (name sym))))

(deftest t-Pred
  (is (= (list 'Pred 'even?) (display-type (Pred even?))))
  (is (= nil (check (Pred even?) 2)))
  (is (= (list 'not (list 'even? 3))) (check (Pred even?) 3))
  (is (= (list 'not (list (list 'fn ['x] (list '> 'x 0)) -1))
        (check (Pred (fn [x] (> x 0))) -1)))
  (is (= (list 'not (list 'annotate.core/valid-type? (list 'Pred :age)))
        (check (Pred :age) {:age 36}))))

(deftest t-canonical-primitives
  (testing "clojure primitives"
    (testing "the Keyword type"
      (is (= 'Keyword (display-type Keyword)))
      (is (= (core-sym 'Keyword) (display-canonical Keyword))))
    (testing "Keyword instances"
      (is (= :test (display-type :test)))
      (is (= :test (display-canonical :test)))
      (is (= nil (check Keyword :test)))
      (is (= (list 'not (list 'instance? 'Keyword 123)) (check Keyword 123))))

    (testing "the Symbol type"
      (is (= 'Symbol (display-type Symbol)))
      (is (= (core-sym 'Symbol) (display-canonical Symbol))))
    (testing "Symbol instances"
      (is (= 'xx (display-type 'xx)))
      ;; ' or ` does not matter, it should return self, but the default
      (is (= 'xx (display-canonical 'xx)))
      (is (= nil (check Symbol 'a-symbol)))
      (is (= nil (check 'xx 'xx)))
      (is (= (list 'not= 'xx :xx) (check 'xx :xx)))
      (is (= (list 'not (list 'instance? 'Symbol :xx)) (check Symbol :xx))))

    (testing "Var"
      (is (= '(not (instance? Var nil)) (check Var nil)))
      (is (= 'symbol? (display-type (var symbol?))))
      ;; (check Var) delegates to it's value
      (is (= nil (check #?(:clj (clojure.lang.Var/create "txt")
                           ;; TODO wtf, why do I have to wrap the val?
                           :cljs (cljs.core/->Var #(identity "txt") (gensym) {})) "txt"))))

    (is (= nil (check Atom (atom nil)))))

  (testing "host primitives"

    (is (= 'String (display-type String)))
    (is (= nil (check String "string")))
    (is (= nil (check "x" "x")))

    (is (= nil (check Date #?(:cljs (js/Date.) :clj (java.util.Date.)))))

    (is (= nil (check Number 123)))
    (is (nil? (check Int 50)))
    ;; js fun :)
    #?(:cljs (is (nil? (check Int 50.0))))
    (is (some? (check Int 50.1)))))

(deftest t-fixed-key?
  (is (= true (fixed-key? :hi)))
  (is (= true (fixed-key? (quote hi))))
  (is (= true (fixed-key? "hi")))
  (is (= true (fixed-key? (optional-key :a))))
  (is (= true (fixed-key? (required-key :a))))
  (is (= false (fixed-key? String))))

(deftest t-Union
  ;; TODO
  (is (= (list 'U 'String 'Number) (display-type (U String Number))))
  (is (= (list 'not (list 'annotate.core/valid-type? (list 'U))) (check (U) "Billy")))
  ;; TODO
  (is (= nil (check (U (Pred list?) (Pred seq?)) (list 1))))
  (is (= (list
           'and
           (list 'not (list 'string? (list 1)))
           (list 'not (list 'integer? (list 1))))
        (check (U (Pred string?) (Pred integer?)) (list 1))))
  ;; TODO numbers are not boxed in js
  (is (= nil) (check (U nil 3 String) 3)))

(deftest t-Intersection
  ;; TODO
  (is (= (list 'I 'Number (list 'Pred 'even?))
        (display-type (I Number (Pred even?)))))
  (is (= (list 'not (list 'annotate.core/valid-type? (list 'I))) (check (I) 39)))
  (is (= nil (check (I (Pred list?) (Pred seq?)) (quote (1)))))
  (is (= (list 'not (list 'empty? (list 1)))
        (check (I (Pred list?) (Pred empty?)) (quote (1))))))

(deftest t-Eq
  (is (= (list 'Eq [true]) (display-type (Eq [true]))))
  (is (= nil (check (Eq [:success]) [:success])))
  (is (= (list 'not= [true] [true true])) (check (Eq [true]) [true true])))

(deftest t-Maps
  ;; TODO cljs.core things are not auto imported or? why is ' not working on Keyword
  (is (= {'Keyword 'String} (display-type {Keyword String})))
  (is (= {:age 'Int, :name 'String} (display-type {:name String, :age Int})))
  (is (= (list 'not (list 'map? #{})) (check {Keyword String} #{})))
  (is (= nil (check {Keyword String} {})))
  (is (= {:name (list 'not (list 'instance? 'String :billy))}
        (check {Keyword String} {:name :billy})))
  (is (= {(list 'not (list 'instance? 'Keyword "name")) "billy"}
        (check {Keyword String} {"name" "billy"})))
  (is (= {:name 'key-not-found} (check {:name String} {:age 36})))

  (is (= nil (check {:name "David", :age Int} {:age 36, :name "David"})))

  (is (= {:age (list 'not (list 'integer? :36)), :name 'key-not-found}
          (check {:name "David", :age Int} {:age :36, :nam "David", "a" :1})))

  (is (= nil (check {Keyword String} {:name "David", :age "36"})))
  (is (= {:a 'key-not-found} (check {(required-key :a) String} {:b "hi"})))
  (is (= nil (check {(required-key :a) String} {:a "hi"})))
  (is (= nil (check {(required-key :a) String} {:a "hi", :b "there"})))
  (is (= nil (check {(optional-key :a) String} {:b "hi"})))
  (is (= nil (check {(optional-key :a) String} {:b "hi", :a "there"})))
  (is (= (list 'not (list 'annotate.core/valid-type? {:name 'String, 'String 'String}))
        (check {:name String, String String} {:name "Bob"})))
  (is (= nil (check {} {})))
  (is (= (list 'not (list 'empty? {:name "David"}))) (check {} {:name "David"})))

(deftest t-Vectors
  (is (= ['String] (display-type [String])))
  (is (= nil (check [] [])))
  (is (= (list 'not (list 'empty? [:hi])) (check [] [:hi])))
  (is (= (list 'not (list 'vector? #{})) (check [] #{})))
  (is (= nil (check [String] [])))
  (is (= nil (check [String] ["hi"])))
  (is (= [(list 'not (list 'instance? 'String :hi))] (check [String] [:hi])))
  (is (= nil (check [String] ["hi" "there" "Billy"])))
  (is (= [nil (list 'not (list 'instance? 'String :there)) nil]
        (check [String] ["hi" :there "Billy"])))
  (is (= nil (check [[Keyword String]] [[:name "Billy"] [:name "Joey"]])))
  (is (= [nil [nil (list 'not (list 'instance? 'String :Joey))]]
        (check [[Keyword String]] [[:name "Billy"] [:name :Joey]]))))

(deftest t-Lists
  (is (= (list 'String) (display-type (list String))))
  (is (= nil (check (list) (list))))
  (is (= (list 'not (list 'empty? (list 1))) (check (list) (list 1))))
  (is (= (list 'not (list 'list? #{})) (check (list) #{})))
  (is (= nil (check (list String) (list))))
  (is (= nil (check (list String) (list "hi"))))
  (is (= (list (list 'not (list 'instance? 'String :hi)))
        (check (list String) (list :hi))))
  (is (= nil (check (list String) (list "hi" "there" "Billy"))))
  (is (= (list nil (list 'not (list 'instance? 'String :there)) nil)
        (check (list String) (list "hi" :there "Billy")))))

(deftest t-Sets
  (is (= #{'String} (display-type #{String})))
  (is (= nil (check #{} #{})))
  (is (= (list 'not (list 'empty? #{:hi})) (check #{} #{:hi})))
  (is (= (list 'not (list 'set? [])) (check #{} [])))
  (is (= #{(list 'not (list 'instance? 'String :hi))}
       (check #{String} #{:hi})))
  (is (= #{nil (list 'not (list 'instance? 'String :there))}
        (check #{String} #{"hi" :there})))
  (is (= (list 'not (list 'annotate.core/valid-type? #{'Keyword 'String}))
        (check #{String Keyword} #{:hi}))))

(deftest t-Regular-Expressions
  #?(:cljs (is (= "abc" (.-source (display-type #"abc")))))
  (is (= nil (check #"[a-z]+" "hi")))
  (is (some? (check #"[a-z]+" "hi3")))
  (is (nil? (check Regex #"a")))
  (is (some? (check Regex "a"))))

(deftest t-Values
  (is (= nil (display-type nil)))
  ;; phew, this heavily relies on autoboxing

  (is (= 3 (display-type 3)))
  ;; TODO do this for Strings as well, it will fail
  (is (= nil (check nil nil)))
  (is (= nil) (check 3 3)))

(deftest t-IFn
  (is (= nil (check (IFn [String => String]) (fn [x] (str "Hello, " x)))))
  (is (= nil
          (check (IFn [=> String] [String => String])
            (fn ([] "Hello, world") ([x] (str "Hello, " x))))))
  (is (= (list 'not (list 'ifn? 39))) (check (IFn [String => String]) 39)))

(deftest t-NonEmpty
  (is (= (list 'NonEmpty ['String]) (display-type (NonEmpty [String]))))
  (is (= nil (check (NonEmpty) ["Billy" "Bobby"])))
  (is (= nil (check (NonEmpty [String]) ["Billy" "Bobby"])))
  (is (= (list 'not (list 'seq []))) (check (NonEmpty) [])))

(deftest t-Empty
  (is (= (list 'Empty 'Vec) (display-type (Empty Vec))))
  (is (= nil (check (Empty) [])))
  (is (= nil (check (Empty Vec) [])))
  (is (= (list 'not (list 'empty? [1 2 3]))
        (check (Empty) [1 2 3]))))

(deftest t-Option
  (is (= (list 'Option 'String) (display-type (Option String))))
  (is (= nil (check (Option String) "hi")))
  (is (= nil (check (Option String) nil)))
  (is (= (list
          'and
          (list 'not (list 'instance? 'String :hi))
          (list 'not (list 'nil? :hi)))
        (check (Option String) :hi))))

(deftest t-Nilable
  (is (= (list 'Nilable 'String) (display-type (Nilable String))))
  (is (= nil (check (Nilable String) "hi")))
  (is (= nil (check (Nilable String) nil)))
  (is (= (list
          'and
          (list 'not (list 'instance? 'String :hi))
          (list 'not (list 'nil? :hi)))
        (check (Nilable String) :hi))))

(deftest t-Count
  (is (= (list 'Count 5) (display-type (Count 5))))
  (is (= (list 'Count 1 5) (display-type (Count 1 5))))
  (is (= nil (check (Count 5) (range 5))))
  (is (= nil (check (Count 1 5) "Billy")))
  (is (= (list '< (list 'count []) 2) (check (Count 2 5) [])))
  (is (= (list '< (list 'count [1]) 2) (check (Count 2 5) [1])))
  (is (= (list '> (list 'count [1 2 3 4 5 '...]) 5)
        (check (Count 2 5) [1 2 3 4 5 6])))
  (is (= (list '> (list 'count (list 0 1 2 3 4 '...)) 5)
        (check (Count 2 5) (range 10)))))

(deftest t-Member
  (is (= (list 'Member 'String) (display-type (Member String))))
  (is (= nil (check (Member String) ["Billy" "Bobby"])))
  (is (= nil (check (Member String) [])))
  (is (= (list
          (list 'not (list 'instance? 'String :billy))
          nil
          (list 'not (list 'instance? 'String :bobby)))
        (check (Member String) [:billy "joey" :bobby]))))

(deftest t-Coll
  (is (= (list 'Coll 'Int) (display-type (Coll Int))))
  (is (= nil (check (Coll String) ["Billy" "Bobby"])))
  (is (= nil (check (Coll String) (list "Billy" "Bobby"))))
  (is (= (list 'not (list 'coll? 3)) (check (Coll Int) 3)))
  (is (= (list 'not (list 'coll? nil))) (check (Coll Int) nil)))

(deftest t-Seq
  (is (= (list 'Seq 'Int) (display-type (Seq Int))))
  (is (= nil (check (Seq) (range 5))))
  (is (= nil (check (Seq Int) (range 5))))
  (is (= nil (check (Seq) (list 1 2 3))))
  (is (= (list
          (list 'not (list 'instance? 'String 0))
          (list 'not (list 'instance? 'String 1))
          (list 'not (list 'instance? 'String 2))
          (list 'not (list 'instance? 'String 3))
          (list 'not (list 'instance? 'String 4)))
        (check (Seq String) (range 5))))
  (is (= (list 'not (list 'seq? []))) (check (Seq) [])))

(deftest t-LazySeq
  (is (= (list 'LazySeq 'Int) (display-type (LazySeq Int))))
  (is (= nil (check (LazySeq) (map inc [1 2 3]))))
  (is (= nil (check (LazySeq Int) (map inc [1 2 3]))))
  (is (= (list 'not (list 'instance? 'LazySeq (list 1 2 3)))
        (check (LazySeq) (list 1 2 3))))
  (is (= (list
          (list 'not (list 'instance? 'String 2))
          (list 'not (list 'instance? 'String 3))
          (list 'not (list 'instance? 'String 4)))
        (check (LazySeq String) (map inc [1 2 3]))))
  (is (= (list 'not (list 'instance? 'LazySeq []))) (check (LazySeq) [])))

(deftest t-Seqable
  (is (= (list 'Seqable 'Int) (display-type (Seqable Int))))
  (is (= nil (check (Seqable) [1 2 3])))
  (is (= nil (check (Seqable Int) [1 2 3])))
  (is (= nil (check (Seqable Int) (list 1 2 3))))
  (is (= nil (check (Seqable Int) (range 5))))
  (is (= nil (check (Seqable Int) #{1 3 2})))
  (is (= nil (check (Seqable [String Int]) {"a" 1, "b" 2})))
  (is (= (list 'not (list 'instance? 'Seqable "hello"))
        (check (Seqable) "hello")))
  (is (= (list 'not (list 'instance? 'Seqable nil)) (check (Seqable) nil)))
  (is (= (list 'not (list 'instance? 'Seqable 3))) (check (Seqable) 3)))

(deftest t-NilableColl
  (is (= (list 'Nilable (list 'Coll 'Int)) (display-type (NilableColl Int))))
  (is (= nil (check (NilableColl) [1 2 3])))
  (is (= nil (check (NilableColl Int) [1 2 3])))
  (is (= nil (check (NilableColl) nil)))
  (is (= nil) (check (NilableColl Int) nil)))

(deftest t-CanSeq
  (is (= (list 'CanSeq 'Int) (display-type (CanSeq Int))))
  (is (= nil (check (CanSeq Int) [1 2 3])))
  (is (= nil (check (CanSeq) (list 1 2 3))))
  (is (= nil (check (CanSeq String) nil)))
  (is (= nil (check (CanSeq) "hello")))
  #_(is (= (list
           'and
           (list 'not (list 'nil? 42))
           (list 'not (list 'instance? 'Seqable 42))
           (list 'not (list 'instance? 'String 42))
           (list 'not (list 'instance? 'Iterable 42))
           (list 'not (list 'instance? 'Map 42))
           (list 'not (list 'annotate.util/array? 42)))
        (check (CanSeq) 42))))

(deftest t-SortedMap
  (is (= (list 'SortedMap ['Keyword 'String])
        (display-type (SortedMap Keyword String))))
  (is (= nil (check (SortedMap Keyword String) (sorted-map :a "1"))))
  (is (= (list [(list 'not (list 'instance? 'Keyword 'a)) nil])
        (check (SortedMap Keyword String) (sorted-map (quote a) "1")))))

(deftest t-KwA
  (is (= (list 'KwA :method 'Named :timeout 'Int)
        (display-type (KwA :method Named :timeout Int))))
  (is (= nil (check (KwA :method Named :timeout Int) {:method :POST})))
  (is (= nil
        (check (KwA :method Named :timeout Int) {:timeout 100, :method :POST})))
  (is (= nil
        (check (KwA :method Named :timeout Int) {:method "DELETE", :redirects 3})))
  (is (= nil (check (KwA :method Named :timeout Int) {})))
  (is (= nil (check (KwA :method Named :timeout Int) nil)))
  (is (= (list
          'and
          {:timeout (list 'not (list 'integer? 50.1))}
          (list 'not (list 'nil? {:timeout 50.1})))
        (check (KwA :method Named :timeout Int) {:timeout 50.1}))))

(deftest t-Pairs
  (is (= (list 'Pairs :method 'Named :timeout 'Int)
       (display-type (Pairs :method Named :timeout Int))))
  (is (= nil
        (check (Pairs :method Named :timeout Int) (list :timeout 50 :method :POST))))
  (is (= nil
        (check (Pairs :method Named :timeout Int) (list :timeout 50 :method "POST" :redirects 3))))
  (is (= {:timeout (list 'not (list 'integer? "50"))}
        (check (Pairs :method Named :timeout Int) (list :timeout "50"))))
  (is (= nil) (check (Pairs :method Named :timeout Int) (list))))

(deftest t-Subset
  (is (= (list 'Subset #{:warn :error}) (display-type (Subset #{:warn :error}))))
  (is (= nil (check (Subset #{:warn :error}) #{:warn :error})))
  (is (= nil (check (Subset #{:warn :error}) #{:error})))
  (is (= nil (check (Subset #{:warn :error}) #{})))
  (is (= (list 'not (list 'clojure.set/subset? #{:fatal :error} #{:warn :error}))
        (check (Subset #{:warn :error}) #{:fatal :error}))))

;; TODO
#?(:clj
   (deftest t-ExMsg
     (is (= nil (check (ExMsg "Fail!") (Exception. "Fail!"))))
     (is (= nil (check (ExMsg (Option String)) (Exception.))))
     (is (= nil (check (ExMsg #"\d+") (Exception. "1234"))))
     (is (= (list 'not (list 'instance? 'Exception :fail))
           (check (ExMsg String) :fail)))))
