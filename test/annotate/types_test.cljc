(ns annotate.types-test
  (:use [annotate core types]
        ;;midje.sweet
        clojure.test
        ))

(deftest t-Pred
  (is (= (display-type (Pred even?)) (list 'Pred 'even?)))
  (is (= nil (check (Pred even?) 2)))
  (is (= (list 'not (list 'even? 3))) (check (Pred even?) 3))
  (is (= (list 'not (list (list 'fn ['x] (list '> 'x 0)) -1))
        (check (Pred (fn [x] (> x 0))) -1)))
  (is (= (list 'not (list 'annotate.core/valid-type? (list 'Pred :age)))
        (check (Pred :age) {:age 36}))))

(deftest t-fixed-key?
  (is (= true (fixed-key? :hi)))
  (is (= true (fixed-key? (quote hi))))
  (is (= true (fixed-key? "hi")))
  (is (= true (fixed-key? (optional-key :a))))
  (is (= true (fixed-key? (required-key :a))))
  (is (= false (fixed-key? String))))

(deftest t-Union
  (is (= (list 'U 'String 'Number) (display-type (U String Number))))
  (is (= (list 'not (list 'annotate.core/valid-type? (list 'U))) (check (U) "Billy")))
  (is (= nil (check (U (Pred list?) (Pred class?)) (list 1))))
  (is (= (list
          'and
          (list 'not (list 'vector? (list 1)))
          (list 'not (list 'class? (list 1))))
        (check (U (Pred vector?) (Pred class?)) (list 1))))
  (is (= nil) (check (U nil 3 String) 3)))

(deftest t-Intersection
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
  (is (= {'Keyword 'String} (display-type {Keyword String})))
  (is (= {:age 'Long, :name 'String} (display-type {:name String, :age Long})))
  (is (= (list 'not (list 'map? #{})) (check {Keyword String} #{})))
  (is (= nil (check {Keyword String} {})))
  (is (= {:name (list 'not (list 'instance? 'String :billy))}
        (check {Keyword String} {:name :billy})))
  (is (= {(list 'not (list 'instance? 'Keyword "name")) "billy"}
        (check {Keyword String} {"name" "billy"})))
  (is (= {:name 'key-not-found} (check {:name String} {:age 36})))
  (is (= nil (check {:name "David", :age Long} {:age 36, :name "David"})))
  (is (= {:age (list 'not (list 'instance? 'Long :36)), :name 'key-not-found}
        (check {:name "David", :age Long} {:age :36, :nam "David", "a" :1})))
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
  (is (= nil (check #"[a-z]+" "hi")))
  (is (= (comp not nil?)) (check #"[a-z]+" "hi3")))

(deftest t-Values
  (is (= nil (display-type nil)))
  (is (= 3 (display-type 3)))
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
  (is (= (list
           'and
           (list 'not (list 'instance? 'Seqable 42))
           (list 'not (list 'nil? 42))
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
          {:timeout (list 'not (list 'integer? 50.0))}
          (list 'not (list 'nil? {:timeout 50.0})))
        (check (KwA :method Named :timeout Int) {:timeout 50.0}))))

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

(deftest t-ExMsg
  (is (= nil (check (ExMsg "Fail!") (Exception. "Fail!"))))
  (is (= nil (check (ExMsg (Option String)) (Exception.))))
  (is (= nil (check (ExMsg #"\d+") (Exception. "1234"))))
  (is (= (list 'not (list 'instance? 'Exception :fail))
        (check (ExMsg String) :fail))))
