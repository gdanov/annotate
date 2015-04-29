(ns annotate.types-test
  (:use [annotate core types] midje.sweet))

(facts "Pred"
  (fact (display-type (Pred even?)) => (list 'Pred 'even?))
  (fact (check (Pred even?) 2) => nil)
  (fact (check (Pred even?) 3) => (list 'not (list 'even? 3)))
  (fact (check (Pred (fn [x] (> x 0))) -1) =>
    (list 'not (list (list 'fn ['x] (list '> 'x 0)) -1)))
  (fact (check (Pred :age) {:age 36}) =>
    (list 'not (list 'annotate.core/valid-type? (list 'Pred :age)))))
(facts "fixed-key?"
  (fact (fixed-key? :hi) => true)
  (fact (fixed-key? (quote hi)) => true)
  (fact (fixed-key? "hi") => true)
  (fact (fixed-key? (optional-key :a)) => true)
  (fact (fixed-key? (required-key :a)) => true)
  (fact (fixed-key? String) => false))
(facts "Union"
  (fact (display-type (U String Number)) => (list 'U 'String 'Number))
  (fact (check (U) "Billy") => (list 'not (list 'annotate.core/valid-type? (list 'U))))
  (fact (check (U (Pred list?) (Pred class?)) (list 1)) => nil)
  (fact (check (U (Pred vector?) (Pred class?)) (list 1)) =>
    (list
     'and
     (list 'not (list 'vector? (list 1)))
     (list 'not (list 'class? (list 1)))))
  (fact (check (U nil 3 String) 3) => nil))
(facts "Intersection"
  (fact (display-type (I Number (Pred even?))) =>
    (list 'I 'Number (list 'Pred 'even?)))
  (fact (check (I) 39) => (list 'not (list 'annotate.core/valid-type? (list 'I))))
  (fact (check (I (Pred list?) (Pred seq?)) (quote (1))) => nil)
  (fact (check (I (Pred list?) (Pred empty?)) (quote (1))) =>
    (list 'not (list 'empty? (list 1)))))
(facts "Eq"
  (fact (display-type (Eq [true])) => (list 'Eq [true]))
  (fact (check (Eq [:success]) [:success]) => nil)
  (fact (check (Eq [true]) [true true]) => (list 'not= [true] [true true])))
(facts "Maps"
  (fact (display-type {Keyword String}) => {'Keyword 'String})
  (fact (display-type {:name String, :age Long}) => {:age 'Long, :name 'String})
  (fact (check {Keyword String} #{}) => (list 'not (list 'map? #{})))
  (fact (check {Keyword String} {}) => nil)
  (fact (check {Keyword String} {:name :billy}) =>
    {:name (list 'not (list 'instance? 'String :billy))})
  (fact (check {Keyword String} {"name" "billy"}) =>
    {(list 'not (list 'instance? 'Keyword "name")) "billy"})
  (fact (check {:name String} {:age 36}) => {:name 'key-not-found})
  (fact (check {:name "David", :age Long} {:age 36, :name "David"}) => nil)
  (fact (check {:name "David", :age Long} {:age :36, :nam "David", "a" :1}) =>
    {:age (list 'not (list 'instance? 'Long :36)), :name 'key-not-found})
  (fact (check {Keyword String} {:name "David", :age "36"}) => nil)
  (fact (check {(required-key :a) String} {:b "hi"}) => {:a 'key-not-found})
  (fact (check {(required-key :a) String} {:a "hi"}) => nil)
  (fact (check {(required-key :a) String} {:a "hi", :b "there"}) => nil)
  (fact (check {(optional-key :a) String} {:b "hi"}) => nil)
  (fact (check {(optional-key :a) String} {:b "hi", :a "there"}) => nil)
  (fact (check {:name String, String String} {:name "Bob"}) =>
        (list 'not (list 'annotate.core/valid-type? {:name 'String, 'String 'String})))
  (fact (check {} {}) => nil)
  (fact (check {} {:name "David"}) => (list 'not (list 'empty? {:name "David"}))))
(facts "Vectors"
  (fact (display-type [String]) => ['String])
  (fact (check [] []) => nil)
  (fact (check [] [:hi]) => (list 'not (list 'empty? [:hi])))
  (fact (check [] #{}) => (list 'not (list 'vector? #{})))
  (fact (check [String] []) => nil)
  (fact (check [String] ["hi"]) => nil)
  (fact (check [String] [:hi]) => [(list 'not (list 'instance? 'String :hi))])
  (fact (check [String] ["hi" "there" "Billy"]) => nil)
  (fact (check [String] ["hi" :there "Billy"]) =>
    [nil (list 'not (list 'instance? 'String :there)) nil])
  (fact (check [[Keyword String]] [[:name "Billy"] [:name "Joey"]]) => nil)
  (fact (check [[Keyword String]] [[:name "Billy"] [:name :Joey]]) =>
    [nil [nil (list 'not (list 'instance? 'String :Joey))]]))
(facts "Lists"
  (fact (display-type (list String)) => (list 'String))
  (fact (check (list) (list)) => nil)
  (fact (check (list) (list 1)) => (list 'not (list 'empty? (list 1))))
  (fact (check (list) #{}) => (list 'not (list 'list? #{})))
  (fact (check (list String) (list)) => nil)
  (fact (check (list String) (list "hi")) => nil)
  (fact (check (list String) (list :hi)) =>
    (list (list 'not (list 'instance? 'String :hi))))
  (fact (check (list String) (list "hi" "there" "Billy")) => nil)
  (fact (check (list String) (list "hi" :there "Billy")) =>
    (list nil (list 'not (list 'instance? 'String :there)) nil)))
(facts "Sets"
  (fact (display-type #{String}) => #{'String})
  (fact (check #{} #{}) => nil)
  (fact (check #{} #{:hi}) => (list 'not (list 'empty? #{:hi})))
  (fact (check #{} []) => (list 'not (list 'set? [])))
  (fact (check #{String} #{:hi}) =>
    #{(list 'not (list 'instance? 'String :hi))})
  (fact (check #{String} #{"hi" :there}) =>
    #{nil (list 'not (list 'instance? 'String :there))})
  (fact (check #{String Keyword} #{:hi}) =>
    (list 'not (list 'annotate.core/valid-type? #{'Keyword 'String}))))
(facts "Regular Expressions"
  (fact (check #"[a-z]+" "hi") => nil)
  (fact (check #"[a-z]+" "hi3") => (comp not nil?)))
(facts "Values"
  (fact (display-type nil) => nil)
  (fact (display-type 3) => 3)
  (fact (check nil nil) => nil)
  (fact (check 3 3) => nil))
(facts "IFn"
  (fact (check (IFn [String => String]) (fn [x] (str "Hello, " x))) => nil)
  (fact (check (IFn [=> String] [String => String]) (fn ([] "Hello, world") ([x] (str "Hello, " x)))) =>
    nil)
  (fact (check (IFn [String => String]) 39) => (list 'not (list 'ifn? 39))))
(facts "NonEmpty"
  (fact (display-type (NonEmpty [String])) => (list 'NonEmpty ['String]))
  (fact (check (NonEmpty) ["Billy" "Bobby"]) => nil)
  (fact (check (NonEmpty [String]) ["Billy" "Bobby"]) => nil)
  (fact (check (NonEmpty) []) => (list 'not (list 'seq []))))
(facts "Empty"
  (fact (display-type (Empty Vec)) => (list 'Empty 'Vec))
  (fact (check (Empty) []) => nil)
  (fact (check (Empty Vec) []) => nil)
  (fact (check (Empty) [1 2 3]) =>
    (list 'not (list 'empty? [1 2 3]))))
(facts "Option"
  (fact (display-type (Option String)) => (list 'Option 'String))
  (fact (check (Option String) "hi") => nil)
  (fact (check (Option String) nil) => nil)
  (fact (check (Option String) :hi) =>
    (list
     'and
     (list 'not (list 'instance? 'String :hi))
     (list 'not (list 'nil? :hi)))))
(facts "Nilable"
  (fact (display-type (Nilable String)) => (list 'Nilable 'String))
  (fact (check (Nilable String) "hi") => nil)
  (fact (check (Nilable String) nil) => nil)
  (fact (check (Nilable String) :hi) =>
    (list
     'and
     (list 'not (list 'instance? 'String :hi))
     (list 'not (list 'nil? :hi)))))
(facts "Count"
  (fact (display-type (Count 5)) => (list 'Count 5))
  (fact (display-type (Count 1 5)) => (list 'Count 1 5))
  (fact (check (Count 5) (range 5)) => nil)
  (fact (check (Count 1 5) "Billy") => nil)
  (fact (check (Count 2 5) []) => (list '< (list 'count []) 2))
  (fact (check (Count 2 5) [1]) => (list '< (list 'count [1]) 2))
  (fact (check (Count 2 5) [1 2 3 4 5 6]) =>
    (list '> (list 'count [1 2 3 4 5 '...]) 5))
  (fact (check (Count 2 5) (range 10)) =>
    (list '> (list 'count (list 0 1 2 3 4 '...)) 5)))
(facts "Member"
  (fact (display-type (Member String)) => (list 'Member 'String))
  (fact (check (Member String) ["Billy" "Bobby"]) => nil)
  (fact (check (Member String) []) => nil)
  (fact (check (Member String) [:billy "joey" :bobby]) =>
    (list
     (list 'not (list 'instance? 'String :billy))
     nil
     (list 'not (list 'instance? 'String :bobby)))))
(facts "Coll"
  (fact (display-type (Coll Int)) => (list 'Coll 'Int))
  (fact (check (Coll String) ["Billy" "Bobby"]) => nil)
  (fact (check (Coll String) (list "Billy" "Bobby")) => nil)
  (fact (check (Coll Int) 3) => (list 'not (list 'coll? 3)))
  (fact (check (Coll Int) nil) => (list 'not (list 'coll? nil))))
(facts "Seq"
  (fact (display-type (Seq Int)) => (list 'Seq 'Int))
  (fact (check (Seq) (range 5)) => nil)
  (fact (check (Seq Int) (range 5)) => nil)
  (fact (check (Seq) (list 1 2 3)) => nil)
  (fact (check (Seq String) (range 5)) =>
    (list
     (list 'not (list 'instance? 'String 0))
     (list 'not (list 'instance? 'String 1))
     (list 'not (list 'instance? 'String 2))
     (list 'not (list 'instance? 'String 3))
     (list 'not (list 'instance? 'String 4))))
  (fact (check (Seq) []) => (list 'not (list 'seq? []))))
(facts "LazySeq"
  (fact (display-type (LazySeq Int)) => (list 'LazySeq 'Int))
  (fact (check (LazySeq) (map inc [1 2 3])) => nil)
  (fact (check (LazySeq Int) (map inc [1 2 3])) => nil)
  (fact (check (LazySeq) (list 1 2 3)) =>
    (list 'not (list 'instance? 'LazySeq (list 1 2 3))))
  (fact (check (LazySeq String) (map inc [1 2 3])) =>
    (list
     (list 'not (list 'instance? 'String 2))
     (list 'not (list 'instance? 'String 3))
     (list 'not (list 'instance? 'String 4))))
  (fact (check (LazySeq) []) => (list 'not (list 'instance? 'LazySeq []))))
(facts "Seqable"
  (fact (display-type (Seqable Int)) => (list 'Seqable 'Int))
  (fact (check (Seqable) [1 2 3]) => nil)
  (fact (check (Seqable Int) [1 2 3]) => nil)
  (fact (check (Seqable Int) (list 1 2 3)) => nil)
  (fact (check (Seqable Int) (range 5)) => nil)
  (fact (check (Seqable Int) #{1 3 2}) => nil)
  (fact (check (Seqable [String Int]) {"a" 1, "b" 2}) => nil)
  (fact (check (Seqable) "hello") =>
    (list 'not (list 'instance? 'Seqable "hello")))
  (fact (check (Seqable) nil) => (list 'not (list 'instance? 'Seqable nil)))
  (fact (check (Seqable) 3) => (list 'not (list 'instance? 'Seqable 3))))
(facts "NilableColl"
  (fact (display-type (NilableColl Int)) => (list 'Nilable (list 'Coll 'Int)))
  (fact (check (NilableColl) [1 2 3]) => nil)
  (fact (check (NilableColl Int) [1 2 3]) => nil)
  (fact (check (NilableColl) nil) => nil)
  (fact (check (NilableColl Int) nil) => nil))
(facts "CanSeq"
  (fact (display-type (CanSeq Int)) => (list 'CanSeq 'Int))
  (fact (check (CanSeq Int) [1 2 3]) => nil)
  (fact (check (CanSeq) (list 1 2 3)) => nil)
  (fact (check (CanSeq String) nil) => nil)
  (fact (check (CanSeq) "hello") => nil)
  (fact (check (CanSeq) 42) =>
    (list
     'and
     (list 'not (list 'instance? 'Seqable 42))
     (list 'not (list 'nil? 42))
     (list 'not (list 'instance? 'String 42))
     (list 'not (list 'instance? 'Iterable 42))
     (list 'not (list 'instance? 'Map 42))
     (list 'not (list 'annotate.util/array? 42)))))
(facts "SortedMap"
  (fact (display-type (SortedMap Keyword String)) =>
    (list 'SortedMap ['Keyword 'String]))
  (fact (check (SortedMap Keyword String) (sorted-map :a "1")) => nil)
  (fact (check (SortedMap Keyword String) (sorted-map (quote a) "1")) =>
    (list [(list 'not (list 'instance? 'Keyword 'a)) nil])))
(facts "KwA"
  (fact (display-type (KwA :method Named :timeout Int)) =>
    (list 'KwA :method 'Named :timeout 'Int))
  (fact (check (KwA :method Named :timeout Int) {:method :POST}) => nil)
  (fact (check (KwA :method Named :timeout Int) {:timeout 100, :method :POST}) =>
    nil)
  (fact (check (KwA :method Named :timeout Int) {:method "DELETE", :redirects 3}) =>
    nil)
  (fact (check (KwA :method Named :timeout Int) {}) => nil)
  (fact (check (KwA :method Named :timeout Int) nil) => nil)
  (fact (check (KwA :method Named :timeout Int) {:timeout 50.0}) =>
    (list
     'and
     {:timeout (list 'not (list 'integer? 50.0))}
     (list 'not (list 'nil? {:timeout 50.0})))))
(facts "Pairs"
  (fact (display-type (Pairs :method Named :timeout Int)) =>
    (list 'Pairs :method 'Named :timeout 'Int))
  (fact (check (Pairs :method Named :timeout Int) (list :timeout 50 :method :POST)) =>
    nil)
  (fact (check (Pairs :method Named :timeout Int) (list :timeout 50 :method "POST" :redirects 3)) =>
    nil)
  (fact (check (Pairs :method Named :timeout Int) (list :timeout "50")) =>
    {:timeout (list 'not (list 'integer? "50"))})
  (fact (check (Pairs :method Named :timeout Int) (list)) => nil))
(facts "Subset"
  (fact (display-type (Subset #{:warn :error})) => (list 'Subset #{:warn :error}))
  (fact (check (Subset #{:warn :error}) #{:warn :error}) => nil)
  (fact (check (Subset #{:warn :error}) #{:error}) => nil)
  (fact (check (Subset #{:warn :error}) #{}) => nil)
  (fact (check (Subset #{:warn :error}) #{:fatal :error}) =>
    (list 'not (list 'clojure.set/subset? #{:fatal :error} #{:warn :error}))))
(facts "ExMsg"
  (fact (check (ExMsg "Fail!") (Exception. "Fail!")) => nil)
  (fact (check (ExMsg (Option String)) (Exception.)) => nil)
  (fact (check (ExMsg #"\d+") (Exception. "1234")) => nil)
  (fact (check (ExMsg String) :fail) =>
    (list 'not (list 'instance? 'Exception :fail))))
