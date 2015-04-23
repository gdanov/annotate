(ns annotate.types-test
  (:use [annotate core types] midje.sweet))

(facts "Pred"
  (fact (display-type (Pred even?)) => (list 'Pred 'even?))
  (fact (validate (Pred even?) 2) => nil)
  (fact (validate (Pred even?) 3) => (list 'not (list 'even? 3)))
  (fact (validate (Pred (fn [x] (> x 0))) -1) =>
    (list 'not (list (list 'fn ['x] (list '> 'x 0)) -1)))
  (fact (validate (Pred :age) {:age 36}) =>
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
  (fact (validate (U) "Billy") => (list 'not (list 'annotate.core/valid-type? (list 'U))))
  (fact (validate (U (Pred list?) (Pred class?)) (list 1)) => nil)
  (fact (validate (U (Pred vector?) (Pred class?)) (list 1)) =>
    (list
     'and
     (list 'not (list 'vector? (list 1)))
     (list 'not (list 'class? (list 1)))))
  (fact (validate (U nil 3 String) 3) => nil))
(facts "Intersection"
  (fact (display-type (I Number (Pred even?))) =>
    (list 'I 'Number (list 'Pred 'even?)))
  (fact (validate (I) 39) => (list 'not (list 'annotate.core/valid-type? (list 'I))))
  (fact (validate (I (Pred list?) (Pred seq?)) (quote (1))) => nil)
  (fact (validate (I (Pred list?) (Pred empty?)) (quote (1))) =>
    (list 'not (list 'empty? (list 1)))))
(facts "Eq"
  (fact (display-type (Eq [true])) => (list 'Eq [true]))
  (fact (validate (Eq [:success]) [:success]) => nil)
  (fact (validate (Eq [true]) [true true]) => (list 'not= [true] [true true])))
(facts "Maps"
  (fact (display-type {Keyword String}) => {'Keyword 'String})
  (fact (display-type {:name String, :age Long}) => {:age 'Long, :name 'String})
  (fact (validate {Keyword String} #{}) => (list 'not (list 'map? #{})))
  (fact (validate {Keyword String} {}) => nil)
  (fact (validate {Keyword String} {:name :billy}) =>
    {:name (list 'not (list 'instance? 'String :billy))})
  (fact (validate {Keyword String} {"name" "billy"}) =>
    {(list 'not (list 'instance? 'Keyword "name")) "billy"})
  (fact (validate {:name String} {:age 36}) => {:name 'key-not-found})
  (fact (validate {:name "David", :age Long} {:age 36, :name "David"}) => nil)
  (fact (validate {:name "David", :age Long} {:age :36, :nam "David", "a" :1}) =>
    {:age (list 'not (list 'instance? 'Long :36)), :name 'key-not-found})
  (fact (validate {Keyword String} {:name "David", :age "36"}) => nil)
  (fact (validate {(required-key :a) String} {:b "hi"}) => {:a 'key-not-found})
  (fact (validate {(required-key :a) String} {:a "hi"}) => nil)
  (fact (validate {(required-key :a) String} {:a "hi", :b "there"}) => nil)
  (fact (validate {(optional-key :a) String} {:b "hi"}) => nil)
  (fact (validate {(optional-key :a) String} {:b "hi", :a "there"}) => nil)
  (fact (validate {:name String, String String} {:name "Bob"}) =>
    (list 'not (list 'annotate.core/valid-type? {:name 'String, 'String 'String}))))
(facts "Vectors"
  (fact (display-type [String]) => ['String])
  (fact (validate [] []) => nil)
  (fact (validate [] #{}) => (list 'not (list 'vector? #{})))
  (fact (validate [String] []) => nil)
  (fact (validate [String] ["hi"]) => nil)
  (fact (validate [String] [:hi]) => [(list 'not (list 'instance? 'String :hi))])
  (fact (validate [String] ["hi" "there" "Billy"]) => nil)
  (fact (validate [String] ["hi" :there "Billy"]) =>
    [nil (list 'not (list 'instance? 'String :there)) nil])
  (fact (validate [[Keyword String]] [[:name "Billy"] [:name "Joey"]]) => nil)
  (fact (validate [[Keyword String]] [[:name "Billy"] [:name :Joey]]) =>
    [nil [nil (list 'not (list 'instance? 'String :Joey))]]))
(facts "Lists"
  (fact (display-type (list String)) => (list 'String))
  (fact (validate (list) (list)) => nil)
  (fact (validate (list) #{}) => (list 'not (list 'list? #{})))
  (fact (validate (list String) (list)) => nil)
  (fact (validate (list String) (list "hi")) => nil)
  (fact (validate (list String) (list :hi)) =>
    (list (list 'not (list 'instance? 'String :hi))))
  (fact (validate (list String) (list "hi" "there" "Billy")) => nil)
  (fact (validate (list String) (list "hi" :there "Billy")) =>
    (list nil (list 'not (list 'instance? 'String :there)) nil)))
(facts "Sets"
  (fact (display-type #{String}) => #{'String})
  (fact (validate #{} #{}) => nil)
  (fact (validate #{} #{:hi}) => (list 'not (list 'empty? #{:hi})))
  (fact (validate #{} []) => (list 'not (list 'set? [])))
  (fact (validate #{String} #{:hi}) =>
    #{(list 'not (list 'instance? 'String :hi))})
  (fact (validate #{String} #{"hi" :there}) =>
    #{nil (list 'not (list 'instance? 'String :there))})
  (fact (validate #{String Keyword} #{:hi}) =>
    (list 'not (list 'annotate.core/valid-type? #{'Keyword 'String}))))
(facts "Regular Expressions"
  (fact (validate #"[a-z]+" "hi") => nil)
  (fact (validate #"[a-z]+" "hi3") => (comp not nil?)))
(facts "Values"
  (fact (display-type nil) => nil)
  (fact (display-type 3) => 3)
  (fact (validate nil nil) => nil)
  (fact (validate 3 3) => nil))
(facts "IFn"
  (fact (validate (IFn [String => String]) (fn [x] (str "Hello, " x))) => nil)
  (fact (validate (IFn [=> String] [String => String]) (fn ([] "Hello, world") ([x] (str "Hello, " x)))) =>
    nil)
  (fact (validate (IFn [String => String]) 39) => (list 'not (list 'ifn? 39))))
(facts "NonEmpty"
  (fact (display-type (NonEmpty [String])) => (list 'NonEmpty ['String]))
  (fact (validate (NonEmpty) ["Billy" "Bobby"]) => nil)
  (fact (validate (NonEmpty [String]) ["Billy" "Bobby"]) => nil)
  (fact (validate (NonEmpty) []) => (list 'not (list 'seq []))))
(facts "Empty"
  (fact (display-type (Empty Vec)) => (list 'Empty 'Vec))
  (fact (validate (Empty) []) => nil)
  (fact (validate (Empty Vec) []) => nil)
  (fact (validate (Empty) [1 2 3]) =>
    (list 'not (list 'empty? [1 2 3]))))
(facts "Option"
  (fact (display-type (Option String)) => (list 'Option 'String))
  (fact (validate (Option String) "hi") => nil)
  (fact (validate (Option String) nil) => nil)
  (fact (validate (Option String) :hi) =>
    (list
     'and
     (list 'not (list 'instance? 'String :hi))
     (list 'not (list 'nil? :hi)))))
(facts "Nilable"
  (fact (display-type (Nilable String)) => (list 'Nilable 'String))
  (fact (validate (Nilable String) "hi") => nil)
  (fact (validate (Nilable String) nil) => nil)
  (fact (validate (Nilable String) :hi) =>
    (list
     'and
     (list 'not (list 'instance? 'String :hi))
     (list 'not (list 'nil? :hi)))))
(facts "Count"
  (fact (display-type (Count 5)) => (list 'Count 5))
  (fact (display-type (Count 1 5)) => (list 'Count 1 5))
  (fact (validate (Count 5) (range 5)) => nil)
  (fact (validate (Count 1 5) "Billy") => nil)
  (fact (validate (Count 2 5) []) => (list '< (list 'count []) 2))
  (fact (validate (Count 2 5) [1]) => (list '< (list 'count [1]) 2))
  (fact (validate (Count 2 5) [1 2 3 4 5 6]) =>
    (list '> (list 'count [1 2 3 4 5 '...]) 5))
  (fact (validate (Count 2 5) (range 10)) =>
    (list '> (list 'count (list 0 1 2 3 4 '...)) 5)))
(facts "Member"
  (fact (display-type (Member String)) => (list 'Member 'String))
  (fact (validate (Member String) ["Billy" "Bobby"]) => nil)
  (fact (validate (Member String) []) => nil)
  (fact (validate (Member String) [:billy "joey" :bobby]) =>
    (list
     (list 'not (list 'instance? 'String :billy))
     nil
     (list 'not (list 'instance? 'String :bobby)))))
(facts "Coll"
  (fact (display-type (Coll Int)) => (list 'Coll 'Int))
  (fact (validate (Coll String) ["Billy" "Bobby"]) => nil)
  (fact (validate (Coll String) (list "Billy" "Bobby")) => nil)
  (fact (validate (Coll Int) 3) => (list 'not (list 'coll? 3)))
  (fact (validate (Coll Int) nil) => (list 'not (list 'coll? nil))))
(facts "Seq"
  (fact (display-type (Seq Int)) => (list 'Seq 'Int))
  (fact (validate (Seq) (range 5)) => nil)
  (fact (validate (Seq Int) (range 5)) => nil)
  (fact (validate (Seq) (list 1 2 3)) => nil)
  (fact (validate (Seq String) (range 5)) =>
    (list
     (list 'not (list 'instance? 'String 0))
     (list 'not (list 'instance? 'String 1))
     (list 'not (list 'instance? 'String 2))
     (list 'not (list 'instance? 'String 3))
     (list 'not (list 'instance? 'String 4))))
  (fact (validate (Seq) []) => (list 'not (list 'seq? []))))
(facts "LazySeq"
  (fact (display-type (LazySeq Int)) => (list 'LazySeq 'Int))
  (fact (validate (LazySeq) (range 5)) => nil)
  (fact (validate (LazySeq Int) (range 5)) => nil)
  (fact (validate (LazySeq) (list 1 2 3)) =>
    (list 'not (list 'instance? 'LazySeq (list 1 2 3))))
  (fact (validate (LazySeq String) (range 5)) =>
    (list
     (list 'not (list 'instance? 'String 0))
     (list 'not (list 'instance? 'String 1))
     (list 'not (list 'instance? 'String 2))
     (list 'not (list 'instance? 'String 3))
     (list 'not (list 'instance? 'String 4))))
  (fact (validate (LazySeq) []) => (list 'not (list 'instance? 'LazySeq []))))
(facts "Seqable"
  (fact (display-type (Seqable Int)) => (list 'Seqable 'Int))
  (fact (validate (Seqable) [1 2 3]) => nil)
  (fact (validate (Seqable Int) [1 2 3]) => nil)
  (fact (validate (Seqable Int) (list 1 2 3)) => nil)
  (fact (validate (Seqable Int) (range 5)) => nil)
  (fact (validate (Seqable Int) #{1 3 2}) => nil)
  (fact (validate (Seqable [String Int]) {"a" 1, "b" 2}) => nil)
  (fact (validate (Seqable) "hello") =>
    (list 'not (list 'instance? 'Seqable "hello")))
  (fact (validate (Seqable) nil) => (list 'not (list 'instance? 'Seqable nil)))
  (fact (validate (Seqable) 3) => (list 'not (list 'instance? 'Seqable 3))))
(facts "NilableColl"
  (fact (display-type (NilableColl Int)) => (list 'Nilable (list 'Coll 'Int)))
  (fact (validate (NilableColl) [1 2 3]) => nil)
  (fact (validate (NilableColl Int) [1 2 3]) => nil)
  (fact (validate (NilableColl) nil) => nil)
  (fact (validate (NilableColl Int) nil) => nil))
(facts "CanSeq"
  (fact (display-type (CanSeq Int)) => (list 'CanSeq 'Int))
  (fact (validate (CanSeq Int) [1 2 3]) => nil)
  (fact (validate (CanSeq) (list 1 2 3)) => nil)
  (fact (validate (CanSeq String) nil) => nil)
  (fact (validate (CanSeq) "hello") => nil)
  (fact (validate (CanSeq) 42) =>
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
  (fact (validate (SortedMap Keyword String) (sorted-map :a "1")) => nil)
  (fact (validate (SortedMap Keyword String) (sorted-map (quote a) "1")) =>
    (list [(list 'not (list 'instance? 'Keyword 'a)) nil])))
(facts "KwA"
  (fact (display-type (KwA :method Named :timeout Int)) =>
    (list 'KwA :method 'Named :timeout 'Int))
  (fact (validate (KwA :method Named :timeout Int) {:method :POST}) => nil)
  (fact (validate (KwA :method Named :timeout Int) {:timeout 100, :method :POST}) =>
    nil)
  (fact (validate (KwA :method Named :timeout Int) {:method "DELETE", :redirects 3}) =>
    nil)
  (fact (validate (KwA :method Named :timeout Int) {}) => nil)
  (fact (validate (KwA :method Named :timeout Int) nil) => nil)
  (fact (validate (KwA :method Named :timeout Int) {:timeout 50.0}) =>
    (list
     'and
     {:timeout (list 'not (list 'integer? 50.0))}
     (list 'not (list 'nil? {:timeout 50.0})))))
(facts "Pairs"
  (fact (display-type (Pairs :method Named :timeout Int)) =>
    (list 'Pairs :method 'Named :timeout 'Int))
  (fact (validate (Pairs :method Named :timeout Int) (list :timeout 50 :method :POST)) =>
    nil)
  (fact (validate (Pairs :method Named :timeout Int) (list :timeout 50 :method "POST" :redirects 3)) =>
    nil)
  (fact (validate (Pairs :method Named :timeout Int) (list :timeout "50")) =>
    {:timeout (list 'not (list 'integer? "50"))})
  (fact (validate (Pairs :method Named :timeout Int) (list)) => nil))
(facts "Subset"
  (fact (display-type (Subset #{:warn :error})) => (list 'Subset #{:warn :error}))
  (fact (validate (Subset #{:warn :error}) #{:warn :error}) => nil)
  (fact (validate (Subset #{:warn :error}) #{:error}) => nil)
  (fact (validate (Subset #{:warn :error}) #{}) => nil)
  (fact (validate (Subset #{:warn :error}) #{:fatal :error}) =>
    (list 'not (list 'clojure.set/subset? #{:fatal :error} #{:warn :error}))))
(facts "ExMsg"
  (fact (validate (ExMsg "Fail!") (Exception. "Fail!")) => nil)
  (fact (validate (ExMsg (Option String)) (Exception.)) => nil)
  (fact (validate (ExMsg #"\d+") (Exception. "1234")) => nil)
  (fact (validate (ExMsg String) :fail) =>
    (list 'not (list 'instance? 'Exception :fail))))
