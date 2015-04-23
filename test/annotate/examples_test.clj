(ns annotate.examples-test
  (:use [annotate core types fns records wrap friendly] midje.sweet annotate.examples))

(facts "annotation"
  (fact (annotation append) => ['String 'String '=> 'String])
  (fact (canonical append) =>
    ['java.lang.String 'java.lang.String '=> 'java.lang.String])
  (fact (-> (var append) meta :doc) => "[String String => String]"))
(facts "multi-arity"
  (fact (greeting) => "Hello, world")
  (fact (greeting "Bobby") => "Hello, Bobby")
  (fact (greeting :Bobby) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/greeting input(s): (not (instance? String :Bobby))"))
  (fact (annotation greeting) => (list ['=> 'String] ['String '=> 'String]))
  (fact (canonical greeting) =>
    (list ['=> 'java.lang.String] ['java.lang.String '=> 'java.lang.String]))
  (fact (-> (var greeting) meta :arglists) => (list [] ['msg]))
  (fact (-> (var greeting) meta :doc) =>
    "([=> String] [String => String])\n\nDoc string"))
(facts "rest args"
  (fact (with-validation (append* "hi")) => "hi")
  (fact (with-validation (append* "hi" " there")) => "hi there")
  (fact (with-validation (append* "hi" :there)) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/append* input(s): ((not (instance? String :there)))")))
(facts "keyword args"
  (fact (with-validation (ping "localhost")) =>
    "url: localhost, method: GET, timeout: ")
  (fact (with-validation (ping "localhost" :method :POST)) =>
    "url: localhost, method: POST, timeout: ")
  (fact (with-validation (ping "localhost" :method "POST")) =>
    "url: localhost, method: POST, timeout: ")
  (fact (with-validation (ping "localhost" :method :POST :timeout 100.0)) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/ping input(s): (and {:timeout (not (integer? 100.0))} (not (nil? {:method :POST, :timeout 100.0})))"))
  (fact (annotation ping) =>
    ['String '& (list 'KwA :method 'Named :timeout 'Int) '=> 'String])
  (fact (canonical ping) =>
    ['java.lang.String
     '&
     (list
      'U
      {(list 'optional-key :method)
       (list
        'U
        'clojure.lang.Symbol
        'clojure.lang.Keyword
        'java.lang.String),
       (list 'optional-key :timeout) (list 'Pred 'integer?)}
      nil)
     '=>
     'java.lang.String])
  (fact (-> (var ping) meta :arglists) =>
    (list ['url '& {:keys ['method 'timeout], :or {'method "GET"}}]))
  (fact (-> (var ping) meta :doc) =>
    "[String & (KwA :method Named :timeout Int) => String]"))
(facts "higher order"
  (fact (with-validation (map* inc (range 5))) => (list 1 2 3 4 5))
  (fact (with-validation (map* 3 (range 5))) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/map* input(s): (not (ifn? 3))"))
  (fact (annotation map*) => ['Fn 'CanSeq '=> 'LazySeq])
  (fact (canonical map*) =>
    [(list 'Pred 'ifn?)
     (list
      'U
      'clojure.lang.Seqable
      nil
      'java.lang.String
      'java.lang.Iterable
      'java.util.Map
      (list 'Pred 'annotate.util/array?))
     '=>
     'clojure.lang.LazySeq])
  (fact (-> (var map*) meta :arglists) => (list ['f 'coll]))
  (fact (-> (var map*) meta :doc) => "[Fn CanSeq => LazySeq]")
  (fact (-> (var map*) meta :added) => "1.0"))
(facts "output type error"
  (fact (with-validation (str* "")) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/str* output: (not (instance? String nil))")))
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
(facts "defrecord"
  (fact (->User "Billy" "Bob") =>
    (map->User {:first-name "Billy", :last-name "Bob"}))
  (fact (->User :billy :bob) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check annotate.examples/->User input(s): (not (instance? String :billy)), (not (instance? String :bob))")))
(facts "record"
  (fact (validate annotate.examples.Person (->Person "Billy")) => nil)
  (fact (validate {:name String} (->Person "Billy")) => nil)
  (fact (validate (I annotate.examples.Person {:name String}) (->Person "Billy")) =>
    nil)
  (fact (validate {:name Int} (->Person "Billy")) =>
    {:name (list 'not (list 'integer? "Billy"))}))
(facts "Protocol"
  (fact (display-type (Protocol Foo)) =>
    (list 'Protocol 'annotate.examples/Foo))
  (fact (validate (Protocol Foo) (->Bar 3)) => nil)
  (fact (validate (Protocol Foo) 3) =>
    (list 'not (list 'satisfies? 'annotate.examples/Foo 3))))
(facts "Recursive"
  (fact (display-type Element) =>
    {:tag 'Keyword,
     :attrs {'Keyword 'String},
     :content (list 'Seqable (list 'U 'String 'Element))})
  (fact (validate Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" "f"]}) =>
    nil)
  (fact (validate Element {:tag :a, :attrs {:b :c}, :content ["d" "e" :f]}) =>
    {:attrs {:b (list 'not (list 'instance? 'String :c))},
     :content
     (list
      nil
      nil
      (list
       'and
       (list 'not (list 'instance? 'String :f))
       (list 'not (list 'map? :f))))})
  (fact (validate Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" {:tag :f, :attrs {}, :content ["g"]}]}) =>
    nil)
  (fact (validate Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" {:tag :f, :attrs {:g :h}, :content [:fail]}]}) =>
    {:content
     (list
      nil
      nil
      (list
       'and
       (list
        'not
        (list
         'instance?
         'String
         {:content [:fail], :attrs {:g :h}, :tag :f}))
       {:attrs {:g (list 'not (list 'instance? 'String :h))},
        :content
        (list
         (list
          'and
          (list 'not (list 'instance? 'String :fail))
          (list 'not (list 'map? :fail))))}))}))
(facts "wrapv"
  (fact (add) => 0)
  (fact (add 1) => 1)
  (fact (add 1 2) => 3)
  (fact (add 1 2 3) => 6)
  (fact (add :1 2 :3) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check add input(s): (not (instance? Number :1)), ((not (instance? Number :3)))"))
  (fact (-> (var add) meta :arglists) => (list [] ['x] ['x 'y] ['x 'y '& 'zs]))
  (fact (-> (var add) meta :doc) =>
    "([=> Num] [Num => Num] [Num Num => Num] [Num Num & (Seq Num) => Num])\n\nAdd some numbers."))
(facts "wrap' with keyword arguments"
  (fact (with-validation (ping* "localhost")) =>
    "url: localhost, method: GET, timeout: ")
  (fact (with-validation (ping* "localhost" :method :POST)) =>
    "url: localhost, method: POST, timeout: ")
  (fact (with-validation (ping* "localhost" :timeout 50 :method "POST")) =>
    "url: localhost, method: POST, timeout: 50")
  (fact (with-validation (ping* "localhost" :method :POST :timeout 100.0)) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check ping* input(s): {:timeout (not (integer? 100.0))}"))
  (fact (annotation ping*) =>
    ['String '& (list 'Pairs :method 'Named :timeout 'Int) '=> 'String])
  (fact (canonical ping*) =>
    ['java.lang.String
     '&
     (list
      'Pairs
      :method
      (list
       'U
       'clojure.lang.Symbol
       'clojure.lang.Keyword
       'java.lang.String)
      :timeout
      (list 'Pred 'integer?))
     '=>
     'java.lang.String])
  (fact (-> (var ping*) meta :arglists) =>
    (list ['url '& {:keys ['method 'timeout], :or {'method "GET"}}]))
  (fact (-> (var ping*) meta :doc) =>
    "[String & (Pairs :method Named :timeout Int) => String]\n\nMake an HTTP request."))
(facts "wrap$"
  (fact (id 3) => 3)
  (fact (-> (var id) meta :doc) => "[String => String]"))
(facts "lazy-validate"
  (fact (vec (lazy-validate Int (range 10))) => [0 1 2 3 4 5 6 7 8 9])
  (fact (vec (lazy-validate (Pred even?) (range 10))) =>
    (throws
     clojure.lang.ExceptionInfo
     "Failed to type check sequence: (not (even? 1))")))
(facts "friendly"
  (facts "Explicit error and not found messages"
    (facts "Replace errors with labels"
      (fact (-> (validate {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})) =>
        {:age "Age not found", :first-name "First name is invalid"}))
    (facts "No errors"
      (fact (-> (validate {:first-name (NonEmpty String), :age Int} {:first-name "Billy", :age 35}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})) =>
        nil))
    (facts "No labels for error keys"
      (fact (-> (validate {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:last-name (label "Last name is invalid" "Last name is missing")})) =>
        {:age 'key-not-found,
         :first-name (list 'not (list 'seq ""))}))
    (facts "Key should not be present if no error on field"
      (fact (-> (validate {:first-name (NonEmpty String), :age Int} {:first-name "Billy"}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})) =>
        {:age "Age not found"}))
    (facts "Nested errors"
      (fact (-> (validate {:name {:first (NonEmpty String), :last (NonEmpty String)}, :age Int} {:name {:first ""}}) (friendly {:name {:first (label "First name is invalid" "First name is missing"), :last (label "Last name is invalid" "Last name is missing")}, :age (label "Age is invalid" "Age not found")})) =>
        {:age "Age not found",
         :name {:last "Last name is missing", :first "First name is invalid"}})))
  (facts "Default error and not found messages"
    (facts "Replace errors with labels"
      (fact (-> (validate {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:first-name (label), :age (label)})) =>
        {:age "Not found", :first-name "Invalid input"}))
    (facts "Nested errors"
      (fact (-> (validate {:name {:first (NonEmpty String), :last (NonEmpty String)}, :age Int} {:name {:first ""}}) (friendly {:name {:first (label), :last (label)}, :age (label)})) =>
        {:age "Not found", :name {:last "Not found", :first "Invalid input"}}))))
