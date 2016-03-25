(ns annotate.examples-test
  #?(:clj
     (:use [annotate core types fns records wrap friendly]
           ;;midje.sweet
           clojure.test
           annotate.examples)))

(deftest t-annotation
  (is (= ['String 'String '=> 'String] (annotation append)))
  (is (= ['java.lang.String 'java.lang.String '=> 'java.lang.String] (canonical append)))
  (is (= "[String String => String]" (-> append var meta :doc))))

(deftest t-multi-arity
  (is (= "Hello, world" (greeting)))
  (is (= "Hello, Bobby" (greeting "Bobby")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        ;;"Failed to type check annotate.examples/greeting input(s): (not (instance? String :Bobby))"
        #"Failed to type check annotate\.examples/greeting input"
        (greeting :Bobby)))
  (is (= (list ['=> 'String] ['String '=> 'String]) (annotation greeting)))
  (is (= (list ['=> 'java.lang.String] ['java.lang.String '=> 'java.lang.String])
        (canonical greeting)))
  (is (= (list [] ['msg]) (-> (var greeting) meta :arglists)))
  (is (= "([=> String] [String => String])\n\nDoc string"
        (-> (var greeting) meta :doc))))

(deftest t-rest-args
  (is (= "hi" (with-checking (append* "hi"))))
  (is (= "hi there" (with-checking (append* "hi" " there"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        ;; "Failed to type check annotate.examples/append* input(s): ((not (instance? String :there)))"
        #"Failed to type check annotate\.examples/append\* input"
        (with-checking (append* "hi" :there)))))

(deftest t-keyword-args
  (is (= "url: localhost, method: GET, timeout: "
        (with-checking (ping "localhost"))))
  (is (= "url: localhost, method: POST, timeout: "
        (with-checking (ping "localhost" :method :POST))))
  (is (= "url: localhost, method: POST, timeout: "
        (with-checking (ping "localhost" :method "POST"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Failed to type check .* input.* 100.*"
        (with-checking (ping "localhost" :method :POST :timeout 100.0))))
  (is (= ['String '& (list 'KwA :method 'Named :timeout 'Int) '=> 'String]
        (annotation ping)))
  (is (= ['java.lang.String
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
         'java.lang.String]
        (canonical ping)))
  (is (= (list ['url '& {:keys ['method 'timeout], :or {'method "GET"}}])
        (-> (var ping) meta :arglists)))
  (is (= "[String & (KwA :method Named :timeout Int) => String]"
        (-> (var ping) meta :doc))))

(deftest t-higher-order
  (is (= (list 1 2 3 4 5) (with-checking (map* inc (range 5)))))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Failed to type check annotate\.examples/map.*3"
        (with-checking (map* 3 (range 5)))))
  (is (= ['Fn 'CanSeq '=> 'LazySeq] (annotation map*)))
  (is (= [(list 'Pred 'ifn?)
         (list
           'U
           'clojure.lang.Seqable
           nil
           'java.lang.String
           'java.lang.Iterable
           'java.util.Map
           (list 'Pred 'annotate.util/array?))
         '=>
         'clojure.lang.LazySeq]
        (canonical map*)))
  (is (= (list ['f 'coll]) (-> (var map*) meta :arglists)))
  (is (= "[Fn CanSeq => LazySeq]" (-> (var map*) meta :doc)))
  (is (= "1.0" (-> (var map*) meta :added))))

(deftest t-output-type-error
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        ;; #"Failed to type check annotate.examples/str* output: (not (instance? String nil))"
        #"Failed to type check annotate\.examples/str\* output"
        (with-checking (str* "")))))

(deftest t-fn
  (is (= "Hello, world" ((fnv [=> String] [] "Hello, world"))))
  (is (= "Bob" ((fnv [String => String] [x] x) "Bob")))
  (is (= "BillyBob"
        ((fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "BillyBob"
        ((fnv cat ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")))
  (is (= "Bob" ((fnv id [String => String] [x] x) "Bob")))
  (is (thrown-with-msg?
     clojure.lang.ExceptionInfo
     #"Failed to type check id input.*42"
     ((fnv id [String => String] [x] x) 42)))
  (is (thrown-with-msg?
     clojure.lang.ExceptionInfo
     #"Failed to type check anonymous input.*10"
     ((fnv [String => String] [x] x) 10)))
  (is (thrown-with-msg?
     clojure.lang.ExceptionInfo
     #"Failed to type check anonymous output.*20"
     ((fnv [String => String] [x] 20) "Bob")))
  (is (= "Bob" ((fna [String => String] [x] x) "Bob")))
  (is (= "Bob" ((fn$ [String => String] [x] x) "Bob"))))

(deftest t-defrecord
  (is (= (map->User {:first-name "Billy", :last-name "Bob"})
        (->User "Billy" "Bob")))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Failed to type check .*:billy.*:bob"
        (->User :billy :bob))))

(deftest t-record
  (is (= nil (check annotate.examples.Person (->Person "Billy"))))
  (is (= nil (check {:name String} (->Person "Billy"))))
  (is (= nil
        (check (I annotate.examples.Person {:name String}) (->Person "Billy"))))
  (is (= {:name (list 'not (list 'integer? "Billy"))}
        (check {:name Int} (->Person "Billy")))))

(deftest t-Protocol
  (is (= (list 'Protocol 'annotate.examples/Foo)
        (display-type (Protocol Foo))))
  (is (= nil (check (Protocol Foo) (->Bar 3))))
  (is (= (list 'not (list 'satisfies? 'annotate.examples/Foo 3))
        (check (Protocol Foo) 3))))

(deftest t-Recursive
  (is (= {:tag 'Keyword,
         :attrs {'Keyword 'String},
         :content (list 'Seqable (list 'U 'String 'Element))}
        (display-type Element)))
  (is (= nil
        (check Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" "f"]})))
  (is (= {:attrs {:b (list 'not (list 'instance? 'String :c))},
         :content
         (list
           nil
           nil
           (list
             'and
             (list 'not (list 'instance? 'String :f))
             (list 'not (list 'map? :f))))}
        (check Element {:tag :a, :attrs {:b :c}, :content ["d" "e" :f]})))
  (is (= nil (check Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" {:tag :f, :attrs {}, :content ["g"]}]})))
  (is (= {:content
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
                  (list 'not (list 'map? :fail))))}))}
        (check Element {:tag :a, :attrs {:b "c"}, :content ["d" "e" {:tag :f, :attrs {:g :h}, :content [:fail]}]}))))

(deftest t-wrapv
  (is (= 0 (add)))
  (is (= 1 (add 1)))
  (is (= 3 (add 1 2)))
  (is (= 6 (add 1 2 3)))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Failed to type check add input"
        (add :1 2 :3)))
  (is (= (list [] ['x] ['x 'y] ['x 'y '& 'zs]) (-> (var add) meta :arglists)))
  (is (= "([=> Num] [Num => Num] [Num Num => Num] [Num Num & (Seq Num) => Num])\n\nAdd some numbers."
        (-> (var add) meta :doc))))

(deftest t-wrap-with-keyword-arguments
  (is (= "url: localhost, method: GET, timeout: "
        (with-checking (ping* "localhost"))))
  (is (= "url: localhost, method: POST, timeout: "
        (with-checking (ping* "localhost" :method :POST))))
  (is (= "url: localhost, method: POST, timeout: 50"
        (with-checking (ping* "localhost" :timeout 50 :method "POST"))))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Failed to type check ping\* input"
        (with-checking (ping* "localhost" :method :POST :timeout 100.0))))
  (is (= ['String '& (list 'Pairs :method 'Named :timeout 'Int) '=> 'String]
        (annotation ping*)))
  (is (= ['java.lang.String
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
         'java.lang.String]
        (canonical ping*)))
  (is (= (list ['url '& {:keys ['method 'timeout], :or {'method "GET"}}])
        (-> (var ping*) meta :arglists)))
  (is (= "[String & (Pairs :method Named :timeout Int) => String]\n\nMake an HTTP request."
        (-> (var ping*) meta :doc))))

(deftest t-wrap$
  (is (= 3 (id 3)))
  (is (= "[String => String]" (-> (var id) meta :doc))))

(deftest t-lazy-check
  (is (= [0 1 2 3 4 5 6 7 8 9] (vec (lazy-check Int (range 10)))))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Failed to type check sequence.*"
        (vec (lazy-check (Pred even?) (range 10))))))

(deftest t-friendly
  (testing "Explicit error and not found messages"
    (testing "Replace errors with labels"
      (is (= {:age "Age not found", :first-name "First name is invalid"}
            (-> (check {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})))))
    (testing "No errors"
      (is (= nil
            (-> (check {:first-name (NonEmpty String), :age Int} {:first-name "Billy", :age 35}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})))))
    (testing "No labels for error keys"
      (is (= {:age 'key-not-found,
             :first-name (list 'not (list 'seq ""))}
            (-> (check {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:last-name (label "Last name is invalid" "Last name is missing")})))))
    (testing "Key should not be present if no error on field"
      (is (= {:age "Age not found"}
            (-> (check {:first-name (NonEmpty String), :age Int} {:first-name "Billy"}) (friendly {:first-name (label "First name is invalid" "First name is missing"), :age (label "Age is invalid" "Age not found")})))))
    (testing "Nested errors"
      (is (= {:age "Age not found",
             :name {:last "Last name is missing", :first "First name is invalid"}}
            (-> (check {:name {:first (NonEmpty String), :last (NonEmpty String)}, :age Int} {:name {:first ""}}) (friendly {:name {:first (label "First name is invalid" "First name is missing"), :last (label "Last name is invalid" "Last name is missing")}, :age (label "Age is invalid" "Age not found")}))))))
  (testing "Default error and not found messages"
    (testing "Replace errors with labels"
      (is (= {:age "Not found", :first-name "Invalid input"}
            (-> (check {:first-name (NonEmpty String), :age Int} {:first-name ""}) (friendly {:first-name (label), :age (label)})))))
    (testing "Nested errors"
      (is (= {:age "Not found", :name {:last "Not found", :first "Invalid input"}}
            (-> (check {:name {:first (NonEmpty String), :last (NonEmpty String)}, :age Int} {:name {:first ""}}) (friendly {:name {:first (label), :last (label)}, :age (label)})))))))
