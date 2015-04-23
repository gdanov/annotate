# annotate

## Purpose

Type annotations and optional validation for functions. Can also be used to validate form fields.  See the friendly namespace for more details on generating friendly error messages.

## Installation

```clojure
[com.roomkey/annotate "1.0.0"]
```

## API Documentation

http://roomkey.github.io/rk-annotate

## Presentation

Run 'lein gorilla' then type C-g C-l in the worksheet to load 'presentation.clj'.

## How to use

Basics

```clojure
(use '[annotate core types])

; Maps
(def User
  {:name String
   (optional-key :email) String
   :address {:city String
             :state String}
   :likes [Keyword]})

(validate User {:name "Billy"
                :address {:city "San Diego"
                          :state "CA"}})

; {:likes key-not-found}

(validate User {:name :Billy
                :email "billy@example.org"
                :address {:city "San Diego"
                          :state :CA}
                :likes [:biking "hiking"]})

; {:name (not (instance? String :Billy)),
;  :address {:state (not (instance? String :CA))},
;  :likes [nil (not (instance? Keyword "hiking"))]}

(validate {Keyword String} {:name "Billy"})
; nil

(validate {Keyword String} {})
; nil

(validate {Keyword String} {:name :Billy})
; {:name (not (instance? String :Billy))}

; Vectors
(validate [Keyword] [:hi :there])
; nil

(validate [Int] (range 10))
; (not (vector? (0 1 2 3 4 ...)))
; Collections are automatically truncated

(validate [[Keyword Int]] [[:joe 10] [:billy 9]])
; nil
; Pairs are easily represented

(validate [Int] [])
; nil

; Lists
; same behavior as vectors

; Sets
(validate #{Keyword} #{:hi :there})
;nil

(validate #{Keyword} #{})
; nil

; Exact values
(validate 1 1)
; nil

(validate [true] [true true])
; nil

; Union
(validate (U Keyword Symbol String) :billy)
; nil

(validate (U Keyword Symbol String) 'billy)
; nil

(validate (U Keyword Symbol String) "billy")
; nil

(validate (U Keyword Symbol String) 5)
; (and (not (instance? Keyword 5)) (not (instance? Symbol 5)) (not (instance? String 5)))

; Intersection
(validate (I Int (Pred even?)) 2)
; nil

(validate (I Int (Pred even?)) 3)
; (not (even? 3))

; Predicates
(validate (Pred odd?) 3)
; nil

(validate (Pred empty?) [1])
; (not (empty? [1]))
```

Functions

```clojure
(use 'annotate.fns)

; Annotation only with no validation or code modifications.
; Use with low-level, performance sensitive fns.
(defna append [String String => String]
  "Append a string to a string."
  [s1 s2] (str s1 s2))

(doc append)
; user/append
; ([s1 s2])
;  [String String => String]

; Append a string to a string.

(annotation append)
; [String String => String]

(canonical append)
; [java.lang.String java.lang.String => java.lang.String]

; Always validated.  No need to be called within with-validation macro.
; Recur not supported.
(defnv greeting ([=> String] [String => String])
  "Doc string"
  ([] (greeting "world"))
  ([msg] (str "Hello, " msg)))

(greeting "Bob")
; "Hello, Bob"

(greeting :Bob)
; ExceptionInfo Failed to type check: (not (instance? String :Bob))

; defn' is only validated if called within with-validation macro.
; Recur is not supported.

; Rest args supported
(defn' append* [String & (Seq String) => String]
  [s1 & args] (apply str s1 args))

(with-validation (append* "Hello " :there :friend))
; ExceptionInfo Failed to type check: ((not (instance? String :there)) (not (instance? String :friend)))

; Keyword args supported
(defn' ping [String & (KwA :method Named :timeout Int) => String]
  [url & {:keys [method timeout]}]
  (str "Ping: " url " with: " method))

(with-validation (ping "localhost" :method :POST))
; "Ping: localhost with: :POST"

(with-validation (ping "localhost" :method :POST :timeout 100.0))
; ExceptionInfo Failed to type check: {:timeout (not (integer? 100.0))}

; Higher order fns
(defn' map* [Fn (CanSeq) => (LazySeq)]
  [f coll] (map f coll))

(with-validation (map* inc (range 5)))
; (1 2 3 4 5)

(with-validation (map* 3 (range 5)))
; ExceptionInfo Failed to type check: (not (ifn? 3))

; defn$ will generate an always validated function when the system property annotate.typecheck is set to 'on'. Otherwise an annotated only function will be generated.
; Add :jvm-opts ["-Dannotate.typecheck=on"] to the dev profile of your project to enable typechecking during development and when running unit tests.
(defn$ echo [String => String]
  [msg]
  msg)
```

NOTE: `defn'`, `defnv`, and `defn$` will remove pre/post conditions from the generated code.
If you need to mimic the behavior of pre/post conditions use `assert` in the body of your function.

Anonymous functions

```clojure
((fnv [String => String] [x] x) "Bob")
; "Bob"
((fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")
; "BillyBob"
((fnv [String => String] [x] 20) "Bob")
; ExceptionInfo Failed to type check anonymous output: (not (instance? String 20))
```

Recursive types

```clojure
(def Element
  {:tag Keyword
   :attrs {Keyword String}
   :content (Seqable (U String #'Element))})

(validate Element
          {:tag :a
           :attrs {:b "c"}
           :content ["d" "e" {:tag :f :attrs {} :content ["g"]}]})
; nil
```

Wrap an existing function outside your control

```clojure
(use 'annotate.wrap)

(defn ping
  "Make an HTTP request."
  [url & {:keys [method timeout] :or {method "GET"}}]
  (str "url: " url ", method: " (name method) ", timeout: " timeout))

(wrap' ping [String & (Pairs :method Named :timeout Int) => String])

(with-validation (ping "localhost" :method :POST))
; "url: localhost, method: POST, timeout: "

(with-validation (ping "localhost" :timeout 100.0))
; Exceptioninfo Failed to type check ping input(s): {:timeout (not (clojure.core/integer? 100.0))}
```

Friendly errors

```clojure
(use 'annotate.friendly)

(-> (validate {:first-name (NonEmpty String)
               :age Int}
              {:first-name ""})
    (friendly {:first-name (label "First name is invalid" "First name is missing")
               :age (label "Age is invalid" "Age not found")}))
; {:age "Age not found", :first-name "First name is invalid"}
```

Type validation

```clojure
(valid-type? #{String})
; true

(valid-type? #{String Keyword})
; false
```
