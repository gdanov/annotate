![Logo](https://raw.githubusercontent.com/roomkey/annotate/master/roomkey-logo-dark.png)

# annotate

## Purpose

* Documenting function input and output types.
* Catching bugs that occur when unexpected data is passed to or returned from a function.
* Validating user data (e.g., data submitted via a form or passed via an API).
* Providing a lingua franca for describing the shape of Clojure data.

Currently there are two projects that offer similar functionality.

* [Schema](https://github.com/Prismatic/schema)
* [core.typed](https://github.com/clojure/core.typed)

Annotate is in the same category as Schema, that is, runtime data validation.  We do, however, attempt to reuse many of the conventions from core.typed (e.g., all types begin with an uppercase letter) as well as specific names from core.typed (e.g., [U](http://roomkey.github.io/annotate/annotate.types.html#var-U) for the union of two or more types).

Annotate was written to provide a consistent and rich out-the-box experience for developers interested in adding types to their functions.  Another primary objective is to provide a means of checking types during development and testing, while generating code that incurs no performance penalty in production.

## Installation

```clojure
[com.roomkey/annotate "1.0.0"]
```

## API Documentation

http://roomkey.github.io/annotate

## How to use

Types in annotate are comprised of Clojure data, and can be composed at runtime to produce more complex types (or less complex types) as needed.  You should be able to express your data purely by composing the existing types provided by annotate.  All types extend the [Typeable](http://roomkey.github.io/annotate/annotate.core.html#var-Typeable) protocol in [annotate.core](http://roomkey.github.io/annotate/annotate.core.html).

Let's take a look at some basic types.  All the built-in types can be found in [annotate.types](http://roomkey.github.io/annotate/annotate.types.html).  The check function, which is used internally, can be found in [annotate.core](http://roomkey.github.io/annotate/annotate.core.html). We'll need to refer that as well for the examples.

```clojure
(use '[annotate core types])
```

### Maps

There are three different ways to express the shape of a map in annotate.

1. A heterogeneous map, that is a map whose keys all conform to a single type, and likewise, whose values all conform to a single type.
2. A map with specific, named keys whose corresponding values conform to a specific type.
3. An empty map. This type checks against an empty map, and only an empty map.

The first two patterns cannot be mixed, as that can lead to ambiguity in the types.

#### Homogenous Maps

Homogenous maps use the Clojure map literal syntax and must contain a single key and value.

The [check](http://roomkey.github.io/annotate/annotate.core.html#var-check) function takes a type and some data to check the type against.  If the shape of the data conforms to the type, then `nil` is returned.  Otherwise, a type error will be returned.

The output from each function is printed on the line beneath the code, and shown as a Clojure comment.

```clojure
(def M {Keyword String})

(check M {:name "Billy"})
;; nil

(check M {})
;; nil

(check M {:name :Billy})
;; {:name (not (instance? String :Billy))}
```
The [Keyword](http://roomkey.github.io/annotate/annotate.types.html#var-Keyword) type is defined in [annotate.types](http://roomkey.github.io/annotate/annotate.types.html), and references clojure.lang.Keyword. The `String` type is a reference to `java.lang.String`, and is automatically imported in all Clojure code.

Notice that the empty map is valid for this type, but a map whose values are Clojure keywords, are not. Also, the error that is returned is a Clojure data structure.

#### Named Maps

Named maps are maps where the keys are defined upfront.  Keyword, symbol, and string keys are automatically assumed to be required, that is they must be present.  You can also indicate that a key is optional, that is, it does not have to present, by wrapping the key in the `optional-key` function.  Notice that this function is lowercase.  That is because it is a component of a type, and not actually a type itself.

Any keys that are not present in the named map type will be ignored when checking types.  This is intentional, and is often referred to as Row Polymorphism in statically typed languages that support it.

```clojure
(def User
  {:name String
   (optional-key :email) String
   :address {:city String
             :state String}})

(check User {:name "Billy"
             :address {:city "San Diego"
                       :state "CA"}})

;; {:likes key-not-found}

(check User {:name :Billy
             :email "billy@example.org"
             :address {:city "San Diego"
                       :state :CA}})

;; {:name (not (instance? String :Billy)),
;;  :address {:state (not (instance? String :CA))}}
```

Notice that the shape of the error is a map itself. In the case where a key is not found, the value for that key is replaced with the symbol `key-not-found`. When the value of a key fails to type check, a Clojure data structure that represents the error is substituted for the value.

#### Empty Map

```clojure
(check {} {})
;; nil

(check {} {:name "Billy"})
;; (not (empty? {:name "Billy"}))
```

### Vectors

There are three different patterns for expressing the shape of a vector in annotate.

1. Homogenous vectors, where the element represents the type for all elements in the vector. Also type checks against the empty vector.
2. A two-tuple, three-tuple, etc. vector, where each element represents the type for that particular position within the fixed length vector.
3. An empty vector, that type checks against the empty vector, and only the empty vector.

#### Homogenous Vectors

```clojure
(check [Keyword] [:hi :there])
;; nil

(check [Int] [1 :2 3])
;; [nil (not (integer? :2)) nil]

(check [Int] (range 10))
;; (not (vector? (0 1 2 3 4 ...)))

(check [Int] [])
;; nil
```

Notice that a `nil` is returned in the position where a particular element type checked, in the case where the data as a whole did not. Also notice that collections are automatically truncated. This is to minimize the possibility of exceptionally large error messages.

#### Vector Tuples

```clojure
(check [[Keyword Int]] [[:joe 10] [:billy 9]])
;; nil
```

#### Empty Vector

```clojure
(check [] [])
;; nil
```

### Lists

Lists have the same behavior as vectors.

### Sets

There are two different patterns for expressing the shape of a set in annotate.

1. Homogenous sets, where the element represents the type for all elements in the set. Also type checks against the empty set.
2. An empty set, that type checks against the empty set, and only the empty set.

```clojure
(check #{Keyword} #{:hi :there})
;; nil

(check #{Keyword} #{})
;; nil

(check #{} #{})
;; nil
```

### Sequences

There are many types that can be used to define a sequence of values.  The most likely to be used is [NillableColl](http://roomkey.github.io/annotate/annotate.types.html#var-NilableColl), which represents a collection of some type or `nil`.

```clojure
(check (NilableColl Int) [1 2 3])
;; nil

(check (NilableColl Int) nil)
;; nil

(check (NilableColl Int) "Billy")
;; (and (not (coll? "Billy")) (not (nil? "Billy")))
```

### Scalar values

Scalar values require no special wrapping.

```clojure
(check 1 1)
;; nil

(check 1 2)
;; (not= 1 2)

(check [true] [true true])
;; nil
```

### Union of types

A union implies that the type is composed of one or more types, and that the data only need conform to at most one of the types. Types are checked in the order they are passed.

```clojure
(check (U Keyword Symbol String) :billy)
;; nil

(check (U Keyword Symbol String) 'billy)
;; nil

(check (U Keyword Symbol String) "billy")
;; nil

(check (U Keyword Symbol String) 5)
;; (and (not (instance? Keyword 5)) (not (instance? Symbol 5)) (not (instance? String 5)))
```

### Intersection of types

A intersection implies that the type is composed of one or more types, and that the data must conform to all of the types. Types are checked in the order they are passed.

```clojure
(check (I Int (Pred even?)) 2)
;; nil

(check (I Int (Pred even?)) 3)
;; (not (even? 3))
```

### Predicates

The [Pred](http://roomkey.github.io/annotate/annotate.types.html#var-Pred) type allows for arbitrary logic when type checking. It takes a predicate function and type checks if the function returns a truthy value.

```clojure
(check (Pred odd?) 3)
;; nil

(check (Pred odd?) 2)
;; (not (odd? 2))
```

### Functions

Annotate provides four variations of the [defn](http://clojuredocs.org/clojure.core/defn) macro:
[defn'](http://roomkey.github.io/annotate/annotate.fns.html#var-defn.27),  [defna](http://roomkey.github.io/annotate/annotate.fns.html#var-defna), [defnv](http://roomkey.github.io/annotate/annotate.fns.html#var-defnv) and [defn$](http://roomkey.github.io/annotate/annotate.fns.html#var-defn.24).

`defn'` modifies the body of your function, adding a conditional switch that when enabled will check the types of the inputs and output against the type annotation. In addition, metadata is added to the var containing the type annotation. Finally, the type annotation is added to the doc string.

`defna` does not modify the body of your function in any way. It does add metadata and append to the doc string like `defn'`, though.

`defnv` works like `defn'`, only type checking is always enabled.

`defn$` will generate an always type checked function when the system property `annotate.typecheck` is set to `on`. Otherwise, an annotated only function will be generated. Add `:jvm-opts ["-Dannotate.typecheck=on"]` to the dev profile of your project to enable type checking during development and when running tests.

Type annotations for fns must be wrapped in a vector or
list. Lists indicate a multi-arty fn and should contain two or more vector forms.

NOTE: `defn'`, `defnv`, and `defn$` will remove pre/post conditions from the generated code.
If you need to mimic the behavior of pre/post conditions use `assert` in the body of your function.

All of the macros for creating type annotated functions are located in [annotate.fns](http://roomkey.github.io/annotate/annotate.fns.html). Let's take a look at some examples.  

```clojure
(use 'annotate.fns)

;; Annotation only with no type checking or code modification.
;; Use with low-level, performance sensitive fns.
(defna append [String String => String]
  "Append a string to a string."
  [s1 s2]
  (str s1 s2))

(doc append)
;; user/append
;; ([s1 s2])
;;  [String String => String]
;;
;; Append a string to a string.

(annotation append)
;; [String String => String]

(canonical append)
;; [java.lang.String java.lang.String => java.lang.String]
```

Notice how we can use the [annotation](http://roomkey.github.io/annotate/annotate.core.html#var-annotation) and [canonical](http://roomkey.github.io/annotate/annotate.core.html#var-canonical) macros to retrieve the type annotation from the var.  By default, the display of the type is as terse as possible. This is a core principal of annotate. Types should be as terse as possible, while providing the more verbose representation when needed.

```clojure
;; Always type checked.
(defnv greeting ([=> String] [String => String])
  "Doc string"
  ([] (greeting "world"))
  ([msg] (str "Hello, " msg)))

(greeting "Bob")
;; "Hello, Bob"

(greeting :Bob)
;; ExceptionInfo Failed to type check user/greeting input(s): (not (instance? String :Bob))
```

Notice that an ExceptionInfo exception is thrown when the function fails to type check. The exception message will always contain the name of the var and it's namespace, as well as whether an input or the output failed to type check.  The data representation of the error can be extracted using `ex-data`, if needed.

```clojure
;; Rest args
(defn' append* [String & (Seq String) => String]
  [s1 & args]
  (apply str s1 args))

(with-checking (append* "Hello " :there :friend))
;; ExceptionInfo Failed to type check user/append* input(s): ((not (instance? String :there)) (not (instance? String :friend)))

;; Keyword args
(defn' ping [String & (KwA :method Named :timeout Int) => String]
  [url & {:keys [method timeout]}]
  (str "Ping: " url " with: " method))

(with-checking (ping "localhost" :method :POST))
;; "Ping: localhost with: :POST"

(with-checking (ping "localhost" :method :POST :timeout 100.0))
;; ExceptionInfo Failed to type check user/ping input(s): (and {:timeout (not (integer? 100.0))} (not (nil? {:method :POST, :timeout 100.0})))

;; Higher order fns
(defn' map* [Fn (CanSeq) => (LazySeq)]
  [f coll]
  (map f coll))

(with-checking (map* inc (range 5)))
;; (1 2 3 4 5)

(with-checking (map* 3 (range 5)))
;; ExceptionInfo Failed to type check user/map* input(s): (not (ifn? 3))
```

Notice that `defn'` is only type checked when called within the [with-checking](http://roomkey.github.io/annotate/annotate.core.html#var-with-checking) macro. Also, notice the type for representing keyword arguments [KwA](http://roomkey.github.io/annotate/annotate.types.html#var-KwA).

```clojure
(defn$ echo [String => String]
  [msg]
  msg)

(echo "Hello")
;; Hello

;; With the system property annotate.typecheck set to 'on'.
(echo 3)
;; ExceptionInfo Failed to type check user/echo input(s): (not (instance? String 3))
```

#### Which variation of defn should I use?

You should consider using `defn$` first, as it can be used to provide type checking during development/testing, while not providing any runtime overhead in production.  Use `defna` when you have a low-level function that needs to make use of loop/recur.  Use `defnv` when you want to guarantee that a call to a function in production will throw an exception if given data with the wrong shape. Finally, `defn'` is not needed in most cases.

### Anonymous functions

Annotate provides four variations of [fn](http://clojuredocs.org/clojure.core/fn): [fn'](http://roomkey.github.io/annotate/annotate.fns.html#var-fn.27), [fna](http://roomkey.github.io/annotate/annotate.fns.html#var-fna), [fnv](http://roomkey.github.io/annotate/annotate.fns.html#var-fnv), and [fn$](http://roomkey.github.io/annotate/annotate.fns.html#var-fn.24).

There behavior is identical to the `defn` variations.

```clojure
((fnv [String => String] [x] x) "Bob")
;; "Bob"
((fnv ([String => String] [String String => String]) ([x] x) ([x y] (str x y))) "Billy" "Bob")
;; "BillyBob"
((fnv [String => String] [x] 20) "Bob")
;; ExceptionInfo Failed to type check anonymous output: (not (instance? String 20))
```

### Recursive types

Recursive types can represented by referencing the var object of the type you are defining. Let's take a look at an example.

```clojure
(def Element
  {:tag Keyword
   :attrs {Keyword String}
   :content (Seqable (U String #'Element))})

(check Element
       {:tag :a
        :attrs {:b "c"}
        :content ["d" "e" {:tag :f :attrs {} :content ["g"]}]})
;; nil
```

### Wrap an existing function outside your control

Annotate allows you to wrap existing functions outside of your control in a type checking function. There are three variations: [wrap'](http://roomkey.github.io/annotate/annotate.wrap.html#var-wrap.27), [wrapv](http://roomkey.github.io/annotate/annotate.wrap.html#var-wrapv), and [wrap$](http://roomkey.github.io/annotate/annotate.wrap.html#var-wrap.24). If you just want to annotate a function without any type checking use [ann](http://roomkey.github.io/annotate/annotate.core.html#var-ann).

```clojure
(use 'annotate.wrap)

(defn ping
  "Make an HTTP request."
  [url & {:keys [method timeout] :or {method "GET"}}]
  (str "url: " url ", method: " (name method) ", timeout: " timeout))

(wrap' ping [String & (Pairs :method Named :timeout Int) => String])

(with-checking (ping "localhost" :method :POST))
;; "url: localhost, method: POST, timeout: "

(with-checking (ping "localhost" :timeout 100.0))
;; Exceptioninfo Failed to type check ping input(s): {:timeout (not (integer? 100.0))}
```

Notice the usage of the [Pairs](http://roomkey.github.io/annotate/annotate.types.html#var-Pairs) type to represent keyword arguments. This is due to a technical limitation, but provides the same behavior as using [KwA](http://roomkey.github.io/annotate/annotate.types.html#var-KwA) for a function under your control.

### Friendly errors

Annotate can be used to check user input and return user-friendly error messages, if needed. The [label](http://roomkey.github.io/annotate/annotate.friendly.html#var-label) function is used to provide an error message and not found message for a particular map key. The [friendly](http://roomkey.github.io/annotate/annotate.friendly.html#var-friendly) function recursively transforms a map of errors into a map of errors where the values are understandable error messages.

```clojure
(use 'annotate.friendly)

(-> (check {:first-name (NonEmpty String)
            :age Int}
           {:first-name ""})
    (friendly {:first-name (label "First name is invalid" "First name is missing")
               :age (label "Age is invalid" "Age not found")}))
;; {:age "Age not found", :first-name "First name is invalid"}
```

### Type validation

If you are curious whether a particular type is a valid type, you can call [valid-type?](http://roomkey.github.io/annotate/annotate.core.html#var-valid-type.3F) on that type.

```clojure
(valid-type? #{String})
;; true

(valid-type? #{String Keyword})
;; false
```

Notice that a set with two elements is not a valid type.

## Presentation

To get you up to speed as quick as possible, we've provided an interactive presentation that was given at Room Key.

Run `lein gorilla` then type `Ctrl-g Ctrl-l` in the worksheet to load 'presentation.clj'.
