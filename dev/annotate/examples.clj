(ns annotate.examples
  (:use [annotate core types fns records wrap friendly]))

;; Never type check
(defna append [String String => String]
  [s1 s2] (str s1 s2))

;; Multi-arity, with doc-string, always type checked
(defnv greeting ([=> String] [String => String])
  "Doc string"
  ([] (greeting "world"))
  ([msg] (str "Hello, " msg)))

;; Rest
(defn' append* [String & (CanSeq String) => String]
  [s1 & args] (apply str s1 args))

;; Keyword args
(defn' ping [String & (KwA :method Named :timeout Int) => String]
  [url & {:keys [method timeout] :or {method "GET"}}]
  (str "url: " url ", method: " (name method) ", timeout: " timeout))

;; Higher order
(defn' map* [Fn (CanSeq) => (LazySeq)]
  {:added "1.0"}
  [f coll] (map f coll))

;; Output type error
(defn' str* [String => String]
  [s]
  (if (> (count s) 0) s nil))

;; defrecord
(defrecordv User [String String]
  [first-name last-name])

;; Type check record keys
(defrecord Person [name])

;; Protocols
(defprotocol Foo
  (foo [this]))

(defrecord Bar [x]
  Foo
  (foo [this] x))

;; Recursive

(def Element
  {:tag Keyword
   :attrs {Keyword String}
   :content (Seqable (U String #'Element))})

;; Wrap
(defn add
  "Add some numbers."
  ([] 0)
  ([x] x)
  ([x y] (+ x y))
  ([x y & zs] (apply + x y zs)))

(wrapv add ([=> Num] [Num => Num] [Num Num => Num] [Num Num & (Seq Num) => Num]))

(defn ping*
  "Make an HTTP request."
  [url & {:keys [method timeout] :or {method "GET"}}]
  (str "url: " url ", method: " (name method) ", timeout: " timeout))

(wrap' ping* [String & (Pairs :method Named :timeout Int) => String])

(defn id [x] x)

(wrap$ id [String => String])
