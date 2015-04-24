;; gorilla-repl.fileformat = 1

;; **
;;; # annotate
;;;
;;; What is it good for?
;;;
;;; * Documentation
;;; * Catching bugs
;;; * Testing
;;; * Validating user data
;;;
;;; ##Basics
;; **

;; @@
(ns juicy-temple
  (:use [annotate core types] clojure.pprint))
;; @@

;; @@
(check String "Billy")
;; @@

;; @@
(check String :Billy)
;; @@

;; @@
(check Int 42)
;; @@

;; @@
(check 1 1)
;; @@

;; @@
(check 1 2)
;; @@

;; **
;;; ##Vectors
;; **

;; @@
(check [true] [true true])
;; @@

;; @@
(check [true] [])
;; @@

;; @@
(check [true] [true false true])
;; @@

;; @@
(check [String] ["Billy" "Bobby"])
;; @@

;; @@
(check [String] ["Billy" "Bobby" :Franky])
;; @@

;; @@
(check [Int] (range 100))
;; @@

;; @@
(check [[Keyword Int]] [[:Joey 10] [:Billy 9]])
;; @@

;; **
;;; ##Sets
;; **

;; @@
(check #{Keyword} #{:Billy :Bobby})
;; @@

;; @@
(check #{Keyword} #{:Billy "Bobby"})
;; @@

;; @@
(not (instance? Keyword "Bobby"))
;; @@

;; **
;;; ##Maps
;; **

;; @@
(check {Keyword String} {:name "Billy", :email "billy@gmail.com"})
;; @@

;; @@
(check {Keyword String} {:name :Billy, :email "billy@gmail.com"})
;; @@

;; @@
(def User
  {:name String
   (optional-key :email) String
   :address {:city String
             :state String}
   :likes [Keyword]})
;; @@

;; @@
(check User {:name "Billy"
             :address {:city "San Diego"
                       :state "CA"}
             :likes [:biking :hiking]
             :age 36})
;; @@

;; @@
(check User {:name "Billy"
             :address {:city "San Diego"
                       :state "CA"}})
;; @@

;; @@
(check User {:name :Billy
             :email "billy@example.org"
             :address {:city "San Diego"
                       :state :CA}
             :likes [:biking "hiking"]})
;; @@

;; @@
(check {"name" String, 'age Int} {"name" "Billy", 'age 36})
;; @@

;; @@
(valid-type? {:name String Keyword Int})
;; @@

;; **
;;; ##Predicates
;; **

;; @@
(check (Pred odd?) 3)
;; @@

;; @@
(check (Pred empty?) [1])
;; @@

;; **
;;; ##Regular Expressions
;; **

;; @@
(def CVV #"[0-9]{3,4}")
;; @@

;; @@
(check CVV "123")
;; @@

;; @@
(check CVV "a12")
;; @@

;; **
;;; ##Union
;; **

;; @@
(check (U Keyword Symbol String) :billy)
;; @@

;; @@
(check (U Keyword Symbol String) "billy")
;; @@

;; @@
(check (U Keyword String) 5)
;; @@

;; **
;;; ##Intersection
;; **

;; @@
(check (I Int (Pred even?)) 2)
;; @@

;; @@
(check (I Int (Pred even?)) 3)
;; @@

;; **
;;; ##Functions
;; **

;; @@
(use 'annotate.fns)
;; @@

;; @@
(defnv append [String String => String]
  [s1 s2] (str s1 s2))
;; @@

;; @@
(pprint (macroexpand '(defnv append [String String => String] [s1 s2] (str s1 s2))))
;; @@

;; @@
(annotation append)
;; @@

;; @@
(canonical append)
;; @@

;; @@
(-> (var append) meta :doc)
;; @@

;; @@
(defnv greeting ([=> String] [String => String])
  "Doc string"
  ([] (greeting "world"))
  ([msg] (str "Hello, " msg)))
;; @@

;; @@
(greeting)
;; @@

;; @@
(greeting "Bob")
;; @@

;; @@
(greeting :hello)
;; @@

;; @@
(defn' append* [String & (Seq String) => String]
  [s1 & args] (apply str s1 args))
;; @@

;; @@
(with-checking (append* "hi" " there"))
;; @@

;; @@
(with-checking (append* "hi" :there))
;; @@

;; @@
(defn$ ping [String & (KwA :method Named :timeout Int) => String]
  [url & {:keys [method timeout] :or {timeout 10}}]
  (str "url: " url ", method: " (name method) ", timeout: " timeout))
;; @@

;; @@
(ping "localhost" :method :POST)
;; @@

;; @@
(ping "localhost" :method "POST")
;; @@

;; @@
(ping "localhost" :method :POST :timeout 100.0)
;; @@

;; @@
(defnv non-empty [String => String]
  [s]
  (when (> (count s) 0) s))
;; @@

;; @@
(non-empty "")
;; @@

;; **
;;; ##Records
;; **

;; @@
(use 'annotate.records)
;; @@

;; @@
(defrecordv Person [String String]
  [first-name last-name])
;; @@

;; @@
(->Person "Billy" "Bob")
;; @@

;; @@
(->Person :Billy :Bob)
;; @@

;; **
;;; ##Anonymous functions
;; **

;; @@
((fnv [String => String] [x] x) "Bob")
;; @@

;; @@
((fnv [String => String] [x] 20) "Bob")
;; @@

;; **
;;; ##Testing
;; **

;; @@
(use '[annotate.test.midje :only [valid]])
;; @@

;; @@
(valid "append* appends" (append* "Billy" " " "Bob"))
;; @@

;; @@
(valid "append* appends" (append* "Billy" " " :Bob))
;; @@

;; **
;;; ##Friendly Errors
;; **

;; @@
(use 'annotate.friendly)
;; @@

;; @@
(check {:first-name (NonEmpty String)
           :age Int}
          {:first-name ""})
;; @@

;; @@
(-> (check {:first-name (NonEmpty String)
            :age Int}
           {:first-name ""})
    (friendly {:first-name (label "First name is invalid" "First name is missing")
               :age (label "Age is invalid" "Age not found")}))
;; @@
