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
(validate String "Billy")
;; @@

;; @@
(validate String :Billy)
;; @@

;; @@
(validate Int 42)
;; @@

;; @@
(validate 1 1)
;; @@

;; @@
(validate 1 2)
;; @@

;; **
;;; ##Vectors
;; **

;; @@
(validate [true] [true true])
;; @@

;; @@
(validate [true] [])
;; @@

;; @@
(validate [true] [true false true])
;; @@

;; @@
(validate [String] ["Billy" "Bobby"])
;; @@

;; @@
(validate [String] ["Billy" "Bobby" :Franky])
;; @@

;; @@
(validate [Int] (range 100))
;; @@

;; @@
(validate [[Keyword Int]] [[:Joey 10] [:Billy 9]])
;; @@

;; **
;;; ##Sets
;; **

;; @@
(validate #{Keyword} #{:Billy :Bobby})
;; @@

;; @@
(validate #{Keyword} #{:Billy "Bobby"})
;; @@

;; @@
(not (instance? Keyword "Bobby"))
;; @@

;; **
;;; ##Maps
;; **

;; @@
(validate {Keyword String} {:name "Billy", :email "billy@gmail.com"})
;; @@

;; @@
(validate {Keyword String} {:name :Billy, :email "billy@gmail.com"})
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
(validate User {:name "Billy"
                :address {:city "San Diego"
                          :state "CA"}
                :likes [:biking :hiking]
                :age 36})
;; @@

;; @@
(validate User {:name "Billy"
                :address {:city "San Diego"
                          :state "CA"}})
;; @@

;; @@
(validate User {:name :Billy
                :email "billy@example.org"
                :address {:city "San Diego"
                          :state :CA}
                :likes [:biking "hiking"]})
;; @@

;; @@
(validate {"name" String, 'age Int} {"name" "Billy", 'age 36})
;; @@

;; @@
(valid-type? {:name String Keyword Int})
;; @@

;; **
;;; ##Predicates
;; **

;; @@
(validate (Pred odd?) 3)
;; @@

;; @@
(validate (Pred empty?) [1])
;; @@

;; **
;;; ##Regular Expressions
;; **

;; @@
(def CVV #"[0-9]{3,4}")
;; @@

;; @@
(validate CVV "123")
;; @@

;; @@
(validate CVV "a12")
;; @@

;; **
;;; ##Union
;; **

;; @@
(validate (U Keyword Symbol String) :billy)
;; @@

;; @@
(validate (U Keyword Symbol String) "billy")
;; @@

;; @@
(validate (U Keyword String) 5)
;; @@

;; **
;;; ##Intersection
;; **

;; @@
(validate (I Int (Pred even?)) 2)
;; @@

;; @@
(validate (I Int (Pred even?)) 3)
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
(with-validation (append* "hi" " there"))
;; @@

;; @@
(with-validation (append* "hi" :there))
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
(validate {:first-name (NonEmpty String)
           :age Int}
          {:first-name ""})
;; @@

;; @@
(-> (validate {:first-name (NonEmpty String)
               :age Int}
              {:first-name ""})
    (friendly {:first-name (label "First name is invalid" "First name is missing")
               :age (label "Age is invalid" "Age not found")}))
;; @@
