(ns annotate.util-test
  #?@(:cljs [(:require-macros [cljs.test :refer [deftest testing is]])
             (:require [cljs.test])])

  (:use [annotate.util
         #?@(:cljs [:only [quote-special truncate parse-doc-attr-map
                           remove-pre-post arity-match? rest-args? parse-arglist]])]
        #?(:clj [clojure.test])))

(deftest t-truncate
  (is (= 36 (truncate 36)))
  (is (= "A" (truncate "A")))
  (is (= {:age 36, :name "David"} (truncate {:name "David", :age 36})))
  (is (= #?(:clj {0 0, 121 121, 65 65, 70 70, 62 62, '... '...}
            :cljs {0 0, 32 32, 64 64, 96 96, 128 128, '... '...})
        (truncate (zipmap (range 200) (range 200)))))
  (is (= {:scores [0 1 2 3 4 '...]} (truncate {:scores (vec (range 100))})))
  (is (= [0 1 2 3 4 '...] (truncate (vec (range 200)))))
  (is (= #?(:clj #{0 121 65 70 62 '...}
            :cljs #{0 32 64 96 128 '...}) (truncate (set (range 200)))))
  (is (= (list 0 1 2 3 4 '...) (truncate (range 200))))
  (is (= "xxxxxxxxxxxxxxxxxxxx...") (truncate (apply str (repeat 50 \x)))))

(deftest t-parse-arglist
  (is (= [(list 'String) 'String] (parse-arglist (quote [String => String]))))
  (is (= [(list 'String) nil] (parse-arglist (quote [String =>]))))
  (is (= [(list) 'String] (parse-arglist (quote [=> String]))))
  (is (= [(list 'String '& (list 'Seq 'String)) 'String]
        (parse-arglist (quote [String & (Seq String) => String])))))

(deftest t-parse-doc-attr-map
  (is (= (list nil nil ['x] 'x) (parse-doc-attr-map (quote ([x] x)))))
  (is (= (list "doc string" nil ['x] 'x)
        (parse-doc-attr-map (quote ("doc string" [x] x)))))
  (is (= (list nil {:added "1.0"} ['x] 'x)
        (parse-doc-attr-map (quote ({:added "1.0"} [x] x)))))
  (is (= (list "doc string" {:added "1.0"} ['x] 'x)
        (parse-doc-attr-map (quote ("doc string" {:added "1.0"} [x] x)))))
  (is (= (list "doc string" nil (list ['x] 'x) (list ['x 'y] 'y))
        (parse-doc-attr-map (quote ("doc string" ([x] x) ([x y] y))))))
  (is (= (list nil {:added "1.0"} (list ['x] 'x) (list ['x 'y] 'y))
        (parse-doc-attr-map (quote ({:added "1.0"} ([x] x) ([x y] y))))))
  (is (= (list "doc string" {:added "1.0"} (list ['x] 'x) (list ['x 'y] 'y))
        (parse-doc-attr-map (quote ("doc string" {:added "1.0"} ([x] x) ([x y] y)))))))

(deftest t-remove-pre-post
  (is (= (list {:pre [(list 'string? 'x)]})
       (remove-pre-post (quote ({:pre [(string? x)]})))))
  (is (= (list {:post [(list 'number? '%)]})
        (remove-pre-post (quote ({:post [(number? %)]})))))
  (is (= (list {:validate [(list 'string? 'x)]} 'x)
        (remove-pre-post (quote ({:validate [(string? x)]} x)))))
  (is (= (list 'x) (remove-pre-post (quote ({:pre [(string? x)]} x)))))
  (is (= (list 'x) (remove-pre-post (quote ({:post [(number? %)]} x)))))
  (is (= (list 'x)
        (remove-pre-post (quote ({:pre [(string? x)], :post [(number? %)]} x)))))
  (is (= (list) (remove-pre-post (quote ()))))
  (is (= (list 'x) (remove-pre-post (quote (x)))))
  (is (= (list 'x 'y)) (remove-pre-post (quote (x y)))))

(deftest t-arity-match?
  (testing "basics"
    (is (= ['(String) 'Integer] (parse-arglist ['String '=> 'Integer])))
    (is (= false (rest-args? '[x])))
    (is (= true (rest-args? '[& x])))
    (is (= false (rest-args? '[x y])))
    (is (= true (rest-args? '[x & y]))))
  (is (= true (arity-match? (quote ([x])) (quote [String => String]))))
  (is (= false (arity-match? (quote ([x y])) (quote [String => String]))))
  (is (= false (arity-match? (quote ([x])) (quote [String Int => String]))))
  (is (= true (arity-match? (quote ([x y])) (quote [String Int => String]))))
  (is (=  true
        (arity-match? (quote ([x] [x y])) (quote ([String => String] [String Int => String])))))
  (is (= true
        (arity-match? (quote ([x] [x & args])) (quote ([String => String] [String & (Seq Int) => String])))))
  (is (= false
        (arity-match? (quote ([x] [x args])) (quote ([String => String] [& (Seq Int) => String])))))
  (is (= false
        (arity-match? (quote ([x] [x & args])) (quote ([String => String] [String String Int => String]))))))

(deftest t-quote-special
  (is (=  ['String (list 'quote '=>) 'String]
        (quote-special (quote [String => String]))))
  (is (= ['String (list 'quote '&) (list 'Seq 'String) (list 'quote '=>) 'String]
        (quote-special (quote [String & (Seq String) => String]))))
  (is (= (list
          ['String (list 'quote '=>) 'String]
          ['String
           (list 'quote '&)
           (list 'Seq 'String)
           (list 'quote '=>)
           'String])
        (quote-special (quote ([String => String] [String & (Seq String) => String]))))))
