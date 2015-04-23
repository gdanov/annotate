(ns annotate.util-test
  (:use midje.sweet annotate.util))

(facts "truncate"
  (fact (truncate {:name "David", :age 36}) => {:age 36, :name "David"})
  (fact (truncate (zipmap (range 200) (range 200))) =>
    {0 0, 121 121, 65 65, 70 70, 62 62, '... '...})
  (fact (truncate {:scores (vec (range 100))}) => {:scores [0 1 2 3 4 '...]})
  (fact (truncate (vec (range 200))) => [0 1 2 3 4 '...])
  (fact (truncate (set (range 200))) => #{0 121 65 70 62 '...})
  (fact (truncate (range 200)) => (list 0 1 2 3 4 '...))
  (fact (truncate (apply str (repeat 50 \x))) => "xxxxxxxxxxxxxxxxxxxx..."))
(facts "parse-arglist"
  (fact (parse-arglist (quote [String => String])) => [(list 'String) 'String])
  (fact (parse-arglist (quote [String =>])) => [(list 'String) nil])
  (fact (parse-arglist (quote [=> String])) => [(list) 'String])
  (fact (parse-arglist (quote [String & (Seq String) => String])) =>
    [(list 'String '& (list 'Seq 'String)) 'String]))
(facts "parse-doc-attr-map"
  (fact (parse-doc-attr-map (quote ([x] x))) => (list nil nil ['x] 'x))
  (fact (parse-doc-attr-map (quote ("doc string" [x] x))) =>
    (list "doc string" nil ['x] 'x))
  (fact (parse-doc-attr-map (quote ({:added "1.0"} [x] x))) =>
    (list nil {:added "1.0"} ['x] 'x))
  (fact (parse-doc-attr-map (quote ("doc string" {:added "1.0"} [x] x))) =>
    (list "doc string" {:added "1.0"} ['x] 'x))
  (fact (parse-doc-attr-map (quote ("doc string" ([x] x) ([x y] y)))) =>
    (list "doc string" nil (list ['x] 'x) (list ['x 'y] 'y)))
  (fact (parse-doc-attr-map (quote ({:added "1.0"} ([x] x) ([x y] y)))) =>
    (list nil {:added "1.0"} (list ['x] 'x) (list ['x 'y] 'y)))
  (fact (parse-doc-attr-map (quote ("doc string" {:added "1.0"} ([x] x) ([x y] y)))) =>
    (list "doc string" {:added "1.0"} (list ['x] 'x) (list ['x 'y] 'y))))
(facts "remove-pre-post"
  (fact (remove-pre-post (quote ({:pre [(string? x)]}))) =>
        (list {:pre [(list 'string? 'x)]}))
  (fact (remove-pre-post (quote ({:post [(number? %)]}))) =>
        (list {:post [(list 'number? '%)]}))
  (fact (remove-pre-post (quote ({:validate [(string? x)]} x))) =>
        (list {:validate [(list 'string? 'x)]} 'x))
  (fact (remove-pre-post (quote ({:pre [(string? x)]} x))) => (list 'x))
  (fact (remove-pre-post (quote ({:post [(number? %)]} x))) => (list 'x))
  (fact (remove-pre-post (quote ({:pre [(string? x)], :post [(number? %)]} x))) =>
        (list 'x))
  (fact (remove-pre-post (quote ())) => (list))
  (fact (remove-pre-post (quote (x))) => (list 'x))
  (fact (remove-pre-post (quote (x y))) => (list 'x 'y)))
(facts "arity-match?"
  (fact (arity-match? (quote ([x])) (quote [String => String])) => true)
  (fact (arity-match? (quote ([x y])) (quote [String => String])) => false)
  (fact (arity-match? (quote ([x])) (quote [String Int => String])) => false)
  (fact (arity-match? (quote ([x y])) (quote [String Int => String])) => true)
  (fact (arity-match? (quote ([x] [x y])) (quote ([String => String] [String Int => String]))) =>
    true)
  (fact (arity-match? (quote ([x] [x & args])) (quote ([String => String] [String & (Seq Int) => String]))) =>
    true)
  (fact (arity-match? (quote ([x] [x args])) (quote ([String => String] [& (Seq Int) => String]))) =>
    false)
  (fact (arity-match? (quote ([x] [x & args])) (quote ([String => String] [String String Int => String]))) =>
    false))
(facts "quote-special"
  (fact (quote-special (quote [String => String])) =>
    ['String (list 'quote '=>) 'String])
  (fact (quote-special (quote [String & (Seq String) => String])) =>
    ['String (list 'quote '&) (list 'Seq 'String) (list 'quote '=>) 'String])
  (fact (quote-special (quote ([String => String] [String & (Seq String) => String]))) =>
    (list
     ['String (list 'quote '=>) 'String]
     ['String
      (list 'quote '&)
      (list 'Seq 'String)
      (list 'quote '=>)
      'String])))
