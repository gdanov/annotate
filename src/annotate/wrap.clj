(ns annotate.wrap
  "Wrap existing functions with validating functions."
  (:use [annotate core fns util]))

(defn- wrap-fn-call [curr-var type]
  (let [[input-types] (parse-arglist type)
        penultimate-type
        (first (drop (- (count input-types) 2) input-types))
        rest-args? (= penultimate-type '&)
        input-syms
        (map (fn [x] (if (= x '&) x (gensym "input"))) input-types)
        fn-args (remove #(= % '&) input-syms)
        fn-call `(~curr-var ~@fn-args)
        fn-call (if rest-args?
                  (cons 'apply fn-call)
                  fn-call)]
    (list (vec input-syms) fn-call)))

(defn- wrap*
  [sym type fn-type]
  (let [curr-var (gensym)
        wrapped-fn
        (if (vector? type)
          (let [[input-syms fn-call] (wrap-fn-call curr-var type)]
            `(~fn-type ~sym ~type ~input-syms ~fn-call))
          `(~fn-type ~sym ~type ~@(map #(wrap-fn-call curr-var %) type)))]
    `(do (alter-var-root (var ~sym) (fn [~curr-var] ~wrapped-fn))
         (ann ~sym ~(quote-special type))
         (assert-arity-match '~sym (lookup-arglists ~sym) '~type)
         (var ~sym))))

(defmacro wrap'
  "Wrap a function with a validating function. Enable validation by
  calling inside the with-validation macro."
  [sym type]
  (wrap* sym type 'annotate.fns/fn'))

(defmacro wrapv
  "Like wrap' but inputs/output are always validated."
  [sym type]
  (wrap* sym type 'annotate.fns/fnv))

(defmacro wrap$
  "Like wrap' but inputs/output are optionally validated. Set the system
  property annotate.typecheck to 'on' to generate an always validated
  function, or to 'off' to only annotate the function. Defaults to
  'off'."
  [sym type]
  (if (typecheck?)
    `(wrap* ~sym ~type 'annotate.fns/fnv)
    `(do (ann ~sym ~(quote-special type))
         (var ~sym))))
