(ns annotate.types-macro
  (:use [annotate core util]))

(defmacro Pred
  "Defines a type determined by the given predicate fn. It’s values are
  the set of values for which the predicate is truthy."
  [f]
  `(annotate.types/PredicateType. ~(if (symbol? f)
                      `(fq-ns (var ~f))
                      `(quote ~f))
     ~f))

(defmacro defcan
  "Define a Canonical type."
  [simple canonical]
  `(def ~simple (annotate.types/CanonicalType. '~simple ~canonical)))

(defmacro augment-constructor
  "augments cljs or js constructor *itself* so that it implementss the
  Typeable contract. Could be used also on non-constructor when the
  type is not predefined"
  [constructor]
  `(cljs.core/specify! ~constructor
    ann/Typeable
    (display-type [this#]
      (let [{:keys [~'ns ~'name]} (meta (var ~constructor))]
        (if *canonical-name*
          (symbol (str ~'ns "/" ~'name))
          (symbol ~'name))))
    (valid-type? [this#] true)
    (check [this# that#]
      (when-not (and (some? that#) (identical? this# (type that#)))
        (list '~'not (list '~'instance? (display-type this#) (truncate that#)))))))

(defmacro augment-type
  "augments cljs or js type(constructor) so that all it's *instances*
  implement the default Typeable contract. "
  [orig-nme]
  `(extend-protocol annotate.core/Typeable
     ~orig-nme
     (display-type [this#] (if (annotate.util/clj-obj? this#)
                             this# (.valueOf this#)))
     (valid-type? [_#] true)
     (check [this# that#]
       (when-not (or
                   (and (annotate.util/cljs-obj? this#)
                     (= (.valueOf this#) that#))
                   ;; for clj instances
                   (= this# that#))
         (list '~'not= this# that#)))))

;; TODO maybe better name?
(defmacro augment-primitive
  [sym]
  `(do
     (augment-constructor ~sym)
     (augment-type ~sym)
     ~sym))

(defmacro defprimitive
  "Defines 'primitive' type for ClojureScript the same way Clojure has
  String, Keyword and co. Needed as in cljs types translate to
  constructor functions and not classes and I haven't figured out how
  to extend them -- this is problem when I check types, not
  instances. Defines contract with the <nme> and the
  predicate. Extends the typedef when <orig-nme> is provided."
  ([nme pred]
   `(def ~nme (annotate.types/PrimitiveType. '~nme '~(symbol (name (ns-name *ns*)) (name nme)) ~pred)))
  ([nme orig-nme pred]
   `(do
      ;; instrument the instances/values
      (augment-type ~orig-nme)
      ;; define the type itself
      (def ~nme (annotate.types/PrimitiveType. '~nme '~orig-nme ~pred)))))

(defmacro IFn
  "Ordered intersection of function arities. Useful for documenting
  higher order functions. Actual input and output types are NOT
  checked."
  [& args]
  `(annotate.types/IFnType. (list ~@(map quote-special args))))
