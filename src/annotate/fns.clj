(ns annotate.fns
  "Type checking defn and fn forms."
  (:require annotate.types)
  (:use [annotate core util]))

(defn- add-checking
  "Add type checking to a fn definition by replacing the params binding
  vector and wrapping the body with type checking code."
  [n [input-types output-type] params body cond-sym]
  (let [input-syms
        (map (fn [x] (if (= x '&) x (gensym "input")))
             params)
        last-two-inputs
        (drop (- (count params) 2) params)
        keyword-args?
        (let [[y z] last-two-inputs]
          (and (= y '&) (map? z)))
        params-bindings
        (if keyword-args?
          (reduce (fn [[prev acc] x]
                    (if (= prev '&)
                      (conj acc {:as x})
                      [x (conj acc x)]))
                  [nil []]
                  input-syms)
          (vec input-syms))
        renamed-bindings
        (vec (interleave (remove-amp params)
                         (remove-amp input-syms)))
        input-type-pairs
        (interleave (remove-amp input-types)
                    (remove-amp input-syms))]
    `(~params-bindings
      (when ~cond-sym
        (check* (str ~n " input(s)") ~@input-type-pairs))
      (let [output# (let ~renamed-bindings ~@body)]
        (when ~cond-sym
          (check* (str ~n " output") ~output-type output#))
        output#))))

(defn- defn*
  "Define a function with validation, add type annotation, and preserve
  expected arglists.  Returns the new var."
  [n t args cond-sym]
  (let [[doc-string attr-map params & body :as pb] (parse-doc-attr-map args)
        params-body (drop 2 pb)
        fq-name `(fq-ns (var ~n))
        b (if (vector? params)
            (add-checking fq-name (parse-arglist t) params (remove-pre-post body) cond-sym)
            (->> params-body
                 (interleave t)
                 (partition 2)
                 (map (fn [[t [params & body]]]
                        (add-checking fq-name (parse-arglist t) params (remove-pre-post body) cond-sym)))))
        arglists (if (vector? params) params
                     (map first params-body))]
    `(do
       (defn ~n ~@(concat (remove nil? (list doc-string attr-map)) b))
       (ann ~n ~(quote-special t))
       (fix-arglists (var ~n) '~arglists)
       (assert-arity-match '~n (lookup-arglists ~n) '~t)
       (var ~n))))

(defmacro defn'
  "Define a function, passing the type annotation after the name of the
  function. Type annotations for fns must be wrapped in vectors or
  lists. Lists indicate a multi-arty fn and should contain two or more
  vector forms. Enable validation by calling inside the with-validation
  macro. Pre/post conditions are removed."
  [n t & args]
  (defn* n t args 'annotate.core/*checking-enabled*))

(defmacro defna
  "Define annotated function.

  Like defn', but without validation. Source code is not modified in any
  way."
  [n t & args]
  `(do
     (defn ~n ~@args)
     (ann ~n ~(quote-special t))
     (assert-arity-match '~n (lookup-arglists ~n) '~t)
     (var ~n)))

(defmacro defnv
  "Define a type checked function.

  Like defn' but inputs/output are always type checked."
  [n t & args]
  (defn* n t args true))

(defmacro defn$
  "Define a function, passing the type annotation after the name of the
  function. Type annotations for fns must be wrapped in vectors or
  lists. Lists indicate a multi-arty fn and should contain two or more
  vector forms. Set the system property annotate.typecheck to 'on' to
  generate an always type checked function, or to 'off' to generate an
  annotated only function.  Defaults to 'off'. Pre/post conditions are
  removed."
  [n t & args]
  (if (typecheck?)
    `(defnv ~n ~t ~@args)
    (let [[doc-string attr-map params & body :as pb] (parse-doc-attr-map args)
          args (if (vector? params)
                 (concat (remove nil? (list doc-string attr-map params))
                         (remove-pre-post body))
                 (->> (drop 2 pb)
                      (map (fn [[params & body]]
                             (list* params (remove-pre-post body))))
                      (concat (remove nil? (list doc-string attr-map)))))]
      `(defna ~n ~t ~@args))))

(defn- fn-internal
  "Define a function with validation, add type annotation, and preserve
  expected arglists.  Returns the new var."
  [n t args cond-sym]
  (let [[params & body] args
        fn-name (if n `'~n "anonymous")
        b (if (vector? params)
            (add-checking fn-name (parse-arglist t) params body cond-sym)
            (->> (interleave t args)
                 (partition 2)
                 (map (fn [[t [params & body]]]
                        (add-checking fn-name (parse-arglist t) params body cond-sym)))))]
    (if n
      `(fn ~n ~@b)
      `(fn ~@b))))

(defmacro fn'
  "Define a function, passing the type annotation after the optional
  name of the anonymous function. Type annotations for fns must be
  wrapped in vectors or lists. Lists indicate a multi-arty fn and should
  contain two or more vector forms."
  [n-or-t & args]
  (let [[n t args] (parse-fn-args n-or-t args)]
    (fn-internal n t args 'annotate.core/*checking-enabled*)))

(defmacro fna
  "Define annotated anonymous function.

  Like fn', but without validation. Source code is not modified in any
  way."
  [n-or-t & args]
  (let [[n t args] (parse-fn-args n-or-t args)]
    (if n
      `(fn ~n ~args)
      `(fn ~args))))

(defmacro fnv
  "Define a type checked anonymous function.

  Like fn' but inputs/output are always type checked."
  [n-or-t & args]
  (let [[n t args] (parse-fn-args n-or-t args)]
    (fn-internal n t args true)))

(defmacro fn$
  "Define a function, passing the type annotation after the optional
  name of the anonymous function. Type annotations for fns must be
  wrapped in vectors or lists. Lists indicate a multi-arty fn and should
  contain two or more vector forms.  Set the system property
  annotate.typecheck to 'on' to generate an always type checked function,
  or to 'off' to generate an annotated only function.  Defaults to
  'off'."
  [n-or-t & args]
  (if (typecheck?)
    `(fnv ~n-or-t ~@args)
    `(fna ~n-or-t ~@args)))
