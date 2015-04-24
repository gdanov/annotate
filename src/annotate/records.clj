(ns annotate.records
  "Create records with type checking constructor functions."
  (:use [annotate fns util]))

(defn- defrecord*
  [fn-type n t fields args]
  `(do
     (defrecord ~n ~fields ~@args)
     (~fn-type ~(symbol (str "->" n)) ~(conj t '=> n)
               ~(str "Positional factory function for class " n ".")
               ~fields
               (new ~n ~@fields))))

(defmacro defrecord'
  "Define a record, passing the type annotation after the name of
  record. Creates a positional factory function with type checking
  characteristics matching the appropriate defn macro."
  [n t fields & args]
  (defrecord* 'annotate.fns/defn' n t fields args))

(defmacro defrecorda
  "Define a record with an annotated factory function.

  Like defrecord', but without type checking. Source code is not modified
  in any way."
  [n t fields & args]
  (defrecord* 'annotate.fns/defna n t fields args))

(defmacro defrecordv
  "Define a record with a type checked factory function.

  Like defrecord', but inputs/output are always type checked."
  [n t fields & args]
  (defrecord* 'annotate.fns/defnv n t fields args))

(defmacro defrecord$
  "Define a record with a type checked factory function.

  Like defrecord', but inputs/output are always type checked. Set the
  system property annotate.typecheck to 'on' to generate an always
  type checked function, or to 'off' to generate an annotated only
  function. Defaults to 'off'."
  [n t fields & args]
  (if (typecheck?)
    `(defrecordv ~n ~t ~fields ~@args)
    `(defrecorda ~n ~t ~fields ~@args)))
