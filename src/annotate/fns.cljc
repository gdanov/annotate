(ns annotate.fns
  "Type annotations/checking for defn and fn forms."
  #?(:clj (:use [annotate types core util])
     :cljs (:require [annotate.types]
                     [annotate.core :refer [check*]]
                     [annotate.util
                      :refer [remove-amp parse-doc-attr-map parse-arglist
                              remove-pre-post quote-special typecheck?
                              parse-fn-args]]))
  #?(:cljs (:require-macros [annotate.core :refer [ann]])))

#?(:clj
   (do
     (defn- add-checking
       "Add type checking to a fn definition by replacing the params binding
  vector and wrapping the body with type checking code."
       [n [input-types output-type] params body cond-sym]
       ;;(println "##" input-types output-type)
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
       "Define a function with type checking, add type annotation, and
  preserve expected arglists. Returns the new var."
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

     (defrecord fn-body [contract params prepost-map body])

     (defrecord fn-def [name doc-string attr-map bodies])

     (defn has-pre-post? [body-src]
       ;; (defn x [] {:pre ...}) causes pre not to be respected, but
       ;; rather the map is the result
       (and (< 2 (count body-src)) (map? (nth body-src 2))))

     (defn is-arg-contract? [form]
       (some (partial = '=>) form))

     (defn parse-args+
       "given raw arglist spec provide vector of one spec per
  arity. shape of result: [[<param>] <return>] returns the symbols
  it was given, no lookup/deref. DOES NOT support the original multi-arity syntax"
       [args]
       (condp apply [args]
         ;; TODO deprecate
         list?   (mapv (comp first parse-args+) args)
         vector? (let [inp (partition-by #(= '=> %) args)
                       inp (if (= 2 (count inp)) (cons '() inp) inp)]
                   ;; TODO make this work
                   #_(assert* [(Coll) (cons '=> nil) (Coll)] (into [] inp)
                       "invalid contract:" args)
                   (assert (and (= 3(count inp)) (= '(=>) (nth inp 1)))
                     (str "invalid contract:" args))
                   [(into [] (first inp)) (first (last inp))])))

     (defn parse-fn-body
       "parses one arity body. expects:
  ([<contract>] [<params>] {:pre }? .+)"
       [body-src]
       ;; TODO use check* currently impossible due to lack of support
       ;; for lists, optional elements (pre-post) and & rest
       #_(check* (str "function arity def '"(pr-str body-src))
           [(I [Any] (Pred is-arg-contract?))
            [Any]
            {}
            Any]
           (into [] body-src))
       (assert (>= (count body-src) 2) (str "body too-short" (truncate body-src)))
       (assert* (I Vec (Pred is-arg-contract?)) (first body-src)
         "can't validate params contract definition")
       (assert* Vec (nth body-src 1) "can't validate params vector")
       (->fn-body
         (first body-src)
         (nth body-src 1)
         (when (has-pre-post? body-src) (nth body-src 2))
         (nthrest body-src (if (has-pre-post? body-src) 3 2))))

     (defn parse-fn-decl*
       "parses function declaration. does no checks and does not parse the contract"
       [fn-src]
       (-> [fn-src (map->fn-def {})]
         ((fn get-type [[fn-src res]]
            (condp = (first fn-src)
              :fn [(rest fn-src) (assoc res :anonymous true :name (gensym))]
              :defn [(rest fn-src) res])))
         ((fn get-name [[fn-src res]]
            (assert (or (:anonymous res)
                      (and (nil? (:anonymous res)) (symbol? (first fn-src))))
              "defn needs a name")
            (if (symbol? (first fn-src))
              [(rest fn-src) (assoc res :name (first fn-src))]
              [fn-src res])))
         ((fn get-doc-string [[fn-src res]]
            (if (and (not (:anonymous res))
                  (string? (first fn-src)))
              [(rest fn-src) (assoc res :doc-string (first fn-src))]
              [fn-src res])))
         ;; TODO meta is ignored atm
         ((fn get-body* [[fn-src res]]
            (assoc res :bodies
              (cond
                (every? list? fn-src) (mapv parse-fn-body fn-src)
                (vector? (first fn-src)) [(parse-fn-body fn-src)]
                :else (assert false (str "can't parse params:" (truncate fn-src)))))))))

     (defn drop-& [params]
       (filter #(not= '& %) params))

     (defn add-checking*
       [fn-name contract params body check-cond-form]
       (let [[inp-contract out-contract] (parse-args+ contract)]
         (assert (= (count inp-contract) (count (drop-& params)))
           (str "contract arity does not match parameter count" contract params))
         `((when ~check-cond-form
             (check* (str "inputs for " ~fn-name) ~@(interleave inp-contract (drop-& params))))
           (let [result# (do ~@body)]
             (when ~check-cond-form (check* (str "output of " ~fn-name) ~out-contract result#))
             result#))))

     (defn- add-contract
       "adds the contract as meta data on the fn value. the contract
       is intended to point to the type instances, not to vars or symbols"
       [fn-decl contract]
       (println "C:" contract)
       `(vary-meta ~fn-decl assoc ~:annotate.types/contract ~contract))

     (defmacro fnvv [& form]
       (let [fn-def (parse-fn-decl* (cons :fn form))]
         `(fn ~(:name fn-def)
            ~@(map (fn [%] `(~(:params %)
                             ;; add-checking
                             ~@(add-checking* (:name fn-def) (:contract %) (:params %)
                                 (:body %) true)))
                (:bodies fn-def)))))

     (defn- fn-internal
       "Define a function with type checking, add type annotation, and
  preserve expected arglists. Returns the new var."
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
           `(fn ~@b)
           ;;(add-contract `(fn ~n ~@b) (parse-args+ t))
           ;;(add-contract `(fn ~@b) (parse-args+ t))
           )))

     (defmacro defn'
       "Define a function, passing the type annotation after the name of the
  function. Type annotations for fns must be wrapped in vectors or
  lists. Lists indicate a multi-arty fn and should contain two or more
  vector forms. Enable type checking by calling inside the with-checking
  macro. Pre/post conditions are removed."
       [n t & args]
       (defn* n t args 'annotate.core/*checking-enabled*))

     (defmacro defna
       "Define annotated function.

  Like defn', but without type checking. Source code is not modified in any
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

  Like fn', but without type checking. Source code is not modified in
  any way."
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
     ))
