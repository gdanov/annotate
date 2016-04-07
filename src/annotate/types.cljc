(ns annotate.types
  "Foundational types for building more complex types."
  (:require [clojure.set :as set]
            [annotate.core :as ann
             :refer [display-type valid-type? check Typeable *canonical-name*]]
            [annotate.util :refer [truncate fq-ns quote-special #?(:clj array?)]])
  #?(:cljs (:require-macros [annotate.types-macro :refer
                             [Pred defcan IFn defprimitive augment-type
                              augment-constructor augment-primitive]])
     :clj (:use [annotate.types-macro]))
  #?(:clj (:import [clojure.lang APersistentMap IPersistentList
                    IPersistentSet IPersistentVector
                    IPersistentCollection])))


(defn- not-valid-type [t]
  `(~'not (valid-type? ~(display-type t))))

(defrecord OptionalKey [k]
  Typeable
  (display-type [this] (list 'optional-key k))
  (valid-type? [this] true))

(defn optional-key
  "Indicates that the given key is optional for a map."
  [k]
  (OptionalKey. k))

(defn optional-key? [x]
  (instance? OptionalKey x))

(defrecord PredicateType [pred-sym pred]
  Typeable
  (display-type [this] (list 'Pred pred-sym))
  (valid-type? [this] (fn? pred))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (when-not (pred that)
        `(~'not (~pred-sym ~(truncate that)))))))

(defrecord RequiredKey [k]
  Typeable
  (display-type [this] (list 'required-key k))
  (valid-type? [this] true))

(defn required-key
  "Indicates that the given key is required for a map."
  [k]
  (RequiredKey. k))

(def key-not-found 'key-not-found)

(defn required-key? [x]
  (instance? RequiredKey x))

(defn implicit-required-key? [x]
  ((some-fn string? keyword? symbol?) x))

(defn fixed-key? [x]
  ((some-fn implicit-required-key? required-key? optional-key?) x))

(defn- check-seqable-0
  [this that]
  (when-not (empty? that)
    `(~'not (~'empty? ~(truncate that)))))

(defn- check-seqable-1
  [to-coll this that]
  (let [t (first this)]
    (if (empty? that) nil
        (let [res (map #(check t %) that)]
          (when-not (every? nil? res)
            (to-coll res))))))

(defn- check-sequential
  [{:keys [pred-sym pred]} to-coll this that]
  (if-not (pred that)
    `(~'not (~pred-sym ~(truncate that)))
    (case (count this)
      0 (check-seqable-0 this that)
      1 (check-seqable-1 to-coll this that)
      (if (not= (count this) (count that))
        `(~'not= (~'count ~that) ~(count this))
        (let [res (->> (map vector this that)
                       (map (fn [[t d]] (check t d))))]
          (when-not (every? nil? res)
            (to-coll res)))))))

(defn- sort-map-by-keys
  [m]
  (sort-by (comp not fixed-key? first) m))

(defn- check-fixed-keys
  [kv-pairs that]
  (->> (for [[k v] kv-pairs
             :while (fixed-key? k)]
         (if (or (implicit-required-key? k) (required-key? k))
           (let [k (if (required-key? k) (:k k) k)]
             (if-not (contains? that k)
               [k key-not-found]
               (when-let [res (check v (get that k))]
                 [k res])))
           (let [k (:k k)]
             (when (contains? that k)
               (when-let [res (check v (get that k))]
                 [k res])))))
       (remove nil?)))

(defn- check-generic-type
  [kv-pairs that]
  (let [fixed-keys
        (->> (take-while (comp fixed-key? first) kv-pairs)
             (map first)
             (map #(if (implicit-required-key? %) % (:k %)))
             set)
        [key-type val-type :as generic-type]
        (first (drop-while (comp fixed-key? first) kv-pairs))]
    (when generic-type
      (for [[k v] (apply dissoc that fixed-keys)
            :let [key-res (check key-type k)
                  val-res (check val-type v)]
            :when (or key-res val-res)]
        [(if (nil? key-res) k key-res)
         (if (nil? val-res) v val-res)]))))

#?(:clj
   (extend-protocol ann/Typeable
       Class
       (display-type [this]
         (symbol (if *canonical-name*
                   (.getCanonicalName this)
                   (.getSimpleName this))))
       (valid-type? [this] true)
       (check [this that]
         (when-not (instance? this that)
           `(~'not (~'instance? ~(display-type this)
                    ~(truncate that)))))

       Object
       (display-type [this] this)
       (valid-type? [this] true)
       (check [this that]
         (when-not (= this that)
           `(~'not= ~this ~(truncate that))))))

(extend-protocol ann/Typeable
  nil
  (display-type [this] nil)
  (valid-type? [this] true)
  (check [this that]
    (when-not (= this that)
      `(~'not (~'nil? ~(truncate that)))))

  #?(:clj  java.util.regex.Pattern :cljs js/RegExp)
  (display-type [this] this)
  (valid-type? [this] true)
  (check [this that]
    (cond (not (string? that))
          `(~'not (~'string? ~(truncate that)))
          (not (re-matches this that))
          `(~'not (~'re-matches ~this ~that))))

  #?(:clj APersistentMap :cljs PersistentArrayMap)
  (display-type [this]
    (into {} (map (fn [[k v]] [(display-type k)
                               (display-type v)])
               this)))
  (valid-type? [this]
    (and (or (every? fixed-key? (keys this))
           (<= (count this) 1))
      (every? (fn [[k v]] (and (valid-type? k)
                            (valid-type? v)))
        this)))
  (check [this that]
    (cond (not (valid-type? this)) (not-valid-type this)
          (not (map? that)) `(~'not (~'map? ~(truncate that)))
          (empty? this) (when-not (empty? that)
                          `(~'not (~'empty? ~(truncate that))))
          :else (let [kv-pairs (sort-map-by-keys this)
                      fixed-key-res
                      (check-fixed-keys kv-pairs that)
                      generic-type-res
                      (check-generic-type kv-pairs that)]
                  (when (or (seq fixed-key-res)
                          (seq generic-type-res))
                    (->> (concat fixed-key-res generic-type-res)
                      (into {}))))))

  #?@(:clj
      [IPersistentList
       (display-type [this]
         (apply list (map display-type this)))
       (valid-type? [this] (every? valid-type? this))
       (check [this that]
         (if-not (valid-type? this)
           (not-valid-type this)
           (check-sequential (Pred list?) list* this that)))]
      :cljs
      [cljs.core/List
       (display-type [this]
         (apply list (map display-type this)))
       (valid-type? [this] (every? valid-type? this))
       (check [this that]
         (if-not (valid-type? this)
           (not-valid-type this)
           (check-sequential (Pred list?) list* this that)))
       ;;copy/paste
       cljs.core/EmptyList
       (display-type [this]
         (apply list (map display-type this)))
       (valid-type? [this] (every? valid-type? this))
       (check [this that]
         (if-not (valid-type? this)
           (not-valid-type this)
           (check-sequential (Pred list?) list* this that)))])

  #?(:clj IPersistentVector :cljs PersistentVector)
  (display-type [this]
    (mapv display-type this))
  (valid-type? [this] (every? valid-type? this))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (check-sequential (Pred vector?) vec this that)))

  #?(:clj IPersistentSet
     :cljs PersistentHashSet)
  (display-type [this]
    (set (map display-type this)))
  (valid-type? [this]
    (and (< (count this) 2) (every? valid-type? this)))
  (check [this that]
    (cond (not (valid-type? this)) (not-valid-type this)
          (not (set? that)) `(~'not (~'set? ~(truncate that)))
          :else (case (count this)
                  0 (check-seqable-0 this that)
                  1 (check-seqable-1 set this that))))
  #?(:clj clojure.lang.Var
     :cljs cljs.core/Var)
  (display-type [this] (-> this meta :name))
  (valid-type? [this] true)
  (check [this that] (check @this that)))

#?(:cljs
   (defrecord PrimitiveType [nme fqname pred]
     Typeable
     (display-type [_] (if *canonical-name* fqname nme))
     (valid-type? [_] (and (symbol? nme) (fn? pred)))
     (check [this that]
       (cond (not (valid-type? this))
             (list 'not (list 'and (list 'symbol? nme) (list 'fn? pred)))

             (not (pred that)) `(~'not (~'instance? ~nme ~that))))))

(defrecord CanonicalType [simple canonical]
  ;; TODO defcan won't work in cljs mode as the typedefs are functions,
  ;; not classes, hence can't be mixed-in
  ;; Keyword *instance* will be mixed in, but the Keyword type itself -- not
  ;; hence {Keyword <AnyType>} won't work out of the box with clj defcan

  Typeable
  #?(:clj
     (display-type [this] (if *canonical-name*
                            (display-type canonical)
                            (display-type simple)))
     :cljs
     (display-type [this] (if *canonical-name*
                            (display-type canonical)
                            ;; TODO looks like I don't neet the conditional anymore
                            (display-type simple))))
  (valid-type? [this] (valid-type? canonical))
  (check [this that] (check canonical that)))

(defn Canonical
  "Define a type with a simple and canonical representation. The
  canonical representation will also serve type checking."
  [simple canonical]
  (CanonicalType. simple canonical))

(defrecord UnionType [ts]
  Typeable
  (display-type [this]
    (apply list 'U (map display-type ts)))
  (valid-type? [this] (and (seq ts) (every? valid-type? ts)))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (when-let [errors (loop [errors [] types ts]
                          (if types
                            (when-let [error (check (first types) that)]
                              (recur (conj errors error) (next types)))
                            (seq errors)))]
        (list* 'and errors)))))

(defn U
  "Union represents the disjunction of the given types, similar to
  logical or. Ordering of types is preserved."
  [& ts]
  (UnionType. ts))

(defrecord IntersectionType [ts]
  Typeable
  (display-type [this]
    (apply list 'I (map display-type ts)))
  (valid-type? [this] (and (seq ts) (every? valid-type? ts)))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (some #(check % that) ts))))

(defn I
  "Intersection represents the conjunction of the given types, similar
  to logical and. Ordering of types is preserved."
  [& ts]
  (IntersectionType. ts))

(defrecord AnyType []
  Typeable
  (display-type [this] 'Any)
  (valid-type? [this] true)
  (check [this that] nil))

(def Any (AnyType.))

;; fns that throw exceptions can return Nothing.
;; Nothing is not a value so it can't be examined.

(defrecord NothingType []
  Typeable
  (display-type [this] 'Nothing)
  (valid-type? [this] true)
  (check [this that]
    `(~'not (~'nothing? ~(truncate that)))))

(def Nothing (NothingType.))

(defn nothing?
  "Given some object x, always returns false. Nothing is not a value,
  therefore all values are not Nothing."
  [x]
  false)

(defrecord EqType [v]
  Typeable
  (display-type [this] (list 'Eq v))
  (valid-type? [this] true)
  (check [this that]
    (when-not (= v that)
      `(~'not= ~v ~(truncate that)))))

(defn Eq
  "Define a type with exactly one member. This type is only necessary in
  cases where the type cannot otherwise be expressed. For
  example, [:success] represents a vector of zero or more elements whose
  value is :success. Use (Eq [:success]) to express the exact value."
  [v]
  (EqType. v))

#?(:clj (def Keyword clojure.lang.Keyword)
   :cljs (def Keyword (augment-primitive cljs.core/Keyword)))
#?(:clj (def Symbol clojure.lang.Symbol)
   :cljs (def Symbol (augment-primitive cljs.core/Symbol)))
#?(:clj (def Ratio clojure.lang.Ratio))
#?(:clj (def Atom clojure.lang.Atom)
   :cljs (defcan Atom (Pred #(instance? cljs.core.Atom %))))
#?(:clj (def Date java.util.Date)
   :cljs (def Date (augment-primitive js/Date)))
#?(:clj (def UUID java.util.UUID)) ;; no UUID in js
#?(:clj (defcan Regex java.util.regex.Pattern)
   :cljs (def Regex (augment-constructor js/RegExp)))

#?(:clj (def Num java.lang.Number)
   :cljs (do
           (def Num (augment-primitive js/Number))
           ;; Num just adds confusion
           (def Number Num)))


#?(:clj (def Var clojure.lang.Var)
   :cljs (def Var (augment-constructor cljs.core/Var)))

(defcan Int (Pred integer?))
(defcan Map (Pred map?))
(defcan Vec (Pred vector?))
(defcan Set (Pred set?))
#?(:clj (defcan List (Pred list?))
   :cljs (do
           ;; not sure why the defcan above is needed
           ;;(defprimitive List cljs.core/List list?)
           ))

#?(:cljs (def String (augment-primitive js/String)))
(defcan Named (U Symbol Keyword String))
(defcan Sorted (Pred sorted?))
(defcan Fn (Pred ifn?))

(defrecord IFnType [arglists]
  Typeable
  (display-type [this]
    (apply list 'IFn (map display-type arglists)))
  (valid-type? [this]
    (and (boolean (seq arglists))
         (every? vector? arglists)
         (->> (mapcat #(map valid-type? %) arglists)
              (every? true?))))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (when-not (ifn? that)
        `(~'not (~'ifn? ~(truncate that)))))))

(defn NonEmpty
  "Defines a type where the given collection has count > 0."
  ([] (Canonical 'NonEmpty (Pred seq)))
  ([coll]
     (Canonical (list 'NonEmpty coll) (I coll (Pred seq)))))

(defn Empty
  "Defines a type where the given collection has count = 0."
  ([] (Canonical 'Empty (Pred empty?)))
  ([coll]
     (Canonical (list 'Empty coll) (I coll (Pred empty?)))))

(defn Option
  "The union of t and nil."
  [t]
  (Canonical (list 'Option t) (U t nil)))

(defn Nilable
  "The union of t and nil."
  [t]
  (Canonical (list 'Nilable t) (U t nil)))

(defrecord CountType [min max]
  Typeable
  (display-type [this] (if (= min max)
                         (list 'Count max)
                         (list 'Count min max)))
  (valid-type? [this] (and (integer? min) (integer? max)
                           (>= max min)))
  (check [this that]
    (let [cnt (count that)]
      (cond (< cnt min)
            `(~'< (~'count ~(truncate that)) ~min)
            (> cnt max)
            `(~'> (~'count ~(truncate that)) ~max)))))

(defn Count
  "Define a type whose count must be between min and max. If max is not
  passed, min and max will be the same."
  ([x]
     (CountType. x x))
  ([min max]
     (CountType. min max)))

(defrecord MemberType [t]
  Typeable
  (display-type [this] (list 'Member (display-type t)))
  (valid-type? [this] (valid-type? t))
  (check [this that]
    (let [res (map #(check t %) that)]
      (when-not (every? nil? res)
        res))))

(defn Member
  "Defines a type that all members of a container conform to."
  [t]
  (MemberType. t))

(defrecord CollType [container-type member-type]
  Typeable
  (display-type [this]
    (display-type (Canonical (list container-type member-type)
                             (I container-type (Member member-type)))))
  (valid-type? [this]
    (and (valid-type? container-type) (valid-type? member-type)))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (check (I container-type (Member member-type)) that))))

(defn Coll
  "A persistent collection with member type member-type and optionally
  container type container-type."
  ([] (Canonical 'Coll (Pred coll?)))
  ([member-type]
     (Coll (Canonical 'Coll (Pred coll?)) member-type))
  ([container-type member-type]
     (CollType. container-type member-type)))

(defn Seq
  "A sequence of member type t."
  ([] (Canonical 'Seq (Pred seq?)))
  ([t] (Coll (Seq) t)))

#?(:cljs
   ;; TODO NOTE that the type was not augmunted! that's because we
   ;; never use values as contract template
   (augment-constructor cljs.core/LazySeq))

(defn LazySeq
  "A lazy sequence of member type t.
  WARNING: Usage of the one-arity version of this function could realize
  the entire Lazy Seq."
  ([] #?(:clj clojure.lang.LazySeq
         :cljs cljs.core/LazySeq))
  ([t] (Coll (LazySeq) t)))

#?(:cljs
   ;; rare case where PrimitiveType is best choice
   ;; maybe have 'augment-protocol' ?
   (def SeqableT (PrimitiveType. 'Seqable cljs.core/ISeqable seqable?)))

(defn Seqable
  "A type that can be used to create a sequence of member type t."
  ([] #?(:clj clojure.lang.Seqable
         :cljs SeqableT
         ))
  ([t] (Coll (Seqable) t)))

(def ^{:doc "A type that can be used to create a collection of member type t or is nil."}
  NilableColl (comp Nilable Coll))

(defn CanSeq
  "A type that can be used to create a seq of member type t.
  WARNING: Strings are seqable. Consider using NilableColl."
  ;; TODO
  ([] (Canonical 'CanSeq (U nil (Seqable) String Map
                           #?@(:clj [Iterable])
                           (Pred array?))))
  ([t] (Coll (CanSeq) t)))

(defn Queue
  "A persistent queue with member type t."
  ([] (Canonical 'Queue clojure.lang.PersistentQueue))
  ([t] (Coll (Queue) t)))

(defn SortedSet
  "A persistent sorted set with member type t."
  ([] (Canonical 'SortedSet (I Set Sorted)))
  ([t] (Coll (SortedSet) t)))

(defn SortedMap
  "A persistent sorted map with member key type kt and value
  type vt."
  ([] (Canonical 'SortedMap (I Map Sorted)))
  ([kt vt] (Coll (SortedMap) [kt vt])))

(defn KwA
  "Given one or more pairs of keyword args to types, returns a
  map type where the keys are optional.

  For example:
  (KwA :method Named :timeout Int)

  Will produce the following type:
  (Option {(optional-key :method) Named
           (optional-key :timeout) Int})"
  [& kt-pairs]
  (->> (partition 2 kt-pairs)
       (map (fn [[k t]] [(optional-key k) t]))
       (into {})
       Option
       (Canonical (apply list 'KwA kt-pairs))))

(defrecord ProtocolType [proto-sym proto]
  Typeable
  (display-type [this] (list 'Protocol proto-sym))
  (valid-type? [this] (and (map? proto) (var? (:var proto))))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (when-not (satisfies? proto that)
        `(~'not (~'satisfies? ~proto-sym ~(truncate that)))))))

(defmacro Protocol
  "Given a protocol returns a type whose values all satisfy
  the protocol."
  [proto]
  `(ProtocolType. (fq-ns (:var ~proto)) ~proto))

(defrecord PairsType [kt-pairs m]
  Typeable
  (display-type [this]
    (apply list 'Pairs (map display-type kt-pairs)))
  (valid-type? [this] (valid-type? m))
  (check [this that]
    (check m (into {} (map vec (partition 2 that))))))

(defn Pairs
  "Given sequential key/value pairs returns a type that behaves like Kw,
  but expects the data to be a sequential list of kev/value pairs
  instead of a map. Use when wrapping a function that takes keyword
  arguments."
  [& kt-pairs]
  (->> (partition 2 kt-pairs)
       (map (fn [[k t]] [(optional-key k) t]))
       (into {})
       (PairsType. kt-pairs)))

(defrecord SubsetType [superset]
  Typeable
  (display-type [this] (list 'Subset (display-type superset)))
  (valid-type? [this] (set? superset))
  (check [this that]
    (if-not (valid-type? this)
      (not-valid-type this)
      (when-not (set/subset? that superset)
        ;; subset? does not get fq
        `(~'not (clojure.set/subset? ~(truncate that) ~superset))))))

(defn Subset
  "Given a set returns a type whose values are a subset of that set."
  [superset]
  (SubsetType. superset))

(defrecord ExMsgType [t]
  Typeable
  (display-type [this] (list 'ExMsg (display-type t)))
  (valid-type? [this] (valid-type? t))
  (check [this that]
    (cond (not (valid-type? this))
          (not-valid-type this)
          #?@(:clj [(not (instance? Exception that))
                    `(~'not (~'instance? ~'Exception ~(truncate that)) )])
          :else
          (let [ex-msg (.getMessage that)]
            (check t ex-msg)))))

(defn ExMsg
  "Returns a type whose values are exceptions with a message of type t."
  [t]
  (ExMsgType. t))

;; Remove auto-generated record fns
#?(:clj
   (do
     (ns-unmap *ns* '->AnyType)
     (ns-unmap *ns* '->CanonicalType)
     (ns-unmap *ns* '->CollType)
     (ns-unmap *ns* '->CountType)
     (ns-unmap *ns* '->EqType)
     (ns-unmap *ns* '->IFnType)
     (ns-unmap *ns* '->IntersectionType)
     (ns-unmap *ns* '->MemberType)
     (ns-unmap *ns* '->NothingType)
     (ns-unmap *ns* '->OptionalKey)
     (ns-unmap *ns* '->PairsType)
     (ns-unmap *ns* '->PredicateType)
     (ns-unmap *ns* '->ProtocolType)
     (ns-unmap *ns* '->RequiredKey)
     (ns-unmap *ns* '->SubsetType)
     (ns-unmap *ns* '->UnionType)
     (ns-unmap *ns* '->ExMsgType)
     (ns-unmap *ns* 'map->AnyType)
     (ns-unmap *ns* 'map->CanonicalType)
     (ns-unmap *ns* 'map->CollType)
     (ns-unmap *ns* 'map->CountType)
     (ns-unmap *ns* 'map->EqType)
     (ns-unmap *ns* 'map->IFnType)
     (ns-unmap *ns* 'map->IntersectionType)
     (ns-unmap *ns* 'map->MemberType)
     (ns-unmap *ns* 'map->NothingType)
     (ns-unmap *ns* 'map->OptionalKey)
     (ns-unmap *ns* 'map->PairsType)
     (ns-unmap *ns* 'map->PredicateType)
     (ns-unmap *ns* 'map->ProtocolType)
     (ns-unmap *ns* 'map->RequiredKey)
     (ns-unmap *ns* 'map->SubsetType)
     (ns-unmap *ns* 'map->UnionType)
     (ns-unmap *ns* 'map->ExMsgType)))
