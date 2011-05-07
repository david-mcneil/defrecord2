(ns defrecord2.defrecord2-core
  "Enhanced defrecord support"
  (:require [clojure.contrib.str-utils2 :as str2]
            [clojure.string :as str])
  (:use [clojure.contrib.generic.functor :only (fmap)]
        [clojure.set :only (difference)]
        [clojure.string :only (join)]
        [clojure.pprint :only (simple-dispatch pprint)]
        [clojure.zip :only (zipper)]
        [matchure :only (if-match)])
  (:import [clojure.lang IPersistentList IPersistentVector IPersistentMap IPersistentSet ISeq]))

;; internal helpers for name conversion

(defn is-upper? [s]
  (= (.toUpperCase s) s))

(defn assemble-words [parts]
  (loop [remaining-parts parts result []]
    (if (seq remaining-parts)
      (let [part (first remaining-parts)]
        (recur (rest remaining-parts)
               (if (is-upper? part)
                 (conj result (.toLowerCase part))
                 (conj (if (seq result)
                         (pop result)
                         []) (str (last result) part)))))
      result)))

(defn camel-to-dashed
  "Convert a name like 'BigBlueCar' to 'big-blue-car'."
  [s]
  (let [parts (remove #(= "" %) (str2/partition s #"[A-Z]"))
        words (assemble-words parts)]
    (join "-" words)))

;; internal helpers for changing records via maps

(defn set-record-field
  "Set a single field on a record."
  [source [key value]]
  (assoc source key value))

(defn set-record-fields
  "Set many fields on a record, from a map."
  [initial value-map]
  (reduce set-record-field initial value-map))

;; universal constructor function

(defmulti new-record
  "A universal constructor function that can create a new instance of any type of record from a current record and a new value map."
  (fn [initial value-map] (class initial)))

(defmacro make-universal-constructor
  "Define the implementation of new-record for this type."
  [ctor-name type-name]
  `(defmethod new-record ~type-name
     [initial# value-map#]
     (~ctor-name initial# value-map#)))

;; internal helper for generating constructor function

(defn expected-keys? [map expected-key-set]
  (not (seq (difference (set (keys map)) expected-key-set))))

(defmacro make-record-constructor
  "Define the constructor functions used to instantiate a record."
  [ctor-name type-name field-list default-record]
  `(defn ~ctor-name
     ([value-map#]
        (~ctor-name ~default-record value-map#))
     ([initial# value-map#]
        {:pre [(or (nil? initial#)
                   (isa? (class initial#) ~type-name))
               (map? value-map#)
               (expected-keys? value-map# ~(set (map keyword field-list)))]}
        (set-record-fields (if (nil? initial#) ~default-record initial#) value-map#))))

;; internal helpers for printing

(defn remove-nil-native-fields [native-keys record]
  (let [extra-keys (difference (set (keys record))
                               native-keys)]
    (let [contents (reduce into [] (for [[k v] record]
                                     (if (or (contains? extra-keys k)
                                             (not (nil? v)))
                                       [k v])))]
      (apply array-map contents))))

(defmacro print-record
  "Low-level function to print a record to a stream using the specified constructor name in the print output and using the provided write-contents function to write out the contents of the record (represented as a map)."
  [ctor ctor-name native-keys record stream write-contents]
  `(do
     (.write ~stream (str "(" ~ctor-name " "))
     (~write-contents (remove-nil-native-fields ~native-keys ~record))
     (.write ~stream  ")")))

(defn print-record-contents
  "Simply write the contents of a record to a stream as a string. Used for basic printing."
  [stream contents]
  (.write stream (str contents)))

(defmacro setup-print-record-method [ctor ctor-name native-keys type-name method-name]
  `(defmethod ~method-name ~type-name [record# writer#]
              (print-record ~ctor ~ctor-name ~native-keys record# writer# (partial print-record-contents writer#))))

(defmacro setup-print-record
  "Define the print methods to print a record nicely (so that records will print in a form that can be evaluated as itself)."
  [ctor ctor-name native-keys type-name]

  `(do (setup-print-record-method ~ctor ~ctor-name ~native-keys ~type-name print-method)
       (setup-print-record-method ~ctor ~ctor-name ~native-keys ~type-name print-dup)))

(defn generate-record-pprint
  "Return a function that can be used in the pprint dispatch mechanism to handle a specific constructor name."
  [ctor ctor-name native-keys]
  (fn [record]
    ;; clojure.pprint/pprint-map is private hence the dance here:
    (print-record ctor ctor-name native-keys record *out* @#'clojure.pprint/pprint-map)))

;; internal helpers - walking data structures

;; w - walker function
;; f - mutator function
;; n - node in data tree being walked

;; helper - generating walking methods like this:
(comment (defmethod prewalk2 Foo [f foo]
                    (if-let [foo2 (f foo)]
                      (new-foo foo2 {:a (prewalk2 f (:a foo2))
                                     :b (prewalk2 f (:b foo2))})))

         (defmethod postwalk2 Foo [f foo]
                    (f (new-foo foo {:a (postwalk2 f (:a foo))
                                     :b (postwalk2 f (:b foo))}))))

(defmulti walk2 (fn [w f n] (class n)))

(defmethod walk2 :default [w f n]
           n)

(defmethod walk2 IPersistentVector [w f n]
           (apply vector (map (partial w f) n)))

(defmethod walk2 IPersistentMap [w f n]
           ;; TODO: handle sorted maps
           (apply array-map (mapcat (partial walk2 w f) n)))

(defmethod walk2 IPersistentSet [w f n]
           (set (map (partial w f) n)))

(defmethod walk2 IPersistentList [w f n]
           (apply list (map (partial w f) n)))

(prefer-method walk2 IPersistentList ISeq)

(defmethod walk2 ISeq [w f n]
           (map (partial w f) n))

(defmacro walking-helper-field
  ([w f n field]
     `[~(keyword field) (~w ~f (~(keyword field) ~n))])
  ([w f n field & more]
     `(concat (walking-helper-field ~w ~f ~n ~field) (walking-helper-field ~w ~f ~n ~@more))))

(defmacro walking-helper-fields
  [w f n fields]
  (if (seq fields)
    `(apply array-map (walking-helper-field ~w ~f ~n ~@fields))
    []))

(defmacro make-prewalk2-method
  "Define the methods used to walk data structures."
  [ctor-name type-name field-list]
  `(defmethod prewalk2 ~type-name [f# n#]
              (if-let [n2# (f# n#)]
                ;; TODO: better check than comparing classes to determine if
                ;; the old contents can be inserted into the new node?
                (if (= (class n#) (class n2#))
                  (let [contents# (walking-helper-fields prewalk2 f# n2# ~field-list)]
                    (~ctor-name n2# contents#))
                  n2#))))

(defmacro make-postwalk2-method
  "Define the methods used to walk data structures."
  [ctor-name type-name field-list]
  `(defmethod postwalk2 ~type-name [f# n#]
              (f# (~ctor-name n# (walking-helper-fields postwalk2 f# n# ~field-list)))))

;;;; zipper methods

(defmulti record-branch?
  "The branch? function to use when creating zippers on records."
  class)

(defmethod record-branch? :default [_]
           false)

(defmethod record-branch? IPersistentVector [n]
           (> (count n) 0))

(defmethod record-branch? IPersistentMap [n]
           (> (count n) 0))

(defmethod record-branch? IPersistentList [n]
           (> (count n) 0))

(defmethod record-branch? ISeq [n]
           (> (count n) 0))

(prefer-method record-branch? IPersistentList ISeq)

(defmacro make-record-branch?-method
  "Generate the record-branch? method for a type."
  [type-name]
  `(defmethod record-branch? ~type-name [_#]
              true))

;;

(defmulti record-node-children
  "The node-children method to use when creating zippers on records."
  class)

(defmethod record-node-children IPersistentVector [n]
           n)

(defmethod record-node-children IPersistentMap [n]
           (seq n))

(defmethod record-node-children IPersistentList [n]
           n)

(defmethod record-node-children ISeq [n]
           n)

(prefer-method record-node-children IPersistentList ISeq)

(defmacro rnc-helper-field
  ([n field]
     `[(~(keyword field) ~n)])
  ([n field & more]
     `(concat (rnc-helper-field ~n ~field) (rnc-helper-field ~n ~@more))))

(defmacro rnc-helper-fields
  [n fields]
  (if (seq fields)
    `(rnc-helper-field ~n ~@fields)
    []))

(defmacro make-record-node-children-method
  "Generate the record-node-children method for a type."
  [type-name field-list]
  `(defmethod record-node-children ~type-name [node#]
              (rnc-helper-fields node# ~field-list)))

;;

(defmulti record-make-node
  "The make-node method to use when creating zippers on records."
  (fn [node children] (class node)))

(defmethod record-make-node IPersistentVector [node children]
           (vec children))

(defmethod record-make-node IPersistentMap [node children]
           (apply hash-map (apply concat children)))

(defmethod record-make-node IPersistentList [node children]
           children)

(defmethod record-make-node ISeq [node children]
           (apply list children))

(prefer-method record-make-node IPersistentList ISeq)

(defmacro apply-to-symbol [f count]
  "Return a function that takes a vector of args and which will do the equivalent of (apply f args). This is suitable for getting 'apply' like functionality from generated record constructors."
  `(fn [x#]
     (let [~'x x#]
       (~f ~@(map (fn [i] (list 'nth 'x i)) (range count)))))  )

(defmacro make-record-make-node-method
  "Generate the record-make-node method for a type."
  [type-name field-list]
  `(defmethod record-make-node ~type-name [_# children#]
              ((apply-to-symbol ~(symbol (str (.getName type-name) ".")) ~(count field-list))
               children#)))

;; helpers for custom zippers

(defn record-branch?-or-map
  "Creates a custom function to use as the branch? fn in the zipper. The field-map contains keys which are classes and values which are sequences of keywords identifying fields in the class. These keywords identify the fields, and their order, which will be visited as children by the zipper. If an empty sequence is placed as a value in the field-map then that class will not be considered a branch point for the zipper."
  [field-map]
  (if (seq field-map)
    (fn [node]
      (let [fields (field-map (class node))]
        (if fields
          (if (seq fields)
            true
            false)
          (record-branch? node))))
    record-branch?))

(defn record-node-children-or-map
  "Creates a custom function to use as the children fn in the zipper. The field-map contains keys which are classes and values which are sequences of keywords identifying fields in the class. These keywords identify the fields, and their order, which will be visited as children by the zipper."
  [field-map]
  (if (seq field-map)
    (fn [node]
      (let [fields (field-map (class node))
            alternate-children-f (if (seq fields) (apply juxt fields))]
        (if alternate-children-f
          (alternate-children-f node)
          (record-node-children node))))
    record-node-children))

(defn record-make-node-or-map
  "Creates a custom function to use as the make-node fn in the zipper using
   a field-map and a constructor map.

   Field-map (may be nil):
    - key = record Class
    - value = seq of field keywords that should be treated as child nodes

   Constructor-map (may be nil):
    - key = record Class
    - val = (fn [node {:field value ...}]) - custom record constructor

   Execution cases:
   1) Fields and custom constructor both specified. Call custom constructor with
      only the custom fields in the second map argument.
   2) Custom constructor specifed but NO field set specified. Call custom
      constructor but include ALL fields in the value map.
   3) Field set specified but NO custom constructor specified.  Call universal
      new-record constructor with node and a map of only the specified fields.
   4) Neither field set nor customer constructor specified.  Construct the Clojure
      record as a Java class with all field values specified."
  [field-map ctor-map]
  (fn [node children]
    (let [type (class node)
          fields (when (seq field-map) (field-map type))
          ctor (when (seq ctor-map) (ctor-map type))]
      (if (or fields ctor)
        (let [record-ctor (or ctor new-record)
              fields (or fields (keys node))
              children-map (zipmap fields children)]
          (record-ctor node children-map))
        (record-make-node node children)))))

;; record?

(defmulti record? (fn [x] (.getName (class x))))

(defmethod record? :default [_]
           false)

(defmacro make-record?
  "Define the implementation of record? for this type."
  [type-name]
  `(defmethod record? ~(.getName (resolve type-name))
     [_#]
     true))

;; record pattern matching

(defmulti record-matcher (fn [form]
                           (let [dispatch-value (if (and (list? form)
                                                         (symbol? (first form)))
                                                  (let [sym (first form)]
                                                    (if-let [resolved (resolve sym)]
                                                      (try
                                                        (let [test-obj (resolved {})]
                                                          (if (record? test-obj)
                                                            (symbol (.getName (class test-obj)))))
                                                        (catch IllegalArgumentException e
                                                          ;;ignore this
                                                          ))
                                                      (throw (RuntimeException. (str "Unknown symbol: " sym))))))]
                             (or dispatch-value
                                 :default))))

(defn seq-to-list [s]
  (reverse (into '() s)))

(defmethod record-matcher :default [x]
           (if (list? x)
             (let [converted-x (seq-to-list (map record-matcher x))]
               converted-x)
             x))

(defmacro make-record-matcher
  "Generate the record-make-node method for a type."
  [ctor-name]
  `(defmethod record-matcher '~(symbol (.getName (class ((resolve ctor-name) {}))))
     [[_# value-map#]]
     (list '~'and (symbol (.getName (class (~ctor-name {}))))
           (fmap record-matcher value-map#))))

(defmacro match-record
  ([matches expr]
     `(match-record ~matches ~expr nil))
  ([[record in] expr fail-expr]
     `(matchure/if-match [~(record-matcher record) ~in] ~expr ~fail-expr)))

;; public entry points

(defmulti prewalk2 (fn [f n] (class n)))

(defmethod prewalk2 :default [f n]
           (walk2 prewalk2 f (f n)))

(defmulti postwalk2 (fn [f n] (class n)))

(defmethod postwalk2 :default [f n]
           (f (walk2 postwalk2 f n)))

(defn record-zip
  "Create a zipper on a tree of records."
  ([node]
     (record-zip node nil nil))
  ([node field-map]
     (record-zip node field-map nil))
  ([node field-map ctor-map]
     (zipper (record-branch?-or-map field-map)
             (record-node-children-or-map field-map)
             (record-make-node-or-map field-map ctor-map)
             node)))

(defmulti dissoc2
  "Enhanced version of dissoc that will return a new record of the same type with the given fields removed.
  (Calling dissoc on a record will yield a map.)"
  (fn [n & ks] (class n)))

(defn dissoc2*
  [ctor-f native-keys n & ks]
  (let [d (apply dissoc n ks)
        extra-keys (difference (set (keys d)) native-keys)
        new-record (ctor-f (select-keys d native-keys))]
    (if (empty? extra-keys)
      new-record
      (merge new-record (select-keys d extra-keys)))))

(defmacro make-dissoc2-method
  [ctor-name type-name native-keys]
  `(defmethod dissoc2 ~type-name [n# & ks#]
              (apply dissoc2* ~ctor-name ~native-keys n# ks#)))

(defmacro defrecord2
  "Defines a record and sets up constructor functions, printing, and pprinting for the new record type."
  ([type-name field-list]
     `(defrecord2 ~type-name ~field-list
        ;; invoke defrecord2 with default constructor function name
        ~(symbol (str "new-" (camel-to-dashed (str type-name))))))
  ([type-name field-list ctor-name & opts+specs]
     `(do
        ;; define the record
        (defrecord ~type-name ~field-list ~@opts+specs)

        ;; define the constructor functions
        (make-record-constructor ~ctor-name
                                 ~type-name
                                 ~field-list
                                 (~(symbol (str type-name ".")) ~@(repeat (count field-list) nil)))
        (make-universal-constructor ~ctor-name ~type-name)

        (make-record? ~type-name)

        ;; setup tree walking methods
        (make-prewalk2-method ~ctor-name ~type-name ~field-list)
        (make-postwalk2-method ~ctor-name ~type-name ~field-list)

        ;; setup dissoc2 method
        (make-dissoc2-method ~ctor-name ~type-name (set (keys (~ctor-name {}))))

        ;; setup zipper methods
        (make-record-branch?-method ~type-name)
        (make-record-node-children-method ~type-name ~field-list)
        (make-record-make-node-method ~type-name ~field-list)

        ;; setup pattern matching
        (make-record-matcher ~ctor-name)

        ;; setup printing
        (let [empty-record# (~ctor-name {})
              native-keys# (set (keys empty-record#))
              pprint-fn# (generate-record-pprint ~ctor-name (quote ~ctor-name) native-keys#)]
          (setup-print-record ~ctor-name (quote ~ctor-name) native-keys# ~type-name)
          ;; setup clojure.pprinting
          (.addMethod simple-dispatch ~type-name pprint-fn#)))))

