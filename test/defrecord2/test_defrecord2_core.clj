(ns defrecord2.test-defrecord2-core
  (:require [clojure.zip :as z]
            [defrecord2.test-record-helper :as helper])
  (:use [clojure test]
        [clojure.string :only (upper-case)]
        [defrecord2.test-record-helper :only (new-foo-helper)]
        [defrecord2.defrecord2-core :only (defrecord2 camel-to-dashed new-record dissoc2 prewalk2
                                            postwalk2 apply-to-symbol record-zip record-branch?
                                            record-node-children record-make-node match-record
                                            record-matcher)]
        [clojure.pprint :only (pprint)])
  (:import [clojure.lang IPersistentVector]))

;;

(deftest test-camel-to-dashed
  (are [expected in] (= expected (camel-to-dashed in))
       "wow" "wow"
       "a" "a"
       "a" "A"
       "a-big" "aBig"
       "a-team" "ATeam"
       "a-team-x" "ATeamX"
       "a-x" "aX"))

;;

(defrecord2 Foo [x y])

(deftest test-defrecord2
  (is (= "(new-foo {:x 10})" (print-str (new-foo {:x 10})))))

(deftest test-defrecord2-pprint
  (is (=
       "(new-foo {:x 10})\n"
       (with-out-str (pprint (new-foo {:x 10})))))
  (is (= "(new-foo {:y 2})\n"
         (with-out-str (pprint (new-foo {:x nil :y 2}))))))

(deftest test-defrecord2-pprint-deep
  (is (= "(new-foo {:x 10,\n          :y\n          (new-foo {:x 10,\n                    :y\n                    (new-foo {:x 10, :y (new-foo {:x 10, :y 20})})})})\n"
         (with-out-str (pprint (new-foo {:x 10
                                         :y (new-foo {:x 10
                                                      :y (new-foo {:x 10
                                                                   :y (new-foo {:x 10 :y 20})})})}))))))

(deftest test-defrecord2-dont-skip-extra-nils-but-do-skip-native-nils
  (is (= "(new-foo {:x 10, :b nil, :a 4})"
         (print-str (assoc (new-foo {:x 10}) :a 4 :b nil))))
  (is (= "(new-foo {:x 10, :b nil, :a 4})"
         (print-str (assoc (new-foo {:x 10 :y nil}) :a 4 :b nil)))))

(deftest test-constructor-wrong-args-first-arg-is-not-a-record
  (is (thrown? AssertionError (new-foo :x {}))))

(deftest test-constructor-wrong-args-second-arg-is-not-a-map
  (is (thrown? AssertionError (new-foo (new-foo {:x 10}) 20))))

(deftest test-constructor-wrong-args-only-arg-is-not-a-map
  (is (thrown? AssertionError (new-foo 20))))

(deftest test-typo-in-field-name
  (is (thrown? AssertionError (new-foo {:a 1})))
  (is (thrown? AssertionError (new-foo {:a 1 :x 10})))
  (is (thrown? AssertionError (new-foo {:a 1 :x 10 :y 20}))))

(deftest test-new-record
  (is (= (new-foo {:x 100})
         (new-record (new-foo {:x 1}) {:x 100})))
  (is (= (new-foo {:x 1 :y 100})
         (new-record (new-foo {:x 1}) {:y 100})))
  (is (thrown? AssertionError
               (new-record (new-foo {:x 1}) {:z 100}))))

(deftest test-dissoc2
  (is (= (new-foo {:x 1})
         (dissoc2 (new-foo {:x 1 :y 2}) :y)))
  (is (= (new-foo {})
         (dissoc2 (new-foo {:x 1 :y 2}) :x :y)))
  (is (= (assoc (new-foo {:x 1}) :z 3)
         (dissoc2 (assoc (new-foo {:x 1 :y 2}) :z 3) :y)))
  (is (= (new-foo {:x 1})
         (dissoc2 (assoc (new-foo {:x 1 :y 2}) :z 3) :y :z))))

;;

(defrecord2 F [z x b a])

(deftest test-single-char-type-name
  (is (= (F. nil nil nil 20)
         (new-f {:a 20}))))

(deftest test-order-of-printed-fields
  (is (= "(new-f {:z 100, :x 99, :b 2, :a 1})"
         (print-str (new-f {:a 1 :b 2 :x 99 :z 100})))))

;; test tree walking

(defmulti test-incrementer class)

(defmethod test-incrementer Number [x]
           (+ 1 x))

(defmethod test-incrementer :default [x]
           x)

(defn test-pre-and-post [expected x]
  (is (= expected
         (prewalk2 test-incrementer x)))
  (is (= expected
         (postwalk2 test-incrementer x)))  )

(deftest test-walk
  (are [expected x] (and (= expected
                            (prewalk2 test-incrementer x))
                         (= expected
                            (postwalk2 test-incrementer x)))

       (new-foo {:x 101 :y 201})
       (new-foo {:x 100 :y 200})

       (new-foo {:x 101
                 :y (new-foo {:x 301 :y 401})})
       (new-foo {:x 100
                 :y (new-foo {:x 300 :y 400})})

       (new-foo {:x [101 201]})
       (new-foo {:x [100 200]})

       (new-foo {:x '(101 201)})
       (new-foo {:x '(100 200)})

       (new-foo {:x {:a 101 :b 201}})
       (new-foo {:x {:a 100 :b 200}})
       
       (new-foo {:x #{101 201}})
       (new-foo {:x #{100 200}})
       
       (new-foo {:x {:a 101
                     :b (new-foo {:x {:a 101 :b 201}})}})
       (new-foo {:x {:a 100
                     :b (new-foo {:x {:a 100 :b 200}})}})

       (new-foo {:x {:a 101
                     :b [(new-foo {:x {:a 101 :b 201}})]}})
       (new-foo {:x {:a 100
                     :b [(new-foo {:x {:a 100 :b 200}})]}})

       (new-foo {:x (range 2 11)})
       (new-foo {:x (range 1 10)})))

;; more tree walking tests

(defrecord2 Bar [a b])

(deftest test-walk-change-type
  (let [node (new-foo {:x (new-foo {:x 1})})
        new-node (new-bar {:a 100})
        mutator (fn [old-node]
                  (if (= Foo (class old-node))
                    new-node
                    old-node))]
    (is (= new-node
           (prewalk2 mutator node)))
    (is (= new-node
           (postwalk2 mutator node)))))

(defmulti test-mutator class)

(defmethod test-mutator :default [x]
           (if (nil? x)
             nil
             (+ 1 x)))

(defmethod test-mutator Foo [x]
           (new-foo x {:x 100}))

(defmethod test-mutator Bar [x]
           nil)

(deftest test-walk-multi-types
  (is (= (new-foo {:x 101})
         (prewalk2 test-mutator (new-foo {:x 1 :y (new-bar {})}))))
  (is (= (new-foo {:x 100})
         (postwalk2 test-mutator (new-foo {:x 1 :y (new-bar {})}))))  )

;; tree walking - mutate clojure data types

(defmulti remove-nils class)

(defmethod remove-nils :default [x]
           x)

(defmethod remove-nils IPersistentVector [v]
           (apply vector (remove nil? v)))

(deftest test-mutate-vector
  (is (= {:x [:a [:b :c]]}
         (prewalk2 remove-nils {:x [:a nil [:b nil :c]]}))))

(defmulti caps class)

(defmethod caps :default [x] x)

(defmethod caps String [s]
           (upper-case s))

(deftest test-mutate-map
  (is (= {:x {:a "WOW"}}
         (prewalk2 caps {:x {:a "wow"}}))))

(deftest test-mutate-nil
  (is (= (new-foo {:x 1})
         (new-foo nil {:x 1}))))

;; record with no fields
(defrecord2 Empty [])

(deftest test-empty
  (is (= (Empty.)
         (new-empty {})))
  (is (= "(new-empty {})"
         (with-out-str (print (new-empty {}))))))

(deftest test-apply-to-symbol
  (comment (is (thrown? ClassNotFoundException
                        (apply Bar. [100 200]))))
  (is (= (new-bar {:a 100, :b 200})
         ((apply-to-symbol Bar. 2) [100 200]))))

(deftest test-zipper
  (let [lll (new-bar {:a (new-foo {:x 11 :y 21})})
        ll (new-bar {:a lll})
        l [ll]
        rl (new-bar {:a 2})
        r (list rl)
        tree (new-foo {:x l :y r})
        zipper (record-zip tree)]
    (is (= l (z/node (z/down zipper))))
    (is (= ll (z/node (z/down (z/down zipper)))))
    (is (= lll (z/node (z/down (z/down (z/down zipper))))))

    (is (= r (z/node (z/right (z/down zipper)))))
    (is (= rl (z/node (z/down (z/right (z/down zipper))))))
    (is (= 2 (z/node (z/down (z/down (z/right (z/down zipper)))))))

    (is (not (record-branch? (z/node (z/down (z/down (z/right (z/down zipper))))))))))


(defn- trace-zipper [f zipper]
  (map f (map z/node (take-while #(not (z/end? %)) (iterate z/next zipper)))))

(deftest test-custom-zipper
  (let [tree (new-bar {:a (new-foo {:x 10 :y 20}) :b (new-foo {:x 100 :y 200})})]
    (is (= [defrecord2.test-defrecord2-core.Bar
            defrecord2.test-defrecord2-core.Foo
            java.lang.Integer
            java.lang.Integer
            defrecord2.test-defrecord2-core.Foo
            java.lang.Integer
            java.lang.Integer]
             (trace-zipper class (record-zip tree))))

    (is (= [defrecord2.test-defrecord2-core.Bar
            defrecord2.test-defrecord2-core.Foo
            defrecord2.test-defrecord2-core.Foo]
             (trace-zipper class (record-zip tree {Foo []}))))

    (is (= [defrecord2.test-defrecord2-core.Bar
            defrecord2.test-defrecord2-core.Foo
            java.lang.Integer]
             (trace-zipper class (record-zip tree {Foo [:x]
                                                   Bar [:b]}))))

    (is (= [(new-bar {:a (new-foo {:x 10, :y 20})
                      :b (new-foo {:x 100, :y 200})})
            (new-foo {:x 100, :y 200})
            100]
             (trace-zipper identity (record-zip tree {Foo [:x]
                                                      Bar [:b]}))))

    (is (= [(new-bar {:a (new-foo {:x 10, :y 20})
                      :b (new-foo {:x 100, :y 200})})
            (new-foo {:x 10, :y 20})
            (new-foo {:x 100, :y 200})]
             (trace-zipper identity (record-zip tree {Foo []}))))

    (is (= [(new-bar {:a (new-foo {:x 10, :y 20})
                      :b (new-foo {:x 100, :y 200})})
            (new-foo {:x 10, :y 20})
            20
            10
            (new-foo {:x 100, :y 200})
            200
            100]
             (trace-zipper identity (record-zip tree {Foo [:y :x]}))))))

(deftest test-custom-zipper-constructor
  (let [tree (new-bar {:a (new-foo {:x 10 :y 20}) :b (new-foo {:x 100 :y 200})})]

    ;; if no ctor-map is provided then the default ctor is used
    (let [zipper (record-zip tree {Foo [:x :y]})]
      (is (= (new-bar {:a (new-foo {:x -1 :y 20}) :b (new-foo {:x 100 :y 200})})
             (z/root (z/edit (z/next (z/next zipper)) (constantly -1))))))
    
    ;; if fields are specifies in the field-map then the new children
    ;; are passed into the ctor fn in a map keyed by field keyword
    (let [zipper (record-zip tree {Foo [:x :y]} {Foo (fn [node {:keys (x y)}]
                                                       (new-foo {:x (+ x 1) :y (+ y 1)}))})]
      (is (= (new-bar {:a (new-foo {:x 0 :y 21}) :b (new-foo {:x 100 :y 200})})
             (z/root (z/edit (z/next (z/next zipper)) (constantly -1))))))

    ;; if no fields are specified in the field-map then the new
    ;; children are passed into the ctor fn in a map keyed by field keyword
    (let [zipper (record-zip tree
                             {}
                             {Foo (fn [node {:keys (x y) :as z}]
                                    ;; reverse the args
                                    (new-foo {:x y :y x}))})]
      (is (= (new-bar {:a (new-foo {:x 20 :y -1}) :b (new-foo {:x 100 :y 200})})
             (z/root (z/edit (z/next (z/next zipper)) (constantly -1))))))

    (let [zipper (record-zip tree {Foo [:x]})]
      (is (= (new-bar {:a (new-foo {:x -1 :y 20}) :b (new-foo {:x 100 :y 200})})
             (z/root (z/edit (z/next (z/next zipper)) (constantly -1))))))
    ))

(deftest test-record-zipper-on-vectors
  ;; record-branch?
  (are [expected node] (= expected (record-branch? node))
       false []
       true [1])
  ;; record-node-children
  (are [expected node] (= expected (record-node-children node))
       [] []
       [1 2] [1 2])
  ;; record-make-node
  (are [expected node children] (= expected (record-make-node node children))
       [] [] []
       [1 2] [] [1 2]))

(deftest test-record-zipper-on-lists
  ;; record-branch?
  (are [expected node] (= expected (record-branch? node))
       false (list)
       false '()
       true '(1))
  ;; record-node-children
  (are [expected node] (= expected (record-node-children node))
       '() (list)
       '() '()
       '(1 2) '(1 2))
  ;; record-make-node
  (are [expected node children] (= expected (record-make-node node children))
       '() '() []
       '(1 2) '() [1 2]))

(defn- to-lazy [s] (map identity s))

(deftest test-record-zipper-on-seqs
  ;; record-branch?
  (are [expected node] (= expected (record-branch? (to-lazy node)))
       false (list)
       false '()
       true '(1))
  ;; record-node-children
  (are [expected node] (= expected (record-node-children (to-lazy node)))
       '() (list)
       '() '()
       '(1 2) '(1 2))
  ;; record-make-node
  (are [expected node children] (= expected (record-make-node (to-lazy node) children))
       '() '() []
       '(1 2) '() [1 2]))

(deftest test-record-zipper-on-maps
  ;; record-branch?  
  (are [expected node] (= expected (record-branch? node))
       false {}
       true {:a 1})
  ;; record-node-children  
  (are [expected node] (= expected (record-node-children node))
       nil {}
       [[:a 1]] {:a 1}
       [[:a 1] [:b 2]] {:a 1 :b 2})
  ;; record-make-node
  (are [expected node children] (= expected (record-make-node node children))
       {} {} []
       {:a 1} {} [[:a 1]]
       {:a 1 :b 2} {} [[:a 1] [:b 2]]
       {:a 1 :b 2} {:c 3 :d 4} [[:a 1] [:b 2]]))

(deftest test-record-zipper-map-navigation-and-editing
  (let [tree {:a {:b 1}}
        z (record-zip tree)]
    (is (= 1
           (-> z z/down  ;; to first mapentry
               z/down    ;; to key of mapentry
               z/right   ;; to value of mapentry (also a map)
               z/down    ;; repeat down/down/right
               z/down
               z/right
               z/node))) ;; to 1
    (is (= {:a {:b 42} :c 999}
           (-> z
               (z/append-child [:c 999])
               z/down z/down z/right z/down z/down z/right
               (z/edit (fn [loc] 42))
               z/root))))) 

(deftest test-pattern-matching
  (is (= 100 (match-record [(new-foo {:x 1
                                      :y (new-bar {:a ?a})})
                            (new-foo {:x 1 :y (new-bar {:a 100 :b 200})})]
                           a
                           :fail)))

  ;; test with a record from another namespace
  (is (= 2 (match-record [(new-foo-helper {:p 1 :q ?a})
                          (new-foo-helper {:p 1 :q 2})]
                         a
                         :fail)))

  ;; match fails, default is to return nil
  (is (= nil (match-record [(new-foo {:x 1
                                      :y (new-bar {:a ?a})}) (new-foo {:x 1 :y (new-foo {:x 100 :y 200})})]
                           a)))

  ;; test explicit failure expression
  (is (= :fail (match-record [(new-foo {:x 1
                                        :y (new-bar {:a ?a})}) (new-foo {:x 1 :y (new-foo {:x 100 :y 200})})]
                             a
                             :fail)))

  ;; deep match fails
  (is (= nil (match-record [(new-foo {:x 1
                                      :y (new-bar {:a ?a :b 2})})
                            (new-foo {:x 1 :y (new-bar {:a 100 :b 200})})]
                           a)))

  ;; deep capture
  (is (= 100 (match-record [(new-foo {:x 1
                                      :y (new-bar {:a ?a :b 2})})
                            (new-foo {:x 1 :y (new-bar {:a 100 :b 2})})]
                           a)))

  ;; use record patterns composed with other conditions
  (is (= 100 (match-record [(and (new-foo {:x 1
                                           :y (new-bar {:a ?a :b 2})})
                                 (= 2 (count ?)))
                            (new-foo {:x 1 :y (new-bar {:a 100 :b 2})})]
                           a)))

  ;; nested captures
  (is (= [100
          (new-bar {:a 100, :b 2})
          (new-foo {:x 1 :y (new-bar {:a 100 :b 2})})]
           (match-record [(and ?foo
                               (new-foo {:x 1
                                         :y ?bar})
                               (new-foo {:x 1
                                         :y (new-bar {:a ?a :b 2})}))
                          (new-foo {:x 1 :y (new-bar {:a 100 :b 2})})]
                         [a bar foo])))

  ;; compound patterns without any record patterns still work
  (is (= 100
         (match-record [(and {:x ?a}
                             (= 1 (count ?)))
                        {:x 100}]
                       a)))

  ;; or works
  (is (= :match
         (match-record [(or {:x ?a}
                            (= 1 (count ?)))
                        {:z 100}]
                       :match)))

  ;; failures without any record patterns work
  (is (= :fail
         (match-record [(or {:x ?a}
                            (= 2 (count ?)))
                        {:z 100}]
                       :match
                       :fail)))

  (binding [*ns* (find-ns 'defrecord2.test-defrecord2-core)]

    (is (= '(and defrecord2.test-defrecord2-core.Foo {:y ?y, :x 1})
           (record-matcher '(new-foo {:x 1 :y ?y}))))

    (is (= '(and defrecord2.test-record-helper.FooHelper {:p 1, :q ?y})
           (record-matcher '(new-foo-helper {:p 1 :q ?y}))))

    (is (= '(and defrecord2.test-record-helper.FooHelperOther {:p 1, :q ?y})
           (record-matcher '(helper/new-foo-helper-other {:p 1 :q ?y})))))

  (is (thrown-with-msg? RuntimeException #"Unknown symbol: new-x"
        (record-matcher '(new-x {}))))

  ;; deeply nested patterns work
  (is (= 100
         (match-record [(and
                         (and
                          (and
                           (and {:x ?a}
                                (= 1 (count ?)))
                           (> 10 (count ?)))
                          (> 100 (count ?)))
                         (= 1 (count ?)))
                        {:x 100}]
                       a))))

(deftest test-pattern-match-with-variables-as-input
  (let [x (new-foo {:x 1 :y 2})]
    (is (= 2 (match-record [(new-foo {:x 1 :y ?y})
                            x]
                           y)))))

(def x (new-foo {:x 1 :y 2}))

(deftest test-pattern-match-with-global-variables-as-input
  (is (= 2 (match-record [(new-foo {:x 1 :y ?y})
                          x]
                         y))))

;;(run-tests)
