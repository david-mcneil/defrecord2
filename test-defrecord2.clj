(ns test-defrecord2
  (:use [clojure test]
        [defrecord2 :only (defrecord2 prewalk2 postwalk2 camel-to-dashed)]
        [clojure.contrib.pprint :only (pprint)])
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
       (with-out-str (pprint (new-foo {:x 10}))))))

(deftest test-defrecord2-pprint-deep
  (is (= "(new-foo {:x 10,\n          :y\n          (new-foo {:x 10,\n                    :y\n                    (new-foo {:x 10, :y (new-foo {:x 10, :y 20})})})})\n"
         (with-out-str (pprint (new-foo {:x 10 :y (new-foo {:x 10 :y (new-foo {:x 10 :y (new-foo {:x 10 :y 20})})})}))))))

(deftest test-defrecord2-dont-skip-extra-nils-but-do-skip-native-nils
  (is (= "(new-foo {:x 10, :b nil, :a 4})"
         (print-str (assoc (new-foo {:x 10}) :a 4 :b nil))))
  (is (= "(new-foo {:x 10, :b nil, :a 4})"
         (print-str (assoc (new-foo {:x 10 :y nil}) :a 4 :b nil)))))

(deftest test-contructor-wrong-args-first-arg-is-not-a-record
  (is (thrown? AssertionError (new-foo :x {}))))

(deftest test-contructor-wrong-args-second-arg-is-not-a-map
  (is (thrown? AssertionError (new-foo (new-foo {:x 10}) 20))))

(deftest test-contructor-wrong-args-only-arg-is-not-a-map
  (is (thrown? AssertionError (new-foo 20))))

(deftest test-typo-in-field-name
  (is (thrown? AssertionError (new-foo {:a 1})))
  (is (thrown? AssertionError (new-foo {:a 1 :x 10})))
  (is (thrown? AssertionError (new-foo {:a 1 :x 10 :y 20}))))

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
  (are [expected x] (and (is (= expected
                                (prewalk2 test-incrementer x)))
                         (is (= expected
                                (postwalk2 test-incrementer x)))))
  
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
  
  (new-foo {:x {:a 101
                :b (new-foo {:x {:a 101 :b 201}})}})
  (new-foo {:x {:a 100
                :b (new-foo {:x {:a 100 :b 200}})}})

  (new-foo {:x {:a 101
                :b [(new-foo {:x {:a 101 :b 201}})]}})
  (new-foo {:x {:a 100
                :b [(new-foo {:x {:a 100 :b 200}})]}})

  (new-foo {:x (range 2 11)})
  (new-foo {:x (range 1 10)}))

;; more tree walking tests

(defrecord2 Bar [a b])

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
;;

(deftest test-mutate-nil
  (is (= (new-foo {:x 1})
         (new-foo nil {:x 1}))))
