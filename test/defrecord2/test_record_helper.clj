(ns defrecord2.test-record-helper
  (:use [defrecord2.defrecord2-core :only (defrecord2)]))

;; these are for testing record pattern matching using types from
;; another namespace

(defrecord2 FooHelper [p q])

(defrecord2 FooHelperOther [p q])
