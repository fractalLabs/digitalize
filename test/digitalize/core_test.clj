(ns digitalize.core-test
  (:require [clojure.test :refer :all]
            [digitalize.core :refer :all]))

(deftest digitalize-works
  (testing "testing digitalize"
    (is (= {:a 1} (digitalize {"A" 1 :b nil :c [] :d {:r []}})))))

(deftest standard-keyword-works
  (testing "standard-keyword"
    (is (= :a-b (standard-keyword "A B")))))

(deftest remove-nils-works
  (testing "remove-nils"
    (is (= [:a] (remove-nils [nil :a [] {:b nil}])))))

(deftest str->number-works
  (testing "str->number"
    (is (= 3332.2 (str->number " 3,3,3,2.2 ")))))
