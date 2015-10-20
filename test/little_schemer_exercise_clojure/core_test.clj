(ns little-schemer-exercise-clojure.core-test
  (:require [clojure.test :refer :all]
            [little-schemer-exercise-clojure.core :refer :all]))


(deftest atom?-test
  (testing "TRUE if argument is a atom"
    (do
      (is (= true (atom? 5)))
      (is (= true (atom? "a")))
      (is (= false (atom? [1 2])))
      (is (= false (atom? '(1 2 3)))))))

(deftest lat?-test
  (testing "lat? return true if the list is composed only by atom"
    (do
      (is (= true (lat? ["Jack" "Sprat" "could" "eat" "no" "chicken" "fat"])))
      (is (= false (lat? [["Jack"] "Sprat" "could" "eat" "no" "chicken" "fat"])))
      (is (= false (lat? ["Jack" ["Sprat" "could"] "eat" "no" "chicken" "fat"])))
      (is (= true (lat? []))))))

(deftest member?-test
  (testing "member? return true if the argument is in the sequence"
    (do
      (is (= true (member? "tea" ["coffee" "tea" "or" "milk"])))
      (is (= false (member? "poached" ["fried" "eggs" "and" "scrambled" "eggs"]))))))
