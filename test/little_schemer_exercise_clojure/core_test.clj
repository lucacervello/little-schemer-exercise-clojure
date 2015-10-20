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

(deftest rember-test
  (testing "rember remove a element from the list"
    (do
      (is (= ["lamb" "chops" "and" "jelly"]
             (rember "mint" ["lamb" "chops" "and" "mint" "jelly"])))
      (is (= '(1 2 3) (rember 4 '(1 2 3 4))))
      (is (= ["coffee" "tea" "cup" "and" "hick" "cup"]
             (rember "cup" ["coffee" "cup" "tea" "cup" "and" "hick" "cup"]))))))

;; BASTA USARE GLI ESEMPI DEL TESTO
;; sono troppo lunghi da scrivere

(deftest firsts-test
  (testing "firsts take the first element of every S-expression"
    (do
      (is (firsts [[1 2 3] [3 2 3] [4 5 3]]) [1 3 4]))))

(deftest insertR-test
  (testing "build a lat with new inserted to the right of the old"
    (do
      (is (= [1 2 3 4 5] (insertR 4 3 [1 2 3 5])))
      (is (= [6 4 3 5 2] (insertR 3 4 [6 4 5 2]))))))

(deftest insertL-test
  (testing "build a lat with new inserted to the left of the old"
    (do
      (is (= [1 2 3 4 5] (insertL 3 4 [1 2 4 5])))
      (is (= [3 4 5 6 7] (insertL 4 5 [3 5 6 7]))))))

(deftest subst-test
  (testing "Subsistitute the old with the new in the lat"
    (do
      (is (= [1 2 3 4 5] (subst 3 6 [1 2 6 4 5])))
      (is (= [1 3 5 7 9] (subst 5 4 [1 3 4 7 9]))))))

(deftest multirember-test
  (testing "remove element in every occurrence"
    (do
      (is (= [1 3 4 5] (multirember 2 [1 2 3 2 4 2 5])))
      (is (= [3 4 5 6] (multirember 2 [3 4 5 6]))))))
