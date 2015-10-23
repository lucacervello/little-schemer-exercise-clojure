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

(deftest multiinsertR-test
  (testing "insert element in every occurence to the right"
    (do
      (is (= [1 2 3 2 3] (multiinsertR 3 2 [1 2 2])))
      (is (= [3 4 5 3 4] (multiinsertR 4 3 [3 5 3]))))))

(deftest multiinsertL-test
  (testing "insert element in every occurrence to the left"
    (do
      (is (= [3 1 4 5 3 1] (multiinsertL 3 1 [1 4 5 1]))))))

(deftest multisubst-test
  (testing "substitute every element in a vector"
    (do
      (is (= [1 2 4 2] (multisubst 2 3 [1 3 4 3]))))))

;; 4. NUMBERS GAMES

(deftest plus-test
  (testing "plus only with zero? inc dec"
    (do
      (is (= 5 (plus 2 3)))
      (is (= 4
             (plus 2 2)
             (plus 4 0)
             (plus 0 4))))))

(deftest minus-test
  (testing "minus only with zero? inc dec"
    (are [x y] (= x y)
      2 (minus 4 2)
      -2 (minus 2 4))))

(deftest addtup-test
  (testing "add every atom in a tuple"
    (are [x y] (= x y)
      6 (addtup [1 2 3])
      10 (addtup [1 2 3 4]))))

(deftest mult-test
  (testing "mult with only zero? inc dec"
    (are [x y] (= x y)
      4 (mult 2 2)
      4 (mult 4 1)
      0 (mult 4 0)
      0 (mult 0 4)
      4 (mult 1 4))))

(deftest tup+-test
  (testing "Add number in the same position"
    (are [x y] (= x y)
      [3 4 5] (tup+ [1 2 3] [2 2 2])
      [7 2 9 3] (tup+ [4 1 4 1] [3 1 5 2])
      [1 2 3 5] (tup+ [0 2] [1 0 3 5])
      [1 2 3 4] (tup+ [1 0 3 4] [0 2]))))

(deftest greater-then-test
  (testing "> with zero? inc dec"
    (are [x y] (= (> x y) (greater-then x y))
      4 5
      5 4
      5 5)))

(deftest minor-then-test
  (testing "< with zero? inc dec"
    (are [x y] (= (< x y) (minor-then x y))
      4 5
      5 4
      5 5)))

(deftest equal-test
  (testing "equal with greater-than and minus-then"
    (are [x y] (= (= x y) (equal x y))
      4 4
      3 4
      4 3)))

(deftest pow-test
  (testing "pow functions power"
    (are [x y] (= (reduce * (repeat y x)) (pow x y))
      2 2
      3 2
      2 3)))

(deftest division-test
  (testing "division functions"
    (are [x y] (= (quot x y) (division x y))
      2 2
      6 3
      6 2
      1 5)))

(deftest length-test
  (testing "count how much item there is in a sequence"
    (are [x] (= (count x) (length x))
      [1 2 3 4 5]
      [2 3]
      []
      [2 3 4 5 6 7 2 4 5])))

(deftest pick-test
  (testing "equals to get"
    (are [x y] (= (get y x) (pick x y))
      0 [1 2 3]
      3 [1 2 3 4 5 6]
      6 [1 2])))
