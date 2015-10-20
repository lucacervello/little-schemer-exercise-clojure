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
