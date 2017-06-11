(ns lagrangian.vec2d-test
  (:require [clojure.test :refer :all]
            [lagrangian.vec2d :refer [seq-distance seq-abs]]))

(def a [23 12])
(def b [8 4])

(deftest seq-abs-test
  (testing "absolute value of zero vector is zero"
    (is (= 0.0 (seq-abs [0 0 0 0]))))
  (testing "empty vector has absolute value zero"
    (is (= 0.0 (seq-abs []))))
  (testing "pythagorean triplets"
    (is (= 5.0 (seq-abs [3 4])))))

(deftest seq-distance-test
  (testing "same vectors have zero distance"
    (is (= 0.0 (seq-distance [1 2] [1 2]))))
  (testing "vectors of same direction have simple distance"
    (is (= 10.0 (seq-distance [15 0] [25 0]))))
  (testing "should be independent of vector ordering"
    (is (= (seq-distance a b) (seq-distance b a))))
  (testing "pythagorean triplets work" 
    (is (= (seq-distance [3 4] [5 6])))))



