(ns aoc.2019.01
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string)))

(defn fuel [mass]
  (- (quot mass 3) 2))

(defn fuel* [mass]
  (->> mass (iterate fuel) rest (take-while pos?) (apply +)))

(defn part-1 [input]
  (->> input (map fuel) (apply +)))

(defn part-2 [input]
  (->> input (map fuel*) (apply +)))

(deftest test-fuel*
  (is (= 2 (fuel* 14)))
  (is (= 966 (fuel* 1969)))
  (is (= 50346 (fuel* 100756))))
