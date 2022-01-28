(ns aoc.2019.01
  (:require
   [clojure.test :refer [deftest is]]))

(defn fuel [mass]
  (- (quot mass 3) 2))

(defn fuel* [mass]
  (->> mass (iterate fuel) rest (take-while pos?) (apply +)))

(defn part-1 []
  (->> "input/2019/01" slurp (re-seq #"\d+") (map parse-long)
       (map fuel) (apply +)))

(defn part-2 []
  (->> "input/2019/01" slurp (re-seq #"\d+") (map parse-long)
       (map fuel*) (apply +)))

(deftest test-fuel*
  (is (= 2 (fuel* 14)))
  (is (= 966 (fuel* 1969)))
  (is (= 50346 (fuel* 100756))))
