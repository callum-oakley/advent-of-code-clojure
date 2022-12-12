(ns aoc.2022.03
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map (fn [r] [(set (take (/ (count r) 2) r)) (set (drop (/ (count r) 2) r))])
       (re-seq #"\w+" s)))

(defn priority [c]
  (- (int c) (if (Character/isLowerCase c) 96 38)))

(defn part-1* [rucksacks]
  (->> rucksacks
       (map #(priority (first (set/intersection (first %) (second %)))))
       (apply +)))

(defn part-2* [rucksacks]
  (->> rucksacks
       (map #(apply set/union %))
       (partition 3)
       (map #(priority (first (apply set/intersection %))))
       (apply +)))

(defn part-1 []
  (->> "input/2022/03" slurp parse part-1*))

(defn part-2 []
  (->> "input/2022/03" slurp parse part-2*))

(def example
  "vJrwpWtwJgWrhcsFMMfFFhFp jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL PmmdzqPrVvPwwTWBwg
   wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn ttgJtRGJQctTZtZT CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest test-example
  (is (= 157 (part-1* (parse example))))
  (is (= 70 (part-2* (parse example)))))
