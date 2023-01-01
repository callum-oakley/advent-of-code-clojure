(ns aoc.2017.02
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines (map #(map read-string (re-seq #"\d+" %)))))

(defn part-1 [sheet]
  (apply + (map #(- (apply max %) (apply min %)) sheet)))

(defn part-2 [sheet]
  (->> sheet
       (map #(some (fn [[a b]]
                     (cond
                       (int? (/ a b)) (/ a b)
                       (int? (/ b a)) (/ b a)))
                   (comb/combinations % 2)))
       (apply +)))

(deftest test-examples
  (is (= 18 (part-1 [[5 1 9 5] [7 5 3] [2 4 6 8]])))
  (is (= 9 (part-2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]))))
