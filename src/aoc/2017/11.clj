(ns aoc.2017.11
  (:require
   [aoc.vectors :refer [+v]]
   [clojure.test :refer [deftest is]]))

;; https://www.redblobgames.com/grids/hexagons/#coordinates-axial
(defn parse [s]
  (map #(case %
          "nw" [-1 0] "n" [0 -1] "ne" [1 -1]
          "sw" [-1 1] "s" [0  1] "se" [1  0])
       (re-seq #"[nesw]+" s)))

(defn dist [[q r]]
  (/ (+ (Math/abs q) (Math/abs r) (Math/abs (+ q r))) 2))

(defn part-1 []
  (->> "input/2017/11" slurp parse (reduce +v) dist))

(defn part-2 []
  (->> "input/2017/11" slurp parse (reductions +v) (map dist) (apply max)))

(deftest test-examples
  (is (= 3 (->> "ne,ne,ne" parse (reduce +v) dist)))
  (is (= 0 (->> "ne,ne,sw,sw" parse (reduce +v) dist)))
  (is (= 2 (->> "ne,ne,s,s" parse (reduce +v) dist)))
  (is (= 3 (->> "se,sw,se,sw,sw" parse (reduce +v) dist))))
