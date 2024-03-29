(ns aoc.2017.11
  (:require
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

;; https://www.redblobgames.com/grids/hexagons/#coordinates-axial
(defn parse [s]
  (map #(case %
          "nw" [-1 0] "n" [0 -1] "ne" [1 -1]
          "sw" [-1 1] "s" [0  1] "se" [1  0])
       (re-seq #"[nesw]+" s)))

(defn dist [[q r]]
  (/ (+ (abs q) (abs r) (abs (+ q r))) 2))

(defn part-1 [dirs]
  (->> dirs (reduce +v) dist))

(defn part-2 [dirs]
  (->> dirs (reductions +v) (map dist) (apply max)))

(deftest test-examples
  (is (= 3 (->> "ne,ne,ne" parse (reduce +v) dist)))
  (is (= 0 (->> "ne,ne,sw,sw" parse (reduce +v) dist)))
  (is (= 2 (->> "ne,ne,s,s" parse (reduce +v) dist)))
  (is (= 3 (->> "se,sw,se,sw,sw" parse (reduce +v) dist))))
