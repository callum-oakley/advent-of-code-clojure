(ns aoc.2021.13
  (:require
   [aoc.ocr :as ocr]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  [(->> s (re-seq #"(\d+),(\d+)") (map #(map read-string (rest %))))
   (->> s (re-seq #"(x|y)=(\d+)") (map #(map read-string (rest %))))])

(defn fold [dots [axis a]]
  (set (map (fn [[x y]]
              (case axis
                x [(if (< a x) (- (* 2 a) x) x) y]
                y [x (if (< a y) (- (* 2 a) y) y)]))
            dots)))

(defn part-1* [[dots folds]]
  (count (fold dots (first folds))))

(defn part-1 []
  (->> "input/2021/13" slurp parse part-1*))

(defn part-2 []
  (let [[dots folds] (->> "input/2021/13" slurp parse)]
    (ocr/parse (ocr/draw (reduce fold dots folds)))))

(deftest test-examples
  (is (= 17 (part-1* (parse "6,10 0,14 9,10 0,3 10,4 4,11 6,0 6,12 4,1 0,13
                             10,12 3,4 3,0 8,4 1,10 2,14 8,10 9,0
                             fold along y=7 fold along x=5")))))
