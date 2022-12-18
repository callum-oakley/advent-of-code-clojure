(ns aoc.2021.17
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map read-string)))

(defn tick [[[x y] [dx dy]]]
  [[(+ x dx) (+ y dy)] [(max (dec dx) 0) (dec dy)]])

(defn hit? [[left right bottom top] velocity]
  (let [[x y] (->> [[0 0] velocity] (iterate tick) (map first)
                   (take-while (fn [[x y]] (and (<= x right) (<= bottom y))))
                   last)]
    (and (<= left x) (<= y top))))

(defn hits [[_ right bottom _ :as target]]
  (filter #(hit? target %)
          (for [dx (range (inc right))
                dy (range bottom (- bottom))]
            [dx dy])))

(defn max-height [[_ dy]]
  (/ (* dy (inc dy)) 2))

(defn part-1 [target]
  (->> target hits (map max-height) (apply max)))

(defn part-2 [target]
  (->> target hits count))

(deftest test-examples
  (is (hit? (parse "target area: x=20..30, y=-10..-5") [7 2]))
  (is (hit? (parse "target area: x=20..30, y=-10..-5") [6 3]))
  (is (hit? (parse "target area: x=20..30, y=-10..-5") [9 0]))
  (is (not (hit? (parse "target area: x=20..30, y=-10..-5") [17 -4])))
  (is (= 45 (part-1 (parse "target area: x=20..30, y=-10..-5"))))
  (is (= 112 (part-2 (parse "target area: x=20..30, y=-10..-5")))))
