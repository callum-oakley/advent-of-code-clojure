(ns aoc.2022.15
  (:require
   [aoc.vector :refer [manhattan-distance]]
   [clojure.math.combinatorics :as comb]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [readings (->> s (re-seq #"-?\d+") (map read-string) (partition 2)
                      (map #(vec (reverse %))) (partition 2))]
    {:sensors (map (fn [[s b]] [s (manhattan-distance s b)]) readings)
     :beacons (set (map second readings))}))

;; Cheating a little by assuming there are no gaps. This turns out to be true.
(defn part-1* [y {:keys [sensors beacons]}]
  (let [min-x (apply min (map (fn [[[sy sx] r]] (- sx (- r (abs (- sy y)))))
                              sensors))
        max-x (apply max (map (fn [[[sy sx] r]] (+ sx (- r (abs (- sy y)))))
                              sensors))]
    (- (inc max-x) min-x (count (filter #(= y (first %)) beacons)))))

;; Find the intersections of the 8 lines that make up the two circles, and then
;; filter those that fall within the boundary.
(defn intersections [s0 s1]
  (for [[[[y0 x0] r0] [[y1 x1] r1]] [[s0 s1] [s1 s0]]
        sign0 [+ -] sign1 [+ -]
        :let [a (- y0 x0 (sign0 r0)) b (+ y1 x1 (sign1 r1))
              pos [(/ (+ b a) 2) (/ (- b a) 2)]]
        :when (and (every? int? pos)
                   (= (manhattan-distance pos [y0 x0]) r0)
                   (= (manhattan-distance pos [y1 x1]) r1))]
    pos))

(defn part-2* [bound {:keys [sensors]}]
  (->> (comb/combinations sensors 2)
       (mapcat (fn [[[c0 r0] [c1 r1]]]
                 (intersections [c0 (inc r0)] [c1 (inc r1)])))
       (remove (fn [pos]
                 (or (not (every? #(<= 0 % bound) pos))
                     (some (fn [[c r]] (<= (manhattan-distance c pos) r))
                           sensors))))
       first ((fn [[y x]] (+ y (* 4000000 x))))))

(defn part-1 [readings]
  (part-1* 2000000 readings))

(defn part-2 [readings]
  (part-2* 4000000 readings))

(def example
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
   Sensor at x=9, y=16: closest beacon is at x=10, y=16
   Sensor at x=13, y=2: closest beacon is at x=15, y=3
   Sensor at x=12, y=14: closest beacon is at x=10, y=16
   Sensor at x=10, y=20: closest beacon is at x=10, y=16
   Sensor at x=14, y=17: closest beacon is at x=10, y=16
   Sensor at x=8, y=7: closest beacon is at x=2, y=10
   Sensor at x=2, y=0: closest beacon is at x=2, y=10
   Sensor at x=0, y=11: closest beacon is at x=2, y=10
   Sensor at x=20, y=14: closest beacon is at x=25, y=17
   Sensor at x=17, y=20: closest beacon is at x=21, y=22
   Sensor at x=16, y=7: closest beacon is at x=15, y=3
   Sensor at x=14, y=3: closest beacon is at x=15, y=3
   Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(deftest test-example
  (is (= 26 (part-1* 10 (parse example))))
  (is (= 56000011 (part-2* 20 (parse example)))))
