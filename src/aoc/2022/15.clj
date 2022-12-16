(ns aoc.2022.15
  (:require
   [aoc.vector :refer [+v manhattan-distance]]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map read-string) (partition 2)
       (map #(vec (reverse %))) (partition 2)))

(defn part-1* [y readings]
  (count (set/difference
          (set (mapcat (fn [[[sy sx :as sensor] beacon]]
                         (let [r (manhattan-distance sensor beacon)
                               z (- r (abs (- sy y)))]
                           (map (fn [x] [y x])
                                (range (- sx z) (inc (+ sx z)))))) readings))
          (set (map second readings)))))

(defn circle [c r]
  (mapcat (fn [i]
            (map #(+v c %)
                 [[i (- r i)] [(- r i) (- i)] [(- i) (- i r)] [(- i r) i]]))
          (range r)))

(defn inside? [[sensor r] pos]
  (<= (manhattan-distance pos sensor) r))

(defn part-2* [bound readings]
  (let [scans (map (fn [[sensor beacon]]
                     [sensor (manhattan-distance sensor beacon)])
                   readings)
        [y x] (->> scans
                   (mapcat (fn [[sensor r]] (circle sensor (inc r))))
                   (remove (fn [pos] (or (not (every? #(<= 0 % bound) pos))
                                         (some #(inside? % pos) scans))))
                   first)]
    (+ y (* 4000000 x))))

(defn part-1 [readings]
  (part-1* 2000000 readings))

;; TODO this is slow (~114s)
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
