(ns aoc.2022.15
  (:require
   [aoc.vector :refer [manhattan-distance +v]]
   [aoc.search :as search]
   [clojure.math.numeric-tower :as math]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [readings (->> s (re-seq #"-?\d+") (map read-string) (partition 2)
                      (map #(vec (reverse %))) (partition 2))]
    {:sensors (map (fn [[s b]] {:pos s :r (manhattan-distance s b)}) readings)
     :beacons (set (map second readings))}))

;; Cheating a little by assuming there are no gaps. This turns out to be true.
(defn part-1* [y {:keys [sensors beacons]}]
  (let [min-x (apply min (map (fn [{[sy sx] :pos r :r}]
                                (- sx (- r (abs (- sy y)))))
                              sensors))
        max-x (apply max (map (fn [{[sy sx] :pos r :r}]
                                (+ sx (- r (abs (- sy y)))))
                              sensors))]
    (- (inc max-x) min-x (count (filter #(= y (first %)) beacons)))))

;; Does the given sensor cover the given box entirely?
(defn covers? [{sensor :pos r :r} {box :pos w :w}]
  (every? #(<= (manhattan-distance sensor %) r)
          (for [y [0 (dec w)] x [0 (dec w)]] (+v box [y x]))))

;; Inspired by 2018 day 23. DFS on the space of progressively smaller bounding
;; boxes, discarding any boxes which are covered by a sensor (in which case they
;; couldn't possibly contain a beacon).
(defn part-2* [bound {:keys [sensors]}]
  ((fn [{[y x] :pos}] (+ y (* x 4000000)))
   (search/dfs {:pos [0 0] :w (inc bound)}
               (fn [{:keys [pos w]}]
                 (remove (fn [box] (some #(covers? % box) sensors))
                         (for [y [0 (math/floor (/ w 2))]
                               x [0 (math/floor (/ w 2))]]
                           {:pos (+v pos [y x]) :w (math/ceil (/ w 2))})))
               identity
               #(= (:w %) 1))))

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
