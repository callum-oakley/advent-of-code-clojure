(ns aoc.2021.06
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [f (->> s (re-seq #"\d+") (map read-string) frequencies)]
    (mapv #(get f % 0) (range 9))))

;; Represent fish populations in a circular buffer of length 9, with a pointer
;; to the fish with internal timer 0. Then incrementing the pointer is
;; equivalent to ageing every fish by 1 day while simultaneously producing our
;; "new" timer-8 generation. The only update to the buffer itself we need to
;; make on each day is to move the timer-0 generation 7 days forwards.
(defn simulate [fish]
  (iterate (fn [[fish i]]
             [(update fish (mod (+ i 7) 9) + (fish i)) (mod (inc i) 9)])
           [fish 0]))

(defn part-* [days fish]
  (apply + (first (nth (simulate fish) days))))

(defn part-1 []
  (->> "input/2021/06" slurp parse (part-* 80)))

(defn part-2 []
  (->> "input/2021/06" slurp parse (part-* 256)))

(deftest test-example
  (let [fish (parse "3,4,3,1,2")]
    (= 26 (part-* 18 fish))
    (= 5934 (part-* 80 fish))
    (= 26984457539 (part-* 256 fish))))
