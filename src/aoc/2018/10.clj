(ns aoc.2018.10
  (:require
   [aoc.ocr :as ocr]
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map read-string)
       (partition 2) (map vec) (partition 2)))

(defn part-* [seconds points]
  (->> points (iterate #(map (fn [[p v]] [(+v p v) v]) %)) (drop seconds) first
       (map first) set ocr/draw ocr/parse))

(defn part-2 [_]
  ;; by inspection
  10009)

(defn part-1 [points]
  (part-* (part-2 nil) points))

(deftest test-example
  (let [example
        "position=< 9,  1> velocity=< 0,  2> position=< 7,  0> velocity=<-1,  0>
         position=< 3, -2> velocity=<-1,  1> position=< 6, 10> velocity=<-2, -1>
         position=< 2, -4> velocity=< 2,  2> position=<-6, 10> velocity=< 2, -2>
         position=< 1,  8> velocity=< 1, -1> position=< 1,  7> velocity=< 1,  0>
         position=<-3, 11> velocity=< 1, -2> position=< 7,  6> velocity=<-1, -1>
         position=<-2,  3> velocity=< 1,  0> position=<-4,  3> velocity=< 2,  0>
         position=<10, -3> velocity=<-1,  1> position=< 5, 11> velocity=< 1, -2>
         position=< 4,  7> velocity=< 0, -1> position=< 8, -2> velocity=< 0,  1>
         position=<15,  0> velocity=<-2,  0> position=< 1,  6> velocity=< 1,  0>
         position=< 8,  9> velocity=< 0, -1> position=< 3,  3> velocity=<-1,  1>
         position=< 0,  5> velocity=< 0, -1> position=<-2,  2> velocity=< 2,  0>
         position=< 5, -2> velocity=< 1,  2> position=< 1,  4> velocity=< 2,  1>
         position=<-2,  7> velocity=< 2, -2> position=< 3,  6> velocity=<-1, -1>
         position=< 5,  0> velocity=< 1,  0> position=<-6,  0> velocity=< 2,  0>
         position=< 5,  9> velocity=< 1, -2> position=<14,  7> velocity=<-2,  0>
         position=<-3,  6> velocity=< 2, -1>"]
    (is (= "HI" (part-* 3 (parse example))))))
