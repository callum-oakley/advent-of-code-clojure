(ns aoc.2021.02
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\w+") (map read-string) (partition 2)))

;; This covers parts 1 and 2: for part-1 treat aim as depth and ignore dep
(defn dive [commands]
  (reduce (fn [[hor aim dep] [op x]]
            (case op
              forward [(+ hor x) aim (+ (* aim x) dep)]
              down [hor (+ aim x) dep]
              up [hor (- aim x) dep]))
          [0 0 0]
          commands))

(defn part-1 []
  (let [[hor dep _] (->> "input/2021/02" slurp parse dive)]
    (* hor dep)))

(defn part-2 []
  (let [[hor _ dep] (->> "input/2021/02" slurp parse dive)]
    (* hor dep)))

(deftest test-example
  (let [commands (parse "forward 5 down 5 forward 8 up 3 down 8 forward 2")]
    (is (= [15 10 60] (dive commands)))))
