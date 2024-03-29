(ns aoc.2019.19
  (:require
   [aoc.2019.intcode :as i]))

(def parse i/parse)

(defn part-1 [mem]
  (let [beam? #(= [1] (i/run-io mem %))]
    (->> (for [x (range 50) y (range 50)] [x y])
         (filter beam?)
         count)))

(defn part-2 [mem]
  (let [beam? #(= [1] (i/run-io mem %))]
    (loop [x 0 y 0]
      (cond
        (not (beam? [(+ 99 x) y])) (recur x (inc y))
        (not (beam? [x (+ 99 y)])) (recur (inc x) y)
        :else (+ (* 10000 x) y)))))
