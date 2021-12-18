(ns aoc.2015.06
  (:require
   [clojure.string :as str]))

(defn parse [instruction]
  (let [[_ op x0 y0 x1 y1]
        (re-matches
         #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
         instruction)]
    [op (read-string x0) (read-string y0) (read-string x1) (read-string y1)]))

(defn part-* [op->f]
  (->> (slurp "input/2015/06")
       str/split-lines
       (map parse)
       (reduce (fn [lights [op x0 y0 x1 y1]]
                 (let [f (op->f op)]
                   (reduce (fn [lights i]
                             (assoc! lights i (f (lights i))))
                           lights
                           (for [x (range x0 (inc x1))
                                 y (range y0 (inc y1))]
                             (+ x (* 1000 y))))))
               (transient (vec (repeat (* 1000 1000) 0))))
       persistent!
       (apply +)))

(defn part-1 []
  (part-* (fn [op]
            (case op
              "turn on" (constantly 1)
              "turn off" (constantly 0)
              "toggle" {0 1 1 0}))))

(defn part-2 []
  (part-* (fn [op]
            (case op
              "turn on" inc
              "turn off" #(max 0 (dec %))
              "toggle" #(+ 2 %)))))
