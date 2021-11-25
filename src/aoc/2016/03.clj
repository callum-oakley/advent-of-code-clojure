(ns aoc.2016.03
  (:require
   [aoc.vectors :refer [transpose]]
   [clojure.test :refer [deftest is]]))

(defn part-* [f]
  (->> (slurp "input/2016/03")
       (re-seq #"\d+")
       (map read-string)
       (partition 3)
       f
       (filter #(let [[a b c] (sort %)] (> (+ a b) c)))
       count))

(defn part-1 []
  (part-* identity))

(defn part-2 []
  (part-* #(->> % (partition 3) (mapcat transpose))))
