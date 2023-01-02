(ns aoc.2016.03
  (:require
   [aoc.vector :refer [transpose]]))

(defn parse [s]
  (partition 3 (map read-string (re-seq #"\d+" s))))

(defn part-1 [triangles]
  (count (filter #(let [[a b c] (sort %)] (> (+ a b) c)) triangles)))

(defn part-2 [triangles]
  (part-1 (mapcat transpose (partition 3 triangles))))
