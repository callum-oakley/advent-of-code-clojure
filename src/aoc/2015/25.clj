(ns aoc.2015.25)

(defn parse [s]
  (map read-string (re-seq #"\d+" s)))

(defn mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn code [n]
  (mod (* 20151125 (mod-pow 252533 (dec n) 33554393)) 33554393))

(defn triangle [n]
  (quot (* n (inc n)) 2))

(defn part-1 [[row col]]
  (code (+ col (triangle (- (+ row col) 2)))))
