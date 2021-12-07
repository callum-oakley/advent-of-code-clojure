(ns aoc.2021.07
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string)))

(defn part-* [cost xs]
  (loop [low (apply min xs) high (apply max xs)]
    (if (= (inc low) high)
      (min (cost low xs) (cost high xs))
      (let [mid (quot (+ low high) 2)]
        (if (< (cost mid xs) (cost (inc mid) xs))
          (recur low mid)
          (recur mid high))))))

(defn cost-1 [align xs]
  (apply + (map #(Math/abs (- % align)) xs)))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn cost-2 [align xs]
  (apply + (map #(triangle (Math/abs (- % align))) xs)))

(defn part-1 []
  (->> "input/2021/07" slurp parse (part-* cost-1)))

(defn part-2 []
  (->> "input/2021/07" slurp parse (part-* cost-2)))

(deftest test-example
  (is (= 37 (part-* cost-1 (parse "16,1,2,0,4,2,7,1,2,14"))))
  (is (= 168 (part-* cost-2 (parse "16,1,2,0,4,2,7,1,2,14")))))
