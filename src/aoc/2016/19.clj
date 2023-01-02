(ns aoc.2016.19
  (:require
   [clojure.test :refer [deftest is]]))

(def parse read-string)

(defn part-1 [n]
  (loop [size n
         top 0
         left (transient (mapv #(mod (inc %) n) (range n)))]
    (if (= 1 size)
      (inc top)
      (let [left (assoc! left top (left (left top)))]
        (recur (dec size) (left top) left)))))

(defn part-2 [n]
  (loop [size n
         top 0
         bottom (dec (quot n 2))
         left (transient (mapv #(mod (inc %) n) (range n)))]
    (if (= 1 size)
      (inc top)
      (let [left (assoc! left bottom (left (left bottom)))]
        (recur (dec size) (left top) (cond-> bottom (odd? size) left) left)))))

(deftest test-examples
  (is (= 3 (part-1 5)))
  (is (= 2 (part-2 5))))
