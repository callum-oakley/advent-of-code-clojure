(ns aoc.2017.17
  (:require
   [clojure.test :refer [deftest is]]))

(defn part-1* [steps]
  (loop [ring [0] head 0 i 1]
    (if (= 2017 i)
      (ring (inc head))
      (recur (vec (concat (subvec ring 0 (inc head))
                          [i]
                          (subvec ring (inc head))))
             (mod (+ steps head 1) (inc i))
             (inc i)))))

;; As above but only keep track of the value to the right of 0
(defn part-2* [steps]
  (loop [res nil head 0 i 1]
    (if (< 50000000 i)
      res
      (recur (if (zero? head) i res)
             (mod (+ steps head 1) (inc i))
             (inc i)))))

(defn part-1 []
  (->> "input/2017/17" slurp read-string part-1*))

(defn part-2 []
  (->> "input/2017/17" slurp read-string part-2*))

(deftest test-example
  (is (= 638 (part-1* 3))))
