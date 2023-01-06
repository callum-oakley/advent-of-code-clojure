(ns aoc.2015.10
  (:require
   [clojure.test :refer [deftest is]]))

(defn look-and-say [s]
  (apply str (eduction (partition-by identity)
                       (map #(str (count %) (first %)))
                       s)))

(defn part-* [n s]
  (count (nth (iterate look-and-say s) n)))

(defn part-1 [s]
  (part-* 40 s))

(defn part-2 [s]
  (part-* 50 s))

(deftest test-look-and-say
  (is (= ["1" "11" "21" "1211" "111221" "312211"]
         (take 6 (iterate look-and-say "1")))))
