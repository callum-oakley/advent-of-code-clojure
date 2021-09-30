(ns aoc.2015.10
  (:require
   [clojure.test :refer [deftest is]]))

(defn look-and-say [s]
  (->> (partition-by identity s)
       (map #(str (count %) (first %)))
       (apply str)))

(defn part-* [n]
  (count (nth (iterate look-and-say (slurp "input/2015/10")) n)))

(defn part-1 []
  (part-* 40))

(defn part-2 []
  (part-* 50))

(deftest test-look-and-say
  (is (= ["1" "11" "21" "1211" "111221" "312211"]
         (take 6 (iterate look-and-say "1")))))

(deftest test-answers
  (is (= 492982 (part-1)))
  (is (= 6989950 (part-2))))
