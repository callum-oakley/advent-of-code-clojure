(ns aoc.2015.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn look-and-say [s]
  (apply str (eduction (partition-by identity)
                       (map #(str (count %) (first %)))
                       s)))

(defn part-* [n]
  (count (nth (iterate look-and-say (str/trim (slurp "input/2015/10"))) n)))

(defn part-1 []
  (part-* 40))

(defn part-2 []
  (part-* 50))

(deftest test-look-and-say
  (is (= ["1" "11" "21" "1211" "111221" "312211"]
         (take 6 (iterate look-and-say "1")))))
