(ns aoc.2015.03
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn part-1* [instructions]
  (set (reductions (fn [[x y] move]
                     (case move
                       \^ [x (inc y)] \> [(inc x) y]
                       \v [x (dec y)] \< [(dec x) y]))
                   [0 0]
                   instructions)))

(defn part-1 [s]
  (count (part-1* s)))

(defn part-2 [instructions]
  (count (set/union (part-1* (flatten (partition 1 2 instructions)))
                    (part-1* (flatten (partition 1 2 (rest instructions)))))))

(deftest test-part-1
  (is (= 2 (part-1 ">")))
  (is (= 4 (part-1 "^>v<")))
  (is (= 2 (part-1 "^v^v^v^v^v"))))

(deftest test-part-2
  (is (= 3 (part-2 "^v")))
  (is (= 3 (part-2 "^>v<")))
  (is (= 11 (part-2 "^v^v^v^v^v"))))
