(ns aoc.2019.09
  (:require
   [aoc.2019.intcode :as i]
   [clojure.test :refer [deftest are]]))

(def parse i/parse)

(defn part-1 [mem]
  (first (i/run-io mem [1])))

(defn part-2 [mem]
  (first (i/run-io mem [2])))

(deftest test-examples
  (are [mem out] (= out (i/run-io mem []))
    [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
    [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]

    [1102 34915192 34915192 7 4 7 99 0]
    [1219070632396864]

    [104 1125899906842624 99]
    [1125899906842624]))
