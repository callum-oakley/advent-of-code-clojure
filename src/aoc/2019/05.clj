(ns aoc.2019.05
  (:require
   [aoc.2019.intcode :as i]
   [clojure.test :refer [deftest is]]))

(def parse i/parse)

(defn part-1 [mem]
  (last (i/run-io mem [1])))

(defn part-2 [mem]
  (last (i/run-io mem [5])))

(deftest test-diagnostics
  (is (every? zero? (butlast (i/run-io (parse (slurp "input/2019/05")) [1])))))
