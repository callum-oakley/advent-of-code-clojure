(ns aoc.2019.05
  (:require
   [aoc.2019.intcode :as i]
   [clojure.test :refer [deftest is]]))

(defn diagnostics [in]
  (i/run-io (i/load "input/2019/05") [in]))

(defn part-1 []
  (last (diagnostics 1)))

(defn part-2 []
  (last (diagnostics 5)))

(deftest test-diagnostics
  (is (every? zero? (butlast (diagnostics 1)))))
