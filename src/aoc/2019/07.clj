(ns aoc.2019.07
  (:require
   [aoc.2019.intcode :as i]
   [clojure.math.combinatorics :as comb]
   [clojure.test :refer [deftest is]]))

(defn amp-1 [mem phases]
  (reduce (fn [signal phase] (first (i/run-io mem [phase signal]))) 0 phases))

(defn amp-2 [mem phases]
  (loop [[a b c d e] (update (mapv #(i/>> (i/run mem) %) phases) 0 i/>> 0)]
    (case [(:state a) (:state b)]
      [:out :in] (recur [(i/>> a) (i/>> b (:out a)) c d e])
      [:out :halt] (:out a)
      (recur [b c d e a]))))

(defn part-1* [mem]
  (apply max (map #(amp-1 mem %) (comb/permutations (range 5)))))

(defn part-2* [mem]
  (apply max (map #(amp-2 mem %) (comb/permutations (range 5 10)))))

(defn part-1 []
  (part-1* (i/load "input/2019/07")))

(defn part-2 []
  (part-2* (i/load "input/2019/07")))

(deftest test-example
  (is (= 43210 (part-1* [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (part-1* [3 23 3 24 1002 24 10 24 1002 23 -1 23
                         101 5 23 23 1 24 23 23 4 23 99 0 0])))
  (is (= 65210 (part-1* [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                         1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])))
  (is (= 139629729 (part-2* [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
                             27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5])))
  (is (= 18216 (part-2* [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55
                         26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001
                         55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0
                         0 0 10]))))
