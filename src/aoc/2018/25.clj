(ns aoc.2018.25
  (:require
   [aoc.vector :refer [manhattan-distance]]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (partition 4 (map read-string (re-seq #"-?\d+" s))))

(defn connected? [point constellation]
  (boolean (some #(<= (manhattan-distance point %) 3) constellation)))

(defn part-1 [points]
  (count (reduce (fn [constellations point]
                   (let [{connected true disconnected false}
                         (group-by #(connected? point %) constellations)]
                     (conj disconnected (apply set/union #{point} connected))))
                 []
                 points)))

(deftest test-examples
  (is (= 2 (part-1 (parse "0 0 0 0 3 0 0 0 0 3 0 0 0 0 3 0 0 0 0 3 0 0 0 6 9 0 0
                            0 2 0 0 0"))))
  (is (= 4 (part-1 (parse "-1 2 2 0 0 0 2 -2 0 0 0 -2 -1 2 0 0 -2 -2 -2 2 3 0 2 
                           -1 -1 3 2 2 -1 0 -1 0 0 2 1 -2 3 0 0 0"))))
  (is (= 3 (part-1 (parse "1 -1 0 1 2 0 -1 0 3 2 -1 0 0 0 3 1 0 0 -1 -1 2 3 -2 0
                           -2 2 0 0 2 -2 0 -1 1 -1 0 -1 3 2 0 2"))))
  (is (= 8 (part-1 (parse "1 -1 -1 -2 -2 -2 0 1 0 2 1 3 -2 3 -2 1 0 2 3 -2 -1 -1
                           1 -2 0 -2 -1 0 -2 2 3 -1 1 2 2 0 -1 -2 0 -2")))))
