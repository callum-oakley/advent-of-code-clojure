(ns aoc.2015.09
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce (fn [distances line]
            (let [[_ from to d] (re-matches #"\s*(\w+) to (\w+) = (\d+)" line)
                  d (read-string d)]
              (assoc distances [from to] d [to from] d)))
          {}
          (str/split-lines s)))

;; We have few enough locations that we can test every route
(defn part-* [optimal distances]
  (->> distances keys flatten set comb/permutations
       (map #(->> (partition 2 1 %)
                  (map distances)
                  (apply +)))
       (apply optimal)))

(defn part-1 []
  (part-* min (parse (slurp "input/2015/09"))))

(defn part-2 []
  (part-* max (parse (slurp "input/2015/09"))))

(deftest test-part-*
  (let [distances (parse "London to Dublin = 464
                          London to Belfast = 518
                          Dublin to Belfast = 141")]
    (is (= 605 (part-* min distances)))
    (is (= 982 (part-* max distances)))))

(deftest test-answers
  (is (= 207 (part-1)))
  (is (= 804 (part-2))))
