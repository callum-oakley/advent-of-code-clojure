(ns aoc.2017.12
  (:require
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines
       (map #(let [[a & bs] (map read-string (re-seq #"\d+" %))] [a bs]))
       (into {})))

(defn part-1* [graph]
  (count (search/dft 0 graph identity)))

(defn part-2* [graph]
  (loop [components 0 unexplored (set (keys graph))]
    (if-let [a (first unexplored)]
      (recur (inc components)
             (apply disj unexplored (search/dft a graph identity)))
      components)))

(defn part-1 []
  (->> "input/2017/12" slurp parse part-1*))

(defn part-2 []
  (->> "input/2017/12" slurp parse part-2*))

(deftest test-example
  (let [graph (parse "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4
                      4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")]
    (is (= 6 (part-1* graph)))
    (is (= 2 (part-2* graph)))))
