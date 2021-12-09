(ns aoc.2015.18
  (:require
   [aoc.vector :refer [+v]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [lines (vec (str/split-lines s))
        max-i (count lines)]
    [max-i (set (for [x (range max-i)
                      y (range max-i)
                      :when (= \# (get-in lines [y x]))]
                  [x y]))]))

(defn neighbors [lit pos]
  (->> [[-1 -1] [ 0 -1] [+1 -1]
        [-1  0]         [+1  0]
        [-1 +1] [ 0 +1] [+1 +1]]
       (map #(+v pos %))
       (filter lit)
       count))

(defn tick [max-i lit]
  (set (for [x (range max-i)
             y (range max-i)
             :let [n (neighbors lit [x y])]
             :when (if (lit [x y])
                     (or (= 2 n) (= 3 n))
                     (= 3 n))]
         [x y])))

(defn part-* [always-lit steps [max-i lit]]
  (count (nth (iterate #(set/union always-lit (tick max-i %))
                       (set/union always-lit lit))
              steps)))

(defn part-1 []
  (part-* #{} 100 (parse (slurp "input/2015/18"))))

(defn part-2 []
  (part-* #{[0 0] [0 99] [99 0] [99 99]} 100 (parse (slurp "input/2015/18"))))

(deftest test-part-*
  (let [data (parse ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####..")]
    (is (= 4 (part-* #{} 4 data)))
    (is (= 17 (part-* #{[0 0] [0 5] [5 0] [5 5]} 5 data)))))
