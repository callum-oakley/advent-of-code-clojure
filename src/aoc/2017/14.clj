(ns aoc.2017.14
  (:require
   [aoc.2017.10 :refer [knot-hash]]
   [aoc.hash :as hash]
   [aoc.search :as search]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn disk [seed]
  (->> (range 128)
       (mapcat (fn [y]
                 (->> (str seed "-" y) knot-hash hash/bits
                      (keep-indexed (fn [x b] (when b [x y]))))))
       set))

(defn part-2* [seed]
  (let [disk (disk seed)
        adjacent (fn [a]
                   (->> [[0 1] [0 -1] [1 0] [-1 0]]
                        (map #(+v a %))
                        (filter disk)))]
    (loop [regions 0 unexplored disk]
      (if-let [a (first unexplored)]
        (recur (inc regions)
               (apply disj unexplored (search/dft a adjacent identity)))
        regions))))

(defn part-1 []
  (->> "input/2017/14" slurp str/trim disk count))

(defn part-2 []
  (->> "input/2017/14" slurp str/trim part-2*))

(deftest test-example
  (is (= 8108 (count (disk "flqrgnkx"))))
  (is (= 1242 (part-2* "flqrgnkx"))))
