(ns aoc.2017.06
  (:require
   [clojure.test :refer [deftest is]]))

(defn target-bank [banks]
  (let [max-bank (apply max banks)]
    (first (keep-indexed (fn [i blocks] (when (= max-bank blocks) i)) banks))))

(defn redistribute [banks]
  (let [t (target-bank banks)]
    (loop [blocks (banks t)
           banks (assoc banks t 0)
           i (mod (inc t) (count banks))]
      (if (pos? blocks)
        (recur (dec blocks) (update banks i inc) (mod (inc i) (count banks)))
        banks))))

(defn part-* [step seen banks]
  (if-let [seen-at-step (seen banks)]
    [step (- step seen-at-step)]
    (recur (inc step) (assoc seen banks step) (redistribute banks))))

(defn part-1 []
  (->> "input/2017/06" slurp (re-seq #"\d+") (mapv read-string)
       (part-* 0 {}) first))

(defn part-2 []
  (->> "input/2017/06" slurp (re-seq #"\d+") (mapv read-string)
       (part-* 0 {}) second))

(deftest test-examples
  (is (= [[0 2 7 0] [2 4 1 2] [3 1 2 3] [0 2 3 4] [1 3 4 1] [2 4 1 2]]
         (take 6 (iterate redistribute [0 2 7 0]))))
  (is (= [5 4] (part-* 0 {} [0 2 7 0]))))
