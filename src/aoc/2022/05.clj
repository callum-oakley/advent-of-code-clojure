(ns aoc.2022.05
  (:require
   [aoc.grid :as grid]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[top bottom] (str/split s #"\n\n")
        g (grid/parse top #(when (Character/isLetter %) %))]
    [(reduce (fn [stacks [y x]]
               (update stacks (inc (/ (dec x) 4)) (fnil conj []) (g [y x])))
             {}
             (sort #(compare %2 %1) (keys g)))
     (->> bottom (re-seq #"\d+") (map read-string) (partition 3))]))

(defn rearrange [p stacks instructions]
  (reduce (fn [stacks [n from to]]
            (let [cut (- (count (stacks from)) n)
                  move (cond-> (subvec (stacks from) cut) (= 1 p) reverse)]
              (-> stacks (update from subvec 0 cut) (update to into move))))
          stacks
          instructions))

(defn part-* [p [stacks instructions]]
  (let [stacks (rearrange p stacks instructions)]
    (->> stacks keys sort (map stacks) (map peek) (apply str))))

(defn part-1 []
  (->> "input/2022/05" slurp parse (part-* 1)))

(defn part-2 []
  (->> "input/2022/05" slurp parse (part-* 2)))

(def example
  "    [D]    \n[N] [C]    \n[Z] [M] [P]\n\n1 2 1 3 1 3 2 2 1 1 1 2")

(deftest test-example []
  (is (= "CMZ" (part-* 1 (parse example))))
  (is (= "MCD" (part-* 2 (parse example)))))