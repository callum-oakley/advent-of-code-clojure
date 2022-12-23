(ns aoc.2022.23 
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(def dirs
  [grid/north grid/south grid/west grid/east])

(defn parse [s]
  (set (keep (fn [[pos c]] (when (= \# c) pos)) (grid/parse s))))

(defn proposal [round elves elf]
  (when (some elves (grid/adjacent-8 elf))
    (some (fn [[y x]]
            (when (every? #(not (elves (+v elf %)))
                          [[(+ y x) (+ x y)] [y x] [(- y x) (- x y)]])
              (+v elf [y x])))
          (map (fn [i] (dirs (mod (+ round i) 4))) (range 4)))))

(defn run [elves]
  (map first
       (iterate
        (fn [[elves round]]
          [(->> elves
                (reduce (fn [proposals elf]
                          (update proposals
                                  (proposal round elves elf) conj elf))
                        {})
                (reduce (fn [elves [proposal proposers]]
                          (if (and (some? proposal) (= 1 (count proposers)))
                            (conj elves proposal)
                            (into elves proposers)))
                        #{}))
           (inc round)])
        [elves 0])))

(defn part-1 [elves]
  (let [[[min-y max-y] [min-x max-x]] (grid/box (first (drop 10 (run elves))))]
    (- (* (- (inc max-y) min-y) (- (inc max-x) min-x)) (count elves))))

(defn part-2 [elves]
  (inc (count (take-while #(apply not= %) (partition 2 1 (run elves))))))

(def example
  "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#..")

(deftest test-example
  (is (= 110 (part-1 (parse example))))
  (is (= 20 (part-2 (parse example)))))
