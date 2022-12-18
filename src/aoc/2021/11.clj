(ns aoc.2021.11
  (:require
   [aoc.grid :as grid]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(- (int %) (int \0))))

(defn inc-energy [octopus->energy octopuses]
  (reduce (fn [[octopus->energy excited] octopus]
            [(update octopus->energy octopus inc)
             (if (= 9 (octopus->energy octopus))
               (conj excited octopus)
               excited)])
          [octopus->energy []]
          octopuses))

(defn flash [octopus->energy octopuses]
  (loop [octopus->energy octopus->energy
         excited octopuses
         flashed []]
    (if (seq excited)
      (let [[octopus->energy excited*]
            (inc-energy octopus->energy
                        (grid/adjacent-8 (peek excited) octopus->energy))]
        (recur octopus->energy
               (into (pop excited) excited*)
               (conj flashed (peek excited))))
      [octopus->energy flashed])))

(defn step [octopus->energy]
  (let [[octopus->energy excited] (inc-energy octopus->energy
                                              (keys octopus->energy))
        [octopus->energy flashed] (flash octopus->energy excited)]
    [(reduce #(assoc %1 %2 0) octopus->energy flashed) (count flashed)]))

(defn part-1* [steps octopus->energy]
  (->> [octopus->energy 0]
       (iterate (fn [[octopus->energy flashes]]
                  (let [[octopus->energy flashes*] (step octopus->energy)]
                    [octopus->energy (+ flashes flashes*)])))
       (drop steps) first second))

(defn part-1 [octopus->energy]
  (part-1* 100 octopus->energy))

(defn part-2 [octopus->energy]
  (loop [octopus->energy octopus->energy steps 0]
    (let [[octopus->energy flashes] (step octopus->energy)]
      (if (= flashes (count octopus->energy))
        (inc steps)
        (recur octopus->energy (inc steps))))))

(def sample
  (str/join "\n"
            ["5483143223" "2745854711" "5264556173" "6141336146" "6357385478"
             "4167524645" "2176841721" "6882881134" "4846848554" "5283751526"]))

(deftest test-examples
  (is (= 9 (part-1* 1 (parse "11111\n19991\n19191\n19991\n11111"))))
  (is (= 204 (part-1* 10 (parse sample))))
  (is (= 1656 (part-1 (parse sample))))
  (is (= 195 (part-2 (parse sample)))))
