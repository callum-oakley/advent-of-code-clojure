(ns aoc.2015.13
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce #(let [[_ a b c d]
                 (re-matches #"\s*(\w+) would (gain|lose) (\d+) .* (\w+)." %2)]
             (assoc %1 [a d] (({"gain" + "lose" -} b) (read-string c))))
          {}
          (str/split-lines s)))

(defn part-* [happiness]
  (let [guests (-> happiness keys flatten set)]
    (->> (comb/permutations (rest guests))
         (map #(->> (concat [(first guests)] % [(first guests)])
                    (partition 2 1)
                    (map (fn [[a b]] (+ (happiness [a b]) (happiness [b a]))))
                    (apply +)))
         (apply max))))

(defn part-1 []
  (-> "input/2015/13" slurp parse part-*))

(defn part-2 []
  (let [happiness (-> "input/2015/13" slurp parse)]
    (part-* (reduce #(assoc %1 [:me %2] 0 [%2 :me] 0)
                    happiness
                    (-> happiness keys flatten set)))))

(deftest test-part-*
  (let [s "Alice would gain 54 happiness units by sitting next to Bob.
           Alice would lose 79 happiness units by sitting next to Carol.
           Alice would lose 2 happiness units by sitting next to David.
           Bob would gain 83 happiness units by sitting next to Alice.
           Bob would lose 7 happiness units by sitting next to Carol.
           Bob would lose 63 happiness units by sitting next to David.
           Carol would lose 62 happiness units by sitting next to Alice.
           Carol would gain 60 happiness units by sitting next to Bob.
           Carol would gain 55 happiness units by sitting next to David.
           David would gain 46 happiness units by sitting next to Alice.
           David would lose 7 happiness units by sitting next to Bob.
           David would gain 41 happiness units by sitting next to Carol."]
    (is (= 330 (part-* (parse s))))))

(deftest test-ansers
  (is (= 709 (part-1)))
  (is (= 668 (part-2))))
