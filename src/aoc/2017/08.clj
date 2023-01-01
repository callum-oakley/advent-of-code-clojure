(ns aoc.2017.08
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines (map #(map read-string (re-seq #"\S+" %)))))

(defn run [instructions]
  (reductions (fn [reg [a op x _ b cmp y]]
                (if ((case cmp == = != not= (resolve cmp)) (get reg b 0) y)
                  (update reg a (fnil (case op inc + dec -) 0) x)
                  reg))
              {}
              instructions))

(defn part-1 [instructions]
  (->> instructions run last vals (apply max)))

(defn part-2 [instructions]
  (->> instructions run (mapcat vals) (apply max)))

(deftest test-example
  (let [instructions (parse "b inc 5 if a > 1
                             a inc 1 if b < 5
                             c dec -10 if a >= 1
                             c inc -20 if c == 10")]
    (is (= [{} {} '{a 1} '{a 1 c 10} '{a 1 c -10}] (run instructions)))))
