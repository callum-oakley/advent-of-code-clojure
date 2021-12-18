(ns aoc.2017.25
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  [(->> s (re-find #"Begin in state (.)") second read-string)
   (->> s (re-find #"checksum after (\d+)") second read-string)
   (->> s (re-seq #"(.):\n.*\n.*(0|1)\.\n.*(left|right)\.\n.*(.)\.\n.*\n.*(0|1)\.\n.*(left|right)\.\n.*(.)\.")
        (map #(let [[state write0 move0 state0 write1 move1 state1]
                    (map read-string (rest %))]
                [state {0 [write0 move0 state0] 1 [write1 move1 state1]}]))
        (into {}))])

(defn run [[initial-state steps transitions]]
  (->> (nth (iterate
             (fn [[tape head state]]
               (let [[write move state]
                     (get-in transitions [state (if (tape head) 1 0)])]
                 [(case write 1 (conj tape head) 0 (disj tape head))
                  ((case move left dec right inc) head)
                  state]))
             [#{} 0 initial-state])
            steps)
       first
       count))

(defn part-1 []
  (->> "input/2017/25" slurp parse run))

(deftest test-example
  (is (= 3 (run (parse "Begin in state A.
                        Perform a diagnostic checksum after 6 steps.

                        In state A:
                          If the current value is 0:
                            - Write the value 1.
                            - Move one slot to the right.
                            - Continue with state B.
                          If the current value is 1:
                            - Write the value 0.
                            - Move one slot to the left.
                            - Continue with state B.

                        In state B:
                          If the current value is 0:
                            - Write the value 1.
                            - Move one slot to the left.
                            - Continue with state A.
                          If the current value is 1:
                            - Write the value 1.
                            - Move one slot to the right.
                            - Continue with state A.")))))
