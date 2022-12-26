(ns aoc.2018.07
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [instructions]
  (reduce (fn [[steps deps] [a b]]
            [(conj steps a b) (update deps b (fnil conj #{}) a)])
          [#{} {}]
          (partition 2 (map first (re-seq #"\b\w\b" instructions)))))

(defn part-1 [[steps deps]]
  (when-let [step (first (sort (filter #(empty? (deps %)) steps)))]
    (str step (part-1 [(disj steps step)
                       (update-vals deps #(disj % step))]))))

(defn part-2* [workers base [steps deps]]
  (loop [steps steps deps deps t 0 tasks nil]
    (let [step (first (sort (filter #(empty? (deps %)) steps)))]
      (cond
        (and step (< (count tasks) workers))
        (recur (disj steps step)
               deps
               t
               (sort-by :timer (conj tasks {:timer (+ base (- (int step) 64))
                                            :step step})))
        (seq tasks)
        (recur steps
               (update-vals deps #(disj % (:step (first tasks))))
               (+ t (:timer (first tasks)))
               (map #(update % :timer - (:timer (first tasks))) (rest tasks)))
        :else
        t))))

(defn part-2 [input]
  (part-2* 5 60 input))

(deftest test-example
  (let [instructions "Step C must be finished before step A can begin.
                      Step C must be finished before step F can begin.
                      Step A must be finished before step B can begin.
                      Step A must be finished before step D can begin.
                      Step B must be finished before step E can begin.
                      Step D must be finished before step E can begin.
                      Step F must be finished before step E can begin."]
    (is (= "CABDFE" (part-1 (parse instructions))))
    (is (= 15 (part-2* 2 0 (parse instructions))))))
