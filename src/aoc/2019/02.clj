(ns aoc.2019.02
  (:require
   [aoc.2019.intcode :as i]
   [clojure.test :refer [deftest are]]))

(def parse i/parse)

(defn gravity-assist [mem noun verb]
  (first (:mem (i/run (assoc mem 1 noun 2 verb)))))

(defn part-1 [mem]
  (gravity-assist mem 12 2))

(defn part-2 [mem]
  (some (fn [[noun verb]]
          (when (= 19690720 (gravity-assist mem noun verb))
            (+ (* 100 noun) verb)))
        (for [noun (range 100) verb (range 100)] [noun verb])))

(deftest test-intcode
  (are [initial final] (= final (take (count initial) (:mem (i/run initial))))
    [1 0 0 0 99] [2 0 0 0 99]
    [2 3 0 3 99] [2 3 0 6 99]
    [2 4 4 5 99 0] [2 4 4 5 99 9801]
    [1 1 1 4 99 5 6 0 99] [30 1 1 4 2 5 6 0 99]))
