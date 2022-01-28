(ns aoc.2019.02
  (:require
   [aoc.2019.intcode :as intcode]
   [clojure.test :refer [deftest are]]))

(defn gravity-assist [noun verb]
  ((intcode/run (assoc (intcode/load "input/2019/02") 1 noun 2 verb)) 0))

(defn part-1 []
  (gravity-assist 12 2))

(defn part-2 []
  (some (fn [[noun verb]]
          (when (= 19690720 (gravity-assist noun verb))
            (+ (* 100 noun) verb)))
        (for [noun (range 100) verb (range 100)] [noun verb])))

(deftest test-intcode
  (are [initial final] (= (intcode/parse final)
                          (intcode/run (intcode/parse initial)))
    "1,0,0,0,99" "2,0,0,0,99"
    "2,3,0,3,99" "2,3,0,6,99"
    "2,4,4,5,99,0" "2,4,4,5,99,9801"
    "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"))
