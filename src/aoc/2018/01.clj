(ns aoc.2018.01)

(defn input []
  (->> "input/2018/01" slurp (re-seq #"-?\d+") (map read-string)))

(defn part-1 []
  (reduce + (input)))

(defn part-2 []
  (loop [fs (reductions + (cycle (input))) seen #{}]
    (if (seen (first fs))
      (first fs)
      (recur (rest fs) (conj seen (first fs))))))
