(ns aoc.2018.01)

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map read-string)))

(defn part-1 [input]
  (reduce + input))

(defn part-2 [input]
  (loop [fs (reductions + (cycle input)) seen #{}]
    (if (seen (first fs))
      (first fs)
      (recur (rest fs) (conj seen (first fs))))))
