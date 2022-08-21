(ns aoc.2018.12
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [patterns (re-seq #"[.#]+" s)]
    [(first patterns)
     (into {} (map vec (partition 2 (rest patterns))))]))

(defn part-* [[initial rules]]
  (map-indexed
   (fn [gen pots]
     (apply + (keep-indexed (fn [i pot] (if (= \# pot) (- i (* 2 gen)))) pots)))
   (iterate (fn [pots]
              (apply str (map #(get rules (apply str %) ".")
                              (partition 5 1 (str "...." pots "....")))))
            initial)))

(defn part-1 []
  (->> "input/2018/12" slurp parse part-* (drop 20) first))

;; Printing successive values, it looks like the score approaches a constant
;; factor of the generation, so we only need to run the simulation long enough
;; for this factor to stabilise.
(defn part-2 []
  (loop [gen 1
         prev-factor nil
         scores (->> "input/2018/12" slurp parse part-* rest)]
    (let [factor (/ (first scores) gen)]
      (if (= factor prev-factor)
        (* 50000000000 factor)
        (recur (inc gen) factor (rest scores))))))

(def example
  "initial state: #..#.#..##......###...###
   ...## => # ..#.. => # .#... => # .#.#. => # .#.## => #
   .##.. => # .#### => # #.#.# => # #.### => # ##.#. => #
   ##.## => # ###.. => # ###.# => # ####. => #")

(deftest test-example
  (is (= 325 (first (drop 20 (part-* (parse example)))))))
