(ns aoc.2021.14
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  [(first s)
   (->> (re-find #"\w+" s) (partition 2 1) frequencies)
   (->> (re-seq #"(\w\w) -> (\w)" s)
        (map #(let [[_ [a b] [c]] %] [[a b] [[a c] [c b]]]))
        (into {}))])

(defn step [reactions pairs]
  (->> pairs
       (map (fn [[pair n]] (when-let [[a b] (reactions pair)] {a n b n})))
       (apply merge-with +)))

(defn grow [n [first-element pairs reactions]]
  (let [elements (->> pairs (iterate #(step reactions %)) (drop n) first
                      (map (fn [[[_ element] n]] {element n}))
                      (apply merge-with +))]
    ;; Counting the second element of every pair accounts for every element
    ;; except the first one.
    (update elements first-element inc)))

(defn part-* [n data]
  (->> (grow n data) vals (apply (juxt max min)) (apply -)))

(defn part-1 []
  (->> "input/2021/14" slurp parse (part-* 10)))

(defn part-2 []
  (->> "input/2021/14" slurp parse (part-* 40)))

(deftest test-examples
  (let [data (parse "NNCB CH -> B HH -> N CB -> H NH -> C HB -> C HC -> B
                     HN -> C NN -> C BH -> H NC -> B NB -> B
                     BN -> B BB -> N BC -> B CC -> N CN -> C")]
    (is (= {\B 1749 \C 298 \H 161 \N 865} (grow 10 data)))
    (is (= 1588 (part-* 10 data)))
    (is (= 2188189693529 (part-* 40 data)))))
