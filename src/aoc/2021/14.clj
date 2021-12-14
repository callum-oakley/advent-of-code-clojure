(ns aoc.2021.14
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  [(->> s (re-find #"\w+") frequencies)
   (->> s (re-find #"\w+") (partition 2 1) frequencies)
   (->> s (re-seq #"(\w\w) -> (\w)")
        (map #(let [[_ [a b] [c]] %] [[a b] c]))
        (into {}))])

(defn step [reactions [elements pairs]]
  (reduce-kv (fn [[elements pairs] [a b] n]
               (let [c (reactions [a b])]
                 [(merge-with + elements {c n})
                  (merge-with + pairs {[a c] n [c b] n})]))
             [elements {}]
             pairs))

(defn grow [n [elements pairs reactions]]
  (first (nth (iterate #(step reactions %) [elements pairs]) n)))

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
