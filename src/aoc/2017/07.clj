(ns aoc.2017.07
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce (fn [[weight children] line]
            (let [[_ disc w c] (re-find #"(\w+) \((\d+)\)(?: -> (.+))?" line)]
              [(assoc weight disc (read-string w))
               (assoc children disc (when c (re-seq #"\w+" c)))]))
          [{} {}]
          (str/split-lines s)))

(defn part-1 [[_ children]]
  (->> children keys (remove (set (apply concat (vals children)))) first))

(defn part-2 [[weight children]]
  (let [weight* (fn weight* [disc]
                  (apply + (weight disc) (map weight* (children disc))))
        balanced? (fn [disc] (apply = (map weight* (children disc))))]
    (loop [disc (part-1 [weight children])]
      (if-let [unbalanced (first (remove balanced? (children disc)))]
        (recur unbalanced)
        (let [bad (->> disc children (group-by weight*) vals
                       (filter #(= 1 (count %))) first first)
              good (->> disc children (remove #{bad}) first)]
          (+ (weight bad) (- (weight* good) (weight* bad))))))))

(deftest test-examples
  (let [[weight children] (parse "pbga (66)
                                  xhth (57)
                                  ebii (61)
                                  havc (66)
                                  ktlj (57)
                                  fwft (72) -> ktlj, cntj, xhth
                                  qoyq (66)
                                  padx (45) -> pbga, havc, qoyq
                                  tknk (41) -> ugml, padx, fwft
                                  jptl (61)
                                  ugml (68) -> gyxo, ebii, jptl
                                  gyxo (61)
                                  cntj (57)")]
    (is (= "tknk" (part-1 [weight children])))
    (is (= 60 (part-2 [weight children])))))
