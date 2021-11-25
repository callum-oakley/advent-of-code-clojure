(ns aoc.2020.07
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "input/2020/07")))

(defn parse-rule [rule]
  (let [[_ outer] (re-find #"(\w+ \w+) bags contain" rule)]
    {outer (->> (re-seq #"(\d+) (\w+ \w+) bag" rule)
                (map (fn [[_ n c]] {c (read-string n)}))
                (into {}))}))

(defn parse [rules]
  (into {} (map parse-rule rules)))

(defn eventually-contains? [rules current target]
  (apply (some-fn #{target} #(eventually-contains? rules % target))
         (keys (rules current))))

(defn count-bags [rules bag]
  (apply + (map (fn [[b n]] (* n (inc (count-bags rules b)))) (rules bag))))

(defn part-1
  ([] (part-1 (parse data)))
  ([rules]
   (count (filter #(eventually-contains? rules % "shiny gold") (keys rules)))))

(defn part-2
  ([] (part-2 (parse data)))
  ([rules] (count-bags rules "shiny gold")))

(def sample
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(def sample-2
  ["shiny gold bags contain 2 dark red bags."
   "dark red bags contain 2 dark orange bags."
   "dark orange bags contain 2 dark yellow bags."
   "dark yellow bags contain 2 dark green bags."
   "dark green bags contain 2 dark blue bags."
   "dark blue bags contain 2 dark violet bags."
   "dark violet bags contain no other bags."])

(deftest test-examples
  (is (= (part-1 (parse sample)) 4))
  (is (= (part-2 (parse sample)) 32))
  (is (= (part-2 (parse sample-2)) 126)))
