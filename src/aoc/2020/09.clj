(ns aoc.2020.09
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (mapv read-string (str/split-lines s)))

(defn part-1
  ([numbers] (part-1 25 numbers))
  ([preamble-len numbers]
   (when (> (count numbers) preamble-len)
     (let [preamble (take preamble-len numbers)
           valid (set (map #(apply + %) (comb/combinations preamble 2)))
           target (nth numbers preamble-len)]
       (if (valid target)
         (recur preamble-len (rest numbers))
         target)))))

(defn part-2
  ([numbers] (part-2 25 numbers))
  ([preamble-len numbers]
   (let [invalid (part-1 preamble-len numbers)]
     (loop [i 0 j 1]
       (let [subset (map numbers (range i j))
             sum (apply + subset)]
         (cond
           (= sum invalid) (+ (apply min subset) (apply max subset))
           (< sum invalid) (recur i (inc j))
           (> sum invalid) (recur (inc i) j)))))))

(def sample
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(deftest test-examples
  (is (= (part-1 5 sample) 127))
  (is (= (part-2 5 sample) 62)))
