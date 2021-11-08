(ns aoc.2020.09
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (mapv read-string (str/split-lines (slurp "input/2020/09"))))

(defn part-1
  ([] (part-1 25 data))
  ([preamble-len numbers]
   (when (> (count numbers) preamble-len)
     (let [preamble (take preamble-len numbers)
           valid (set (map #(apply + %) (comb/combinations preamble 2)))
           target (nth numbers preamble-len)]
       (if (valid target)
         (recur preamble-len (rest numbers))
         target)))))

(defn part-2
  ([] (part-2 25 data))
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

(deftest test-part-1
  (is (= (part-1 5 sample) 127))
  (is (= (part-1) 3199139634)))

(deftest test-part-2
  (is (= (part-2 5 sample) 62))
  (is (= (part-2) 438559930)))
