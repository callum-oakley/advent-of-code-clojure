(ns aoc.2020.25
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (map read-string (str/split-lines (slurp "input/2020/25"))))

(defn steps [subject]
  (iterate #(mod (* subject %) 20201227) 1))

(defn loops [key]
  (some (fn [[n val]] (when (= val key) n)) (map-indexed vector (steps 7))))

(defn part-1
  ([] (part-1 data))
  ([[card door]] (first (drop (loops door) (steps card)))))

(deftest test-part-1
  (is (= (part-1 [5764801 17807724]) 14897079))
  (is (= (part-1) 7032853)))
