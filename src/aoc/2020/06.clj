(ns aoc.2020.06
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (str/split s #"\n\n"))

(defn process-group [combine group]
  (->> (str/split-lines group)
       (map set)
       (apply combine)
       count))

(defn part-1 [groups]
  (apply + (map #(process-group set/union %) groups)))

(defn part-2 [groups]
  (apply + (map #(process-group set/intersection %) groups)))

(def sample
  ["abc" "a\nb\nc" "ab\nac" "a\na\na\na" "b"])

(deftest test-part-1
  (is (= (part-1 sample) 11))
  (is (= (part-2 sample) 6)))
