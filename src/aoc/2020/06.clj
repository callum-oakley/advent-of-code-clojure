(ns aoc.2020.06
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(def data
  (str/split (slurp "input/2020/06") #"\n\n"))

(defn process-group [combine group]
  (->> (str/split-lines group)
       (map set)
       (apply combine)
       count))

(defn part-1
  ([] (part-1 data))
  ([groups] (apply + (map #(process-group set/union %) groups))))

(defn part-2
  ([] (part-2 data))
  ([groups]
   (apply + (map #(process-group set/intersection %) groups))))

(def sample
  ["abc" "a\nb\nc" "ab\nac" "a\na\na\na" "b"])

(deftest test-part-1
  (is (= (part-1 sample) 11))
  (is (= (part-1) 6457)))

(deftest test-part-2
  (is (= (part-2 sample) 6))
  (is (= (part-2) 3260)))
