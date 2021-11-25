(ns aoc.2020.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (map read-string (str/split-lines (slurp "input/2020/10"))))

(defn part-1
  ([] (part-1 data))
  ([adapters]
   (let [device (+ (apply max adapters) 3)
         diffs (->> (conj adapters 0 device)
                    sort
                    (partition 2 1)
                    (map (fn [[a b]] (- b a)))
                    frequencies)]
     (* (get diffs 1 0) (get diffs 3 0)))))

(defn part-2
  ([] (part-2 data))
  ([adapters]
   (let [device (+ (apply max adapters) 3)
         ;; The number of routes to each adapter is the sum of the number of
         ;; routes to each of the possible previous adapters (it's like a messy
         ;; Pascal's triangle) so we can get the solution in one pass.
         routes (reduce
                 (fn [r a]
                   (assoc r a
                          (apply + (map #(get r % 0) (range (- a 3) a)))))
                 {0 1}
                 (sort (conj adapters device)))]
     (get routes device))))

(deftest test-examples
  (is (= (part-1 [16 10 15 5 1 11 7 19 6 12 4]) 35))
  (is (= (part-1 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19
                  38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3]) 220))
  (is (= (part-2 [16 10 15 5 1 11 7 19 6 12 4]) 8))
  (is (= (part-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19
                  38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3]) 19208)))
