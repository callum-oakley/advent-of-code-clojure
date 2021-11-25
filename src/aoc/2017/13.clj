(ns aoc.2017.13
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines (map #(map read-string (re-seq #"\d+" %)))))

(defn caught [delay scanners]
  (filter (fn [[depth range]]
            (zero? (mod (+ delay depth) (* 2 (dec range)))))
          scanners))

(defn part-1 []
  (->> "input/2017/13" slurp parse (caught 0) (map #(apply * %)) (apply +)))

(defn part-2* [scanners]
  ;; Testing scanners in order of increasing range saves us some time
  (let [scanners (sort-by second scanners)]
    (first (remove #(seq (caught % scanners)) (range)))))

(defn part-2 []
  (->> "input/2017/13" slurp parse part-2*))

(deftest test-example
  (is (= [[0 3] [6 4]] (caught 0 [[0 3] [1 2] [4 4] [6 4]])))
  (is (= 10 (part-2* [[0 3] [1 2] [4 4] [6 4]]))))
