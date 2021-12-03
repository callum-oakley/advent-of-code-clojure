(ns aoc.2021.03
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"[01]+") (map #(Integer/parseInt % 2))))

(defn most-common-bit [xs i]
  (let [f (->> xs (map #(bit-test % i)) frequencies)]
    (max-key #(get f % 0) false true)))

(defn part-1* [width xs]
  (->> (range width)
       (reduce (fn [[gamma epsilon] i]
                 (if (most-common-bit xs i)
                   [(bit-set gamma i) epsilon]
                   [gamma (bit-set epsilon i)]))
               [0 0])
       (apply *)))

(defn rating [width criteria xs]
  (loop [i (dec width) xs xs]
    (if (= 1 (count xs))
      (first xs)
      (let [target (criteria xs i)]
        (recur (dec i) (filter #(= target (bit-test % i)) xs))))))

(defn part-2* [width xs]
  (* (rating width most-common-bit xs)
     (rating width (complement most-common-bit) xs)))

(defn part-1 []
  (->> "input/2021/03" slurp parse (part-1* 12)))

(defn part-2 []
  (->> "input/2021/03" slurp parse (part-2* 12)))

(deftest test-example
  (let [sample (parse "00100 11110 10110 10111 10101 01111
                       00111 11100 10000 11001 00010 01010")]
    (is (= 198 (part-1* 5 sample)))
    (is (= 230 (part-2* 5 sample)))))
