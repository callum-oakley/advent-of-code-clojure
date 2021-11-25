(ns aoc.2017.10
  (:require
   [aoc.hash :as hash]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn rev [n knot i length]
  (reduce (fn [k j]
            (assoc k (mod (+ i j) n) (knot (mod (- (+ i length) j 1) n))))
          knot
          (range length)))

(defn sparse-hash [n lengths]
  (first (reduce (fn [[knot i skip] length]
                   [(rev n knot i length) (+ i length skip) (inc skip)])
                 [(vec (range n)) 0 0]
                 lengths)))

(defn knot-hash [s]
  (->> (concat (.getBytes s) [17 31 73 47 23]) (repeat 64) (apply concat)
       (sparse-hash 256) (partition 16) (map #(apply bit-xor %))))

(defn part-1 []
  (->> "input/2017/10" slurp (re-seq #"\d+") (map read-string) (sparse-hash 256)
       (take 2) (apply *)))

(defn part-2 []
  (->> "input/2017/10" slurp str/trim knot-hash hash/hex))

(deftest test-examples
  (is (= [3 4 2 1 0] (sparse-hash 5 [3 4 1 5])))
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (hash/hex (knot-hash ""))))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (hash/hex (knot-hash "AoC 2017"))))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (hash/hex (knot-hash "1,2,3"))))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (hash/hex (knot-hash "1,2,4")))))
