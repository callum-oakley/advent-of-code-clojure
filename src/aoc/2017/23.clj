(ns aoc.2017.23
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(defn parse [s]
  (->> s str/split-lines (mapv #(map read-string (re-seq #"\S+" %)))))

(defn run [mem head reg muls]
  (let [read #(if (int? %) % (get reg % 0))]
    (if-let [[op x y] (get mem head)]
      (case op
        set (recur mem (inc head) (assoc reg x (read y)) muls)
        sub (recur mem (inc head) (assoc reg x (- (read x) (read y))) muls)
        mul (recur mem (inc head) (assoc reg x (* (read x) (read y))) (inc muls))
        jnz (recur mem (+ head (if (zero? (read x)) 1 (read y))) reg muls))
      muls)))

(defn prime? [n]
  (every? (fn [d] (pos? (mod n d))) (range 2 (inc (math/sqrt n)))))

(defn part-1 [mem]
  (run mem 0 {} 0))

;; By following the code and checking with some print statements: the program
;; is counting up in 17s from 105700 to 122700 (inclusive), and incrementing h
;; each time it finds a composite number.
(defn part-2 [_]
  (->> (range 105700 122701 17) (remove prime?) count))
