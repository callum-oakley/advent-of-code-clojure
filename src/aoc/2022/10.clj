(ns aoc.2022.10 
  (:require
   [aoc.ocr :as ocr]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map #(map read-string (re-seq #"\S+" %)) (str/split-lines s)))

(defn cpu [instructions] 
  (reduce (fn [signal [op a]]
            (case op
              noop (conj signal (peek signal))
              addx (conj signal (peek signal) (+ (peek signal) a))))
          [1]
          instructions))

(defn crt [signal]
  (reduce-kv (fn [screen cycle sprite]
               (let [x (mod cycle 40) y (quot cycle 40)]
                 (cond-> screen
                   (<= (dec sprite) x (inc sprite)) (conj [x y]))))
             #{}
             signal))

(defn part-1* [instructions]
  (let [signal (cpu instructions)]
    (apply + (map #(* (signal (dec %)) %) (range 20 260 40)))))

(defn part-2* [instructions]
  (ocr/draw (crt (cpu instructions))))

(defn part-1 []
  (->> "input/2022/10" slurp parse part-1*))

(defn part-2 []
  (->> "input/2022/10" slurp parse part-2* ocr/parse))

(def example
  "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4
   noop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5
   addx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop
   noop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8
   addx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7
   noop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop
   noop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33
   noop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2
   addx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop
   addx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26
   addx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9
   addx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1
   addx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6
   addx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1
   addx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop")

(deftest test-example
  (is (= 13140 (part-1* (parse example))))
  (is (= (str/join "\n" ["##..##..##..##..##..##..##..##..##..##.."
                         "###...###...###...###...###...###...###."
                         "####....####....####....####....####...."
                         "#####.....#####.....#####.....#####....."
                         "######......######......######......####"
                         "#######.......#######.......#######....."])
         (part-2* (parse example)))))