(ns aoc.2022.25 
  (:require
   [clojure.test :refer [deftest is are]]))

(defn snafu->int [s]
  (apply + (map (fn [c b] (* b (case c \2 2 \1 1 \0 0 \- -1 \= -2)))
                (reverse s) (iterate #(* % 5) 1))))

(defn int->snafu [i]
  (loop [i i snafu ""]
    (if (zero? i)
      snafu
      (case (mod i 5)
        0 (recur (quot i 5) (str \0 snafu))
        1 (recur (quot i 5) (str \1 snafu))
        2 (recur (quot i 5) (str \2 snafu))
        3 (recur (inc (quot i 5)) (str \= snafu))
        4 (recur (inc (quot i 5)) (str \- snafu))))))

(defn part-1 [s]
  (int->snafu (apply + (map snafu->int (re-seq #"\S+" s)))))

(def example
  "1=-0-2 12111 2=0= 21 2=01 111 20012 112 1=-1= 1-12 12 1= 122")

(deftest test-example
  (are [i snafu] (and (= i (snafu->int snafu)) (= (int->snafu i) snafu))
    1         "1"      2         "2"       3         "1="
    4         "1-"     5         "10"      6         "11"
    7         "12"     8         "2="      9         "2-"
    10        "20"     15        "1=0"     20        "1-0"
    2022      "1=11-2" 12345     "1-0---0" 314159265 "1121-1110-1=0")
  (is (= "2=-1=0" (part-1 example))))
