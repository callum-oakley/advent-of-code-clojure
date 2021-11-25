(ns aoc.2015.08
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-1* [s]
  (+ 2 (first (reduce (fn [[res escape?] c]
                        (if escape?
                          (if (= c \x)
                            [(+ 3 res) false]
                            [(inc res) false])
                          (if (= c \\)
                            [res true]
                            [res escape?])))
                      [0 false]
                      s))))

(defn part-2* [s]
  (apply + 2 (map #(case % \" 1 \\ 1 0) s)))

(defn part-1 []
  (->> (slurp "input/2015/08")
       str/split-lines
       (map part-1*)
       (apply +)))

(defn part-2 []
  (->> (slurp "input/2015/08")
       str/split-lines
       (map part-2*)
       (apply +)))

(deftest test-part-1*
  (is (= 2 (part-1* "\"\"")))
  (is (= 2 (part-1* "\"abc\"")))
  (is (= 3 (part-1* "\"aaa\\\"aaa\"")))
  (is (= 5 (part-1* "\"\\x27\""))))

(deftest test-part-2*
  (is (= 4 (part-2* "\"\"")))
  (is (= 4 (part-2* "\"abc\"")))
  (is (= 6 (part-2* "\"aaa\\\"aaa\"")))
  (is (= 5 (part-2* "\"\\x27\""))))
