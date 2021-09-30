(ns aoc.2015.05
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn nice-1? [s]
  (and (<= 3 (->> s (filter (set "aeiou")) count))
       (->> s (partition 2 1) (some (fn [[a b]] (= a b))))
       (not (re-find #"ab|cd|pq|xy" s))))

(defn non-overlapping-pair? [s]
  (and (<= 4 (count s))
       (or (str/includes? (subs s 2) (subs s 0 2))
           (recur (subs s 1)))))

(defn nice-2? [s]
  (and (non-overlapping-pair? s)
       (->> s (partition 3 1) (some (fn [[a _ b]] (= a b))))))

(defn part-1 []
  (->> (slurp "input/2015/05") str/split-lines (filter nice-1?) count))

(defn part-2 []
  (->> (slurp "input/2015/05") str/split-lines (filter nice-2?) count))

(deftest test-nice-1?
  (is (nice-1? "ugknbfddgicrmopn"))
  (is (nice-1? "aaa"))
  (is (not (nice-1? "jchzalrnumimnmhp")))
  (is (not (nice-1? "haegwjzuvuyypxyu")))
  (is (not (nice-1? "dvszwmarrgswjxmb"))))

(deftest test-nice-2?
  (is (nice-2? "qjhvhtzxzqqjkmpb"))
  (is (nice-2? "xxyxx"))
  (is (not (nice-2? "uurcxstgmygtbstg")))
  (is (not (nice-2? "ieodomkazucvgmuy"))))

(deftest test-answers
  (is (= 236 (part-1)))
  (is (= 51 (part-2))))
