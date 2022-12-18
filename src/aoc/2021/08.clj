(ns aoc.2021.08
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"[a-g]+") (map set)
       (partition 14) (map #(partition-all 10 %))))

;; Across any 10 calibration signals, the same segments will always appear the
;; same number of times. For example the top segment will always appear 8
;; times, while the bottom segment will always appear 7 times. It turns out
;; that if we replace the wire labels with these frequencies of occurance on
;; the standard wiring, each digit corresponds to a unique multiset of
;; frequencies (freqs->digit below).
;;
;; Applying this same process to any given set of 10 calibration signals and
;; comparing to the standard allows us to deduce how they are wired.

(def standard-wiring
  {"abcefg" 0 "cf" 1 "acdeg" 2 "acdfg" 3 "bcdf" 4
   "adbfg" 5 "abdefg" 6 "acf" 7 "abcdefg" 8 "abcdfg" 9})

(def freqs->digit
  (let [f (->> standard-wiring keys (apply concat) frequencies)]
    (update-keys standard-wiring #(sort (map f %)))))

(defn wiring [signals]
  (let [f (->> signals (apply concat) frequencies)]
    (->> signals
         (map (fn [signal] [signal (freqs->digit (sort (map f signal)))]))
         (into {}))))

(defn output [[calibration-signals out-signals]]
  (apply str (map (wiring calibration-signals) out-signals)))

(defn part-1 [entries]
  (->> entries (map #(seq (output %))) flatten (filter (set "1478")) count))

(defn part-2 [entries]
  (->> entries (map #(parse-long (output %))) (apply +)))

(def sample
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
   fdgacbe cefdb cefbgd gcbe
   edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
   fcgedb cgb dgebacf gc
   fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
   cg cg fdcagb cbg
   fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
   efabcd cedba gadfec cb
   aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
   gecf egdcabf bgf bfgea
   fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
   gebdcfa ecba ca fadegcb
   dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
   cefg dcbef fcge gbcadfe
   bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
   ed bcgafe cdgba cbgef
   egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
   gbdfcae bgc cg cgb
   gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
   fgae cfgab fg bagce")

(deftest test-example
  (is (= 26 (part-1 (parse sample))))
  (is (= 61229 (part-2 (parse sample)))))
