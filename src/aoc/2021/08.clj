(ns aoc.2021.08
  (:require
   [aoc.map-updates :refer [update-keys]]
   [clojure.math.combinatorics :as comb]
   [clojure.test :refer [deftest is]]))

(def signal->digit
  (update-keys
   {"abcefg" 0 "cf" 1 "acdeg" 2 "acdfg" 3 "bcdf" 4
    "adbfg" 5 "abdefg" 6 "acf" 7 "abcdefg" 8 "abcdfg" 9}
   set))

(def permutations
  (->> "abcdefg" comb/permutations (map #(zipmap "abcdefg" %))))

(defn parse [s]
  (->> s (re-seq #"[a-g]+") (map set) (partition 14)))

(defn decode [signal permutation]
  (set (map permutation signal)))

(defn valid? [permutation signals]
  (every? #(signal->digit (decode % permutation)) signals))

(defn output [signals]
  (let [p (->> permutations (filter #(valid? % signals)) first)]
    (apply str (map #(signal->digit (decode % p)) (drop 10 signals)))))

(defn part-1* [entries]
  (->> entries (map #(seq (output %))) flatten (filter (set "1478")) count))

(defn part-2* [entries]
  (->> entries (map #(Integer/parseInt (output %))) (apply +)))

(defn part-1 []
  (->> "input/2021/08" slurp parse part-1*))

(defn part-2 []
  (->> "input/2021/08" slurp parse part-2*))

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
  (is (= 26 (part-1* (parse sample))))
  (is (= 61229 (part-2* (parse sample)))))
