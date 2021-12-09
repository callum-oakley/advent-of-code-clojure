(ns aoc.2016.06
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-* [opt messages]
  (->> (transpose messages)
       (map #(key (apply opt val (frequencies %))))
       (apply str)))

(defn part-1 []
  (->> "input/2016/06" slurp str/split-lines (part-* max-key)))

(defn part-2 []
  (->> "input/2016/06" slurp str/split-lines (part-* min-key)))

(deftest test-part-*
  (is (= "easter" (part-* max-key ["eedadn" "drvtee" "eandsr" "raavrd"
                                   "atevrs" "tsrnev" "sdttsa" "rasrtv"
                                   "nssdts" "ntnada" "svetve" "tesnvt"
                                   "vntsnd" "vrdear" "dvrsen" "enarar"]))))
