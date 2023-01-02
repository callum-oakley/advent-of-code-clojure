(ns aoc.2016.06
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def parse str/split-lines)

(defn part-* [opt messages]
  (->> (transpose messages)
       (map #(key (apply opt val (frequencies %))))
       (apply str)))

(defn part-1 [messages]
  (part-* max-key messages))

(defn part-2 [messages]
  (part-* min-key messages))

(deftest test-part-*
  (is (= "easter" (part-* max-key ["eedadn" "drvtee" "eandsr" "raavrd"
                                   "atevrs" "tsrnev" "sdttsa" "rasrtv"
                                   "nssdts" "ntnada" "svetve" "tesnvt"
                                   "vntsnd" "vrdear" "dvrsen" "enarar"]))))
