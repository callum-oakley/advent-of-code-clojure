(ns aoc.2020.19
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (map str/split-lines (str/split (slurp "input/2020/19") #"\n\n")))

(defn parse-rule [rule]
  (let [[left right] (str/split rule #": ")]
    [(read-string left) (map read-string (str/split right #" "))]))

(defn parse [[rules messages]]
  [(into {} (map parse-rule rules)) messages])

(defn build-re [rules max-depth n]
  (if (pos? max-depth)
    (str/join
     (map
      #(if (int? %)
         (str "(" (build-re rules (dec max-depth) %) ")")
         %)
      (rules n)))
    "x"))

(defn part-1
  ([] (part-1 (parse data)))
  ([[rules messages]]
   (let [re (re-pattern (build-re rules 16 0))]
     (count (filter #(re-matches re %) messages)))))

(defn part-2
  ([] (part-2 (parse data)))
  ([[rules messages]]
   (let [rules* (assoc rules 8 '(42 | 42 8) 11 '(42 31 | 42 11 31))
         re (re-pattern (build-re rules* 16 0))]
     (count (filter #(re-matches re %) messages)))))

(def sample-1
  [["0: 4 1 5"         "1: 2 3 | 3 2"     "2: 4 4 | 5 5"
    "3: 4 5 | 5 4"     "4: \"a\""         "5: \"b\""]
   ["ababbb" "bababa" "abbbab" "aaabbb" "aaaabbb"]])

(def sample-2
  [["42: 9 14 | 10 1"  "9: 14 27 | 1 26"  "10: 23 14 | 28 1" "1: \"a\""
    "11: 42 31"        "5: 1 14 | 15 1"   "19: 14 1 | 14 14" "12: 24 14 | 19 1"
    "16: 15 1 | 14 14" "31: 14 17 | 1 13" "6: 14 14 | 1 14"  "2: 1 24 | 14 4"
    "0: 8 11"          "13: 14 3 | 1 12"  "15: 1 | 14"       "17: 14 2 | 1 7"
    "23: 25 1 | 22 14" "28: 16 1"         "4: 1 1"           "20: 14 14 | 1 15"
    "3: 5 14 | 16 1"   "27: 1 6 | 14 18"  "14: \"b\""        "21: 14 1 | 1 14"
    "25: 1 1 | 1 14"   "22: 14 14"        "8: 42"            "26: 14 22 | 1 20"
    "18: 15 15"        "7: 14 5 | 1 21"   "24: 14 1"]
   ["abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
    "bbabbbbaabaabba"
    "babbbbaabbbbbabbbbbbaabaaabaaa"
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
    "bbbbbbbaaaabbbbaaabbabaaa"
    "bbbababbbbaaaaaaaabbababaaababaabab"
    "ababaaaaaabaaab"
    "ababaaaaabbbaba"
    "baabbaaaabbaaaababbaababb"
    "abbbbabbbbaaaababbbbbbaaaababb"
    "aaaaabbaabaaaaababaa"
    "aaaabbaaaabbaaa"
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
    "babaaabbbaaabaababbaabababaaab"
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]]
  )

(deftest test-part-1
  (is (= (part-1 (parse sample-1)) 2))
  (is (= (part-1) 118)))

(deftest test-part-2
  (is (= (part-1 (parse sample-2)) 3))
  (is (= (part-2 (parse sample-2)) 12))
  (is (= (part-2) 246)))
