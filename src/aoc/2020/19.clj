(ns aoc.2020.19
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-rule [rule]
  (let [[left right] (str/split rule #": ")]
    [(read-string left) (map read-string (str/split right #" "))]))

(defn parse [s]
  (let [[rules messages]
        (map #(map str/trim (str/split-lines %)) (str/split s #"\n\n"))]
    [(into {} (map parse-rule rules)) messages]))

(defn build-re [rules max-depth n]
  (if (pos? max-depth)
    (str/join
     (map
      #(if (int? %)
         (str "(" (build-re rules (dec max-depth) %) ")")
         %)
      (rules n)))
    "x"))

(defn part-1 [[rules messages]]
  (let [re (re-pattern (build-re rules 16 0))]
    (count (filter #(re-matches re %) messages))))

(defn part-2 [[rules messages]]
  (let [rules* (assoc rules 8 '(42 | 42 8) 11 '(42 31 | 42 11 31))
        re (re-pattern (build-re rules* 16 0))]
    (count (filter #(re-matches re %) messages))))

(def sample-1
  "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n
   ababbb\nbababa\nabbbab\naaabbb\naaaabbb")

(def sample-2
  "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31
   5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14
   31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12
   15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1
   20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14
   25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21
   24: 14 1\n
   abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba
   babbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa
   bbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab
   ababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb
   abbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa
   aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab
   aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(deftest test-examples
  (is (= (part-1 (parse sample-1)) 2))
  (is (= (part-1 (parse sample-2)) 3))
  (is (= (part-2 (parse sample-2)) 12)))
