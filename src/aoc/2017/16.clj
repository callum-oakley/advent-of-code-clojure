(ns aoc.2017.16
  (:require
   [aoc.string :as aocstr]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map #(case (get % 0)
          \s ['spin [(read-string (subs % 1))]]
          \x ['exchange (map read-string (re-seq #"\d+" (subs % 1)))]
          \p ['partner [(get % 1) (get % 3)]])
       (str/split s #",")))

(defn part-1* [s moves]
  (reduce (fn [s [move [x y]]]
            (case move
              spin (aocstr/rotate-right s x)
              exchange (aocstr/swap s x y)
              partner (aocstr/swap s (str/index-of s x) (str/index-of s y))))
          s
          moves))

(defn part-1 [moves]
  (part-1* "abcdefghijklmnop" moves))

(defn repeating-cycle [f initial]
  (loop [x initial
         cycle [x]]
    (let [x (f x)]
      (if (= initial x)
        cycle
        (recur x (conj cycle x))))))

(defn part-2 [moves]
  (let [cycle (repeating-cycle #(part-1* % moves) "abcdefghijklmnop")]
    (cycle (mod 1000000000 (count cycle)))))

(deftest test-example
  (is (= "baedc" (part-1* "abcde" (parse "s1,x3/4,pe/b")))))
