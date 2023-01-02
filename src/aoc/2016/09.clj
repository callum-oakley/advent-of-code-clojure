(ns aoc.2016.09
  (:require
   [clojure.test :refer [deftest is]]))

(defn decompress [s]
  (if-let [[_ prefix len mul suffix]
           (re-matches #"([A-Z]*)\((\d+)x(\d+)\)(.*)" s)]
    (let [len (read-string len)
          mul (read-string mul)]
      (str prefix
           (apply str (repeat mul (subs suffix 0 len)))
           (decompress (subs suffix len))))
    s))

(defn part-1 [s]
  (-> s decompress count))

(defn part-2 [s]
  (if-let [[_ prefix len mul suffix]
           (re-matches #"([A-Z]*)\((\d+)x(\d+)\)(.*)" s)]
    (let [len (read-string len)
          mul (read-string mul)]
      (+ (count prefix)
         (* mul (part-2 (subs suffix 0 len)))
         (part-2 (subs suffix len))))
    (count s)))

(deftest test-decompress
  (is (= "ADVENT" (decompress "ADVENT")))
  (is (= "ABBBBBC" (decompress "A(1x5)BC")))
  (is (= "XYZXYZXYZ" (decompress "(3x3)XYZ")))
  (is (= "ABCBCDEFEFG" (decompress "A(2x2)BCD(2x2)EFG")))
  (is (= "(1x3)A" (decompress "(6x1)(1x3)A")))
  (is (= "X(3x3)ABC(3x3)ABCY" (decompress "X(8x2)(3x3)ABCY"))))

(deftest test-part-2
  (is (= (count "XYZXYZXYZ") (part-2 "(3x3)XYZ")))
  (is (= (count "XABCABCABCABCABCABCY") (part-2 "X(8x2)(3x3)ABCY")))
  (is (= 241920 (part-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A")))
  (is (= 445 (part-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"))))
