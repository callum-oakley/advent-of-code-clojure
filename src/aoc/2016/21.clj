(ns aoc.2016.21
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[_ _ a _ b c d] (str/split s #" ")]
    (condp #(str/starts-with? %2 %1) s
      "swap position" ['swap-position (read-string a) (read-string c)]
      "swap letter" ['swap-letter (first a) (first c)]
      "rotate left" ['rotate-left (read-string a)]
      "rotate right" ['rotate-right (read-string a)]
      "rotate based" ['rotate-based (first d)]
      "reverse positions" ['reverse-positions (read-string a) (read-string b)]
      "move position" ['move-position (read-string a) (read-string c)])))

(defn remove-at [p i]
  (str (subs p 0 i) (subs p (inc i))))

(defn insert-at [p i c]
  (str (subs p 0 i) c (subs p i)))

(defn swap-position [p i j]
  (-> p
      (remove-at i) (insert-at i (get p j))
      (remove-at j) (insert-at j (get p i))))

(defn rotate-left [p n]
  (str (subs p (mod n (count p))) (subs p 0 (mod n (count p)))))

(defn rotate-based [p c]
  (let [i (str/index-of p c)]
    (rotate-left p (- (+ i (if (>= i 4) 2 1))))))

(defn rotations [p]
  (->> p count range (map #(rotate-left p %))))

(defn part-* [password instructions]
  (reduce
   (fn [p [op x y]]
     (case op
       swap-position (swap-position p x y)
       swap-letter (swap-position p (str/index-of p x) (str/index-of p y))
       rotate-left (rotate-left p x)
       rotate-right (rotate-left p (- x))
       rotate-based (rotate-based p x)
       invert-rotate-based (->> p rotations
                                (filter #(= p (rotate-based % x))) first)
       reverse-positions (str (subs p 0 x)
                              (str/reverse (subs p x (inc y)))
                              (subs p (inc y)))
       move-position (-> p (remove-at x) (insert-at y (get p x)))))
   password
   instructions))

(defn part-1 []
  (part-* "abcdefgh" (map parse (str/split-lines (slurp "input/2016/21")))))

(defn part-2 []
  (->> "input/2016/21" slurp str/split-lines (map parse)
       (map (fn [[op x y :as instruction]]
              (case op
                rotate-left ['rotate-right x]
                rotate-right ['rotate-left x]
                rotate-based ['invert-rotate-based x]
                move-position ['move-position y x]
                instruction)))
       reverse
       (part-* "fbgdceah")))

(deftest test-part-*
  (is (= "decab" (part-* "abcde"
                         (map parse
                              ["swap position 4 with position 0"
                               "swap letter d with letter b"
                               "reverse positions 0 through 4"
                               "rotate left 1 step"
                               "move position 1 to position 4"
                               "move position 3 to position 0"
                               "rotate based on position of letter b"
                               "rotate based on position of letter d"])))))

(deftest test-answers
  (is (= "baecdfgh" (part-1)))
  (is (= "cegdahbf" (part-2))))
