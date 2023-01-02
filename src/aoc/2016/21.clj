(ns aoc.2016.21
  (:require
   [aoc.string :as aocstr]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map (fn [line]
         (let [[_ _ a _ b c d] (str/split line #" ")]
           (condp #(str/starts-with? %2 %1) line
             "swap position" ['swap-position (read-string a) (read-string c)]
             "swap letter" ['swap-letter (first a) (first c)]
             "rotate left" ['rotate-left (read-string a)]
             "rotate right" ['rotate-right (read-string a)]
             "rotate based" ['rotate-based (first d)]
             "reverse" ['reverse-positions (read-string a) (read-string b)]
             "move" ['move-position (read-string a) (read-string c)])))
       (map str/trim (str/split-lines s))))

(defn rotate-based [p c]
  (let [i (str/index-of p c)]
    (aocstr/rotate-left p (- (+ i (if (>= i 4) 2 1))))))

(defn rotations [p]
  (->> p count range (map #(aocstr/rotate-left p %))))

(defn part-* [password instructions]
  (reduce
   (fn [p [op x y]]
     (case op
       swap-position (aocstr/swap p x y)
       swap-letter (aocstr/swap p (str/index-of p x) (str/index-of p y))
       rotate-left (aocstr/rotate-left p x)
       rotate-right (aocstr/rotate-right p x)
       rotate-based (rotate-based p x)
       invert-rotate-based (->> p rotations
                                (filter #(= p (rotate-based % x))) first)
       reverse-positions (str (subs p 0 x)
                              (str/reverse (subs p x (inc y)))
                              (subs p (inc y)))
       move-position (-> p
                         (aocstr/remove-at x)
                         (aocstr/insert-at y (get p x)))))
   password
   instructions))

(defn part-1 [instructions]
  (part-* "abcdefgh" instructions))

(defn part-2 [instructions]
  (->> instructions
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
                         (parse "swap position 4 with position 0
                                 swap letter d with letter b
                                 reverse positions 0 through 4
                                 rotate left 1 step
                                 move position 1 to position 4
                                 move position 3 to position 0
                                 rotate based on position of letter b
                                 rotate based on position of letter d")))))
