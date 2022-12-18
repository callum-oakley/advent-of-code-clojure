(ns aoc.2021.24
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (map #(map read-string (str/split % #" ")) (str/split-lines s)))

(defn digits [n]
  (if (< n 10) [n] (conj (digits (quot n 10)) (mod n 10))))

(defn valid? [instructions input]
  (loop [instructions instructions
         reg '{w 0 x 0 y 0 z 0}
         input (digits input)]
    (if-let [[op a b] (first instructions)]
      (case op
        inp (recur (rest instructions) (assoc reg a (first input)) (rest input))
        add (recur (rest instructions) (update reg a + (get reg b b)) input)
        mul (recur (rest instructions) (update reg a * (get reg b b)) input)
        div (recur (rest instructions) (update reg a quot (get reg b b)) input)
        mod (recur (rest instructions) (update reg a mod (get reg b b)) input)
        eql (recur (rest instructions)
                   (assoc reg a (if (= (get reg a a) (get reg b b)) 1 0))
                   input))
      (zero? (reg 'z)))))

;; Rough scribbles that got me to the answer. Not very well explained. The more
;; useful insight is to think of z in base 26, and 1 operations push a new
;; digit on the right (the "condition" is impossible to satisfy), while 26
;; operations pop a digit off the right as long as the condition is satisfied
;; (and every one must be, since there are exactly 7 pushes and 7 pops).
(defn valid?* [input]
  (let [w (digits input)]
    (reduce-kv (fn [z i [a b c]]
                 (if (= (w i) (+ (mod z 26) b))
                   (quot z a)
                   (+ (* (quot z a) 26) (w i) c)))
               0
               ;; when a is 1 the if condition is always false since b is > 9,
               ;; so we mulitply z by 26 7 times, we need to divide by 26 7
               ;; times to make up for it, so all the conditions must be hit on
               ;; the a = 26 cases.
               ;;             maximizing        minimizing
               [[1 11 6]    ; 9 [x]             9 [x]
                [1 13 14]   ; 9 [x]             2 [x]
                [1 15 14]   ; 3 [x]             1 [x]
                [26 -8 10]  ; 3 + 14 - 8 = 9    1 + 14 - 8 = 7
                [1 13 9]    ; 4 [x]             1 [x]
                [1 15 12]   ; 8 [x]             1 [x]
                [26 -11 8]  ; 8 + 12 - 11 = 9   1 + 12 - 11 = 2
                [26 -4 13]  ; 4 + 9 - 4 = 9     1 + 9 - 4 = 6
                [26 -15 12] ; 9 + 14 - 15 = 8   2 + 14 - 15 = 1
                [1 14 6]    ; 9 [x]             3 [x]
                [1 14 9]    ; 1 [x]             1 [x]
                [26 -1 15]  ; 1 + 9 - 1 = 9     1 + 9 - 1 = 9
                [26 -8 4]   ; 9 + 6 - 8 = 7     3 + 6 - 8 = 1
                [26 -14 10] ; 9 + 6 - 14 = 1    9 + 6 - 14 = 1
                ])))

(defn part-1 [instructions]
  (let [n 99394899891971] (when (valid? instructions n) n)))

(defn part-2 [instructions]
  (let [n 92171126131911] (when (valid? instructions n) n)))
