(ns aoc.2019.22
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest are]]))

;; We can represent each step of the shuffle as a linear map
;;
;;     card -> a * card + b
;;
;; and linear maps are closed under composition, so in fact the whole shuffle
;; is a linear map.

(defn parse [s]
  (map
   #(cond
      (re-find #"stack" %) [-1 -1]
      (re-find #"cut" %) [1 (- (read-string (re-find #"-?\d+" %)))]
      (re-find #"increment" %) [(read-string (re-find #"-?\d+" %)) 0])
   (str/split-lines s)))

(defn compose [m shuffles]
  (reduce (fn [[a b] [c d]] [(mod (* a c) m) (mod (+ (* b c) d) m)])
          [1 0]
          shuffles))

;; relies on Fermat's Little Theorem, only valid for prime m
(defn invert [m [a b]]
  (let [a-inv (.modPow (biginteger a) (biginteger (- m 2)) (biginteger m))]
    [(mod a-inv m) (mod (- (* a-inv b)) m)]))

(defn pow [m n shuffle]
  (cond
    (zero? n) [1 0]
    (neg? n) (recur m (- n) (invert m shuffle))
    (odd? n) (compose m [shuffle (pow m (dec n) shuffle)])
    (even? n) (recur m (quot n 2) (compose m [shuffle shuffle]))))

(defn eval-shuffle [m card [a b]]
  (mod (+ (* a card) b) m))

(defn part-1 []
  (->> "input/2019/22" slurp parse (compose 10007) (eval-shuffle 10007 2019)))

(defn part-2 []
  (->> "input/2019/22" slurp parse (compose 119315717514047)
       (pow 119315717514047 -101741582076661)
       (eval-shuffle 119315717514047 2020)))

(deftest test-examples
  (are [res s]
    (= (map #(.indexOf res %) (range 10))
       (map #(->> s parse (compose 10) (eval-shuffle 10 %)) (range 10)))
    [9 8 7 6 5 4 3 2 1 0] "deal in to new stack"
    [3 4 5 6 7 8 9 0 1 2] "cut 3"
    [6 7 8 9 0 1 2 3 4 5] "cut -4"
    [0 7 4 1 8 5 2 9 6 3] "deal with increment 3"
    [0 3 6 9 2 5 8 1 4 7] "deal with increment 7
                           deal into new stack
                           deal into new stack"
    [3 0 7 4 1 8 5 2 9 6] "cut 6
                           deal with increment 7
                           deal into new stack"
    [6 3 0 7 4 1 8 5 2 9] "deal with increment 7
                           deal with increment 9
                           cut -2"
    [9 2 5 8 1 4 7 0 3 6] "deal into new stack
                           cut -2
                           deal with increment 7
                           cut 8
                           cut -4
                           deal with increment 7
                           cut 3
                           deal with increment 9
                           deal with increment 3
                           cut -1"))
