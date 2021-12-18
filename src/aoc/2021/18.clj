(ns aoc.2021.18
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn carry [n i x]
  (if (vector? n)
    (update n i carry i x)
    (+ x n)))

(defn explode [n]
  (second
   ((fn go [depth n]
      (when (vector? n)
        (if (= 4 depth)
          [(n 0) 0 (n 1)]
          (if-let [[left m right] (go (inc depth) (n 0))]
            [left [m (carry (n 1) 0 right)] 0]
            (when-let [[left m right] (go (inc depth) (n 1))]
              [0 [(carry (n 0) 1 left) m] right])))))
    0 n)))

(defn split [n]
  (if (vector? n)
    (if-let [m (split (n 0))]
      [m (n 1)]
      (when-let [m (split (n 1))]
        [(n 0) m]))
    (when (<= 10 n)
      [(quot n 2) (+ (quot n 2) (mod n 2))])))

(defn +s
  ([x] x)
  ([x y]
   (loop [z [x y]]
     (if-let [z (explode z)]
       (recur z)
       (if-let [z (split z)]
         (recur z)
         z))))
  ([x y & more]
   (reduce +s (+s x y) more)))

(defn magnitude [n]
  (if (vector? n)
    (+ (* 3 (magnitude (n 0))) (* 2 (magnitude (n 1))))
    n))

(defn part-2* [ns]
  (apply max (for [x ns y ns] (magnitude (+s x y)))))

(defn part-1 []
  (->> "input/2021/18" slurp str/split-lines (map read-string)
       (apply +s) magnitude))

(defn part-2 []
  (->> "input/2021/18" slurp str/split-lines (map read-string) part-2*))

(deftest test-examples
  (let [homework [[[[0 [5 8]] [[1 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]]
                  [[[5 [2 8]] 4] [5 [[9 9] 0]]]
                  [6 [[[6 2] [5 6]] [[7 6] [4 7]]]]
                  [[[6 [0 7]] [0 9]] [4 [9 [9 0]]]]
                  [[[7 [6 4]] [3 [1 3]]] [[[5 5] 1] 9]]
                  [[6 [[7 3] [3 2]]] [[[3 8] [5 7]] 4]]
                  [[[[5 4] [7 7]] 8] [[8 3] 8]]
                  [[9 3] [[9 9] [6 [4 9]]]]
                  [[2 [[7 7] 7]] [[5 8] [[9 3] [0 2]]]]
                  [[[[5 2] 5] [8 [3 7]]] [[5 [7 5]] [4 4]]]]]
    (is (= 4140 (magnitude (apply +s homework))))
    (is (= 3993 (part-2* homework)))))
