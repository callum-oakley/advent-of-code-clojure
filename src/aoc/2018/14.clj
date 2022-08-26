(ns aoc.2018.14
  (:require
   [clojure.test :refer [deftest is]]))

(defn recipes []
  (concat
   [3 7]
   ((fn go [a b s]
      (lazy-seq
       (let [new (map #(- (int %) 48) (str (+ (s a) (s b))))
             s (into s new)]
         (concat
          new
          (go (mod (+ a (s a) 1) (count s)) (mod (+ b (s b) 1) (count s)) s)))))
    0 1 [3 7])))

(defn part-1* [n]
  (->> (recipes) (drop n) (take 10) (apply str)))

(defn part-2* [target]
  (let [target (mapv #(- (int %) 48) target)]
    (loop [r (recipes) n 0]
      (if (= target (take (count target) r)) n (recur (rest r) (inc n))))))

(defn part-1 []
  (->> "input/2018/14" slurp read-string part-1*))

(defn part-2 []
  (->> "input/2018/14" slurp (re-find #"\d+") part-2*))

(deftest test-examples
  (is (= "5158916779" (part-1* 9)))
  (is (= "0124515891" (part-1* 5)))
  (is (= "9251071085" (part-1* 18)))
  (is (= "5941429882" (part-1* 2018)))
  (is (= 9 (part-2* "51589")))
  (is (= 5 (part-2* "01245")))
  (is (= 18 (part-2* "92510")))
  (is (= 2018 (part-2* "59414"))))
