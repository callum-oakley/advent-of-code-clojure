(ns aoc.2022.21 
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (into {} (map (fn [line]
                  (let [[m & job] (map read-string (re-seq #"[^:\s]+" line))]
                    [m (if (= 1 (count job)) (first job) job)]))
                (str/split-lines s))))

(defn part-1 [monkeys]
  ((fn go [m]
     (if (int? (monkeys m))
       (monkeys m)
       (let [[x op y] (monkeys m)]
         (eval (list op (go x) (go y))))))
   'root))

(defn part-2 [monkeys]
  ((fn go [m]
     (cond
       (= m 'humn) [1 0]                  ;; 1h + 0
       (int? (monkeys m)) [0 (monkeys m)] ;; 0h + (monkeys m)
       :else (let [[x op y] (monkeys m)
                   [a b] (go x)           ;; ah + b
                   [c d] (go y)]          ;; ch + d
               (if (= m 'root)
                 (/ (- d b) (- a c)) ;; ah + b = ch + d => h = (d - b) / (a - c)
                 (case op
                   + [(+ a c) (+ b d)]
                   - [(- a c) (- b d)]
                   ;; optimistic guess that we never have to handle the
                   ;; difficult cases for * or /
                   * (cond (zero? a) [(* b c) (* b d)]
                           (zero? c) [(* d a) (* d b)])
                   / (when (zero? c) [(/ a d) (/ b d)]))))))
   'root))

(def example
  "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt
   dvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4
   pppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32")

(deftest test-example
  (is (= 152 (part-1 (parse example))))
  (is (= 301 (part-2 (parse example)))))
