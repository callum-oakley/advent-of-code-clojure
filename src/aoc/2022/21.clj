(ns aoc.2022.21 
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (into {} (map (fn [line]
                  (let [[m & job] (map read-string (re-seq #"[^:\s]+" line))]
                    [m (if (= 1 (count job)) (first job) (vec job))]))
                (str/split-lines s))))

(defn part-1 [monkeys]
  ((fn go [m]
     (if (int? (monkeys m))
       (monkeys m)
       (let [[x op y] (monkeys m)]
         (eval (list op (go x) (go y))))))
   'root))

(defn part-2 [monkeys]
  (let [f (fn [h] (-> monkeys (assoc-in ['root 1] '-) (assoc 'humn h) part-1))]
    ;; Since monkeys is a tree, f is a linear function of h, so we can find the
    ;; root by taking the value of f at two points and extrapolating.
    (/ (f 0) (- (f 0) (f 1)))))

(def example
  "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt
   dvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4
   pppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32")

(deftest test-example
  (is (= 152 (part-1 (parse example))))
  (is (= 301 (part-2 (parse example)))))
