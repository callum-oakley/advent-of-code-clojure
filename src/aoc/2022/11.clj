(ns aoc.2022.11
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]])
  (:import clojure.lang.PersistentQueue))

(defn parse [s]
  (mapv (fn [block]
          (let [lines (vec (str/split-lines block))]
            {:items (into PersistentQueue/EMPTY
                          (map read-string (re-seq #"\d+" (lines 1))))
             :op (let [[a op b] (map read-string
                                     (re-seq #"old|\+|\*|\d+" (lines 2)))]
                   (eval (list 'fn ['old] (list op a b))))
             :test (read-string (re-find #"\d+" (lines 3)))
             :true (read-string (re-find #"\d+" (lines 4)))
             :false (read-string (re-find #"\d+" (lines 5)))
             :inspections 0}))
        (str/split s #"\n\n")))

(defn play [p rounds monkeys]
  ;; We only care about whether worry levels are divisible by our tests, so we
  ;; can take the modulus with the LCM of all the divisors at each step.
  (let [m (reduce lcm (map :test monkeys))]
    (reduce (fn [monkeys i]
              (if-let [item (peek (:items (monkeys i)))]
                (let [item ((:op (monkeys i)) item)
                      item (case p 1 (quot item 3) 2 (mod item m))
                      throw (if (zero? (mod item (:test (monkeys i))))
                              (:true (monkeys i))
                              (:false (monkeys i)))]
                  (recur (-> monkeys
                             (update-in [i :inspections] inc)
                             (update-in [i :items] pop)
                             (update-in [throw :items] conj item))
                         i))
                monkeys))
            monkeys
            (apply concat (repeat rounds (range (count monkeys)))))))

(defn part-* [p monkeys]
  (->> monkeys (play p (case p 1 20 2 10000))
       (map :inspections) (sort >) (take 2) (apply *)))

(defn part-1 []
  (->> "input/2022/11" slurp parse (part-* 1)))

(defn part-2 []
  (->> "input/2022/11" slurp parse (part-* 2)))

(def example
  "Monkey 0:
     Starting items: 79, 98
     Operation: new = old * 19
     Test: divisible by 23
       If true: throw to monkey 2
       If false: throw to monkey 3

   Monkey 1:
     Starting items: 54, 65, 75, 74
     Operation: new = old + 6
     Test: divisible by 19
       If true: throw to monkey 2
       If false: throw to monkey 0

   Monkey 2:
     Starting items: 79, 60, 97
     Operation: new = old * old
     Test: divisible by 13
       If true: throw to monkey 1
       If false: throw to monkey 3

   Monkey 3:
     Starting items: 74
     Operation: new = old + 3
     Test: divisible by 17
       If true: throw to monkey 0
       If false: throw to monkey 1")

(deftest test-example
  (is (= 10605 (part-* 1 (parse example))))
  (is (= 2713310158 (part-* 2 (parse example)))))