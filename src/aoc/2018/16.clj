(ns aoc.2018.16
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ops
  '#{addr addi mulr muli banr bani borr bori
     setr seti gtir gtri gtrr eqir eqri eqrr})

(defn parse [s]
  (let [[samples instructions] (str/split s #"\n\n\n")]
    [(->> samples (re-seq #"\d+") (map read-string)
          (partition 4) (map vec) (partition 3))
     (->> instructions (re-seq #"\d+") (map read-string) (partition 4))]))

(defn tick [reg [op a b c]]
  (assoc reg c (case op
                 addr (+ (reg a) (reg b))
                 addi (+ (reg a) b)
                 mulr (* (reg a) (reg b))
                 muli (* (reg a) b)
                 banr (bit-and (reg a) (reg b))
                 bani (bit-and (reg a) b)
                 borr (bit-or (reg a) (reg b))
                 bori (bit-or (reg a) b)
                 setr (reg a)
                 seti a
                 gtir (if (> a (reg b)) 1 0)
                 gtri (if (> (reg a) b) 1 0)
                 gtrr (if (> (reg a) (reg b)) 1 0)
                 eqir (if (= a (reg b)) 1 0)
                 eqri (if (= (reg a) b) 1 0)
                 eqrr (if (= (reg a) (reg b)) 1 0))))

(defn valid-ops [[before [_ a b c] after]]
  (set (filter #(= after (tick before [% a b c])) ops)))

(defn eliminate
  "Reduces a one->many map to a one->one map by process of elimination."
  [one->many]
  (loop [one->many one->many one->one {}]
    (if-let [[k v] (some (fn [[k vs]] (when (= 1 (count vs)) [k (first vs)]))
                         one->many)]
      (recur (update-vals one->many #(disj % v)) (assoc one->one k v))
      one->one)))

(defn code->ops [samples]
  (reduce (fn [code->ops [_ [code _ _ _] _ :as sample]]
            (update code->ops code set/intersection (valid-ops sample)))
          (zipmap (range 16) (repeat ops))
          samples))

(defn part-1 []
  (->> "input/2018/16" slurp parse first
       (filter #(<= 3 (count (valid-ops %)))) count))

(defn part-2 []
  (first
   (let [[samples instructions] (parse (slurp "input/2018/16"))
         code->op (eliminate (code->ops samples))]
     (reduce (fn [reg [code a b c]] (tick reg [(code->op code) a b c]))
             [0 0 0 0]
             instructions))))

(deftest test-example
  (is (= 3 (count (valid-ops [[3 2 1 1] [9 2 1 2] [3 2 2 1]])))))
