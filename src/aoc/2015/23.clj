(ns aoc.2015.23
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (mapv #(map read-string (re-seq #"\S+" %)) (str/split-lines s)))

(defn run [m head registers]
  (if-let [[op x y] (get m head)]
    (case op
      hlf (recur m (inc head) (update registers x quot 2))
      tpl (recur m (inc head) (update registers x * 3))
      inc (recur m (inc head) (update registers x inc))
      jmp (recur m (+ x head) registers)
      jie (recur m (if (even? (registers x)) (+ y head) (inc head)) registers)
      jio (recur m (if (= 1 (registers x)) (+ y head) (inc head)) registers))
    (registers 'b)))

(defn part-1 []
  (run (parse (slurp "input/2015/23")) 0 '{a 0 b 0}))

(defn part-2 []
  (run (parse (slurp "input/2015/23")) 0 '{a 1 b 0}))
