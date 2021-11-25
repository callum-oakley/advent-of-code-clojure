(ns aoc.2016.12
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (mapv #(map read-string (re-seq #"\S+" %)) (str/split-lines s)))

(defn run [m head reg]
  (if-let [[op x y] (get m head)]
    (let [read #(if (int? %) % (reg %))]
      (case op
        cpy (recur m (inc head) (assoc reg y (read x)))
        inc (recur m (inc head) (update reg x inc))
        dec (recur m (inc head) (update reg x dec))
        jnz (recur m (if (zero? (read x)) (inc head) (+ y head)) reg)))
    (reg 'a)))

(defn part-1 []
  (run (parse (slurp "input/2016/12")) 0 '{a 0 b 0 c 0 d 0}))

(defn part-2 []
  (run (parse (slurp "input/2016/12")) 0 '{a 0 b 0 c 1 d 0}))
