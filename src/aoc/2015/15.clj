(ns aoc.2015.15
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn integer-partitions [n parts]
  (if (= 1 parts)
    [[n]]
    (mapcat (fn [m]
              (map #(conj % m) (integer-partitions (- n m) (dec parts))))
            (range (inc n)))))

(defn parse [s]
  (map (fn [line]
         (->> (re-seq #"(\w+) (-?\d+)" line)
              (map (fn [[_ a b]] [(keyword a) (read-string b)]))
              (into {})))
       (str/split-lines s)))

(defn update-vals [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn mix [ingredients recipe]
  (update-vals (->> (map (fn [ingredient spoons]
                           (update-vals ingredient #(* spoons %)))
                         ingredients
                         recipe)
                    (apply merge-with +))
               #(max 0 %)))

(defn part-* [p ingredients]
  (->> (integer-partitions 100 (count ingredients))
       (map #(mix ingredients %))
       (filter #(or (= 1 p) (= 500 (:calories %))))
       (map #(apply * (vals (dissoc % :calories))))
       (apply max)))

(defn part-1 []
  (->> "input/2015/15" slurp parse (part-* 1)))

(defn part-2 []
  (->> "input/2015/15" slurp parse (part-* 2)))

(def sample
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
   Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(deftest test-mix
  (is (= {:capacity 68 :durability 80 :flavor 152 :texture 76}
         (dissoc (mix (parse sample) [44 56]) :calories))))

(deftest test-part-*
  (is (= 62842880 (part-* 1 (parse sample))))
  (is (= 57600000 (part-* 2 (parse sample)))))

(deftest test-answers
  (is (= 222870 (part-1)))
  (is (= 117936 (part-2))))
