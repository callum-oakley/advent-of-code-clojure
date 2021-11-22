(ns aoc.2017.18
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines (mapv #(map read-string (re-seq #"\S+" %)))))

(defn run-1 [mem]
  (loop [head 0 reg {} last nil]
    (let [read #(if (int? %) % (get reg % 0))]
      (when-let [[op x y] (mem head)]
        (case op
          snd (recur (inc head) reg (read x))
          rcv (if (zero? (read x)) (recur (inc head) reg last) last)
          set (recur (inc head) (assoc reg x (read y)) last)
          add (recur (inc head) (assoc reg x (+ (read x) (read y))) last)
          mul (recur (inc head) (assoc reg x (* (read x) (read y))) last)
          mod (recur (inc head) (assoc reg x (mod (read x) (read y))) last)
          jgz (recur (+ head (if (pos? (read x)) (read y) 1)) reg last))))))

(defn run-2 [mem reg in out]
  (a/go-loop [head 0 reg reg sent 0]
    (let [read #(if (int? %) % (get reg % 0))]
      (when-let [[op x y] (mem head)]
        (case op
          snd (do (a/>! out (read x)) (recur (inc head) reg (inc sent)))
          ;; poor man's deadlock check
          rcv (if-let [v (a/alt! in ([v] v) (a/timeout 10) nil)]
                (recur (inc head) (assoc reg x v) sent)
                sent)
          set (recur (inc head) (assoc reg x (read y)) sent)
          add (recur (inc head) (assoc reg x (+ (read x) (read y))) sent)
          mul (recur (inc head) (assoc reg x (* (read x) (read y))) sent)
          mod (recur (inc head) (assoc reg x (mod (read x) (read y))) sent)
          jgz (recur (+ head (if (pos? (read x)) (read y) 1)) reg sent))))))

(defn part-1 []
  (->> "input/2017/18" slurp parse run-1))

(defn part-2* [mem]
  (let [in0 (a/chan 100) in1 (a/chan 100)]
    (run-2 mem '{p 0} in0 in1)
    (a/<!! (run-2 mem '{p 1} in1 in0))))

(defn part-2 []
  (->> "input/2017/18" slurp parse part-2*))

(deftest test-examples
  (let [example-1 "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a
                   set a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
        example-2 "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"]
    (is (= 4 (run-1 (parse example-1))))
    (is (= 3 (part-2* (parse example-2))))))

(deftest test-answers
  (is (= 3423 (part-1)))
  (is (= 7493 (part-2))))
