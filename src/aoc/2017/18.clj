(ns aoc.2017.18
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]])
  (:import clojure.lang.PersistentQueue))

(defn parse [s]
  (->> s str/split-lines (mapv #(map read-string (re-seq #"\S+" %)))))

(defn run [mem head reg]
  (let [read #(if (int? %) % (get reg % 0))]
    (when-let [[op x y] (mem head)]
      (case op
        snd {:state :snd
             :x (read x)
             :k (fn [] (run mem (inc head) reg))}
        rcv {:state :rcv
             :x (read x)
             :k (fn [in] (run mem (inc head) (assoc reg x in)))}
        set (recur mem (inc head) (assoc reg x (read y)))
        add (recur mem (inc head) (assoc reg x (+ (read x) (read y))))
        mul (recur mem (inc head) (assoc reg x (* (read x) (read y))))
        mod (recur mem (inc head) (assoc reg x (mod (read x) (read y))))
        jgz (recur mem (+ head (if (pos? (read x)) (read y) 1)) reg)))))

(defn part-1* [mem]
  (loop [m (run mem 0 {})
         last nil]
    (case (:state m)
      :snd (recur ((:k m)) (:x m))
      :rcv (if (zero? (:x m))
             (recur ((:k m) 0) last)
             last))))

(defn part-2* [mem]
  (loop [in0 PersistentQueue/EMPTY
         in1 PersistentQueue/EMPTY
         {s0 :state x0 :x k0 :k :as m0} (run mem 0 '{p 0})
         {s1 :state x1 :x k1 :k :as m1} (run mem 0 '{p 1})
         sent 0]
    (cond
      (and (= :rcv s0) (peek in0)) (recur (pop in0) in1 (k0 (peek in0)) m1 sent)
      (and (= :rcv s1) (peek in1)) (recur in0 (pop in1) m0 (k1 (peek in1)) sent)
      (= :snd s0) (recur in0 (conj in1 x0) (k0) m1 sent)
      (= :snd s1) (recur (conj in0 x1) in1 m0 (k1) (inc sent))
      :else sent)))

(defn part-1 []
  (->> "input/2017/18" slurp parse part-1*))

(defn part-2 []
  (->> "input/2017/18" slurp parse part-2*))

(deftest test-examples
  (let [example-1 "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a
                   set a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
        example-2 "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"]
    (is (= 4 (part-1* (parse example-1))))
    (is (= 3 (part-2* (parse example-2))))))

(deftest test-answers
  (is (= 3423 (part-1)))
  (is (= 7493 (part-2))))
