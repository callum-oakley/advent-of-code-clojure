(ns aoc.2018.04
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [log]
  (first
   (reduce
    (fn [[guard->minute->freq guard start] [_ _ _ _ minute* event* guard*]]
      (case (read-string event*)
        Guard [guard->minute->freq (parse-long guard*) nil]
        falls [guard->minute->freq guard (parse-long minute*)]
        wakes [(reduce
                (fn [guard->minute->freq minute]
                  (update-in guard->minute->freq [guard minute] (fnil inc 0)))
                guard->minute->freq
                (range start (parse-long minute*)))
               guard
               nil]))
    [{} nil nil]
    (map #(re-seq #"[0-9A-Za-z]+" %) (sort log)))))

(defn part-* [p guard->minute->freq]
  (let [guard (apply max-key
                     #(apply (case p 1 + 2 max)
                             (vals (guard->minute->freq %)))
                     (keys guard->minute->freq))
        minute->freq (guard->minute->freq guard)
        minute (apply max-key minute->freq (keys minute->freq))]
    (* guard minute)))

(defn part-1 []
  (->> "input/2018/04" slurp str/split-lines parse (part-* 1)))

(defn part-2 []
  (->> "input/2018/04" slurp str/split-lines parse (part-* 2)))

(deftest test-example
  (let [guard->minute->freq (parse ["[1518-11-01 00:00] Guard #10 begins shift"
                                    "[1518-11-01 00:05] falls asleep"
                                    "[1518-11-01 00:25] wakes up"
                                    "[1518-11-01 00:30] falls asleep"
                                    "[1518-11-01 00:55] wakes up"
                                    "[1518-11-01 23:58] Guard #99 begins shift"
                                    "[1518-11-02 00:40] falls asleep"
                                    "[1518-11-02 00:50] wakes up"
                                    "[1518-11-03 00:05] Guard #10 begins shift"
                                    "[1518-11-03 00:24] falls asleep"
                                    "[1518-11-03 00:29] wakes up"
                                    "[1518-11-04 00:02] Guard #99 begins shift"
                                    "[1518-11-04 00:36] falls asleep"
                                    "[1518-11-04 00:46] wakes up"
                                    "[1518-11-05 00:03] Guard #99 begins shift"
                                    "[1518-11-05 00:45] falls asleep"
                                    "[1518-11-05 00:55] wakes up"])]
    (is (= 240 (part-* 1 guard->minute->freq)))
    (is (= 4455 (part-* 2 guard->minute->freq)))))
