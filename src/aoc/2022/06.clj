(ns aoc.2022.06
  (:require
   [clojure.test :refer [deftest is]]))

(defn marker [m signal]
  (loop [signal signal i 0]
    (if (apply distinct? (take m signal))
      (+ i m)
      (recur (rest signal) (inc i)))))

(defn part-1 [signal]
  (marker 4 signal))

(defn part-2 [signal]
  (marker 14 signal))

(deftest test-example
  (is (= 7 (marker 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (is (= 5 (marker 4 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (is (= 6 (marker 4 "nppdvjthqldpwncqszvftbrmjlhg")))
  (is (= 10 (marker 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (is (= 11 (marker 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))
  (is (= 19 (marker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (is (= 23 (marker 14 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (is (= 23 (marker 14 "nppdvjthqldpwncqszvftbrmjlhg")))
  (is (= 29 (marker 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (is (= 26 (marker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))
