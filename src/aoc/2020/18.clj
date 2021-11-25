(ns aoc.2020.18
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is are]]))

(def data
  (str/split-lines (slurp "input/2020/18")))

;; I didn't know about it at the time, but this would have been a great
;; application for walk/postwalk.
(defn math [p [x f y g & more]]
  (cond
    (seq? x) (recur p (list* (math p x) f y g more))
    (seq? y) (recur p (list* x f (math p y) g more))
    (and g (> (p g) (p f))) (recur p (list x f (math p (list* y g more))))
    f (recur p (list* ((resolve f) x y) g more))
    :else x))

(defn part-1 []
  (apply + (pmap #(math {'+ 0 '* 0} (read-string (str "(" % ")"))) data)))

(defn part-2 []
  (apply + (pmap #(math {'+ 1 '* 0} (read-string (str "(" % ")"))) data)))

(deftest test-examples
  (are [x y] (= (math {'+ 0 '* 0} x) y)
    '(1 + (2 * 3) + (4 * (5 + 6)))                     51
    '(2 * 3 + (4 * 5))                                 26
    '(5 + (8 * 3 + 9 + 3 * 4 * 3))                     437
    '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))       12240
    '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2) 13632)
  (are [x y] (= (math {'+ 1 '* 0} x) y)
    '(1 + (2 * 3) + (4 * (5 + 6)))                     51
    '(2 * 3 + (4 * 5))                                 46
    '(5 + (8 * 3 + 9 + 3 * 4 * 3))                     1445
    '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))       669060
    '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2) 23340))
