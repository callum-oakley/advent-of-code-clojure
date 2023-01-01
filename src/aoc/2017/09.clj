(ns aoc.2017.09
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce (fn [state c]
            (case (:mode state)
              :normal (case c
                        \{ (update state :depth inc)
                        \} (-> state
                               (update :score + (:depth state))
                               (update :depth dec))
                        \< (assoc state :mode :garbage)
                        state)
              :garbage (case c
                         \> (assoc state :mode :normal)
                         \! (assoc state :mode :escape)
                         (update state :garbage inc))
              :escape (assoc state :mode :garbage)))
          {:mode :normal :score 0 :garbage 0 :depth 0}
          s))

(defn part-1 [data]
  (:score data))

(defn part-2 [data]
  (:garbage data))

(deftest test-examples
  (is (= 1 (:score (parse "{}"))))
  (is (= 6 (:score (parse "{{{}}}"))))
  (is (= 5 (:score (parse "{{},{}}"))))
  (is (= 16 (:score (parse "{{{},{},{{}}}}"))))
  (is (= 1 (:score (parse "{<a>,<a>,<a>,<a>}"))))
  (is (= 9 (:score (parse "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))
  (is (= 9 (:score (parse "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))
  (is (= 3 (:score (parse "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))
  (is (= 0 (:garbage (parse "<>"))))
  (is (= 17 (:garbage (parse "<random characters>"))))
  (is (= 3 (:garbage (parse "<<<<>"))))
  (is (= 2 (:garbage (parse "<{!>}>"))))
  (is (= 0 (:garbage (parse "<!!>"))))
  (is (= 0 (:garbage (parse "<!!!>>"))))
  (is (= 10 (:garbage (parse "<{o\"i!a,<{i<a>")))))
