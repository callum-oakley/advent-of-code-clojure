(ns aoc.2017.09
  (:require
   [clojure.test :refer [deftest is]]))

(defn part-* [s]
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

(defn part-1 []
  (->> "input/2017/09" slurp part-* :score))

(defn part-2 []
  (->> "input/2017/09" slurp part-* :garbage))

(deftest test-examples
  (is (= 1 (:score (part-* "{}"))))
  (is (= 6 (:score (part-* "{{{}}}"))))
  (is (= 5 (:score (part-* "{{},{}}"))))
  (is (= 16 (:score (part-* "{{{},{},{{}}}}"))))
  (is (= 1 (:score (part-* "{<a>,<a>,<a>,<a>}"))))
  (is (= 9 (:score (part-* "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))
  (is (= 9 (:score (part-* "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))
  (is (= 3 (:score (part-* "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))
  (is (= 0 (:garbage (part-* "<>"))))
  (is (= 17 (:garbage (part-* "<random characters>"))))
  (is (= 3 (:garbage (part-* "<<<<>"))))
  (is (= 2 (:garbage (part-* "<{!>}>"))))
  (is (= 0 (:garbage (part-* "<!!>"))))
  (is (= 0 (:garbage (part-* "<!!!>>"))))
  (is (= 10 (:garbage (part-* "<{o\"i!a,<{i<a>")))))

(deftest test-answers
  (is (= 21037 (part-1)))
  (is (= 9495 (part-2))))
