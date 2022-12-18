(ns aoc.2021.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def close
  {\( \) \[ \] \{ \} \< \>})

(def open?
  (set (keys close)))

(defn parse [s]
  (map #(reduce (fn [pending c]
                  (cond
                    (open? c) (conj pending (close c))
                    (= c (peek pending)) (pop pending)
                    :else (reduced {:corrupted c})))
                ()
                %)
       (str/split-lines s)))

(def score-1
  {\) 3 \] 57 \} 1197 \> 25137})

(defn part-1 [lines]
  (->> lines (keep :corrupted) (map score-1) (apply +)))

(defn median [xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn score-2 [pending]
  (reduce (fn [pts c] (+ (case c \) 1 \] 2 \} 3 \> 4) (* 5 pts))) 0 pending))

(defn part-2 [lines]
  (->> lines (remove :corrupted) (map score-2) median))

(deftest test-example
  (let [sample (str/join "\n"
                         ["[({(<(())[]>[[{[]{<()<>>" "[(()[<>])]({[<{<<[]>>("
                          "{([(<{}[<>[]}>{[]{[(<()>" "(((({<>}<{<{<>}{[]{[]{}"
                          "[[<[([]))<([[{}[[()]]]" "[{[{({}]{}}([{[{{{}}([]"
                          "{<[[]]>}<{[{[{[]{()[[[]" "[<(<(<(<{}))><([]([]()"
                          "<{([([[(<>()){}]>(<<{{" "<{([{{}}[<[[[<>{}]]]>[]]"])]
    (is (= 26397 (part-1 (parse sample))))
    (is (= 288957 (part-2 (parse sample))))))
