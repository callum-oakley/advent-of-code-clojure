(ns aoc.2021.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def close
  {\( \) \[ \] \{ \} \< \>})

(def open?
  (set (keys close)))

(defn parse [line]
  (reduce (fn [pending c]
            (cond
              (open? c) (conj pending (close c))
              (= c (peek pending)) (pop pending)
              :else (reduced {:corrupted c})))
          ()
          line))

(def score-1
  {\) 3 \] 57 \} 1197 \> 25137})

(defn part-1* [lines]
  (->> lines (map parse) (keep :corrupted) (map score-1) (apply +)))

(defn median [xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn score-2 [pending]
  (reduce (fn [pts c] (+ (case c \) 1 \] 2 \} 3 \> 4) (* 5 pts))) 0 pending))

(defn part-2* [lines]
  (->> lines (map parse) (remove :corrupted) (map score-2) median))

(defn part-1 []
  (->> "input/2021/10" slurp str/split-lines part-1*))

(defn part-2 []
  (->> "input/2021/10" slurp str/split-lines part-2*))

(deftest test-example
  (let [sample ["[({(<(())[]>[[{[]{<()<>>" "[(()[<>])]({[<{<<[]>>("
                "{([(<{}[<>[]}>{[]{[(<()>" "(((({<>}<{<{<>}{[]{[]{}"
                "[[<[([]))<([[{}[[()]]]" "[{[{({}]{}}([{[{{{}}([]"
                "{<[[]]>}<{[{[{[]{()[[[]" "[<(<(<(<{}))><([]([]()"
                "<{([([[(<>()){}]>(<<{{" "<{([{{}}[<[[[<>{}]]]>[]]"]]
    (is (= 26397 (part-1* sample)))
    (is (= 288957 (part-2* sample)))))
