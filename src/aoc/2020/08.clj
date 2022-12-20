(ns aoc.2020.08
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-instruction [instruction]
  (let [[op arg] (str/split instruction #" ")]
    [(keyword op) (read-string arg)]))

(defn parse [s]
  (mapv parse-instruction (str/split-lines s)))

(defn run [instructions]
  (loop [i 0 acc 0 seen #{}]
    (cond
      (contains? seen i) [:loop acc]
      (= i (count instructions)) [:ok acc]
      :else (let [[op arg] (instructions i)]
              (case op
                :acc (recur (inc i)   (+ acc arg) (conj seen i))
                :jmp (recur (+ i arg)    acc      (conj seen i))
                :nop (recur (inc i)      acc      (conj seen i)))))))

(defn part-1 [instructions]
  (nth (run instructions) 1))

(defn part-2 [instructions]
  (first
   (keep-indexed
    (fn [i [op]]
      (when-let [op ({:jmp :nop :nop :jmp} op)]
        (let [[res acc] (run (assoc-in instructions [i 0] op))]
          (when (= res :ok) acc))))
    instructions)))

(def sample
  (str/join "\n" ["nop +0" "acc +1" "jmp +4"
                  "acc +3" "jmp -3" "acc -99"
                  "acc +1" "jmp -4" "acc +6"]))

(deftest test-examples
  (is (= (part-1 (parse sample)) 5))
  (is (= (part-2 (parse sample)) 8)))
