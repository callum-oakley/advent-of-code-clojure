(ns aoc.2019.21
  (:require
   [aoc.2019.intcode :as i]
   [clojure.string :as str]))

(def parse i/parse)

(defmacro springscript [mem & body]
  `(->> '~body
        (partition-all 3)
        (mapcat #(map int (str (str/join " " %) "\n")))
        (i/run-io ~mem)
        last))

(defn part-1 [mem]
  (springscript mem
    OR A T
    AND B T
    AND C T
    NOT T T
    AND D T
    OR T J
    WALK))

(defn part-2 [mem]
  (springscript mem
    OR A T
    AND B T
    AND C T
    NOT T T
    AND D T
    OR E J
    OR H J
    AND T J
    RUN))

(defn -main []
  (i/run-interactive (parse (slurp "input/2019/21"))))
