(ns aoc.2019.21
  (:require
   [aoc.2019.intcode :as i]
   [clojure.string :as str]))

(defmacro springscript [& body]
  `(->> '~body
        (partition-all 3)
        (mapcat #(map int (str (str/join " " %) "\n")))
        (i/run-io (i/load "input/2019/21"))
        last))

(defn part-1 []
  (springscript
    OR A T
    AND B T
    AND C T
    NOT T T
    AND D T
    OR T J
    WALK))

(defn part-2 []
  (springscript
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
  (i/run-interactive (i/load "input/2019/21")))
