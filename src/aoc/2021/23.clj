(ns aoc.2021.23
  (:require
   [aoc.grid :as grid]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (update-vals (->> s grid/parse (remove (comp #{\# \space} val)) (into {}))
               #(case % \. nil %)))

(defn scrap []
  (parse (str/join "\n" ["#############"
                         "#...........#"
                         "###B#C#B#D###"
                         "  #A#D#C#A#  "
                         "  #########  "])))
