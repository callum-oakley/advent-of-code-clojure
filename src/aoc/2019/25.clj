(ns aoc.2019.25
  (:require
   [aoc.2019.intcode :as i]))

(def parse i/parse)

(defn part-1 [mem]
  (->> ["west" "west" "north" "take space heater" "south" "east" "south" "south"
        "take sand" "north" "north" "east" "east" "take mug" "east" "south"
        "east" "south" "take easter egg" "north" "west" "west" "south" "west"
        "south" "south"]
       (mapcat #(map int (str % "\n")))
       (i/run-io mem)
       (map char)
       (apply str)
       (re-find #"You should be able to get in by typing (\d+) on the keypad")
       second))

(defn -main []
  (i/run-interactive (parse (slurp "input/2019/25"))))
