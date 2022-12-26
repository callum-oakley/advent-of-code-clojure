(ns aoc.2018.13
  (:require
   [aoc.grid :as g]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def turns (cycle [g/left identity g/right]))

(defn parse [s]
  [(g/parse s (fn [c]
                (cond
                  (= \+ c) :intersection
                  (= \/ c) {g/north g/east g/east g/north
                            g/south g/west g/west g/south}
                  (= \\ c) {g/north g/west g/east g/south
                            g/south g/east g/west g/north}
                  (#{\| \- \^ \> \v \<} c) identity)))
   (g/parse s (fn [c]
                (cond
                  (= \^ c) {:dir g/north :turns turns}
                  (= \> c) {:dir g/east :turns turns}
                  (= \v c) {:dir g/south :turns turns}
                  (= \< c) {:dir g/west :turns turns})))])

(defn move [pos track cart]
  (if (= track :intersection)
    (let [dir ((first (:turns cart)) (:dir cart))]
      [(+v pos dir) {:dir dir :turns (rest (:turns cart))}])
    [(+v pos (track (:dir cart))) (update cart :dir track)]))

(defn tick [tracks carts]
  (reduce (fn [[carts* crashes] pos]
            (if-let [cart (carts* pos)]
              (let [[pos* cart*] (move pos (tracks pos) cart)]
                (if (carts* pos*)
                  [(dissoc carts* pos pos*) (conj crashes pos*)]
                  [(-> carts* (dissoc pos) (assoc pos* cart*)) crashes]))
              [carts* crashes]))
          [carts []]
          (sort (keys carts))))

(defn part-1 [[tracks carts]]
  (loop [[carts crashes] (tick tracks carts)]
    (if (first crashes)
      (str/join "," (reverse (first crashes)))
      (recur (tick tracks carts)))))

(defn part-2 [[tracks carts]]
  (loop [[carts] (tick tracks carts)]
    (if (= 1 (count carts))
      (str/join "," (reverse (first (keys carts))))
      (recur (tick tracks carts)))))

(def example-1
  (str/join "\n" ["/->-\\        " "|   |  /----\\" "| /-+--+-\\  |"
                  "| | |  | v  |" "\\-+-/  \\-+--/" "  \\------/   "]))

(def example-2
  (str/join "\n" ["/>-<\\  " "|   |  " "| /<+-\\" "| | | v" "\\>+</ |"
                  "  |   ^" "  \\<->/"]))

(deftest test-examples
  (is (= "3,0" (part-1 (parse "->---<-"))))
  (is (= "7,3" (part-1 (parse example-1))))
  (is (= "6,4" (part-2 (parse example-2)))))
