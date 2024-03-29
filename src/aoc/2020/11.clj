(ns aoc.2020.11
  (:require
   [aoc.grid :as grid]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(case % \L :empty \. :floor)))

(def directions
  (for [y [-1 0 1] x [-1 0 1] :when (not= [y x] [0 0])] [y x]))

(defn adjacent [seats [y x]]
  (map (fn [[dy dx]] (seats [(+ y dy) (+ x dx)])) directions))

(defn ray [seat [dy dx]]
  (rest (iterate (fn [[y x]] [(+ y dy) (+ x dx)]) seat)))

(defn visible [seats seat]
  (map
   #(some #{:occupied :empty}
          (take-while some? (map seats (ray seat %))))
   directions))

(defn step [nearby tolerance seats]
  (into {}
        (pmap
         (fn [[seat state]]
           (let [c (count (filter #{:occupied} (nearby seats seat)))]
             [seat (case state
                     :empty (if (zero? c) :occupied :empty)
                     :occupied (if (>= c tolerance) :empty :occupied)
                     state)]))
         seats)))

(defn simulate [nearby tolerance seats]
  (let [seats* (step nearby tolerance seats)]
    ;; Checking for equality here rather then returning a flag from step seems
    ;; wasteful, but it actually makes a negligable difference to the runtime.
    (if (= seats* seats)
      seats
      (recur nearby tolerance seats*))))

(defn part-1 [seats]
  (count (filter #{:occupied} (vals (simulate adjacent 4 seats)))))

(defn part-2 [seats]
  (count (filter #{:occupied} (vals (simulate visible 5 seats)))))

(def sample
  (str "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\n"
       "L.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"))

(deftest test-examples
  (is (= (part-1 (parse sample)) 37))
  (is (= (part-2 (parse sample)) 26)))
