(ns aoc.2022.22 
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v *v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[tiles instructions] (str/split s #"\n\n")]
    [(grid/parse tiles #(case % \. :empty \# :wall \space nil))
     (->> instructions (re-seq #"\d+|R|L") (map read-string))]))

(defn wrap-1 [tiles]
  (fn [[[y x] dir]]
    (let [[dy dx :as dir*] (map abs dir)]
      (loop [pos [(* y dx) (* x dy)]]
        (let [pos* (+v pos dir*)]
          (if (some neg? dir)
            (if (and (tiles pos) (not (tiles pos*))) [pos dir] (recur pos*))
            (if (tiles pos) [pos dir] (recur pos*))))))))

;; Given the side length of a face, expands a mapping between edges of faces (7
;; joins) to a full mapping between tiles.
(defn wrap-2 [z joins]
  (let [edge (fn [[y x] dir]
               (map (fn [i]
                      (case dir
                        [-1 0] [(* y z) (+ i (* x z))]
                        [0 1] [(+ (* y z) i) (+ (* x z) (dec z))]
                        [1 0] [(+ (* y z) (dec z)) (+ (* x z) (dec z) (- i))]
                        [0 -1] [(+ (* y z) (dec z) (- i)) (* x z)]))
                    (range z)))]
    (reduce
     (fn [wrap [[face0 dir0] [face1 dir1]]]
       (reduce (fn [wrap [pos0 pos1]]
                 (assoc wrap
                        [pos0 dir0] [pos1 (*v -1 dir1)]
                        [pos1 dir1] [pos0 (*v -1 dir0)]))
               wrap
               (map vector (edge face0 dir0) (reverse (edge face1 dir1)))))
     {}
     joins)))

(defn part-* [wrap [tiles instructions]]
  (let [move (fn [pos dir]
               (let [[pos* dir*] (if (tiles (+v pos dir))
                                   [(+v pos dir) dir]
                                   (wrap [pos dir]))]
                 (if (= :wall (tiles pos*)) [pos dir] [pos* dir*])))
        [[y x] dir]
        (reduce (fn [[pos dir] instruction]
                  (case instruction
                    L [pos (grid/left dir)]
                    R [pos (grid/right dir)]
                    0 [pos dir]
                    (recur (move pos dir) (dec instruction))))
                ((wrap-1 tiles) [[0 0] grid/east])
                instructions)]
    (+ (* 1000 (inc y)) (* 4 (inc x))
       (case dir [0 1] 0 [1 0] 1 [0 -1] 2 [-1 0] 3))))

(defn part-1 [[tiles instructions]]
  (part-* (wrap-1 tiles) [tiles instructions]))

(defn part-2 [data]
  (part-* (wrap-2 50 {[[0 1] grid/north] [[3 0] grid/west]
                      [[0 1] grid/west]  [[2 0] grid/west]
                      [[0 2] grid/north] [[3 0] grid/south]
                      [[0 2] grid/east]  [[2 1] grid/east]
                      [[0 2] grid/south] [[1 1] grid/east]
                      [[1 1] grid/west]  [[2 0] grid/north]
                      [[2 1] grid/south] [[3 0] grid/east]})
          data))

(def example
  (str
   "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n"
   "........#...\n..#....#....\n..........#.\n        ...#....\n"
   "        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"))

(deftest test-example
  (is (= 6032 (part-1 (parse example))))
  (is (= 5031 (part-* (wrap-2 4 {[[0 2] grid/north] [[1 0] grid/north]
                                 [[0 2] grid/east]  [[2 3] grid/east]
                                 [[0 2] grid/west]  [[1 1] grid/north]
                                 [[1 0] grid/west]  [[2 3] grid/south]
                                 [[1 0] grid/south] [[2 2] grid/south]
                                 [[1 1] grid/south] [[2 2] grid/east]
                                 [[1 2] grid/east]  [[2 3] grid/north]})
                      (parse example)))))
