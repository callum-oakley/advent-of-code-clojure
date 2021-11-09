(ns aoc.2017.03
  (:require
   [aoc.vectors :refer [+v *v manhattan-distance]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def rotate-clockwise
  {[0 1] [-1 0] [-1 0] [0 -1] [0 -1] [1 0] [1 0] [0 1]})

(defn spiral []
  (cons
   [0 0]
   ((fn go [ring v dir]
      (lazy-seq
       (concat
        (take (* 2 ring) (rest (iterate #(+v dir %) v)))
        (if (= dir [1 0])
          (go (inc ring)
              (+v [1 -1] (*v (* 2 ring) dir) v)
              (rotate-clockwise dir))
          (go ring
              (+v (*v (* 2 ring) dir) v)
              (rotate-clockwise dir))))))
    1 [1 -1] [0 1])))

(defn part-1* [n]
  (manhattan-distance (nth (spiral) (dec n))))

(defn part-2* []
  (map first
       (reductions (fn [[_ squares] square]
                     (let [value (->> (for [x [-1 0 1]
                                            y [-1 0 1]]
                                        (+v [x y] square))
                                      (keep squares)
                                      (apply +))]
                       [value (assoc squares square value)]))
                   [1 {[0 0] 1}]
                   (rest (spiral)))))

(defn part-1 []
  (part-1* (read-string (slurp "input/2017/03"))))

(defn part-2 []
  (first (filter #(> % (read-string (slurp "input/2017/03"))) (part-2*))))

(deftest test-examples
  (is (= 0 (part-1* 1)))
  (is (= 3 (part-1* 12)))
  (is (= 2 (part-1* 23)))
  (is (= 31 (part-1* 1024)))
  (is (= [1 1 2 4 5 10 11 23 25 26 54 57 59 122 133
          142 147 304 330 351 362 747 806]
         (take 23 (part-2*)))))

(deftest test-answers
  (is (= 326 (part-1)))
  (is (= 363010 (part-2))))
