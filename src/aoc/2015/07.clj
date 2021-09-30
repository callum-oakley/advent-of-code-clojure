(ns aoc.2015.07
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is are]]))

(defn parse [s]
  (->> (str/split-lines s)
       (map (fn [line]
              (let [[in out] (str/split line #"->")
                    in (mapv read-string (str/split (str/trim in) #" "))]
                [(read-string out)
                 (case (count in)
                   1 ['ID (in 0)]
                   2 in
                   3 [(in 1) (in 0) (in 2)])])))
       (into {})))

(def emulate
  (memoize
   (fn [circuit wire]
     (let [[gate x y] (circuit wire)
           signal #(if (int? %) % (emulate circuit %))]
       (bit-and 0xffff ;; keep us in 16 bits
                (case gate
                  ID                      (signal x)
                  AND    (bit-and         (signal x) (signal y))
                  OR     (bit-or          (signal x) (signal y))
                  LSHIFT (bit-shift-left  (signal x) (signal y))
                  RSHIFT (bit-shift-right (signal x) (signal y))
                  NOT    (bit-not         (signal x))))))))

(defn part-1 []
  (emulate (parse (slurp "input/2015/07")) 'a))

(defn part-2 []
  (let [circuit (parse (slurp "input/2015/07"))]
    (emulate (assoc circuit 'b ['ID (emulate circuit 'a)]) 'a)))

(deftest test-emulate
  (let [circuit (parse "123 -> x
                        456 -> y
                        x AND y -> d
                        x OR y -> e
                        x LSHIFT 2 -> f
                        y RSHIFT 2 -> g
                        NOT x -> h
                        NOT y -> i")]
    (are [wire signal] (= signal (emulate circuit wire))
      'd 72     'e 507    'f 492    'g 114
      'h 65412  'i 65079  'x 123    'y 456)))

(deftest test-answers
  (is (= 956 (part-1)))
  (is (= 40149 (part-2))))
