(ns aoc.2019.16
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [signal]
  (mapv parse-long (re-seq #"\d" signal)))

(defn pattern [x z]
  (case (mod (quot (inc z) (inc x)) 4) 0 0 1 1 2 0 3 -1))

(defn fft [signal n]
  (reduce
   (fn [signal _]
     (mapv (fn [x]
             (mod (abs (apply + (map #(* (pattern x %) (signal %)) (range n))))
                  10))
           (range n)))
   signal
   (range 100)))

;; Relies on offset being more than n / 2, in which case pattern is 0 for z < x
;; and 1 for z >= x and we're just doing cumulative sums.
(defn fft-fast [signal n offset]
  (persistent!
   (reduce (fn [signal _]
             (reduce (fn [signal z]
                       (assoc! signal z
                               (mod (+ (signal z) (signal (inc z))) 10)))
                     signal
                     (range (- (count signal) 2) -1 -1)))
           (transient (mapv #(signal (mod % (count signal))) (range offset n)))
           (range 100))))

(defn part-1* [signal]
  (apply str (take 8 (fft signal (count signal)))))

(defn part-2* [signal]
  (let [offset (parse-long (apply str (take 7 signal)))]
    (apply str (take 8 (fft-fast signal (* 10000 (count signal)) offset)))))

(defn part-1 []
  (->> "input/2019/16" slurp parse part-1*))

(defn part-2 []
  (->> "input/2019/16" slurp parse part-2*))

(deftest test-example
  (is (= [0 1 1 0 0 -1 -1 0 0 1 1 0 0 -1 -1] (map #(pattern 1 %) (range 15))))
  (is (= "24176176" (part-1* (parse "80871224585914546619083218645595"))))
  (is (= "73745418" (part-1* (parse "19617804207202209144916044189917"))))
  (is (= "52432133" (part-1* (parse "69317163492948606335995924319873"))))
  (is (= "84462026" (part-2* (parse "03036732577212944063491565474664"))))
  (is (= "78725270" (part-2* (parse "02935109699940807407585447034323"))))
  (is (= "53553731" (part-2* (parse "03081770884921959731165446850517")))))
