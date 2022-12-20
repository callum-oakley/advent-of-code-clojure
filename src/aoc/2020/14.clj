(ns aoc.2020.14
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-instruction [instruction]
  (let [[_ mask index value]
        (re-matches
         #"(?:(?:mask = ([X01]{36}))|(?:mem\[(\d+)\] = (\d+)))"
         instruction)]
    (if mask
      {:op :mask
       :or-mask (read-string (apply str "2r" (map {\0 \0 \1 \1 \X \0} mask)))
       :and-mask (read-string (apply str "2r" (map {\0 \0 \1 \1 \X \1} mask)))
       :xor-masks (->> (reverse mask)
                       (map-indexed #({\X (bit-shift-left 1 %1)} %2))
                       (keep identity)
                       comb/subsets
                       (map #(apply + %)))}
      {:op :mem
       :index (read-string index)
       :value (read-string value)})))

(defn parse [s]
  (map parse-instruction (str/split-lines s)))

(defn step-1 [[mem or-mask and-mask] instruction]
  (case (:op instruction)
    :mask [mem (:or-mask instruction) (:and-mask instruction)]
    :mem [(assoc mem
                 (:index instruction)
                 (bit-and and-mask (bit-or or-mask (:value instruction))))
          or-mask
          and-mask]))

(defn step-2 [[mem or-mask xor-masks] instruction]
  (case (:op instruction)
    :mask [mem (:or-mask instruction) (:xor-masks instruction)]
    :mem [(into mem
                (map
                 (fn [xor-mask]
                   [(bit-xor xor-mask (bit-or or-mask (:index instruction)))
                    (:value instruction)])
                 xor-masks))
          or-mask
          xor-masks]))

(defn part-1 [instructions]
  (apply + (vals (first (reduce step-1 [{} 0 0] instructions)))))

(defn part-2 [instructions]
  (apply + (vals (first (reduce step-2 [{} 0 0] instructions)))))

(def sample-1
  (str "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n"
       "mem[8] = 11\nmem[7] = 101\nmem[8] = 0"))

(def sample-2
  (str "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\n"
       "mask = 00000000000000000000000000000000X0XX\nmem[26] = 1"))

(deftest test-examples
  (is (= (part-1 (parse sample-1)) 165))
  (is (= (part-2 (parse sample-2)) 208)))
