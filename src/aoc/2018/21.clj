(ns aoc.2018.21
  (:require
   [aoc.2018.16 :refer [tick]]
   [aoc.2018.19 :refer [parse]]))

;; TODO port to clojure instead of emulating
(defn valid []
  (let [[ip instructions] (parse (slurp "input/2018/21"))]
    ((fn go [reg]
       (lazy-seq
        (let [reg* (update (tick reg (instructions (reg ip))) ip inc)]
          (if (= 28 (reg ip))
            (cons (reg 5) (go reg*))
            (go reg*)))))
     [0 0 0 0 0 0])))

(defn part-1 []
  (first (valid)))

(defn part-2 []
  (reduce (fn [[seen values] value]
            (if (seen value)
              (reduced (peek values))
              [(conj seen value) (conj values value)]))
          [#{} []]
          (valid)))

; #ip 1
; 00: seti 123 0 5      |
; 01: bani 5 456 5      |
; 02: eqri 5 72 5       |
; 03: addr 5 1 1        |
; 04: seti 0 0 1        |
; 05: seti 0 9 5        \ when test fails, jump to 2 and loop forever
; 06: bori 5 65536 2    \ r2 = r5 ^ 65536 (72 ^ 65536 = 65608)
; 07: seti 7571367 9 5  \ r5 = 7571367
; 08: bani 2 255 4      \ r4 = r2 & 255 (65608 & 255 = 72)
; 09: addr 5 4 5        \ r5 = r5 + r4 (7571367 + 72 = 7571439)
; 10: bani 5 16777215 5 \ r5 = r5 & 16777215 (7571439 & 16777215 = 7571439)
; 11: muli 5 65899 5    \ r5 = r5 * 65899 (7571439 * 65899 = 498950258661)
; 12: bani 5 16777215 5 \ r5 = r5 & 16777215 (498950258661 & 16777215 = 12632037)
; 13: gtir 256 2 4      |
; 14: addr 4 1 1        |
; 15: addi 1 1 1        |
; 16: seti 27 1 1       |
; 17: seti 0 2 4        \ if 256 > r2, jump to 28, else r4 = 0
; 18: addi 4 1 3        \ r3 = r4 + 1
; 19: muli 3 256 3      \ r3 = r3 * 256
; 20: gtrr 3 2 3        |
; 21: addr 3 1 1        |
; 22: addi 1 1 1        |
; 23: seti 25 6 1       |
; 24: addi 4 1 4        \ if r3 > r2, jump to 26, else r4 = r4 + 1
; 25: seti 17 8 1       \ jump to 18
; 26: setr 4 6 2        \ r2 = r4
; 27: seti 7 4 1        \ jump to 8
; 28: eqrr 5 0 4        |
; 29: addr 4 1 1        |
; 30: seti 5 5 1        \ if r5 = r0, halt, else jump to 6