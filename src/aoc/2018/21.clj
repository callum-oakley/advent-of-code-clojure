(ns aoc.2018.21
  (:require
   [aoc.2018.16 :as d16]
   [aoc.2018.19 :as d19]
   [clojure.test :refer [deftest is]]))

(def parse d19/parse)

;; The program halts after the test on line 28 if r0 is equal to r5. r0 doesn't
;; otherwise feature in the calculation, so by running the program and noting r5
;; each time we reach line 28, we produce a sequence of valid settings for r0 to
;; cause the program to halt.
(defn valid [[ip instructions]]
  ((fn go [reg]
     (lazy-seq
      (let [reg* (update (d16/tick reg (instructions (reg ip))) ip inc)]
        (if (= 28 (reg ip))
          (cons (reg 5) (go reg*))
          (go reg*)))))
   [0 0 0 0 0 0]))

;; Part 2 requires us to find the last element of the above sequence before a
;; repeat, and this turns out to be fairly slow. Reimplementing the process in
;; Clojure gives a much needed speed boost. See corresponding line numbers in
;; comments (lines 01-07 are the bitwise AND test).
(defn valid* []
  ((fn go [r2 r5]
     (lazy-seq
      (let [r4 (bit-and r2 255)                    ; 08
            r5 (+ r5 r4)                           ; 09
            r5 (bit-and r5 16777215)               ; 10
            r5 (* r5 65899)                        ; 11
            r5 (bit-and r5 16777215)]              ; 12
        (if (< r2 256)                             ; 13-17
          (cons r5 (go (bit-or r5 65536) 7571367)) ; 28-30, 06, 07
          (loop [r4 0]                             ;
            (let [r3 (+ r4 1)                      ; 18
                  r3 (* r3 256)]                   ; 19
              (if (< r2 r3)                        ; 20-23
                (go r4 r5)                         ; 26-27
                (recur (inc r4)))))))))            ; 24-25
   65536 7571367))

(defn part-1 [[ip instructions]]
  (first (valid [ip instructions])))

(defn part-2 [_]
  (reduce (fn [[seen values] value]
            (if (seen value)
              (reduced (peek values))
              [(conj seen value) (conj values value)]))
          [#{} []]
          (valid*)))

(deftest test-valid*
  (is (= (take 10 (valid (parse (slurp "input/2018/21")))) (take 10 (valid*)))))

; #ip 1
; 00: seti 123 0 5
; 01: bani 5 456 5
; 02: eqri 5 72 5
; 03: addr 5 1 1
; 04: seti 0 0 1
; 05: seti 0 9 5
; 06: bori 5 65536 2
; 07: seti 7571367 9 5
; 08: bani 2 255 4
; 09: addr 5 4 5
; 10: bani 5 16777215 5
; 11: muli 5 65899 5
; 12: bani 5 16777215 5
; 13: gtir 256 2 4
; 14: addr 4 1 1
; 15: addi 1 1 1
; 16: seti 27 1 1
; 17: seti 0 2 4
; 18: addi 4 1 3
; 19: muli 3 256 3
; 20: gtrr 3 2 3
; 21: addr 3 1 1
; 22: addi 1 1 1
; 23: seti 25 6 1
; 24: addi 4 1 4
; 25: seti 17 8 1
; 26: setr 4 6 2
; 27: seti 7 4 1
; 28: eqrr 5 0 4
; 29: addr 4 1 1
; 30: seti 5 5 1
