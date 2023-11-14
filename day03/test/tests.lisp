(ql:quickload "fiveam")
(in-package :fiveam)
(def-suite* binary-diagnostic)

(load "./src/binary-diagnostic.lisp")

(defparameter *sample* '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))

(defparameter *numbers* (string-list-to-bit-vector-list *sample*))

(test string-to-bit-vector-convert-string
      (is (equalp #*1011 (string-to-bit-vector "1011"))))

(test bit-vector-to-list-convert-bit-vector
      (is (equalp '(1 0 1 1) (bit-vector-to-list #*1011))))

(test binary-string-to-list-converts-string
      (is (equalp '(1 0 1 1) (binary-string-to-list "1011"))))

(test binary-string-list-to-bit-vector-list-converts-list-of-string
        (is (equalp #*00100 (car *numbers*)))
        (is (equalp #*11110 (cadr *numbers*))))

(test nth-common-bit-finds-the-most-frequent-nth-bit-in-a-list-of-bit-vectors
        (is (eql 1 (nth-common-bit 0 *numbers* 1)))
        (is (eql 0 (nth-common-bit 1 *numbers* 1))))

(test common-bits-finds-the-most-frequent-nth-bits-in-a-list-of-bit-vectors
        (is (equalp '(1 0 1 1 0) (common-bits *numbers*))))

(test common-bits-narrow-find-the-most-frequent-nth-bits-narrowing-the-input
        (is (equalp '(1 0 1 1 1) (common-bits-narrow *numbers* 1)))
        (is (equalp '(0 1 0 1 0) (common-bits-narrow *numbers* 0))))

(test value-converts-binary-digits-to-integer
      (is (eql 27 (value '(1 1 0 1 1)))))

(test binary-complement-reverse-all-bits
      (is (equalp '(1 1 0 1 0) (binary-complement '(0 0 1 0 1)))))

(test solve-a-find-power-consumption
      (is (eql 2967914 (solve-a "../data/day03.txt"))))

(test solve-b-find-life-support-ratin
      (is (eql 7041258 (solve-b "../data/day03.txt"))))

(run!)

