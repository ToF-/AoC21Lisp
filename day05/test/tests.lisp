(ql:quickload "fiveam")
(in-package :fiveam)
(def-suite* hydrothermal-venture)

(load "./src/hydrothermal-venture.lisp")
(require 'cl-ppcre)

(defparameter *sample* "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defparameter *coords* (mapcar #'read-coords (cl-ppcre:split "\\n" *sample*)))
(test read-coords-from-a-string
      (is (equalp (cons (cons 23 17) (cons 8 4807)) (read-coords "23,17 -> 8,4807")))
      (is (equalp (cons (cons 0 9) (cons 5 9)) (car *coords*))))

(run!)

