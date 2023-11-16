(ql:quickload "fiveam")
(in-package :fiveam)
(def-suite* giant-squid)

(load "./src/giant-squid.lisp")
(require 'cl-ppcre)

(defparameter *sample* "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defparameter *numbers* (mapcar #'read-numbers (cl-ppcre:split "\\n" *sample*)))

(defparameter *grids* (create-grids (cdr *numbers*)))

(defun copy-grids (grids)
  (make-array '(3 5 5)
              :initial-contents
              (loop for i below (array-dimension grids 0)
                    collect (loop for j below (array-dimension grids 1)
                                  collect (loop for k below (array-dimension grids 2)
                                                collect (aref grids i j k))))))
(test read-numbers-can-parse-input-lines
      (is (equalp
            '((7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1)
              NIL
              (22 13 17 11 0)
              (8 2 23 4 24)
              (21 9 14 16 7)
              (6 10 3 18 5)
              (1 12 20 15 19)
              NIL
              (3 15 0 2 22)
              (9 18 13 17 5)
              (19 8 7 25 23)
              (20 11 10 24 4)
              (14 21 16 12 6)
              NIL
              (14 21 17 24 4)
              (10 16 15 9 19)
              (18 8 23 26 20)
              (22 11 13 6 5)
              (2 0 12 3 7))
            *numbers*)))

(test create-grids-can-create-grids-for-numbers
      (is (equalp #3A(((22 13 17 11 0)
                       (8 2 23 4 24)
                       (21 9 14 16 7)
                       (6 10 3 18 5)
                       (1 12 20 15 19))
                      ((3 15 0 2 22)
                       (9 18 13 17 5)
                       (19 8 7 25 23)
                       (20 11 10 24 4)
                       (14 21 16 12 6))
                      ((14 21 17 24 4)
                       (10 16 15 9 19)
                       (18 8 23 26 20)
                       (22 11 13 6 5)
                       (2 0 12 3 7)))
                  *grids*)))

(test access-to-a-grid-in-the-array
      (let ((grids (copy-grids *grids*)))
        (progn
          (setf (aref grids 2 1 3) nil)
          (setf (aref grids 2 1 4) nil)
          (is (equalp nil (aref grids 2 1 3))))))

(test grid-sum-yields-sum-of-numbers-in-a-grid
      (let ((grids (copy-grids *grids*)))
        (progn
          (setf (aref grids 0 0 0) nil)
          (setf (aref grids 0 1 1) nil)
          (is (eql 276 (grid-sum grids 0))))))

(run!)

