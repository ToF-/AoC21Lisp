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

(test after-finding-a-number-the-number-is-marked
      (let ((grids (copy-grids *grids*)))
        (progn
          (mark-number 8 grids)
          (is (equalp nil (aref grids 0 1 0)))
          (is (equalp nil (aref grids 1 2 1)))
          (is (equalp nil (aref grids 2 2 1)))
          (is (eql 292 (grid-sum 0 grids)))
          (is (eql 316 (grid-sum 1 grids)))
          (is (eql 317 (grid-sum 2 grids)))
          )))

(test after-marking-a-row-or-a-column
      (let ((grids (copy-grids *grids*)))
        (progn
          (mark-number 22 grids)
          (mark-number 13 grids)
          (mark-number 17 grids)
          (mark-number 11 grids)
          (mark-number 0 grids)
          (mark-number 14 grids)
          (mark-number 10 grids)
          (mark-number 18 grids)
          (mark-number 22 grids)
          (mark-number 2 grids)
          (is (equalp T (winning-gridp 0 grids)))
          (is (equalp nil (winning-gridp 1 grids)))
          (is (equalp T (winning-gridp 2 grids)))
          )))

(test extracting-a-grid
      (is (equalp '((22 13 17 11 0)
                      (8 2 23 4 24)
                      (21 9 14 16 7)
                      (6 10 3 18 5)
                      (1 12 20 15 19))
                    (extract-grid 0 *grids*))))
        
(test finding-first-winner
      (let ((grids (copy-grids *grids*)))
      (is (equalp 4512 (first-win (grid-results (car *numbers*) grids))))))

(test finding-last-winner
      (let ((grids (copy-grids *grids*)))
        (is (equalp 1924 (last-win (grid-results (car *numbers*) grids))))))

(test solve-a-find-first-winner-with-puzzle
      (is (equalp 64084 (solve 'a "../data/day04.txt"))))

(test solve-b-find-last-winner-with-puzzle
      (is (equalp 12833 (solve 'b "../data/day04.txt"))))
(run!)

