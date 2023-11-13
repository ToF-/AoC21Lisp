(ql:quickload "fiveam")
(in-package :fiveam)
(def-suite* live)

(load "./src/live.lisp")

(defparameter *sample* '((forward . 5) (down . 5) (forward . 8) (up . 3) (down . 8) (forward . 2)))

(test position-depth-after-commands-change
      (is (equalp (cons 15 10) 
                  (position-depth *sample*))))

(test solve-a-tells-the-position-after-all-commands
      (is (eql 1694130 (solve-a "../../data/day02.txt"))))

(test position-depth-aim-after-commands-change
      (is (equalp (cons 15 (cons 60 10))
                  (position-depth-aim *sample*))))

(test solve-b-tells-the-position-after-all-commands
      (is (eql 1698850445 (solve-b "../../data/day02.txt"))))

(run!)
