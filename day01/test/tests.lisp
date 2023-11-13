(ql:quickload "fiveam")
(in-package :fiveam)
(def-suite* sonar-sweep)

(load "./src/sonar-sweep.lisp")

(test number-of-increases-in-depth
      (is (eql 7 (increases '(199 200 208 210 200 207 240 269 260 263)))))

(test solve-puzzle-a-tells-the-number-of-increases-in-depth-in-the-input-file
      (is (eql 1791 (solve-puzzle-a "../data/day01.txt"))))

(test triplet-increases-tells-the-number-of-increases-in-triplet-sums
      (is (eql 5 (number-increases-in-triplet-sums
                   '(199 200 208 210 200 207 240 269 260 263)))))

(test solve-puzzle-b-tells-the-number-of-increases-in-depth-triplet-sums-in-the-input-file
      (is (eql 1822 (solve-puzzle-b "../data/day01.txt"))))
(run!)
