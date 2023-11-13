(defun number-increases (depths)
  (labels ((number-increases-acc (n depths)
            (if (null depths)
              0
              (let ((depth (car depths))
                    (remain (cdr depths)))
                (+ (if (< n depth) 1 0)
                  (number-increases-acc depth remain))))))
    (number-increases-acc 999999 depths)))

(defun number-increases-in-triplet-sums (depths)
  (labels ((number-increases-acc (acc n depths)
             (if (< (length depths) 3)
               acc
               (let* ((depth-a (car depths))
                      (depth-b (cadr depths))
                      (depth-c (caddr depths))
                      (remain (cdr depths))
                      (triplet-sum (+ depth-a depth-b depth-c)))
                 (number-increases-acc 
                   (+ acc (if (< n triplet-sum) 1 0))
                   triplet-sum
                   remain)))))
    (number-increases-acc 0 999999 depths)))

(defun read-integers (file-path)
  (with-open-file (file file-path :direction :input)
    (loop for line = (read-line file nil)
          while line
          collect (parse-integer line))))

(defun solve-puzzle-a (file-path)
    (number-increases (read-integers file-path)))

(defun solve-puzzle-b (file-path)
    (number-increases-in-triplet-sums (read-integers file-path)))


