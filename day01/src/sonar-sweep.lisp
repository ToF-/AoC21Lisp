(defun number-increases (depths)
  (labels ((number-increases-acc (n depths)
            (if (null depths)
              0
              (let ((depth (car depths))
                    (remain (cdr depths)))
                (+ (if (< n depth) 1 0)
                  (number-increases-acc depth remain))))))
    (number-increases-acc 999999 depths)))

(defun number-increases-in-triplet-sums (depths-list)
  (let* ((depths (make-array
                   (length depths-list)
                   :initial-contents depths-list))
         (limit (- (length depths) 3)))
    (progn (setf track 999999)
           (setf acc 0)
           (loop for i from 0 to (- (length depths) 3) do
                 (let ((sum (+ (aref depths i) (aref depths (1+ i)) (aref depths (1+ (1+ i))))))
                   (progn (setf acc (if (> sum track) (1+ acc) acc))
                          (setf track sum))))
           acc)))

(defun read-integers (file-path)
  (with-open-file (file file-path :direction :input)
    (loop for line = (read-line file nil)
          while line
          collect (parse-integer line))))

(defun solve-puzzle-a (file-path)
    (number-increases (read-integers file-path)))

(defun solve-puzzle-b (file-path)
    (number-increases-in-triplet-sums (read-integers file-path)))


