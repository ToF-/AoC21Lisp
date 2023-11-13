(defun increases (number-list)
  (let* ((limit (length number-list))
         (numbers (make-array
                    limit
                    :initial-contents number-list)))
    (progn
      (setf track 999999)
      (setf acc 0)
      (loop for i from 0 to (1- limit) do
            (progn
              (setf n (aref numbers i))
              (setf acc (if (> n track) (1+ acc) acc))
              (setf track n)))
      acc)))

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
    (increases (read-integers file-path)))

(defun solve-puzzle-b (file-path)
    (number-increases-in-triplet-sums (read-integers file-path)))


