(require 'uiop)
(defparameter *max-number* 999999)

(defun increases (number-list)
  (let* ((limit (length number-list))
         (numbers (make-array limit
                              :initial-contents number-list))
         (prev *max-number*)
         (result 0))
    (loop for n across numbers do
          (progn
            (setf result (if (> n prev) (1+ result) result))
            (setf prev n)))
    result))

(defun number-increases-in-triplet-sums (number-list)
  (let* ((limit (- (length number-list) 3))
         (numbers (make-array (+ limit 3)
                              :initial-contents number-list))
         (prev *max-number*)
         (result 0))
    (loop for i from 0 to limit do
          (let* ((j (1+ i))
                 (k (1+ j))
                 (sum (+ (aref numbers i)
                         (aref numbers j)
                         (aref numbers k))))
            (progn
              (setf result (if (> sum prev) (1+ result) result))
              (setf prev sum))))
    result))

(defun read-integers (file-path)
  (mapcar #'parse-integer (uiop:read-file-lines file-path)))

(defun solve-puzzle-a (file-path)
    (increases (read-integers file-path)))

(defun solve-puzzle-b (file-path)
    (number-increases-in-triplet-sums (read-integers file-path)))


