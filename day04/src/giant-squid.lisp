(require 'uiop)
(require 'cl-ppcre)

(defun read-numbers (s)
  (with-input-from-string
        (line (concatenate 'string "(" 
                           (cl-ppcre:regex-replace-all "," s " ")
                          ")"))
        (read line)))

(defun read-numbers-from-file (file-path)
  (mapcar #'read-numbers (uiop:read-file-lines file-path)))

(defun create-grids (numbers)
  (labels ((create-grid-list (numbers)
                             (if (null numbers)
                               ()
                               (let ((grid-numbers (subseq (cdr numbers) 0 5))
                                     (remaining-numbers (subseq numbers 6)))
                                 (cons grid-numbers
                                       (create-grid-list remaining-numbers))))))
    (let* ((grids (create-grid-list numbers))
           (n (length grids)))
      (make-array (list n 5 5) :initial-contents grids))))

(defun grid-sum (grids i)
  (labels ((num-or-zero (n) (if (null n) 0 n)))
    (apply #'+ (loop for j below 5 collect
                   (apply #'+ (loop for k below 5
                                    collect (num-or-zero (aref grids i j k))))))))

