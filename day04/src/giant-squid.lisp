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
                               (let* ((grid-numbers (subseq (cdr numbers) 0 5))
                                      (remaining-numbers (subseq numbers 6))
                                ) 

                                 (cons grid-numbers
                                       (create-grid-list remaining-numbers))))))
    (let* ((grids (create-grid-list numbers))
           (n (length grids)))
      (make-array (list n 5 5) :initial-contents grids))))

(defun grid-sum (i grids)
  (labels ((num-or-zero (n) (if (null n) 0 n)))
    (apply #'+ (loop for j below 5 collect
                   (apply #'+ (loop for k below 5
                                    collect (num-or-zero (aref grids i j k))))))))

(defun mark-number (n grids)
  (let ((max-grid (car (array-dimensions grids))))
    (loop for i below max-grid
          do (loop for j below 5
                   do (loop for k below 5 do
                            (setf (aref grids i j k)
                                  (if (equalp (aref grids i j k) n)
                                    nil
                                    (aref grids i j k))))))))

(defun extract-grid (i grids)
  (loop for j below 5
            collect (loop for k below 5
                          collect (aref grids i j k))))

(defun winning-gridp (i grids)
  (let ((grid (extract-grid i grids)))
    (labels ((winningp (l)
                       (cond ((null l) nil)
                             ((equalp '(nil nil nil nil nil) (car l)) t)
                             (t (winningp (cdr l)))))
             (transpose (l) (apply #'mapcar #'list l))
             )
      (or (winningp grid) (winningp (transpose grid))))))

(defun erase-grid (i grids)
  (loop for j below 5
        do (loop for k below 5
                 do (setf (aref grids i j k) -1))))

(defun grid-results (numbers grids)
  (if (null numbers)
    nil
    (let ((result nil)
          (n (car numbers)))
      (progn
        (setq result nil)
        (setq n (car numbers))
        (mark-number n grids)
        (loop for i below (car (array-dimensions grids))
              do (if (winning-gridp i grids)
                   (progn 
                     (setq result (* n (grid-sum i grids)))
                     (erase-grid i grids))
                   ()))
        (cons result (grid-results (cdr numbers) grids))))))

(defun first-win (results)
  (cond ((null results) nil)
        ((equalp nil (car results)) (first-win (cdr results)))
        (t (car results)))) 

(defun last-win (results)
  (labels ((last-win-track (track results)
                           (cond ((null results) track)
                                 ((equalp nil (car results)) (last-win-track track (cdr results)))
                                 (t (last-win-track (car results) (cdr results))))))
    (last-win-track nil results)))

(defun read-strings (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun solve-a (filepath)
  (let* ((puzzle (read-numbers-from-file filepath))
         (numbers (car puzzle))
         (grids (create-grids (cdr puzzle))))
    (first-win (grid-results numbers grids))))

(defun solve-b (filepath)
  (let* ((puzzle (read-numbers-from-file filepath))
         (numbers (car puzzle))
         (grids (create-grids (cdr puzzle))))
    (last-win (grid-results numbers grids))))
