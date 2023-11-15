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
  (if (null numbers)
    ()
    (let ((grid-numbers (subseq (cdr numbers) 0 5))
          (remaining-numbers (subseq numbers 6)))
      (cons (make-array '(5 5) :initial-contents grid-numbers)
            (create-grids remaining-numbers)))))
