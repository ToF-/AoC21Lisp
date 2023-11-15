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

