(require 'uiop)

(defun read-numbers (s)
  (read (concatenate 'string "(" s ")")))

(defun read-numbers-from-file (file-path)
  (mapcar #'read-numbers (uiop:read-file-lines file-path)))

