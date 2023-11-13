
(defun position-depth (commands)
  (labels ((position-depth-acc (result commands)
               (if (null commands)
                 result
                 (let ((postn (car result))
                       (depth (cdr result))
                       (command (caar commands))
                       (value (cdar commands))
                       (remain (cdr commands)))
                   (cond ((equalp 'forward command) 
                          (position-depth-acc (cons (+ postn value) depth) remain))
                         ((equalp 'down command)
                          (position-depth-acc (cons postn (+ depth value)) remain))
                         ((equalp 'up command) 
                          (position-depth-acc (cons postn (- depth value)) remain)))))))
    (position-depth-acc (cons 0 0) commands)))

(defun position-depth-aim (commands)
  (labels ((position-depth-aim-acc 
             (result commands)
             (if (null commands)
                 result
                 (let ((postn (car result))
                       (depth (cadr result))
                       (aim (cddr result))
                       (command (caar commands))
                       (value (cdar commands))
                       (remain (cdr commands)))
                   (cond ((equalp 'forward command)
                          (position-depth-aim-acc (cons (+ postn value) (cons (+ depth (* aim value)) aim)) remain))
                         ((equalp 'down command)
                          (position-depth-aim-acc (cons postn (cons depth (+ aim value))) remain))
                         ((equalp 'up command)
                          (position-depth-aim-acc (cons postn (cons depth (- aim value))) remain)))))))
    (position-depth-aim-acc (cons 0 (cons 0 0)) commands)))


(defun read-commands-from-file (file-path)
  (with-open-file (file file-path :direction :input)
    (loop for command = (read file nil nil)
          while command
          collect (cons command (read file nil nil)))))

(defun solve-a (file-path)
  (let ((result (position-depth (read-commands-from-file file-path))))
    (* (car result) (cdr result))))

(defun solve-b (file-path)
  (let ((result (position-depth-aim (read-commands-from-file file-path))))
    (* (car result) (car (cdr result)))))
