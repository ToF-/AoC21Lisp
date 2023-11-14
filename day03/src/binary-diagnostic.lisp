
(defun string-to-bit-vector (s)
  (coerce (mapcar #'digit-char-p (coerce s 'list)) 'bit-vector))

(defun string-list-to-bit-vector-list (ss)
  (mapcar #'string-to-bit-vector ss))

(defun bit-vector-to-list (v)
  (coerce v 'list))

(defun nth-common-bit (pos vectors criterion)
  (let* ((bits (mapcar (lambda (vec) (aref vec pos)) vectors))
         (ones (apply #'+ bits))
         (zeros (- (length vectors) ones)))
    (cond ((> ones zeros) (if (eql 1 criterion) 1 0))
          ((< ones zeros) (if (eql 1 criterion) 0 1))
          (t criterion))))

(defun common-bits (vectors)
  (loop for n from 0 to (- (array-dimension (car vectors) 0) 1)
        collect (nth-common-bit n vectors 1)))

(defun common-bits-narrow (vectors criterion)
  (labels ((common-bit (vectors pos criterion)
                       (if (eql 1 (length vectors))
                         (bit-vector-to-list (car vectors))
                         (let* ((select (nth-common-bit pos vectors criterion))
                                (remain (remove-if-not (lambda (v) (eql select (aref v pos))) vectors)))
                           (common-bit remain (1+ pos) criterion)))))
    (common-bit vectors 0 criterion)))

(defun transpose (l)
  (apply #'mapcar #'list l))

(defun value (digits)
  (reduce (lambda (acc digit) (+ (* 2 acc) digit)) digits))

(defun binary-complement (bits)
  (mapcar (lambda (b) (- 1 b)) bits))

(defun epsilon-and-gamma (vectors)
  (let* ((epsilon (common-bits vectors))
         (gamma (binary-complement epsilon)))
    (cons (value epsilon) (value gamma))))

(defun oxigen-and-scrubber (vectors)
  (let* ((oxigen (common-bits-narrow vectors 1))
         (scrubber (common-bits-narrow vectors 0)))
    (cons (value oxigen) (value scrubber))))


(defun read-strings (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun solve-a (filename)
  (let ((result (epsilon-and-gamma
                  (string-list-to-bit-vector-list (read-strings filename)))))
    (* (car result) (cdr result))))

(defun solve-b (filename)
  (let ((result (oxigen-and-scrubber
                  (string-list-to-bit-vector-list (read-strings filename)))))
    (* (car result) (cdr result))))

