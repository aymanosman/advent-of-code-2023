(defun first-digit (line)
  (parse-integer
   (format nil "~A" (find-if #'digit-char-p line))))

(defun last-digit (line)
  (parse-integer
   (format nil "~A" (find-if #'digit-char-p line :from-end t))))

(assert (equal (first-digit "abc7de8fg")
               7))

(defun make-number (line)
  (+ (* (first-digit line) 10)
     (last-digit line)))

(progn
  (assert (equal (make-number "t7")
                 77))

  (assert (equal (make-number "8")
                 88))

  (assert (equal (make-number "xxx1a8y")
                 18)))

(defun answer (input)
  (loop for line = (read-line input nil)
        while line
        sum (make-number line)))

;; 55172
(defun main ()
  (with-open-file (file "input1")
    (print (answer file))))
