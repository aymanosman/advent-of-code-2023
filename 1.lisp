(defun char-number (char)
  (- (char-code char) (char-code #\0)))

(defun first-digit (line)
  (char-number (find-if #'digit-char-p line)))

(defun last-digit (line)
  (char-number (find-if #'digit-char-p line :from-end t)))

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

(defun answer-1 (input)
  (loop for line = (read-line input nil)
        while line
        sum (make-number line)))

;; 55172
(defun part-1 ()
  (with-open-file (input "input/1")
    (print (answer-1 input))))

;;; Part 2

(defvar *digits* '(0 1 2 3 4 5 6 7 8 9))

(defun match (seq1 seq2 start)
  (equal seq1 (subseq seq2 start (min (length seq2)
                                      (+ (length seq1) start)))))

(defun match-digit (line start)
  (dolist (digit *digits*)
    (when (or (match (format nil "~A" digit) line start)
              (match (format nil "~R" digit) line start))
      (return-from match-digit digit))))

(assert (equal (match-digit "xone" 0) nil))

(assert (equal (match-digit "xone" 1) 1))

(assert (equal (match-digit "xone" 2) nil))

(defun parse-digits (line &optional (start 0))
  (cond
    ((> start (1- (length line)))
     nil)
    (t
     (let ((match (match-digit line start)))
       (if match
           (cons match (parse-digits line (1+ start)))
           (parse-digits line (1+ start)))))))

(assert (equal (parse-digits "abcone2threexyz")
               '(1 2 3)))

(defun make-number-2 (line)
  (let ((digits (parse-digits line)))
    (+ (* (first digits) 10)
       (car (last digits)))))

(assert (equal (make-number-2 "two1nine")
               29))
(assert (equal (make-number-2 "eightwothree")
               83))
(assert (equal (make-number-2 "4nineeightseven2")
               42))
(assert (equal (make-number-2 "7pqrstsixteen")
               76))
(assert (equal (make-number-2 "oneight")
               18))

(defun answer-2 (input)
  (loop for line = (read-line input nil)
        while line
        sum (make-number-2 line)))

;; 54925
(defun part-2 ()
  (with-open-file (file "input/1")
    (print (answer-2 file))))
