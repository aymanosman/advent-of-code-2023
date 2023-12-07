(defvar *example* "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(load "strings")

(defun read-numbers (string)
  (with-input-from-string (stream string)
    (loop for num = (read stream nil)
          while num
          collect num)))

(defun parse-card (line)
  (let* ((parts (split-string line #\:))
         (parts (split-string (second parts) #\|))
         (winning (read-numbers (first parts)))
         (have (read-numbers (second parts))))
    (list :winning winning
          :have have)))

(defun card-value (line)
  (let* ((card (parse-card line))
         (matches (intersection (getf card :winning) (getf card :have ))))
    (cond
      ((null matches)
       0)
      (t
       (expt 2 (1- (length matches)))))))

(defun answer-1 (input)
  (loop for line = (read-line input nil)
        while line
        sum (card-value line)))

(assert (equal (answer-1 (make-string-input-stream *example*))
               13))

;; 23441
(defun part-1 ()
  (with-open-file (input "input/4")
    (print (answer-1 input))))
