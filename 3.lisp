(defvar *example* "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defun parse-regions (stream)
  "Return two values number-regions, symbol-regions"
  (let ((i 0)
        (j 0)
        (number '())
        (number-region '())
        (numbers '())
        (symbols '()))
    (labels ((start-number (char)
               (assert (null number))
               (continue-number char))
             (continue-number (char)
               (push char number)
               (push (cons i j) number-region))
             (finish-number ()
               (when number
                 (push (cons (parse-integer (coerce (reverse number) 'string))
                             (reverse number-region))
                       numbers))
               (setf number nil
                     number-region nil))
             (symbol-region (i j)
               (flet ((row (i j)
                        (list (cons i (1- j))
                              (cons i j)
                              (cons i (1+ j)))))
                 (append (row (1- i) j)
                         (row i j)
                         (row (1+ i) j)))))
      (loop
        (let ((char (read-char stream nil)))
          (if char
              (cond
                ((char= char #\Newline)
                 (finish-number)
                 (incf i)
                 (setf j 0))
                ((char= char #\.)
                 (finish-number)
                 (incf j))
                ((digit-char-p char)
                 (if number
                     (continue-number char)
                     (start-number char))
                 (incf j))
                (t ;; symbol
                 (finish-number)
                 (push (cons char (symbol-region i j)) symbols)
                 (incf j)))
              (return)))))
    (values (reverse numbers)
            (reverse symbols))))

(defun region-union (left right)
  (union left right :test #'equal))

(defun region-intersection (left right)
  (intersection left right :test #'equal))

(defun answer-1 (input)
  (multiple-value-bind (numbers symbols) (parse-regions input)
    (let ((symbol (cons #\* (reduce #'region-union (mapcar #'cdr symbols)))))
      (loop for number in numbers
            when (region-intersection (cdr number) (cdr symbol))
              sum (car number)))))

(assert (equal (answer-1 (make-string-input-stream *example*))
               4361))

;; 537732
(defun part-1 ()
  (with-open-file (input "input/3")
    (print (answer-1 input))))
