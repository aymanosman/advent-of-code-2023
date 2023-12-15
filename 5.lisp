(defvar *example* "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(load "strings")
(load "functions")

(defun parse-numbers (string)
  (mapcar #'parse-integer (remove "" (split-string string #\space) :test 'equal)))

(defun read-seeds (input)
  (let* ((line (read-line input))
         (parts (split-string line #\:)))
    (parse-numbers (second parts))))

(defun apply-clause (clause arg)
  (destructuring-bind (dest source range) clause
    (when (<= source arg (1- (+ source range)))
      (+ arg (- dest source)))))

(assert (equal (apply-clause '(50 98 2) 10) nil))
(assert (equal (apply-clause '(50 98 2) 97) nil))
(assert (equal (apply-clause '(50 98 2) 98) 50))
(assert (equal (apply-clause '(50 98 2) 99) 51))
(assert (equal (apply-clause '(50 98 2) 100) nil))

(defun make-function (clauses)
  (cond
    ((null clauses)
     #'identity)
    (t
     (let ((g (make-function (cdr clauses))))
       (lambda (arg)
         (or (apply-clause (car clauses) arg)
             (funcall g arg)))))))

(defun read-chunks (input)
  (labels ((read-chunk (input)
             (let (acc)
               (loop
                 (let ((line (read-line input nil)))
                   (cond
                     ((null line)
                      (return (reverse acc)))
                     ((string= line "")
                      (if (null acc)
                          (return (read-chunk input))
                          (return (reverse acc))))
                     (t
                      (push line acc)))))))
           (parse-chunk (strings)
             (loop for line in (cdr strings)
                   collect (parse-numbers line))))
    (loop for chunk = (read-chunk input)
          while chunk
          collect (parse-chunk chunk))))

(defun read-functions (input)
  (mapcar #'make-function (read-chunks input)))

(defun read-function (input)
  (reduce #'compose (reverse (read-functions input))))

(defun answer-1 (input)
  (let ((seeds (read-seeds input))
        (location-function (read-function input)))
    (loop for seed in seeds
          minimize (funcall location-function seed))))

(assert (equal (answer-1 (make-string-input-stream *example*))
               35))

;; 323142486
(defun part-1 ()
  (with-open-file (input "input/5")
    (print (answer-1 input))))
