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

(defun make-function (clauses)
  clauses)

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

(defun function-declarations (seed-var)
  `(declare (optimize (speed 3) (safety 0) (debug 0))
            (type fixnum ,seed-var)))

(defun build-function (seed-var clauses &optional reverse-p)
  (labels ((build-clause (clause)
             (destructuring-bind (dest source range) clause
               (multiple-value-bind (dest source) (if reverse-p
                                                      (values source dest)
                                                      (values dest source))
                 `((<= ,source ,seed-var ,(1- (+ source range)))
                   (+ ,seed-var ,(- dest source)))))))
    `(lambda (,seed-var)
       (cond
         ,@(mapcar #'build-clause clauses)
         (t ,seed-var)))))

(progn
  (assert (equal (build-function 'seed '((50 98 2)))
                 `(lambda (seed)
                    (cond
                      ((<= 98 seed 99)
                       (+ seed -48))
                      (t
                       seed)))))
  (assert (equal (build-function 'seed '((50 98 2)) t)
                 `(lambda (seed)
                    (cond
                      ((<= 50 seed 51)
                       (+ seed 48))
                      (t
                       seed))))))

(defun compile-function (clauses reverse-p)
  (compile nil (build-function (gensym "SEED") clauses reverse-p)))

(defun compile-functions (functions &optional reverse-p)
  (reduce #'compose
          (mapcar (lambda (function)
                    (compile-function function reverse-p))
                  (cond (reverse-p functions)
                        (t (reverse functions))))))

(defun read-function (input &optional reverse-p)
  (compile-functions (read-functions input) reverse-p))

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


;; Part 2

(let* ((funs (list (make-function '((50 98 2)
                                    (52 50 48)))
                   (make-function '((0 15 37)
                                    (37 52 2)
                                    (39 0 15)))))
       (forward (compile-functions funs))
       (backward (compile-functions funs t)))
  (loop for i below 100
        do (assert (equal (funcall backward (funcall forward 2))
                          2))))

(defun compile-range-function (seed-var seeds)
  (flet ((compile-clause (cons)
           `(<= ,(car cons) ,seed-var ,(1- (+ (car cons) (cdr cons))))))
    `(lambda (,seed-var)
       (or ,@(mapcar #'compile-clause (loop for (start range) on seeds by #'cddr
                                            collect (cons start range)))))))

(assert (equal (compile-range-function 'seed '(79 14 55 13))
               `(lambda (seed)
                  (or (<= 79 seed 92)
                      (<= 55 seed 67)))))

(defun make-range-function (seeds)
  (compile nil (compile-range-function (gensym "SEED") seeds)))

(defun answer-2 (input)
  (let* ((seeds (read-seeds input))
         (in-range-p (make-range-function seeds))
         (seed-function (read-function input t)))
    (loop for location from 0
          until (funcall in-range-p (funcall seed-function location))
          finally (return location))))

(assert (equal (answer-2 (make-string-input-stream *example*))
               46))

;; 79874951
(defun part-2 ()
  (with-open-file (input "input/5")
    (print (answer-2 input))))
