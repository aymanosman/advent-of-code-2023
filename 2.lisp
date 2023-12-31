(load "strings")

(defun parse-game-from-string (string)
  (flet ((parse-game-set (string)
           (mapcar (lambda (string)
                     (let* ((parts (split-string (string-trim '(#\Space) string) #\Space))
                            (color (second parts))
                            (number (first parts)))
                       (list (cond
                               ((string= color "red") :red)
                               ((string= color "green") :green)
                               ((string= color "blue") :blue))
                             (parse-integer number))))
                   (split-string string #\,))))
    (let ((parts (split-string string #\:)))
      (list :id (parse-integer (second (split-string (first parts) #\Space)))
            :sets (mapcar #'parse-game-set (split-string (second parts) #\;))))))

(assert (equal (parse-game-from-string "Game 12: 7 red, 8 blue; 6 blue, 6 red, 2 green; 6 blue, 4 green")
               '(:id 12
                 :sets (((:red 7) (:blue 8))
                        ((:blue 6) (:red 6) (:green 2))
                        ((:blue 6) (:green 4))))))

(defun game-id (game)
  (getf game :id))

(defun game-sets (game)
  (getf game :sets))

(defun game-set-totals (set)
  (flet ((sum (color)
           (loop for element in set
                 when (eql color (car element))
                   sum (second element))))
    (list :red (sum :red)
          :green (sum :green)
          :blue (sum :blue))))

(defun game-set-possible-p (set configuration)
  (let ((totals (game-set-totals set)))
    (and (<= (getf totals :red) (getf configuration :red))
         (<= (getf totals :green) (getf configuration :green))
         (<= (getf totals :blue) (getf configuration :blue)))))

(assert (equal (game-set-possible-p '((:blue 8)) '(:red 0 :green 0 :blue 8))
               t))

(assert (equal (game-set-possible-p '((:blue 9)) '(:red 0 :green 0 :blue 8))
               nil))

(defun game-possible-p (game configuration)
  (every (lambda (set)
           (game-set-possible-p set configuration))
         (game-sets game)))

(defun answer-1 (input)
  (loop for line = (read-line input nil)
        while line
        for game = (parse-game-from-string line)
        when (game-possible-p game '(:red 12 :green 13 :blue 14))
          sum (game-id game)))

;; 2679
(defun part-1 ()
  (with-open-file (input "input/2")
    (print (answer-1 input))))

(defun game-power (game)
  (flet ((max-color (color set)
           (loop for element in set
                 when (eql color (car element))
                   maximize (second element))))
    (multiple-value-bind (red green blue)
        (loop for set in (game-sets game)
              maximize (max-color :red set) into red
              maximize (max-color :green set) into green
              maximize (max-color :blue set) into blue
              finally (return (values red green blue)))
      (* red green blue))))

(assert (equal (game-power (parse-game-from-string "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
               48))

(defun answer-2 (input)
  (loop for line = (read-line input nil)
        while line
        for game = (parse-game-from-string line)
        sum (game-power game)))

;; 77607
(defun part-2 ()
  (with-open-file (input "input/2")
    (print (answer-2 input))))
