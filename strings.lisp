(defun split-string (string separator &optional (start 0))
  (cond
    ((< start (length string))
     (let ((delim (position separator string :start start)))
       (if delim
           (cons (subseq string start delim)
                 (split-string string separator (1+ delim)))
           (list (subseq string start)))))
    (t
     nil)))
