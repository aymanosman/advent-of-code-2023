(defun compose (&rest functions)
  (cond
    ((null (cdr functions))
     (car functions))
    (t
     (let ((g (apply #'compose (cdr functions))))
       (lambda (arg)
         (funcall (car functions) (funcall g arg)))))))

(assert (equal (funcall (compose #'1+ #'1+) 1)
               3))

(assert (equal (funcall (compose (lambda (acc) (cons 1 acc))
                                 (lambda (acc) (cons 2 acc)))
                        '(3))
               '(1 2 3)))
