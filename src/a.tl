(setf a 5
(print (+ a 10))
(print "hi")
(defun fact (n) (cond ((= n 0) 1) (T (* n (fact (- n 1))))))
(print (fact 6))
(setf a (fact 7))
(print (incf a))
