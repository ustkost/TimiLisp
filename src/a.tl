(defun len (lst) (cond ((null lst) 0) (T (+ 1 (len (cdr lst))))))
(print (len (list 1 2 3))) "3"

(defun rev (lst) (and lst (append (rev (cdr lst)) (list (car lst))))) "Reversing list"
(print (rev (list () 1 1 (list 1 4 8))))

(setf x (quote (cond ((> n 0) (* (+ (setf n (- n 1)) 1) (eval x))) (T 1)))) "Factorial"
(setf n 5)
(print (eval x)) "Returns 120"

(print (/ 720 6 5 3 2 1)) "Arbitrary number of arguments!"

(defun sqrt1 (n l r) (let ((m (/ (+ l r) 2)) (v (* m m))) (cond ((>= l (- r 1)) r) ((< v n) (sqrt1 n m r)) (T (sqrt1 n l m)))))

(defun sqrt (n) (sqrt1 n 0 n))

(print (sqrt 100)) "print integer square root"
