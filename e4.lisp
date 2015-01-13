; Rutherford Le
; Blaise Albuquerque
; E4
; problems 7, 72, 76

; problem 7 fast
(defun e7()
  (do ((i 0 (1+ i))(n 0 (if (testprime i)(1+ n)n)))((= n 10001) (1- i))))
(defun range(n)
  (do ((i n (1- i))(lst nil (cons i lst)))((<= i 0) lst)))
(defun testprime(n)
  (cond ((null n) nil)((= n 1) nil)((= n 2) t)((= 0 (mod n 2)) nil)(t (let ((end (floor (sqrt n))))
  (do ((i 3 (+ i 2)))((>  i end) t)(if (= 0 (mod n i))
  (return-from testprime nil)))))))
  (time(print(e7)))
  
 ; problem 72
  (defun totient (n)
  (let ((pf (histogram (prime-factors n))))
    (apply #'*(mapcar (lambda (x)(let ((p (car x))(k (cadr x)))(* (expt p k) (- 1 (/ p))))) pf))))

(defun e72 (&optional (limit 1000000))
  (reduce #'+ (mapcar #'totient (range 2 limit))))

(defun prime-factors (n)
  (if (> n 1)(let ((limit (isqrt n)))
	(do ((i 2 (1+ i)))((> i limit) (list n))
	  (if (zerop (mod n i))(return-from prime-factors (cons i (prime-factors (/ n i)))))))))
	  
(defun range(n &optional end)
  (if (null end)(range 1 n)(do ((i end (1- i))(lst nil (cons i lst)))((< i n) lst))))
  
  (defun histogram (list)
  (let ((ht (make-hash-table))(res))
    (dolist (s list)(incf (gethash s ht 0)))(maphash (lambda (key value) (push (list key value) res)) ht)
    res))
	(time(print(e72)))
  
 ; problem 76
(defun phelper (n max)
  (let ((tmp (gethash (list n max) parthash)))
    (if tmp tmp (loop for i from 1 to max sum (phelper (- n i) (min i (- n i) max)) into p
            finally (return (setf (gethash (list n max) parthash) p))))))

(defun e76 ()
   (setq parthash (make-hash-table :test 'equal))
  (setf (gethash (list 0 0) parthash) 1)
  (setf (gethash (list 1 1) parthash) 1)
  (loop for n from 1 to 200
        do (phelper n n))
  (gethash (list 100 99) parthash))
  (time (print (e76)))
  