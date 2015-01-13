(defun hi()
(Compile-file "test.lisp")
(load "test.lisp"))

(defun filter(num maxVal)
	
	(setf grabLess '())
	(loop for i from 0 to (-(length num)1) do
	(if (< (nth i num) maxVal)
	(setf grabLess (append grabLess (list (nth i num))))))
	grabLess
)

(defun oddones (num)
	(setf sum 0)
	(loop for i from 0 to (- (length num) 1) do
	(if (= (nth i num) 1)
	(setf sum(+ sum 1))
	))
	(if (/= (mod sum 2) 0)
	(setf val t))
) 

(defun mycons (val num)
	(setf num (append (list val) num))num)

(defun myreverse (num)
	(setf numb '())
	(loop for i from 0 to (- (length num) 1) do
	(setf numb(append (list (nth i num)) numb)))numb)

(defun longest (l1 l2)
	(setf a(loop for i in l1 count i))
	(setf b(loop for j in l2 count j))
	(if (> a b) l1 (if (> b a) l2 nil)))

(defun sequences (a b)
	(setf c '())
	(loop for i from a to b do
	(setf c(cons i c)))
	(setf c(reverse c))c)
	
	(defun flatten (l)
	(if l
		(if (atom (first l)) 
			(cons (first l) (flatten (rest l)))
				(append (flatten (first l))
					(flatten (rest l))))))
					
(defun Summer (l1)
       (setq return '(0 0 0))
       (loop for i from 0 to (- (length l1) 1) do
               (if (= (length (nth i l1)) 3)
                       (setq return (mapcar '+ return (nth i l1)))))
       return
)


(defun maxbit (l)
(+ i 1)
(if (null l) first c)
(if (= (nth i l) 0)
(setq a i)
(progn(+ i 1)))
(progn(if (= (nth i l) 0) (maxbit (l))))
(progn(if (= (nth i l) 1)
	(setq c (append (subseq l 0 a) (nthcdr (+ a 1) l)))
	(progn(setq c (append (subseq c 0 i) (nthcdr (+ i 1) c))))
	)))

