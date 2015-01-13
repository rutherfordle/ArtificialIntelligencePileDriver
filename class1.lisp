(defun myreverse (l)
	(if (null l) nil
		append (myreverse (rest l)) (list (first l))))

(defun cl ()
	(compile-file "class1.lisp")
	(load "class1.fasl"))

(defun squares (1)
	(mapcar #' (lamda (x) (* x x)) 1))
	
(defun flatten (l)
	(if l
		(if (atom (first l)) 
			(cons (first l) (flatten (rest l)))
				(append (flatten (first l))
					(flatten (rest l))))))
					
(defun seq (a b)
	(if (<= a b)
		(cons a (seq (1 + a)b))))
		
(defun primap (a)
	(member 0 (mapcar #'(lamda (x)(mod n x))(seq)))