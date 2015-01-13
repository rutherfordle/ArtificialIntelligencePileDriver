;Rutherford Le
;Blaise Albuquerque

;CMPS 140

;----------------------------------------------------

;p31 is-prime
(defun factorial (N)
	;Computes the factorial of N
	(if (= N 1) 1
		(* N (factorial (- N 1)))))
		
(defun is-prime (N)
	;((n-1)!+1)/n
	;if equation is equal to 0, then it's prime
	(if (= 0 N) (eq 0 1)
	(if (/= 1 n)
		(eq 0 (rem
			 (+ (factorial (- N 1)) 1)
			 N))
		(eq 1 1))))

;----------------------------------------------------

; p32
; Euclid's algorithm
; efficient method of finding GCD

(defun gcd (p q)
	(cond ((= p q) p)
		((< p q) (gcd p (- q p)))
    (t(gcd q (- p q)))))

;----------------------------------------------------

; p33
; Determine whether two positive integer numbers are coprime.
; Two numbers are coprime if their greatest common divisor equals 1.
; Example:
; * (coprime 35 64)
; T

(defun coprime (A B)
	(if (plusp A) 
		(if (plusp B) 
		;algorithm for coprime
		(eq 1 (gcd A B))
	)))
;----------------------------------------------------

; p34
(defun totient-phi (k)
	(trec k 1 0)
)

(defun trec (k n c)
;n is the increment
;c is the prime counter
	(if (< n k) ;obv n < k		
		(if (coprime n k)
			(progn 
			;groups the true statements
				(setf c (+ 1 c))
				(trec k (+ n 1) c))
		(trec k (+ n 1) c)) ;if false
	c))

;----------------------------------------------------

;p35
(defun prime-factors (K)
	(recur K 2 '()))
	
(defun recur (K N L)
	(if (/= 1 K)
		(if (is-prime N)
			(if (eq 0 (mod K N))
				(progn
					(setq L (append L (list N)))
					(recur (/ K N) N L)) ;divides k/n
			(recur k (+ N 1) L)) ;
		(recur k (+ N 1) L))
	L))
			
;----------------------------------------------------

;p36
(defun prime-factors-mult (k)
      (setq P (prime-factors k))
      (recurthis k 0 1 '() '() p))
     
(defun recurthis (k n i li a p)
      (if (null (nth n p))
              a
              (if (eq (nth (+ n 1) p) (nth n p)) 
                      (progn
                              (setq i (+ i 1)) 
                              (recurthis k (+ n 1) i li a p))
                      (progn(setq li (append li (list(nth n p))))
                              (setq li (append li (list i)))
                              (setq a (append a (list li)))
                              (setq i 1)
                              (recurthis k (+ n 1) i '() a p)))
      ))
;----------------------------------------------------

;p39
(defun p39 (A B)
	(if (< A B)
		(recpm A B '())	; A < B
	(recpm B A '()))) 	; B < A	
	
(defun recpm (A B L)
	(if (<=  A B)
		(if (is-prime A)
			(progn
			(setq L (append L (list A)))
			(recpm (+ A 1) B L)
			
			)
		(recpm (+ A 1) B L))
	L))
	
;----------------------------------------------------

;p40
(defun goldbach  (K)
	(solution 3 K))

(defun solution (A K)
(if (eq 2 K) nil
	(if (eq 0 (mod k 2))								;checks if K is even
		(if (is-prime A)								;checks if A is prime
			(progn 
				(setq B (- k A)) 						;subs K-A = B
				(if (is-prime B)						;checks if B is prime.
					(progn								;SUCESSS, A and B are prime
						(append (list A) (list B)))
				(solution (+ A 1) K)))				;if B not prime, re-iterate with A+1
		(solution (+ A 1) K))						;if A not prime, re-iterate with A+1
	)	)											;if K isn't even, NIL
)
	
;----------------------------------------------------

;p41
(defun goldbach-list (A B &optional (C 0))
	(if (< A 3) (setq A 3))
	(if (< A B) (gbl A B C) (gbl B A C)))
	
(defun gbl (A B C)
	(if (<= A B)
		  (if (eq 0 (mod A 2))        ;checks if req even met
				  (progn
						  (setq L (goldbach A))        ;sets L equal to the goldback number
						  (setf gfirst (nth 0 L))
						  (setf glast (nth 1 L))
						  (if (< C gfirst) ;C < A
							  (progn
							  (format t "~D = ~D + ~D~%" A gfirst glast)))
						  (gbl (+ A 1) B C))                ;
		  (gbl (+ A 1) B C))))
;----------------------------------------------------