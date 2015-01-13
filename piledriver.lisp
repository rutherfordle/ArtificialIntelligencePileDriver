(defun hi ()
(Compile-file "piledriver.lisp")
(load "piledriver.lisp"))

(defun zeros (n)
	(if (> n 0) (cons 0 (zeros (- n 1)))))
	
(defun makegrid (n)
	(mkgrd n (- n 1)))
	
(defun mkgrd (n m)
	(if (> n 0) (cons (zeros m) (mkgrd (- n 1) m))))

(defun incr-rand (move amt)
	(let ((x (1+ (random (length move)))))
		(append (subseq move 0 (1- x)) (cons (+ (nth (- x 1) move) amt) (nthcdr x move)))))
	
(defun agent-play-random (grid turn)
	;(setf i 0)
	(setf agent(zeros (-(length grid)1)))
   ;(loop for i from 1 to (length agent) do
  
  ; (cons 1 zeros(-(length grid)1))))
  (loop for i from 0 to (- 1 turn) do
  (format t "this")
 (setf (nth i agent) 1))(setf turn 0)agent)

  ;(random-move grid turn (zeros (- (length grid) 1))))
;(append (subseq agent 0 (1- i)) (cons (+ (nth (- i 1) agent) 1) (nthcdr i agent))))(- turn 1)))

(defun random-move (grid turn move)
	(if (> turn 0)
		(random-move grid (- turn 1) (incr-rand move 1))
		move))
    
(defun agent-all-first (grid turn move)
    (setf curr(currList grid turn move)))
(defun currList (grid turn move)
	(let ((x '()))
		(loop for i from 0 to (- (length (first move)) 1) do
			(setf x (append x (list (score move i)))))
		x))

(defun score (move pile)
	(let ((largest 0) (scores '()))
		(loop for i from 0 to (- (length move) 1) do
			(setf scores (append scores (list (nth pile (nth i move))))))
		(setf largest (reduce 'max scores))
		(if (/= (count largest scores) 1)
			'()
			(position largest scores))))
			
	; (loop for i from 0 to (- (length grid) 1) do	;outer
		; (setf count 0)
		; (loop for j from 0 to (- (length grid) 2) do ;inner
			; (setq val (nth j (nth i grid)))
				
		; )
		
	; )
	
(defun agent-all-rand (grid turn)
    (incr-rand (zeros (- (length grid) 1)) turn))

(defun any-negative (somelist)
	(if (< (first somelist) 0)
		t
		(if (= (length somelist) 1) 
			nil 
			(any-negative (rest somelist)))))

(defun all-nums (somelist)
	(if (= (length somelist) 1)
		(numberp (first somelist))
		(and (numberp (first somelist)) (all-nums (rest somelist)))))

(defun validate (move players i)
	(cond
		((not move) nil)
		((not (listp move)) nil)
		((/= (length move) (1- (length players))) nil)
		((not (all-nums move)) nil)
		((any-negative move) nil)
		((/= (eval (cons '+ move)) i) nil)
		(t)))
		
(defun rearrange (grid i)
	(if (= i 0)
		grid
		(cons (nth i grid) (append (subseq grid 0 i) (subseq grid (+ i 1))))))
		
(defun winner-list (grid)
	(let ((winners '()))
		(loop for i from 0 to (- (length (first grid)) 1) do
			(setf winners (append winners (list (winner grid i)))))
		winners))
		
(defun winner (grid pile)
	(let ((largest 0) (scores '()))
		(loop for i from 0 to (- (length grid) 1) do
			(setf scores (append scores (list (nth pile (nth i grid))))))
		(setf largest (reduce 'max scores))
		(if (/= (count largest scores) 1)
			'()
			(position largest scores))))
		
(defun calculate-scores (grid players winners)
	(let ((scores (zeros (- (length players) 1))))
		(dotimes (i (- (length players) 1))
			(if (and (nth i scores) (nth i winners))
				(setf (nth i scores) (+ (nth i scores) (sum-all-but grid i (nth i winners))))
				'()))
		scores))
	
(defun sum-all-but (grid pile player)
	(let ((sum 0))
		(loop for i from 0 to (- (length grid) 1) do
			(if (/= i player)
				(setf sum (+ sum (nth pile (nth i grid))))
				0))
		sum))
	
(defun sum-winner-scores (winners scores)
	(let ((finals (zeros (1+ (length winners)))))
		(dotimes (i (length winners))
			(if (nth i winners)
				(setf (nth (nth i winners) finals) (+ (nth (nth i winners) finals) (nth i scores)))))
		finals))
	
(defun run ()
	(defvar players)
	(setf players (list 'agent-all-first 'agent-all-rand 'agent-play-random))
	
	(defvar disqualified)
	(setf disqualified '())
	
	(defvar grid)
	(setf grid (makegrid (length players)))
	(loop for i from 1 to (length players) do
		(let ((newgrid (makegrid (length players))))
			(format t "turn ~D" i)(terpri)
			(dotimes (j (length players))
				(if (member j disqualified)
					'()
					(progn
						(setf move (funcall (nth j players) (rearrange grid j) i))
						(format t "player ~D moves ~S" j move)(terpri)
						(if (validate move players i)
							(setf (nth j newgrid) (mapcar '+ (nth j grid) move))
							(progn (format t "player ~D disqualified!" j) (setf disqualified (cons j disqualified)))))))
			(setf grid newgrid))
		(format t "board: ~S~%" grid)(terpri))
		
	(defvar winners)
	(setf winners (winner-list grid))
	(defvar scores)
	(setf scores (calculate-scores grid players winners))
	(setf scores (sum-winner-scores winners scores))
	(dotimes (i (length players))
		(format t "player ~D: ~D points" i (nth i scores))
		(terpri)))
