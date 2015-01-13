(defun zeros (n)
 (if (> n 0) (cons 0 (zeros (- n 1)))))

(defun makegrid (n) ; this function will generate a grid for an n-player game
 (mkgrd n (- n 1)))

(defun mkgrd (n m)
 (if (> n 0) (cons (zeros m) (mkgrd (- n 1) m))))
(defun incr-rand (move amt)
 (let ((x (1+ (random (length move)))))
 (append (subseq move 0 (1- x)) (cons (+ (nth (- x 1) move) amt) (nthcdr x move)))))

(defun agent-play-random (grid turn) ; this agent will bid each point randomly
 (random-move grid turn (zeros (- (length grid) 1))))

(defun random-move (grid turn move)
 (if (> turn 0)
 (random-move grid (- turn 1) (incr-rand move 1))
 move))

(defun agent-all-first (grid turn) ; this agent puts all points into the first pile
 (cons turn (zeros (- (length grid) 2))))

(defun agent-all-rand (grid turn) ; this agent puts all points into a randomly chosen pile each turn
 (incr-rand (zeros (- (length grid) 1)) turn))