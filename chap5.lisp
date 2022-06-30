;; chap5

(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden. there is a well in front of you.))
			(attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *wizard-nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden. there is a well in front of you.))
			(attic (you are in the attic. there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			 (attic (living-room downstairs ladder))))

(defparameter *wizard-edges* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			 (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; chap6

(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

;; chap7

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

;; chap8

(defparameter *congestion-city-nodes* nil)

(defparameter *congestion-city-edges* nil)

(defparameter *visited-nodes* nil)

(defparameter *node-num* 30)

(defparameter *edge-num* 45)

(defparameter *worm-num* 3)

(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
			  collect (random-node))))
    (loop for n from 1 to *node-num*
	  collect (append (list n)
			  (cond ((eql n wumpus) '(wumpus))
				((within-two n wumpus edge-alist) '(blood!)))
			  (cond ((member n glow-worms)
				 '(glow-worm))
				((some (lambda (worm)
					 (within-one n worm edge-alist))
				       glow-worms)
				 '(lights!)))
			  (when (some #'cdr (cdr (assoc n edge-alist)))
			    '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city)
  )

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*))
		      n))
		(list node '?)))
	  (remove-duplicates
	   (append *visited-nodes*
		   (mapcan (lambda (node)
			     (mapcar #'car
				     (cdr (assoc node
						 *congestion-city-edges*))))
			   *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
		     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
	(handle-new-place edge pos charging)
	(princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
	  ((member 'wumpus node) (if charging
				     (princ "You found the Wumpus!")
				     (princ "You ran into the Wumpus")))
	  (charging (princ "You wasted your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))

;; chap9

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
	       (unless (gethash node visited)
		 (setf (gethash node visited) t)
		 (mapc (lambda (edge)
			 (traverse edge))
		       (gethash node edge-tab)))))
      (traverse node))
    visited))

(defparameter *player-health* nil)

(defparameter *player-agility* nil)

(defparameter *player-strength* nil)

(defparameter *monsters* nil)

(defparameter *monster-builders* nil)

(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda(m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))

(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "  ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
	     (princ (type-of m))
	     (princ "! "))
      (progn (princ "You hit the ")
	     (princ (type-of m))
	     (princ ", knocking off ")
	     (princ x)
	     (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

(defstruct (orc (:include monster)) (club-level (randval 8)))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))

(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off")
	     (princ x)
	     (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))

(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))

(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand cathces your leg with his whip, taking off 2 agility points! ")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength* 2)))))

;; chap10

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
		     :y (ash *height* -1)
		     :energy 1000
		     :dir 0
		     :genes (loop repeat 8
				   collecting (1+ (random 10))))))

(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1)
					  ((or (= dir 1) (= dir 5)) 0)
					  (t -1)))
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0)))
				 *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
	       (let ((xnu (- x (car genes))))
		 (if (< xnu 0)
		     0
		     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
		 8)))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
	    (genes (copy-list (animal-genes animal)))
	    (mutation (random 8)))
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0))
			     *animals*))
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
	below *height*
	do (progn (fresh-line)
		  (princ "|")
		  (loop for x
		        below *width*
			do (princ (cond ((some (lambda (animal)
						 (and (= (animal-x animal) x)
						      (= (animal-y animal) y)))
					       *animals*)
					 #\M)
					((gethash (cons x y) *plants*) #\*)
					(t #\space))))
		  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
			   below x
			 do (update-world)
			 if (zerop (mod i 1000))
			   do (princ #\.))
		   (update-world))
	       (evolution))))))

;; chap13

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
	 (loop (with-open-stream (stream (socket-accept socket))
		 (let* ((url (parse-url (read-line stream)))
			(path (car url))
			(header (get-header stream))
			(params (append (cdr url)
					(get-content-params stream header)))
			(*standard-output* stream))
		   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ "<html><form>What is your name?<input name='name' /></form></html>")
	    (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))

;; chap15

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		     collect (list (random *num-players*)
				   (1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
	do (progn (fresh-line)
		  (loop repeat (- *board-size* y)
			do (princ "  "))
		  (loop for x below *board-size*
			for hex = (aref board (+ x (* *board-size* y)))
			do (format t "~a-~a " (player-letter (first hex))
				   (second hex))))))

(defun game-tree (board player spare-dice first-move)
  (list player
	board
	(add-passing-move board
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
		  (game-tree (add-new-dice board player (1- spare-dice))
			     (mod (1+ player) *num-players*)
			     0
			     t))
	    moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (mapcan (lambda (src)
	      (when (eq (player src) cur-player)
		(mapcan (lambda (dst)
			  (when (and (not (eq (player dst) cur-player))
				     (> (dice src) (dice dst)))
			    (list
			     (list (list src dst)
				   (game-tree (board-attack board cur-player
							    src dst (dice src))
					      cur-player
					      (+ spare-dice (dice dst))
					      nil)))))
			(neighbors src))))
	    (loop for n below *board-hexnum*
		  collect n))))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
		     for hex across board
		     collect (cond ((eq pos src) (list player 1))
				   ((eq pos dst) (list player (1- dice)))
				   (t hex)))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
	  for n from 1
	  do (let ((action (car move)))
	       (fresh-line)
	       (format t "~a. " n)
	       (if action
		   (format t "~a -> ~a" (car action) (cadr action))
		   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
		      collect (car hex)))
	 (totals (mapcar (lambda (player)
			   (cons player (count player tally)))
			 (remove-duplicates tally)))
	 (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
	    (remove-if (lambda (x)
			 (not (eq (cdr x) best)))
		       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w))))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
	(apply (if (eq (car tree) player)
		   #'max
		   #'min)
	       (get-ratings tree player))
	(let ((w (winners (cadr tree))))
	  (if (member player w)
	      (/ 1 (length w))
	      0)))))

(defun get-ratings (tree player)
  (mapcar (lambda (move)
	    (rate-position (cadr move) player))
	  (caddr tree)))

(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
	((zerop (car tree)) (play-vs-computer (handle-human tree)))
	(t (play-vs-computer (handle-computer tree)))))

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
	(setf (gethash pos previous) (funcall old-neighbors pos)))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
	(setf (gethash rest previous) (apply old-game-tree rest)))))

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
	(setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
	  (setf (gethash tree tab)
		(funcall old-rate-position tree player))))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
	     (cond ((zerop n) (append (reverse acc) lst))
		   ((null lst) (reverse acc))
		   (t (let ((cur-player (caar lst))
			    (cur-dice (cadar lst)))
			(if (and (eq cur-player player)
				 (< cur-dice *max-dice*))
			    (f (cdr lst)
			       (1- n)
			       (cons (list cur-player (1+ cur-dice)) acc))
			    (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

;; chap17

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
		     (list ,@(mapcar (lambda (x)
				       `(cons ',(car x) ,(cdr x)))
				     (pairs atts)))
		     nil)
	  ,@body
	  (print-tag ',name nil t)))

(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	       (if ,g
		   (let ((head (car ,g))
			 (tail (cdr ,g)))
		     ,yes)
		   ,no))))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   "xmlns:xlink" "http://www.w3.org/1999/xlink" height ,height width ,width)
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))

(defun svg-style (color)
  (format nil
	  "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
	  (append color
		  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
		  cy (cdr center)
		  r radius
		  style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
			       "~{~a,~a ~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))

(defun have (object)
  (member object (cdr (inventory))))

(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
	   (eq subject 'chain)
	   (eq object 'bucket)
	   (have 'chain)
	   (have 'bucket)
	   (not *chain-welded*))
      (progn (setf *chain-welded* t)
	     '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))

(defparameter *bucket-filled* nil)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
	   (eq subject 'bucket)
	   (eq object 'well)
	   (have 'bucket)
	   *chain-welded*)
      (progn (setf *bucket-filled* 't)
	     '(the bucket is now full of water))
      '(you cannot dunk like that.)))

(pushnew 'dunk *allowed-commands*)
(pushnew 'weld *allowed-commands*)

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
	    (if (and (eq *location* ',place)
		     (eq subject ',subj)
		     (eq object ',obj)
		     (have ',subj))
		,@body
		'(i cant ,command like that.)))
	  (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
      (progn (setf *chain-welded* 't)
	     '(the chain is now securely welded to the bucket.))
      '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
      (progn (setf *bucket-filled* 't)
	     '(the bucket is now full of water))
      '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
	((have 'frog) '(the wizard awakens and sees that you stole his frog.
			he is so upset he banishes you to the
			netherworlds- you lose! the end.))
	(t '(the wizard awakens from his slumber and greets you warmly.
	     he hands you the magic low-carb donut- you win! the end.))))

;; chap18

(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced nil)
	   (,value nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
	     (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst
	  (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
	  (cons (funcall fun (lazy-car lst))
		(lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
	     (if (lazy-null lst-cur)
		 (force (lazy-mapcan fun (lazy-cdr lst)))
		 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
	  x
	  (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
		       (game-tree (add-new-dice board player
						(1- spare-dice))
				  (mod (1+ player) *num-players*)
				  0
				  t))
		 moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
	   (lazy-mapcan
	    (lambda (dst)
	      (if (and (not (eq (player dst)
				cur-player))
		       (> (dice src) (dice dst)))
		  (make-lazy
		   (list (list (list src dst)
			       (game-tree (board-attack board
							cur-player
							src
							dst
							(dice src))
					  cur-player
					  (+ spare-dice (dice dst))
					  nil))))
		  (lazy-nil)))
	    (make-lazy (neighbors src)))
	   (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
		      collect n)))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
	       (unless (lazy-null moves)
		 (let* ((move (lazy-car moves))
			(action (car move)))
		   (fresh-line)
		   (format t "~a. " n)
		   (if action
		       (format t "~a -> ~a" (car action) (cadr action))
		       (princ "end turn")))
		 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil)
	    (lazy-mapcar (lambda (move)
			   (list (car move)
				 (limit-tree-depth (cadr move) (1- depth))))
			 (caddr tree)))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
			      (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
		    (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
	((zerop (car tree)) (play-vs-computer (handle-human tree)))
	(t (play-vs-computer (handle-computer tree)))))

(defun score-board (board player)
  (loop for hex across board
	for pos from 0
	sum (if (eq (car hex) player)
		(if (threatened pos board)
		    1
		    2)
		-1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
	 (player (car hex))
	 (dice (cadr hex)))
    (loop for n in (neighbors pos)
	  do (let* ((nhex (aref board n))
		    (nplayer (car nhex))
		    (ndice (cadr nhex)))
	       (when (and (not (eq player nplayer)) (> ndice dice))
		 (return t))))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
			   (rate-position (cadr move) player))
			 (caddr tree))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(apply (if (eq (car tree) player)
		   #'max
		   #'min)
	       (get-ratings tree player))
	(score-board (cadr tree) player))))

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (>= x upper-limit)
		     (list x)
		     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (<= x lower-limit)
		     (list x)
		     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(if (eq (car tree) player)
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit))
	    (apply #'min (ab-get-ratings-min tree
					     player
					     upper-limit
					     lower-limit)))
	(score-board (cadr tree) player))))

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
				     (car tree)
				     most-positive-fixnum
				     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

