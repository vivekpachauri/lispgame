;;;code for grand theft wumpus game
;;;first load the graph util file
(load "graphutils")

;;;define the global variables to capture the number of city location, city roads etc.
(defparameter *city-nodes* nil)
(defparameter *city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;;;create a random list of edges to connect all the nodes
(defun random-node()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eq a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) (eql (car x) node)) edge-list))

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
		      (disconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when disconnected
		   (find-island disconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1 (mapcar (lambda (edge)
				  (list (cdr edge)))
				(remove-duplicates (direct-edges node1 edge-list)
						   :test #'equal))))
	 (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cop)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cop :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (to-check-node from-node edge-alist)
  (member target-node (neighbors from-node edge-alist)))

(defun within-two (to-check-node from-node edge-alist)
  (or (within-one to-check-node from-node edge-alist)
      (some (lambda (x)
	      (within-one x from-node edge-alist))
	    (neighbors to-check-node edge-alist))))

;;;function that builds the final node alist (basically the final
;;;map of the city)
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num* collect (random-node))))
    (loop for n from 1 to *node-num* collect
	 (append (list n)
		 (cond ((eql n wumpus) '(wumpus))
		       ((within-two n wumpus edge-alist) '(blood)))
		 (cond ((member n glow-worms) '(glow-worm))
		       ((some (lambda (worm)
				(within-one n worm edge-alist))
			      glow-worms)
			'(lights!)))
		 (when (some #'cdr (cdr (assoc n edge-alist)))
		   '(sirens!))))))

;;;Initializing a new game of grand theft wumpus
(defun new-game()
  (setf *city-edges* (make-city-edges))
  (setf *city-nodes* (make-city-nodes *city-edges*))
  (setf *player-position* (find-empty-node))
  (setf *visited-nodes* (list *player-position*))
  (draw-city))

(defun find-empty-node()
  (let ((x (random-node)))
    (if (cdr (assoc x *city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city()
  (ugraph->png "city" *city-nodes* *city-edges*))

(defun known-city-nodes ()
  "function to return only those nodes of the city which have already been
visited"
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *city-nodes*)))
		  (if (eql node *player-position*)
		      (append n '(*))
		      n))
		(list node '?)))
	  (remove-duplicates 
	   (append *visited-nodes* 
		   (mapcan (lambda (node)
			     (mapcar #'car (cdr (assoc node *city-edges*))))
			   *visited-nodes*)))))

(defun known-city-edges()
"function to return only those edges of the city which have already been
visited. If an edge's two nodes have not yet been visited then any cop
information about that edge or street will not be shown"
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *city-edges*)))))
	  *visited-nodes*))

(defun draw-known-city ()
  "function to draw only that part of the city which has already been visited"
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))


;;;Initializing a new game of grand theft wumpus while also drawing
;;;only those nodes which have already been visited
(defun new-game()
  (setf *city-edges* (make-city-edges))
  (setf *city-nodes* (make-city-nodes *city-edges*))
  (setf *player-position* (find-empty-node))
  (setf *visited-nodes* (list *player-position*))
  (draw-city)
  (draw-known-city))

(defun walk-to (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charge)
  (let ((edge (assoc pos (cdr (assoc *player-position*
				     *city-edges*)))))
    (if edge
	(handle-new-place edge pos charge)
	(princ "That location does not exist!!"))))

(defun handle-new-place (edge pos charge)
  (let* ((node (assoc pos *city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-position* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into cop. Game Over."))
	  ((member 'wumpus node) (if charging
				     (princ "You found the Wumpus!")
				     (princ "You ran into Wumpus")))
	  (charging (princ "You waster your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a glow-worm gang. You are now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-table)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
	       (unless (gethash node  visited)
		 (setf (gethash node visited) t)
		 (mapc (lambda (edge)
			 (traverse edge))
		       (gethash node edge-table)))))
      (traverse node))
    visited))
