;;;code for grand theft wumpus
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

