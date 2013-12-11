(defparameter *nodes* '((living-room 
                            (you are in the living room. a wizard is snoring on the couch.))
                        (garden
                            (you are in a beautiful garden. there is a well in front of you.))
                        (attic
                            (you are in the attic. there is a giant welding torch in the corner.))))
                            
(defun describe-location (loc nodes)
  (cadr (assoc loc nodes)))

(defparameter *edges* '((living-room
                            (garden west door)
                            (attic upstairs ladder))
                        (garden
                            (living-room east door))                       
                        (attic
                            (living-room downstairs ladder))))
                            
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (loc edges)
   (apply (function append) 
          (mapcar (function describe-path) 
                  (cdr (assoc loc edges)))))


(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply (function append)
	   (mapcar (function describe-obj)
		   (objects-at loc objs obj-locs)))))

(defparameter *curr-loc* 'living-room)

(defun look ()
  (append (describe-location *curr-loc* *nodes*)
	  (describe-paths *curr-loc* *edges*)
	  (describe-objects *curr-loc* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction 
		    (cdr (assoc *curr-loc* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *curr-loc* (car next))
	       (look))
	'(you cannot go that way.))))

(defun pick (obj)
  ;---Alternate Alert--- created this function instead of pickup
  (if (member obj (objects-at *curr-loc* *objects* *object-locations*))
      (progn
	(push (list obj 'body) *object-locations*)
	`(picked up ,obj))
      `(cannot pick up ,obj)))

(defun pickup (obj)
  (cond ((member obj 
		 (objects-at *curr-loc* *objects* *object-locations*))
	 (push (list obj 'body) *object-locations*)
	 `(you are now carrying the ,obj))
	(t '(you cannot get that.))))

(defun show-inventory ()
  ;---Alt Alert--- this function is an alternate to the inventory function
  (labels ((carrying (obj)
	     `(You are carrying ,obj with you.)))
    (apply #'append 
	   (mapcar #'carrying (objects-at 'body *objects* *object-locations*)))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl ()
  (let ((command (game-read)))
    (unless (or (eq (car command) 'quit)
		(eq (car command) 'exit))
      (game-print (game-eval command))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pick pickup inventory show-inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      `(sorry cannot understand ,sexp command)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\. #\?)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst)) 'list)
			     t
			     nil)
		 'string))
  (fresh-line))