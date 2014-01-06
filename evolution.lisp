;;;this file will contain the code for the evolution game

;;global variables
(defparameter *width* 100)
(defparameter *height* 300)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-enerty* 80)

;;hash table to contain the location of each plant
(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  "function to grow a plant"
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  "function to randomly grow plants in jungle and outside the jungle"
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;;animal information
(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list (make-animal
	 :x (ash *width* -1)
	 :y (ash *height* -1)
	 :energy 1000
	 :dir 0
	 :genes (loop repeat 8 collecting (1+ (random 10))))))

(defun move (animal)
  "function to move this animal by one step depending on its direction and genes"
  (let ((direction (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= direction 2)
						(< direction 5)) 1)
					  ((or (= direction 1)
					       (= direction 5)) 0)
					  (t -1))
				    *width)
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= direction 0)
						(< direction 3)) -1)
					  ((and (>= direction 4)
						(< direction 7)) 1)
					  (t 0))
				    *height*)
				 *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  "funchtion to handle the turning of animal. this function will decide if and how much (depending on the genes of the animal) the animal should turn on a given day"
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
  "function to handle the animal eating. the logic is simple, if there is a plant at the animal's current location then consume it"
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  "function to handle the animal reproduction"
  (let ((e (animal-energy animal)))
    (when (> e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
	    (genes (copy-list (animal-genes animal)))
	    (mutation (random 8)))
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))

(defun update-world ()
  "function to take care of all the events in a day of the simulation"
  (setf *animals* (remove-if (lambda (animal) (<= (animal-energy animal) 0)) *animals*))
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))

(defun draw-world ()
  "function to draw the existing situation of the world on the console"
  (loop for y below *height*
       do (progn (fresh-line) (princ "|")
		 (loop for x below *width* do
		      (princ (cond ((some (lambda (animal)
					    (and (= (animal-x animal) x)
						 (= (animal-y animal) y)))
					  *animals*)
				    #\M)
				   ((gethash (cons x y) *plants*) #\*)
				   (t #\space))))
		 (princ "|"))))

(defun evolution ()
  "function to drive the evolution simulation"
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i below x
		      do (update-world)
		      if (zerop (mod i 1000))
		      do (princ #\.))
		   (update-world))
	       (evolution))))))

