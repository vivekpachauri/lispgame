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
  )