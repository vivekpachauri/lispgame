;;;this file will contain the code for the orc battle game

;;global variables to track the three attributes of the place,
;;health, agility, and strength
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;;this will be an array which will contain the actual monsters in the game
(defparameter *monsters* nil)

;;this will be an array which will contain a list of monster building
;;functions for our game
(defparameter *monster-builders* nil)

;;this will control the number of monsters in the game
(defparameter *monster-num* 12)

;;the controller function which will call the helper functions to drive the game
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over..."))
  (when (monsters-dead)
    (princ "Congratulations!!! You killed all the enemies. You Won.")))

(defun game-loop()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

(defun init-player()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ") (princ *player-health*)
  (princ ", and agility of ") (princ *player-agility*)
  (princ ", and a strength of ") (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab, [d]ouble swing, [r]ound house: ")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ") (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

(defun randval(n)
  (1+ (random (max 1 n))))

;;helper functions for player attack
(defun random-monster()
  "function to pick a random monster"
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
 "function to pick a particular monster"
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (princ "Pick between 1 and ") (princ *monster-num*)
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn
		(princ "That monster is already dead.")
		(pick-monster))
	      m)))))

;;Monster management functions
(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

(defun monster-dead (m)
  "function to check if the givem monster is dead"
  (<= (monster-health m) 0))

(defun monsters-dead ()
  "function to check if all the monsters are dead"
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  "function to display information about all the monsters"
  (fresh-line)
  (princ "Your foes: ")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "  ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn
		 (princ "(Health=")
		 (princ (monster-health m))
		 (princ ") ")
		 (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  "function to register hitting the monster m with x power"
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed ") (princ (type-of m)) (princ "!"))
      (progn (princ "You hit the ") (princ (type-of m))
	     (princ ", knocking off ") (princ x) (princ " health points."))))

(defmethod monster-show (m)
  "generic method to display information about monster m"
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m)
  "generic method to register the monster m attacking")

;;define the orc data type
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  "orc specific method to display the information about the monster"
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club."))

(defmethod monster-attack ((m orc))
  "orc specific method to register attack by this monster"
  (let ((x (randval (orc-club-level m))))
    (fresh-line)
    (princ "An orc swing his club at you and knocks off ")
    (princ x) (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  "hydra specific method to display information about the monster"
  (princ "A malicious hydra with ") (princ (hydra-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  "hydra specific method to register attack to the monster"
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "Corpse of the fully decapacitated and decapacitated hydra falls
to the ground")
      (progn (princ "You lop off ") (princ x) (princ " of the hydra's heads!"))))

(defmethod monster-attack ((m hydra))
  "hydra specific method to register the hydra attacking the player"
  (let ((x (randval (ash (monster-health m) -1))))
    (fresh-line)
    (princ "A hydra attacks you with ") (princ x)
    (princ " of its heads! It also grows back one of its head back! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;;define the structure type for slimy monster
(defstruct (slimy-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slimy-mold *monster-builders*)

(defmethod monster-show ((m slimy-mold))
  "Slimy mold monster specifiec method to describe the monster"
  (princ "A slimy mold with a slimeness of ")
  (princ (slimy-mold-sliminess m)))

(defmethod monster-attack ((m slimy-mold))
  "Slimy mold specific method to register the slimy mold attacking the player"
  (let ((x (randval (slimy-mold-sliminess m))))
    (fresh-line)
    (princ "A slimy mold wraps around your leg and decreases your agility by ")
    (princ x) (princ "!")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face and takes away a health point!!")
      (decf *player-health* 1))))

;;methods for the cunning brigand
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  "Brigand specific method to register the bigand attacking the player"
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (fresh-line)
	   (princ "A Brigand hits you with his slingshot taking off 2 health points")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (fresh-line)
	   (princ "A Brigand catches your leg with his whip taking off 2 agility points")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (fresh-line)
	   (princ "A Brigand strikes your arm with his whip reducing your strength by 2")
	   (decf *player-strength* 2)))))

