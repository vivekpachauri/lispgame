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
  (initialize-monsters)
  (initialize-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over..."))
  (when (monsters-dead)
    (princ "Congratulations!!! You killed all the enemies. You Won.")))

(defun game-loop()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monster-dead)
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
	 (unless (monster-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monster-dead)
		   (monster-hit (random-monster) 1))))))

(defun randval(n)
  (1+ (random (max 1 n))))