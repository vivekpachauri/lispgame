;;;the follwing code is an exercise to use map-reduce to do word count.

;;function to take a string and return a list such that each character of the string
;;is mapped to the integer 1
(defun mapper (sentence)
  (labels ((remove-space-or-period (sentence)
	     (remove-if (lambda (x) (or (equal x #\ ) (equal x #\.))) sentence)))
    (map 'list (lambda (x)
	       (list x 1))
	 (remove-space-or-period sentence))))


;;now that we have our list with each character mapped to 1, we need to sort this and then keep passing all
;;of the elements of the same type to a reducer function.
;;I guess I should first write the reducer which will take a bunch of cons cells and then return the
;;count of them
(defun reducer (lst)
  (reduce (lambda (x y) (list (car x) (+ (cadr x) (cadr y)))) lst))


;;now i guess i need the intermediate driver function which will do the work
;;of iterating over the list returned by the mapper and call the reducer with the
;;right chunks of data
(defun driver (lst)
  ;;this function should iterate over the list returned by the mapper and
  ;;first sort the elements of the list and then iterate over the list
  ;;passing the right chunks to the reducer to generate an aggregate
  ;;--exercise-- how to compare two characters
  ;;answer - prepend 'char' before the comparison operators, example - char<, char> char=,
  ;;char/=, char<=, char>=. These are case sensitive comparison operator, for case
  ;;insensitive comparison operators, use char followed by a dash followed by coparison
  ;;type in words and for all predicates other than equal, use wordp format (greaterp etc.)
  (let* ((sorted (sort lst #'char< :key #'car))
	 (previous '((#\. 0)))
	 (aggregate '()))
					;now we have the sorted list, map over this list
    (map 'list 
	 (lambda(x)
					;if x is different than previous then
					;set previous to x and pass everything previous
					;to the reducer, this means that previous has to
					;be an aggregate
	   (cond
	     ((equal (car (last previous)) x)
	      (fresh-line)
	      (princ "modifying previous")
	      (setf previous (append previous (list x)))
	      (princ previous))
					;if not equal then call reducer with the current
					;value in previous and then change the previous to x
	     (t 
	      (setf aggregate (append (reducer previous) aggregate))
	      (fresh-line)
	      (princ "modifying aggregate")
	      (print aggregate)
	      (setf previous (list x)))))
	 sorted)
    aggregate))
)




;;function to remove the last element of a list, this i created just as an exercise
(defun remove-last (lst)
  (if (null (cdr lst))
	    '()
	    (append (list (car lst)) (remove-last (cdr lst)))))


;;;string stream
(defparameter ostr (make-string-output-stream))

;;;defining custom error conditions
(define-condition foo () ()
  (:report (lambda (condition stream)
	     (princ "stop fooing around idiot!!" stream))))

(define-condition bar () ()
  (:report (lambda (condition stream)
	     (princ "stop baring around dummies!!" stream))))

(defun bad-function ()
  (let ((r (random 100)))
    (if (< r 50)
	(error 'foo)
	(error 'bar))))

