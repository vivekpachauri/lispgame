;;;function to decode the request parameters and convert the
;;;character codes into characters
(defun http-char (c1 c2 &optional (default #\Space))
  "function to convert an ascii code into its corresponding character"
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
			      :radix 16
			      :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  "function to decode the string containing http characters which might incldue
ascii coded characters"
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))