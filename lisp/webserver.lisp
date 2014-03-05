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
  "function to break down the parameter list with multiple key-value pairs and create an a list of those"
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (request)
  (let* ((url (subseq request
		      (+ 2 (position #\space request))
		      (position #\space request :from-end t)))
	 (x (position #\? url)))
    (if x 
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun get-header (steam)
  "function to read the header information of the received request"
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i (cons (intern (string-upcase (subseq s 0 i)))
			    (subseq s (+ 2 i)))))))
    (when h (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

