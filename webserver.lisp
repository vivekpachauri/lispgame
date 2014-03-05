;;;function to decode the request parameters and convert the
;;;character codes into characters
(defun http-char (c1 c2 &options (default #\Space))
  (let ((code (parse-interger (coerce (list c1 c2) 'string)
			      :radix 16
			      :junk-allowed t)))
    (if code
	(code-char code)
	default)))

