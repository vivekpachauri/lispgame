
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp)
		 (prin1-to-string exp)))