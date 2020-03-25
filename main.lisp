(import 'charset:utf-8 'keyword)
(defun split (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun split-string (string &optional (separator " "))
  (split string separator))

(defun write-spaces (n)
	(cond 
	((< n 0) (princ " "))
	((eq 0 n) nil)
	(T (write-spaces (- n 1))))
	(princ " ")	
)

(defun format-write (word)
	(princ word)
	(write-spaces (- 50 (length word)))
)

(defun print-list (A)
  (if (null A) nil (format-write (car A)))
  (cond ((null A) A)
  (T (print-list (cdr A)))))
  

(defun load-file (filename)  
(with-open-file (s filename :external-format :utf-8)
  (if (string= "csv" (nth 1 (split-string filename ".")))
  (loop for c = (read-line s nil)
        while c do (print-list (split-string c ","))
		(terpri)))
  (if (string= "tsv" (nth 1 (split-string filename ".")))
  (loop for c = (read-line s nil)
        while c do
		(print-list (split-string c "	"))
		(terpri)))
))
