(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv" t))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv" t))


(defun split-by-one-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-one-equal (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\= string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-one-less (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\< string :start i)
          collect (subseq string i j)
          while j))

(defun get-last(lst)
	(cond 
		((null (cdr lst)) (car lst))
		(t (get-last (cdr lst)))
		)
)



(defun load-table(tableName)
	(cond
		((string= tableName "map_zal")  map_zal)
		((string= tableName "mp_assistants") mp_assistants)
		((string= tableName "plenary_register_mps") plenary_register_mps)
		((string= (get-last (split-by-one-dot tableName)) "csv") (simple-table:read-csv tableName t))
		((string= (get-last (split-by-one-dot tableName)) "tsv") (simple-table:read-tsv tableName t))
		(t (print "file type not define"))
)
)

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun split-by-one-comma(string)
    (loop for i = 0 then (1+ j)
          as j = (position #\, string :start i)
          collect (subseq string i j)
          while j))


(defun read-file(nameFile)
(let ((in (open nameFile :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))
)


(defun generation(n)
	(cond
		((= n 0) nil)
		(t (list* (- n 1) (generation (- n 1))))
	)
)

(defun vector-to-list(vec n)
	(cond
		((= (length vec) n) nil)
		(t (list* (aref vec n) (vector-to-list vec (+ n 1))))
	)
)
(defun string-intersection(str lst)
	(cond
		((null lst) nil)
		((string-equal str (car lst)) t)
		(t (string-intersection str (cdr lst)))
	)
)

(defun cut-list-to-el(lst str)
(cond
((null lst) lst)
((string-equal (car lst) str) (cdr lst))
(t (cut-list-to-el (cdr lst) str))
)
)

(defun position-col(str vec n)
(cond
((string-equal str (aref vec n)) n)
(t (position-col str vec (+ n 1)))
)
)

