;(load "quicklisp.lisp")
;(quicklisp-quickstart:install)
;(ql:quickload :cl-csv)

;(format t "~{~{~a,~T~}~% ~}" (cl-csv:read-csv #P"map_zal-skl9.csv"))

(defun loadFile (filename)
    (format t "~{~{~a,~T~}~% ~}" (cl-csv:read-csv (pathname filename))))

(defun loadFile2 (filename)
    (car (cl-csv:read-csv (pathname filename))))

(defun get-coll-index (coll-name coll-list &optional (number 0))
    (cond ((equal coll-name  (car coll-list)) number)
          ((not coll-list) (princ "oops") (terpri) number)
          (t (get-coll-index coll-name (cdr coll-list) (+ number 1) ))))

(defun select (filename columns)
    (get-colls (cl-csv:read-csv (pathname filename)) (get-all-indexs columns (car (cl-csv:read-csv (pathname filename))))))

(defun get-by-index (the-list index)
    (cond ((= index 0) (car the-list)) 
          (t (get-by-index (cdr the-list) (- index 1)))))

(defun get-colls (table indexs &optional (result nil))
    (cond ((not table) result)
          (t (get-colls (cdr table) indexs (append result (cons (get-from-the-row indexs (car table)) nil))))))

(defun is-necessary (indexs current)
    (cond ((not indexs) nil)
          ((= (car indexs) current) t)
          (t (is-necessary (cdr indexs) current))))

(defun get-all-indexs (in-colls coll-list &optional (indexs nil))
    (cond ((not in-colls) indexs)
          (t (get-all-indexs (cdr in-colls) coll-list (cons (get-coll-index (car in-colls) coll-list) indexs)))))

(defun get-from-the-row (indexs row &optional (filtered-row nil))
    (cond ((not indexs) filtered-row)
          (t (get-from-the-row (cdr indexs) row (cons (get-by-index row (car indexs)) filtered-row)))))

(defun select-distinct (filename coll-name)
    (make-up-distinct (select filename (cons coll-name nil)))
)

(defun make-up-distinct (original &optional (final-list nil))
    (cond ((not original) final-list)
          ((not (is-there final-list (car original))) (make-up-distinct (cdr original) (append final-list (cons (car original) nil))))
          (t (make-up-distinct (cdr original) final-list)))
          
)

(defun is-there (list element)
    (cond ((equal list nil) nil)
          ((equal (car list) element) t)
          (t (is-there (cdr list) element))))

;(princ (select-distinct "f2.csv" "row"))

(defun between (table collumn-name bottom-border top-border)
    (between-req (cdr table) (get-coll-index collumn-name (car table)) bottom-border top-border)
)

(defun between-req (table collumn-index bottom-border top-border &optional final-list)
    (cond ((equal table nil) final-list)
          ((between-two (get-by-index (car table) collumn-index) bottom-border top-border) (between-req (cdr table) collumn-index bottom-border top-border (append final-list (cons (car table) nil))))
          (t (between-req (cdr table) collumn-index bottom-border top-border final-list))))

(defun between-two (element bottom top)
    (cond ((and (< (parse-integer element) top) (> (parse-integer element) bottom)) t)
          (t nil))
)

(princ (between (select "f2.csv" '("row" "col")) "row" 10 15))

;(princ (between-two 2 2 6))

;(princ (nconc '(1 2 3) (cons 4 nil)))


;(princ (is-there '() "world"))



;(princ (select "f2.csv" '("row" "col")))
;(select "f2.csv" 1)

;(with-open-file (stream filename)
;    (let ((contents (make-string (file-length stream))))
;      (read-sequence contents stream)
;      contents))