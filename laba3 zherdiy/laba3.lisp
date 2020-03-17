;спочатку треба завантажити quicklisp.lisp
;далі запустити sbcl та відкрити файл командою (load)
;далі можна відкрити файл csv за допомогою команди (loadfile "ім'я файлу")

(load "/Users/admin/quicklisp/setup.lisp")
(ql:quickload :cl-csv)

(defun loadfile(filename)
    (format t "~{~{~a,~T~}~% ~}" (cl-csv:read-csv (pathname filename)))
) 


