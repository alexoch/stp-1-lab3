(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload :cl-csv)

(defun loadFile (filename)
    (format t "~{~{~a,~T~}~% ~}" (cl-csv:read-csv (pathname filename))))