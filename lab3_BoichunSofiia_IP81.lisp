;сдля начала я загрузила библиотеку quicklisp.lisp
;затем в sbcl открыла файл библиотеки командой (load)
;после этого открывала файл csv, используя (loadfile "имя файла")

(load "D:/FICT/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload :cl-csv)

(defun loadfile(filename)
    (format t "~{~{~a,~T~}~% ~}" (cl-csv:read-csv (pathname filename)))
)

(LOADFILE "D:/ФІОТ/2ий курс/4ый семестр/ФП/Лаб 3/lab3/6.csv")

