;Для запуску в командному рядку необхідно спочатку прописати (load "1.lisp") a тоді (loadFile "FileName")
;Для роботи програми необхідно встановити quicklisp
;У рядок #5 необхідно вставити шлях до свого setup.lisp файлу
(load "C:/Users/laslo/quicklisp/setup.lisp")
(ql:quickload :cl-csv)
(defun loadFile (filename)
  (format t "~{~{~a, ~T~}~% ~}"(cl-csv:read-csv (pathname fileName))))