;;; Require modules
(load "helpers.lisp")

;;; Read file into list by lines
(defun read-file-by-lines (input-stream &optional (table-data '()))
	(setq line (read-line input-stream nil))
	(cond
		(line (read-file-by-lines input-stream (append table-data (list line))))
		(t table-data)))