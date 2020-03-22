;;;; Variant 13

;;; Require modules
(load "command.lisp")

;;; Get table function
(defun main ()
	;; Read table index
	(setq query (read-line))
	(clear-input)
	;; Parse user command
	(setq command (parse-command query))
	;; Execute user's command
	(execute-command command query))

(main)