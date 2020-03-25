(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv" t))


(load "func.lisp")


(defun cut-parameter (command)
	  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
  )

(defun parse-command (commandQuery)
  (let ((openBracketPosition (position #\( commandQuery)))
	(setq openBracketPosition (cond
					((not openBracketPosition) 0)
					(t openBracketPosition)
					))
	(subseq commandQuery 0 openBracketPosition)
	)
)
(defun inquiry-to-db(str)
	(pprint "Error, this function exists;)"
	)
)


(defun execute-command(str)
	(let ((command (parse-command str)))
	(cond
	  ((string-equal command "exit") (exit))
	  ((string-equal command "inquiry") (pprint (inquiry-to-db str)))
	  ((string-equal command "load") (pprint (load-table (cut-parameter str))))
	  ((string-equal command "show") (read-file (cut-parameter str)))
	  ((string-equal command "show"))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
)

(defun show-menu()
(princ 
"exit() - вихід.
load(*ім'я таблиці*) - завантажити таблицю.
show(*ім'я таблиці*)  - показати вміст документа.
inquiry(*запит*) - запит до бд.
"
)
)

(defun start-run ()
    (loop
    (terpri)
    (princ "[user]:")
    (terpri)
    (execute-command (read-line))
    )
)
(show-menu)    
(start-run)
;(write (execute-command "inquiry(SELECt col from map_zal-skl9.csv where row=2)"))
;(pprint (simple-table:read-csv #P"mp-posts_full.csv"))

