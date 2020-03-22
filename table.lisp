;;; Parse table
(defun parse-table (table-data)
	(cond
		((ends-with-str table-path "tsv") (parse-table-separator table-data (string #\tab)))
		((ends-with-str table-path "csv") (parse-table-separator table-data ","))
		(t (error "Not supported file extension. File path: ~S" table-path))))

;;; Parse table with passed separator
(defun parse-table-separator (table-data separator)
	(mapcar #'(lambda (row) (split-str row separator)) table-data))

;;; Pretty table output to stdout
(defun pretty-table-print (table)
	(mapcar
		#'(lambda (row)
			(mapcar
				#'(lambda (val)
					(format t "~15A " val))
				row)
			(format t "~C" #\linefeed))
		table))