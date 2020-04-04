(defun expansion (str1 str2)
	(setq match (mismatch str2 str1 :from-end t))
	(or (not match) (= 0 match)))

(defun space_str (string &optional (separator "  "))
  (space_str_dop string separator))

(defun space_str_dop (string &optional (separator "  ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(space_str_dop (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r)))) 


(defun read_file_in_line (OurInput &optional (TableData '()))
	(setq line (read-line OurInput nil))
	(cond
		(line (read_file_in_line OurInput (append TableData (list line))))
		(t TableData))) 


(defun recognize_file (TableData)
	(cond
		((expansion OurName "tsv") (recognize_file_separator TableData "|"))
		((expansion OurName "csv") (recognize_file_separator TableData ","))
		(t (error "Not correct file extension. Pls correct the mistake. File path: ~S" OurName))))


(defun recognize_file_separator (TableData separator)
	(mapcar #'(lambda (row) (space_str row separator)) TableData))


(defun print_table (table)
	(mapcar
		#'(lambda (row)
			(mapcar
				#'(lambda (val)
					(format t "~15A " val))
				row)
			(format t "~C" #\linefeed))
		table)) 



(setq table_path "./")
(import 'charset:utf-8 'keyword)


(defun command_analysis (query)
	(setq index (search "(" query))
	(cond
		((string= (substring query 0 index) "load") "load")
		((string= (substring query 0 index) "exit") "exit")	))	


(defun command_row (query)
	(setq
		paramStartIndex (+ (search "(" query) 2)
		paramEndIndex (- (search ")" query) 1)
		param (substring query paramStartIndex paramEndIndex))
	param)


(defun command_execution (command query)
	(cond
		((string= command "load")
			(setq
				OurName (command_row query)				
				TableData (read_file_in_line (open OurName :external-format :utf-8))
				table (recognize_file TableData))
			(print_table table))
		((string= command "exit") (exit))
		(t (error "Command is not correct: ~S" query)))) 




	
	(setq query (read-line))	
	
	(setq command (command_analysis query))
	
	(command_execution command query))

