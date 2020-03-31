(import 'charset:utf-8 'keyword)

(defun ReadCommand (str)
	(setq Index (search "(" str))
	(cond
		((string= (substring str 0 Index) "load") "load")
		(t (print "Неизвестная команда"))))

(defun ParseTable (Data)
	(cond
		((EndOfSting Name "tsv") (TableSeparator Data "|"))
		((EndOfSting Name "csv") (TableSeparator Data ","))
		(t (print "Ошибка открытия файла"))))

(defun TableSeparator (Data separator)
	(mapcar #'(lambda (row) (Split row separator)) Data))

(defun PrintTable (Table)
	(mapcar
		#'(lambda (row)
			(mapcar
				#'(lambda (val)
					(format t "~10A " val))
				row)
			(format t "~C" #\linefeed))
		Table)) 


(defun GetNameFile (str)
	(setq
		StartIndex (+ (search "(" str) 2)
		EndIndex (- (search ")" str) 1)
		param (substring str StartIndex EndIndex))
	param)

	(defun ReadFile (Input &optional (Data '()))
	(setq line (read-line Input nil))
	(cond
		(line (ReadFile Input (append Data (list line))))
		(t Data))) 

(defun ExecuteProg (command str)
	(cond
		((string= command "load")
			(setq
				Name (GetNameFile str)
				Data (ReadFile (open Name :external-format :utf-8))
				Table (ParseTable Data))
			(PrintTable Table))
		)) 

(defun EndOfSting (str1 str2)
	(setq match (mismatch str2 str1 :from-end t))
	(or (not match) (= 0 match)))

(defun Split (string &optional (separator "  "))
  (Split1 string separator))

(defun Split1 (string &optional (separator "  ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(Split1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r)))) 

(setq data (read-line))
(setq command (ReadCommand data))
(ExecuteProg command data)
