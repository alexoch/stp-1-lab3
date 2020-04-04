#lang racket

(require "pretty-table.rkt")

#|load("C:/Users/Smart/Desktop/2course/fp/lab3/plenary_register_mps-skl9.tsv")|#
#|load("C:/Users/Smart/Desktop/2course/fp/lab3/mp-assistants.csv")|#



(define command null)
(define loadFile null)
(define type null)
(define input null)
(define index 0)
(define FileList null)
(define header null)
(define tableByColumn (make-hash))

(define (CreateTableByColumn table)
  (set! index 0)
  (for ([h header])
    (hash-set! tableByColumn h (map (lambda (lst)
                                      (append (list-ref lst index)))
                                    FileList))
    (set! index (+ index 1))))


(define (listFromFile separator)
(set! FileList (map (lambda (lst)
       (string-split lst separator))
       (file->lines loadFile)))
  (set! header (first FileList)))


(define (ReadFile)
  (cond
    ((equal? type "csv") (listFromFile ","))
    ((equal? type "tsv") (listFromFile "\t"))
    (#t (println "error"))))


(define (GetType)
  (set! type (second (string-split loadFile "."))))


(define (CheckFile)
  (cond
    ((file-exists? (second (string-split input "\""))) (set! loadFile (second (string-split input "\""))))
    (#t (println "File don't find"))))



(define (load)
  
  (CheckFile)
  (GetType)
  (ReadFile)
  (CreateTableByColumn tableByColumn)
  (PrettyTableOutput tableByColumn header))
  


(define (GetCommand)
  (cond
    ((equal? input "") (println "String is empty"))
    ((equal? (substring input 0 4) "load") (set! command "load"))
    (#t (println "Command don't find"))))



(define (run)
  (set! input (read-line))
  (GetCommand)
  (cond
    ((equal? command "load") (load))
    (#t (println "error"))))

(run)








