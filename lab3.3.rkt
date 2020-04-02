#lang racket
(define (table-show temp)
   (displayln
    (map
    (lambda (str)
      (or
       (and
        (<= (string-length str) 5)
         (~a(~a str  #:min-width 5 #:align 'left #:right-pad-string " ") #:max-width 5 #:limit-marker "."))
       (and
        (<= (string-length str) 15)
         (~a(~a str  #:min-width 15 #:align 'left #:right-pad-string " ") #:max-width 15 #:limit-marker "."))
       (and
        (> (string-length str) 15)
        (~a(~a str #:max-width 30 #:limit-marker "...") #:min-width 30 #:align 'left #:right-pad-string " ")
        )))
    (string-split (string-replace temp "," "\t") "\t"))
    ))
(define (read_all-file first second)
  (or
   (and
    (and
     (not (eq? (read-line first) eof))
     (table-show (read-line second))
    (read_all-file  first second)))
   (displayln "Reading done successfully")
   ))
(define (load file)
  (read_all-file
    (open-input-file file) (open-input-file file)))

