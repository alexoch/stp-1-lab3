#lang racket

(provide PrettyTableOutput)

(define (PrettyTableOutput tableR headerR)
  (map (lambda (h)
         (PrintRow (index-of headerR h) tableR headerR)
        (display "\n"))
       headerR))
  
 

(define (max-field-width table) (quotient 160 (hash-count table)))

  
(define (minWidthForField lst)
  (first (sort (map (lambda (l)
       (append (string-length l)))
         lst) >)))

(define (minWidth lst tableR)
  (cond
    ((>(minWidthForField lst) (max-field-width tableR)) (max-field-width tableR))
    (#t (minWidthForField lst))))



(define (PrintRow rowIndex tableR headerR)
 (for ([i headerR])
        (display (string-append "|" (~a (string-append (list-ref (hash-ref tableR i) rowIndex)) 
                    #:max-width (max-field-width tableR)
                    #:min-width (minWidth (hash-ref tableR i) tableR)
                    #:limit-marker "..."
                    #:align 'center
                    )  ))
  ))