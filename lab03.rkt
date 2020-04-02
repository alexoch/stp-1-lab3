#lang racket

(define (parse-line line)
 (string-split line (car (regexp-match #rx"[^0-9a-zA-Z_-]" line))))

(define (gen-table file [vect '()])
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        vect
        (gen-table file (append vect (list (parse-line line))))
    )
  )
)

(define (fix-table table)
  (let ([line-length (length (car table))])
    (filter (lambda (line) (<= (length line) line-length )) table)
  )
)



(define (display-table table)
  (for-each (lambda (line)
              (for-each (lambda (arg)
              (if (= (length line) (+ 1 (index-of line arg)))
                  (displayln (~a arg #:min-width 20 #:max-width 20 #:limit-marker "...  " #:align 'center #:right-pad-string " " #:left-pad-string " "))
                  (display (~a arg #:min-width 20 #:max-width 20 #:limit-marker "...  " #:align 'center #:right-pad-string " " #:left-pad-string " ")))
               )
            line)
              )
            table)
  )

(let user-input ()
    (display "Input: ")
    (define a (read-line (current-input-port) 'any-one))
  (cond
    [(regexp-match #rx"load\\(\"[a-zA-Z0-9_-]*\\.[a-zA-Z]*\"\\)" a)
     (let ([file-name (substring a 6 (- (string-length a) 2))])
       (display-table (fix-table (gen-table (open-input-file file-name)))))]
    [else
    (displayln "incorrect command")]
    )
    (user-input))

