#lang racket
(require racket/cmdline)

(define input (read-line))
(define file (list-ref (string-split input "\"") 1))
(define command (list-ref (string-split input "(") 0))
(define fileType (list-ref (string-split file ".") 1))
(define separ
  (cond
    [(equal? fileType "csv") ","]
    [(equal? fileType "tsv") "/t"]))


(define (display-lines lines)
  (map (lambda (line)
         (displayln (format-line line separ (map string-length (string-split (list-ref lines 2) ",")))))
       lines))

(define (format-line line separator sizes)
  (map (lambda (str size)
         (~a str #:min-width (+ 4 (* size 2)) #:align 'center))
       (string-split (string-append line " ") separator)
       sizes))

(define (load filename) (display-lines (file->lines filename)))

(if (equal? command "load") (load file) (printf "error"))


