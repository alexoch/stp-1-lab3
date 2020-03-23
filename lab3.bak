#lang racket
(require (planet neil/csv:1:=7) net/url)
 
(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
 
(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) '("\t")))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list " "))))
  (define (->string row) (string-join row "\t" #:after-last "\n"))
  (string-append* (map ->string (cons head rows))))
 

(define (cli)
  (writeln "Ласкаво просимо до lab 3 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port)))
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
(when (<= (string-length option) 6) (error 'помилка "невірно задано ім'я файлу"))
  (define scope1 (string-length option))
  (define scope2 (string-length option))
  (when (> scope1 6)(set! scope1 (substring option 4 6))
    (set! scope2 (substring option (- (string-length option) 2))))
  (define command-syntax (and (string-ci=? (substring option 0 4) "load") (and (string-ci=? scope1 "(\"") (string-ci=? scope2 "\")"))))
  (define filename (substring option 6 (- (string-length option) 2)))
  (define file-format "")
  (when (> (string-length filename) 4)
    (set! file-format (substring filename (- (string-length filename) 4))))
   (if(equal? command-syntax #t)
      (if(string-ci=? file-format ".txt")
         (cond
           [(string-ci=? filename "mps-declarations_rada.txt")
            (define csv-file (file->string "mps-declarations_rada.txt"))
            (display (all-rows (open-input-string csv-file)))]
           [(string-ci=? filename "mp-posts_full.txt")
            (define csv-file (file->string "mp-posts_full.txt"))
            (display (all-rows (open-input-string csv-file)))]
           [(string-ci=? filename "map_zal-skl9.txt")
            (define csv-file (file->string "map_zal-skl9.txt"))
            (display (all-rows (open-input-string csv-file)))]
           [(string-ci=? filename "plenary_register_mps-skl9.txt")
            (define csv-file (file->string "plenary_register_mps-skl9.txt"))
            (display (all-rows (open-input-string csv-file)))])
         (writeln "Невірно введено формат файлу. Будь ласка, спробуйте ще.")
         )
      (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")
 ))
(cli )