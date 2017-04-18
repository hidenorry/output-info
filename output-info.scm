#!/usr/bin/env gosh
(use srfi-1)
(use srfi-13)
(use gauche.collection)
(use slib)
(require 'format)

;; " Frequencies --   -160.0433                41.0884                65.1919"
;; " Full point group                 CS      NOp   2"
;; " SCF Done:  E(RwB97XD) =  -810.001892305     A.U. after    1 cycles"

(define (get-string-length str)
  (set! *lenstring* (string-length str)))
(define (string2lis str)
  ;; (string2lis "    a b c = 1 2 3 ") 
  (string-split
   (string-trim-both str)
   (string->regexp "\\s+")))

(define (freq? str)
  ;; (freq? " Frequencies --   -160.0433                41.0884                65.1919")
  (and (> *lenstring* 14)
       (string=? (substring str 0 12) " Frequencies")))
(define (make-get-freq)
  ;;(let1 fn (make-get-freq) (fn " Frequencies --   -160.0433                41.0884                65.1919") (fn " Frequencies --   4 5 6")) => (-160.0433 41.0884 65.1919 4 5 6)
  (let ((lfreq '()))
    (lambda (str)
      (set! lfreq
            (append!
             lfreq
             (map string->number
                  (cddr
                   (string2lis str)))))
      lfreq)))

(define (point-group? str)
  ;; (point-group? " Full point group                 CS      NOp   2")
  (and (> *lenstring* 17)
       (string=? (substring str 0 17) " Full point group")))
(define (get-point-group str)
  ;;(get-point-group " Full point group                 CS      NOp   2") => "CS"
  (list-ref (string2lis str) 3))

(define (optcomp? str)
  ;; (optcomp? " Optimization completed.")
  (and
   (>= *lenstring* 24)
   (string=? str " Optimization completed.")))

(define (scfdone? str)
  ;; (scfdone? " SCF Done:  E(RHF) =  -805.101282458     A.U. after    7 cycles")
  (if (> *lenstring* 40)
      (string=? (substring  str 1 9) "SCF Done")
      #f))
(define (scfene str)
  ;; (scfene " SCF Done:  E(RHF) =  -805.101282458     A.U. after    7 cycles")
  (string->number (ref (string2lis str) 4)))

(define *lenstring* 0)
(define (search-keys-from-file file)
  (let ((energy #f)
        (freq   #f)
        (lpgroup '())
        (conv   #f)
        (get-freq (make-get-freq)))
    (with-input-from-file file
      (lambda ()
        (port-for-each
         (lambda (line)
           (set! *lenstring* (string-length line))
           (cond ((scfdone? line) (set! energy (scfene line)))
                 ((optcomp? line) (set! conv #t))
                 ((freq? line) (set! freq (get-freq line)))
                 ((point-group? line) (push! lpgroup (get-point-group line)))))
        read-line)))
    (values energy freq lpgroup conv)))

(define (freq-counter lis)
  ;; (freq-counter '(-1 0 1 2 3 -2 -3)) => 3
  (letrec ((rec (lambda (lis counter)
                  (if (null? lis)
                      counter
                      (if (< (car lis) 0.0)
                          (rec (cdr lis) (+ counter 1))
                          (rec (cdr lis) counter))))))
    (rec lis 0)))

(define (main args)
  (let ((files (cdr args)))
    (for-each
     (lambda (file)
       (receive (ene freq lpg conv)
           (search-keys-from-file file)
         (let ((file (if file file "no file name"))
               (ene  (if ene ene "no energy data"))
               (freq (if freq freq '("no frequency data")))
               )
           (format #t
"------------------------
filename:      ~a
energy:        ~f
frequency:     ~{~@{~10a ~}~}  minus/all:~3@a/~3a
point group:   ~{~@{~a ~}~}
converged?:    ~a
------------------------
"
file
ene
(if (string? (car freq)) freq (take freq (freq-counter freq)))
(if (string? (car freq)) '- (freq-counter freq))
(if (string? (car freq)) '- (length freq))
(fold (lambda (x y) (cons (car x) y)) '() (group-collection lpg :test string=?))
conv))))
     files)))
