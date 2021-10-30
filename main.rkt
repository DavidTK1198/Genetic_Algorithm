#lang racket
;
; ===============================================
;
; (c) 2021
; version 1.0.0 2021-10-24
;
; -----------------------------------------------
; EIF400 Paradigmas de Programación
; 2do ciclo 2021, grupo 03(8pm)
; Proyecto 1
;
; 117250610 Barrientos Araya, Daniel
;
; ===============================================
;
(define (delete  L)
  (filter(lambda (x) (= x 1))L))


(define f
  (lambda (L k)
     (if (not (= (length (car L)) k))
       #t #f)
    ))

       
(define addx
  (lambda (x C f? k )
    (cond ((empty? C)
           (list (list x)))
          ((f? C k)
          (cons (append (car C) (list x)) (cdr C)))
          (else
           (cons (car C) (addx x (cdr C) f?  k)))
          )))


(define (genome n k L)
   (if (not (= (length L)k))
       (genome n k (addx (random 0 2) L f n ))
       (if (not (=(length (last L))n))
          (genome n k (addx (random 0 2) L f n ))
       L)))

(genome 12 4 '())

  
