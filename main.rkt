#lang racket
;
; ===============================================
;
; (c) 2021
; version 1.0.0 2021-10-24
;
; -----------------------------------------------
; EIF400 Paradigmas de Programación
; 2do ciclo 2021, grupo 02(8pm)
; Proyecto 1
;
; 117250610 Barrientos Araya, Daniel
;
; ===============================================
;
(define addx
  (lambda (r x C f?)
    (cond ((empty? C)
           (list (list (list x) r)))
          ((f? r (car C))
           (cons (list(append (caar C) (list x)) (cadr(car C))) (cdr C)))
          (else
           (cons (car C) (addx r x (cdr C) f?)))
          )))
(define f
  (lambda (x C)
    (= x (cadr C) )
    ))
(define (genome n k L);Creamos una lista con valores auxiliares
   (if (not (= (length L)n));la cual nos ayudara a representar
        (genome n k (cons (random 1 (+ k 1)) L));los diferentes valores que pueden ser tomados
            L))   ;basados en el problema de la mochila
          ;los cuales se utilizan como un wrapper para los valores posibles basado en la lista recibida por el usuario


;(define (population n size count k L);Creamos una población,"iterando" hasta llegar
 ;   (if (not (= size count));a la cantidad digitada por el usuario
  ;      (cons (genome n k L) (population n size (+ count 1) k L))
        ;L
  ;))
;(addx 1 2 '() f)
;(addx 2 3 '( ((2) 1) ) f)
(addx 2 6 '( ((2) 1) ((3) 2) ) f)
(addx 3 8 '(((2) 1) ((3 6) 2))  f)
(addx 3 4 '(((2) 1) ((3 6) 2) ((8) 3))  f)
;(addx (genome 6 5 '()) 1 '() f )
;(addx (genome 6 5 '()) 2 '(((1) 1)) f)
;(addx (genome 6 5 '()) 2 '(((1) 1) ((2) 2)) f)
;(define (fitness genome L2)0)
(define (convert)0)
;(genome 6 3 '())

;(population 7 6 0  4 '())



  
