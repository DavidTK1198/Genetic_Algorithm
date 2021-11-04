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

(define (addition L)
     (list (car L) (foldl + 0 (car L))))

(define (transform L1 L2 L3)
  (if (empty? L1)
     (map addition L3)
      (transform (cdr L1) (cdr L2) (addx (car L2) (car L1) L3 f))))
      

(define (population size count k L L2);Creamos una población,"iterando" hasta llegar
    (if (not (= size count));a la cantidad digitada por el usuario
        (cons (genome (length L2) k '())  (population size (+ count 1) k L L2))
        L
  ))


 (define (substract lst x)
  (map (lambda (n) (abs(- x n))) lst))


(define (fitness-x L mean)
         (if (empty? L)
             0
       (expt (+ (apply + (substract (caar L) mean)) (fitness-x (cdr L) mean)) 2)
       )
  )
  
  (define (fitness L k)
       (if (not(=(transform L) k))

         
;(addx 1 2 '() f)
;(addx 2 3 '( ((2) 1) ) f)
;(addx 2 6 '( ((2) 1) ((3) 2) ) f)
;(addx 3 8 '(((2) 1) ((3 6) 2))  f)
;(addx 3 4 '(((2) 1) ((3 6) 2) ((8) 3))  f)
;(genome '(5 6 7 8 9 11) (random_L (length '(5 6 7 8 9 11)) 4 '()) '() )
;(addx (genome 6 5 '()) 2 '(((1) 1)) f)
;(addx (genome 6 5 '()) 2 '(((1) 1) ((2) 2)) f)
;(define (fitness genome L2)0)
;(addition '((7 8 11) 1) )
(fitness-x (transform '(1 2 3 5 7 7 8) (genome 7 4 '()) '()) (/ 33 4.))
;(population 500 0  4 '() '(1 2 3 5 7 7 8))



  
