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

 (define (substract lst x);No utilizada
  (map (lambda (n) (abs(- x n))) lst))

;Funciones de apoyo
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

(define (addition L)
     (list (car L) (foldl + 0 (car L))))

(define (transform L1 L2 L3)
  (if (empty? L1)
     (map addition L3)
      (transform (cdr L1) (cdr L2) (addx (car L2) (car L1) L3 f))))

(define (low_fitness x y)
   (< (last  x) (last y)))


(define (delete L)
    (filter(lambda (x) (> (last x) -1))L))
;------------------------------------------------------------------------------------

(define (genome L L2 k)
   (if (not (= (length L2) (length L)))
        (genome L (cons (random 1 (+ k 1)) L2)k)
            (list L2 L k)))  
         

(define (population size count k L);Creamos una población,"iterando" hasta llegar
    (if (not (= size count));a la cantidad digitada por el usuario
        (cons (genome L '() k) (population size (+ count 1) k L))
        '()
  ))

(define (fitness-x L mean)
         (if (empty? L)
             0
        (+ (expt (abs(- (cadr (car L)) mean))2) (fitness-x (cdr L) mean))
       )
  )
  
  (define (fitness L)
      (if (not(= (length (transform (cadr L) (car L) '() )) (caddr L) ))
          (append L (list -1) )
      (append L (list (fitness-x (transform (cadr L)  (car L) '() ) (/ (apply + (car L)) (caddr L)))))
      ))


      
;(genome '(1 2 4 5 6 7 8) '() 4)

(define (select-pair population)
  (delete (sort (map fitness population) low_fitness)))

(select-pair (population  30 0 4 '(1 2 4 5 6 7 8)))


  
