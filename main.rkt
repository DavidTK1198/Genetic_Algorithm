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
     (list (car L) (apply + (car L))))


(define (transform L1 L2 L3)
  (if (empty? L1)
      (map addition L3)
      (transform (cdr L1) (cdr L2) (addx (car L2) (car L1) L3 f))))

(define (low_fitness x y)
   (< (last  x) (last y)))


(define (delete L)
    (filter(lambda (x) (> (last x) -1))L))

(define (print L)
  (display (transform (cadr L)  (car L) '() )))

(define (done? L count gen ind )
  (display (car L))
  (newline)
   (if (not(= count gen))
     (done? (select-pair (crossover (select-pair L) '() ind 0))
     (+ count 1) gen ind)
     #t
   ))
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

(define (mean L)
    (if (empty? L)
         0
      (apply + (cons (cadr(car L)) (list(mean (cdr L) ))))))
             
  
  (define (fitness L)
      (if (not(= (length (transform (cadr L) (car L) '() )) (caddr L) ))
          (append L (list -1) )
       (if  ( < (length L) 4) 
(append L (list (fitness-x (transform (cadr L)  (car L) '() )  (round( /(mean (transform (cadr L)  (car L) '() )) (third L)))  )))
      L) ))

  
 
   (define (mutation L count r k)
     (if (empty? L)
         L
     (if (not (= r count))
         (cons (car L) (mutation (cdr L) (+ count 1) r k))
        (cons (random 1 (+ k  1)) (cdr L)))))
      
 (define (crossover-f L1 L2 r count)
        (if (not (and (empty? L1) (empty? L2)))
           (if   (< count r)
           (cons (car L1) (crossover-f (cdr L1) (cdr L2) r (+ count 1)))
           (cons (car L2) (crossover-f (cdr L1) (cdr L2) r (+ count 1))))
           L1))
            
            
(define (crossover-x L1 L2)
 (list (mutation (crossover-f (car L1) (car L2)
              (random 1 (+ (length (car L1))  1))0) 0 (random 1 (+ (length (car L1))  1))
             (third L1)) (cadr L1) (third L1)))


 (define (crossover L L2 size count)
    (if (empty? L2)
   (crossover L (fitness (crossover-x (car (shuffle L)) (cadr (shuffle L)))) size count)
     (if (not (= size count))
     (if  (or(> (last L2) (last(car L))) (< (last L2) 0))
 (crossover L (fitness(crossover-x (car L) (cadr L))) size count)
(cons L2 (crossover L (fitness(crossover-x (car L) (cadr L))) size (+ count 1)))
)'())))
         

         
     
(define (select-pair population)
  (delete (sort (map fitness population) low_fitness)))

 (define (resolver-x  population count gen ind)
   (done?
    (select-pair population) count gen ind))

(resolver-x (population 20 0 4 '( 2 3 5 6 9 32 1 2 5 12 31 15)) 0 500 5)



  
