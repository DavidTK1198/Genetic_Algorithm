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
  (filter(lambda (x) (= x 1))L));Eliminamos los números 0 de la lista


(define f
  (lambda (L n)
     (if (not (= (length (car L)) n));comprobamos que la sublista binaria de cada subgrupo
       #t #f)                        ;posean el tamaño adecuado
    ))

       
(define addx ;función encargada de añadir o crear una lista con sublistas
  (lambda (x C f? n );basado en cierto criterio determinado por la función f
    (cond ((empty? C);en este caso f nos indica si cada sublista posee
           (list (list x)));la longitud adecuada basado en la lista de
          ((f? C n);números "reales"
          (cons (append (car C) (list x)) (cdr C)))
          (else
           (cons (car C) (addx x (cdr C) f?  n)))
          )))


(define (genome n k L);Creamos una sublista con valores binarios
   (if (not (= (length L)k));la cual nos ayudara a representar
       (genome n k (addx (random 0 2) L f n ));los diferentes valores que pueden ser representados
       (if (not (=(length (last L))n));basados en el problema de la mochila
          (genome n k (addx (random 0 2) L f n ));se agrupa un individuo el cual posee sublistas con 0 y 1
       L))) ;los cuales se utilizan como un wrapper para los valores posibles basado en la lista recibida por el usuario


(define (population  size count n k L);Creamos una población,"iterando" hasta llegar
    (if (not (= size count));a la cantidad digitada por el usuario
        (cons (genome n k L) (population size (+ count 1) n k L))
        L
  ))
(population 7 0 5 4 '())

  
