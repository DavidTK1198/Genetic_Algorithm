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
(define addx;añade un elemento y lo relaciona a una "clase o grupo"
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
     (list (car L) (apply + (car L))));calcula la suma interna de los subgrupos


(define (transform L1 L2 L3);transforma la notación interna de los individuos
  (if (empty? L1);a la representación final para el usuario
      (map addition L3)
      (transform (cdr L1) (cdr L2) (addx (car L2) (car L1) L3 f))))

(define (low_fitness x y);Compara cual individuo posee mejor función de ajuste
   (< (last  x) (last y)))


(define (delete L)
    (filter(lambda (x) (> (last x) -1))L))

(define (print L)
  (display (transform (cadr L)  (car L) '() )));"Pintamos la representamos al usuario"

(define (done? L count gen ind e?)
  (print (car L))
  (newline)
   (if (not(= count gen))
     (done? (sortbyFitness(generation(select-pair L e?) '() (- ind 2) 0))
     (+ count 1) gen ind e?)
     (display "Proceso Terminado")
   ))
;------------------------------------------------------------------------------------

(define (genome L L2 k);creamos un individuo que tiene una representación interna como una lista que puede tomar valores
   (if (not (= (length L2) (length L)));aleatorios entre 1 y K
        (genome L (cons (random 1 (+ k 1)) L2)k);ejemplo (1 2 4 2 3 3 1)
            (list L2 L k)));esto me indica donde colocar los valores reales a la hora de hacer la conversión  
                           ;cada valor me indica a que subgrupo pertenece

(define (population size count k L);Creamos una población,"iterando" hasta llegar
    (if (not (= size count));a la cantidad digitada por el usuario
        (cons (genome L '() k) (population size (+ count 1) k L))
        '()
  ))

(define (fitness-x L mean)
         (if (empty? L)
             0
        (+  (abs(- (cadr (car L)) mean)) (fitness-x (cdr L) mean))
       )
  )

(define (mean L);recibe el individuo y acumula, la suma de cada subgrupo
    (if (empty? L)
         0
      (apply + (cons (cadr(car L)) (list(mean (cdr L) ))))))
             
  
  (define (fitness L)
      (if (not(= (length (transform (cadr L) (car L) '() )) (caddr L) ))
          (append L (list -1) )
       (if  ( < (length L) 4) 
(append L (list (fitness-x (transform (cadr L)  (car L) '() )  (round( /(mean (transform (cadr L)  (car L) '() )) (third L)))  )))
      L) ))

  
 
   (define (mutation L count r  r2 k);realiza una mutación de un elemento de la lisa de forma aleatoria
     (if (empty? L);recibe un r que es la posición donde se va a mutar
            L      ;es un numero aleatorio de 1 a  la logitud de la lista del individuo valores que puede tomar K
     (if (not (= r count))
         (cons (car L) (mutation (cdr L) (+ count 1) r r2 k))
         (if (= (car L) r2)
         (mutation L count r  (random 1 (+ k 1)) k)
        (cons r2 (cdr L))))));Se remplaza el valor
      
 (define (crossover-f L1 L2 r r2 count);Realice un corte en 2 puntos aleatorios en el primer padre
        (if (or (not(< r r2)) ( = r2 r));donde el segmento entre esos valores se separa y se une a los 
            (crossover-f L1 L2 (random 1  (length  L1)) (random 1  (length  L1)) count);extremos del segundo padre 
        (if (not (and (empty? L1) (empty? L2)));se valida que los numeros aleatorios esten es un rango optimo
           (if  (not (> count r))
           (cons (car L2) (crossover-f (cdr L1) (cdr L2) r r2 (+ count 1)))
           (if (not(> count r2))
           (cons (car L1) (crossover-f (cdr L1)  (cdr L2) r r2 (+ count 1)))
           (cons (car L2) (crossover-f (cdr L1)  (cdr L2) r r2 (+ count 1)))))
           L1)))
            
            
(define (crossover L1 L2);Función encargada de cruzar los padres selecionados
 (list (mutation (crossover-f (car L1) (car L2);se ejecuta un cruce de 2 puntos de forma aleatoria
             (random 2 (length (car L1)) ) (random 2 (length (car L1))) 0) 0 (random 1  (length (car L1)))
          (random 2 (+ (third L1) 1))   (third L1)) (cadr L1) (third L1)))


 (define (generation L L2 size count)
    (if (empty? L2)
   (append (generation L  (fitness (crossover (car L) (cadr L)))  size (+ count 1)) (list(car L)) (list(cadr L)))
     (if (not (= size count))
     (if  (or(> (last L2) (last(car L))) (< (last L2) 0))
   (generation L (fitness(crossover (car L) (cadr L))) size count)
  (cons L2 (generation L (fitness(crossover (car L) (cadr L))) size (+ count 1)))
 )'())))
           
(define (sortbyFitness population)
   (delete (sort (map fitness population) low_fitness)));ordena a los individuos en base a su función de ajuste
                                                       ;Aplica pena de muerte a los individuos no aptos

(define (select-pair population e?);en este caso se seleciono elitismo basado en los 2 mejores de cada generación
  (if (equal? e? #f);si se aplica elitismo se seleciona a los 2 mejores que estan en la primera posición
    (list (car population) (cadr population));si no se realiza de eligen los padres de forma aleatoria
    (list (car (shuffle population)) (cadr (shuffle population))))) 

(define (resolver-x  population count gen ind)
   (done? (sortbyFitness population) count gen ind #f))

(resolver-x (population 30 0 4 '( 2 3 5 6 9 32 1 2 5 12 31 15)) 0 900 80)



  
