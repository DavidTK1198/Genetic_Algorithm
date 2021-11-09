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
    (= x (cadr C) );evalua que el número a insertar pertenezca a ese subgrupo
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
    (filter(lambda (x) (> (last x) -1))L));elimina los individuos que possen listas vacías

(define (print L)
  (display (transform (cadr L)  (car L) '() )));"Pintamos la representamos al usuario"

(define (done? L count gen ind e?);función de apoyo encargada de "iterar" cada generación
  (print (car L));pinta el mejor de cada generación
  (newline)
   (if (not(= count gen));mientras el contador no sea = a la generación
      (if (equal? e? #t);si se usa elitismo se recuperaran los mejores 2 de cada generación(existen varias formas de aplicarlo)
     (done? (sortbyFitness(append (generation L '() (select-pair L) ind  0)  (list (car L)) (list (cadr L))));ordenamos por
     (+ count 1) gen ind e?);función objetivo cada generación y realizamos un llamado recursivo
     (done? (sortbyFitness(generation L '() (select-pair L) ind  0))
     (+ count 1) gen ind e?))
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

(define (fitness-x L mean);recibe cada subgrupo y el promedio
         (if (empty? L);calcula las diferencias de cada subgrupo y las suma
             0
        (+  (abs(- (cadr (car L)) mean)) (fitness-x (cdr L) mean))
       )
  )

(define (total L);recibe el individuo y acumula, la suma de cada subgrupo
    (if (empty? L)
         0
      (apply + (cons (cadr(car L)) (list(total (cdr L) ))))))
  
  (define (fitness L);calcula la función de ajuste/función objetivo en está implementación
      (if (not(= (length (transform (cadr L) (car L) '() )) (caddr L) ));si los grupos no son del tamaño K
          (append L (list -1) );Donde K indica cuantos subgrupos son, retorna -1 al no ser optimó
       (if  ( < (length L) 4);caso contrario llama a la funcion fitness-x para realizar el total y dividirlo entre K
(append L (list (fitness-x (transform (cadr L)  (car L) '() )  (round( /(total (transform (cadr L)  (car L) '() )) (third L)))  )))
      L) ));nota la función transform convierte un individuo como (1 2 3 4 4 3 3) a la representación real que se muestra al
           ;usuario, de está manera logramos obtener los valores para calcular la función objetivo
 
   (define (mutation L count r  r2 k);realiza una mutación de un elemento de la lisa de forma aleatoria
     (if (empty? L);recibe un r que es la posición donde se va a mutar
            L      ;es un numero aleatorio de 1 a  la logitud de la lista del individuo valores que puede tomar K
     (if (not (= r count));mientras no encuentre el punto a puntar avanza
         (cons (car L) (mutation (cdr L) (+ count 1) r r2 k))
         (if (= (car L) r2);revisamos que el punto a cambiar no sea el mismo al de la r2(gen mutado)
         (mutation L count r  (random 1 (+ k 1)) k)
        (cons r2 (cdr L))))));Se remplaza el valor
      
 (define (crossover-f L1 L2 r r2 count);Realiza un corte en 2 puntos aleatorios en el primer padre
        (if (or (not(< r r2)) ( = r2 r));donde el segmento entre esos valores se separa y se une a los 
            (crossover-f L1 L2 (random 1  (length  L1)) (random 1  (length  L1)) count);extremos del segundo padre 
        (if (not (and (empty? L1) (empty? L2)));se valida que los numeros aleatorios esten es un rango optimo
           (if  (not (> count r));mientras no sea el primer punto de corte avanza
           (cons (car L2) (crossover-f (cdr L1) (cdr L2) r r2 (+ count 1)))
           (if (not(> count r2));avanza hasta encontrar el segundo punto de corte
           (cons (car L1) (crossover-f (cdr L1)  (cdr L2) r r2 (+ count 1)))
           (cons (car L2) (crossover-f (cdr L1)  (cdr L2) r r2 (+ count 1)))));realiza la construción del nuevo individuo
           L1)))
     ;recibe 2 padres y los respectivos puntos a realizar el corte       
            
(define (crossover L1 L2);wrapper de la Función encargada de cruzar los padres selecionados
 (list (mutation (crossover-f (car L1) (car L2);se ejecuta un cruce de 2 puntos de forma aleatoria
             (random 2 (length (car L1)) ) (random 2 (length (car L1))) 0) 0 (random 1  (length (car L1)))
          (random 1 (+ (third L1) 1))   (third L1)) (cadr L1) (third L1)));luego se encarga de realizar una mutación a cada nuevo
                                                                          ;hijo

 (define (generation L L2 L3 size count);función encargada de generar cada generación
    (if (empty? L2);recibe la población original,una sublista vacía y los padres selecionados
    (generation L (fitness (crossover (car L3) (cadr L3))) (select-pair L) size count)
     (if (not (= size count));mientras no alcanze el tamaño de la generación continuara "iterando"
     (if  (or(> (last L2) (last(car L))) (< (last L2) 0))
  (generation L (fitness (crossover (car L3) (cadr L3))) (select-pair L) size count)
(cons L2  (generation L (fitness (crossover (car L3) (cadr L3))) (select-pair L) size (+ count 1)))
)'())))
           
(define (sortbyFitness population)
   (delete (sort (map fitness population) low_fitness)));ordena a los individuos en base a su función de ajuste
                                                       ;Aplica pena de muerte a los individuos no aptos

(define (select-pair population);Aplicamos en metodo de seleción donde elije al mejor individuo con cualquier otro 
    (list (car   population) (cadr (shuffle population)))) 

(define (resolver-x  population count gen ind e?);wrapper de la función que utiliza el usuario
   (done? (sortbyFitness population) count gen ind e?))

(define (resolver gen ind e? k L);función pricipal que usa el usuario
  (resolver-x (population ind 0 k L) 0 gen ind e?))
  

;(resolver 500 30 #t 4 '(30 60 90 25 20 15 16 120 200 43 18 30 30))
 (resolver 500 6 #t 4 '(1 2 3 5 6 9 32 1 2 5 12 31 15)) 
