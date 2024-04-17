#lang racket
; clase 4
(define lista0(list 1 2 3 4 5 6 7 8 9 10 11))
(define lista1(list 1 2 3))
(define lista2(list 4 5 6))
(define lista3 null)
(define caracteres "hola mundo como estan")

(define (add-first-element N lista)
  (cons N lista)) ; agrega elemento al inicio

(define (add-end-element N lista) ; agrega elemento al final
  (if (null? lista) 
      (cons N null)
      (cons (car lista)(add-end-element N (cdr lista)))))

(define (my-append L1 L2) ;une dos listas
  (if (null? L1)
      (if (not (null? L2)) ;cuando L1 este vacia y L2 no lo este
          (cons (car L2) (my-append L1 (cdr L2)))
          null)
      (cons (car L1) (my-append (cdr L1) L2 ))));si la primera condicon es falsa

(define (largo-lista L1);largo de la lista recursion de cola
  (define (inside-len L1 N)
    (if (null? L1)
       N
    (inside-len (cdr L1) (+ N 1))))
  (inside-len L1 0))

(define (largo-lista-rn L1) ;largo de la lista recursion natural
  (if (null? L1)
     0
     (+ 1 (largo-lista-rn (cdr L1)))))

(define(my-list-ref L1 N) ;me da los que estan en la N posicion
  (if (not (null? L1)) 
      (if (= N 0)
         (car L1)
         (my-list-ref (cdr L1)(- N 1)))
      #f))

(define (sumar-elementos-rn L1) ;suma los elemtos de una lista recursion natural
  (if (null? L1) 
      0
      (+ (car L1) (sumar-elementos-rn (cdr L1)))))

(define (sumar-elementos-rc L1) ;suma los elemtos de una lista recursion de cola
  (define (sum suma L1)
    (if (null? L1)
        suma
        (sum (+ suma (car L1)) (cdr L1))))
  (sum 0 L1))

(define (promedio listax);calcula el promedio de una lista
  (/(sumar-elementos-rc listax)(largo-lista listax)))

(define (media . parametros) ;el punto se utiliza para aplicar un indeterminado numero de parametros que seran tratados como lista
  (/(apply + parametros)(length parametros)))

(define (suma-o-resta boo pam1 pam2)
  ((if boo + -) pam1 pam2))

(define (suma-o-resta2 v-f pam1 pam2)
  (if v-f (+ pam1 pam2 )(- pam1 pam2)))

;funciones a considerar

(define (primerelemento lista) (first lista))

(define (el-resto lista) (rest lista))

(define (segundoelemento lista) (second lista))

(define (tercerelemento lista) (third lista))

(define (cuartoelemento lista) (fourth lista))

(define (quintoelemento lista) (fifth lista))

(define (decimoelemento lista)(tenth lista))

(define (ultimo-elemto lista)(last lista))

(define (longitud-de-la-lista lista)(length lista))

(define (unir-listas L1 L2)(append L1 L2))

(define (invertir L1)(reverse L1))

(define (es-parte-de-lista? element L1)(member element L1)) ;me da los elementos que se encuentran posterior al elemento que encuentro.

(define (aplicar-funcion F L1)(map F L1)) ; aplica la funcion F a todos los elementos de la lista (L1) ojo no modifica L1 solo L1 no es variable

(define (es-vacia? lista)(empty? lista));devuelve un boleano dependiendo de si la lista esta vacia, verdare si lo esta y falso si no

;(andmap funcBOOL nlistas) solo se aplica a funciones que usen booleanos y devolvera un boleano dependiendo de si la condicion que trate la funcion es verdadero a todos los

;elementos de la lista, el caso contrario devolvera un falso

;(ormap funcBOOL nlistas) lo msimo pero algun elemento debe cumplir la condicion

;(filter funcBOOL nlistas) filtra los elementos dependiendo de la condicion dada por funcBOOL

;(apply funcion lista) toma los parametros de una funcion por separados y aplica la funcion

;(procedure? func)    Verifica si func es una función

;(defined? func)       Verifica si func está definida


(define (esta? element L1)
  (if(member element L1) "esta" "no esta"))

(define (estaa? element L1)
  (if(member element L1) #t #f))

(define (get-ids-station2 . sections-line) ; dominio secciones-que componen una linea, recorrido ids de las estaciones que componen la linea. 
  (define (get-ids-station-envolotorio sections-line)
  (if (< 1 (length sections-line)) 
      (cons (get-id-station (get-section-point1 (first sections-line)))
            (get-ids-station-envolotorio (cdr sections-line)))
      (cons (first (get-section-point1 (first sections-line)))
            (cons (get-id-station (get-section-point2 (first sections-line))) '()))))
  (get-ids-station-envolotorio sections-line))


































