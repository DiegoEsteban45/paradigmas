#lang racket
; clase 4
(define lista0(list 1 2 3 4 5 6 7 8 9 10 11))
(define lista1(list 1 2 3))
(define lista2(list 4 5 6))
(define lista3 null)


(define (add-first-element N lista)(cons N lista)) ; agrega elemento al inicio

(define (add-end-element N lista)(if (null? lista) ; agrega elemento al final
                                     (cons N null)
                                     (cons (car lista)(add-end-element N (cdr lista)))))

(define (my-append L1 L2) ;une dos listas
                            (if(null? L1)
                            (if (not (null? L2)) ;cuando L1 este vacia y L2 no lo este
                            (cons (car L2) (my-append L1 (cdr L2)))
                                  null)
                            (cons (car L1) (my-append (cdr L1) L2 ))));si la primera condicon es falsa

(define (largo-lista L1);largo de la lista recursion de cola
  (define (inside-len L1 N)(if(null? L1)
                              N
    (inside-len (cdr L1) (+ N 1))))(inside-len L1 0))

(define (largo-lista-rn L1)(if(null? L1);largo de la lista recursion natural
                              0
                              (+ 1 (largo-lista-rn (cdr L1)))))

(define(my-list-ref L1 N)(if (not(null? L1)) ;me da los que estan en la N posicion
                            (if(= N 0)
                            (car L1)
                            (my-list-ref (cdr L1)(- N 1)))#f))

;funciones a considerar

(define (primerelemento lista) (first lista))

(define (el-resto lista) (rest lista))

(define (segundoelemento lista) (second lista))

(define (tercerelemento lista) (third lista))

(define (cuartoelemento lista) (fourth lista))

(define (quintoelemento lista) (fifth lista))

(define (decimoelemento lista) (tenth lista))

(define (ultimo-elemto lista)(last lista))

















