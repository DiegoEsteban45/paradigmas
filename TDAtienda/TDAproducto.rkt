#lang racket

;TDA PRODUCTO

(define(make-product SKU NAME PRICE )
  (if(and)))




;pertencia

(define (is-valid-product product)
  (if ( and (number? (car product))
            (> (car product) 0)
            (string? (second product))
            (number? (third product))
            (> (third product) 0)
            (number? (fourth product))
            (> (fourth product) 0)
            (<= (fourth product) 100)
            (boolean? (fifth product))
            )#t
             #f))

