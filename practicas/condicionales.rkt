#lang racket

;clase 5 condicionales


(define (son-iguales? X Y)
  (equal? X Y))

;(and "parametros" ) -> si son todos verdaderos, devueleve #t si no, #f
;(or  "parametros" ) -> si alguno es verdaderos, devueleve #t si no, #f
; (not "parametros") -> niega los que sea


(define (que-soy x)
  (cond ;con "cond" puedo concatenar condiciones ya que anidar muchos if puede resultar mal para la maquina
    [(string? x) "soy un strign"]
    [(number? x)  "soy un numero"]
    [(boolean? x)   "soy un booleano"]
    [(char? x )  "coy un caracter"]
    [else "no c lo que soy"]))







