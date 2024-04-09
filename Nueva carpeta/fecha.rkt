#lang racket
;Implementación TDA Fecha
;Representación
;(Mes) Int X (Dia) Int X (Año) Int

;constructor
(define mes?                ;construyo un mes 
  (lambda (m)
     (and (integer? m)
          (> m 0) (< m 13))))

(define dia?                  ;construyo un dia
  (lambda (d)
    (and (integer? d)
         (> d 0) (< d 31))))

(define agno? integer?) ;contruyo un año

(define fecha                ;construyo la fecha completa
  (lambda (dia mes agno)
    (if (and (dia? dia)
             (mes? mes)
             (agno? agno))
         (list dia mes agno)
         null)))

;pertencia
(define fecha?
  (lambda (f)
    (and (list? f)
         (= (length f) 3)
         (dia? (car f))
         (mes? (car (cdr f)))
         (agno? (car (cdr (cdr f)))))))

(define fecha2?
  (lambda (f)
    (and (list? f)
         (= (length f) 3)
         (not (null? (fecha (car f)
                (cadr f)
                (caddr f))))
         )))

;selectores
(define dia
  (lambda (f)
    (if (fecha? f)
        (car f)
        0)))

(define dia2 car)
(define mes cadr)
(define agno caddr)



(define fecha->string
  (lambda (f)
    (string-append
       (dia->string (dia f))
       " de "
       (mes->string (mes f))
       " del "
       (agno->string (agno f)))))


(define dia->string number->string)

(define agno->string number->string)

(define mes->string
  (lambda (m)
    (cond
      [(= m 1) "Enero"]
      [(= m 2) "Febrero"]
      [(= m 3) "Marzo"]
      [(= m 4) "Abril"]
      [(= m 5) "Mayo"]
      [(= m 6) "Junio"]
      [(= m 7) "Julio"]
      [(= m 8) "Agosto"]
      [(= m 9) "Septiembre uyuyui"]
      [(= m 10) "Octubre"]
      [(= m 11) "Noviembre"]
      [(= m 12) "Diciembre"]
      [else "?"])))
          

