#lang racket

;TDA STATION

;CONSTRUCTORES
(define  r 1 )   ; tipo de estacion funcion r  devuelve 1 
(define m 2)  ; tipo de estacion funcion m devuelve 2
(define  c 3) ; tipo de estacion funcion c devuelve 3
(define  t 4 )  ; tipo de estacion funcion t devuelve 4
(define undefined-type -1)
(define undefined-station (list -1 "undefined-station" undefined-type -1))

(define (station id name type stop-time)
  (if (and
       (number? id)
       (> id 0)
       (string? name)
       (or (eq? type r)
            (eq? type m)
            (eq? type c)
            (eq? type t))
       (>= stop-time 0))
      (list id name type stop-time)
      undefined-station))

;PERTENENCIA

(define (is-a-type-station? Station)
  (cond
   [(eq?  (third Station) r)   #t]
   [(eq?  (third Station) m) #t]
   [(eq?  (third Station) c)  #t]
   [(eq?  (third Station) t)   #t]
   [(eq?  (third Station) undefined-type)  #f]
   [else #f]))

(define (is-station? Station)
  (if(and (= 4 (length Station))
      (not (equal? (station (first Station) (second Station) (third Station) (fourth Station)) undefined-station)))
     #t
     #f))

;SELECTORES
(define (get-id-station Station) ;dominio station, recorrido number (id station)
  (if(is-station? Station)
     (first Station)
     -1))

(define(get-name-station Station) ;dominio station, recorrido string (name station)
  (if(is-station? Station)
     (second Station)
  "station-null"))

(define (get-type-station Station) ;dominio station, recorrido function r,m,c,t (type station)
  (if (is-station? Station)
     (third Station)
   undefined-type ))

(define (get-stop-time-station Station) ;dominio station, recorrido number (stop-time station)
  (if (is-station? Station)
     (fourth Station)
  -1))

;TDA section - constructor (2)

(define undefined-section (list undefined-station undefined-station -1 -1))

(define (stations-equal? Station1 Station2)
  (if (and
       (is-station? Station1)
       (is-station? Station2)
       (or
        (equal? (car Station1)(car Station2))
        (equal? (second Station1)(second Station2))))
      #t #f))

(define (section point1 point2 distance cost) ; debe crear un tramo entre dos estaciones 
    (if (and
    (not(stations-equal? point1 point2))
    (number? distance)
    (> distance 0 )
    (number? cost)
    (>= cost 0))
       (list point1 point2 distance cost)
       undefined-section))

  ;PERTENENCIA (2)

  (define (is-a-section? Section)
   (if(and (= 4 (length Section))
      (not (equal? (section (first Section) (second Section) (third Section) (fourth Section)) undefined-section)))
     #t
     #f))

;SELECTORES (2)

(define (get-section-point1 section)
  (first section))

(define (get-section-point2 section)
  (second section))

;TDA line (3)

(define (get-ids-station sections-line)
  (if (< 1 (length sections-line)) 
      (cons (get-id-station (get-section-point1 (first sections-line)))
            (get-ids-station (cdr sections-line)))
      (cons (first (get-section-point1 (first sections-line)))
            (cons (get-id-station (get-section-point2 (first sections-line))) '())
            )))

(define (in-list? element L1) ; si el elemento esta verdadero, si no, falso
  (if(member element L1) #t #f))

(define (check-ids-line-consistency list-ids) ;chekea que las estaciones no se repitan verdadero si no se repiten, falso si se repiten
  (define (check-ids id list-ids)
  (if(empty? list-ids)
     #t
     (if(in-list? id list-ids)
        #f
        (check-ids (car list-ids) (cdr list-ids)))))
  (check-ids (car list-ids)(cdr list-ids)))

(define (Check-consistency sections) ; checkea si la linea esta bien constituida en cuanto a sus secciones solo para lineas con doble terminal 
            (if (<= (length sections) 2)
                (if(equal? (first(second sections)) (second (first sections)))
                   #t #f)            
                (if(equal? (first(second sections)) (second(first sections)))
                   (Check-consistency (cdr sections))
                   #f)))

(define (line id-line name-line rail-type . sections)
  (if (not (= (length sections) 0))
          (if (and (equal? (get-type-station(second (last sections))) t) (equal? (get-type-station(first (first sections))) t) (not(equal?(second (last sections))(first (first sections)))))
               (if (Check-consistency sections)
                   (if (check-ids-line-consistency (get-ids-station sections))
                       (list id-line name-line rail-type sections)
                       (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                   (list -1 "undefined-line" "undefined-rail-type" undefined-section))
               (if (equal? (second (last sections)) (first (first sections))) ;circular 
                   (if (Check-consistency sections)
                       (if(check-ids-line-consistency (cdr(get-ids-station sections)))
                          (list id-line name-line rail-type sections)
                          (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                       (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                   (list -1 "undefined-line" "undefined-rail-type" undefined-section)))
          (list -1 "undefined-line" "undefined-rail-type" undefined-section)))


          
          
               
                   
                   
                       
                          
                   
               
                    
               
               
          
          
           
     
          
      

;pruebitas
;(define (line id-line name-line rail-type . sections)
;  (list id-line name-line rail-type (first(second sections))(second(first sections))))


;(section (station 01 "musach" r 23 ) (station 02 "mschile" r 23 ) 455 23)
;(section (station 02 "mschile" r 23 ) (station 03 "mlol" r 23 ) 455 23)






