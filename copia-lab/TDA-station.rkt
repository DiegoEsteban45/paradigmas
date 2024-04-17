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

  (define (section? Section)
   (if(and (= 4 (length Section))
      (not (equal? (section (first Section) (second Section) (third Section) (fourth Section)) undefined-section)))
     #t
     #f))

;SELECTORES (2)

(define (get-section-point1 section)
  (first section))

(define (get-section-point2 section)
  (second section))

(define(get-distance-section section)
  (third section))

(define(get-cost-section section)
  (fourth section))

;OTRAS FUNCIONES

(define (swap-stations sections)
  (section (second sections) (first sections) (third sections) (fourth sections)))


;TDA line (3)

;CONSTRUCTOR(3)
(define undefined-line (list -1 "undefined-line" "undefined-rail-type" undefined-section))
;------------------------------------------------------------------------------ordenar-los-tramos-----------------------------------------------------------------------------------------------------------------------------
(define (sort-sections sections)
  
  (define (check-start-terminal section) 
    (equal? (get-type-station (get-section-point1 section)) t ))
  
  (define (check-finish-terminal section)
    (equal? (get-type-station (get-section-point2 section)) t ))
  
  (define (get-nex-section section nex-section )
    (equal?(get-section-point2 section)(get-section-point1 nex-section)))
  
  (define (get-sort-sections sections first-section)
  (with-handlers ([exn:fail? (lambda (exn) (cons undefined-section null))]) ; Si ocurre un error
    (if (= 1 (length sections))
        (cons (car sections) null)
        (cons first-section (get-sort-sections (remove first-section sections) (car (filter (lambda (nex-section)(get-nex-section first-section nex-section )) sections)))))))
  
  (define (check-start-circular section-start section-end)
    (equal? (get-section-point1 section-start) (get-section-point2 section-end)))
  
  (define (search-start-circular sections) ;me busca la primera seccion en ian linea circular
    (with-handlers ([exn:fail? (lambda (exn) undefined-section null)])
    (if (empty? sections)
        undefined-section
        (if(empty? (filter(lambda (section-end)(check-start-circular (car sections) section-end) sections)))
           (search-start-circular (cdr sections))
           (car (filter(lambda (section-end)(check-start-circular (car sections) section-end) sections)))))))
  
  (if (and (= 1 (length( filter check-start-terminal sections))) (= 1 (length( filter check-finish-terminal sections)))) ;vemos si existe un unico tramo con una estacion inicical "t" y final "t"
      (get-sort-sections sections (car ( filter check-start-terminal sections))) ;retorno las secciones ordenadas de una linea t-t
      (get-sort-sections sections (search-start-circular sections))))               ;retorno las secciones oredenadas de una linea circular
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (get-ids-station sections-line)     ;dominio: sections-line(list of sections associated with a line)
  (if (< 1 (length sections-line))              ;reocorrido: list-ids-station(list of station IDs belonging to a line) 
      (cons (get-id-station (get-section-point1 (first sections-line)))
            (get-ids-station (cdr sections-line)))
      (cons (first (get-section-point1 (first sections-line)))
            (cons (get-id-station (get-section-point2 (first sections-line))) '())
            )))

(define (id-in-list? id list-ids)     ; dominio: id(station id) x list-ids(station ids)
  (if(member id list-ids) #t #f))   ; recorrido: boolean

(define (check-ids-line-consistency list-ids) ; list-ids(station ids)
  (define (check-ids id list-ids)
    (if (not(> 0 id))
        (if(empty? list-ids)
           #t
           (if (and (id-in-list? id list-ids)(> 0 id)) 
               #f
               (check-ids (car list-ids) (cdr list-ids))))
        #f))
  (check-ids (car list-ids)(cdr list-ids)))

(define (check-consistency sections-line);dominio: sections-line(list of sections associated with a line) recorrido: boolean
  (if (section? (car sections-line))
      (if (not(<= (length sections-line) 1))
          (if (= (length sections-line) 2)                         
              (if(equal? (first(second sections-line)) (second (first sections-line))) 
                 #t #f)            
              (if(and (equal? (first(second sections-line))(second(first sections-line)))(not(equal? (fourth (first (second sections-line))) t)))
                 (check-consistency (cdr sections-line))
                 #f))
          (if(= (length sections-line) 0)
             #f #t))
  #f))
  
(define (check-id-n-rt-line id-line name-line rail-type)
  (if(and
      (number? id-line)
      (< 0 id-line)
      (string? name-line)
      (string? rail-type))
  #t #f))



(define (line id-line name-line rail-type . sections) 
  (let ([sorted-sections (sort-sections sections)])                               ; Almacena el resultado de sort-sections en sorted-sections
    (if  (and (not (= (length sorted-sections) 0))(check-id-n-rt-line id-line name-line rail-type))                           
        (if (and (equal? (get-type-station(second (last sorted-sections))) t)
                 (equal? (get-type-station(first (first sorted-sections))) t)
                 (not(equal?(second (last sorted-sections))(first (first sorted-sections)))))
                 (if (check-ids-line-consistency (get-ids-station sorted-sections))
                     (list id-line name-line rail-type sorted-sections)
                     undefined-line)
             (if (and (equal? (second (last sorted-sections)) (first (first sorted-sections))) (and (not (equal? (get-type-station(second (last sorted-sections))) t )) (not (equal? (get-type-station(first (first sorted-sections))) t ))))  ;circular 
                     (if(check-ids-line-consistency (cdr(get-ids-station sorted-sections)))
                        (list id-line name-line rail-type sorted-sections)
                        undefined-line)
                     undefined-line))
                 undefined-line)))

;PERTENENCIA

(define (line? line)
  (if(= 4 (length line))
     (if (and (not (= (length (last line)) 0))(check-id-n-rt-line (first line) (second line) (third line)))                          
         (if (and (equal? (get-type-station(second (last (last line)))) t)
                  (equal? (get-type-station(first (first (last line)))) t)
                  (not(equal?(second (last (last line)))(first (first (last line))))))
             (if (check-consistency (last line))
                 (if (check-ids-line-consistency (get-ids-station (last line)))
                     #t #f)
                 #f)
             (if (equal? (second (last (last line))) (first (first (last line)))) ;circular 
                 (if (check-consistency (last line))
                     (if(check-ids-line-consistency (cdr(get-ids-station (last line))))
                        #t
                        #f)
                     #f)
                 #f))
         #f)
     #f))
  

(define (check-circularity-line line)
  (if (line? line)
      (if(equal? (second(last(last line))) (first(first(last line))))
         #t #f)
      #f))

;SELECTORES


;MODIFICADORES OTRAS-FUNCIONES

(define (get-order-stations s1 sections)
    ( -
      (length
       (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections)))))
      (length
       (member s1 
     (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections))))))                
  ))

;---------------------------------------------------------------------largo-------------------------------------------------------------------------------------------------------

(define (line-length line)
  (if(not(= -1(first line)))
     (apply + (map get-distance-section (last line)))
     -1))

(define (line-section-length station1 station2 line)
  (define (get-lenght index-s1 index-s2 sections dis)
  (if (= 0 index-s2)
      dis
      (get-lenght (+ 1 index-s1) (- index-s2 1) sections (+ dis (get-distance-section (list-ref sections index-s1))))))
  
  (define(calculate-length station1 station2 line-sections)
    (if (< (get-order-stations station1 line-sections)(get-order-stations station2 line-sections))               
        (if (<= 0 (get-order-stations station1 line-sections))
            (get-lenght (get-order-stations station1 line-sections)
                        (- (get-order-stations station2 line-sections) (get-order-stations station1 line-sections))
                        line-sections
                        0)
            0)
        (if (<= 0 (get-order-stations station2 line-sections))
            (get-lenght  (get-order-stations station2 line-sections)
                         (- (get-order-stations station1 line-sections) (get-order-stations station2 line-sections))
                         line-sections
                         0)
            0)))
  (if (line? line) 
      (calculate-length station1 station2 (last line))
      #f)) 
;-------------------------------------------------------------------------cost----------------------------------------------------------------------------------------------
(define(line-cost line)
  (define(cost sections)
    (if(empty? sections)
     0
     (+ (get-cost-section(first sections)) (cost (cdr sections)))))
  (cost (last line)))

(define (line-section-cost station1 station2 line)
  (define (calculate-cost index-s1 index-s2 sections cost)
    (if (= 0 index-s2)
        cost
        (calculate-cost (+ 1 index-s1) (- index-s2 1) sections (+ cost (get-cost-section (list-ref sections index-s1))))))
  
  (define (process-cost station1 station2 line-sections)
    (if (< (get-order-stations station1 line-sections)(get-order-stations station2 line-sections))               
        (if (<= 0 (get-order-stations station1 line-sections))
            (calculate-cost (get-order-stations station1 line-sections)
                            (- (get-order-stations station2 line-sections) (get-order-stations station1 line-sections))
                            line-sections
                            0)
            0)
        (if (<= 0 (get-order-stations station2 line-sections))
            (calculate-cost  (get-order-stations station2 line-sections)
                             (- (get-order-stations station1 line-sections) (get-order-stations station2 line-sections))
                             line-sections
                             0)
            0)))
  (if (line? line)
      (process-cost station1 station2 (last line))
      #f))
;--------------------------------------------------------------------------agregar section------------------------------------------------------------------------------------------

#|
(define (line-add-section line section)
  (define (equal-sections? section sections) ; verificamos si la section ya existe
    (if (empty? sections)
        #t
    (if(not(or
        (and (stations-equal? (get-point1-section(section)) (get-poitn1-section(car (sections)))); comparamos si las estaciones inciales de un tramo son iguales 
             (stations-equla? (get-point2-section(section)) (get-poitn2-section(car (sections))))) ;comparamos si las estaciones finales de un tramo son iguales 
        (and (stations-equal? (get-point1-section(section)) (get-poitn1-section (swap-stations(car (sections)))))
             (stations-equal? (get-point1-section(section)) (get-poitn2-section (swap-stations(car (sections))))))))
       #f)
    (equal-sections? section (cdr sections))))

  (define (make))

  
                                      
  (if (and (line? line) (section? section))
      (if (not(equal-sections? section (last(line))))
          (if (and (equal? (get-point2-section section) (get-point1-section (car (last line)))) (equal? (get-type (get-point1-section section) t)))
             
     --------------         
|#


      
      
  

(define salto "---------")
(sort-sections (list (section (station 04 "mdelta" c 23) (station 05 "mepsilon" r 23) 410 36)
(section (station 02 "mbeta" r 23) (station 03 "mgamma" m 23) 310 31)
(section (station 06 "mzeta" r 23) (station 07 "meta" t 23) 610 46)
(section (station 03 "mgamma" m 23) (station 04 "mdelta" c 23) 410 36)
(section (station 05 "mepsilon" r 23) (station 06 "mzeta" r 23) 510 41)
(section (station 01 "malpha" t 23) (station 02 "mbeta" r 23) 210 26)))

salto
salto
salto
 
(sort-sections (list (section (station 06 "msixth" r 23) (station 01 "mstart" t 23) 700 50)
(section (station 05 "mfifth" r 23) (station 06 "msixth" r 23) 600 45)
(section (station 04 "mfourth" c 23) (station 05 "mfifth" r 23) 500 40)
(section (station 03 "mthird" m 23) (station 04 "mfourth" c 23) 400 35)
(section (station 02 "msecond" r 23) (station 03 "mthird" m 23) 300 30)
(section (station 01 "mstart" t 23) (station 02 "msecond" r 23) 200 25)
))

salto
salto
salto
 
(sort-sections (list (section (station 03 "mthird" m 23) (station 04 "mfourth" c 23) 400 35)
(section (station 06 "msixth" r 23) (station 01 "mstart" t 23) 700 50)
(section (station 02 "msecond" r 23) (station 03 "mthird" m 23) 300 30)
(section (station 01 "mstart" t 23) (station 02 "msecond" r 23) 200 25)
(section (station 05 "mfifth" r 23) (station 06 "msixth" r 23) 600 45)
))

salto
salto
salto

(line 01 "linea1" "noc" (section (station 04 "mdelta" c 23) (station 05 "mepsilon" r 23) 410 36)
(section (station 02 "mbeta" r 23) (station 03 "mgamma" m 23) 310 31)
(section (station 06 "mzeta" r 23) (station 07 "meta" t 23) 610 46)
(section (station 03 "mgamma" m 23) (station 04 "mdelta" c 23) 410 36)
(section (station 05 "mepsilon" r 23) (station 06 "mzeta" r 23) 510 41)
(section (station 01 "malpha" t 23) (station 02 "mbeta" r 23) 210 26)
)
salto
salto
salto

(sort-sections (list (section (station 04 "mthird" t 23) (station 03 "mfourth" t 23) 400 35) (section (station 05 "lol" t 23) (station 06 "ñp" t 23) 400 35)))

(line 02 "linea4" "lol" (section (station 04 "mthird" t 23) (station 03 "mfourth" t 23) 400 35) (section (station 05 "lol" t 23) (station 06 "ñp" t 23) 400 35))







 


