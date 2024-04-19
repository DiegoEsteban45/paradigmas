#lang racket

;TDA STATION

;CONSTRUCTORES
(define  r 1 )   ; tipo de estacion funcion r  devuelve 1 
(define m 2)  ; tipo de estacion funcion m devuelve 2 mantencion
(define  c 3) ; tipo de estacion funcion c devuelve 3 
(define  t 4 )  ; tipo de estacion funcion t devuelve 4 terminal
(define undefined-type -1)
(define undefined-station (list -1 "undefined-station" undefined-type -1))

(define (station id name type stop-time)
  (if (and
       (number? id)
       (>= id 0)
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

(define (check-ids-line-consistency list-ids)
  (define (myequal? x y)
    (= x y))
  (define (check-ids id list-ids)
    (if (not (empty? list-ids))
        (if (and (not (empty? (filter (lambda (y) (myequal? id y)) list-ids))) (<= 0 id))
            #f
            (check-ids (car list-ids) (cdr list-ids)))
        #t))
  (if (> 0 (car list-ids))
      #f
      (check-ids (car list-ids) (cdr list-ids))))


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
      (<= 0 id-line)
      (string? name-line)
      (string? rail-type))
  #t #f))

(define (forma-parte-de-alguna-section? sections-m sections)
  (if (empty? sections-m)
     #t
  (if (ormap (lambda(station2)(equal? (get-section-point1 (car sections-m)) station2)) (map get-section-point1 sections))
      (forma-parte-de-alguna-section? (cdr sections-m) sections)
      #f)))

(define (is-not-type-m? section)
  (not(equal? (get-type-station (get-section-point2 section)) m)))

(define (is-type-m? section)
  (equal? (get-type-station (get-section-point2 section)) m))

(define (line id-line name-line rail-type . sections) 
  (let ([sorted-sections (sort-sections (remove-duplicates(filter is-not-type-m? sections)))][sections-m (filter is-type-m? sections)])    ; Almacena el resultado de sort-sections en sorted-sections
    (if  (and (not (= (length sorted-sections) 0))(check-id-n-rt-line id-line name-line rail-type))                           
        (if (and (equal? (get-type-station(second (last sorted-sections))) t)
                 (equal? (get-type-station(first (first sorted-sections))) t)
                 (not(equal?(second (last sorted-sections))(first (first sorted-sections)))))
                 (if (check-ids-line-consistency (get-ids-station sorted-sections))
                      (if (and (not(empty? sections-m)) (forma-parte-de-alguna-section? sections-m sorted-sections))
                        (list id-line name-line rail-type (append sorted-sections sections-m ))
                        (list id-line name-line rail-type sorted-sections))
                     undefined-line)
             (if (and (equal? (second (last sorted-sections)) (first (first sorted-sections)))
                      (and(not(equal?(get-type-station(second(last sorted-sections)))t))
                          (not(equal?(get-type-station(first (first sorted-sections))) t ))))  ;circular 
                     (if(check-ids-line-consistency (cdr(get-ids-station sorted-sections)))
                        (list id-line name-line rail-type sorted-sections)
                        undefined-line)
                     undefined-line))
                 undefined-line)))

;PERTENENCIA

(define (line? line)
(let ([line-1 (filter is-not-type-m? (last line))])
  (if( = 4 (length line))
     (if (and (not (= (length line-1) 0))(check-id-n-rt-line (first line) (second line) (third line)))                          
         (if (and (equal? (get-type-station(second (last line-1))) t)
                  (equal? (get-type-station(first (first line-1))) t)
                  (not(equal?(get-section-point2 (last line-1))(get-section-point1 (first line-1)))))
             (if (check-consistency line-1)
                 (if (check-ids-line-consistency (get-ids-station line-1))
                     #t #f)
                 #f)
             (if (equal? (get-section-point2 (last line-1))(get-section-point1 (first line-1))) ;circular 
                 (if (check-consistency line-1)
                     (if(check-ids-line-consistency (cdr(get-ids-station line-1)))
                        #t
                        #f)
                     #f)
                 #f))
         #f)
     #f)))
  
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
     (apply + (map get-distance-section (filter is-not-type-m? (last line))))
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
      (process-cost station1 station2 (filter is-not-type-m? (last line)))
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

(line 02 "linea2" "cdf" 
(section (station 1 "Neptuno" r 45) (station 2 "Pajaritos" c 45) 3 14)
(section (station 2 "Pajaritos" c 45) (station 3 "Las Rejas" r 45) 2.5 10)
(section (station 3 "Las Rejas" r 45) (station 4 "Ecuador" r 60) 4.5 17)
(section (station 10 "Los Héroes" c 60) (station 11 "La Moneda" r 40) 4.3 17)
(section (station 4 "Ecuador" r 60) (station 5 "San Alberto Hurtado" r 40) 4.7 18)
(section (station 5 "San Alberto Hurtado" r 40) (station 6 "Universidad de Santiago de Chile" c 40) 4.3 17)
(section (station 7 "Estación Central" c 45) (station 8 "Unión Latinoamericana" r 30) 2.5 10)
(section (station 8 "Unión Latinoamericana" r 30) (station 9 "República" r 40) 4.5 17)
(section (station 0 "San Pablo" t 90) (station 1 "Neptuno" r 45) 4 15)
(section (station 9 "República" r 40) (station 10 "Los Héroes" c 60) 4.7 18)
(section (station 2 "Pajaritos" c 45) (station 3 "Las Rejas" r 45) 2.5 10)
(section (station 6 "Universidad de Santiago de Chile" c 40) (station 7 "Estación Central" c 45) 3.8 12)
(section (station 11 "La Moneda" r 40) (station 12 "Universidad de Chile" c 90) 3.8 12)
(section (station 12 "Universidad de Chile" c 90) (station 13 "Santa Lucía" r 40) 4.5 17)
(section (station 13 "Santa Lucía" r 40) (station 14 "Universidad Católica" c 60) 4.7 18)
(section (station 15 "Baquedano" r 40) (station 16 "Los Dominicos" t 90) 4.2 17)
(section (station 1 "tu" r 45) (station 17 "Cochera Neptuno" m 3600) 3.8 12)
(section (station 14 "Universidad Católica" c 60) (station 15 "Baquedano" r 40) 4.3 17))










 








