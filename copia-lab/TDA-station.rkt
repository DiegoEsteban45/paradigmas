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

(define(get-distance-section section)
  (third section))

(define(get-cost-section section)
  (fourth section))


;TDA line (3)

;CONSTRUCTOR(3)

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
  (if(empty? list-ids)
     #t
     (if(id-in-list? id list-ids)
        #f
        (check-ids (car list-ids) (cdr list-ids)))))
  (check-ids (car list-ids)(cdr list-ids)))

(define (check-consistency sections-line)           ;dominio: sections-line(list of sections associated with a line) recorrido: boolean
  (if (<= (length sections-line) 2)                         
      (if(equal? (first(second sections-line)) (second (first sections-line)))
         #t #f)            
      (if(and (equal? (first(second sections-line))(second(first sections-line)))(not(equal? (fourth (first (second sections-line))) t)))
         (check-consistency (cdr sections-line))
         #f)))

(define (check-id-n-rt-line id-line name-line rail-type)
  (if(and
      (number? id-line)
      (< 0 id-line)
      (string? name-line)
      (string? rail-type))
  #t #f))

(define (line id-line name-line rail-type . sections) ; dominio: id-line (int) x name-line (string) x rail-type(string) x sections(a minimum of two sections)
  (if (and (not (= (length sections) 0))(check-id-n-rt-line id-line name-line rail-type))                           ; recorrido: line
          (if (and (equal? (get-type-station(second (last sections))) t)
                   (equal? (get-type-station(first (first sections))) t)
                   (not(equal?(second (last sections))(first (first sections)))))
               (if (check-consistency sections)
                   (if (check-ids-line-consistency (get-ids-station sections))
                       (list id-line name-line rail-type sections)
                       (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                   (list -1 "undefined-line" "undefined-rail-type" undefined-section))
               (if (equal? (second (last sections)) (first (first sections))) ;circular 
                   (if (check-consistency sections)
                       (if(check-ids-line-consistency (cdr(get-ids-station sections)))
                          (list id-line name-line rail-type sections)
                          (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                       (list -1 "undefined-line" "undefined-rail-type" undefined-section))
                   (list -1 "undefined-line" "undefined-rail-type" undefined-section)))
          (list -1 "undefined-line" "undefined-rail-type" undefined-section)))

;PERTENENCIA

(define (line? line) 
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
          #f))

(define (check-circularity-line line)
  (if (line? line)
      (if(equal? (second(last(last line))) (first(first(last line))))
         #t #f)
      #f))

;SELECTORES


;MODIFICADORES OTRAS-FUNCIONES

(define (line-length line)
  (if(not(= -1(first line)))
     (apply + (map get-distance-section (last line)))
     -1))

(define(line-cost line)
  (define(cost sections)
    (if(empty? sections)
     0
     (+ (get-cost-section(first sections)) (cost (cdr sections)))))
  (cost (last line)))

(define (get-order-stations s1 sections)
    ( -
      (length
       (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections)))))
      (length
       (member s1 
     (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections))))))                
  ))

(define (get-lenght index-s1 index-s2 sections dis)
                  (if (= 0 index-s2)
                      dis
                      (get-lenght (+ 1 index-s1) (- index-s2 1) sections (+ dis (get-distance-section (list-ref sections index-s1))))))

(define (line-section-length station1 station2 line)
  (if (not(empty? (last line)))
      (if (< (get-order-stations station1(last line)) (get-order-stations station2 (last line)))            ; quiere decir que la station 1 esta antes        
          (if (<= 0 (get-order-stations station1 (last line)))
              (get-lenght (get-order-stations station1 (last line))
                          (- (get-order-stations station2 (last line)) (get-order-stations station1 (last line)))
                          (last line)
                          0)
                    0)
          (if (<= 0 (get-order-stations station2 (last line)))
              (get-lenght  (get-order-stations station2 (last line))
                          (- (get-order-stations station1 (last line)) (get-order-stations station2 (last line)))
                          (last line)
                          0)
                    0))
      0))
      
            
            
                
                
                
                
                    
                
          
  









          
          
               
                   
                   
                       
                          
                   
               
                    
               
               
          
          
           
     
          
      

;pruebitas
;(define (line id-line name-line rail-type . sections)
;  (list id-line name-line rail-type (first(second sections))(second(first sections))))


;(section (station 01 "musach" r 23 ) (station 02 "mschile" r 23 ) 455 23)
;(section (station 02 "mschile" r 23 ) (station 03 "mlol" r 23 ) 455 23)






