#lang racket
;TDA STATION

;CONSTRUCTORES
(define  r "regular" )   ; tipo de estacion funcion r  devuelve 1 
(define m "mantencion")  ; tipo de estacion funcion m devuelve 2 mantencion
(define  c "combinacion") ; tipo de estacion funcion c devuelve 3 
(define  t "terminal" )  ; tipo de estacion funcion t devuelve 4 terminal
(define undefined-type -1)
(define undefined-station (list -1 "undefined-station" undefined-type -1))

(define (station id name type stop-time)
    (if (and
          (number? id)(>= id 0)(string? name)
          (or (eq? type r)(eq? type m)(eq? type c)(eq? type t))(>= stop-time 0))
        (list id name type stop-time)
        undefined-station))

;PERTENENCIA
(define (is-station? Station)
  (if(and (= 4 (length Station))
      (not (equal? (station (first Station) (second Station) (third Station) (fourth Station)) undefined-station)))
     #t #f))

;SELECTORES
(define(get-id-station Station) ;dominio station, recorrido number (id station)
  (first Station))
    
(define(get-name-station Station) ;dominio station, recorrido string (name station)
  (second Station))
 
(define (get-type-station Station) ;dominio station, recorrido function r,m,c,t (type station)
  (third Station))
  
(define (get-stop-time-station Station) ;dominio station, recorrido number (stop-time station)
  (fourth Station))
 
;TDA section - constructor (2)

(define undefined-section (list undefined-station undefined-station -1 -1))

(define (stations-equal? Station1 Station2)
  (if (and (is-station? Station1) (is-station? Station2)
           (or (equal? (car Station1)(car Station2)) (equal? (second Station1)(second Station2))))
      #t #f))

(define (section point1 point2 distance cost) ; debe crear un tramo entre dos estaciones 
    (if (and (not(stations-equal? point1 point2))(number? distance)(> distance 0 )(number? cost)(>= cost 0))
        (list point1 point2 distance cost)
        undefined-section))

  ;PERTENENCIA (2)
(define (section? Section)
    (if (and (= 4 (length Section))
             (not (equal? (section (first Section)(second Section)(third Section)(fourth Section)) undefined-section)))
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
    (with-handlers ([exn:fail? (lambda (exn) (cons undefined-section null))])
    (if (empty? sections)
        undefined-section
        (if(empty? (filter (lambda (section-end)(check-start-circular (car sections) section-end)) sections))
           (search-start-circular (cdr sections))
           (car (filter (lambda (section-end)(check-start-circular (car sections) section-end)) sections))))))

  (if (and (= 1 (length( filter check-start-terminal sections))) (= 1 (length( filter check-finish-terminal sections)))) ;vemos si existe un unico tramo con una estacion inicical "t" y final "t"
      (get-sort-sections sections (car ( filter check-start-terminal sections))) ;retorno las secciones ordenadas de una linea t-t
      (get-sort-sections sections (search-start-circular sections))))               ;retorno las secciones oredenadas de una linea circular
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (get-ids-station sections-line)     
  (if (< 1 (length sections-line))              
      (cons (get-id-station (get-section-point1 (first sections-line)))
            (get-ids-station (cdr sections-line)))
      (cons (first (get-section-point1 (first sections-line)))
            (cons (get-id-station (get-section-point2 (first sections-line))) '()))))

(define (check-ids-line-consistency list-ids)
  (define (check-ids id list-ids)
    (if (not (empty? list-ids))
        (if (and (not (empty? (filter (lambda (y) (equal? id y)) list-ids))) (<= 0 id))
            #f
            (check-ids (car list-ids) (cdr list-ids)))
        #t))
  (if (> 0 (car list-ids))
      #f
      (check-ids (car list-ids) (cdr list-ids))))
  
(define (check-id-n-rt-line id-line name-line rail-type)
  (if (and (number? id-line)(<= 0 id-line)(string? name-line)(string? rail-type))
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
                     (list id-line name-line rail-type sections))
             (if (and (equal? (second (last sorted-sections)) (first (first sorted-sections)))
                      (and(not(equal?(get-type-station(second(last sorted-sections)))t))
                          (not(equal?(get-type-station(first (first sorted-sections)))t))))  ;circular 
                     (if (check-ids-line-consistency (cdr(get-ids-station sorted-sections)))
                         (if (and (not(empty? sections-m)) (forma-parte-de-alguna-section? sections-m sorted-sections))
                             (list id-line name-line rail-type (append sorted-sections sections-m ))
                             (list id-line name-line rail-type sorted-sections))
                        (list id-line name-line rail-type sections))
                     (list id-line name-line rail-type sections)))
                 (list id-line name-line rail-type sections))))

;PERTENENCIA

(define (line? line)
  (define (check-consistency sections-line)
    (if (section? (car sections-line))
        (if (not (<= (length sections-line) 1))
            (if (= (length sections-line) 2)                         
                (if (equal? (first (second sections-line)) (second (first sections-line))) 
                    #t #f)            
                (if (and (equal? (first (second sections-line)) (second (first sections-line))) (not (equal? (fourth (first (second sections-line))) t)))
                    (check-consistency (cdr sections-line))
                    #f))
            (if (= (length sections-line) 0)
                #f #t))
        #f))
    (let ([line-1 (filter is-not-type-m? (last line))]
          [sections-m (filter is-type-m? (last line))])
      (if (= 4 (length line))
          (if (and (not (= (length line-1) 0)) (check-id-n-rt-line (first line) (second line) (third line)))                          
              (if (and (equal? (get-type-station (second (last line-1))) t)
                       (equal? (get-type-station (first (first line-1))) t)
                       (not (equal? (get-section-point2 (last line-1)) (get-section-point1 (first line-1)))))
                  (if (check-consistency line-1)
                      (if (check-ids-line-consistency (get-ids-station line-1))
                          #t #f)
                      #f)
                  (if (and (equal? (get-section-point2 (last line-1)) (get-section-point1 (first line-1)))
                           (not (equal? (get-type-station (get-section-point2 (last line-1))) t))
                           (not (equal? (get-type-station (get-section-point1 (first line-1))) t))) ;circular 
                      (if (check-consistency line-1)
                          (if (check-ids-line-consistency (cdr (get-ids-station line-1)))
                              (if (empty? sections-m)
                                  #t
                                  (if (forma-parte-de-alguna-section? sections-m line-1)
                                      #t #f))
                              #f)                          
                          #f)
                      #f))
              #f)
          #f)))
  
(define (check-circularity-line line)
  (if (line? line)
      (if(equal? (second(last (filter is-not-type-m? (last line)))) (first (first(filter is-not-type-m? (last line)))))
         #t #f)
      #f))

;MODIFICADORES OTRAS-FUNCIONES

(define (get-order-stations s1 sections)
    ( - (length (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections)))))
        (length (member s1 (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections))))))))

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
      -1)) 
;-------------------------------------------------------------------------cost----------------------------------------------------------------------------------------------
(define(line-cost line)
  (define(cost sections)
    (if(empty? sections)
     0
     (+ (get-cost-section(first sections)) (cost (cdr sections)))))
  (cost (filter is-not-type-m? (last line))))

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
      -1))
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (line-add-section line section)
  (let ([sorted-sections (sort-sections (remove-duplicates (cons section (last line))))])
    (if (not (equal? undefined-section (last sorted-sections)))
        (list (first line) (second line) (third line) sorted-sections)
        (list (first line) (second line) (third line) (remove-duplicates (append (last line)(cons section null)))))))


  
  
















 







