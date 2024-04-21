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
          (integer? id)(>= id 0)(string? name)
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
 
;TDA SECTION (2)

;CONSTRUCTORES
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

;TDA line (3)

;CONSTRUCTORES (3)
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
(define (get-ids-stations-line sections-line) ;dominio, los tramos que componen una linea en formato de lista     
  (if (< 1 (length sections-line))              
      (cons (get-id-station (get-section-point1 (first sections-line)))
            (get-ids-stations-line (cdr sections-line)))
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
  (if (and (integer? id-line)(<= 0 id-line)(string? name-line)(string? rail-type))
     #t #f))

(define (is-in-section? sections-m sections)
  (if (empty? sections-m)
     #t
  (if (ormap (lambda(station2)(equal? (get-section-point1 (car sections-m)) station2)) (map get-section-point1 sections))
      (is-in-section? (cdr sections-m) sections)
      #f)))

(define (is-not-type-m-p2? section)
  (not(equal? (get-type-station (get-section-point2 section)) m)))

(define (is-type-m-p2? section)
  (equal? (get-type-station (get-section-point2 section)) m))

(define (line id-line name-line rail-type . sections) 
  (let ([sorted-sections (sort-sections (remove-duplicates(filter is-not-type-m-p2? sections)))][sections-m (remove-duplicates(filter is-type-m-p2? sections))]) ; Almacena el resultado de sort-sections en sorted-sections
    (if  (and (not (= (length sorted-sections) 0))(check-id-n-rt-line id-line name-line rail-type))                           
        (if (and (equal? (get-type-station(get-section-point2 (last sorted-sections))) t)
                 (equal? (get-type-station(get-section-point1 (first sorted-sections))) t)
                 (not(equal?(get-section-point2 (last sorted-sections))(get-section-point1 (first sorted-sections)))))
                 (if (check-ids-line-consistency (get-ids-stations-line sorted-sections))
                      (if (and (not(empty? sections-m)) (is-in-section? sections-m sorted-sections))
                        (list id-line name-line rail-type (append sorted-sections sections-m ))
                        (list id-line name-line rail-type sorted-sections))
                     (list id-line name-line rail-type sections))
             (if (and (equal? (second (last sorted-sections)) (first (first sorted-sections)))
                      (and(not(equal?(get-type-station(get-section-point2(last sorted-sections)))t))
                          (not(equal?(get-type-station(get-section-point1(first sorted-sections)))t))))  ;circular 
                     (if (check-ids-line-consistency (cdr(get-ids-stations-line sorted-sections)))
                         (if (and (not(empty? sections-m)) (is-in-section? sections-m sorted-sections))
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
                (if (equal? (get-section-point1 (second sections-line)) (get-section-point2 (first sections-line))) 
                    #t #f)            
                (if (and (equal? (get-section-point1 (second sections-line)) (get-section-point2 (first sections-line))) (not (equal? (get-type-station (get-section-point1 (second sections-line))) t)))
                    (check-consistency (cdr sections-line))
                    #f))
            (if (= (length sections-line) 0)
                #f #t))
        #f))
    (let ([sectionsLine (filter is-not-type-m-p2? (get-sections-line line))]
          [sections-m (filter is-type-m-p2? (get-sections-line line))])
      (if (= 4 (length line))
          (if (and (not (= (length sectionsLine) 0)) (check-id-n-rt-line (first line) (second line) (third line)))                          
              (if (and (equal? (get-type-station (second (last sectionsLine))) t)
                       (equal? (get-type-station (first (first sectionsLine))) t)
                       (not (equal? (get-section-point2 (last sectionsLine)) (get-section-point1 (first sectionsLine)))))
                  (if (check-consistency sectionsLine)
                      (if (check-ids-line-consistency (get-ids-stations-line sectionsLine))
                          #t #f)
                      #f)
                  (if (and (equal? (get-section-point2 (last sectionsLine)) (get-section-point1 (first sectionsLine)))
                           (not (equal? (get-type-station (get-section-point2 (last sectionsLine))) t))
                           (not (equal? (get-type-station (get-section-point1 (first sectionsLine))) t))) ;circular 
                      (if (check-consistency sectionsLine)
                          (if (check-ids-line-consistency (cdr (get-ids-stations-line sectionsLine)))
                              (if (empty? sections-m)
                                  #t
                                  (if (is-in-section? sections-m sectionsLine)
                                      #t #f))
                              #f)                          
                          #f)
                      #f))
              #f)
          #f)))
  
;SELECTORES

(define (get-sections-line line)
  (fourth line))

;MODIFICADORES OTRAS-FUNCIONES

(define (get-order-stations s1 sections)
    ( - (length (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections)))))
        (length (member s1 (cons (get-name-station(get-section-point1 (first sections))) (apply list (map get-name-station(map get-section-point2 sections))))))))

;---------------------------------------------------------------------largo-------------------------------------------------------------------------------------------------------
(define (line-length line)
  (if(not(= -1 (first line)))
     (apply + (map get-distance-section (filter is-not-type-m-p2? (last line))))
     (error 'line-length "invalid line")))

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
      (calculate-length station1 station2 (get-sections-line line))
      (error 'line-section-length "invalid line"))) 
;-------------------------------------------------------------------------cost----------------------------------------------------------------------------------------------
(define(line-cost line)
  (define(cost sections)
    (if(empty? sections)
     0
     (+ (get-cost-section(first sections)) (cost (cdr sections)))))
  (cost (filter is-not-type-m-p2? (get-sections-line line))))

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
      (process-cost station1 station2 (filter is-not-type-m-p2? (get-sections-line line)))
      (error 'line-section-cost "invalid line")))
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (line-add-section line section)
  (let ([sorted-sections (sort-sections (remove-duplicates (cons section (last line))))])
    (if (not (equal? undefined-section (last sorted-sections)))
        (list (first line) (second line) (third line) sorted-sections)
        (list (first line) (second line) (third line) (remove-duplicates (append (last line)(cons section null)))))))

;TDA PCAR (4)

;CONSTRUCTOR

(define undefined-pcar (list -1 -1 "undefined model" "undefined-car-type"))

(define tr "terminal-pcar")
(define ct "central-pcar")

(define (pcar id-pcar capacity model car-type)
  (if (and (integer? id-pcar) (<= 0 id-pcar)
           (number? capacity)(<= 0 capacity)
           (string? model)
           (or(equal? car-type tr)(equal? car-type ct))
      )
     (list id-pcar capacity model car-type)
     undefined-pcar))

;SELECTORES

(define (get-id-pcar pcar)
  (first pcar))

(define (get-capacity-pcar pcar)
  (second pcar))

(define (get-model-pcar pcar)
  (third pcar))

(define (get-car-type-pcar pcar)
  (fourth pcar))


;TDA TRAIN(5)

;CONSTRUCTOR

(define undefined-train (list -1 "undefined-maker" "undefined-rail-type" -1 -1 undefined-pcar))
(define incorrect-assembly (list "incorrect assembly"))

(define (train id-train maker rail-type speed station-stay-time . pcars)
  (define (check-consistency-pcars rest-pcars first-pcar last-pcar) ;todos deben tener el mismo modelo, solo pueden haber dos terminales
    (if (equal? (car rest-pcars) last-pcar)
        #t
      (if (and (equal? (get-model-pcar first-pcar)(get-model-pcar (first rest-pcars)))(equal? ct (get-car-type-pcar(first rest-pcars))))
          (check-consistency-pcars (cdr rest-pcars) first-pcar last-pcar )
          #f
      )))
      (if(and (integer? id-train )(<= 0 id-train)(string? maker)(string? rail-type)(number? speed)(<= 0 speed))
        (if (and (< 1 (length pcars))(equal? (get-model-pcar(first pcars))(get-model-pcar(last pcars)))(equal? (get-car-type-pcar(first pcars))tr)(equal? (get-car-type-pcar(last pcars)) tr))
            (if (check-consistency-pcars  (cdr pcars) (first pcars) (last pcars))
                (list id-train maker rail-type speed station-stay-time pcars)
                (list id-train maker rail-type speed station-stay-time (cons incorrect-assembly)));si falla la consistencia, se agrega un pcar malo
                (if (empty? pcars)
                    (list id-train maker rail-type speed station-stay-time pcars)
                    (list id-train maker rail-type speed station-stay-time (cons incorrect-assembly pcars))))
      undefined-train)        
      )

(define (make-train id-train maker rail-type speed station-stay-time pcars)
  (define (check-consistency-pcars rest-pcars first-pcar last-pcar) ;todos deben tener el mismo modelo, solo pueden haber dos terminales
    (if (equal? (car rest-pcars) last-pcar)
        #t
      (if (and (equal? (get-model-pcar first-pcar)(get-model-pcar (first rest-pcars)))(equal? ct (get-car-type-pcar(first rest-pcars))))
          (check-consistency-pcars (cdr rest-pcars) first-pcar last-pcar )
          #f
      )))
      (if(and (integer? id-train )(<= 0 id-train)(string? maker)(string? rail-type)(number? speed)(<= 0 speed))
        (if (and (< 1 (length pcars))(equal? (get-model-pcar(first pcars))(get-model-pcar(last pcars)))(equal? (get-car-type-pcar(first pcars))tr)(equal? (get-car-type-pcar(last pcars)) tr))
            (if (check-consistency-pcars  (cdr pcars) (first pcars) (last pcars))
                (list id-train maker rail-type speed station-stay-time pcars)
                (list id-train maker rail-type speed station-stay-time (cons incorrect-assembly pcars)));si falla la consistencia, se agrega un pcar malo
                (if (empty? pcars)
                    (list id-train maker rail-type speed station-stay-time pcars)
                    (list id-train maker rail-type speed station-stay-time (cons incorrect-assembly pcars))))
      undefined-train)        
      )

;SELECTORES
(define (get-id-train train)
  (first train))
(define (get-maker-train train)
  (second train))
(define (get-rail-type-train train)
  (third train))
(define (get-speed-train train)
  (fourth train))
(define (get-station-stay-time-train train)
  (fifth train))
(define (get-train-pcars train)
  (sixth train))

;PERTENENCIA

(define (train? train)
  (define (check-consistency-pcars rest-pcars first-pcar last-pcar) 
    (if (equal? (car rest-pcars) last-pcar)
        #t
      (if (and (equal? (get-model-pcar first-pcar)(get-model-pcar (first rest-pcars)))(equal? ct (get-car-type-pcar(first rest-pcars))))
          (check-consistency-pcars (cdr rest-pcars) first-pcar last-pcar )
          #f
      )))
      (if (or (empty? (get-train-pcars train))(equal? (last(get-train-pcars train)) ct))
          #f
      (if (equal? (first(get-train-pcars train)) incorrect-assembly)
          (if (equal? (first(cdr(get-train-pcars train))) tr)
              (check-consistency-pcars (cdr(cdr (get-train-pcars train))) (first(cdr(get-train-pcars train))) (last(get-train-pcars train)))
              #f)
              (check-consistency-pcars (cdr (get-train-pcars train)) (first(get-train-pcars train)) (last(get-train-pcars train))))))

(define (Train? train)
  (not(or (empty?(get-train-pcars train)) (equal?(car(get-train-pcars train)) incorrect-assembly))))
  
;OTROS

(define (remove-pcars-inconcistency train)
  (if(and(not(empty? (get-train-pcars train)))(equal? (car(get-train-pcars train)) incorrect-assembly))
     (list  (get-id-train train)(get-maker-train train)(get-rail-type-train train)(get-speed-train train)(get-station-stay-time-train train)(cdr(get-train-pcars train)))
     train))
;add
(define (train-add-car train pcar position)
  
  (define (add-car-in-pcars position pcar Pcars)
        (if (equal? position 0)
            (cons pcar null)
            (cons (first Pcars) (add-car-in-pcars (- position 1) pcar (cdr Pcars))))) 
  (let ([Train (remove-pcars-inconcistency train)])    
  (if(>= (length (get-train-pcars Train)) position)   
     (if(= (length(get-train-pcars Train)) position)
        (make-train (get-id-train Train)(get-maker-train Train)(get-rail-type-train Train)(get-speed-train Train)(get-station-stay-time-train Train)
               (add-car-in-pcars  position  pcar (get-train-pcars Train)))      
        (if(= (length (get-train-pcars Train)) 0)
           (make-train (get-id-train Train)(get-maker-train Train)(get-rail-type-train Train)(get-speed-train Train)(get-station-stay-time-train Train)
                      (list pcar))
           (make-train (get-id-train Train)(get-maker-train Train)(get-rail-type-train Train)(get-speed-train Train)(get-station-stay-time-train Train)
                      (append (add-car-in-pcars  position  pcar (get-train-pcars Train)) (member (list-ref (get-train-pcars Train) position) (get-train-pcars Train))))))
      (error 'train-add-car "position out of range"))))

;remove
(define (train-remove-car train position)
    (define (remove-car-in-pcars position Pcars)
        (if (equal? position 0)
            null
            (cons (first Pcars) (remove-car-in-pcars (- position 1) (cdr Pcars))))) 

  (let ([Train (remove-pcars-inconcistency train)])
  (if(> (length (get-train-pcars Train)) position)   
     (if(= (length(get-train-pcars Train)) position)
        (make-train (get-id-train Train)(get-maker-train Train)(get-rail-type-train Train)(get-speed-train Train)(get-station-stay-time-train Train)
               (remove-car-in-pcars  position  (get-train-pcars Train)))      
        (if(= (length (get-train-pcars Train)) 0)
            train
           (make-train (get-id-train Train)(get-maker-train Train)(get-rail-type-train Train)(get-speed-train Train)(get-station-stay-time-train Train)
                      (append (remove-car-in-pcars  position (get-train-pcars Train)) (cdr(member (list-ref (get-train-pcars Train) position) (get-train-pcars Train)))))))
      (error 'train-remove-car "position to remove out of range"))))

(define (train-capacity train)
  (define (get-capacity-train capacity train-pcars)
    (if (empty? train-pcars)
        capacity
        (get-capacity-train (+ capacity (get-capacity-pcar(first train-pcars))) (cdr train-pcars))))
  (let ([Train (remove-pcars-inconcistency train)])
    (get-capacity-train 0 (get-train-pcars Train))))


;TDA DRIVER (6)

(define undefined-driver (list -1 "undefined-name-driver" "undefined-train-maker"))

(define (driver id-driver name-driver train-maker)
      (if (and (integer? id-driver) (<= 0 id-driver) (string? name-driver)(string? train-maker))
      (list id-driver name-driver train-maker)
      undefined-driver))


;TDA SUBWAY

(define undefined-subway (list -1 "undefine name subway"))

(define (subway id-subway name-subway)
       (if(and (integer? id-subway)(<= 0 id-subway)(string? name-subway))
       (list id-subway name-subway null null null)
       undefined-subway))

;TDA SUBWAY MODIFICADOR

(define (subway-add-train subway . trains)
  ())


;SELECTORES

(define subway)





      


















     




  
  
















 








