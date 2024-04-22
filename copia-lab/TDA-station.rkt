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

(define (get-ids-stations-line sections-line)
  (define (get-id-p2 section)
    (get-id-station(get-section-point2 section)))
  (cons(get-id-station(get-section-point1(first sections-line)))(map get-id-p2 sections-line )))

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

(define( get-id-line line)
  (first line)) 

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

;SELECTOR

(define (get-id-driver driver)
    (first driver))

;TDA SUBWAY

(define undefined-subway (list -1 "undefine name subway"))

(define (subway id-subway name-subway)
      (if(and (integer? id-subway)(<= 0 id-subway)(string? name-subway))
       (list id-subway name-subway null null null)
       undefined-subway))

;TDA SUBWAY MODIFICADOR

(define (subway-add-train subway . trains) 
  (define (check-ids-train-consistency list-id-trains)
    (define (check-ids id list-id-trains)
      (if (not (empty? list-id-trains))
          (if (and (not (empty? (filter (lambda (y) (equal? id y)) list-id-trains))) (<= 0 id))
              #f
              (check-ids (car list-id-trains) (cdr list-id-trains)))
          #t))
    (if (> 0 (car list-id-trains))
        #f
        (check-ids (car list-id-trains) (cdr list-id-trains)))) 
  (if (empty? (get-subway-train subway))
    (if(check-ids-train-consistency (map get-id-train trains))
        (list (get-subway-id subway)(get-subway-name subway) trains (get-subway-line subway)(get-subway-driver subway))
        (error 'subway-add-train "Repeated trains are being added"))
    (if(check-ids-train-consistency (append (map get-id-train trains)(map get-id-train (get-subway-train subway))))
        (list (get-subway-id subway)(get-subway-name subway) (append (get-subway-train subway) trains)(get-subway-line subway)(get-subway-driver subway))
        (list (get-subway-id subway)(get-subway-name subway) (get-subway-train subway) (get-subway-line subway)(get-subway-driver subway)))))
 ;---------------------------------------------------------------------------
(define (subway-add-line subway . lines)
  
    (define (condition-to-filter-station-c Section)
      (not(or(equal?(get-type-station(get-section-point1 Section))c) 
             (equal?(get-type-station(get-section-point2 Section))c))))
  
    (define (filter-station-type-c sections-line)
      (filter condition-to-filter-station-c sections-line))
    (define (check-duplicates-id-lines lines)
      (if(check-duplicates (map get-id-line lines))
        #t #f))
  
    (define (check-duplicates-stations-in-line lines)
     (if (check-duplicates(append*(map remove-duplicates
         (map get-ids-stations-line(map filter-station-type-c 
         (map get-sections-line lines))))))
         #t #f))
  
   (if (empty? (get-subway-line subway))
       (if (nor (check-duplicates-id-lines lines) (check-duplicates-stations-in-line lines))
           (list (get-subway-id subway) (get-subway-name subway) (get-subway-train subway) lines (get-subway-driver subway))
           (error 'subway-add-line "Repeated stations or lines"))
       (if (nor (check-duplicates-id-lines (append lines (get-subway-line subway)))(check-duplicates-stations-in-line (append lines (get-subway-line subway))))
           (list (get-subway-id subway) (get-subway-name subway) (get-subway-train subway) (append (get-subway-line subway) lines) (get-subway-driver subway))
           (error 'subway-add-line "Repeated stations or lines"))))
;-------------------------------------------------------------------------
(define (subway-add-driver subway . drivers)
  (if (empty? (get-subway-driver subway))
      (if (not(check-duplicates(map get-id-driver drivers)))
        (list (get-subway-id subway) (get-subway-name subway) (get-subway-train subway) (get-subway-line subway) drivers)
        (error 'subway-add-driver "Repeated drivers"))
      (if (not(check-duplicates (map get-id-driver (append drivers (get-subway-driver subway)))))
        (list (get-subway-id subway) (get-subway-name subway) (get-subway-train subway) (get-subway-line subway) (append (get-subway-driver subway) drivers))
        (error 'subway-add-driver "Repeated drivers"))))
      
;SELECTORES

(define (get-subway-id subway)
  (first subway))

(define (get-subway-name subway)
  (second subway))

(define (get-subway-train subway)
  (third subway))

(define (get-subway-line subway)
  (fourth subway))

(define (get-subway-driver subway)
  (fifth subway))




(define salto "---------")
(define e0 (station 0 "San Pablo" t 90))
(define e1 (station 1 "Neptuno" r 45))
(define e2 (station 2 "Pajaritos" c 45))
(define e3 (station 3 "Las Rejas" r 45))
(define e4 (station 4 "Ecuador" r 60))
(define e5 (station 5 "San Alberto Hurtado" r 40))
(define e6 (station 6 "Universidad de Santiago de Chile" c 40))
(define e7 (station 7 "Estación Central" c 45))
(define e8 (station 8 "Unión Latinoamericana" r 30))
(define e9 (station 9 "República" r 40))
(define e10 (station 10 "Los Héroes" c 60))
(define e11 (station 11 "La Moneda" r 40))
(define e12 (station 12 "Universidad de Chile" c 90))
(define e13 (station 13 "Santa Lucía" r 40))
(define e14 (station 14 "Universidad Católica" c 60))
(define e15 (station 15 "Baquedano" r 40))
(define e16 (station 16 "Los Dominicos" t 90))
(define e17 (station 17 "Cochera Neptuno" m 3600))
(define e18 (station 18 "El Llano" r 60))
(define e19 (station 19 "Franklin" r 50))
(define e20 (station 20 "Rondizzoni" r 55))
(define e21 (station 21 "Parque O'Higgins" r 65))
(define e22 (station 22 "Toesca" r 65))
(define e23 (station 23 "Santa Ana" r 65))
(define e24 (station 24 "Puente Cal y Canto" r 65))
(define e25 (station 25 "mantencion parqueO" m 65))


;tramos l1
(define s0 (section e0 e1 4 15))
(define s1 (section e1 e2 3 14))
(define s2 (section e2 e3 2.5 10))
(define s3 (section e3 e4 4.5 17))
(define s4 (section e4 e5 4.7 18))
(define s5 (section e5 e6 4.3 17))
(define s6 (section e6 e7 3.8 12))
(define s7 (section e7 e8 2.5 10))
(define s8 (section e8 e9 4.5 17))
(define s9 (section e9 e10 4.7 18))
(define s10 (section e10 e11 4.3 17))
(define s11 (section  e11 e12 3.8 12))
(define s12 (section e12 e13 4.5 17))
(define s13 (section e13 e14 4.7 18))
(define s14 (section e14 e15 4.3 17))
(define s15 (section e15 e16 4.2 17))
(define s16 (section e1 e17 3.8 12))

;tramos l2
(define s17 (section e18 e19 4 15))
(define s18 (section e19 e20 3 12))
(define s19 (section e20 e21 5 18))
(define s20 (section e21 e22 4.5 16))
(define s21 (section e22 e10 4.2 16))
(define s22 (section e10 e23 4.2 16))
(define s23 (section e23 e24 4.2 16))
(define s24 (section e24 e18 28 90))
(define s25 (section e21 e25 28 78))

(define l1 (line 1 "Línea 1" "UIC 60 ASCE" s0 s1 s3 s2 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16))

(define l2 (line 2 "Línea 2" "100 R.E."))


(define l2a (line-add-section l2 s17))
(define l2b (line-add-section l2a s18))
(define l2c (line-add-section l2b s19))
(define l2d (line-add-section l2c s20))
(define l2e (line-add-section l2d s21))
(define l2f (line-add-section l2e s22))
(define l2g (line-add-section l2f s23))
(define l2h (line-add-section l2g s24))
(define l2i (line-add-section l2h s19))
(define l2j (line-add-section l2h s25))


;creando carros
(define pc0 (pcar 0 100 "NS-74" tr))
(define pc1 (pcar 1 100 "NS-74" ct))
(define pc2 (pcar 2 150 "NS-74" ct))
(define pc3 (pcar 3 100 "NS-74" ct))
(define pc4 (pcar 4 100 "NS-74" tr))
(define pc5 (pcar 5 100 "AS-2014" tr))
(define pc6 (pcar 6 100 "AS-2014" ct))
(define pc7 (pcar 7 100 "AS-2014" ct))
(define pc8 (pcar 8 100 "AS-2014" ct))
(define pc9 (pcar 9 100 "AS-2014" tr))
(define pc10 (pcar 10 100 "AS-2014" tr))
(define pc11a (pcar 11 100 "AS-2016" tr))
(define pc11 (pcar 12 100 "AS-2016" ct))
(define pc12 (pcar 13 100 "AS-2016" ct))
(define pc13 (pcar 14 150 "AS-2016" ct))
(define pc14 (pcar 15 100 "AS-2016" ct))
(define pc15 (pcar 16 100 "AS-2016" ct))
(define pc16 (pcar 17 100 "AS-2016" ct))
(define pc17 (pcar 18 100 "AS-2016" tr))

;creando trenes
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc0 pc1 pc2 pc3 pc4)) ;tren válido
(define t2 (train 2 "CAF" "100 R.E." 70  2 pc5 pc6 pc7 pc8 pc9)) ;tren válido

(define t0a (train-add-car t0 pc5 0))
(define t0b (train-add-car t0a pc6 1))
(define t0c (train-add-car t0b pc7 2))
(define t0d (train-add-car t0c pc8 3))
(define t0e (train-add-car t0d pc9 4)) ;tren válido


(define d0 (driver 0 "Juan" "CAF"))
(define d1 (driver 1 "Alejandro" "Alsthom"))
(define d2 (driver 2 "Diego" "Alsthom"))
(define d3 (driver 3 "Pedro" "CAF"))
(define d4 (driver 4 "Nicolas" "CAF"))

;Creando Metros
(define sw0 (subway 0 "Metro de Santiago"))
(define sw1 (subway 1 "Subte"))

;Agregando trenes
(define sw0a (subway-add-train sw0 t1 t2 t0e))

; Estaciones
(define eA (station 34 "Estación A" t 90))
(define eB (station 35 "Estación B" c 60))
(define eC (station 36 "Estación C" r 50))
(define eD (station 37 "Estación D" t 90))
(define eAM (station 38 "Mantenimiento A" m 3600))

; Tramos de la línea
(define sAB (section eA eB 4 15))
(define sBC (section eB eC 3 14))
(define sCD (section eC eD 2.5 10))
(define sAD (section eA eAM 1 5))

; Creación de la línea
(define l3 (line 3 "Línea 3" "UIC 60 ASCE" sAB sBC sCD sAD))

;Agregando lineas
(define sw0b (subway-add-line sw0a l1 l2h))
sw0b
salto
;Agregando drivers
(define sw0c (subway-add-driver sw0b d0 d1 d2 d3))
sw0c
(define sw0d (subway-add-driver sw0c d4))
sw0d

      


















     




  
  
















 








