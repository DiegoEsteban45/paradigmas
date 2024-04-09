#lang racket
;Implementación del TDA Producto
;Representación: String X Entero+ +{0}

;Constructores
;(define producto cons)
;(define producto (lambda (e1 e2) (cons e1 e2)))
(define producto
  (lambda (nombre precio)
    (if (and (string? nombre)
             (integer? precio)
             (>= precio 0))
        (cons nombre precio)
        null)))

;pertenencia
(define producto? (lambda (p)
                    (and (pair? p)
                         (not (null? p))
                         (not (null? (producto (car p) (cdr p)))))))

;selectores
(define getNombre car)
(define getPrecio cdr)

;comparador
(define equalProducto? equal?)
;(define equalProducto? (lambda (p1 p2)
;                         (and (eqv? (getNombr p1) (getNombre p2))
 ;                             (= (getPrecio p1) (getPrecio p2)))))

;modificadores
(define setPrice (lambda (p newPrice)
                   (producto (getNombre p) newPrice)))

;TDA Item
;Item = Producto X Cantidad

;constructores
(define item (lambda (prod cant)
               (if (and (producto? prod)
                        (>= cant 0))
                   (cons prod cant)
                   null
                   )))

;(define item cons)

;selectores
(define getProducto2 car)
(define getProducto (lambda (i) (if (null? i) null (car i))))
(define getCantidad cdr)
(define getCantidad2 cdr)
(define getPrecioItem (lambda (i) (if (null? i) 0 (* (cdr (car i)) (cdr i)))))

;modificadores
(define setCantidad (lambda (i newCant) (item (getProducto i) newCant)))

;otra
(define equalItem? (lambda (i1 i2)
                     (equalProducto? (getProducto i1)
                                     (getProducto i2))))



;TDA CarritoCompra
;Representación
;CarritoCompra = carritoVacio | Item X CarritoCompra
;Lista = null | elemento X Lista

;constructores
(define carritoVacio null)
(define carritoCompra carritoVacio)
(define addItem (lambda (cc prod cantidad)
                  (let ([i (item prod cantidad)])
                    (updateItem cc i))))

(define updateItem (lambda (cc i)
                     (if (null? cc)
                         (cons i null)
                         (if (equalItem? i (firstItem cc))
                             (cons (setCantidad i (+ (getCantidad i) (getCantidad (firstItem cc)))) (nextItems cc))
                             (cons (firstItem cc)
                                   (updateItem (nextItems cc) i))))))

;alternativa declarativa de addItem
;(define addItem2 (lambda (cc prod cantidad)
;                   (let ([i (filter (lambda (i1) (equalProducto? prod (getProducto i1))) cc)]
;                         [is (filter (lambda (i1) (not (equalProducto? prod (getProducto i1)))) cc)])
;                     (cons (item prod (+ cantidad (getCantidad2 i))) is))))
                 
(define firstItem car)
(define nextItems cdr)

;pertenencia
(define carritoVacio? null?)

;otras
(define totalItemsCarrito (lambda (cc)
                       (if (carritoVacio? cc)
                           0
                           (+ (getCantidad (firstItem cc)) (totalItemsCarrito (nextItems cc))))))

;versión full declarativa de la función anterior
(define totalCarritoDecl (lambda (cc)
         (apply + (map getCantidad cc))))


(define totalAPagarCarrito (lambda (cc)
                       (if (carritoVacio? cc)
                           0
                           (let ([i (firstItem cc)])
                                (+ (* (getPrecio (getProducto i)) (getCantidad i))
                                   (totalAPagarCarrito (nextItems cc)))))))


;versión full declarativa de la función anterior
(define totalAPagarCarritoDecl (lambda (cc)
                      (apply + (map (lambda (i) (* (getCantidad i) (getPrecio (getProducto i)))) cc))))



;Otra alternativa para totalItemsCarrito
;Una función que permite sumar las cantidades de items en carrito para ser aplicada con apply
(define sumaCantidadItems (lambda (i1 . is)
      (define sumaAux (lambda (is)
           (if (null? is)
               0
               (+ (getCantidad (car is)) (sumaAux (cdr is))))))

      (+ (getCantidad i1) (sumaAux is))))


;Se podría usar así (apply sumaCantidadItems Carrito)                          

(define p1 (producto "leche" 800))
(define p2 (producto "agua" 600))
(define p3 (producto "carne" 200))

(define cc1 (addItem (addItem (addItem (addItem carritoVacio p1 5)
           p2 7) p1 3) p3 10))

;(define cc2 (addItem2 (addItem2 (addItem2 (addItem2 carritoVacio p1 5)
;           p2 7) p1 3) p3 10))


;---------------------

;TDA Usuario
    ; Tienen carrito
    ; nombre

;constructor
;   usuario: nombre X carrito

;selectores
;   getNombreUsuario: usuario -> nombre
;   getCarritoUsuario: usuario -> carrito

;Implementación del TDA
;representación: nombre: String
;                carrito: CarritoCompra
;                par nombre carrito

;constructor
;(define usuario cons)
(define usuario (lambda (nombre)
                  (if (string? nombre)                 
                       (cons nombre carritoVacio)
                           null)))

(define getNombreUsuario car)
(define getCarritoUsuario cdr)

(define addProductoCarritoUsuario
     (lambda (u p c)
         (cons (getNombreUsuario u)
               (addItem (getCarritoUsuario u) p c))))

;TDA sistema

;sistema = ListaUsuarios
;ListaUsuarios = vacio | Usuario X ListaUsuarios


(define sistema (lambda (nombre)
                  (cons nombre null)))

(define register (lambda (nombre pass tienda)
                   (let ([u (usuario nombre pass)])
                        (tienda (getNombreTienda tienda) (addUser u (getUsuarios tienda))))))




;TDA Usuarios
(define addUser (lambda (u us)
           (if (null? (filter
                             (lambda (u1)
                                 (equal? (getUserName u1) (getUserName u)))
                             us))
               (cons u us)
               us)))

(define addUserAlFinal (lambda (u us)
                (if (null? us)
                    (cons u null)
                    (if (equal? (getUserName u) (getUserName (car us)))
                        us
                        (cons (car us) (addUserAlFinal u (cdr us)))))))



;TDA Usuario
(define usuario cons
        
               
               
                         
                     






               