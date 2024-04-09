#lang racket
;TDA producto-catalogo
;especificación
;producto-catalogo(id,nombre,precio,stock,categoria)
;get-nombre
;get-precio
;get-id
;get-stock
;get-categoria
;set-precio
;set-stock


;implementación
;representación
;id(int) X nombre(string) X precio (int) X categoria (string) X stock (int)

(define producto-catalogo-vacio null)

;desc: constructor de un producto catalogo
;dom: int X string X int X string X int
;rec: producto-catalogo
(define producto-catalogo (lambda (id nombre precio categoria stock)
              (list id nombre precio categoria stock)
))

;desc: selector id
;dom: producto-catalogo
;rec: int (id)
(define get-id car)

;desc: selector nombre
;dom: producto-catalogo
;rec: string (nombre)
(define get-nombre cadr)

;desc: selector precio
;dom: producto-catalogo
;rec: int (precio)
(define get-precio caddr)

  
;desc: selector categoria
;dom: producto-catalogo
;rec: string (categoría)
(define get-categoria cadddr)

;desc: selector stock
;dom: producto-catalogo
;rec: int (stock)
(define get-stock (lambda (p) (list-ref p 4)))


;desc: modificador de precio
;dom: producto-catalogo X int+
;rec: producto-catalogo
(define set-precio (lambda (p new-precio)
        (producto-catalogo (get-id p)
                           (get-nombre p)
                           new-precio
                           (get-categoria p)
                           (get-stock p))))


;desc: modificador de stock
;dom: producto-catalogo X int
;rec: producto-catalogo
(define set-stock (lambda (p change)
        (producto-catalogo (get-id p)
                           (get-nombre p)
                           (get-precio p)
                           (get-categoria p)
                           (+ (get-stock p) change))))


;TDA Catalogo
;especificación
;constructor : catalogo
;get-producto-catalogo (catalogo, id)
;set-producto-catalogo (catalogo, id, new-producto)
;add-producto-catalogo (catalogo, producto-catalogo)
;remove-producto-catalogo (catalogo, id)}

(define catalogo-vacio null)
(define catalogo-vacio? null?)

(define catalogo (lambda (p . ps)
                   (cons p ps)))

;desc: retorna un producto desde el catalogo a partir de su id
;dom: catalogo X int
;rec: producto-catalogo
(define get-producto-catalogo (lambda (cat id)
     (let ([subcat (filter (lambda (p)
                    (= id (get-id p))) cat)])
           (if (catalogo-vacio? subcat)
               producto-catalogo-vacio
               (car subcat)))))
         