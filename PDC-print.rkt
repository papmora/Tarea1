;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Proyecto1.0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;-----------------------------------------------------------------------------------
;Función a ejecutar PDC-paint
;-----------------------------------------------------------------------------------
;Librerias
;-----------------------------------------------------------------------------------
(require racket/gui)
(require (lib "graphics.ss" "graphics")); se importa la libreria graphics
(open-graphics) ; se abre el modo gráfico
;-----------------------------------------------------------------------------------
;Definir juego
;-----------------------------------------------------------------------------------
(define (PDC-paint largo matriz)

  ;Revisa si la matriz es válida
  (cond ( (pdc-test largo matriz)

  (define cordenada (exact-round(+ (* largo 70.88) 22)))

  (define ventana (open-viewport "PDC-paint" cordenada cordenada)); se declara una variable z que guarda una ventana del tamaño necesario para el juego
  (define mapa-pixeles (open-pixmap "PDC-pant" cordenada cordenada)) ; se declara una variable p que guarda una ventana del tanmaño necesario para el jeugo

  ;Pinta el tablero de Ajedrez
  (pintar 0 0 0 largo ventana mapa-pixeles)
    
  ;llama a la funcion juego
  (juego matriz ventana mapa-pixeles 0 largo))
  (else
   #f)))
;---------------------------------------------------------------------------------
;Iniciar juego
;---------------------------------------------------------------------------------
(define margen 10) ;se deja un margen de 10 pixeles a cada lado. Esta se debe tener en cuenta para evaluar la posicion de mouse
(define negro "white") ; se define una variable negro que contendra el color negro
(define gris (make-rgb 0.40 0.40 0.40)) ; se define una variable gris que contendra el color gris para el tablero
;-------------------------------------------------------------------------
;codigo para dibujar el tablero
;-------------------------------------------------------------------------
(define(pintar u h v matriz ventana mapa-pixeles)
  (for ([v (in-range 11 (* matriz 71.33) 71)]) ; V es la variable en el eje y
    (if (= u 0)                                ; h la variable del eje x
        (for ([h (in-range 11 (* matriz 65.55) 142)])
          ((draw-solid-rectangle mapa-pixeles) (make-posn h v) 70 70 gris)
          (set! u (+ 1 1))
          )
        (for ([h (in-range 82 (* matriz 65.55) 142)])
          ((draw-solid-rectangle mapa-pixeles) (make-posn h v) 70 70 gris)
          (set! u (- 1 1)))))
;-----------------------------------------------------------------------------
; codigo para dibujar las lineas verticales
;-----------------------------------------------------------------------------
  (for ([h (in-range 10 (+ 11 (* 70.88 matriz)) 71)])
    ((draw-line mapa-pixeles) (make-posn h 10) (make-posn h (+ 11 (* 70.88 matriz)) ) "black"))
;-----------------------------------------------------------------------------
; codigo para dibujar las lineas horizontales
;-----------------------------------------------------------------------------
  (for ([v (in-range 10 (+ 12 (* 70.88 matriz)) 71)])
    ((draw-line mapa-pixeles) (make-posn 10 v) (make-posn (+ 10 (* 70.88 matriz)) v) "black")))
;-----------------------------------------------------------------------------
; funcion para dibujar el caballo(posición)
;-----------------------------------------------------------------------------
(define (caballo i j matriz ventana mapa-pixeles)
  (cond
    ((and (and (> i -1) (<=  i (- matriz 1))) (and (> j -1) (< j matriz)))
      (begin
        (define a (+ (* i 71) margen))
        (define b (+ (* j 71) margen))
         
           ((draw-solid-rectangle mapa-pixeles) (make-posn a b) 71 71 "yellow")
            (copy-viewport mapa-pixeles ventana)
            ((draw-solid-rectangle mapa-pixeles) (make-posn a b) 71 71 "yellow")))))
;----------------------------------------------------------------------------
;Busqueda de elementos
;----------------------------------------------------------------------------
;Función que encuentra la columna en que esta un elemento den una matriz
(define (buscar-row largo matriz list row)
  (cond ( (and (null? matriz) (null? list))
          0)
        ( (null? list)
          (buscar-row largo (cdr matriz) (car matriz) (* row 0)))
        ( (equal? largo (car list))
          row)
        (else
         (buscar-row largo matriz (cdr list) (+ row 1)))))
;Función que encuentra la fila en que se encuentra un elemnto en una matriz
(define (buscar-colum largo matriz list colum)
  (cond ( (and (null? matriz) (null? list))
          '())
        ( (null? list)
          (buscar-colum largo (cdr matriz) (car matriz) (+ colum 1)))
        ( (equal? largo (car list))
          colum)
        (else
         (buscar-colum largo matriz (cdr list) colum))))
;---------------------------------------------------------------------------------
; La funcion juego evalua la posicion donde se da clic y determina si se puede mover o no
;---------------------------------------------------------------------------------
(define (juego matriz ventana mapa-pixeles elemento largo)
  (cond ( (> (* largo largo) elemento)
      (caballo (buscar-row elemento matriz '() 0) (buscar-colum elemento matriz '() -1) largo ventana mapa-pixeles)
      (sleep 1)
      (juego matriz ventana mapa-pixeles (+ elemento 1) largo))
  (else
   (juego matriz ventana mapa-pixeles elemento largo))))                    
;----------------------------------------------------------------------------------
;;Algoritmo PCD-Test encargado de validar si la matriz es solución
;----------------------------------------------------------------------------------
;Validacion: Matriz tamaño NxN
;----------------------------------------------------------------------------------
(define (is-nxn matrix n)
  (cond
    [(and (= (len matrix) n) (rows-n matrix n)) #t]
    [else #f]))
    
;Función que valida el tamaño de una lista
(define (len list)
  (cond
    [(empty? list)  0]
    [(cons? list)   (+ 1 (len (cdr list)))]))

;Función que valida que las filas de una matriz sean de tamaño N
(define (rows-n matrix n)
  (cond
    [(empty? matrix) #t]
    [(= (len (car matrix)) n) (rows-n (cdr matrix) n)]
    [else #f]))
;-----------------------------------------------------------------------------------
;Funcion que indica la posicion de un elemento en la matriz
;-----------------------------------------------------------------------------------
(define (pos matrix ele)
  (pos-aux matrix ele 0))

;Funcion auxiliar de la posicion de un elemento en la matriz
(define (pos-aux matrix ele column)
  (cond
    [(empty? matrix) '(-1 -1)]
    [(member ele (car matrix)) (list column (pos-in-list (car matrix) ele))]
    [else (pos-aux (cdr matrix) ele (+ column 1))]))

;Funcion que indica la posicion de un elemento en una lista
(define (pos-in-list list ele)
  (pos-in-list-aux list ele 0))

;Funcion auxiliar que indica la posicion de un elemento en una lista
(define (pos-in-list-aux list ele initial-pos)
  (cond
    [(empty? list) -1]
    [(= (car list) ele) initial-pos]
    [else (pos-in-list-aux (cdr list) ele (+ initial-pos 1))]))
;-------------------------------------------------------------------------------------
;Validacion: Los elementos de la matriz no se repiten
;-------------------------------------------------------------------------------------
(define (not-repeated matrix n)
  (not-repeated-aux matrix (- (* n n) 1)))

;Funcion que verifica que cada elemento de la matriz sea diferente en el intervalo: [0, n^2 - 1]
(define (not-repeated-aux matrix position)
  (cond
    [(< position 0) #t]
    [(= (car (pos matrix position)) -1) #f]
    [else (not-repeated-aux matrix (- position 1))]))
;---------------------------------------------------------------------------------------
;Validacion: Movimiento valido (forma una letra L)
;---------------------------------------------------------------------------------------
(define (l-move pos1 pos2)
  (or
   [and (= (- (row pos1) 1) (row pos2)) (= (+ (column pos1) 2) (column pos2))]
   [and (= (- (row pos1) 2) (row pos2)) (= (+ (column pos1) 1) (column pos2))]
   [and (= (- (row pos1) 2) (row pos2)) (= (- (column pos1) 1) (column pos2))]
   [and (= (- (row pos1) 1) (row pos2)) (= (- (column pos1) 2) (column pos2))]
   [and (= (+ (row pos1) 1) (row pos2)) (= (- (column pos1) 2) (column pos2))]
   [and (= (+ (row pos1) 2) (row pos2)) (= (- (column pos1) 1) (column pos2))]
   [and (= (+ (row pos1) 2) (row pos2)) (= (+ (column pos1) 1) (column pos2))]
   [and (= (+ (row pos1) 1) (row pos2)) (= (+ (column pos1) 2) (column pos2))]))

;Fila segun posicion en matriz
(define (row pos)
  (car pos))

;Columna segun posicion en matriz
(define (column pos)
  (car (cdr pos)))
;----------------------------------------------------------------------------------------
;Funcion PCD-Test
;----------------------------------------------------------------------------------------
;Se deben ejecutar las validaciones indicadas anteriormente
(define (pdc-test n matrix)
  (cond
    [(is-nxn matrix n)
     (cond
       [(not-repeated matrix n) (pdc-test-aux n matrix (- (* n n) 1) 0)]
       [else #f]
       )]
    [else #f]))
;------------------------------------------------------------------------------------------
;Funcion PDC-Test auxiliar, verifica que para cada elemento haya un movimiento en L valido
;------------------------------------------------------------------------------------------
(define (pdc-test-aux n matrix max-element element)
  (cond
    [(= element max-element) #t]
    [(l-move (pos matrix element) (pos matrix (+ element 1))) (pdc-test-aux n matrix max-element (+ element 1))]
    [else #f]))