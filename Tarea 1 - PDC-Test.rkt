;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Tarea 1 - PCD-Test|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;PCD-Test
;------------------------------------Validacion: Matriz tamaño NxN--------------------------------------
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

;-----------------------Funcion que indica la posicion de un elemento en la matriz---------------------
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

;--------------------------Validacion: Los elementos de la matriz no se repiten-------------------------

(define (not-repeated matrix n)
  (not-repeated-aux matrix (- (* n n) 1)))

;Funcion que verifica que cada elemento de la matriz sea diferente en el intervalo: [0, n^2 - 1]
(define (not-repeated-aux matrix position)
  (cond
    [(< position 0) #t]
    [(= (car (pos matrix position)) -1) #f]
    [else (not-repeated-aux matrix (- position 1))]))
  

;----------------------------Validacion: Movimiento valido (forma una letra L)--------------------------
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

;-------------------------------------------Funcion PCD-Test--------------------------------------------
;Se deben ejecutar las validaciones indicadas anteriormente
(define (pdc-test n matrix)
  (cond
    [(is-nxn matrix n)
     (cond
       [(not-repeated matrix n) (pdc-test-aux n matrix (- (* n n) 1) 0)]
       [else #f]
       )]
    [else #f]))
  

;Funcion PDC-Test auxiliar, verifica que para cada elemento haya un movimiento en L valido
(define (pdc-test-aux n matrix max-element element)
  (cond
    [(= element max-element) #t]
    [(l-move (pos matrix element) (pos matrix (+ element 1))) (pdc-test-aux n matrix max-element (+ element 1))]
    [else #f]))