;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Tarea 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;funcion principal, verifica que N no sea 0 y devuelve funcion auxiliar para resolver el problema 
(define (principal N)
  (cond ((= N 0) 0)
        (else
         (principal-aux 0 1 (crear-matriz N) (mov-posibles))
         )))

;;funcion auxiliar de la funcion principal, recibe la posicion, el valorque se irá poniendo en los indices de la
;;matriz, una matriz N*N, y una lista con los movimientos posibles para el caballo.
;;(define (principal-aux pos val matriz mov-posibles)

;;funcion para imprimir la matriz
(define (imprimir matriz)
  (cond ((null? matriz) '() )
        (else matriz)))
  

; Función que crea la matriz
(define (crear-matriz rows&cols)(
  cond ((equal? rows&cols 1) '(0))(
    else (crear_Tablero_Aux rows&cols rows&cols '())
    )
  ))

; Funcion auxiliar para crear la matriz
(define (crear-matriz-aux num_of_rc cont res)(
  cond ((equal? cont 0) res) (
    else (crear-matriz-aux num_of_rc (- cont 1) (append res (list
      (lista-ceros num_of_rc '())) ))
    )
  ))

; Funcion que crea la una lista con todos los valores igual a 0
(define (lista-ceros num_elemt rList)(
  cond ((equal? num_elemt 0) rList)(
    else (lista-ceros (- num_elemt 1) (append rList (list 0)))
    )
  ))

;Funcion para cambiar el valor de algun elemento de la matriz
(define (cambiar-valor matrix row col value)(
  cond ((null? matrix) '())(
    else (cambiar-valor-aux matrix row col '() value)
    )
  ))

;Funcion auxiliar para cambiar el valor de algun elemento de la matriz
(define (cambiar-valor-aux matrix row col res value)(
  cond ((not (equal? row 0)) (cambiar-valor-aux (cdr matrix) (- row 1) col
    (append res (list (car matrix))) value))
       ((and (equal? row 0) (not (equal? col 0))) (append (append res
         (list (susti value col (car matrix) '()))) (cdr matrix)))(
           else (append (append res (list (susti value col (car matrix) '())))
             (cdr matrix))
           )
  ))

;Funcion que obtiene el elemento de una lista
(define (obt-ele-lista matrix row col)(
  cond ((not (equal? row 0)) (get_Val (cdr matrix) (- row 1) col))
       ((and (equal? row 0) (not (equal? col 0))) (obt-ele-lista-aux col (car matrix)))
       (else (caar matrix)
         )
  ))

;Funcion auxiliar para obtener el elemento de una lista
(define (obt-ele-lista-aux pos lista)(
  cond ((equal? pos 0) (car lista))(
    else (obt-ele-lista-aux (- pos 1) (cdr lista))
    )
  ))

;Funcion que cambiar el elemento de una lista
(define (cambiar-ele-lista value pos lista res)(
  cond ((equal? pos 0) (append res (append (list value) (cdr lista))))(
    else (cambiar-ele-lista value (- pos 1) (cdr lista) (append res (list (car lista))))
    )
  ))



