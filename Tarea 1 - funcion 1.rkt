;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Tarea 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;funcion principal, verifica que N no sea inferior a 4 y devuelve funcion auxiliar para resolver el problema 
(define(PDC-Sol n posicion)
  ;;(cond (< n 4) '())
  (PDC-Sol-aux n (* n n) (crear-matriz n) 1 (car posicion) (cadr posicion) (- n 1) '(-1 -2 -2 -1 1 2 2 1) '(-2 -1 1 2 2 1 -1 -2) 0 0))
  

;;funcion auxiliar de la funcion principal, recibe un n, un tamaño (n*n), una matriz llena de ceros, el valor que irá en cada posicion de la matriz, posicion en x, posicion en y, limite de la matriz,
;; lista de posibles movimientos en x, lista de posibles movimientos en y, un contador y un flag que indica cuando se terminó la tarea
(define (PDC-Sol-aux n tamaño matriz val p-x p-y lim lx-posibles ly-posibles cont flag);auxiliar
  
  (cond ((equal? val 1) (PDC-Sol-aux n tamaño (cambiar-valor matriz p-x p-y val) (+ val 1) p-x p-y lim lx-posibles ly-posibles cont flag )) ;verifica el primer movimiento y lo anota en la matriz y aumenta el mov en 1
        
        ((equal? flag 1)(imprimir matriz)); verifica si existe solucion, esto se da cuando flag es igual a 1
        
        ((< cont 8)    ; verifica si se recorrio todo el lx-posibles y ly-posibles
                (cond ((and(and(and(>= (+ p-x (obt-ele-lista lx-posibles cont)) 0) (<= (+ p-x (obt-ele-lista lx-posibles cont)) lim))
                               (and(>= (+ p-y (obt-ele-lista  ly-posibles cont)) 0) (<= (+ p-y (obt-ele-lista ly-posibles cont)) lim))
                             )
                           (equal? (obt-ele-matriz matriz (+ p-y (obt-ele-lista ly-posibles cont)) (+ p-x (obt-ele-lista lx-posibles cont)) ) 0)
                        ) ; verifica si la nueva x y, son moviminetos validos, si puede moverse ahi solo si en ese indice es 0
                       
                (cond ((< val tamaño) (PDC-Sol-aux n tamaño (cambiar-valor matriz (+ p-x (obt-ele-lista  lx-posibles cont)) (+ p-y (obt-ele-lista ly-posibles cont)) val)
                                       (+ val 1) (+ p-x (obt-ele-lista lx-posibles cont)) (+ p-y (obt-ele-lista  ly-posibles cont)) lim lx-posibles ly-posibles 0 0 ) ; verifica si se han abarcado todos los movimientos tamaño, si no entonces actualiza el nuevo x y, y devuelve el mov aumentado en 1

                                      
;-------------------------------- AQUI VA EL BACKTRAKING PARA HACER TODAS LAS SOLUCIONES --------------------------------------------------------------------------------------------------------------
                                      
                                     ;;(cambiar-valor matriz (+ p-x (obt-ele-lista-aux lx-posibles cont)) (+ p-y (obt-ele-lista-aux ly-posibles cont)) 0);Backtraking en caso de que no se encuentre solucion actualiza la matriz en el punto que se esta  verificando y lo vuelve 0 para devolverse y recalcular
                                     ;;(PDC-Sol-aux n tamaño matriz  val  p-x   p-y  lim lx-posibles ly-posibles (+ cont 1) 0 ) ; aumenta el contador cont en 1 para acceso al lx-posibles y ly-posibles de movimiento

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      
                                     )             
                                     
                (else (PDC-Sol-aux n tamaño matriz val 0 0  lim lx-posibles ly-posibles 8 1 )))  ;si ya se abarco la solucion completa se sale del ciclo de cont y actualiza flag en 1 
                       )
                      (else (PDC-Sol-aux n tamaño matriz  val  p-x   p-y  lim lx-posibles ly-posibles (+ cont 1) 0 )))) ; si no se ha llegado la solucion aumenta el contador cont para abarcar otro movimiento en los lx-posibles, ly-posibles
         

         
         ))
  


;;funcion para imprimir la matriz
(define (imprimir matriz)
  (cond ((null? matriz) '() )
        (else matriz)))
  

; Función que crea la matriz
(define (crear-matriz n)(
  cond ((equal? n 1) '(0))(
    else (crear-matriz-aux n n '())
    )
  ))

; Funcion auxiliar para crear la matriz
(define (crear-matriz-aux n cont matriz)(
  cond ((equal? cont 0) matriz) (
    else (crear-matriz-aux n (- cont 1) (append matriz (list
      (lista-ceros n '())) ))
    )
  ))

; Funcion que crea la una lista con todos los valores igual a 0
(define (lista-ceros n lista)(
  cond ((equal? n 0) lista)(
    else (lista-ceros (- n 1) (append lista (list 0)))
    )
  ))

;Funcion para cambiar el valor de algun elemento de la matriz
(define (cambiar-valor matrix fil col valor)(
  cond ((null? matrix) '())(
    else (cambiar-valor-aux matrix fil col '() valor)
    )
  ))

;Funcion auxiliar para cambiar el valor de algun elemento de la matriz
(define (cambiar-valor-aux matrix fil col res valor)(
  cond ((not (equal? fil 0)) (cambiar-valor-aux (cdr matrix) (- fil 1) col
    (append res (list (car matrix))) valor))
       ((and (equal? fil 0) (not (equal? col 0))) (append (append res
         (list (cambiar-ele-lista valor col (car matrix) '()))) (cdr matrix)))(
           else (append (append res (list (cambiar-ele-lista valor col (car matrix) '())))
             (cdr matrix))
           )
  ))

;Funcion que obtiene el elemento de una matriz
(define (obt-ele-matriz matrix fil col)(
  cond ((not (equal? fil 0)) (obt-ele-matriz (cdr matrix) (- fil 1) col))
       ((and (equal? fil 0) (not (equal? col 0))) (obt-ele-lista (car matrix) col))
       (else (caar matrix)
         )
  ))

;Funcion auxiliar para obtener el elemento de una lista
(define (obt-ele-lista lista pos)(
  cond ((equal? pos 0) (car lista))(
    else (obt-ele-lista (cdr lista) (- pos 1))
    )
  ))

;Funcion que cambiar el elemento de una lista
(define (cambiar-ele-lista valor pos lista res)(
  cond ((equal? pos 0) (append res (append (list valor) (cdr lista))))(
    else (cambiar-ele-lista valor (- pos 1) (cdr lista) (append res (list (car lista))))
    )
  ))



