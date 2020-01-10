#lang racket
(define tablero
  (list 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 1 2 0 0 0
        0 0 0 2 1 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 ))

(define (getPosicion pos) ;Devuelve el valor de tablero correspondiente a las (x, y) de pos
  (list-ref tablero (+ (* 8 (list-ref pos 0)) (list-ref pos 1) ))) 

(define (dentroDelTablero coords) ;Comprueba si una posicion está dentro del tablero (x,y dentro de [0,7])
  (and (< (list-ref coords 0) 8) (>= (list-ref coords 0) 0) (< (list-ref coords 1) 8) (>= (list-ref coords 1) 0)))

(define (opuesto ficha) ;Devuelve el color de la ficha del contrario  
  (+ 1 (remainder ficha 2)))

(define (setPosicion pos jugador [res '()])
  (for/list ([i (range 64)])
    (cond
    [(equal? (list (remainder i 8) (floor (/ i 8))) pos) (append res jugador)]
    [else (append res (list-ref tablero i))]))
 )

(define (avanza coords direccion) ;Devuelve una casilla colindante en la direccion especificada
  (cond
    [(equal? direccion 0) (list (+ (list-ref coords 0) 0) (+ (list-ref coords 1) -1))];N
    [(equal? direccion 1) (list (+ (list-ref coords 0) 1) (+ (list-ref coords 1) -1))];NE
    [(equal? direccion 2) (list (+ (list-ref coords 0) 1) (+ (list-ref coords 1) 0))] ;E
    [(equal? direccion 3) (list (+ (list-ref coords 0) 1) (+ (list-ref coords 1) 1))] ;SE
    [(equal? direccion 4) (list (+ (list-ref coords 0) 0) (+ (list-ref coords 1) 1))] ;S
    [(equal? direccion 5) (list (+ (list-ref coords 0) -1) (+ (list-ref coords 1) 1))];SO
    [(equal? direccion 6) (list (+ (list-ref coords 0) -1) (+ (list-ref coords 1) 0))];O
    [(equal? direccion 7) (list (+ (list-ref coords 0) -1) (+ (list-ref coords 1) -1))];NO
    ))

;Comprueba si existe una ficha de color x más allá de n fichas de color y seguidas
(define (fichaProxima coords color direccion [avistada #f])
  (cond
    [(not (dentroDelTablero coords)) #f] ;Nos hemos salido del tablero
    [(equal? (getPosicion coords) (opuesto color)) (fichaProxima (avanza coords direccion) color direccion #t)] ;A seguir buscando
    [(and avistada (equal? color (getPosicion coords))) #t] ;Premio!
    [else #f]
    )
 )

(define (esLegal posicion color);Devuelve si es legal poner una ficha color en la posicion indicada
  (cond
    [(not (equal? (getPosicion posicion) 0)) #f]
    [(for/or ([i (range 8)])
      (fichaProxima (avanza posicion i) color i)) #t]
    [else #f]))

(esLegal '(2 3) 1)