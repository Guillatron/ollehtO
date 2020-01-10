#lang racket
;JUGADOR 1 = BLANCO/////////JUGADOR 2 = NEGRO
(require (lib "Graphics.ss""graphics"))
(open-graphics)

;TABLERO
(define ventana (open-viewport "JUEGO-OTHELLO" 1100 768) )
(define color-fondo (make-rgb 29/255 166/255 28/255))
((draw-viewport ventana) color-fondo)

;Dibuja lineas horizontales
(for ([i (list 16 108 200 292 384 476 568 660 752)])((draw-line ventana)(make-posn 348 i) (make-posn 1084 i )))
;Dibuja lineas verticales
(for ([i (list 348 440 532 624 716 808 900 992 1084)] )((draw-line ventana)(make-posn i 16) (make-posn i 752)))

;Título
(define marcador-negras1(make-posn 50 190)) (define marcador-negras2(make-posn 150 190))
(define marcador-blancas1(make-posn 50 230))(define marcador-blancas2(make-posn 150 230))
(define titulo (make-posn 40 10)) ((draw-string ventana) marcador-negras1 "NEGRAS:" "black");((draw-string ventana) marcador-negras2 totalNegras "black")
((draw-string ventana) marcador-blancas1 "BLANCAS:" "white") ;((draw-string ventana) marcador-blancas2 totalBlancas "white")

;Imagen
(((draw-pixmap-posn "othello.png")ventana)titulo)

;BOTON IA
(define boton-ia (make-posn 114 300))
(define texto-ia (make-posn 155 335))
((draw-solid-rectangle ventana) boton-ia 120 60 "white")
((draw-string ventana) texto-ia "V.S  IA" "black")

;BOTON 1vs1
(define boton-11 (make-posn 114 400))
(define texto-11 (make-posn 146 435))
((draw-solid-rectangle ventana) boton-11 120 60 "white")
((draw-string ventana) texto-11 "1  V.S  1" "black")

;(close-viewport ventana )
;(close-graphics)
(define tablero
  (list 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 1 2 0 0 0
        0 0 0 2 1 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 ))

;Casilla from xy
(define (coordenadaACasilla coordenada)
  (list (floor (/ (- (list-ref coordenada 0) 348) 92))  (floor (/ (- (list-ref coordenada 1) 16) 92))))



;Pinta Ficha dada una posicion, ventana y el turno/color
(define (pintarFicha posicion ventana jugador)
    (cond [(equal? jugador 1) 
           ((draw-solid-ellipse ventana) posicion 92 92 "white") ]
          [(equal? jugador 2)
           ((draw-solid-ellipse ventana) posicion  92 92 "black") ]))

;Le pasa a PintarFicha el tablero para que lo imprima en la ventana
(define (pintarTablero ventana tablero)
 (for ([i (in-range 63)])
   (if (not(equal? (list-ref tablero i) 0)) (pintarFicha (make-posn (+ 348 (*(remainder i 8) 92)) (+ 16(*(quotient i 8)92))) ventana (list-ref tablero i)) void)
   ))
(pintarTablero ventana tablero);llamada a la funcion

(define (getPosicion pos) ;Devuelve el valor de tablero correspondiente a las (x, y) de pos
  (list-ref tablero (+ (* 8 (list-ref pos 0)) (list-ref pos 1) )))

(define (setPosicion tablero pos jugador [res '()])
  (for/list ([i (range 64)])
    (cond
    [(equal? (list (remainder i 8) (floor (/ i 8))) pos) (append res jugador)]
    [else (append res (list-ref tablero i))]))
 )
   
(define (dentroDelTablero coords) ;Comprueba si una posicion está dentro del tablero (x,y dentro de [0,7])
  (and (< (list-ref coords 0) 8) (>= (list-ref coords 0) 0) (< (list-ref coords 1) 8) (>= (list-ref coords 1) 0)))

(define (opuesto ficha) ;Devuelve el color de la ficha del contrario  
  (+ 1 (remainder ficha 2)))

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

(define (juegoAcabado tablero)
  ( (equal? (index-of tablero 0) #f) ))

(define (esLegal posicion color);Devuelve si es legal poner una ficha color en la posicion indicada
  (cond
    [(not (equal? (getPosicion posicion) 0)) #f]
    [(for/or ([i (range 8)])
      (fichaProxima (avanza posicion i) color i)) #t]
    [else #f]))

;Pinta una ficha según la posición donde se clickea
(define (pintaFichaClick ventana jugador tablero [pintadas '()])
  (let ([pos (mouse-click-posn (get-mouse-click ventana))]) ;Se almacena la posicion xy de los pixeles clickada
    (let ([coords (make-posn (+ 350 (* (quotient (- (posn-x pos) 348) 92) 92)) (+ 18 (* 92 (quotient (- (posn-y pos) 16) 92)) ))])  ;Se almacena la casilla XY seleccionada
  (if (esLegal (coordenadaACasilla (list (posn-x coords) (posn-y coords))) jugador)(pintarFicha coords ventana jugador tablero) #f))))

(pintaFichaClick ventana 1); llamada a la función


