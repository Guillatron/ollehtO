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
;Dibuja lienas verticales
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
;Pinta una ficha según la posición donde se clickea
(define (pintaFichaClick ventana jugador)
  (let ([pos (mouse-click-posn (get-mouse-click ventana))])
  (pintarFicha  (make-posn (+ 350 (* (quotient (- (posn-x pos) 348) 92) 92))(+ 18 (* 92 (quotient (- (posn-y pos) 16) 92)) ))  ventana jugador)))
(pintaFichaClick ventana 1); llamada a la función