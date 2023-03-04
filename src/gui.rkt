#lang racket

(require racket/gui racket/include)
(include "logic.rkt")

; Variables globales
(define player-name "")
(define color-value 1) ; 1 -> blue, 2 -> white
(define possible-values '(8 9 10 11 12 13 14 15 16))
(define rows 8)
(define cols 8)
(define board '(()))
(define current-panel 0)

#|
Nombre: set-player-name 
Descripción: Almacena el nombre del jugador que iniciará la partida contra la PC
Entradas: name ->  Nombre del jugador
Salida: Nombre almacenado
|#
(define (set-player-name name)
    (set! player-name name))

#|
Nombre: set-color-value 
Descripción: Almacena el valor asociado al color de la ficha seleccionada por el jugador
Entradas: color ->  Valor del color de la ficha
Salida: Valor del color almacenado
|#
(define (set-color-value color)
    (set! color-value color))

#|
Nombre: set-rows-cols
Descripción: Almacena el número de filas y columnas asociadas a las dimensiones del tablero
Entradas: num-rows -> número de filas
          num-cols -> número de columnas
Salida: Dimensiones del tablero almacenadas
|#
(define (set-rows-cols num-rows num-cols)
    (set! rows num-rows)
    (set! cols num-cols))

#|
Nombre: set-init-board
Descripción: Crea una matriz de rows x cols vacia que representa el tablero al inicio de la partida
Entradas: No tiene entrradas
Salida: Matriz vacia con las dimensiones del tablero
|#
(define (set-init-board)
    (set! board (generate-array rows cols)))

#|
Nombre: update-board
Descripción: Actualiza la matriz o tablero cuando o el jugador/PC agregan una nueva ficha
Entradas: array -> Matriz con una nueva ficha agregada
Salida: Tablero actualizado
|#
(define (update-board array)
    (set! board array))

; Función principal
#|
Nombre: 4Line
Descripción: Función principal que abre la ventana de inicio
Entradas: No tiene entradas
Salida: Ventana de inicio
|#
(define (4Line)
    (interface))

; Interfaz gráfica
#|
Nombre: interface
Descripción: Maneja el flujo y ejecución de todos los aspectos relacionados con la interfaz del juego
Entradas: No tiene entradas
Salida: Interfaz gráfica del juego
|#
(define (interface)

; Ventana de inicio
#|
Nombre: start-window
Descripción: Crea, distribuye y controla todos los elementos de la ventana de inicio, así como el
             acceso a las demás ventanas
Entradas: No tiene entradas
Salida: Primer ventana al iniciar el juego
|#
(define start-window
    (new frame% [label "Start Window"] [width 600] [height 300]))

    ; Principal contenedor de la ventana
    (define start-pane
        (new pane% [parent start-window]))

    ; Distribución de la ventana
    (define start-panel
        (new vertical-panel% [parent start-pane]))

    (define top-panel
        (new horizontal-panel% [parent start-panel] [alignment '(center top)]))

    (define top-panel-left
        (new vertical-panel% [parent top-panel] [alignment '(left top)] [horiz-margin 5] [vert-margin 5]))

    (define top-panel-center
        (new vertical-panel% [parent top-panel] [alignment '(center center)]))

    (define top-panel-right
        (new vertical-panel% [parent top-panel] [alignment '(right top)] [horiz-margin 5] [vert-margin 5]))

    (new message% [parent top-panel-left]
                  [label "SETTINGS"]
                  [font (make-font  #:size 10 #:family 'swiss)])

    ; Widget para establecer el nombre del jugador que se enfrentará a la PC
    (define text-field
        (new text-field% [parent top-panel-center]
                         [label "Name:"]
                         [init-value "You"]
                         [font (make-font #:size 10 #:family 'swiss)]))

    ; Widget para determinar las dimensiones del tablero
    (define combo-field
        (new combo-field% [parent top-panel-center]
                          [label "Field Size:"]
                          [init-value "Default (8x8)"]
                          [font (make-font #:size 10 #:family 'swiss)]
                          [choices '("Default (8x8)" "Large (16x16)" "Other")]
                          [callback (λ (b e) (on-combo-field))]))

    ; Función que actualiza las dimensiones del tablero
    (define (on-combo-field)
        (cond [(equal? (send combo-field get-value) "Other") (board-size-window start-window) (send combo-field set-value (string-append "Other (" (~v rows) "x" (~v cols) ")"))]))

    ; Botón para llamar a la ventana de información 
    (new button% [parent top-panel-right]
                 [label "About"]
                 [callback (λ (b e) (about-window))])
    
    (define center-panel
        (new horizontal-panel% [parent start-panel] [alignment '(center center)]))

    (define center-panel-left
        (new vertical-panel% [parent center-panel] [alignment '(left top)] [horiz-margin 5] [vert-margin 5]))

    (define center-panel-center
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (new vertical-panel% [parent center-panel] [alignment '(right top)] [horiz-margin 60])

    (define center-panel-center-left
        (new horizontal-panel% [parent center-panel-center] [alignment '(left bottom)]))

    (define center-panel-center-center
        (new horizontal-panel% [parent center-panel-center] [alignment '(center bottom)]))

    (define center-panel-center-right
        (new horizontal-panel% [parent center-panel-center] [alignment '(right bottom)]))

    (new message% [parent center-panel-left]
                  [label "SELECT YOUR COLOR"]
                  [font (make-font #:size 10 #:family 'swiss)])

    (define target1 (make-bitmap 50 50))
    (define dc1 (new bitmap-dc% [bitmap target1]))
    (send dc1 set-brush (make-object color% 70 130 180 1.0) 'transparent)
    (send dc1 set-pen "white" 2 'transparent)
    (send dc1 draw-rectangle 0 0 50 50)
    (send dc1 set-brush (make-object color% 0 150 255 1.0) 'solid)
    (send dc1 set-pen (make-object color% 0 150 255 1.0) 1 'solid)
    (send dc1 draw-ellipse 4 5 40 40)
    (send target1 save-file "src\\resources\\blue-token.png" 'png)

    ; blue-token
    (new message% [parent center-panel-center-left]
                  [label target1])

    (define target2 (make-bitmap 50 50))
    (define dc2 (new bitmap-dc% [bitmap target2]))
    (send dc2 set-brush (make-object color% 70 130 180 1.0) 'transparent)
    (send dc2 set-pen "white" 2 'transparent)
    (send dc2 draw-rectangle 0 0 50 50)
    (send dc2 set-brush "white" 'solid)
    (send dc2 set-pen "white" 1 'solid)
    (send dc2 draw-ellipse 4 5 40 40)
    (send target2 save-file "src\\resources\\white-token.png" 'png)

    ; white-token
    (new message% [parent center-panel-center-right]
                  [label target2])   

    ; Widget para determinar con que color de ficha el jugador se enfretara con la PC
    (define radio-box
        (new radio-box% [parent center-panel-center-center]
                        [label ""]
                        [font (make-font #:size 10 #:family 'swiss)] 
                        [choices (list "Blue               " "White")]
                        [style '(horizontal)]
                        [callback (λ (b e) (on-radio-box))]))

    ; Función que actualiza el color de la ficha del jugador
    (define (on-radio-box)
        (cond [(zero? (send radio-box get-selection)) (set-color-value 1)]
              [else (set-color-value 2)]))

    (define bottom-panel
        (new horizontal-panel% [parent start-panel] [alignment '(center bottom)]))

    (define target (make-bitmap 600 30))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-brush (make-object color% 0 250 154 1.0) 'solid)
    (send dc set-pen (make-object color% 0 250 154 1.0) 2 'transparent)
    (send dc draw-rectangle 0 0 600 30)
    (send dc set-font (make-font #:size 12 #:family 'swiss #:weight 'bold))
    (define-values (w h d a) (send dc get-text-extent "Start Match"))
    (send dc set-text-foreground "white")
    (send dc draw-text "Start Match" (/ (- 600 w) 2) (/ (- 30 h) 2))

    ; Botón para llamar a la ventana de juego
    (new button% [parent bottom-panel]
                 [label target]
                 [callback (λ (b e) (on-play-button (send text-field get-value) (send combo-field get-value)))])

    ; Verifica el nombre del jugador, las dimensiones del tablero y el color de la ficha antes de iniciar la partida
    (define (on-play-button name dimensions)
        (validate-dimensions dimensions)
        (validate-name name)
        (set-init-board)
        (create-board)
        (start-match))

    (define (validate-name name)
        (cond ((zero? (string-length name)) (set-player-name "You"))
              (else (set-player-name name))))

    (define (validate-dimensions dimensions)
        (cond ((equal? "Default (8x8)" dimensions) (set-rows-cols 8 8))
              ((equal? "Large (16x16)" dimensions) (set-rows-cols 16 16))))

    (define (create-board)
        (cond ((>= rows cols) (create-board-aux1))
              (else (create-board-aux2))))

    (define (create-board-aux1)
        (define target (make-bitmap (* rows 40) (* rows 40)))
        (define dc (new bitmap-dc% [bitmap target]))
        
        (define (draw-board array)
            (draw-board-aux array 0 0 5 5))

        (define (draw-board-aux array rx ry cx cy)
            (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) (send target save-file "src\\resources\\init-board.png" 'png))
                    (else (draw-board-aux (cdr array) 0 (+ ry 40) 4 (+ cy 40)))))

        (define (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) #f)
                    (else (draw-in-bitmap-aux (car array) rx ry cx cy))))

        (define (draw-in-bitmap-aux lst rx ry cx cy)
            (add-cell lst rx ry cx cy)
            (cond ((null? lst) #f)
                    (else (draw-in-bitmap-aux (cdr lst) (+ rx 40) ry (+ cx 40) cy))))

        (define (add-cell lst rx ry cx cy)
            (cond ((null? lst) #f)
                    ((equal? (car lst) 0) (draw-empty-cell rx ry))
                    ((equal? (car lst) 1) (draw-blue-token rx ry cx cy))
                    ((equal? (car lst) 2) (draw-white-token rx ry cx cy))))

        (define (draw-empty-cell rx ry)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40))

        (define (draw-blue-token rx ry cx cy)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40)
            (send dc set-brush (make-object color% 0 150 255 1.0) 'solid)
            (send dc set-pen (make-object color% 0 150 255 1.0) 1 'solid)
            (send dc draw-ellipse cx cy 30 30))

        (define (draw-white-token rx ry cx cy)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40)
            (send dc set-brush "white" 'solid)
            (send dc draw-ellipse cx cy 30 30))

        (draw-board board))

    (define (create-board-aux2)
        (define target (make-bitmap (* cols 40) (* cols 40)))
        (define dc (new bitmap-dc% [bitmap target]))
        
        (define (draw-board array)
            (draw-board-aux array 0 0 5 5))

        (define (draw-board-aux array rx ry cx cy)
            (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) (send target save-file "src\\resources\\init-board.png" 'png))
                    (else (draw-board-aux (cdr array) 0 (+ ry 40) 4 (+ cy 40)))))

        (define (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) #f)
                    (else (draw-in-bitmap-aux (car array) rx ry cx cy))))

        (define (draw-in-bitmap-aux lst rx ry cx cy)
            (add-cell lst rx ry cx cy)
            (cond ((null? lst) #f)
                    (else (draw-in-bitmap-aux (cdr lst) (+ rx 40) ry (+ cx 40) cy))))

        (define (add-cell lst rx ry cx cy)
            (cond ((null? lst) #f)
                    ((equal? (car lst) 0) (draw-empty-cell rx ry))
                    ((equal? (car lst) 1) (draw-blue-token rx ry cx cy))
                    ((equal? (car lst) 2) (draw-white-token rx ry cx cy))))

        (define (draw-empty-cell rx ry)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40))

        (define (draw-blue-token rx ry cx cy)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40)
            (send dc set-brush (make-object color% 0 150 255 1.0) 'solid)
            (send dc set-pen (make-object color% 0 150 255 1.0) 1 'solid)
            (send dc draw-ellipse cx cy 30 30))

        (define (draw-white-token rx ry cx cy)
            (send dc set-brush (make-object color% 70 130 180 1.0) 'solid)
            (send dc set-pen "white" 1 'solid)
            (send dc draw-rectangle rx ry 40 40)
            (send dc set-brush "white" 'solid)
            (send dc draw-ellipse cx cy 30 30))

        (draw-board board))

; Ventana de información
#|
Nombre: about-window
Descripción: Crea, distribuye y controla todos los elementos de la ventana de información
Entradas: No tiene entradas
Salida: Ventana de información
|#
(define (about-window)
    (define about-dialog
        (new dialog% [label "About"] [width 600] [height 300]))

    ; Principal contenedor de la ventana
    (define about-pane
        (new pane% [parent about-dialog]))

    ; Distribución de la ventana
    (define about-panel
        (new vertical-panel% [parent about-pane] [alignment '(center center)]))
    
    (define label-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center center)]))

    (define button-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center bottom)] [vert-margin 5]))

    ; Botón para regresar a la ventana de inicio
    (new button%  [parent button-panel] [label "Back"] [callback (λ (b e) (send about-dialog show #f))])   
        
    (send about-dialog show #t))

; Algunas ventanas emergentes
#|
Nombre: board-size-window
Descripción: Ventana emergente para que el jugador pueda establecer la dimensión del tablero
Entradas: place -> Lugar donde se colocará la ventana
Salida: Dimension del tablero que puede ser distinta a las de default 
|#
(define (board-size-window place)
    (define board-size (new dialog% [parent place] [label "Board Size"] [width 280] [height 120]))
    (define pane (new pane% [parent board-size]))
    (define panel (new vertical-panel% [parent pane] [alignment '(center center)] [spacing 0]))
    (define top-panel (new horizontal-panel% [parent panel] [alignment '(center top)]))
    (define bottom-panel (new horizontal-panel% [parent panel] [alignment '(center bottom)]))

    (define choice1 (new choice%
                    [label "Rows:"]
                    [parent top-panel]
                    [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                    [callback (λ (b e) (on-choice1 (send choice1 get-selection)))]))

    (define (on-choice1 selection)
        (set-rows-cols (list-ref possible-values selection) cols))

    (define choice2 (new choice%
                    [label "Cols:"]
                    [parent top-panel]
                    [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                    [callback (λ (b e) (on-choice2 (send choice2 get-selection)))]))

    (define (on-choice2 selection)
        (set-rows-cols rows (list-ref possible-values selection)))

    (new button%  [parent bottom-panel]
                  [label "OK"]
                  [callback (λ (b e) (send board-size show #f))])

    (send board-size show #t))

; Ventana de juego
#|
Nombre: start-match
Descripción: Crea, distribuye y controla todos los elementos de la ventana de juego
Entradas: No tiene entradas
Salida: Ventana de juego
|#
(define (start-match)
    (send start-window show #f)
    
    (define game-window
        (new frame% [parent start-window] [label "4Line"] [width 800] [height 600]))

    ; Principales funciones de la ventana
    (define (put-token-color pc)
        (cond ((and pc (equal? color-value 1)) "src\\resources\\white-token.png")
              ((and pc (equal? color-value 2)) "src\\resources\\blue-token.png")
              ((and (not pc) (equal? color-value 1)) "src\\resources\\blue-token.png")
              ((and (not pc) (equal? color-value 2)) "src\\resources\\white-token.png")))

    (define (columns-choices num-cols)
        (cond ((zero? num-cols) '())
              (else (append (columns-choices (- num-cols 1)) (list (~v num-cols))))))

    (define (update-center-panel game-panel panel)
        (send game-panel delete-child panel)
        (define center-panel (new vertical-panel% [parent game-panel] [alignment '(left center)]))
        (new message% [parent center-panel]
                      [label (read-bitmap "src\\resources\\init-board.png")])
        (set! current-panel center-panel))

    ; Principal contenedor de la ventana
    (define game-pane
        (new pane% [parent game-window]))

    ; Distribución de la ventana
    (define game-panel
        (new horizontal-panel% [parent game-pane] [alignment '(center center)]))
    
    (define left-panel
        (new vertical-panel% [parent game-panel] [alignment '(center center)] [horiz-margin 5] [vert-margin 5]))

    (define left-panel-up
        (new vertical-panel% [parent left-panel] [alignment '(center bottom)]))

    (new message% [parent left-panel-up]
                  [label "PLAYERS"]
                  [font (make-font  #:size 12 #:family 'swiss)])

    (define left-panel-center
        (new vertical-panel% [parent left-panel] [alignment '(center center)]))

    (define left-panel-center1
        (new horizontal-panel% [parent left-panel-center] [alignment '(center center)]))

    (new message% [parent left-panel-center1]
                  [label (read-bitmap (put-token-color #f))])

    (new message% [parent left-panel-center1]
                  [label (string-append player-name " |")]
                  [font (make-font  #:size 10 #:family 'swiss)])

    (define choice1 (new choice%
                    [label "Add at column: "]
                    [font (make-font  #:size 10 #:family 'swiss)]
                    [parent left-panel-center1]
                    [choices (columns-choices cols)]
                    [callback (λ (b e) (send choice1 enable #f) (on-choice1 (+ (send choice1 get-selection) 1)))]))

    (define (on-choice1 col-selection)
        (update-board (add-token col-selection color-value board '()))
        (create-board)
        (update-center-panel game-panel current-panel)
        (sleep/yield 1.5)
        (play-pc (if (equal? color-value 1) 2 1)))

    (define left-panel-center2
        (new horizontal-panel% [parent left-panel-center] [alignment '(center center)]))

    (new message% [parent left-panel-center2]
                  [label (read-bitmap (put-token-color #t))])

    (new message% [parent left-panel-center2]
                  [label "Computer                             "]
                  [font (make-font  #:size 10 #:family 'swiss)])

    (define (play-pc color-value-pc)
        (update-board (playsIA board color-value-pc))
        (create-board)
        (update-center-panel game-panel current-panel)
        (send choice1 enable #t))

    (define left-panel-down
        (new vertical-panel% [parent left-panel] [alignment '(center bottom)]))

    ; Botón para regresar a la ventana de inicio
    (new button% [parent left-panel-down]
                 [label "Back"]
                 [callback (λ (b e) (send game-window show #f) (send start-window show #t))])

    (define center-panel
        (new vertical-panel% [parent game-panel] [alignment '(left center)]))
    
    (new message% [parent center-panel]
                  [label (read-bitmap "src\\resources\\init-board.png")])

    (set! current-panel center-panel)

    (send game-window show #t))

(send start-window show #t))

(4Line)