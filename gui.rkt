#lang racket

(require racket/gui racket/include)
(include "logic.rkt")

; Variables globales
(define player-name "")
(define color-value 1) ; 1 -> blue, 2 -> white
(define possible-values '(8 9 10 11 12 13 14 15 16))
(define rows 8)
(define cols 8)
(define play? #f)
(define board '(()))
(define current-row 0)
(define current-col 0)
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

#|
Nombre: columns-choices
Descripción: Función que devuelve todas las posibles columnas que el jugador puede seleccionar
Entradas: num-cols -> Número de fila
Salida: Lista con el numero de columnas
|#
(define (columns-choices num-cols)
    (cond ((zero? num-cols) '())
          (else (append (columns-choices (- num-cols 1)) (list (~v num-cols))))))

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
    (new frame% [label "Start Window"] [width 600] [height 250]))

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
                  [font (make-font  #:size 10 #:family 'swiss #:weight 'bold)]
                  [color (make-object color% 51 100 156 1.0)])

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

    ; Bitmap para botón About
    (define about-target (make-bitmap 50 25))
    (define about-dc (new bitmap-dc% [bitmap about-target]))
    (send about-dc set-brush (make-object color% 51 100 156 1.0) 'solid)
    (send about-dc set-pen (make-object color% 51 100 156 1.0) 2 'solid)
    (send about-dc draw-rectangle 0 0 50 25)
    (send about-dc set-font (make-font #:size 10 #:family 'swiss #:weight 'bold))
    (define-values (wa ha da aa) (send about-dc get-text-extent "About"))
    (send about-dc set-text-foreground "white")
    (send about-dc draw-text "About" (/ (- 50 wa) 2) (/ (- 25 ha) 2))

    ; Botón para llamar a la ventana de información 
    (new button% [parent top-panel-right]
                 [label about-target]
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
                  [font (make-font #:size 10 #:family 'swiss #:weight 'bold)]
                  [color (make-object color% 51 100 156 1.0)])

    ; Bitmap para ficha azul
    (define blue-target (make-bitmap 50 50))
    (define blue-dc (new bitmap-dc% [bitmap blue-target]))
    (send blue-dc set-brush (make-object color% 70 130 180 1.0) 'transparent)
    (send blue-dc set-pen "white" 2 'transparent)
    (send blue-dc draw-rectangle 0 0 50 50)
    (send blue-dc set-brush (make-object color% 0 150 255 1.0) 'solid)
    (send blue-dc set-pen (make-object color% 0 150 255 1.0) 1 'solid)
    (send blue-dc draw-ellipse 4 5 40 40)
    (send blue-target save-file "resources\\blue-token.png" 'png)

    ; Color ficha azul
    (new message% [parent center-panel-center-left]
                  [label blue-target])

    ; Bitmap para ficha blanca
    (define white-target (make-bitmap 50 50))
    (define white-dc (new bitmap-dc% [bitmap white-target]))
    (send white-dc set-brush (make-object color% 70 130 180 1.0) 'transparent)
    (send white-dc set-pen "white" 2 'transparent)
    (send white-dc draw-rectangle 0 0 50 50)
    (send white-dc set-brush "white" 'solid)
    (send white-dc set-pen "white" 1 'solid)
    (send white-dc draw-ellipse 4 5 40 40)
    (send white-target save-file "resources\\white-token.png" 'png)

    ; Color ficha blanca
    (new message% [parent center-panel-center-right]
                  [label white-target])   

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

    ; Bitmap para botón Start Match
    (define match-target (make-bitmap 600 30))
    (define match-dc (new bitmap-dc% [bitmap match-target]))
    (send match-dc set-brush (make-object color% 0 250 154 1.0) 'solid)
    (send match-dc set-pen (make-object color% 0 250 154 1.0) 2 'solid)
    (send match-dc draw-rectangle 0 0 600 30)
    (send match-dc set-font (make-font #:size 16 #:family 'swiss #:weight 'bold))
    (define-values (w h d a) (send match-dc get-text-extent "Start Match"))
    (send match-dc set-text-foreground "white")
    (send match-dc draw-text "Start Match" (/ (- 600 w) 2) (/ (- 30 h) 2))

    ; Botón para llamar a la ventana de juego
    (new button% [parent bottom-panel]
                 [label match-target]
                 [callback (λ (b e) (on-play-button (send text-field get-value) (send combo-field get-value)))])

    ; Función que valida el nombre del jugador y las dimensiones del tablero antes de iniciar la partida
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
        (define target (make-bitmap (* rows 40) (* (+ rows 1) 40)))
        (define dc (new bitmap-dc% [bitmap target]))
        
        (define (draw-board array)
            (draw-board-aux array 0 0 5 5))

        (define (add-column-number lst reps rx ry)
            (cond ((> reps cols) #f)
                  (else (send dc set-brush "white" 'transparent)
                        (send dc set-pen "white" 1 'transparent)
                        (send dc draw-rectangle rx ry (* rows 40) 40)
                        (send dc set-font (make-font #:size 16 #:family 'swiss))
                        (define-values (w h d a) (send dc get-text-extent (car lst)))
                        (send dc set-text-foreground "black")
                        (send dc draw-text (car lst) (+ rx 12) (+ ry 6))
                        (add-column-number (cdr lst) (+ reps 1) (+ rx 40) ry))))

        (define (draw-board-aux array rx ry cx cy)
            (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) (add-column-number (columns-choices cols) 1 rx ry) (send target save-file "resources\\init-board.png" 'png))
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

        (define (add-column-number lst reps rx ry)
            (cond ((> reps cols) #f)
                  (else (send dc set-brush "white" 'transparent)
                        (send dc set-pen "white" 1 'transparent)
                        (send dc draw-rectangle rx ry (* rows 40) 40)
                        (send dc set-font (make-font #:size 16 #:family 'swiss))
                        (define-values (w h d a) (send dc get-text-extent (car lst)))
                        (send dc set-text-foreground "black")
                        (send dc draw-text (car lst) (+ rx 12) (+ ry 6))
                        (add-column-number (cdr lst) (+ reps 1) (+ rx 40) ry))))

        (define (draw-board-aux array rx ry cx cy)
            (draw-in-bitmap array rx ry cx cy)
            (cond ((null? array) (add-column-number (columns-choices cols) 1 rx ry) (send target save-file "resources\\init-board.png" 'png))
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

    ; Widget para determinar el número de filas del tablero 
    (define choice1 (new choice%
                    [label "Rows:"]
                    [parent top-panel]
                    [font (make-font #:size 10 #:family 'swiss)]
                    [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                    [callback (λ (b e) (on-choice1 (send choice1 get-selection)))]))

    (define (on-choice1 selection)
        (set-rows-cols (list-ref possible-values selection) cols))

    ; Widget para determinar el número de columnas del tablero 
    (define choice2 (new choice%
                    [label "   Cols:"]
                    [parent top-panel]
                    [font (make-font #:size 10 #:family 'swiss)]
                    [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                    [callback (λ (b e) (on-choice2 (send choice2 get-selection)))]))

    (define (on-choice2 selection)
        (set-rows-cols rows (list-ref possible-values selection)))

    ; Bitmap para botón OK
    (define ok-target (make-bitmap 60 25))
    (define ok-dc (new bitmap-dc% [bitmap ok-target]))
    (send ok-dc set-brush (make-object color% 0 250 154 1.0) 'solid)
    (send ok-dc set-pen (make-object color% 0 250 154 1.0) 2 'solid)
    (send ok-dc draw-rectangle 0 0 60 25)
    (send ok-dc set-font (make-font #:size 10 #:family 'swiss #:weight 'bold))
    (define-values (w h d a) (send ok-dc get-text-extent "OK"))
    (send ok-dc set-text-foreground "white")
    (send ok-dc draw-text "OK" (/ (- 60 w) 2) (/ (- 25 h) 2))

    ; Botón para cerrar board-size-window
    (new button%  [parent bottom-panel]
                  [label ok-target]
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
        (new frame% [parent start-window] [label "4Line"] [width 600] [height 400]))

    ; Principales funciones de la ventana
    ; Función para volver a empezar el juego
    (define (play-again)
        (send game-window show #f)
        (send start-window show #t))

    ; Función que agrega el color de la ficha para el jugador y la PC
    (define (put-token-color pc)
        (cond ((and pc (equal? color-value 1)) "resources\\white-token.png")
              ((and pc (equal? color-value 2)) "resources\\blue-token.png")
              ((and (not pc) (equal? color-value 1)) "resources\\blue-token.png")
              ((and (not pc) (equal? color-value 2)) "resources\\white-token.png")))

    ; Función que actualiza el tablero cuando el jugador o la PC agregan una ficha
    (define (update-center-panel game-panel panel)
        (send game-panel delete-child panel)
        (define center-panel (new vertical-panel% [parent game-panel] [alignment '(left center)]))
        (new message% [parent center-panel]
                      [label (read-bitmap "resources\\init-board.png")])
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
        (new vertical-panel% [parent left-panel] [alignment '(left top)]))

    ; Bitmap para botón Back
    (define back-target (make-bitmap 50 25))
    (define back-dc (new bitmap-dc% [bitmap back-target]))
    (send back-dc set-brush (make-object color% 220 20 60 1.0) 'solid)
    (send back-dc set-pen (make-object color% 220 20 60 1.0) 2 'solid)
    (send back-dc draw-rectangle 0 0 50 25)
    (send back-dc set-font (make-font #:size 10 #:family 'swiss #:weight 'bold))
    (define-values (wb hb db ab) (send back-dc get-text-extent "Back"))
    (send back-dc set-text-foreground "white")
    (send back-dc draw-text "Back" (/ (- 50 wb) 2) (/ (- 25 hb) 2))

    ; Botón para regresar a la ventana de inicio
    (new button% [parent left-panel-up]
                 [label back-target]
                 [callback (λ (b e) (send game-window show #f) (send start-window show #t))])

    (define left-panel-center
        (new vertical-panel% [parent left-panel] [alignment '(center center)] [vert-margin 75]))

    (new message% [parent left-panel-center]
                  [label "Players"]
                  [font (make-font  #:size 15 #:family 'swiss #:weight 'bold)]
                  [color (make-object color% 51 100 156 1.0)])

    (define left-panel-down
        (new vertical-panel% [parent left-panel-center] [alignment '(center top)] [vert-margin 25]))

    (define left-panel-down1
        (new horizontal-panel% [parent left-panel-down] [alignment '(center center)]))

    ; Colocar color de ficha para el jugador
    (new message% [parent left-panel-down1]
                  [label (read-bitmap (put-token-color #f))])

    ; Colocar nombre del jugador
    (new message% [parent left-panel-down1]
                  [label (string-append player-name " |")]
                  [font (make-font  #:size 10 #:family 'swiss)])

    ; Widget para determinar en cual columna el jugador va a agregar una ficha
    (define choice1 (new choice%
                    [label "Add at column: "]
                    [font (make-font  #:size 10 #:family 'swiss)]
                    [parent left-panel-down1]
                    [choices (columns-choices cols)]
                    [callback (λ (b e) (send choice1 enable #f) (on-choice1 (+ (send choice1 get-selection) 1)))]))

    ; Función para actualizar el tablero cuando el jugador agrega una ficha
    (define (on-choice1 col-selection)
        (cond ((not (is-column-available? board col-selection))
                    (sleep/yield 0.2)
                    (send game-window enable #f)
                    (warning-window game-window (string-append "Column " (~v col-selection) " is already full."))
                    (if (and (not (send game-window is-enabled?)) (not play?)) (continue-playing) #t))
              (else (player-choice col-selection))))

    (define (player-choice col-selection)
        (update-board (add-token col-selection color-value board '()))
        (create-board)
        (update-center-panel game-panel current-panel)
        (set! current-row (get-row-pos col-selection board 0 #f rows))
        (cond ((win? col-selection current-row board color-value)
                    (sleep/yield 0.5)
                    (send game-window enable #f)
                    (winner-window game-window (string-append player-name " won the match!"))
                    (if (and (not (send game-window is-enabled?)) (not play?)) (play-again) #t))
              ((is-full? board)
                    (sleep/yield 0.5)
                    (send game-window enable #f)
                    (winner-window game-window "The match stayed tied!")
                    (if (and (not (send game-window is-enabled?)) (not play?)) (play-again) #t))
              (else (sleep/yield 1) 
                    (pc-choice (if (equal? color-value 1) 2 1)))))

    (define (continue-playing)
        (send game-window show #t) 
        (send game-window enable #t) 
        (send choice1 enable #t))

    (define left-panel-down2
        (new horizontal-panel% [parent left-panel-down] [alignment '(center center)]))

    ; Colocar color de ficha para PC
    (new message% [parent left-panel-down2]
                  [label (read-bitmap (put-token-color #t))])

    ; Colocar nombre Computer
    (new message% [parent left-panel-down2]
                  [label "Computer                             "]
                  [font (make-font  #:size 10 #:family 'swiss)])

    ; Función para actualizar el tablero cuando la PC agrega una ficha
    (define (pc-choice color-value-pc)
        (set! current-col (caadr (playsIA board color-value-pc)))
        (set! current-row (cadadr (playsIA board color-value-pc)))
        (update-board (car (playsIA board color-value-pc)))
        (create-board)
        (update-center-panel game-panel current-panel)
        (cond ((win? current-col current-row board color-value-pc) 
                    (sleep/yield 0.5) 
                    (send game-window enable #f) 
                    (winner-window game-window "Computer won the match!") 
                    (if (and (not (send game-window is-enabled?)) (not play?)) (play-again) #t))
              ((is-full? board)
                    (sleep/yield 0.5)
                    (send game-window enable #f)
                    (winner-window game-window "The match stayed tied!")
                    (if (and (not (send game-window is-enabled?)) (not play?)) (play-again) #t))
              (else (send choice1 enable #t))))
              
    (define center-panel
        (new vertical-panel% [parent game-panel] [alignment '(left center)]))
    
    ; Agregar tablero vacio
    (new message% [parent center-panel]
                  [label (read-bitmap "resources\\init-board.png")])
    (set! current-panel center-panel)

    #|
    Nombre: winner-window
    Descripción: Ventana que indica quien ha ganado la partida o si hubo un empate entre el jugador y la PC
    Entradas: place -> Lugar donde se colocará la ventana
              msj -> Mensaje que se mostrará en la ventana
    Salida: Ventana de gane o empate
    |#
    (define (winner-window place msj)
    (define winner-dialog (new dialog% [parent place] [label "Winner"] [stretchable-width #f] [stretchable-height #f] [style '(no-caption)]))
    (define pane (new pane% [parent winner-dialog]))
    (define panel (new vertical-panel% [parent pane] [alignment '(center center)]))
    
    (define h1-panel (new horizontal-panel% [parent panel] [alignment '(center center)] [vert-margin 20]))
    (new message% [parent h1-panel]
                  [label msj]
                  [font (make-font  #:size 12 #:family 'swiss)])

    (define h2-panel (new horizontal-panel% [parent panel] [alignment '(center bottom)]))

    ; Bitmap para botón Play Again
    (define play-target (make-bitmap 190 30))
    (define play-dc (new bitmap-dc% [bitmap play-target]))
    (send play-dc set-brush (make-object color% 0 250 154 1.0) 'solid)
    (send play-dc set-pen (make-object color% 0 250 154 1.0) 2 'transparent)
    (send play-dc draw-rectangle 0 0 190 30)
    (send play-dc set-font (make-font #:size 12 #:family 'swiss #:weight 'bold))
    (define-values (wp hp dp ap) (send play-dc get-text-extent "Play Again"))
    (send play-dc set-text-foreground "white")
    (send play-dc draw-text "Play Again" (/ (- 190 wp) 2) (/ (- 30 hp) 2))

    (set! play? #f)
    ; Botón para regresar a la ventana de inicio
    (new button% [parent h2-panel] [label play-target] [callback (λ (b e) (send winner-dialog show #f) (set! play? #t) (play-again))])

    ; Bitmap para botón Exit
    (define exit-target (make-bitmap 190 30))
    (define exit-dc (new bitmap-dc% [bitmap exit-target]))
    (send exit-dc set-brush (make-object color% 220 20 60 1.0) 'solid)
    (send exit-dc set-pen (make-object color% 220 20 60 1.0) 2 'transparent)
    (send exit-dc draw-rectangle 0 0 190 30)
    (send exit-dc set-font (make-font #:size 12 #:family 'swiss #:weight 'bold))
    (define-values (we he de ae) (send exit-dc get-text-extent "Exit"))
    (send exit-dc set-text-foreground "white")
    (send exit-dc draw-text "Exit" (/ (- 190 we) 2) (/ (- 30 he) 2))

    ; Botón para cerrar el programa
    (new button% [parent h2-panel] [label exit-target] [callback (λ (b e) (send winner-dialog show #f) (set! play? #t) (send game-window show #f))])

    (send winner-dialog show #t))

    #|
    Nombre: warning-window
    Descripción: Ventana que le indica al jugador que no puede agregar la ficha sobre una columna que ya esta llena
    Entradas: place -> Lugar donde se colocará la ventana
              msj -> Mensaje que se mostrará en la ventana
    Salida: Ventana de advertencia
    |#
    (define (warning-window place msj)
    (define warning-dialog (new dialog% [parent place] [label "Warning"] [stretchable-width #f] [stretchable-height #f] [style '(no-caption)]))
    (define pane (new pane% [parent warning-dialog]))
    (define panel (new vertical-panel% [parent pane] [alignment '(center center)]))
    
    (define h1-panel (new horizontal-panel% [parent panel] [alignment '(center center)] [horiz-margin 60] [vert-margin 10]))
    (new message% [parent h1-panel]
                  [label msj]
                  [font (make-font  #:size 12 #:family 'swiss)])

    (define h2-panel (new horizontal-panel% [parent panel] [alignment '(center bottom)]))

    ; Bitmap para botón OK
    (define ok-target (make-bitmap 60 25))
    (define ok-dc (new bitmap-dc% [bitmap ok-target]))
    (send ok-dc set-brush (make-object color% 0 250 154 1.0) 'solid)
    (send ok-dc set-pen (make-object color% 0 250 154 1.0) 2 'solid)
    (send ok-dc draw-rectangle 0 0 60 25)
    (send ok-dc set-font (make-font #:size 10 #:family 'swiss #:weight 'bold))
    (define-values (w h d a) (send ok-dc get-text-extent "OK"))
    (send ok-dc set-text-foreground "white")
    (send ok-dc draw-text "OK" (/ (- 60 w) 2) (/ (- 25 h) 2))

    (set! play? #f)
    ; Botón para cerrar board-size-window
    (new button%  [parent h2-panel]
                  [label ok-target]
                  [callback (λ (b e) (send warning-dialog show #f) (set! play? #t) (send game-window show #t) (send game-window enable #t) (send choice1 enable #t))])

    (send warning-dialog show #t))

    (send game-window show #t))

(send start-window show #t))

(4Line)