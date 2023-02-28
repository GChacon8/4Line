#lang racket

(require racket/gui racket/include)
(include "logic.rkt")

; Función principal
#|
Nombre: 4Line
Descripción: Función principal que verifica las dimensiones del tablero al iniciar el programa
Entradas: rows -> Número de filas del tablero
          columns -> Número de columnas del tablero
Salida: Informa sobre las restricciones con respecto a las dimensiones del tablero y llama a la interfaz si
        todo está correcto
|#
(define (4Line rows columns)
    (cond ((and (< rows 8) (< columns 8) (display "The minimum size of the board must be 8x8")))
          ((and (> rows 16) (> columns 16)) (display "The maximum size of the board must be 16x16"))
          (else (interface))))

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
                  [label "SETTINGS"])

    (define text-field
        (new text-field% [parent top-panel-center]
                         [label "Name:"]
                         [init-value "You"]))

    (define combo-field
        (new combo-field% [parent top-panel-center]
                          [label "Field Size:"]
                          [init-value "Default (8x8)"]
                          [choices '("Default (8x8)" "Large (16x16)" "Other")]))

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
        (new horizontal-panel% [parent center-panel-center] [alignment '(right bottom)] [horiz-margin 0]))

    (new message% [parent center-panel-left]
                  [label "SELECT YOUR COLOR"])

    ; blue-token
    (new message% [parent center-panel-center-left]
                  [label (read-bitmap "src\\resources\\blue-token.png")])

    ; white-token
    (new message% [parent center-panel-center-right]
                  [label (read-bitmap "src\\resources\\white-token.png")])   

    (define radio-box
        (new radio-box% [parent center-panel-center-center]
                        [label ""] 
                        [choices (list "Blue               " "White")]
                        [style '(horizontal)]
                        [callback (λ (b e) (display "Test"))]))

    (define bottom-panel
        (new horizontal-panel% [parent start-panel] [alignment '(center bottom)]))

    ; Botón para llamar a la ventana de juego
    (new button% [parent bottom-panel]
                 [vert-margin 5]
                 [label "        Start Match        "]
                 [callback (λ (b e) (on-play-button))])

    (define (on-play-button)
        (start-match))

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

    ; Principal contenedor de la ventana
    (define game-pane
        (new pane% [parent game-window]))

    ; Distribución de la ventana
    (define game-panel
        (new vertical-panel% [parent game-pane] [alignment '(center center)]))
    
    (define label-panel
        (new horizontal-panel% [parent game-panel] [alignment '(center center)]))

    (define button-panel
        (new horizontal-panel% [parent game-panel] [alignment '(center bottom)] [vert-margin 5]))

    ; Botón para regresar a la ventana de inicio
    (new button% [parent button-panel]
                 [label "Back"]
                 [callback (λ (b e) (send game-window show #f) (send start-window show #t))])

    (send game-window show #t))

(send start-window show #t))

(4Line 8 8)