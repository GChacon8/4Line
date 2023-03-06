
#|
Nombre:
Descripción: Genera una fila de la matriz solicitada, con la cantidad de columnas especificada
Entradas: 
Salidas: 
|#
(define (generate-columns columns lst)
  (cond ((equal? columns 0) (cons lst '()))
        (else (generate-columns (- columns 1) (cons 0 lst)))))


#|
Nombre:
Descripción: Repite una fila encima de otra hasta saciar la cantidad de filas solicitada
Entradas: 
Salidas: 
|#
(define (generate-rows rows lst array)
  (cond ((equal? rows 0) array)
        (else (generate-rows (- rows 1) lst (append array lst)))))


#|
Nombre:
Descripción: Combina generate-columns y generate-rows para crear la matriz
Entradas: 
Salidas: 
|#
(define (generate-array rows columns)
  (generate-rows rows (generate-columns columns '()) '()))


#|
Nombre:
Descripción: Revisa que no haya ceros en una lista especifica
Entradas: 
Salidas: 
|#
(define (is-full-row? row)
  (cond
    ((null? row) #t)
    ((equal? (car row) 0) #f)
    (else (is-full-row? (cdr row)))))


#|
Nombre:
Descripción: Utiliza el is-full-row? para revisar que todas las filas de la matriz no tengan cero
Entradas: 
Salidas: 
|#
(define (is-full? matrix)
  (cond
    ((equal? #t (is-full-row? (car matrix))) #t)
    (else #f)))


#|
Nombre:
Descripción: Revisa si el elemento especificado en una fila es igual a cero
Entradas: 
Salidas: 
|#
(define (is-empty-row? column row)
  (cond
    ((equal? (list-ref row (- column 1)) 0) #t)
    (else #f)))


#|
Nombre:
Descripción: Cambia el valor en la posicion dada por otro elemento especificado y retorna la lista con el cambio
Entradas: 
Salidas: 
|#
(define (change-element position element listB listR)
  (cond
    ((equal? position 1) (append listR (list element) (cdr listB)))
    (else (change-element (- position 1) element (cdr listB) (append listR (list(car listB)))))))


#|
Nombre:
Descripción: Retorna una lista sin su ultimo elemento
Entradas: 
Salidas: 
|#
(define (without-tail lista)
  (reverse(cdr(reverse lista))))


#|
Nombre:
Descripción: Realiza el movimiento especificado por un jugador en la columna dada, retornando la matriz con la jugada realizada
Entradas: 
Salidas: 
|#
(define (add-token column player matrix matrixP)
  (cond
    ((is-empty-row? column (list-ref matrix (- (length matrix) 1))) (append (without-tail matrix) (list(change-element column player (list-ref matrix (- (length matrix) 1)) '())) matrixP))
    (else (add-token column player (without-tail matrix) (append (list(list-ref matrix (- (length matrix) 1))) matrixP)))))


#|
Nombre: set-element-in
Descripción: Función que reemplaza el valor en una posicion exacta de una matriz, por un nuevo valor.
Entradas: column: Columna en la que se encuentra el elemento a reemplazar.
          row: Fila en la que se encuentra el elemento a reemplazar.
          newElement: Elemento que va a reemplazar al elemento anterior.
          matrix: Matriz que representa al tablero de juego.
Salidas: newMatrix: Matriz actualizada con el nuevo elemento posicionado en la fila y columna indicada.
|#
(define (set-element-in column row newElement matrix)
  (set-element-in-aux column row newElement matrix '()))

(define (set-element-in-aux column row newElement matrix newMatrix)
  (cond ((null? matrix) newMatrix)
        ((> row (length (car matrix))) null)
        ((> row 1) (set-element-in-aux column (- row 1) newElement (cdr matrix) (append newMatrix (list (car matrix)))))
        ((= row 1) (set-element-in-aux column (- row 1) newElement (cdr matrix) (append newMatrix (list (set-element-in-aux2 column (car matrix) newElement '())))))
        (else (set-element-in-aux column (- row 1) newElement (cdr matrix) (append newMatrix (list (car matrix)))))))

(define (set-element-in-aux2 column row newElement newRow)
  (cond ((> column (length row)) null)
        ((null? row) newRow)
        ((> column 1) (set-element-in-aux2 (- column 1) (cdr row) newElement (append newRow (list (car row)))))
        ((= column 1) (set-element-in-aux2 (- column 1) (cdr row) newElement (append newRow (list newElement))))
        (else (set-element-in-aux2 (- column 1) (cdr row) newElement (append newRow (list (car row)))))))


#|
Nombre: get-element-in
Descripción: Función que devuelve el valor en una posición exacta de una matriz.
Entradas: column: Columna en la que se encuentra el elemento a consultar.
          row: Fila en la que se enecuentra el elemento a consultar.
          matrix: Matriz que representa al tablero de juego.
Salidas: Elemento ubicado en la fila y columna proporcionada.
|#
(define (get-element-in column row matrix)
  (cond ((or (null? matrix) (> row (length (car matrix))) (= row 0)) null)
        ((> row 1) (get-element-in column (- row 1) (cdr matrix)))
        (else (get-element-in-aux column (car matrix)))))

(define (get-element-in-aux column row)
  (cond ((or (null? row) (> column (length row)) (= column 0)) null)
        ((> column 1) (get-element-in-aux (- column 1) (cdr row)))
        (else (car row))))


#|
Nombre:
Descripción: Funcion
Entradas: 
Salidas: 
|#
(define (get-row-pos column matrix coord-row flag rows)
  (cond
    ((not (equal? (get-element-in column 1 matrix) 0)) 1)
    ((equal? flag #t) (+ 1 (- rows coord-row)))
    ((not (equal? 0 (list-ref (car(reverse matrix)) (- column 1)))) (get-row-pos column (without-tail matrix) (+ coord-row 1) flag rows))
    (else (get-row-pos column matrix coord-row #t rows))))

(define (win? column row matrix player)
  (cond
    ((or (odd-diagonal-win column row matrix player) (pair-diagonal-win column row matrix player) (vertical-win column row matrix player) (horizontal-win column row matrix player)) #t)
    (else #f)))


#|
Nombre:
Descripción: Vertical
Entradas: 
Salidas: 
|#
(define (vertical-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in column (+ row 1) matrix) player) (vertical-win-up column (+ row 1) matrix (+ cont 1) player flag))
    (else (vertical-win-up column row matrix cont player #f))))

(define (vertical-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in column (- row 1) matrix) player) (vertical-win-down column (- row 1) matrix (+ cont 1) player flag))
    (else (vertical-win-down column row matrix cont player #f))))

(define (vertical-win column row matrix player)
  (cond
  ((>= (+ (vertical-win-down column row matrix 0 player #t) (vertical-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))


#|
Nombre:
Descripción: Horizontal
Entradas: 
Salidas: 
|#
(define (horizontal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (+ column 1) row matrix) player) (horizontal-win-up (+ column 1) row matrix (+ cont 1) player flag))
    (else (horizontal-win-up column row matrix cont player #f))))

(define (horizontal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (- column 1) row matrix) player) (horizontal-win-down (- column 1) row matrix (+ cont 1) player flag))
    (else (horizontal-win-down column row matrix cont player #f))))

(define (horizontal-win column row matrix player)
  (cond
  ((>= (+ (horizontal-win-down column row matrix 0 player #t) (horizontal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))


#|
Nombre:
Descripción:Diagonal par
Entradas: 
Salidas: 
|#
(define (pair-diagonal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (+ column 1) (+ row 1) matrix) player) (pair-diagonal-win-up (+ column 1) (+ row 1) matrix (+ cont 1) player flag))
    (else (pair-diagonal-win-up column row matrix cont player #f))))

(define (pair-diagonal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (- column 1) (- row 1) matrix) player) (pair-diagonal-win-down (- column 1) (- row 1) matrix (+ cont 1) player flag))
    (else (pair-diagonal-win-down column row matrix cont player #f))))

(define (pair-diagonal-win column row matrix player)
  (cond
  ((>= (+ (pair-diagonal-win-down column row matrix 0 player #t) (pair-diagonal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))


#|
Nombre:
Descripción: Diagonal impar
Entradas: 
Salidas: 
|#
(define (odd-diagonal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (- column 1) (+ row 1) matrix) player) (odd-diagonal-win-up (- column 1) (+ row 1) matrix (+ cont 1) player flag))
    (else (odd-diagonal-win-up column row matrix cont player #f))))

(define (odd-diagonal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (get-element-in (+ column 1) (- row 1) matrix) player) (odd-diagonal-win-down (+ column 1) (- row 1) matrix (+ cont 1) player flag))
    (else (odd-diagonal-win-down column row matrix cont player #f))))

(define (odd-diagonal-win column row matrix player)
  (cond
  ((>= (+ (odd-diagonal-win-down column row matrix 0 player #t) (odd-diagonal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))


#|
Nombre:
Descripción: Funcion que verifica si una columna esta disponible para insertar una ficha
Entradas: 
Salidas: 
|#
(define (is-column-available? matrix column)
  (cond ((= (get-element-in column 1 matrix) 0) #t)
        (else #f)))

#| ------------------------------------------------------------------------------- Greedy Algorythm ------------------------------------------------------------------------------- |#


#|
Nombre: playsIA
Descripción: Función que inicia el proceso del algoritmo goloso.
Entradas: matrix: Matriz que representa al tablero de juego.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: No tiene salidas (Efectua un llamado a la funcion viability).
|#
(define (playsIA matrix playerUsingIA)
  (viability matrix playerUsingIA))


#|
Funcion candidates: Por sugerencia del profesor sobre el progreso realizado una vez hecha la consulta, 
se opto por omitir el paso de la funcion candidates, ya que la funcion viability desempeña eficientemente 
el trabajo que deberian realizar ambas funciones por separado.
|#


#|
Nombre: viability
Descripción: Función que selecciona los candidatos reales (unicamente aquellos donde se puede poner una ficha).
Entradas: matrix: Matriz que representa al tablero de juego.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: No tiene salidas (Efectua un llamado a la función objective)
|#
(define (viability matrix playerUsingIA)
  (viability-aux matrix playerUsingIA (length (car matrix)) (length matrix) '()))

(define (viability-aux matrix playerUsingIA columns rows candidatesList)
  (cond ((= columns 0) (objective matrix playerUsingIA candidatesList '()))
        ((= rows 0) (viability-aux matrix playerUsingIA (- columns 1) (length matrix) candidatesList))
        ((= (get-element-in columns rows matrix) 0) (viability-aux matrix playerUsingIA (- columns 1) (length matrix) (append candidatesList (list (list columns rows)))))
        (else (viability-aux matrix playerUsingIA columns (- rows 1) candidatesList))))


#|
Nombre: objective
Descripción: Función que asigna una valoración a cada uno de los candidatos viables.
Entradas: matrix: Matriz que representa al tablero de juego.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
          candidatesList: Lista de candidatos reales provenientes de la función viability.
          candidatesRankedList: Lista vacía a la que se insertaran los candidatos y su correspondiente valoración según el formato (columna, fila, valoración).
Salidas: No tiene salidas (Realiza un llamado a la función selection)
|#
(define (objective matrix playerUsingIA candidatesList candidatesRankedList)
  (cond ((null? candidatesList) (selection matrix playerUsingIA candidatesRankedList))
        (else (objective matrix playerUsingIA (cdr candidatesList) (append candidatesRankedList (objetivo-aux matrix (car candidatesList) playerUsingIA))))))


#|
Nombre: objetivo-aux
Descripción: Función que asigna un valor a cada uno de los candidatos según el resultado de las funciones connecting1, connecting2, connecting3, connecting4 y blocking-rivals-win.
Entradas: matrix: Matriz que representa al tablero de juego.
          candidateToRank: Candidato que se encuentra en evauación.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: Lista que contiene la columna, la fila y la valoración del candidato.
|#
(define (objetivo-aux matrix candidateToRank playerUsingIA)
  (cond ((connecting4 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(6))))
        ((blocking-rivals-win matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(5))))
        ((blocking-rivals-movement matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(4))))
        ((connecting3 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(3))))
        ((connecting2 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(2))))
        ((connecting1 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(1))))
        (else (list candidateToRank 0))))


#|
Nombre: connecting1
Descripción: Función que verifica que la máquina conectaria una sola ficha en fila en esa posición.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de conectar unicamente una ficha en fila o #f en caso de conectar más o menos de una.
|#
(define (connecting1 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        (else #f)))


#|
Nombre: connecting2
Descripción: Función que verifica que la máquina conectarian dos fichas en fila en esa posición.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de conectar dos fichas en fila o #f en caso de conectar más o menos de dos.
|#
(define (connecting2 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 2) 1) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        (else #f)))


#|
Nombre: connecting3
Descripción: Función que verifica que la máquina conectarian tres fichas en fila en esa posición.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de conectar tres fichas en fila o #f en caso de conectar más o menos de tres.
|#
(define (connecting3 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        (else #f)))


#|
Nombre: blocking-rivals-win
Descripción: Función que verifica si se bloquearía una posible victoria del rival.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de bloquear la victoria del rival o #f en caso contrario. 
|#
(define (blocking-rivals-win matrix column row playerUsingIA)
  (cond ((= playerUsingIA 1) (blocking-rival-win-aux matrix column row 2))
        (else (blocking-rival-win-aux matrix column row 1))))

(define (blocking-rival-win-aux matrix column row playerUsingRival)
  (cond ((>= (+ (vertical-win-down column row matrix 0 playerUsingRival #t) (vertical-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (horizontal-win-down column row matrix 0 playerUsingRival #t) (horizontal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (pair-diagonal-win-down column row matrix 0 playerUsingRival #t) (pair-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (odd-diagonal-win-down column row matrix 0 playerUsingRival #t) (odd-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        (else #f)))

#|
Nombre: blocking-rivals-movement
Descripción: Función que verifica si se bloquearía una posible movimiento agresivo del rival.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de bloquear un movimiento del rival o #f en caso contrario. 
|#
(define (blocking-rivals-movement matrix column row playerUsingIA)
  (cond ((= playerUsingIA 1) (blocking-rival-movement-aux matrix column row 2))
        (else (blocking-rival-movement-aux matrix column row 1))))

(define (blocking-rival-movement-aux matrix column row playerUsingRival)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingRival #t) (vertical-win-up column row matrix 0 playerUsingRival #t) 1) 3) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingRival #t) (horizontal-win-up column row matrix 0 playerUsingRival #t) 1) 3) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingRival #t) (pair-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 3) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingRival #t) (odd-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 3) #t)
        (else #f)))

#|
Nombre: connecting4
Descripción: Función que verifica que la máquina conectarian cuatro fichas en fila en esa posición.
Entradas: matrix: Matriz que representa al tablero de juego.
          column: Columna del candidato a evaluar.
          row: Fila del candidato a evaluar.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
Salidas: #t en caso de conectar cuatro fichas en fila o #f en caso de conectar más o menos de cuatro.
|#
(define (connecting4 matrix column row playerUsingIA)
  (cond ((>= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        (else #f)))


#|
Nombre: selection
Descripción: Función que selecciona el mejor de los candidatos una vez han sido valorados.
Entradas: matrix: Matriz que representa al tablero de juego.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
          candidatesRanked: Lista de candidatos una vez han sido valorizados por la funcion objective.
Salidas: No tiene salidas (Invoca a la función solution).
|#
(define (selection matrix playerUsingIA candidatesRanked)
  (selection-aux matrix playerUsingIA (reverse (cdr (reverse (car candidatesRanked)))) (car (reverse (car candidatesRanked))) (reverse (cdr (reverse (car candidatesRanked)))) (car (reverse (car candidatesRanked))) (cdr candidatesRanked)))

(define (selection-aux matrix playerUsingIA bestCandidate bestRank candidate rank candidatesRanked)
  (cond ((and (null? candidatesRanked) (> rank bestRank)) (solution matrix playerUsingIA candidate))
        ((null? candidatesRanked) (solution matrix playerUsingIA bestCandidate))
        ((> rank bestRank) (selection-aux matrix playerUsingIA candidate rank (reverse (cdr (reverse (car candidatesRanked)))) (car (reverse (car candidatesRanked))) (cdr candidatesRanked)))
        (else (selection-aux matrix playerUsingIA bestCandidate bestRank (reverse (cdr (reverse (car candidatesRanked)))) (car (reverse (car candidatesRanked))) (cdr candidatesRanked)))))


#|
Nombre: solution
Descripción: Función que toma el mejor de los candidatos y modifica la matriz aplicando dicho candidato.
Entradas: matrix: Matriz que representa al tablero de juego.
          playerUsingIA: Número de jugador que se encuentra utilizando la IA.
          bestCandidate: Mejor candidato de la lista proveniente de la función selection.
Salidas: No tiene saldas (Llama a la función set-element-in y esta se encarga de retornar la matriz modificada).
|#
(define (solution matrix playerUsingIA bestCandidate)
  (cond ((= playerUsingIA 1) (list (set-element-in (car bestCandidate) (car (cdr bestCandidate)) 1 matrix) bestCandidate))
        (else (list (set-element-in (car bestCandidate) (car (cdr bestCandidate)) 2 matrix) bestCandidate))))