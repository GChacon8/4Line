
; Genera una fila de la matriz solicitada, con la cantidad de columnas especificada
(define (generate-columns columns lst)
  (cond ((equal? columns 0) (cons lst '()))
        (else (generate-columns (- columns 1) (cons 0 lst)))))

; Repite una fila encima de otra hasta saciar la cantidad de filas solicitada
(define (generate-rows rows lst array)
  (cond ((equal? rows 0) array)
        (else (generate-rows (- rows 1) lst (append array lst)))))

; Combina generate-columns y generate-rows para crear la matriz
(define (generate-array rows columns)
  (generate-rows rows (generate-columns columns '()) '()))

; Revisa que no haya ceros en una lista especifica, en este caso representarian las filas
(define (isFullRow row)
  (cond
    ((null? row) #t)
    ((equal? (car row) 0) #f)
    (else (isFullRow (cdr row)))))

; Utiliza el revisaLlenoFila para revisar que todas las filas de la matriz no tengan cero
(define (isFull matrix)
  (cond
    ((equal? #t (isFullRow (car matrix))) #t)
    (else #f)))

; Revisa si el elemento especificado en una fila es igual a cero
(define (isEmptyRow column row)
  (cond
    ((equal? (list-ref row (- column 1)) 0) #t)
    (else #f)))

; Cambia el valor en la posicion dada por otro elemento especificado y retorna la lista con el cambio
(define (changeElement position element listB listR)
  (cond
    ((equal? position 1) (append listR (list element) (cdr listB)))
    (else (changeElement (- position 1) element (cdr listB) (append listR (list(car listB)))))))

; Retorna una lista sin su ultimo elemento
(define (withoutTail lista)
  (reverse(cdr(reverse lista))))

; Realiza el movimiento especificado por un jugador en la columna dada, retornando la matriz con la jugada realizada
(define (move column player matrix matrixP)
  (cond
    ((isEmptyRow column (list-ref matrix (- (length matrix) 1))) (append (withoutTail matrix) (list(changeElement column player (list-ref matrix (- (length matrix) 1)) '())) matrixP))
    (else (move column player (withoutTail matrix) (append (list(list-ref matrix (- (length matrix) 1))) matrixP)))))

;(move 2 1 '((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 2 0 0 0 0 0 0) (0 2 0 0 0 0 0 0) (0 1 0 0 0 0 0 0)) '())