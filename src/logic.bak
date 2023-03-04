
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

; Revisa que no haya ceros en una lista especifica
(define (is-full-row? row)
  (cond
    ((null? row) #t)
    ((equal? (car row) 0) #f)
    (else (is-full-row? (cdr row)))))

; Utiliza el is-full-row? para revisar que todas las filas de la matriz no tengan cero
(define (is-full? matrix)
  (cond
    ((equal? #t (is-full-row? (car matrix))) #t)
    (else #f)))

; Revisa si el elemento especificado en una fila es igual a cero
(define (is-empty-row? column row)
  (cond
    ((equal? (list-ref row (- column 1)) 0) #t)
    (else #f)))

; Cambia el valor en la posicion dada por otro elemento especificado y retorna la lista con el cambio
(define (change-element position element listB listR)
  (cond
    ((equal? position 1) (append listR (list element) (cdr listB)))
    (else (change-element (- position 1) element (cdr listB) (append listR (list(car listB)))))))

; Retorna una lista sin su ultimo elemento
(define (without-tail lista)
  (reverse(cdr(reverse lista))))

; Realiza el movimiento especificado por un jugador en la columna dada, retornando la matriz con la jugada realizada
(define (add-token column player matrix matrixP)
  (cond
    ((is-empty-row? column (list-ref matrix (- (length matrix) 1))) (append (without-tail matrix) (list(change-element column player (list-ref matrix (- (length matrix) 1)) '())) matrixP))
    (else (add-token column player (without-tail matrix) (append (list(list-ref matrix (- (length matrix) 1))) matrixP)))))

;(add-token 2 1 '((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 2 0 0 0 0 0 0) (0 2 0 0 0 0 0 0) (0 1 0 0 0 0 0 0)) '())