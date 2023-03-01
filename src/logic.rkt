
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