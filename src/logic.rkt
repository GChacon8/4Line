#lang racket
;;Genera una fila de la matriz solicitada, con la cantidad de columnas especificada.
(define (generaColumnas columnas lista)
  (cond
    ((equal? columnas 0) (cons lista '()))
    (else (generaColumnas (- columnas 1) (cons 0 lista)))))

;;Repite una fila encima de otra hasta saciar la cantidad de filas solicitada.
(define (generaFilas filas lista matriz)
  (cond
    ((equal? filas 0) matriz)
    (else (generaFilas (- filas 1) lista (append matriz lista)))))

;;Combina el generaColumnas y el generaFilas para crear la matriz
(define (generaMatriz filas columnas)
  (generaFilas filas (generaColumnas columnas '()) '()))