#lang racket

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

; Funcion que devuelve el valor en una posicion exacta de una matriz.
(define (getElementIn column row matrix)
  (cond ((or (null? matrix) (> row (length (car matrix))) (= row 0)) null)
        ((> row 1) (getElementIn column (- row 1) (cdr matrix)))
        (else (getElementIn-Aux column (car matrix)))))

(define (getElementIn-Aux column row)
  (cond ((or (null? row) (> column (length row)) (= column 0)) null)
        ((> column 1) (getElementIn-Aux (- column 1) (cdr row)))
        (else (car row))))

;Vertical
(define (vertical-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn column (+ row 1) matrix) player) (vertical-win-up column (+ row 1) matrix (+ cont 1) player flag))
    (else (vertical-win-up column row matrix cont player #f))))

(define (vertical-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn column (- row 1) matrix) player) (vertical-win-down column (- row 1) matrix (+ cont 1) player flag))
    (else (vertical-win-down column row matrix cont player #f))))

(define (vertical-win column row matrix player)
  (cond
  ((>= (+ (vertical-win-down column row matrix 0 player #t) (vertical-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))

;Horizontal
(define (horizontal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (+ column 1) row matrix) player) (horizontal-win-up (+ column 1) row matrix (+ cont 1) player flag))
    (else (horizontal-win-up column row matrix cont player #f))))

(define (horizontal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (- column 1) row matrix) player) (horizontal-win-down (- column 1) row matrix (+ cont 1) player flag))
    (else (horizontal-win-down column row matrix cont player #f))))

(define (horizontal-win column row matrix player)
  (cond
  ((>= (+ (horizontal-win-down column row matrix 0 player #t) (horizontal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))

;Diagonal par

(define (pair-diagonal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (+ column 1) (+ row 1) matrix) player) (pair-diagonal-win-up (+ column 1) (+ row 1) matrix (+ cont 1) player flag))
    (else (pair-diagonal-win-up column row matrix cont player #f))))

(define (pair-diagonal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (- column 1) (- row 1) matrix) player) (pair-diagonal-win-down (- column 1) (- row 1) matrix (+ cont 1) player flag))
    (else (pair-diagonal-win-down column row matrix cont player #f))))

(define (pair-diagonal-win column row matrix player)
  (cond
  ((>= (+ (pair-diagonal-win-down column row matrix 0 player #t) (pair-diagonal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))

;Diagonal impar

(define (odd-diagonal-win-up column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (- column 1) (+ row 1) matrix) player) (odd-diagonal-win-up (- column 1) (+ row 1) matrix (+ cont 1) player flag))
    (else (odd-diagonal-win-up column row matrix cont player #f))))

(define (odd-diagonal-win-down column row matrix cont player flag)
  (cond
    ((equal? cont 3) cont)
    ((equal? flag #f) cont)
    ((equal? (getElementIn (+ column 1) (- row 1) matrix) player) (odd-diagonal-win-down (+ column 1) (- row 1) matrix (+ cont 1) player flag))
    (else (odd-diagonal-win-down column row matrix cont player #f))))

(define (odd-diagonal-win column row matrix player)
  (cond
  ((>= (+ (odd-diagonal-win-down column row matrix 0 player #t) (odd-diagonal-win-up column row matrix 0 player #t) 1) 4) #t)
  (else #f)))

; Greeady Algorythm

; Funcion Candidatos: Por sugerencia del profesor sobre el progreso realizado una vez hecha la consulta, se opto por omitir el paso de la funcion candidatos, ya que la funcion
; viabilidad ya desempeña eficientemente el trabajo que deberian realizar ambas funciones por separado.

; Funcion Viabilidad: Selecciona los candidatos reales.
(define (viabilidad matrix playerUsingIA)
  (viabilidadAux matrix playerUsingIA (length (car matrix)) (length matrix) '()))

(define (viabilidadAux matrix playerUsingIA columns rows candidatesList)
  (cond ((= columns 0) (objetivo matrix playerUsingIA candidatesList '()))
        ((= rows 0) (viabilidadAux matrix (- columns 1) (length matrix) candidatesList))
        ((= (getElementIn columns rows matrix) 0) (viabilidadAux matrix playerUsingIA (- columns 1) (length matrix) (append candidatesList (list (list columns rows)))))
        (else (viabilidadAux matrix playerUsingIA columns (- rows 1) candidatesList))))

; Funcion Objetivo: Asigna una valoracion a cada uno de los candidatos viables.
(define (objetivo matrix playerUsingIA candidatesList candidatesRankedList)
  (cond ((null? candidatesList) candidatesRankedList)
        (else (objetivo matrix playerUsingIA (cdr candidatesList) (append candidatesRankedList (objetivoAux matrix (car candidatesList) playerUsingIA))))))

(define (objetivoAux matrix candidateToRank playerUsingIA)
  (cond ((connecting4 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(5))))
        ((blockingRivalWin matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(4))))
        ((connecting3 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(3))))
        ((connecting2 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(2))))
        ((connecting1 matrix (car candidateToRank) (car (cdr candidateToRank)) playerUsingIA) (list (append candidateToRank '(1))))
        (else (list candidateToRank 0))))

(define (connecting1 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 1) #t)
        (else #f)))

(define (connecting2 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 2) 1) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 2) #t)
        (else #f)))

(define (connecting3 matrix column row playerUsingIA)
  (cond ((= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        ((= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 3) #t)
        (else #f)))

(define (blockingRivalWin matrix column row playerUsingIA)
  (cond ((= playerUsingIA 1) (blockingRivalWinAux matrix column row 2))
        (else (blockingRivalWinAux matrix column row 1))))

(define (blockingRivalWinAux matrix column row playerUsingRival)
  (cond ((>= (+ (vertical-win-down column row matrix 0 playerUsingRival #t) (vertical-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (horizontal-win-down column row matrix 0 playerUsingRival #t) (horizontal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (pair-diagonal-win-down column row matrix 0 playerUsingRival #t) (pair-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        ((>= (+ (odd-diagonal-win-down column row matrix 0 playerUsingRival #t) (odd-diagonal-win-up column row matrix 0 playerUsingRival #t) 1) 4) #t)
        (else #f)))

(define (connecting4 matrix column row playerUsingIA)
  (cond ((>= (+ (vertical-win-down column row matrix 0 playerUsingIA #t) (vertical-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (horizontal-win-down column row matrix 0 playerUsingIA #t) (horizontal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (pair-diagonal-win-down column row matrix 0 playerUsingIA #t) (pair-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        ((>= (+ (odd-diagonal-win-down column row matrix 0 playerUsingIA #t) (odd-diagonal-win-up column row matrix 0 playerUsingIA #t) 1) 4) #t)
        (else #f)))