#lang racket

(define tm33 '((1 2 3)
               (4 5 6)
               (7 8 9)))
(define tm44 '((1 2 3 10)
               (4 5 6 11)
               (7 8 9 12)
               (13 14 15 16)))

(define tm34 '((1 2 3 10)
               (4 5 6 11)
               (7 8 9 12)))

(define tm43 '((1 2 3)
               (4 5 6)
               (7 8 9)
               (10 11 12)))

(define tmxv1 '((X X X)
                (- - -)
                (- - -)))
(define tmxv7 '((X X X)
                (- X -)
                (- - X)))


(define tmxv2 '( (- - -)
                 (X X X)
                 (- - -)))

(define tmxv3 '( (- - -)
                 (- - -)
                 (X X X)))

(define tmxv4 '( (- - -)
                 (- - -)
                 (X - X)))

(define tmxo5 '( (- - O)
                 (- - O)
                 (O - O)))

(define tmxo6 '( (- O O - - -)
                 (- - O X X X)
                 (O - O O O O)))

(define tmxo7 '((- - O)
                (- - O)
                (- X O)
                (X - O)
                (X X O)
                (O O O)))

(define tmxo8 '((- - O)
                (- - O)
                (- X O)
                (X - O)
                (X X O)
                (O O X)))


(define tmx10 '((- - - - - - - - - -)
                (- - - - - - - - - -)
                (X X X X X X X X X X)
                (- - - - - - - - - -)
                (X X X X X X X X X X)
                (- - - - - - - - - -)
                (- - - - - - - - - -)
                (- - - - - - - - - -)
                (- - - - - - - - - -)
                (- - - - - - - - - -)))

(define tmx101 '((O - - - - - - - - -)
                (- O - - - - - - - -)
                (X X O X X X X X X X)
                (- - - O - - - - - -)
                (X X X X O X X X X X)
                (- - - - - O - - - -)
                (- - - - - - O - - -)
                (- - - - - - - O - -)
                (- - - - - - - - O -)
                (- - - - - - - - - O)))




;Functions that generate game's matrix
(define (generateMatrix numrows numcols)
  (cond
    (( or (>= 2 numrows) (>= 2 numcols)
         (<= 11 numrows) (<= 11 numcols))#f)
    (else (generateMatrix_aux numrows numcols numcols '() '() ))))

(define (generateMatrix_aux numrows numcols ininumcols row matrix)
  (cond
    ((zero? numrows) matrix)
    ((zero? numcols) (generateMatrix_aux (- numrows 1) ininumcols ininumcols '() (append matrix (list row))))
    (else (generateMatrix_aux numrows (- numcols 1) ininumcols (append row (list '-)) matrix))))


;Function to get an element at a given position in a list
(define (getAt index mlist)
  (cond
    (( or (> index (length mlist)) (<= index 0)) #f);validates indexes
    ((equal? index 1) (car mlist))
    (else (getAt (- index 1) (cdr mlist)))))

;Function to get an element at a given position in a matrix
(define (getAtMatrix indexi indexj matrix)
  (getAt indexj (getAt indexi matrix)))

;Function to set an element at a given position in a list
(define (setAt index element mlist)
  (cond
     (( or (> index (length mlist)) (<= index 0)) #f);validates indexes
     ((equal? index 1) (append (list element) (cdr mlist)))
     (else (append (list (car mlist)) (setAt (- index 1) element (cdr mlist))))))

;Functions to set an element at a given position in a matrix
(define (setAtMatrix indexi indexj element matrix)
  (cond
    (( or (> indexi (length matrix)) (<= indexi 0) (> indexj (length matrix)) (<= indexj 0)) #f) ;validates indexes
    (else (setAtMatrix_aux indexi indexj element matrix))))
                  
(define (setAtMatrix_aux indexi indexj element matrix)
  (cond
    ((equal? indexi 1) (append (list (setAt indexj element (car matrix))) (cdr matrix)))
    (else (append (list (car matrix)) (setAtMatrix_aux (- indexi 1) indexj element (cdr matrix))))))

;Function that returns a column given an index
(define (getColumn indexj matrix)
  (cond
    (( or (> indexj (length (car matrix))) (<= indexj 0)) #f);validates indexes
    (else (getColumn_aux indexj (length matrix) matrix))))

(define (getColumn_aux indexj numrows matrix)
  (cond
    ((zero? numrows) '())
    (else (append  (getColumn_aux indexj (- numrows 1) matrix) (list (getAtMatrix numrows indexj matrix))))))

;Function that returns a row given an index
(define (getRow indexi matrix)
  (cond
    (( or (> indexi (length matrix)) (<= indexi 0)) #f);validates indexes
    (else (getRow_aux indexi (length (car matrix)) matrix))))

(define (getRow_aux indexi numcols matrix)
  (cond
    ((zero? numcols) '())
    (else (append  (getRow_aux indexi (- numcols 1) matrix) (list (getAtMatrix indexi numcols matrix))))))

;Function that gets a diagonal in square or nonsquare matrix
(define (getDiagonals matrix)
  (cond
    ((equal? (length matrix) (length (car matrix))) (getSquareDiagonal (length matrix) matrix))
    ((>  (length (car matrix)) (length matrix)) (getNonSquareDiagonal 1 1 1 (length matrix) (length (car matrix)) (- (length (car matrix)) (- (length matrix) 1)) '() '() matrix))
    (else (getNonSquareDiagonal 1 1 1 (length matrix) (length (car matrix)) (- (length matrix) (- (length (car matrix))  1)) '() '() matrix))))



;Function that gets the diagonal of a square matrix
(define (getSquareDiagonal numrows matrix)
  (cond
    ((zero? numrows) '())
    (else (append (getSquareDiagonal (- numrows 1) matrix) (list (getAtMatrix numrows numrows matrix))))))

;Function that gets the diagonals of a nonsquare matrix
(define (getNonSquareDiagonal i j indexdiag numrows numcols numdiags pardiag totdiag matrix)
  (cond
    ((zero? numdiags) totdiag)
    ((> numcols numrows)
     (cond
       ((equal? j (+ indexdiag numrows)) (getNonSquareDiagonal 1 (+ indexdiag 1) (+ indexdiag 1) numrows numcols (- numdiags 1) '() (append totdiag (list pardiag)) matrix))
       (else (getNonSquareDiagonal (+ i 1) (+ j 1) indexdiag numrows numcols numdiags (append pardiag (list (getAtMatrix i j matrix))) totdiag matrix))))
    ((< numcols numrows)
     (cond
       ((equal? i (+ indexdiag numcols)) (getNonSquareDiagonal (+ indexdiag 1) 1 (+ indexdiag 1) numrows numcols (- numdiags 1) '() (append totdiag (list pardiag)) matrix))
       (else (getNonSquareDiagonal (+ i 1) (+ j 1) indexdiag numrows numcols numdiags (append pardiag (list (getAtMatrix i j matrix))) totdiag matrix))
       ))))
     
;(getDiagonals tm34)
;(getDiagonals tm43)
  

;Function that gets a inverse diagonal in square or nonsquare matrix
(define (getInvDiagonals matrix)
  (cond
    ((equal? (length matrix) (length (car matrix))) (getSquareInvDiagonal (length matrix) 1 matrix))
     ((>  (length (car matrix)) (length matrix)) (getNonSquareInvDiagonal (length matrix) 1 1 (length matrix) (length (car matrix)) (- (length (car matrix)) (- (length matrix) 1)) '() '() matrix))
    (else (getNonSquareInvDiagonal 1 (length (car matrix)) 1 (length matrix) (length (car matrix)) (- (length matrix) (- (length (car matrix))  1)) '() '() matrix))))  

;Function that gets the  inverse diagonal of a square matrix
(define (getSquareInvDiagonal numrows numcols matrix)
  (cond
    ((zero? numrows) '())
    (else (append (getSquareInvDiagonal (- numrows 1) (+ numcols 1) matrix) (list (getAtMatrix numrows numcols matrix))))))

;Function that gets the inverse diagonals of a nonsquare matrix
(define (getNonSquareInvDiagonal i j   indexdiag numrows numcols numdiags pardiag totdiag matrix)
  (cond
    ((zero? numdiags) totdiag)
    ((> numcols numrows)
     (cond
      ((equal? j (+ indexdiag numrows)) (getNonSquareInvDiagonal numrows (+ indexdiag 1) (+ indexdiag 1) numrows numcols (- numdiags 1) '() (append  totdiag (list pardiag)) matrix))
       (else (getNonSquareInvDiagonal (- i 1) (+ j 1) indexdiag numrows numcols numdiags (append (list (getAtMatrix i j matrix)) pardiag ) totdiag matrix))))
    ((< numcols numrows)
     (cond
       ((equal? i (+ indexdiag numcols)) (getNonSquareInvDiagonal (+ indexdiag 1) numcols (+ indexdiag 1) numrows numcols (- numdiags 1) '() (append  totdiag (list pardiag) ) matrix))
       (else (getNonSquareInvDiagonal (+ i 1) (- j 1) indexdiag numrows numcols numdiags (append  pardiag (list (getAtMatrix i j matrix))) totdiag matrix))))))
    



;Function that verifies if there's a win

(define (checkVictory player matrix)
  (cond
    ((checkVictoryRow player matrix) #t)
    ((checkVictoryColumn player matrix) #t)
    ((checkVictoryDiagonal player matrix) #t)
    (else #f)))
    




;Functions that verify if there's a win in a row

(define (checkVictoryRow player matrix)
  (checkVictoryRow_aux player (length matrix) matrix ))

(define (checkVictoryRow_aux player numRows matrix)
  (cond
    ((zero? numRows) #f)
    ((checkRow player (getRow numRows matrix)) #t)
    (else (checkVictoryRow_aux player (- numRows 1) matrix))))

;Functions that verify if there's a win in a column
(define (checkVictoryColumn player matrix)
  (checkVictoryColumn_aux player (length (car matrix)) matrix ))

(define (checkVictoryColumn_aux player numCols matrix)
  (cond
    ((zero? numCols) #f)
    ((checkRow player (getColumn numCols matrix)) #t)
    (else (checkVictoryColumn_aux player (- numCols 1) matrix))))

;Functions that verify if there's a win in a diagonal
(define (checkVictoryDiagonal player matrix)
  (cond
    ((equal? (length matrix) (length (car matrix)))
     (cond
       ((checkRow player (getDiagonals matrix)) #t)
       ((checkRow player (getInvDiagonals matrix)) #t)
       (else #f)))
    (else
     (cond
       ((checkVictoryDiagonal_aux player (getDiagonals matrix)) #t)
       ((checkVictoryDiagonal_aux player (getInvDiagonals matrix)) #t)
       (else #f)))))

(define (checkVictoryDiagonal_aux player diagonals)
  (cond
    ((null? diagonals) #f)
    ((checkRow player (car diagonals)) #t)
    (else (checkVictoryDiagonal_aux player (cdr diagonals)))))

    
;Function that checks if all the elements in a row are the same as the player's token  
(define (checkRow player row)
  (cond
    ((null? row) #t)
    ((not (equal? (car row) player)) #f)
    (else (checkRow player (cdr row)))))




(define (setRow index  ele matrix)
  (setRow_aux index ele (length (car matrix)) matrix))
(define (setRow_aux index ele numcols matrix)
  (cond
    ((zero? numcols) matrix)
    (else (setRow_aux index ele (- numcols 1) (setAtMatrix index numcols ele matrix)))))                     
  
  