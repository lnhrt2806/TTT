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

(define tmx   '((X - O)
                (O X -)
                (- O X)))

(define tmxv7 '((O O X)
                (- X -)
                (- - O)))


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
                 (- - -)
                 (- - -)))

(define tmxo6 '( (- O O - - -)
                 (- - O X X X)
                 (O - O O O O)))

(define tmxo7 '((- - O)
                (- - O)
                (- X O)
                (X - -)
                (X - -)
                (O - -)))

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

(define tmx11 '((- - - - - - - - - O)
                (- - - - - - - - O -)
                (X X X X X X X O X X)
                (- - - - - - O - - -)
                (X X X X X O X X X X)
                (- - - - O - - - - -)
                (- - - O - - - - - -)
                (- - O - - - - - - -)
                (- O - - - - - - - -)
                (O - - - - - - - - -)))

(define tmx101 '((O - - - - - - - - -)
                (- O - - - - - - - -)
                (X X O X - - X - X X)
                (- - - O - - - - - -)
                (X X - X O X X - X X)
                (- - - - - - - - - -)
                (- - - - - - O - - -)
                (- - - - - - - - - -)
                (- - - - - - - - O -)
                (- - - - - - - - - O)))

(define t0 '((- - - - X)
             (- - - X -)
             (O - - O O)
             (- - - - -)))

(define t00 '((- - - X -)
              (- - X - -)
              (O - - O O)
              (- - - - -)))

(define t02 '((X - - - -)
              (- X - - -)
              (O - - O O)
              (- - - - -)))

(define t03 '((- - - - )
              (- - - X )
              (- - O - )
              (- - O - )
              (- - - O )))

(define t05 '((- - - - )
              (- - - X )
              (- O - - )
              (O - - - )
              (- - - X )))

(define t033 '((- - - X )
               (- - - - )
               (- - - X )
               (- - X X )))

             

(define t1 '((- O - O X O)
             (X O X O X O)
             (X O X X X X)
             (O X O - X -)))

(define t2 '((- X O - -)
             (- X - - X)
             (X X - X O)
             (- X X O X)
             (X - O O O)
             (O O O - X)
             (X - O X -)))

(define t3 '((O O O X X -)
             (O - O X - X)
             (O X O O O X)
             (X X X O - -)
             (- O - - - X)
             (O X - O X X)))

(define t4 '((O O O X X -)
             (O - O X - X)
             (O X O O O X)
             (X X - O - -)
             (- O X - - X)
             (O X - O X X)))

(define tm35 '((- - - - -)
               (- - - - -)
               (- - - - -)))

(define tm37 '((- - - - - - -)
               (- - O O O O -)
               (- - - - - - -)))




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

;Function that generates a test matrix
(define (generateTestMatrix numrows numcols)
  (cond
    (( or (>= 2 numrows) (>= 2 numcols)
         (<= 11 numrows) (<= 11 numcols))#f)
    (else (generateTestMatrix_aux numrows numcols numcols '() '() ))))

(define (generateTestMatrix_aux numrows numcols ininumcols row matrix)
  (cond
    ((zero? numrows) matrix)
    ((zero? numcols) (generateTestMatrix_aux (- numrows 1) ininumcols ininumcols '() (append matrix (list row))))
    (else (generateTestMatrix_aux numrows (- numcols 1) ininumcols (append row (list (list-ref '(- X O) (random 3)) )) matrix))))



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
    (( or (> indexi (length matrix)) (<= indexi 0) (> indexj (length (car matrix))) (<= indexj 0)) #f) ;validates indexes
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
    ((or (checkVictoryRow player matrix)
         (checkVictoryColumn player matrix)
         (checkVictoryDiagonal player matrix)) #t)
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

;Function that counts how many elements are left to fill the row
(define (countRow player row)
  (countRow_aux player row (length row) 0))


(define (countRow_aux player row lengthrow result)
    (cond
    ((null? row) (- lengthrow result))
    ((equal? (car row) player) (countRow_aux player (cdr row) lengthrow (+ result 1)))
    (else (countRow_aux player (cdr row) lengthrow result))))
  

;Function that counts how many elements different from a token  are in a row
(define (countRowD player row)
  (cond
    ((null? row) 0)
    ((equal? player 'X)
     (cond
       ((equal? (car row) 'O) (+ 1 (countRowD player (cdr row))))
       (else (+ 0 (countRowD player (cdr row))))))
    (else
     (cond
       ((equal? (car row) 'X) (+ 1 (countRowD player (cdr row))))
       (else (+ 0 (countRowD player (cdr row))))))))

;Function that counts how many spaces are left to win the line






;Function that returns a list with lists with the indexes of all the available positions in the matrix
(define (getAvailablePositions matrix)
  (getAvailablePositions_aux (length matrix) (length (car matrix)) 1 1 '() matrix))

(define (getAvailablePositions_aux numrows numcols i j poslist matrix)
  (cond
    ((equal? i (+ numrows 1)) poslist)
    ((equal? j (+ numcols 1)) (getAvailablePositions_aux numrows numcols (+ i 1) 1 poslist matrix))
    ((equal? '- (getAtMatrix i j matrix)) (getAvailablePositions_aux numrows numcols i (+ j 1) (append poslist (list (list i j))) matrix))
    (else (getAvailablePositions_aux numrows numcols i (+ j 1) poslist matrix))))

;Function that returns a list with lists with the indexes of all the available positions in a line
(define (getEmpPos line matrix)
  (getEmpPos_aux 1 (getAt 1 line) (getAt 2 line) (getAt 4 line) (length (getAt 4 line)) (length matrix) (length (car matrix))'() ))
(define (getEmpPos_aux aux1 type num line lenline numrows numcols result)
  (cond
    ((zero? lenline) result)
    ((equal? type 'row)
     (cond
       ((equal? (getAt lenline line) '-) (getEmpPos_aux aux1 type num line (- lenline 1) numrows numcols (append (list (list num lenline)) result)))
       (else (getEmpPos_aux aux1 type num line (- lenline 1) numrows numcols result))))
    ((equal? type 'column)
     (cond
       ((equal? (getAt lenline line) '-) (getEmpPos_aux aux1 type num line (- lenline 1) numrows numcols (append (list (list lenline num )) result)))
       (else (getEmpPos_aux aux1 type num line (- lenline 1) numrows numcols result))))
    ((equal? type 'diagonal)
     (cond
       ((and (equal? (getAt lenline line) '-) (> numrows numcols));REVISAAAAAAAAAAAAAAAR
        (getEmpPos_aux aux1 type num line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list (+ numcols (- num 1)) lenline)) result)))
       ((and (equal? (getAt lenline line) '-) (> numcols numrows))
        (getEmpPos_aux aux1 type num line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list numrows (+ numrows (- num 1)))) result)))
       ((and (equal? (getAt lenline line) '-) (equal? numrows numcols))
        (getEmpPos_aux aux1 type num line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list lenline lenline)) result)))
       (else (getEmpPos_aux aux1 type num line (- lenline 1) (- numrows 1) (- numcols 1) result))))
    ((equal? type 'invdiagonal)
     (cond
         ((and (equal? (getAt lenline line) '-) (> numrows numcols))
          (getEmpPos_aux (+ aux1 1)  type  num line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list (+ lenline (- num 1)) aux1)) result)))
         ((and (equal? (getAt lenline line) '-) (equal? numrows numcols))
          (getEmpPos_aux (+ aux1 1) type num line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list lenline aux1)) result)))         
         ((and (equal? (getAt lenline line) '-) (> numcols numrows))
          (getEmpPos_aux (+ aux1 1) type (+ num 1) line (- lenline 1) (- numrows 1) (- numcols 1) (append (list (list numrows num)) result)))
         (else
          (cond
            ((or (> numrows numcols) (equal? numrows numcols)) (getEmpPos_aux (+ aux1 1) type num line (- lenline 1) (- numrows 1) (- numcols 1) result))
            (else (getEmpPos_aux (+ aux1 1) type (+ num 1) line (- lenline 1) (- numrows 1) (- numcols 1) result))))))))
          


;Function that returns the type, number, quantiti of tokens and line which is closest to win
(define (getMostFullLine player matrix)
  (getMostFullLine_aux player matrix 11 '()))

 (define (getMostFullLine_aux player matrix greater result)
   (cond
     ((< (getAt 3 (getMostFullRow player  1 11 (length matrix) matrix '())) greater)
      (getMostFullLine_aux player matrix (getAt 3 (getMostFullRow player  1 11 (length matrix) matrix '())) (getMostFullRow player  1 11 (length matrix) matrix '())))
     ((< (getAt 3 (getMostFullColumn player 1 11 (length (car matrix)) matrix '())) greater)
      (getMostFullLine_aux player matrix (getAt 3 (getMostFullColumn player 1 11 (length (car matrix)) matrix '())) (getMostFullColumn player 1 11 (length (car matrix)) matrix '())))
     ((< (getAt 3 (getMostFullDiagonal player 1 11 (length (getDiagonals matrix)) (getDiagonals matrix) '()))  greater)
      (getMostFullLine_aux player matrix (getAt 3 (getMostFullDiagonal player 1 11 (length (getDiagonals matrix)) (getDiagonals matrix) '())) (getMostFullDiagonal player 1 11 (length (getDiagonals matrix)) (getDiagonals matrix) '()))) 
     ((< (getAt 3 (getMostFullInvDiagonal player 1 11 (length (getInvDiagonals matrix)) (getInvDiagonals matrix) '())) greater)
      (getMostFullLine_aux player matrix (getAt 3 (getMostFullInvDiagonal player 1 11 (length (getInvDiagonals matrix)) (getInvDiagonals matrix) '())) (getMostFullInvDiagonal player 1 11 (length (getInvDiagonals matrix)) (getInvDiagonals matrix) '())))
     (else
      (cond
        ((null? result) result)
        (else (append result (list (getEmpPos result matrix))))))))
  


;Function that returns the type, number of row, quantity of tokens and row where there is more chance to win
(define (getMostFullRow player greatindex greater numrows matrix result)
  (cond
    ((zero? numrows) (append (list 'row) (list greatindex greater) (list result))) 
    ((and (< (countRow player (getAt numrows matrix)) greater) (zero? (countRowD player (getAt numrows matrix))))
          (getMostFullRow player numrows (countRow player (getAt numrows matrix)) (- numrows 1) matrix (getAt numrows matrix)))
    (else (getMostFullRow player greatindex greater (- numrows 1) matrix result))))


;Function that returns the type, number of column, quantity of tokens and column where there is more chance to win
(define (getMostFullColumn player greatindex greater numcols matrix result)
  (cond
    ((zero? numcols) (append (list 'column) (list greatindex greater) (list result))) 
    ((and (< (countRow player (getColumn numcols matrix)) greater) (zero? (countRowD player (getColumn numcols matrix))))
          (getMostFullColumn player numcols (countRow player (getColumn numcols matrix)) (- numcols 1) matrix (getColumn numcols matrix)))
    (else (getMostFullColumn player greatindex greater (- numcols 1) matrix result))))

;Function that returns the type, number of diagonal (from left to right, up to down), quantity of tokens and diagonal where there is more chance to win
(define (getMostFullDiagonal player greatindex greater numdiagonals diagonals result)
  (cond
    ((not (list? (car diagonals)))
     (cond
       ((zero? (countRowD player diagonals)) (append (list 'diagonal) (list 1 (countRow player diagonals)  diagonals)))
       (else (append (list 'diagonal) (list 1 11 '())))))
    ((zero? numdiagonals) (append (list 'diagonal) (list greatindex greater) (list result)))
    ((and (< (countRow player (getAt numdiagonals diagonals)) greater) (zero? (countRowD player(getAt numdiagonals diagonals))))
     (getMostFullDiagonal player numdiagonals (countRow player (getAt numdiagonals diagonals)) (- numdiagonals 1) diagonals (getAt numdiagonals diagonals)))
    (else (getMostFullDiagonal player greatindex greater (- numdiagonals 1) diagonals result))))

;Function that returns the type, number of invdiagonal (from left to right, up to down), quantity of tokens and invdiagonal where there is more chance to win
(define (getMostFullInvDiagonal player greatindex greater numinvdiagonals invdiagonals result)
  (cond
    ((not (list? (car invdiagonals)))
     (cond
       ((zero? (countRowD player invdiagonals)) (append (list 'invdiagonal) (list 1 (countRow player invdiagonals) invdiagonals)))
       (else (append (list 'invdiagonal) (list 1 11 '())))))
    ((zero? numinvdiagonals) (append (list 'invdiagonal) (list greatindex greater) (list result)))
    ((and (< (countRow player (getAt numinvdiagonals invdiagonals)) greater) (zero? (countRowD player(getAt numinvdiagonals invdiagonals))))
         (getMostFullInvDiagonal player numinvdiagonals (countRow player (getAt numinvdiagonals invdiagonals)) (- numinvdiagonals 1) invdiagonals (getAt numinvdiagonals invdiagonals)))
    (else (getMostFullInvDiagonal player greatindex greater (- numinvdiagonals 1) invdiagonals result))))

;Function that checks viability to put a token in certain place
;(player1 user, player2 computer
(define (checkViability player1 player2 matrix)
  (checkViability_aux player1 player2 (getMostFullLine player1 matrix) (getMostFullLine player2 matrix) (getAvailablePositions matrix) matrix))

(define (checkViability_aux player1 player2 gamep1 gamep2 avaipositions matrix)
  (cond
    ((and (null? gamep1) (null? gamep2)) "gameover")
    ((and (not (null? gamep2)) (equal? (length (getAt 5 gamep2)) 1)) (getAt 1 (getAt 5 gamep2)))  ;REVISAR SI PROGRAMA SE EMPIEZA A CAER
    ((null? gamep1) (getAt 1 (getAt 5 gamep2))) 
    ((equal? (length (getAt 5 gamep1)) 1) (getAt 1 (getAt 5 gamep1)))
    (else
     (cond
       ((null? gamep2) (getAt 1 (getAt 5 gamep1)))
       ((not (null? (intersection (getAt 5 gamep1) (getAt 5 gamep2)))) (car (intersection (getAt 5 gamep1) (getAt 5 gamep2))))
       (else (getAt 1 (getAt 5 gamep1)))))))

;Function that puts the token where it is more viable

(define (putToken player1 player2 matrix)
  (cond
    ((list? (checkViability player1 player2 matrix))
     (print (checkViability player1 player2 matrix))
     (newline)
     (setAtMatrix (car (checkViability player1 player2 matrix)) (cadr (checkViability player1 player2 matrix)) player2 matrix))
    (else (append (list "No movements left to win") matrix))))





;turn 0 player, 1 computer
(define (TTT numrows numcols)
  (game_aux 'O 'X (generateMatrix numrows numcols) 0))

(define (game_aux player1 player2 matrix turn)
  
  (cond
    ((equal? (car matrix) "No movements left to win") (pretty-print (cdr matrix) "Tie"))
    ((checkVictory player1 matrix) (pretty-print matrix "Player wins"))
    ((checkVictory player2 matrix) (pretty-print  matrix "Computer wins"))
    (else
     (pretty-print matrix "InProgress")
     (cond
       ((zero? turn)
        (define entry (playerTurn))
        (cond
          ((miembro entry (getAvailablePositions matrix))
           (game_aux player1 player2 (setAtMatrix (car entry) (cadr entry) player1 matrix) 1))
          (else
           (print "Posicion ocupada")
           (game_aux player1 player2 matrix turn) )))
       
       (else (game_aux player1 player2 (putToken player1 player2 matrix) 0))))))


;Function that gets the user's input

(define (playerTurn)
  (display "Input Row: ")
  (define row (read-line))
  (display "Input Col: ")
  (define col (read-line))
  (append (list (string->number row)) (list (string->number col))))

;Function that declares if an element is in a list or not
(define (miembro ele lista)
  (cond
    ((null? lista) #f)
    ((equal? (car lista) ele) #t)
    (else (miembro ele (cdr lista)))))

;Function that returns the element of intersection between two list
(define (intersection line1 line2)
  (cond
    ((null? line1) '())
    ((miembro (car line1) line2) (append (list (car line1)) (intersection (cdr line1) line2)))
    (else (intersection (cdr line1) line2))))




;Function that prints matrix
(define (pretty-print board condition)
  (for ([i (length board)])
    (for ([j (length (car board))])
      (printf "~a\t" (list-ref (list-ref board i) j)))
    (newline))
  (newline)
  condition)
     
;(getMostFullLine 'O tm37)

                  
  
  