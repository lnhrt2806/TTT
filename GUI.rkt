#lang racket/gui
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
;Create a button matrix inside the parent component
(define (button-matrix parent function matrix (j 0))
  (cond ((null? matrix)
         #t)
        (else
         (button-matrix-aux (new horizontal-panel% [parent parent]
                                     [alignment '(center center)])
                            function
                            (car matrix)  j 0)
         (button-matrix parent function  (cdr matrix) (+ j 1) )
         )
    )
  )

(define (button-matrix-aux parent function row  i j )
  (cond ((null? row)
         #t)
        (else
         (new button% [parent parent]
             [label  (~a (car row))]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         ;(send msg set-label (string-append   "Button click "  (~a i) " "  (~a j) ))
                         (function (list i j))
                         )]
             )
         (button-matrix-aux parent  function  (cdr row) i (+ j 1))
         )
    )
  )
(define t3 '((O O O X X -)
             (O - O X - X)
             (O X O O O X)
             (X X X O - -)
             (- O - - - X)
             (O X - O X X)))


(button-matrix frame pretty-print t3 )
; Show the frame by calling its show method
(send frame show #t)