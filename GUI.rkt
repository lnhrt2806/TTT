#lang racket/gui

; Make a frame by instantiating the frame% class
(define (create-gui rows cols)
  (define frame (new frame% [label "TTT"]
                     [width (* 100 cols)]
                     [height (* 100 rows)]))

  (define row-container (new vertical-panel%	 
                             [parent frame]))
  


  ; empezar en row 1
  (define (create-rows row rows)
    (cond ( (> row rows) '())
          (else (append (list (new horizontal-panel%	 
                                   [parent row-container]	 
                                   [min-height 100]
                                   [stretchable-height #T])) (create-rows (+ 1 row) rows)))))
  
  (define row-list (create-rows 1 rows))



                

  ; Derive a new canvas (a drawing window) class to handle events
  (define my-canvas%
    (class canvas% ; The base class is canvas%
      ; Define overriding method to handle mouse events
      (define/override (on-event event)
        (send msg set-label (send this get-label)))
      ; Define overriding method to handle keyboard events
      (define/override (on-char event)
        (send msg set-label "Canvas keyboard"))
      ; Call the superclass init, passing on all init args
      (super-new)))
  
  (define (create-cols col cols row newrow rowl)
    (cond ((null? rowl) '())
          ((<= col cols) (create-cols (+ col 1) cols row (append newrow (list (new my-canvas%
                                                                                   [parent (car rowl)]
                                                                                   [style '(border)]
                                                                                   [label (string-append "(" (number->string row) " " (number->string col) ")")]
                                                                                   [min-width 100]	 
                                                                                   [min-height 100]	 
                                                                                   [stretchable-width #F]	 
                                                                                   [stretchable-height #F]);; que contenga el canvas a ser insertado en el car de rowl
                                                                              )) rowl))
          (else (append (list newrow) (create-cols 1 cols (+ row 1) '() (cdr rowl))))))
          


  (define matriz-canvas (create-cols 1 cols 1 '() row-list))



  ; Make some pens and brushes
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define no-brush (make-object brush% "BLACK" 'transparent))
  (define red-pen (make-object pen% "RED" 2 'solid))
  (define blue-pen (make-object pen% "BLUE" 2 'solid))
  
  ; Define a procedure to draw a face
  (define (draw-x dc)
    (send dc set-pen blue-pen)
    (send dc set-brush no-brush)
    (send dc draw-line 20 20 80 80)
    (send dc draw-line 80 20 20 80))


  (define (draw-o dc)
    (send dc set-brush no-brush)
    (send dc set-pen red-pen)
    (send dc draw-ellipse 20 20 60 60))
  
  (define (clear dc)
    (send dc clear))

  (define (leer matriz matrizc)
    (for-each
     (lambda (row rowc) (leer-aux row rowc)) matriz matrizc)
    )

  (define (leer-aux row rowc)
    (for-each
     (lambda (ele can) (change ele can)) row rowc
    ))

  (define (change ele can)
    (cond ((equal? ele 'X)
           (clear (send can get-dc))
           (draw-x (send can get-dc)))
          ((equal? ele 'O)
           (clear (send can get-dc))
           (draw-o (send can get-dc)))
          (else (clear (send can get-dc)))))  

  ; Make a static text message in the frame
  (define msg (new message% [parent frame]
                   [label "No events so far..."]))






  (send frame show #t)
  
  (define tmx null)
  
  (set! tmx   '((X - O)
                (O X -)
                (- O X)))
  (sleep/yield 1)
  
  (leer tmx matriz-canvas)

  (sleep/yield 1)

  (set! tmx   '((O - O)
                (O - -)
                (X O -)))
 
  (leer tmx matriz-canvas)

  )
