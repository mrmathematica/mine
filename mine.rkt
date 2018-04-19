#lang racket/gui

(define height 16)
(define width 30)
(define number 99)
(define remain number)

(define (board-ref board x y)
  (vector-ref (vector-ref board x)
              y))
(define (board-set! board x y v)
  (vector-set! (vector-ref board x)
               y
               v))

(define (make-board height width number)
  (define board
    (build-vector height
                  (lambda (_)
                    (make-vector width #f))))
  (let lp ((i number))
    (when (positive? i)
      (let ((y (random height))
            (x (random width)))
        (if (board-ref board y x)
            (lp i)
            (begin (board-set! board y x #t)
                   (lp (sub1 i)))))))
  board)
(define mine
  (make-board height width number))

(define (init-view mine height width)
  ;0-8 for revealed cell with number
  ;#f for untouched cell
  ;#t for cell labeled as mine
  ;no support for `?` for now
  (build-vector height
                (lambda (_)
                  (make-vector width #f))))
(define view (init-view mine height width))

(define (safe-ref board x y)
  (if (and (<= 0 x)
           (< x height)
           (<= 0 y)
           (< y width)
           (eq? (board-ref board x y) #t))
      1
      0))

(define (count board x y)
  (+ (safe-ref board (sub1 x) (sub1 y))
     (safe-ref board (sub1 x) y)
     (safe-ref board (sub1 x) (add1 y))
     (safe-ref board x (sub1 y))
     (safe-ref board x (add1 y))
     (safe-ref board (add1 x) (sub1 y))
     (safe-ref board (add1 x) y)
     (safe-ref board (add1 x) (add1 y))))

(define (victory?)
  (let/ec k
    (for* ((y (in-range height))
           (x (in-range width)))
      (unless (board-ref mine y x)
        (unless (number? (board-ref view y x))
          (k #f))))
    (k #t)))  

(define frame
  (new frame%
       [label "Mine"]))

(define game-canvas%
  (class canvas%
    
    (define (left x y)
      (when (and (<= 0 x)
                 (< x width)
                 (<= 0 y)
                 (< y height))
        (cond ((board-ref view y x)
               (void))
              ((board-ref mine y x)
               (lost x y))
              (else
               (define v (count mine y x))
               (board-set! view y x v)
               (draw-tile (send this get-dc) x y)
               (when (zero? v)
                 (middle x y))
               (when (victory?)
                  (send message1 set-label "You win")
                 (sleep 3)
                 (send frame show #f))))))

    (define (right x y)
      (define v (board-ref view y x))
      (when (boolean? v)
        (board-set! view y x (not v))
        (draw-tile (send this get-dc) x y)
        (set! remain
              ((if v
                   add1
                   sub1)
               remain))
        (send message2 set-label (number->string remain))))
    
    (define (middle x y)
      (define v (board-ref view y x))
      (when (and (number? v)
                 (= v (count view y x)))
        (left (sub1 x) (sub1 y))
        (left (sub1 x) y)
        (left (sub1 x) (add1 y))
        (left x (sub1 y))
        (left x (add1 y))
        (left (add1 x) (sub1 y))
        (left (add1 x) y)
        (left (add1 x) (add1 y))))

    (define (lost x y)
      (define dc (send this get-dc))
      (send dc set-brush "yellow" 'solid)
      (send dc draw-rectangle (* x 40) (* y 40) 40 40)
      (send message1 set-label "You lost")
      (sleep 3)
      (send frame show #f))
  
    (define/override (on-event me)
      (case (send me get-event-type)
        [(left-up)
         (left (quotient (send me get-x) 40)
               (quotient (send me get-y) 40))]
        [(right-up)
         (right (quotient (send me get-x) 40)
                (quotient (send me get-y) 40))]
        [(middle-up)
         (middle (quotient (send me get-x) 40)
                 (quotient (send me get-y) 40))]
        [else (void)]))

    (super-new)))

(define (paint-callback canvas dc)
  (for* ((y (in-range height))
         (x (in-range width)))
    (draw-tile dc x y)))

(define font
  (make-object font% 25 'default))
(define (draw-tile dc x y)
  (define v (board-ref view y x))
  (cond ((not v)
         (send dc set-brush "gray" 'solid)
         (send dc draw-rectangle (* x 40) (* y 40) 40　40))
        ((number? v)
         (send dc set-brush "light gray" 'solid)
         (send dc draw-rectangle　(* x 40)　(* y 40)　40　40)
         (send dc set-font font)
         (send dc draw-text (number->string v) (+ (* x 40) 10) (+ (* y 40) 1)))
        (else
         (send dc set-brush "red" 'solid)
         (send dc draw-rectangle　(* x 40)　(* y 40)　40　40))))

(new game-canvas%
     [parent frame]
     [paint-callback paint-callback]
     [min-width (* width 40)]
     [min-height (* height 40)]
     [stretchable-width #f]
     [stretchable-height #f])

(define pane
  (new horizontal-pane%
       [parent frame]))

(define message1
  (new message%
       [label ""]
       [parent pane]
       [min-width 300]))

(define message2
  (new message%
       [label (number->string remain)]
       [parent pane]))

(send frame show #t)
