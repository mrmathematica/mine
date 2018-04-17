#lang racket/gui

(define height 16)Q
(define width 30)
(define number 99)

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
      (let ((x (random height))
            (y (random width)))
        (if (board-ref board x y)
            (lp i)
            (begin (board-set! board x y #t)
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
           (< y width))
      (if (board-ref board x y)
          1
          0)
      0))

(define (count x y)
  (+ (safe-ref mine (sub1 x) (sub1 y))
     (safe-ref mine (sub1 x) y)
     (safe-ref mine (sub1 x) (add1 y))
     (safe-ref mine x (sub1 y))
     (safe-ref mine x (add1 y))
     (safe-ref mine (add1 x) (sub1 y))
     (safe-ref mine (add1 x) y)
     (safe-ref mine (add1 x) (add1 y))))

(define (lost x y)
  ;todo
  (error x y))

(define (left x y)
  (cond ((board-ref view x y)
         (void))
        ((board-ref mine x y)
         (lost x y))
        (else
         (board-set! view x y
                     (count x y)))))

(define (right x y)
  (unless (board-ref view x y)
    (board-set! view x y #t)))

(define (vectory? view)
  ;no #f in view
  (andmap (lambda (line)
            (andmap identity (vector->list line)))
          (vector->list view)))

;(define (double x y)
  
(define frame
  (new frame%
       [label "Mine"]))

(define game-canvas%
  (class canvas%
    (inherit refresh)

    (define/override (on-event me)
      (case (send me get-event-type)
        [(left-up)
         (left (quotient (send me get-y) 40)
               (quotient (send me get-x) 40))
         (refresh)]
        [else (void)]))

    (super-new)))

(new game-canvas%
     [parent frame]
     [min-width (* width 40)]
     [min-height (* height 40)]
     [stretchable-width #f]
     [stretchable-height #f])

(send frame show #t)

