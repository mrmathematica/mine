#lang racket/gui

(require racket/set
         racket/hash
         math/number-theory)

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

(define (safe-ref board x y v)
  (if (and (<= 0 x)
           (< x height)
           (<= 0 y)
           (< y width)
           (eq? (board-ref board x y) v))
      1
      0))

(define (board-count board x y v)
  (+ (safe-ref board (sub1 x) (sub1 y) v)
     (safe-ref board (sub1 x) y v)
     (safe-ref board (sub1 x) (add1 y) v)
     (safe-ref board x (sub1 y) v)
     (safe-ref board x (add1 y) v)
     (safe-ref board (add1 x) (sub1 y) v)
     (safe-ref board (add1 x) y v)
     (safe-ref board (add1 x) (add1 y) v)))

(define (victory?)
  (let/ec k
    (for* ((y (in-range height))
           (x (in-range width)))
      (unless (board-ref mine y x)
        (unless (number? (board-ref view y x))
          (k #f))))
    (k #t)))

(define (wrong-label?)
  (let/ec k
    (for* ((y (in-range height))
           (x (in-range width)))
      (when (eq? (board-ref view y x) #t)
        (unless (eq? (board-ref mine y x) #t)
          (k #t))))
    (k #f)))

(define (collect-equations)
  (list->set
   (for*/fold ((equations '()))
              ((y (in-range height))
               (x (in-range width)))
     (if (and (number? (board-ref view y x))
              (positive? (board-count view y x #f)))
         (cons (generate-equation x y)
               equations)
         equations))))
;equation is (cons n points), where n is number of mines, points is list of (x . y)
(define (generate-equation x y)
  (cons (- (board-ref view y x)
           (board-count view y x #t))
        (collect view y x #f)))
(define (safe-collect board y x v)
  (if (and (<= 0 y)
           (< y height)
           (<= 0 x)
           (< x width)
           (eq? (board-ref board y x) v))
      (cons x y)
      #f))
(define (collect board y x v)
  (filter identity
          (list (safe-collect board (sub1 y) (sub1 x) v)
                (safe-collect board (sub1 y) x v)
                (safe-collect board (sub1 y) (add1 x) v)
                (safe-collect board y (sub1 x) v)
                (safe-collect board y (add1 x) v)
                (safe-collect board (add1 y) (sub1 x) v)
                (safe-collect board (add1 y) x v)
                (safe-collect board (add1 y) (add1 x) v))))

(define (full-equations)
  (set-add (collect-equations)
           (remain-equation)))
(define (remain-equation)
  (cons remain
        (for*/list ((y (in-range height))
                    (x (in-range width))
                    #:unless (board-ref view y x))
          (cons x y))))

;solve the 2 most basic case:
;number of mine is 0
;mumber of mines is the smae as number of remaining tiles
(define (simple-solver equations)
  (define solution (make-hash))
  (for ((e equations))
    (cond ((zero? (car e))
           (for ((p (cdr e)))
             (hash-set! solution p 0)))
          ((= (car e) (length (cdr e)))
           (for ((p (cdr e)))
             (hash-set! solution p 1)))))
  solution)

(define (simple b e)
  (unless (wrong-label?)
    (simple2)))

(define (simple2)
  (let lp ()
    (define solution (simple-solver (full-equations)))
    (unless (hash-empty? solution)
      (hash-for-each solution
                     (lambda (p v)
                       (if (zero? v)
                           (send game left (car p) (cdr p))
                           (send game right (car p) (cdr p)))))
      (lp))))

(define (equations->table equations)
  (define table (make-hash))
  (for ((e equations))
    (for ((p (cdr e)))
      (hash-update! table
                    p
                    (lambda (es) (cons e es))
                    '())))
  table)

(define (group equations)
  (define table (equations->table equations))
  (let lp ((groups '())
           (equations equations))
    (if (set-empty? equations)
        groups
        (let ((visited? (mutable-set))
              (queue (list (set-first equations))))
          (let lp2 ()
            (if (empty? queue)
                (lp (cons visited? groups)
                    (set-subtract equations visited?))
                (let ((e (car queue)))
                  (set! queue (cdr queue))
                  (set-add! visited? e)
                  (for* ((p (cdr e))
                         (e (hash-ref table p '()))
                         #:unless (set-member? visited? e))
                    (set! queue (cons e queue)))
                  (lp2))))))))

(define (block-solve equations)
  (define table (equations->table equations))
  (define solution '())
  (define (try assign to-assign)
    (cond ((empty? to-assign)
           (set! solution (cons assign solution)))
          (else
           (when (test (car to-assign) 0 assign table)
             (try (hash-set assign (car to-assign) 0)
                  (cdr to-assign)))
           (when (test (car to-assign) 1 assign table)
             (try (hash-set assign (car to-assign) 1)
                  (cdr to-assign))))))
  (try (make-immutable-hash)
       (sort (hash-keys table) <
             #:key (lambda (p) (+ (* (car p) 16)
                                  (cdr p)))))
  solution)

(define (test p v assign table)
  (define new-assign (hash-set assign p v))
  (for/and ((e (hash-ref table p)))
    (let* ((n (car e))
           (ps (cdr e))
           (mines (map (lambda (p) (hash-ref new-assign p #f)) ps))
           (existing (apply + (filter number? mines))))
      (<= existing
          n
          (+ existing (count not mines))))))

;number of untouched tiles
(define (board-size)
  (apply +
         (map (lambda (r) (vector-count not r))
              (vector->list view))))

(define (block-prob equations)
  ;assumes there is only one block to solve in the full board
  (define ss (block-solve equations))
  (define st (make-hash))
  (define block-size (hash-count (car ss)))
  (define puzzle-size (board-size))
  (define sum 0)
  (for ((s ss))
    (define n (apply + (hash-values s)))
    (define comb (place puzzle-size remain block-size n))
    (hash-for-each s
                   (lambda (p v)
                     (hash-update! st
                                   p
                                   (lambda (old-v)
                                     (+ old-v (* v comb)))
                                   0)))
    (set! sum (+ sum comb)))
  (make-immutable-hash (hash-map st
                                 (lambda (p v)
                                   (cons p (/ v sum))))))

(define (place N n M m)
  (my-binomial (- N M) (- n m)))

(define (my-binomial n m)
  (if (or (negative? m)
          (negative? n))
      0
      (binomial n m)))

(define (safe b e)
  (unless (wrong-label?)
    (safe2)))

(define (safe2)
  (let lp ()
    (simple2)
    (define block-solution
      (map block-prob (group (collect-equations))))
    (unless (empty? block-solution)
      (define solution
        (apply hash-union block-solution))
      (define n (apply + (hash-values solution)))
      (define sure
        (filter (lambda (s)
                  (integer? (cdr s)))
                (hash->list
                 (if (< remain (+ n 1))
                     (block-prob (full-equations))
                     solution))))
      (unless (empty? sure)
        (for ((s sure))
          (if (zero? (cdr s))
              (send game left (caar s) (cdar s))
              (send game right (caar s) (cdar s))))
        (lp)))))
             
(define (auto b e)
  (let/ec k
    (let lp ()
      (safe2)
      (define block-solution
        (map block-prob (group (collect-equations))))
      (define front-solution
        (if (empty? block-solution)
            (make-immutable-hash)
            (apply hash-union block-solution)))
      (define n (apply + (hash-values front-solution)))
      (define solution
        (if (< remain (+ n 1))
            (block-prob (full-equations))
            (make-full front-solution n)))
      (when (empty? solution)
        (k (void)))
      (define p (find-min solution))
      (unless p
        (k (void)))
      (send game left (car p) (cdr p))
      (lp))))

(define (make-full solution n)
  (define untouched
    (for*/list ((y (in-range height))
                (x (in-range width))
                #:unless (or (board-ref view y x)
                             (hash-has-key? solution (cons x y))))
      (cons x y)))
  (define v (/ (- remain n)
               (length untouched)))
  (hash-union solution
              (make-immutable-hash
               (map (lambda (p)
                      (cons p v))
                    untouched))))

(define (find-min solution)
  (define c 2)
  (define b '())
  (for (((p v) solution))
    (cond ((< v c)
           (set! b (list p))
           (set! c v))
          ((= v c)
           (set! b (cons p b)))))
  (values b c))

(define (hint b e)
  (define block-solution
    (map block-prob (group (collect-equations))))
  (define front-solution
    (if (empty? block-solution)
        (make-immutable-hash)
        (apply hash-union block-solution)))
  (define n (apply + (hash-values front-solution)))
  (define solution
    (if (< remain (+ n 1))
        (block-prob (full-equations))
        (make-full front-solution n)))
  (define dc (send game get-dc))
  (unless (empty? solution)
    (let-values (((m p)
                  (find-min solution)))
      (send message1 set-label (number->string p))
      (for ((d m))
        (green #t d dc))
      (sleep 3)
      (send message1 set-label "")
      (for ((d m))
        (green #f d dc)))))

(define (green g p dc)
  (send dc set-brush (if g "green" "gray") 'solid)
  (send dc draw-rectangle (* (car p) 40) (* (cdr p) 40) 40 40))

(define frame
  (new frame%
       [label "Mine"]))

(define game-canvas%
  (class canvas%
    
    (define/public (left x y)
      (when (and (<= 0 x)
                 (< x width)
                 (<= 0 y)
                 (< y height))
        (cond ((board-ref view y x)
               (void))
              ((board-ref mine y x)
               (lost x y))
              (else
               (define v (board-count mine y x #t))
               (board-set! view y x v)
               (draw-tile (send this get-dc) x y)
               (when (zero? v)
                 (middle x y))
               (when (victory?)
                 (send message1 set-label "You win")
                 (sleep 3)
                 (send frame show #f))))))

    (define/public (right x y)
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
                 (= v (board-count view y x #t)))
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
         (send dc draw-rectangle (* x 40) (* y 40) 40 40))
        ((number? v)
         (send dc set-brush "light gray" 'solid)
         (send dc draw-rectangle (* x 40) (* y 40) 40 40)
         (send dc set-font font)
         (send dc draw-text (number->string v) (+ (* x 40) 10) (+ (* y 40) 1)))
        (else
         (send dc set-brush "red" 'solid)
         (send dc draw-rectangle (* x 40) (* y 40) 40 40))))

(define game
  (new game-canvas%
       [parent frame]
       [paint-callback paint-callback]
       [min-width (* width 40)]
       [min-height (* height 40)]
       [stretchable-width #f]
       [stretchable-height #f]))

(define pane
  (new horizontal-pane%
       [parent frame]))

(define message1
  (new message%
       [label ""]
       [parent pane]
       [min-width 300]))

(void
 (new button%
      [label "Simple solver"]
      [parent pane]
      [callback simple])
        
 (new button%
      [label "Safe solver"]
      [parent pane]
      [callback safe])

 (new button%
      [label "Hint"]
      [parent pane]
      [callback hint]))

(define message2
  (new message%
       [label (number->string remain)]
       [parent pane]))

(send frame show #t)
