#lang racket/gui

(require ffi/unsafe
         racket/set
         racket/hash
         math/number-theory)

(define gdi32
  (ffi-lib "gdi32"))

(define user32
  (ffi-lib "user32"))

(define GetDCEx
  (get-ffi-obj 'GetDCEx user32
               (_fun _pointer (_pointer = #f) (_uint = 3) -> _pointer)))

(define GetPixel
  (get-ffi-obj 'GetPixel gdi32
               (_fun _pointer _int _int -> _uint32)))

(define (p->c p)
  (list (bitwise-bit-field p 0 8)
        (bitwise-bit-field p 8 16)
        (bitwise-bit-field p 16 24)))

(define GetForegroundWindow
  (get-ffi-obj 'GetForegroundWindow user32
               (_fun -> _pointer)))

(define GetWindowTextLengthA
  (get-ffi-obj 'GetWindowTextLengthA user32
               (_fun _pointer -> _int)))

(define PostMessage
  (get-ffi-obj 'PostMessageA user32
               (_fun _pointer _uint _uint _uint -> _bool)))

(define-cstruct _RECT
  ((left _long) (top _long) (right _long) (bottom _long)))

(define GetWindowRect
  (let ((get
         (get-ffi-obj 'GetWindowRect user32
                      (_fun _pointer _RECT-pointer -> _bool))))
    (lambda (hWnd)
      (define lpRect (make-RECT 0 0 0 0))
      (get hWnd lpRect)
      lpRect)))

(define SetCursorPos
  (get-ffi-obj 'SetCursorPos user32
               (_fun _int _int -> _bool)))

(define (left hWnd x y)
  (SetCursorPos (+ x (RECT-left rect))
                (+ y (RECT-top rect)))
  (sleep 0.05)
  (PostMessage hWnd #x0201 1 0)
  (PostMessage hWnd #x0202 1 0)
  (sleep 0.05))

(define (right hWnd x y)
  (SetCursorPos (+ x (RECT-left rect))
                (+ y (RECT-top rect)))
  (sleep 0.05)
  (PostMessage hWnd #x0204 2 0)
  (PostMessage hWnd #x0205 2 0))


(define window
  (GetForegroundWindow))
(define rect
  (GetWindowRect window))

(define len (GetWindowTextLengthA window))

(define GetWindowTextA
  (get-ffi-obj 'GetWindowTextA user32
               (_fun _pointer (s : (_bytes o len)) (_int = len) -> _int
                     -> s)))

(define text (GetWindowTextA window))
(unless (and (= (bytes-ref text 0) 201)
             (= (bytes-ref text 1) 168))
  (error "current windows is not 扫雷"))
(define dc
  (GetDCEx window))

(define xbase 48)
(define ybase 90)
(define grid 18)

(define (read x y)
  (define p1
    (GetPixel dc
              (+ (* x grid) xbase)
              (+ (* y grid) ybase)))
  (define p2
    (GetPixel dc
              (+ (* x grid) xbase 0)
              (+ (* y grid) ybase 2)))
  (define p4
    (GetPixel dc
              (+ (* x grid) xbase 3)
              (+ (* y grid) ybase 3)))
  (define p8
    (GetPixel dc
              (+ (* x grid) xbase -2)
              (+ (* y grid) ybase 2)))
  (cond ((= p1 12472384)
         1)
        ((and (>= (bitwise-bit-field p8 0 8) 150)
              (< (bitwise-bit-field p8 8 16) 50)
              (< (bitwise-bit-field p8 16 24) 50))
         8)        
        ((and (>= (bitwise-bit-field p2 8 16) 90)
              (< (bitwise-bit-field p2 0 8) 80)
              (< (bitwise-bit-field p2 16 24) 80))
         2)
        ((and (>= (bitwise-bit-field p1 0 8) 150)
              (< (bitwise-bit-field p1 8 16) 60)
              (< (bitwise-bit-field p1 16 24) 70))
         3)
        ((and (>= (bitwise-bit-field p4 16 24) 120)
              (< (bitwise-bit-field p4 0 8) 40)
              (< (bitwise-bit-field p4 8 16) 40))
         4)
        ((and (>= (bitwise-bit-field p1 0 8) 100)
              (< (bitwise-bit-field p1 8 16) 20)
              (< (bitwise-bit-field p1 16 24) 20))
         5)
        ((and (< (bitwise-bit-field p1 0 8) 100)
              (> (bitwise-bit-field p1 8 16) 100)
              (> (bitwise-bit-field p1 16 24) 100))
         6)
        ((and (>= (bitwise-bit-field p2 0 8) 150)
              (< (bitwise-bit-field p2 8 16) 100)
              (< (bitwise-bit-field p2 16 24) 100))
         7)
        ((and (>= (bitwise-bit-field p1 0 8) 160)
              (>= (bitwise-bit-field p1 8 16) 170)
              (>= (bitwise-bit-field p1 16 24) 180))
         0)
        (else
         (println x)
         (println y)
         (println (p->c p1))
         (println (p->c p2))
         (println (p->c p4))
         (println (p->c p8)))))

(define (read-p x y)
  (define target (make-bitmap 9 9))
  (define a-dc (new bitmap-dc% [bitmap target]))
  (for* ((x1 (in-range 9))
         (y1 (in-range 9)))
    (define color
      (apply make-object color%
             (p->c
              (GetPixel dc
                        (+ (* x grid) xbase
                           x1 -4
                           0)
                        (+ (* y grid) ybase
                           y1 -4
                           0)))))
    (send a-dc set-pen color 0 'solid)
    (send a-dc draw-point x1 y1))
  (make-object image-snip% target))

(define (open x y)
  (left window
        (+ (* x grid) xbase)
        (+ (* y grid) ybase)))
(define (mark x y)
  (right window
        (+ (* x grid) xbase)
        (+ (* y grid) ybase)))

(define height 16)
(define width 30)
(define remain 99)

(define-syntax-rule (board-ref x y)
  (vector-ref (vector-ref board y)
              x))
(define-syntax-rule (board-set! board x y v)
  (vector-set! (vector-ref board y)
               x
               v))

(define board
  ;0-8 for revealed cell with number
  ;#f for untouched cell
  ;#t for known mine
  (build-vector height
                (lambda (_)
                  (make-vector width #f))))

(define (safe-ref x y v)
  (if (and (<= 0 x)
           (< x width)
           (<= 0 y)
           (< y height)
           (eq? (board-ref x y) v))
      1
      0))

;count number of v in the neighbore of x y
(define (board-count x y v)
  (+ (safe-ref (sub1 x) (sub1 y) v)
     (safe-ref (sub1 x) y v)
     (safe-ref (sub1 x) (add1 y) v)
     (safe-ref x (sub1 y) v)
     (safe-ref x (add1 y) v)
     (safe-ref (add1 x) (sub1 y) v)
     (safe-ref (add1 x) y v)
     (safe-ref (add1 x) (add1 y) v)))

(define (collect-equations)
  (list->set
   (for*/fold ((equations '()))
              ((x (in-range width))
               (y (in-range height)))
     (if (and (number? (board-ref x y))
              (positive? (board-count x y #f)))
         (cons (generate-equation x y)
               equations)
         equations))))
;equation is (cons n points), where n is number of mines, points is list of (x . y)
(define (generate-equation x y)
  (cons (- (board-ref x y)
           (board-count x y #t))
        (collect x y)))
(define (safe-collect x y)
  (if (and (<= 0 x)
           (< x width)
           (<= 0 y)
           (< y height)
           (eq? (board-ref x y) #f))
      (cons x y)
      #f))
(define (collect x y)
  (filter identity
          (list (safe-collect (add1 x) y)
                (safe-collect (add1 x) (add1 y))
                (safe-collect x (add1 y))
                (safe-collect (sub1 x) (add1 y))
                (safe-collect (sub1 x) y)
                (safe-collect (sub1 x) (sub1 y))
                (safe-collect x (sub1 y))
                (safe-collect (add1 x) (sub1 y)))))

(define (full-equations)
  (set-add (collect-equations)
           (remain-equation)))
(define (remain-equation)
  (cons remain
        (for*/list ((x (in-range width))
                    (y (in-range height))
                    #:unless (board-ref x y))
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
(define (simple)
  (let lp ()
    (define solution (simple-solver (full-equations)))
    (unless (hash-empty? solution)
      (hash-for-each solution
                     (lambda (p v)
                       (if (zero? v)
                           (open-and-read (car p) (cdr p))
                           (sudo-right (car p) (cdr p)))))
      (lp))))

(define (open-and-read x y)
  (open x y)
  (recursive-read x y))
(define (recursive-read x y)
  (when (and (<= 0 x)
             (< x width)
             (<= 0 y)
             (< y height)
             (eq? (board-ref x y) #f))
    (define n (read x y))
    (board-set! board x y n)
    (when (zero? n)
      (recursive-read (add1 x) y)
      (recursive-read (add1 x) (add1 y))
      (recursive-read x (add1 y))
      (recursive-read (sub1 x) (add1 y))
      (recursive-read (sub1 x) y)
      (recursive-read (sub1 x) (sub1 y))
      (recursive-read x (sub1 y))
      (recursive-read (add1 x) (sub1 y)))))

(define (sudo-right x y)
  (unless (board-ref x y)
    (board-set! board x y #t)
    ;(mark x y)
    (set! remain (sub1 remain))))

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
             #:key (lambda (p) (+ (* (car p) height)
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
              (vector->list board))))

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

(define (safe)
  (let lp ()
    (simple)
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
              (open-and-read (caar s) (cdar s))
              (sudo-right (caar s) (cdar s))))
        (lp)))))

(define (auto)
  (let/ec k
    (let lp ()
      (safe)
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
      (open-and-read (car p) (cdr p))
      (lp))))

(define (make-full solution n)
  (define untouched
    (for*/list ((x (in-range width))
                (y (in-range height))
                #:unless (or (board-ref x y)
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
  (car b))

(sleep 1)
(open-and-read (random width) (random height))
(with-handlers ((exn? void))
  (auto))

(define ReleaseDC
  (get-ffi-obj 'ReleaseDC user32
               (_fun _pointer _pointer -> _bool)))
(ReleaseDC window dc)
