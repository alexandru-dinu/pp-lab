; we want to see what kind of evaluation Racket uses
; normal or applicative

; infinite list
(define (loop x) (cons x (loop x)))

; constant function
(define (stop x) 1)

; test evaluation system
(define (ends x) (stop (loop x)))

; if we call ends (with any argument), Racket will start to evaluate loop (which will not end)
; thus meaning that Racket uses an applicative (eager) evaluation system

; Haskell, on the other hand, will return 1 (will call stop with argument (loop x),
; and since stop is a constant function, it will return 1, without evaluating loop
