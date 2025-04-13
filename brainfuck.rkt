#lang racket

;Interpreter for the Brainfuck language

; Data area
(define memory-size 30000)
(define data (make-vector memory-size 0))

(define (memory-forward-overflow x)
  (if (= x (- memory-size 1))
      0
      (+ x 1)))
(define (memory-back-underflow x)
  (if (= x 0)
      (- memory-size 1)
      (- x 1)))

(define (inc-overflow x)
  (if (= x 255)
      0
      (+ x 1)))
(define (dec-underflow x)
  (if (= x 0)
      255
      (- x 1)))

(define (eval code data data-pointer loop-start)
  (cond [(null? code) null]
        [(equal? (car code) #\>) (eval (cdr code)
                                       data
                                       (memory-forward-overflow data-pointer)
                                       loop-start)]
        [(equal? (car code) #\<) (eval (cdr code)
                                       data
                                       (memory-back-underflow data-pointer)
                                       loop-start)]
        [(equal? (car code) #\+) (eval (cdr code)
                                       (vector-set/copy data data-pointer (inc-overflow (vector-ref data data-pointer)))
                                       data-pointer loop-start)]
        [(equal? (car code) #\-) (eval (cdr code)
                                       (vector-set/copy data data-pointer (dec-underflow (vector-ref data data-pointer)))
                                       data-pointer loop-start)]
        [(equal? (car code) #\.) (begin (writeln (~a (integer->char (vector-ref data data-pointer))))
                                        (eval (cdr code) data data-pointer loop-start))]
        [(equal? (car code) #\,) (eval (cdr code) (vector-set/copy data data-pointer (read))
                                       data-pointer)]
        [(equal? (car code) #\[) (eval (cdr code) data data-pointer (cdr code))]
        [(equal? (car code) #\]) (if (= (vector-ref data data-pointer) 0)
                                       (eval (cdr code) data data-pointer loop-start)
                                       (eval loop-start data data-pointer loop-start))]             
        [#t null]))

        