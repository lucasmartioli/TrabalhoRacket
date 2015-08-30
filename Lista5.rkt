#lang racket

(require rackunit)
(require rackunit/text-ui)

(define >?-tests
  (test-suite
   ">? tests"
   (check-equal? (>? 0 0) #f)
   (check-equal? (>? 0 2) #f)
   (check-equal? (>? 2 0) #t)
   (check-equal? (>? 2 3) #f)
   (check-equal? (>? 4 3) #t)))

(define (>? a b)
  (cond
    [(zero? a) #f]
    [(zero? b) #t]
    [else (>? (sub1 a) (sub1 b))]))


(define <?-tests
  (test-suite
   "<? tests"
   (check-equal? (<? 0 0) #f)
   (check-equal? (<? 0 2) #t)
   (check-equal? (<? 2 0) #f)
   (check-equal? (<? 2 3) #t)
   (check-equal? (<? 4 3) #f)))

(define (<? a b)
  (cond
    [(zero? b) #f]    
    [(zero? a) #t]
    [else (<? (sub1 a) (sub1 b))]))

;;>=
(define <=?-tests
  (test-suite
   "<=? tests"
   (check-equal? (<=? 0 0) #t)
   (check-equal? (<=? 0 2) #t)
   (check-equal? (<=? 2 0) #f)
   (check-equal? (<=? 2 3) #t)
   (check-equal? (<=? 4 3) #f)))

(define (<=? a b)
  (cond 
    [(>? a b) #f]
    [else #t]) 
 )

(define >=?-tests
  (test-suite
   ">=? tests"
   (check-equal? (>=? 0 0) #t)
   (check-equal? (>=? 0 2) #f)
   (check-equal? (>=? 2 0) #t)
   (check-equal? (>=? 2 3) #f)
   (check-equal? (>=? 4 3) #t)))

(define (>=? a b)
  (cond 
    [(<? a b) #f]
    [else #t]) 
 )

(define =?-tests
  (test-suite
   "= tests"
   (check-equal? (=? 0 0) #t)
   (check-equal? (=? 0 2) #f)
   (check-equal? (=? 2 0) #f)
   (check-equal? (=? 2 3) #f)
   (check-equal? (=? 3 3) #t)
   (check-equal? (=? 4 3) #f)))

(define (=? a b)
  (cond 
    [(<? a b) #f]
    [(>? a b) #f]
    [else #t]) 
 )


(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes <?-tests >?-tests <=?-tests >=?-tests =?-tests)
