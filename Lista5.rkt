#lang racket

(require rackunit)
(require rackunit/text-ui)

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



(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes <?-tests)
