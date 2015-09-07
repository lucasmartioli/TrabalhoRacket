#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.8

;; Lista -> Natural
;; Conta quantos elementos da lista satisfazem o predicado
(define ex.6.8-tests
  (test-suite
   "Testes ex. 6.8"
   (check-equal? (cont positive? (list 1 -1 2 3 -2 5)) 4)
   (check-equal? (cont positive? (list -1 -2 )) 0)   
   (check-equal? (cont positive? empty) 0)
   (check-equal? (cont primo? (list 1 2 3 4 5 6)) 3)
   (check-equal? (cont primo? (list 1 4 6)) 0)))

(define (cont f lst)
  (define (iter f lst contador) 
  (cond
    [(empty? lst) contador]
    [(f (first lst) ) (iter f (rest lst) (add1 contador))]
    [else (iter f (rest lst) contador)]))
  (iter f lst 0))

(define (primo? x)
  (cond
    [(< x 2) #f]
    [(equal? x 2) #t]
    [(= (menor_divisor x 2) x) #t]
    [else #f]))

(define (menor_divisor x y)
  (cond
    [(> y x) (error "Aprende usar a função!")]
    [(divisivel? x y) y]
    [else (menor_divisor x (add1 y))]))

(define (divisivel? a b)
  (equal? (remainder a b) 0))


;;;;;;;;;;;;;;;;;;;;
;; Funções para auxiliar nos testes

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes ex.6.8-tests)
