#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.1

;; Natural -> Natural
;; Devolve o fatorial do parametro n.
(define fatorial-tests
  (test-suite
   "fatorial tests"
   (check-equal? (fatorial 0) 1)
   (check-equal? (fatorial 1) 1)
   (check-equal? (fatorial 2) 2)
   (check-equal? (fatorial 3) 6)
   (check-equal? (fatorial 4) 24)
   (check-equal? (fatorial 5) 120)
   (check-equal? (fatorial 6) 720)
   (check-equal? (fatorial 7) 5040)
   (check-equal? (fatorial 10) 3628800)))

(define (fatorial n)
  (cond
    [( = n 0 ) 1]
    [else ( * n (fatorial (sub1 n ) ))]))

;;;;;;;;;;;;;;;;;;;;
;; Exemplo 4.1

;; Natural -> Natural
;; Cada funcao deve receber como parametro dois numeros naturais e executar a operac˜ao aritmetica apropriada. (- + * )
(define operacoes-tests
  (test-suite
   "operacoes tests"
   (check-equal? (soma 3 0) 3)
   (check-equal? (soma 7 1) 8)
   (check-equal? (soma 18 2) 20)
   (check-equal? (multiplica 0 25) 0)
   (check-equal? (multiplica 1 25) 25)
   (check-equal? (multiplica 25 1) 25)
   (check-equal? (multiplica 5 5) 25)
   (check-equal? (multiplica 3 10) 30)
   (check-equal? (subtrai 3 10) -7)
   (check-equal? (subtrai 20 15) 5)
   (check-equal? (subtrai 0 0) 0)
   (check-equal? (subtrai 1 1) 0)
   (check-equal? (subtrai 3 2) 1)
   (check-equal? (subtrai 1 0) 1))) 

(define (soma a b)
   (if (zero? b)
       a
       (add1 (soma a (sub1 b)))))

(define (subtrai a b)
   (if (zero? b)
       a
       (sub1 (subtrai a (sub1 b)))))

(define (multiplica a b)
  (if (= 1 b)
      a
      (soma a (multiplica a (sub1 b)))))


;;;;;;;;;;;;;;;;;;;;
;; Funções para auxiliar nos testes

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes fatorial-tests
                operacoes-tests)
