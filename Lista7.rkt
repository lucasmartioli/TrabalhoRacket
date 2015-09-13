#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 para 3.2

;;Qualquer Lista -> Lista
;;remove o elemento E da lista de entrada e devolve a mesma lista sem o elemento E.
(define ex3.2acc-tests
  (test-suite
   "Testes Ex. 3.2acc"
   (check-equal? (remove-todos 3 empty) empty)
   (check-equal? (remove-todos 3 (list 5)) (list 5))
   (check-equal? (remove-todos 2 (list 2 5)) (list 5))
   (check-equal? (remove-todos 2 (list 6 6 7 2 5 2 7)) (list 6 6 7 5 7))))

(define (remove-todos e lst)
  (define (iter elem lista novalista)
    (cond
      [(empty? lista) novalista]
      [(equal? (first lista) elem) (iter elem (rest lista) novalista)]
      [else (iter elem (rest lista) (append novalista (list(first lista))))]))
  (iter e lst empty))    
    

;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 para 3.7

;; Lista Naturais -> Lista
;; Devolve uma nova lista sem os numeros pares.
(define ex3.7acc-tests
  (test-suite
   "Testes Ex. 3.7acc"
   (check-equal? (remove-pares empty) empty)
   (check-equal? (remove-pares (list 2)) empty)                 
   (check-equal? (remove-pares (list 1 2)) (list 1))                 
   (check-equal? (remove-pares (list 5 2 3 6 7 6)) (list 5 3 7))))                 

(define (remove-pares lst)
  (define (iter lista novalista)
    (cond
      [(empty? lista) novalista]
      [(not (odd? (first lista))) (iter (rest lista) novalista)]
      [else (iter (rest lista) (append novalista (list(first lista))))]))
  (iter lst empty))    


;;;;;;;;;;;;;;;;;;;;
;; Exemplo 4.5

;; Natural -> Natural
;; Conta a quantidade de primos no intervalo entre x e y.

(define ex4.5-tests
  (test-suite
   "Testes Ex. 4.5"
   (check-equal? (quantos_primos 0 10) 4)
   (check-equal? (quantos_primos 0 1) 0)
   (check-equal? (quantos_primos 0 2) 1)
   (check-equal? (quantos_primos 0 3) 2)
   (check-equal? (quantos_primos 0 100) 25)))

(define (quantos_primos x y)
  (define (iter x y quantidade)
    (cond
      [(> x y) quantidade]
      [(primo? x) (iter (add1 x) y (add1 quantidade))]
      [else (iter (add1 x) y quantidade)]))
  (iter x y 0))

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
(executa-testes ex3.2acc-tests
                ex3.7acc-tests
                ex4.5-tests)
