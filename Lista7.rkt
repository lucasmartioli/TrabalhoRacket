#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 7.1 para 3.2

;;Qualquer Lista -> Lista
;;remove o elemento E da lista de entrada e devolve a mesma lista sem o elemento E.
(define ex3.2acc-tests
  (test-suite
   "Testes Ex. 3.2acc 7.1"
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
   "Testes Ex. 3.7acc 7.1"
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
;; Exercício 7.1 para 3.9

;; Lista -> Numero
;; Encontra o maior valor da lista

(define ex3.9acc-tests
  (test-suite
   "Testes Ex. 3.9acc 7.1"
   (check-exn exn:fail? (thunk (maximo empty)))
   (check-equal? (maximo (list 35 72 44 80 31)) 80)
   (check-equal? (maximo (list 6)) 6)
   (check-equal? (maximo (list 1 3)) 3)
   (check-equal? (maximo (list 7 3 2 4)) 7)
   (check-equal? (maximo (list 28 4 78 3)) 78)))

(define (maximo list)
  (define (iter lista valormaximo)
    (cond
      [(empty? lista) valormaximo]
      [else (iter (rest lista) (primeiroMaior? valormaximo (first lista)))]))

  (cond
    [(empty? list) (error "Lista vazia")]
    [else (iter list (first list))]))

(define (primeiroMaior? a b)
  (if (>= a b) a b))


;;;;;;;;;;;;;;;;;;;;
;; Exercício 4.1

;; Natural -> Natural
;; Devolve o fatorial do parametro n.
(define ex4.1acc-tests
  (test-suite
   "Testes Ex. 4.1 para 7.2"
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
  (define (iter i acumulador)
    (cond
      [(<= i 1) acumulador]
      [else (iter (sub1 i) (* acumulador i))]))
  (iter n 1))

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
                ex3.9acc-tests
                ex4.1acc-tests)
