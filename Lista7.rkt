#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.2

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
;; Exemplo 4.2

;; Natural -> Natural
;; Cada funcao deve receber como parametro dois numeros naturais e executar a operac˜ao aritmetica apropriada. (- + * )
(define ex4.2-tests
  (test-suite
   "Testes Ex. 4.2"
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
                ex4.2-tests
                ex4.5-tests)
