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
;; Exercício 6.10

;; Listas -> Lista
;; Concatena varias listas
(define ex.6.10-tests
  (test-suite
   "Testes ex. 6.10"
   (check-equal? (concatena (list 1 2 3) (list 4) (list 5 6)) (list 1 2 3 4 5 6))
   (check-equal? (concatena (list 1 2 3)) (list 1 2 3))
   (check-equal? (concatena empty) empty)
   (check-equal? (concatenaduaslistas empty empty) empty)
   (check-equal? (concatenaduaslistas (list 1 2 3) empty) (list 1 2 3))
   (check-equal? (concatenaduaslistas (list 1 2 3) (list 4)) (list 1 2 3 4))))

(define (concatena lst . outraslistas)
  (define (iter lista listas)
    (cond
      [(empty? listas) lista]
      [else (iter (concatenaduaslistas lista (first listas)) (rest listas))]))
  (iter lst outraslistas))

(define (concatenaduaslistas lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (cons (first lst1)
                (concatena (rest lst1) lst2))]))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.11

;; Listas -> Lista
;;

;;(define (mapeia f lst . outraslistas)
;;  (define (iter f lista listas)
;;    (cond
;;      [(empty? listas) lista]
;;      [else (iter f (mapeia2listas f lista (first listas)) (rest listas))]))
;;  (iter f lst outraslistas))

;;(define (mapeia2listas f lst lst2)
;;  (cond
;;    [(empty? lst) empty]
;;    [else (cons (f (first lst) (first lst2))
;;                (mapeia2listas f (rest lst) (rest lst2)))]))


;;;;;;;;;;;;;;;;;;;;
;; Exercício 6.12

;; Numeros -> Lista
;; Retorna uma lista dos elementos que tem a mesma paridade do primeiro item que foi apresentado

(define ex.6.12-tests
  (test-suite
   "Testes Ex. 6.12"
   (check-equal? (same-parity 7 20 53 45) (list 7 53 45))
   (check-equal? (same-parity 7) (list 7))
   (check-equal? (same-parity 2) (list 2))
   (check-equal? (same-parity 7 9 1 3 2) (list 7 9 1 3))
   (check-equal? (same-parity 8 3 5 8 12) (list 8 8 12))
   ))

(define (same-parity primeiro . elementos)
  (cond
    [(even? primeiro) (cons primeiro (filter even? elementos))]
    [else (cons primeiro (filter odd? elementos))]))


;;;;;;;;;;;;;;;;;;;;
;; Funções para auxiliar nos testes

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes ex.6.8-tests
                ex.6.10-tests
                ex.6.12-tests)
