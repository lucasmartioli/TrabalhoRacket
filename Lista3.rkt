#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.1

;;Qualquer Lista -> Lógico
;;Verdadeiro se um determinado elemento está em uma lista. Falso caso contrário
(define ex3.1-tests
  (test-suite
   "Testes Ex. 3.1"
   (check-equal? (esta-na-lista? 3 empty) #f)
   (check-equal? (esta-na-lista? 3 (list 5)) #f)
   (check-equal? (esta-na-lista? 2 (list 2 5)) #t)))

(define (esta-na-lista? e lst)
  (cond 
    [(empty? lst) #f]
    [else (if (= (first lst) e) #t (esta-na-lista? e (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.2

;;Qualquer Lista -> Lista
;;Verdadeiro se um determinado elemento está em uma lista. Falso caso contrário
(define ex3.2-tests
  (test-suite
   "Testes Ex. 3.2"
   (check-equal? (remove-todos 3 empty) empty)
   (check-equal? (remove-todos 3 (list 5)) (list 5))
   (check-equal? (remove-todos 2 (list 2 5)) (list 5))
   (check-equal? (remove-todos 2 (list 6 6 7 2 5 2 7)) (list 6 6 7 5 7))))

(define (remove-todos e lst)
  (cond 
    [(empty? lst) empty]
    [(equal? (first lst) e) (remove-todos e (rest lst))]
    [else (cons (first lst) (remove-todos e (rest lst)))]))


;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.7

;; Lista Naturais -> Lista
;; Devolve uma nova lista sem os numeros pares.
(define ex3.7-tests
  (test-suite
   "Testes Ex. 3.7"
   (check-equal? (remove-pares empty) empty)
   (check-equal? (remove-pares (list 2)) empty)                 
   (check-equal? (remove-pares (list 1 2)) (list 1))                 
   (check-equal? (remove-pares (list 5 2 3 6 7 6)) (list 5 3 7))))                 

(define (remove-pares lst)
  (filter odd? lst))

;; Integer -> Lógic
;; Verdadeiro se o número é impar.
(define (odd? e)
  (if (equal? (modulo e 2) 0) #f #t))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.8

;; Lista -> Número
;; Devolve o ultimo elemento de uma lst. Se a lista for vazia, gera um erro.

(define ex3.8-tests
  (test-suite
   "Testes Ex. 3.8"
   (check-exn exn:fail? (thunk (ultimo empty)))
   (check-equal? (ultimo (list 4)) 4)
   (check-equal? (ultimo (list 2 4 8 3)) 3)
   (check-equal? (ultimo (list 8 7 8 7)) 7)))

(define (ultimo lst)
  (cond
    [(empty? lst) (error "Lista vazia")]
    [(empty? (rest lst)) (first lst)]
    [(ultimo (rest lst))]))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.12

;; Lista -> Lista
;; Devolve uma nova lista que com apenas uma ocorrencia dos elementos repetidos consecutivos.
(define ex3.12-tests
  (test-suite
   "Testes Ex. 3.12"
   (check-equal? (remove-duplicates empty) empty)
   (check-equal? (remove-duplicates (list 1 2 3 3 3 4 5 5 6 6 6 6 6 7)) (list 1 2 3 4 5 6 7))
   (check-equal? (remove-duplicates (list 6 7 8)) (list 6 7 8))))
   

(define (remove-duplicates lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (cons (first lst) (remove-duplicates (rest lst)))]    
    [else (if (equal? (first lst) (first (rest lst)))
              (remove-duplicates (rest lst))
              (cons (first lst) (remove-duplicates (rest lst))))]))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.13

;; Lista -> Lista
;; Devolve uma nova lista aninhada como os mesmo elementos de lst mas em ordem reversa
(define ex3.13-tests
  (test-suite
   "Testes Ex. 3.13"
   (check-equal? (reverse* empty) empty)
   (check-equal? (reverse* (list (list 2 3) 8 (list 9 (list 10 11) 50) (list 10) 70)) (list 70 (list 10) (list 50 (list 11 10) 9) 8 (list 3 2)))
   (check-equal? (reverse* (list 6 7 8)) (list 8 7 6))))

(define (reverse* lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst)) (append (append (reverse* (rest lst)) (list (reverse* (first lst)))))]
    [else (append (reverse* (rest lst)) (list (first lst)))]))


;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.15

;; Arvore -> Arvore
;; devolva uma nova arvore binaria que e como t mas com n somado a cada elemento.
(define ex3.15-tests
  (test-suite
   "Testes Ex. 3.15"
   (check-equal? (soma-arvore empty 7) empty)
   (check-equal? (soma-arvore t0 7) (arvore-bin 17 '() '()))
   (check-equal? (soma-arvore t1 2) (arvore-bin 11 (arvore-bin 12 '() '()) '()))
   (check-equal? (soma-arvore t2 9) (arvore-bin 16 (arvore-bin 17 '() '()) (arvore-bin 18 (arvore-bin 19 '() '()) '())))
   (check-equal? (soma-arvore t4 1) (arvore-bin 4
                                                (arvore-bin 8 (arvore-bin 9 '() '()) (arvore-bin 10 (arvore-bin 11 '() '()) '()))
                                                (arvore-bin 8 (arvore-bin 9 '() '()) (arvore-bin 10 (arvore-bin 11 '() '()) '()))))))

(struct arvore-bin (v esq dir) #:transparent)

(define t0 (arvore-bin 10 empty empty))

(define t1 (arvore-bin 9
                       t0
                       empty))

(define t2 (arvore-bin 7
                       (arvore-bin 8 empty empty)
                       t1))

(define t3 (arvore-bin 4
                       (arvore-bin 3 empty empty)
                       empty))

(define t4 (arvore-bin 3
                       t2
                       t2))

(define tissoehbinario (arvore-bin 5
                                   t3
                                   (arvore-bin 8 empty (arvore-bin 8 empty t0))))

(define tissoehbinario2 (arvore-bin 5
                                   t3
                                   (arvore-bin 8 (arvore-bin 6 empty empty) (arvore-bin 8 empty t0))))

(define (soma-arvore t n)
(cond
  [(empty? t) empty]
  [else (arvore-bin (+ (arvore-bin-v t) n)
                  (soma-arvore (arvore-bin-esq t) n)
                  (soma-arvore (arvore-bin-dir t) n))]))



;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.16

;; Arvore -> Lógico
;; verifique se uma arvore binaria e uma arvore binaria de busca

(define ex3.16-tests
  (test-suite
   "Testes Ex. 3.16"
   (check-equal? (arvore-binaria-busca? tissoehbinario) #t)
   (check-equal? (arvore-binaria-busca? tissoehbinario2) #t)
   (check-equal? (arvore-binaria-busca? empty) #t)
   (check-equal? (arvore-binaria-busca? t0) #t)
   (check-equal? (arvore-binaria-busca? t3) #t)
   (check-equal? (arvore-binaria-busca? t2) #f)))


(define (arvore-binaria-busca? t)  
  (define (iter t f)
    (cond
      [(empty? t) #t]      
      [(f (arvore-bin-v t))
       (and (iter (arvore-bin-esq t) (λ (x) (<= x (arvore-bin-v t))))
            (iter (arvore-bin-dir t) (λ (x) (>= x (arvore-bin-v t)))))]
      [else #f])) 
  (iter t (λ (x) (= x (arvore-bin-v t)))))

;;;;;;;;;;;;;;;;;;;;
;; Exercício 3.17

;; Arvore Binaria -> Lógico
;; verifique se um elemento esta em uma arvore binaria de busca.


(define ex3.17-tests
  (test-suite
   "Testes Ex. 3.17"
   (check-equal? (esta-na-arvore? tissoehbinario 8) #t)
   (check-equal? (esta-na-arvore? tissoehbinario2 6) #t)
   (check-equal? (esta-na-arvore? tissoehbinario2 16) #f)
   (check-equal? (esta-na-arvore? tissoehbinario2 1) #f)
   (check-equal? (esta-na-arvore? empty 0) #f)
   (check-equal? (esta-na-arvore? t0 10) #t)
   (check-equal? (esta-na-arvore? t0 9) #f)
   (check-equal? (esta-na-arvore? t3 3) #t)
   (check-equal? (esta-na-arvore? t3 4) #t)
   (check-equal? (esta-na-arvore? t3 5) #f)))

(define (esta-na-arvore? t n)
  (cond
    [(empty? t) #f]
    [(equal? (arvore-bin-v t) n)]
    [else (if (< n (arvore-bin-v t) )
              (esta-na-arvore? (arvore-bin-esq t) n)
              (esta-na-arvore? (arvore-bin-dir t) n))]))

;;;;;;;;;;;;;;;;;;;;
;; Funções para auxiliar nos testes

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes ex3.1-tests
                ex3.2-tests
                ex3.7-tests
                ex3.8-tests
                ex3.12-tests
                ex3.13-tests
                ex3.15-tests
                ex3.16-tests
                ex3.17-tests)
