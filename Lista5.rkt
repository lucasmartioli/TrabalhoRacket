#lang racket

(require rackunit)
(require rackunit/text-ui)

;;EXERCICIO 5.1
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

;;EXERCICIO 5.3
(define drop-tests
  (test-suite
   "drop tests"
   (check-equal? (drop (list 1 2 3) 0) (list 1 2 3))
   (check-equal? (drop (list 3 6 8 9) 2) (list 8 9))
   (check-equal? (drop (list 3 6 8 9) 4) empty)
   (check-equal? (drop (list 6 8 9) 4) empty)
   (check-equal? (drop empty 0) empty)
   (check-equal? (drop empty 2) empty)))

(define (drop list1 n)
  (cond 
    [(zero? n) list1]
    [(empty? list1) empty]
    [else (drop (rest list1) (sub1 n))]
    )
 )

;;EXERCICIO 5.4

;;take copiado para ser utilizado na resolucao do exercicio 5.4
(define take-tests
  (test-suite
   "take tests"
   (check-equal? (take empty 0) empty)
   (check-equal? (take (list 1 2 3) 0) empty)
   (check-exn exn:fail? (thunk (take empty 1)))
   (check-equal? (take (list 4 2 3) 1) (list 4))
   (check-equal? (take (list 4 2 5) 3) (list 4 2 5))
   (check-exn exn:fail? (thunk (take (list 4 2 5) 4)))))

(define (take lst n)
  (cond
    [(zero? n) empty]
    [(empty? lst) (error "Lista vazia")]
    [else (cons (first lst)
                (take (rest lst) (sub1 n)))]))

(define remove-at-tests
  (test-suite
   "remove-at tests"
   (check-equal? (remove-at (list 1 2 3) 0) (list 1 2 3))
   (check-equal? (remove-at (list 3 6 8 9) 2) (list 3 8 9))
   (check-equal? (remove-at (list 3 6 8 9) 1) (list 6 8 9))
   (check-equal? (remove-at (list 3 6 8 9) 4) (list 3 6 8))
   (check-equal? (remove-at (list 6 8 9) 3) (list 6 8))
   (check-equal? (remove-at empty 0) empty)
   (check-equal? (remove-at empty 2) empty)))

(define (remove-at list1 n)
  (cond 
    [(zero? n) list1]
    [(empty? list1) empty]
    [else (append (take list1 (sub1 n)) (drop list1 n)) ])
  )

;;EXERCICIO 5.5

(define insert-at-tests
  (test-suite
   "insert-at tests"
   (check-equal? (insert-at (list 1 2 3) 2 1) (list 2 1 2 3))
   (check-equal? (insert-at (list 3 6 8 9) 7 2) (list 3 7 6 8 9))
   (check-equal? (insert-at (list 3 6 8 9) 1 1) (list 1 3 6 8 9))
   (check-equal? (insert-at (list 3 6 8 9) 4 4) (list 3 6 8 4 9))
   (check-equal? (insert-at (list 6 8 9) 3 3) (list 6 8 3 9)) 
   ))

(define (insert-at list1 value pos)
  (append (append (take list1 (sub1 pos)) (list value)) (drop list1 (sub1 pos)) ) )

;; TESTES
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes <?-tests >?-tests <=?-tests >=?-tests =?-tests drop-tests take-tests remove-at-tests insert-at-tests)
