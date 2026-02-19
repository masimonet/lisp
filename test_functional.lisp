;;; ==========================================================
;;; TEST SUITE FOR SELF-MODIFYING INTERPRETER (CLEAN STYLE)
;;; ==========================================================

;; 1. BASIC ARITHMETIC
(+ 10 20)
(* 5 5)
(- 100 1)

(define square (lambda (x) (* x x)))
(square 10)

(define make-counter (lambda () 
  ((lambda (count) 
     (lambda () 
       (set! count (+ count 1)) 
       count)) 
   0)))

(define c1 (make-counter))
(c1)
(c1)

(define my-list (list 1 2 3))
(car my-list)
(cdr my-list)

(define data (list 1 2 3))
(set-car! data 99)
data

(define code '(+ 5 5))

(eval code)

(set-car! code '*)

code


(eval code)

(exit)