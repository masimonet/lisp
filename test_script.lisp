;;; ==========================================================
;;; TEST FUNCIONAL: CON VERIFICACIÓN AUTOMÁTICA
;;; ==========================================================

;; HELPER
(define check (lambda (val expected)
  (if (eq val expected)
      (list val 'CORRECTO)
      (list val 'ERROR))))

(check (+ 10 20) 30)
(check (* 5 5) 25)

(define x 100)
(check x 100)

(define cuadrado (lambda (n) (* n n)))
(check (cuadrado 5) 25)

(define sumar-x (lambda (y) (+ x y)))
(check (sumar-x 50) 150)
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define paso-recursivo
  (lambda (rec)
    (lambda (n)
      (if (< n 2)
          1
          (* n (rec (- n 1)))))))

(define factorial (Z paso-recursivo))

(check (factorial 5) 120)


(check (car (cons 1 2)) 1)

(exit)