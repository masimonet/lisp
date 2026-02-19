(defstruct closure params body env)



(defmethod print-object ((c closure) stream)
  (format stream "#<CLOSURE params:~A>" (closure-params c)))



(defun lookup (var env)
  (if (null env)
      (error "Unbound variable: ~A" var)
      (let ((pair (assoc var (first env))))
        (if pair
            (cdr pair)
            (lookup var (rest env))))))

(defun extend-env (vars vals env)
  (cons (pairlis vars vals) env))

(defun define-in-env (var val env)
  (cons (acons var val (first env)) 
        (rest env)))



(defun evaluate (x env)
  (cond
    ((null x) (cons x env))
    ((eq x t) (cons x env))
    ((atom x) 
     (if (symbolp x)
         (cons (lookup x env) env)
         (cons x env)))

    ((eq (first x) 'quote)
     (cons (second x) env))

    ((eq (first x) 'if)
     (let* ((res-cond (evaluate (second x) env))
            (val-cond (car res-cond))
            (env-cond (cdr res-cond)))
       (if val-cond
           (evaluate (third x) env-cond)
           (evaluate (fourth x) env-cond))))

    ((eq (first x) 'lambda)
     (cons (make-closure :params (second x) 
                         :body (third x) 
                         :env env)
           env))

    ((eq (first x) 'define)
     (let* ((res-val (evaluate (third x) env))
            (val     (car res-val))
            (env-new (cdr res-val)))
       (cons (second x) 
             (define-in-env (second x) val env-new))))

    (t
     (evaluate-application (first x) (rest x) env))))



(defun evaluate-application (func-exp arg-exps env)
  (let* ((res-func (evaluate func-exp env))
         (func     (car res-func))
         (env-1    (cdr res-func)))
    
    (let ((args-result (evaluate-list arg-exps env-1)))
      (let ((args (car args-result))
            (env-final (cdr args-result)))
        
        (apply-func func args env-final)))))



(defun evaluate-list (exps env)
  (if (null exps)
      (cons '() env)
      (let* ((res-first (evaluate (car exps) env))
             (val-first (car res-first))
             (env-next  (cdr res-first))
             (res-rest  (evaluate-list (cdr exps) env-next)))
        (cons (cons val-first (car res-rest))
              (cdr res-rest)))))

(defun apply-func (fn args caller-env)
  (cond
    ((functionp fn) 
     (cons (apply fn args) caller-env))
    
    ((closure-p fn)
     (let* ((lexical-env (extend-env (closure-params fn) args (closure-env fn)))
            (res (evaluate (closure-body fn) lexical-env)))
       (cons (car res) caller-env)))
    
    (t (error "Not a function: ~A" fn))))



(defun make-initial-env ()
  (let ((env (list (list)))) 
    (dolist (pair `((+ . ,#'+) (- . ,#'-) (* . ,#'*) (/ . ,#'/)
                    (> . ,#'>) (< . ,#'<) (eq . ,#'eq)
                    (cons . ,#'cons) (car . ,#'car) (cdr . ,#'cdr)
                    (list . ,#'list) (null . ,#'null) (print . ,#'print)))
          (push pair (first env)))
    env))



(defun repl (current-env)
  (format t "~%Func-Lisp >> ")
  (force-output)
  
  (let ((input (handler-case (read)
                 (end-of-file () '(exit)))))
                 
    (if (equal input '(exit))
        (format t "Goodbye.~%")
        (let* ((result-pair (handler-case (evaluate input current-env)
                              (error (e) (format t "Error: ~A" e) nil)))
               (value       (if result-pair (car result-pair) nil))
               (next-env    (if result-pair (cdr result-pair) current-env)))
          
          (when result-pair
            (format t "=> ~A~%" value)) 
          
          (repl next-env)))))

(repl (make-initial-env))