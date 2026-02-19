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



(defun define-variable (var val env)
  (let ((frame (first env)))
    (let ((pair (assoc var frame)))
      (if pair
          (setf (cdr pair) val)
          (push (cons var val) (first env)))))
  var)



(defun set-variable! (var val env)
  (if (null env)
      (error "Cannot set! unbound variable: ~A" var)
      (let ((pair (assoc var (first env))))
        (if pair
            (setf (cdr pair) val)
            (set-variable! var val (rest env))))))



(defun evaluate (x env)
  (cond
    ((null x) x)
    ((eq x t) x)
    ((atom x) 
     (if (symbolp x)
         (lookup x env)
         x))

    ((eq (first x) 'quote) 
     (second x))
    
    ((eq (first x) 'if)
     (if (evaluate (second x) env)
         (evaluate (third x) env)
         (evaluate (fourth x) env)))
    
    ((eq (first x) 'lambda)
     (make-closure :params (second x) 
                   :body (third x) 
                   :env env))
    
    ((eq (first x) 'define)
     (define-variable (second x) 
                      (evaluate (third x) env) 
                      env))

    
    ((eq (first x) 'set!)
     (set-variable! (second x) 
                    (evaluate (third x) env) 
                    env))
    
    (t
     (let ((func (evaluate (first x) env))
           (args (mapcar #'(lambda (arg) (evaluate arg env)) (rest x))))
       (apply-func func args)))))


(defun apply-func (fn args)
  (cond
    ((functionp fn) (apply fn args))
    ((closure-p fn)
     (evaluate (closure-body fn)
               (extend-env (closure-params fn)
                           args
                           (closure-env fn))))
    (t (error "Attempt to apply non-function: ~A" fn))))


(defparameter *global-env* (list (list))) 

(defun install-primitive (sym func)
  (push (cons sym func) (first *global-env*)))


(install-primitive '+ #'+)
(install-primitive '- #'-)
(install-primitive '* #'*)
(install-primitive '/ #'/)
(install-primitive '> #'>)
(install-primitive '< #'<)
(install-primitive 'eq #'eq)

(install-primitive 'cons #'cons)
(install-primitive 'car #'car)
(install-primitive 'cdr #'cdr)
(install-primitive 'list #'list)
(install-primitive 'null #'null)

(install-primitive 'set-car! (lambda (x y) (setf (car x) y) x))
(install-primitive 'set-cdr! (lambda (x y) (setf (cdr x) y) x))

(install-primitive 'print #'print)
(install-primitive 'read #'read)
(install-primitive 'eval (lambda (x) (evaluate x *global-env*)))



(defun repl ()
  (format t "~%Mut-Lisp >> ")
  (force-output)
  
  (loop
    (let ((input (handler-case (read)
                   (end-of-file () '(exit)))))
      
      (if (equal input '(exit)) 
          (return))
    

      (handler-case
          (let ((result (evaluate input *global-env*)))
            (format t "=> ~A~%" result) 
            (force-output))
        (error (e) 
          (format t "Error: ~A~%" e)
          (force-output)))

      (format t "~%Mut-Lisp >> ")
      (force-output))))

(repl)