
(defun aplication-head (aplication-expr)
  (car aplication-expr))

(defun aplication-args (aplication-expr)
  (cdr aplication-expr))

(defun variable-p (expr)
  (unless (find expr '(lambda λ))
    (symbolp expr)))

(defun external-p (expr)
  (when (consp expr)
    (functionp (car expr) )))

(defun aplication-p (expr)
  (when (consp expr)
    (and (not (functionp (car expr) ))
         (not (find (car expr) '(lambda λ))))))

(defun abstraction-p (expr)
  (when (consp expr)
    (and (not (functionp (car expr) ))
         (find (car expr) '(lambda λ)))))

(defun abstraction-args (expr)
  (second expr))

(defun abstraction-body (expr)
  (third expr))
      
(defun alpha-conversion (aplication-expr)
  (cons 
   (alpha-head (aplication-head aplication-expr)
               (alpha-args 
                (aplication-args aplication-expr)
                (second (aplication-head aplication-expr) )))
   (aplication-args aplication-expr)))

(defun alpha-args (args symb)
  (alpha-gensyms (intersection symb 
                               (remove 0 (remove-duplicates (alpha-vlist args))))))

(defun alpha-gensyms (list)
  (and list
       (cons (cons (car list)
                    (gensym)) 
              (alpha-gensyms (cdr list)))))

(defun alpha-vlist (args)
  ( and args
        (cond ((variable-p args) (list args))
              ((consp args) (append (alpha-vlist (car args))
                                     (alpha-vlist (cdr args))))
              (t (list 0)))))
  

(defun alpha-variable (var vars)
  (or (cdr (find var vars :key #'car))
      var))

(defun alpha-head(head vars)
  (mapcar (lambda (x)
            (cond ( (variable-p x)
                (alpha-variable x vars))
              ( (consp x) (alpha-head x vars))
              (t x)))
          head))

(defun substitute-expr (expr old new)
  (if (consp expr)
      (mapcar (lambda (x)
                    (substitute-expr x old new))
                  expr)
    (if (eql expr old) new expr)))

(defun beta-reduction(aplication-expr)
  ( let (( l-evaled (lvl (aplication-head aplication-expr))))
    (if (abstraction-p l-evaled)
        (beta-abstraction (aplication-head (alpha-conversion (cons l-evaled 
                                                                   (aplication-args aplication-expr))))
                          (aplication-args aplication-expr))

          aplication-expr)))

(defun beta-abstraction(abstraction args)
  (lvl  (if args
      (if (cdr (abstraction-args abstraction))
           (append
            (list
             (list 'λ (cdr (abstraction-args abstraction))
                   (substitute-expr (abstraction-body abstraction)
                                    (car (abstraction-args abstraction))
                                    (car args))))
           (cdr args))
        (let ((substituted (substitute-expr (abstraction-body abstraction)
                                  (car (abstraction-args abstraction))
                                  (car args))))
          (if (cdr args)
              (cons (lvl substituted) (cdr args))
            (lvl substituted))))

                           abstraction)))

(defvar *base* '())

(defun l-define (new old)
  (push (cons new (if (consp old)
                      (handle-defs old *base*)
                    old)) *base*))

(defun handle-defs (expr defs)
  (if (consp expr)
    (mapcar (lambda (x)
              (if (consp x)
                  (handle-defs x defs)
                (or (cdr (find x defs :key #'car ))
                    x))) expr)
    (or (cdr (find expr defs :key #'car ))
                    expr)))
                       
( setf *base* '())

(defun lvl (expr)
  (print expr)
  (cond ((atom expr) expr)
        ((abstraction-p expr) expr)
        ((aplication-p expr) (beta-reduction expr))
        ((external-p expr)  (handle-defs(external-apply expr) *base*))
        (t expr)))

(defun external-apply (expr)
  (apply (car expr) (mapcar #'lvl (cdr expr))))


(defun l-eval (expr)
  (if *base*
      (lvl (handle-defs expr *base*))
    (lvl expr)))

(l-define 'nil '(λ (a b) b))
(l-define 't '(λ (a b) a))
(l-define 'if '(λ (p x z) (p x z)))
(l-define 'and '(λ (x y) (if x y nil)))
(l-define '+ `(λ (a b) (,#'+ a b) )) 
(l-define '= `(λ (a b) (,#'= a b) ))
(l-define '* `(λ (a b) (,#'* a b) ))
(l-define '- `(λ (a b) (,#'- a b) ))
(l-define '%fact0 `(λ (u n)
                  (if (= n 0)
                      1
                    (* n (u u (- n 1))))) )

(l-define '%fact `(λ (fun n)
                  (if (= n 0)
                      1
                    (* n (fun (- n 1))))) )

(l-define '@Y '(λ (f) ((λ (x2) (f (x2 x2)))
                  (λ (x2) (f (x2 x2))))))

(l-define 'fact '(@Y %fact))

(l-define 'fact0 `( %fact0 %fact0))

(l-define '%fib '(λ ( fun n )
                       (if ( = n 0)
                           0
                         (if (= n 1)
                             1
                           ( + (fun (- n 1)) (fun (- n 2)))))))

(l-define 'fib '(@Y %fib))








                
  
  
             
  