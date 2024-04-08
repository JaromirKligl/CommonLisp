

(defun setupcond(args body)
  (if (cdr args)  
      (append (setupcond (cdr args) body)
           (list (list (third (car args)) (append (list 'lambda-c (mapcar #'first (cdr args)))  body ))))
    (list`( ,(third (car args)) ,@body  )   )))
 

(defmacro lambda-c ((first &rest carried) &body body)
  (if carried
      (let* ((args (mapcar (lambda (x)
                            (list x nil (gensym))) carried))
             (body-cond (setupcond args body) ))

        `(lambda ( ,first  &optional ,@args )
          (cond ,@body-cond
                ( t ,(append (list 'lambda-c carried) body)   ))))
            
         
      `(lambda (,first)
         ,@body)))

(print (macroexpand-1 '(lambda-c (x y z) ( + x y z))))
  