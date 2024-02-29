;
; 29. Unora , tahle verze je hnusna ale funguje , je to 1. verze tady ale mozna i posledni , kdo vy estli to udelam hezcejc :)
;
( defmacro my-dotimes ( ( name times &optional result ) &body body )
  ( if ( and ( numberp times ) 
             ( < times 11 ))

      ( let ((intr-1  (gensym) )
             ( intr-2 (gensym) )
             ( vyraz result))
       ( progn
         `( let (( ,name 0 ))
            ,@( dotimes ( intr-1 ( - times 1 ) vyraz)
                ( push  `( incf ,name ) vyraz  )
                ( dolist ( intr-2 body )
                  ( push intr-2 vyraz ))) 

            ,@body
            ,result)))
         
 `( dotimes ( ,name ,times ,result ) ,@body)))
