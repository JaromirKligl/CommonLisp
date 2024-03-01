; 1. Brezna
; 1000x hezci a lepsi 
; pridany komentare
( defmacro my-dotimes ( ( name times &optional result ) &body body )  
  ( let (( stack '() ))  ; compile-time promena nezpusobuje capture
    ( if ( and ( numberp times )
               ( < times 11 ))
               ;And zmenen poradi actually fixne problem se slozenimy vyrazy 
               ; pokud by se prvni testovalo times < 11 tak by to spadlo pri vyrazu ( print 5 )
        ( progn
          ( push result stack)
          ( dotimes ( loop-var times)
            ( push `( incf ,name ) stack )
            ( setf stack ( append body stack ) ))
          ( append `( let  (( ,name 0 )) ) stack )) ; Append je lepsi reseni nez dolist push xd how obv
      `( dotimes ( ,name ,times ,result ) ,@body ))))




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
