
( defun pow2 (x)
  ( * x x))

( defun s/ (x &rest list )
               ( apply #'/ ( cons x  ( mapcar #'( lambda (x) ( if ( eql x 0) 0.5 x)) list  )) ))

;( let* (( del #'/ ) 
 ;         (/ ( lambda (x &rest list )
  ;             ( apply del ( cons x  ( mapcar #'( lambda (x) ( if ( eql x 0) 0.00001 x)) list  )) ))))
   ;                 (apply / '(1 0 )) )

( defun rect-triangle ( base atitude)
  ( lambda ( c)
    ( if c base atitude)))

( defun rect-alpha ( rect-triangle )
  ( asin ( / ( rect-base rect-triangle )   ( rect-hypotenuse rect-triangle ))))

( defun rect-beta ( rect-triangle )
  ( - ( / pi 2 ) ( rect-alpha rect-triangle )))

( defun rect-base ( rect-triangle)
  ( funcall rect-triangle t ))

( defun rect-atitude ( rect-triangle)
  ( funcall rect-triangle nil ))

( defun rect-hypotenuse ( rect-triangle )
  ( sqrt ( + ( pow2 ( rect-atitude rect-triangle ) ) ( pow2 ( rect-base rect-triangle ) ))))

( defun rect-draw ( rect-triangle )
  ( im:chain  (im:rotated  ( im:scaled ( im:segment ) ( rect-base rect-triangle ) ) ( / pi -2 ) )
              ( im:scaled ( im:segment ) ( rect-atitude rect-triangle ) )
              ( im:rotated (im:scaled ( im:segment ) ( rect-hypotenuse rect-triangle ) ) ( - pi ( rect-alpha rect-triangle  )))))

( defun rect-draw-hypotenuse ( rect-triangle )
  ( im:rotated (im:scaled ( im:segment ) ( rect-hypotenuse rect-triangle ) ) ( +  ( rect-beta rect-triangle  )  (/ pi 2) ) ))

( defun function-graph ( fun presnost  rozpeti )
  ( labels (( draw-fun-segment ( fun presnost start end )
     ( if (  > start end )
         ( im:scaled ( im:segment ) 0 )
       ( let* ( ( rozdil ( - ( funcall  fun ( - start presnost ) ) ( funcall  fun start ) ))
                ( segment ( rect-triangle 1 ( abs  rozdil ) ) ))
         ( if ( > rozdil 0 )
             ( im:chain ( rect-draw-hypotenuse segment ) ( draw-fun-segment fun presnost ( + start presnost ) end ))
           ( im:chain ( im:vflipped ( rect-draw-hypotenuse segment ) ) ( draw-fun-segment fun presnost ( + start presnost ) end )))))))
    ( im:standardized ( im:hflipped ( draw-fun-segment fun presnost ( + ( * -1 rozpeti ) presnost )  rozpeti)))))
  
         
         
         




     
