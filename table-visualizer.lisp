;
; VYROBIL
;
;
; JAROMIR
;
; KLIGL
;
; AMOGUS     

( defun table-nth ( tbl row column )
  ( funcall tbl row column ))

( defun table-to-list ( tbl rows columns )
  ( labels ( 
      ( ttol ( tbl rows columns column )
                 ( and ( not ( eql column columns ))
                   ( cons ( row-t-list tbl rows 0 column ) 
                           ( ttol tbl rows columns ( + column 1 )))))
    ( row-t-list ( tbl rows row column )
                 ( and ( not ( eql row rows ))
                             ( cons ( table-nth tbl row column )
                                    ( row-t-list tbl rows ( + row 1 ) column )))))
    ( ttol tbl rows columns 0)))

( defun from-table-nth ( tbl rowofs columnofs )
  ( lambda ( row column )
          ( funcall tbl ( + row rowofs ) ( + column columnofs ))))

( defun display-one ()
  ( im:standardized ( im:segment )))

( defun display-two ()
  ( im:standardized ( im:chain ( im:rotated ( im:segment )  ( / pi 2) ) ( im:segment)
                               ( im:rotated ( im:segment )   ( / pi -2) ) 
                               ( im:segment ) 
                               ( im:rotated ( im:segment )   ( / pi 2) ))))

( defun display-three ()
  ( im:standardized
    ( im:column 
      ( im:chain 
        ( im:rotated ( im:segment )   ( / pi -2) ) 
        ( im:segment ) )
      ( im:chain 
        ( im:rotated ( im:segment )   ( / pi -2) ) 
        ( im:segment ) 
        ( im:rotated ( im:segment )   ( / pi 2) )))))

( defun display-four () 
  ( im:standardized ( im:hflipped ( im:chain
                      ( im:rotated ( im:segment ) pi )
                      ( im:rotated ( im:segment ) ( / pi 2  ))
                      ( im:rotated ( im:segment ) pi )))))
                      
( defun display-five () 
  ( im:vflipped ( display-two )))
( defun display-six ()
  ( im:vflipped ( im:hflipped ( display-nine ) ) ))

( defun display-nine () 
  ( im:split ( im:scaled ( im:segment ) 1/2 ) ( display-five )))

( defun display-seven ()
  ( im:chain ( im:rotated ( im:scaled ( im:segment ) 1/2 ) ( / pi -2 ))
             ( im:rotated ( im:segment ) pi )))

( defun display-eighth ()
  ( im:split ( display-six )
             ( im:scaled ( im:rotated ( im:segment ) pi ) 1/2 )))

( defun display-zero () 
  ( im:standardized ( im:chain ( im:rotated ( im:segment ) ( / pi 2 ))
                               ( im:scaled ( im:segment ) 2 )
                               ( im:rotated ( im:segment ) ( / pi -2 ))
                               ( im:rotated ( im:scaled ( im:segment ) 2 ) pi ))))

( defun display-numb ( numb )
  ( im:deck ( im:gap )
  ( cond ( ( eql numb 1 ) (display-one ) )
         ( ( eql numb 2 ) (display-two ) )
         ( ( eql numb 3 ) (display-three ) )
         ( ( eql numb 4 ) (display-four ) )
         ( ( eql numb 5 ) (display-five ) ) 
         ( ( eql numb 6 ) (display-six ) ) 
         ( ( eql numb 7 ) (display-seven ) ) 
         ( ( eql numb 8 ) (display-eighth ) ) 
         ( ( eql numb 9 ) (display-nine ) ) 
         ( t (display-zero ) ))

)) 

( defun display-negative-number ( number )
   ( im:row
     ( im:scaled ( im:rotated ( im:segment ) ( / pi 2 ) ) 1/2 )
     ( im:scaled ( im:gap ) 1/3 )
     ( display-positive-number ( abs number ))))

( defun display-positive-number ( number )
  ( labels (( dnumber (number) 
            ( if ( eql number 0 )
              ( im:scaled ( im:segment ) 0)
              ( im:row ( dnumber ( floor number 10 ))
                       ( im:scaled ( im:gap ) 1/3 )
                       ( display-numb ( rem number 10 ))))))
    ( if ( eql number 0 )
       ( display-numb ( display-zero ) )
      ( im:row
      ( dnumber number )
      ( im:scaled ( im:gap ) 1/3 )
))))

( defun display-number ( number )
                ( im:standardized
                 ( if ( >= number 0 )
                     (display-positive-number number)
                   ( display-negative-number number ))))

( defun display-table ( tbl columns rows )
  ( labels( ( display-row  ( tbl rows row column ) 
     ( and ( not ( eql row rows ))
           ( im:row  ( display-number ( table-nth tbl row column ) )
                    ( im:scaled ( im:gap ) 1/3 )
                    ( display-row tbl rows ( + 1 row ) column ))))
     ( dtable ( tbl rows columns column )
       (if ( not ( eql column columns ))
                    ( im:column 
                      ( dtable tbl rows columns ( + column 1 ))
                      ( im:scaled ( im:gap ) 1/3 )
                      ( display-row tbl rows 0 column )) 
         ( im:scaled  ( im:segment ) 0))))
    ( im:scaled ( im:standardized ( dtable tbl rows columns 0 ) ) 3.5 ) ))

;testy 
; nezapomen loadnout grafickou knihovnu od pana Docenta Krupky
;display-table ( lambda (x y) (* x y ) ) 11 11
;display-table ( lambda (x y) (+ x y ) ) 11 11
;display-table ( lambda (x y)  17  ) 10 5