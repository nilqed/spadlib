(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BIBASIS_TSET)) 
(PUT 'SETTRESET 'NUMBER-OF-ARGS 0) 
(PUT 'SETTRESET 'DEFINED-ON-LINE '41) 
(PUT 'SETTRESET 'DEFINED-IN-FILE 'BIBASIS/BIBASIS_TSET.RED) 
(PUT 'SETTRESET 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SETTRESET NIL
    (PROG ()
      (SETQ FLUIDBIBASISJANETTREEROOTNODE NIL)
      (SETQ FLUIDBIBASISSETT (CONS NIL NIL)))) 
(PUT 'SETTINSERT 'NUMBER-OF-ARGS 1) 
(PUT 'SETTINSERT 'DEFINED-ON-LINE '48) 
(PUT 'SETTINSERT 'DEFINED-IN-FILE 'BIBASIS/BIBASIS_TSET.RED) 
(PUT 'SETTINSERT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETTINSERT (TRIPLE)
    (PROG ()
      (JANETTREEINSERT TRIPLE)
      (SETQ FLUIDBIBASISSETT (CONS TRIPLE FLUIDBIBASISSETT)))) 
(PUT 'SETTCOLLECTNONMULTIPROLONGATIONS 'NUMBER-OF-ARGS 1) 
(PUT 'SETTCOLLECTNONMULTIPROLONGATIONS 'DEFINED-ON-LINE '55) 
(PUT 'SETTCOLLECTNONMULTIPROLONGATIONS 'DEFINED-IN-FILE
     'BIBASIS/BIBASIS_TSET.RED) 
(PUT 'SETTCOLLECTNONMULTIPROLONGATIONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETTCOLLECTNONMULTIPROLONGATIONS (TRIPLELIST)
    (COND
     ((CAR FLUIDBIBASISSETT)
      (PROG (LASTTRIPLE TMPPOLYNOM TMPTRIPLE LASTNONMULTIVAR)
        (SETQ LASTNONMULTIVAR 0)
        (SETQ LASTTRIPLE (CAR FLUIDBIBASISSETT))
        (SETQ LASTNONMULTIVAR
                (ISUB1 (MONOMGETFIRSTMULTIVAR (CAR (GETV LASTTRIPLE 1)))))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE LASTNONMULTIVAR I)) (RETURN NIL)))
          (PROGN
           (COND
            ((NOT (TRIPLEISPROLONGEDBY LASTTRIPLE I))
             (PROGN
              (SETQ TMPPOLYNOM
                      (POLYNOMMULTIPLYBYMONOM (GETV LASTTRIPLE 1)
                       (GETV FLUIDBIBASISSINGLEVARIABLEMONOMIALSS I)))
              (TRIPLESETPROLONGEDBY LASTTRIPLE I)
              (COND
               ((CAR TMPPOLYNOM)
                (PROGN
                 (SETQ TMPTRIPLE
                         (CREATETRIPLEWITHANCESTOR TMPPOLYNOM
                          (GETV LASTTRIPLE 2)))
                 (TRIPLESETPROLONGSET TMPTRIPLE (GETV LASTTRIPLE 3))
                 (SORTEDTRIPLELISTINSERT TRIPLELIST TMPTRIPLE)
                 NIL)))
              NIL)))
           NIL)
          (SETQ I (PLUS2 I 1))
          (GO LAB)))))) 
(PUT 'SETTPRINT 'NUMBER-OF-ARGS 0) 
(PUT 'SETTPRINT 'DEFINED-ON-LINE '77) 
(PUT 'SETTPRINT 'DEFINED-IN-FILE 'BIBASIS/BIBASIS_TSET.RED) 
(PUT 'SETTPRINT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SETTPRINT NIL
    (PROG (CURRENTTRIPLE)
      (PRIN2 "SetT( ")
      (SETQ CURRENTTRIPLE FLUIDBIBASISSETT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR CURRENTTRIPLE)) (RETURN NIL)))
        (PROGN
         (PRIN2 (CAR CURRENTTRIPLE))
         (PRIN2 ", ")
         (SETQ CURRENTTRIPLE (CDR CURRENTTRIPLE))
         NIL)
        (GO WHILELABEL))
      (PRIN2 " )")
      (TERPRI))) 
(ENDMODULE) 