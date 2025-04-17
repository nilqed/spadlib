(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FINITISE)) 
(FLUID '(*TRA INTVAR)) 
(EXPORTS (LIST 'FINITISE)) 
(IMPORTS
 (LIST 'NEWPLACE 'GETSQRTSFROMPLACES 'INTERR 'COMPLETEPLACES2 'SQRTSIGN)) 
(IMPORTS (LIST 'MKILIST 'EXTENPLACE)) 
(PUT 'FINITISE 'NUMBER-OF-ARGS 2) 
(PUT 'FINITISE 'DEFINED-ON-LINE '37) 
(PUT 'FINITISE 'DEFINED-IN-FILE 'ALGINT/FINITISE.RED) 
(PUT 'FINITISE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINITISE (PLACES MULTS)
    (PROG (PLACESMISC MULTSMISC M N SQRTS PLACES0 MULTS0 PLACESINF MULTSINF)
      (NEWPLACE (LIST (CONS INTVAR INTVAR)))
      (SETQ SQRTS (GETSQRTSFROMPLACES PLACES))
      (SETQ PLACESMISC PLACES)
      (SETQ MULTSMISC MULTS)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACESMISC) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (EQCAR (CDAR (CAR PLACESMISC)) 'QUOTIENT)
                (GREATERP N (CAR MULTSMISC)))
           (PROGN
            (SETQ N (CAR MULTSMISC))
            (SETQ M (MULTIPLICITY-FACTOR (CAR PLACESMISC))))))
         (SETQ PLACESMISC (CDR PLACESMISC))
         (SETQ MULTSMISC (CDR MULTSMISC)))
        (GO WHILELABEL))
      (COND ((EQUAL N 0) (INTERR "Why did we call finitise ??")))
      (SETQ N (DIVIDE N M))
      (COND
       ((AND (NEQ (CDR N) 0) *TRA)
        (PROGN
         (PRIN2 "Cannot get the poles moved precisely because of ramification")
         (TERPRI)
         "Cannot get the poles moved precisely because of ramification")))
      (COND ((LESSP (CDR N) 0) (SETQ N (PLUS (MINUS 1) (CAR N))))
            (T (SETQ N (CAR N))))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (COND
          ((ATOM (CDAR (CAR PLACES)))
           (PROGN
            (SETQ PLACES0 (CONS (CAR PLACES) PLACES0))
            (SETQ MULTS0 (CONS (CAR MULTS) MULTS0))))
          ((EQ (CAR (CDAR (CAR PLACES))) 'QUOTIENT)
           (PROGN
            (SETQ PLACESINF (CONS (CAR PLACES) PLACESINF))
            (SETQ MULTSINF (CONS (CAR MULTS) MULTSINF))))
          (T
           (PROGN
            (SETQ PLACESMISC (CONS (CAR PLACES) PLACESMISC))
            (SETQ MULTSMISC (CONS (CAR MULTS) MULTSMISC)))))
         (SETQ PLACES (CDR PLACES))
         (SETQ MULTS (CDR MULTS)))
        (GO WHILELABEL))
      (COND
       (PLACES0
        (PROGN
         (SETQ PLACES0 (COMPLETEPLACES2 PLACES0 MULTS0 SQRTS))
         (SETQ MULTS0 (CDR PLACES0))
         (SETQ PLACES0 (CAR PLACES0))
         (SETQ M (MULTIPLICITY-FACTOR (CAR PLACES0)))
         (SETQ MULTS0
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U MULTS0)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (PLUS U (TIMES N M))) (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (U) (PLUS U (TIMES N M))) (CAR U))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T
        (PROGN
         (SETQ PLACES0
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U (SQRTSIGN SQRTS INTVAR))
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (CONS (CONS INTVAR INTVAR) U))
                                     (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (U) (CONS (CONS INTVAR INTVAR) U))
                             (CAR U))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ MULTS0
                 (MKILIST PLACES0
                          (TIMES N (MULTIPLICITY-FACTOR (CAR PLACES0))))))))
      (SETQ PLACESINF
              (COMPLETEPLACES2 PLACESINF MULTSINF
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U (EXTENPLACE (CAR PLACESINF)))
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (U) (CAR U)) (CAR U)) NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (U) (CAR U)) (CAR U)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ MULTSINF (CDR PLACESINF))
      (SETQ PLACESINF (CAR PLACESINF))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACESINF) (RETURN NIL)))
        (PROGN
         (SETQ M (MULTIPLICITY-FACTOR (CAR PLACESINF)))
         (COND
          ((NEQ (CAR MULTSINF) (TIMES N M))
           (PROGN
            (SETQ PLACESMISC (CONS (CAR PLACESINF) PLACESMISC))
            (SETQ MULTSMISC
                    (CONS (DIFFERENCE (CAR MULTSINF) (TIMES N M))
                          MULTSMISC)))))
         (SETQ PLACESINF (CDR PLACESINF))
         (SETQ MULTSINF (CDR MULTSINF)))
        (GO WHILELABEL))
      (RETURN
       (LIST (NCONC PLACES0 PLACESMISC) (NCONC MULTS0 MULTSMISC) (MINUS N))))) 
(PUT 'MULTIPLICITY-FACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'MULTIPLICITY-FACTOR 'DEFINED-ON-LINE '123) 
(PUT 'MULTIPLICITY-FACTOR 'DEFINED-IN-FILE 'ALGINT/FINITISE.RED) 
(PUT 'MULTIPLICITY-FACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULTIPLICITY-FACTOR (PLACE)
    (PROG (N)
      (SETQ N 1)
      (PROG (U)
        (SETQ U PLACE)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (COND
            ((AND (EQ (CAR U) INTVAR) (EQCAR (CDR U) 'EXPT))
             (SETQ N (TIMES N (CADDR (CDR U)))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (RETURN N))) 
(ENDMODULE) 