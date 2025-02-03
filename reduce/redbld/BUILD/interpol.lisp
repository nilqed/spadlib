(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTERPOL)) 
(PUT 'INTERPOL 'NUMBER-OF-ARGS 3) 
(PUT 'INTERPOL 'DEFINED-ON-LINE '30) 
(PUT 'INTERPOL 'DEFINED-IN-FILE 'POLY/INTERPOL.RED) 
(PUT 'INTERPOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTERPOL (FC X PTS)
    (PROG (D Q S P1 P2 X1 X2 F1 F2 FNEW)
      (COND
       ((OR (NOT (EQCAR FC 'LIST)) (NOT (EQCAR PTS 'LIST))
            (NOT (EQUAL (LENGTH FC) (LENGTH PTS))))
        (RERROR 'POLY 19 "Illegal parameters for interpol")))
      (SETQ S
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (PAIR (CDR FC) (CDR PTS)))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS (SIMP (CAR P))
                                          (CONS (SIMP (CDR P))
                                                (SIMP (CDR P)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (SIMP (CAR P))
                                  (CONS (SIMP (CDR P)) (SIMP (CDR P)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (SIMP X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR S)) (RETURN NIL)))
        (PROGN
         (SETQ Q NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR S)) (RETURN NIL)))
           (PROGN
            (SETQ P1 (CAR S))
            (SETQ S (CDR S))
            (SETQ P2 (CAR S))
            (SETQ F1 (CAR P1))
            (SETQ F2 (CAR P2))
            (SETQ X1 (CADR P1))
            (SETQ X2 (CDDR P2))
            (SETQ D (ADDSQ X1 (NEGSQ X2)))
            (COND
             ((NULL (CAR D))
              (RERROR 'POLY 20
                      "Interpolation impossible if two points are equal")))
            (SETQ FNEW
                    (MULTSQ
                     (ADDSQ (MULTSQ (ADDSQ X (NEGSQ X2)) F1)
                            (NEGSQ (MULTSQ (ADDSQ X (NEGSQ X1)) F2)))
                     (INVSQ D)))
            (SETQ Q (CONS (CONS FNEW (CONS X1 X2)) Q))
            NIL)
           (GO WHILELABEL))
         (SETQ S (REVERSIP Q))
         NIL)
        (GO WHILELABEL))
      (RETURN (PREPSQ (CAAR S))))) 
(FLAG '(INTERPOL) 'OPFN) 
(ENDMODULE) 