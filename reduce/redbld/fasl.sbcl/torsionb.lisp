(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TORSIONB)) 
(FLUID '(*TRA *TRMIN INTVAR NESTEDSQRTS)) 
(EXPORTS (LIST 'BOUND-TORSION)) 
(PUT 'BOUND-TORSION 'NUMBER-OF-ARGS 2) 
(PUT 'BOUND-TORSION 'DEFINED-ON-LINE '34) 
(PUT 'BOUND-TORSION 'DEFINED-IN-FILE 'ALGINT/TORSIONB.RED) 
(PUT 'BOUND-TORSION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BOUND-TORSION (DIVISOR DOF1K)
    (PROG (FIELD PRIME1 PRIME2 PRIME3 MINIMUM PLACES NON-P1 NON-P2 NON-P3 CURVE
           CURVE2 NESTEDSQRTS)
      (SETQ PLACES
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U DIVISOR)
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
                (GO LOOPLABEL)))
      (SETQ CURVE (GETSQRTSFROMPLACES PLACES))
      (COND (NESTEDSQRTS (RERROR 'ALGINT 3 "Not yet implemented"))
            (T (SETQ CURVE2 CURVE)))
      (PROG (U)
        (SETQ U PLACES)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROG ()
             (SETQ U (CDAR U))
             (COND ((AND (EQCAR U 'QUOTIENT) (EQUAL (CADR U) 1)) (RETURN NIL)))
             (SETQ U (SUBSTITUTESQ (SIMP U) (LIST (CONS INTVAR 0))))
             (SETQ FIELD (UNION FIELD (SQRTSINSQ U NIL)))
             (SETQ U (LIST (CONS INTVAR (PREPSQ U))))
             (PROG (V)
               (SETQ V CURVE2)
              LAB
               (COND ((NULL V) (RETURN NIL)))
               ((LAMBDA (V)
                  (SETQ FIELD
                          (UNION FIELD (SQRTSINSQ (SUBSTITUTESQ V U) NIL))))
                (CAR V))
               (SETQ V (CDR V))
               (GO LAB))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (SETQ PRIME1 2)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NULL (SETQ NON-P1 (GOOD-REDUCTION CURVE DOF1K FIELD PRIME1))))
          (RETURN NIL)))
        (SETQ PRIME1 (NEXTPRIME PRIME1))
        (GO WHILELABEL))
      (SETQ PRIME2 (NEXTPRIME PRIME1))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NULL (SETQ NON-P2 (GOOD-REDUCTION CURVE DOF1K FIELD PRIME2))))
          (RETURN NIL)))
        (SETQ PRIME2 (NEXTPRIME PRIME2))
        (GO WHILELABEL))
      (SETQ PRIME3 (NEXTPRIME PRIME2))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NULL (SETQ NON-P3 (GOOD-REDUCTION CURVE DOF1K FIELD PRIME3))))
          (RETURN NIL)))
        (SETQ PRIME3 (NEXTPRIME PRIME3))
        (GO WHILELABEL))
      (SETQ MINIMUM (FIX (SQRT (FLOAT (TIMES NON-P1 NON-P2 NON-P3)))))
      (SETQ MINIMUM
              (MIN MINIMUM
                   (TIMES NON-P1 (MAX-POWER PRIME1 (MIN NON-P2 NON-P3)))))
      (SETQ MINIMUM
              (MIN MINIMUM
                   (TIMES NON-P2 (MAX-POWER PRIME2 (MIN NON-P1 NON-P3)))))
      (SETQ MINIMUM
              (MIN MINIMUM
                   (TIMES NON-P3 (MAX-POWER PRIME3 (MIN NON-P2 NON-P1)))))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRINC "Torsion is bounded by ")
         (PROGN (PRIN2 MINIMUM) (TERPRI) MINIMUM))))
      (RETURN MINIMUM))) 
(PUT 'MAX-POWER 'NUMBER-OF-ARGS 2) 
(PUT 'MAX-POWER 'DEFINED-ON-LINE '75) 
(PUT 'MAX-POWER 'DEFINED-IN-FILE 'ALGINT/TORSIONB.RED) 
(PUT 'MAX-POWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAX-POWER (P N)
    (PROG (ANS)
      (SETQ ANS 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ ANS N)) (RETURN NIL)))
        (SETQ ANS (TIMES ANS P))
        (GO WHILELABEL))
      (SETQ ANS (QUOTIENT ANS P)))) 
(PUT 'GOOD-REDUCTION 'NUMBER-OF-ARGS 4) 
(PUT 'GOOD-REDUCTION 'DEFINED-ON-LINE '85) 
(PUT 'GOOD-REDUCTION 'DEFINED-IN-FILE 'ALGINT/TORSIONB.RED) 
(PUT 'GOOD-REDUCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GOOD-REDUCTION (CURVE DOF1K FIELD PRIME)
    (PROG (U)
      (SETQ U (ALGEBRAIC-FACTORISE PRIME FIELD))
      (INTERR "Good reduction not finished"))) 
(ENDMODULE) 