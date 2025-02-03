(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RESIDUE)) 
(PACKAGES_TO_LOAD (LIST 'TAYLOR)) 
(CREATE-PACKAGE '(RESIDUE) '(CONTRIB MISC)) 
(FLUID '(*TAYLOR-MAX-PRECISION-CYCLES*)) 
(SETQ *TAYLOR-MAX-PRECISION-CYCLES* 20) 
(PUT 'POLYNOMQQQ 'NUMBER-OF-ARGS 1) 
(PUT 'POLYNOMQQQ 'DEFINED-ON-LINE '45) 
(PUT 'POLYNOMQQQ 'DEFINED-IN-FILE 'RESIDUE/RESIDUE.RED) 
(PUT 'POLYNOMQQQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLYNOMQQQ (X)
    ((LAMBDA (XX)
       (COND ((FIXP XX) 1) ((NOT (ONEP (CDR (SETQ XX (CADR XX))))) NIL)
             (T
              (PROG (KERNS KERN AA VAR FFORM MVV DEGG)
                (SETQ FFORM (SFP (CAAAR (CAR XX))))
                (SETQ VAR (REVAL1 (CADR X) T))
                (COND
                 (FFORM
                  (PROGN
                   (SETQ XX (CAR XX))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (NEQ XX 1)) (RETURN NIL)))
                     (PROGN
                      (SETQ MVV (CAAAR XX))
                      (SETQ DEGG (CDAAR XX))
                      (SETQ XX (CDAR XX))
                      (COND
                       ((OR (ATOM MVV) (ATOM (CAR MVV)))
                        (PROGN
                         (COND
                          ((NOT (FREEOF MVV VAR))
                           (PROGN
                            (SETQ XX 1)
                            (SETQ KERNS (LIST (LIST 'SIN VAR))))))))
                       (T
                        (SETQ KERNS
                                (APPEND (APPEND (KERNELS MVV) (KERNELS DEGG))
                                        KERNS)))))
                     (GO WHILELABEL))))
                 (T (SETQ KERNS (KERNELS (*Q2F XX)))))
               AA
                (COND ((NULL KERNS) (RETURN 1)))
                (SETQ KERN (FIRST KERNS))
                (SETQ KERNS (CDR KERNS))
                (COND
                 ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                 (T (GO AA)))))))
     (REVAL1 (CAR X) NIL))) 
(PUT 'POLYNOMQQ 'PSOPFN 'POLYNOMQQQ) 
(PUT 'TTTTYPE_RATPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-ON-LINE '72) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-IN-FILE 'RESIDUE/RESIDUE.RED) 
(PUT 'TTTTYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TTTTYPE_RATPOLY (U)
    ((LAMBDA (XX)
       (COND ((FIXP XX) 1) ((NOT (EQCAR XX '*SQ)) NIL)
             (T
              (AND
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CAR (CADR XX)) 1)) (REVAL1 (CADR U) T)))
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CDR (CADR XX)) 1))
                      (REVAL1 (CADR U) T)))))))
     (REVAL1 (CAR U) NIL))) 
(FLAG '(TYPE_RATPOLY) 'BOOLEAN) 
(PUT 'TYPE_RATPOLY 'PSOPFN 'TTTTYPE_RATPOLY) 
(PUT 'TYPE_RATPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'TYPE_RATPOLY 'DEFINED-ON-LINE '83) 
(PUT 'TYPE_RATPOLY 'DEFINED-IN-FILE 'RESIDUE/RESIDUE.RED) 
(PUT 'TYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPE_RATPOLY (F Z) (TTTTYPE_RATPOLY (LIST F Z))) 
(PUT 'RESIDUE 'NUMBER-OF-ARGS 3) 
(FLAG '(RESIDUE) 'OPFN) 
(PUT 'RESIDUE 'DEFINED-ON-LINE '88) 
(PUT 'RESIDUE 'DEFINED-IN-FILE 'RESIDUE/RESIDUE.RED) 
(PUT 'RESIDUE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESIDUE (F X A)
    (PROG (TMP NUMERATOR DENOMINATOR NUMCOF DENCOF)
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'FACTORIAL)))
        (AEVAL (REDERR (REVALX "residue of factorial not yet implemented")))))
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'BINOMIAL)))
        (AEVAL (REDERR (REVALX "residue of binomial not yet implemented")))))
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'POCHHAMMER)))
        (AEVAL (REDERR (REVALX "residue of Pochhammer not yet implemented")))))
      (SETQ TMP (AEVAL (LIST 'TAYLORTOSTANDARD (LIST 'TAYLOR F X A 0))))
      (COND
       ((EVALEQUAL (AEVAL A) (AEVAL 'INFINITY))
        (SETQ TMP
                (AEVAL
                 (LIST 'MINUS
                       (LIST 'SUB (LIST 'EQUAL X (LIST 'QUOTIENT 1 X))
                             TMP))))))
      (COND ((BOOLVALUE* (REVALX (LIST 'POLYNOMQQ TMP X))) (RETURN 0)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART TMP 0)) (AEVAL 'TAYLOR))
        (AEVAL (REDERR (REVALX "taylor fails")))))
      (COND
       ((NOT (TYPE_RATPOLY (REVALX TMP) (REVALX X))) (RETURN (AEVAL 'NIL))))
      (SETQ TMP (AEVAL (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS X A)) TMP)))
      (SETQ NUMERATOR (AEVAL (LIST 'NUM TMP)))
      (SETQ DENOMINATOR (AEVAL (LIST 'DEN TMP)))
      (COND
       ((OR (EVALEQUAL (AEVAL NUMERATOR) 0)
            (EVALLESSP (AEVAL (LIST 'DEG DENOMINATOR X)) 1))
        (RETURN 0))
       (T
        (PROGN
         (SETQ NUMCOF
                 (AEVAL
                  (LIST 'COEFFN NUMERATOR X
                        (LIST 'DIFFERENCE (LIST 'DEG DENOMINATOR X) 1))))
         (COND ((EVALEQUAL (AEVAL NUMCOF) 0) (RETURN 0)))
         (COND
          ((FREEOF (REVALX DENOMINATOR) (REVALX X))
           (SETQ DENCOF (AEVAL DENOMINATOR)))
          (T (SETQ DENCOF (AEVAL (LIST 'LCOF DENOMINATOR X)))))
         (RETURN (AEVAL (LIST 'QUOTIENT NUMCOF DENCOF)))
         (AEVAL 'NIL)))))) 
(PUT 'POLEORDER 'NUMBER-OF-ARGS 3) 
(FLAG '(POLEORDER) 'OPFN) 
(PUT 'POLEORDER 'DEFINED-ON-LINE '114) 
(PUT 'POLEORDER 'DEFINED-IN-FILE 'RESIDUE/RESIDUE.RED) 
(PUT 'POLEORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLEORDER (F X A)
    (PROG (TMP DENOMINATOR)
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'FACTORIAL)))
        (AEVAL
         (REDERR (REVALX "poleorder of factorial not yet implemented")))))
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'BINOMIAL)))
        (AEVAL (REDERR (REVALX "poleorder of binomial not yet implemented")))))
      (COND
       ((NOT (FREEOF (REVALX F) (REVALX 'POCHHAMMER)))
        (AEVAL
         (REDERR (REVALX "poleorder of Pochhammer not yet implemented")))))
      (SETQ TMP (AEVAL (LIST 'TAYLORTOSTANDARD (LIST 'TAYLOR F X A 0))))
      (COND
       ((EVALEQUAL (AEVAL A) (AEVAL 'INFINITY))
        (SETQ TMP
                (AEVAL
                 (LIST 'MINUS
                       (LIST 'SUB (LIST 'EQUAL X (LIST 'QUOTIENT 1 X))
                             TMP))))))
      (COND ((BOOLVALUE* (REVALX (LIST 'POLYNOMQQ TMP X))) (RETURN 0)))
      (SETQ DENOMINATOR (AEVAL (LIST 'DEN TMP)))
      (RETURN (AEVAL (LIST 'DEG DENOMINATOR X))))) 
(ENDMODULE) 