(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'POLYDIV)) 
(PUT 'DIVIDE 'PSOPFN 'POLY-DIVIDE) 
(PUT 'POLY-DIVIDE 'NUMBER-OF-ARGS 1) 
(PUT 'POLY-DIVIDE 'DEFINED-ON-LINE '59) 
(PUT 'POLY-DIVIDE 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'POLY-DIVIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLY-DIVIDE (U) (POLY-DIVIDE* U NIL NIL)) 
(REMPROP 'REMAINDER 'POLYFN) 
(PUT 'REMAINDER 'PSOPFN 'POLY-REMAINDER) 
(PUT 'MOD 'PSOPFN 'POLY-REMAINDER) 
(PUT 'POLY-REMAINDER 'NUMBER-OF-ARGS 1) 
(PUT 'POLY-REMAINDER 'DEFINED-ON-LINE '65) 
(PUT 'POLY-REMAINDER 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'POLY-REMAINDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLY-REMAINDER (U) (POLY-DIVIDE* U 'REMAINDER NIL)) 
(PUT 'POLY_QUOTIENT 'PSOPFN 'POLY-QUOTIENT) 
(PUT 'POLY-QUOTIENT 'NUMBER-OF-ARGS 1) 
(PUT 'POLY-QUOTIENT 'DEFINED-ON-LINE '69) 
(PUT 'POLY-QUOTIENT 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'POLY-QUOTIENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLY-QUOTIENT (U) (POLY-DIVIDE* U 'QUOTIENT NIL)) 
(INFIX (LIST 'DIVIDE 'MOD 'POLY_QUOTIENT)) 
(PRECEDENCE (LIST 'DIVIDE 'FREEOF)) 
(PRECEDENCE (LIST 'POLY_QUOTIENT 'DIVIDE)) 
(PRECEDENCE (LIST 'MOD 'POLY_QUOTIENT)) 
(PUT 'PSEUDO_DIVIDE 'PSOPFN 'PSEUDO-DIVIDE) 
(PUT 'PSEUDO-DIVIDE 'NUMBER-OF-ARGS 1) 
(PUT 'PSEUDO-DIVIDE 'DEFINED-ON-LINE '84) 
(PUT 'PSEUDO-DIVIDE 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'PSEUDO-DIVIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSEUDO-DIVIDE (U) (POLY-DIVIDE* U NIL T)) 
(PUT 'PSEUDO_REMAINDER 'PSOPFN 'PSEUDO-REMAINDER) 
(PUT 'PSEUDO-REMAINDER 'NUMBER-OF-ARGS 1) 
(PUT 'PSEUDO-REMAINDER 'DEFINED-ON-LINE '88) 
(PUT 'PSEUDO-REMAINDER 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'PSEUDO-REMAINDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSEUDO-REMAINDER (U) (POLY-DIVIDE* U 'REMAINDER T)) 
(PUT 'PSEUDO_DIV 'PSOPFN 'PSEUDO-QUOTIENT) 
(PUT 'PSEUDO_QUOTIENT 'PSOPFN 'PSEUDO-QUOTIENT) 
(PUT 'PSEUDO-QUOTIENT 'NUMBER-OF-ARGS 1) 
(PUT 'PSEUDO-QUOTIENT 'DEFINED-ON-LINE '93) 
(PUT 'PSEUDO-QUOTIENT 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'PSEUDO-QUOTIENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSEUDO-QUOTIENT (U) (POLY-DIVIDE* U 'QUOTIENT T)) 
(FLUID '(KORD*)) 
(PUT 'POLY-DIVIDE* 'NUMBER-OF-ARGS 3) 
(PUT 'POLY-DIVIDE* 'DEFINED-ON-LINE '99) 
(PUT 'POLY-DIVIDE* 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'POLY-DIVIDE* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLY-DIVIDE* (U FN PSEUDO)
    (COND ((EQCAR (CADR U) 'LIST) (TYPERR (CADR U) 'POLYNOMIAL))
          ((EQCAR (CAR U) 'LIST)
           (CONS 'LIST
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (CDAR U))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V)
                                       (POLY-DIVIDE** (CONS V (CDR U)) FN
                                                      PSEUDO))
                                     (CAR V))
                                    NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (V)
                               (POLY-DIVIDE** (CONS V (CDR U)) FN PSEUDO))
                             (CAR V))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (POLY-DIVIDE** U FN PSEUDO)))) 
(PUT 'POLY-DIVIDE** 'NUMBER-OF-ARGS 3) 
(PUT 'POLY-DIVIDE** 'DEFINED-ON-LINE '106) 
(PUT 'POLY-DIVIDE** 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'POLY-DIVIDE** 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLY-DIVIDE** (U FN PSEUDO)
    (PROG (P Q X NEW_KORD)
      (COND ((NULL (CDR U)) (REDERR "Divisor required")))
      (COND
       ((GREATERP (LENGTH U) 3)
        (REDERR "Division operators take 2 or 3 arguments.")))
      (COND ((NULL (SETQ Q (*Q2F (SIMP* (CADR U))))) (REDERR "Zero divisor")))
      (SETQ P (*Q2F (SIMP* (CAR U))))
      (COND
       ((AND (EQ FN 'REMAINDER) (FIXP P) (FIXP Q))
        (COND ((GREATERP Q 0) (RETURN (EVALMOD U))) (T (TYPERR Q "modulus")))))
      (COND
       ((AND (CDDR U) (SETQ X (*A2K (CADDR U)))
             (NOT (AND KORD* (EQ X (CAR KORD*)))))
        ((LAMBDA (KORD*)
           (PROGN
            (SETQ NEW_KORD T)
            (UPDKORDER X)
            (SETQ P (REORDER P))
            (SETQ Q (REORDER Q))))
         KORD*)))
      (SETQ U (COND (PSEUDO (PSEUDO-QREMF P Q X)) (T (QREMF P Q))))
      (SETQ P (CAR U))
      (SETQ Q (CDR U))
      (COND
       (NEW_KORD
        (PROGN
         (COND ((NOT (EQ FN 'REMAINDER)) (SETQ P (REORDER P))))
         (COND ((NOT (EQ FN 'QUOTIENT)) (SETQ Q (REORDER Q)))))))
      (RETURN
       (COND ((EQ FN 'REMAINDER) (MK*SQ (CONS Q 1)))
             ((EQ FN 'QUOTIENT) (MK*SQ (CONS P 1)))
             (T (LIST 'LIST (MK*SQ (CONS P 1)) (MK*SQ (CONS Q 1)))))))) 
(PUT 'PSEUDO-QREMF 'NUMBER-OF-ARGS 3) 
(PUT 'PSEUDO-QREMF 'DEFINED-ON-LINE '140) 
(PUT 'PSEUDO-QREMF 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'PSEUDO-QREMF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PSEUDO-QREMF (U V VAR)
    (PROG (NO_VAR M N K Q0 Q CAR_V CAR_U VV)
      (SETQ NO_VAR (NULL VAR))
      (SETQ M
              (COND
               ((OR (OR (ATOM U) (ATOM (CAR U)))
                    (AND VAR (NOT (EQ (CAAAR U) VAR))))
                0)
               (T (PROGN (COND ((NOT VAR) (SETQ VAR (CAAAR U)))) (CDAAR U)))))
      (SETQ N
              (COND
               ((OR (OR (ATOM V) (ATOM (CAR V)))
                    (AND VAR (NOT (EQ (CAAAR V) VAR))))
                0)
               (T (PROGN (COND ((NOT VAR) (SETQ VAR (CAAAR V)))) (CDAAR V)))))
      (SETQ U (COND ((ZEROP M) (LIST U)) (T (COEFFS U))))
      (SETQ V (COND ((ZEROP N) (LIST V)) (T (COEFFS V))))
      (COND
       ((AND NO_VAR (NOT (AND (DOMAINP_LIST V) (DOMAINP_LIST U))))
        (MSGPRI "Main division variable selected is" VAR NIL NIL NIL)))
      (SETQ K (DIFFERENCE M N))
      (SETQ CAR_V (CAR V))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ K 0)) (RETURN NIL)))
        (PROGN
         (SETQ Q0 (CONS (SETQ CAR_U (CAR U)) Q0))
         (SETQ VV (CDR V))
         (SETQ U
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C (CDR U))
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (PROGN
                                        (SETQ C
                                                (COND
                                                 (*PHYSOP-LOADED
                                                  (PHYSOP-MULTF C CAR_V))
                                                 (T (POLY-MULTF C CAR_V))))
                                        (COND
                                         (VV
                                          (PROGN
                                           (SETQ C
                                                   (ADDF C
                                                         (NEGF
                                                          (COND
                                                           (*PHYSOP-LOADED
                                                            (PHYSOP-MULTF CAR_U
                                                             (CAR VV)))
                                                           (T
                                                            (POLY-MULTF CAR_U
                                                                        (CAR
                                                                         VV)))))))
                                           (SETQ VV (CDR VV)))))
                                        C))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C)
                               (PROGN
                                (SETQ C
                                        (COND
                                         (*PHYSOP-LOADED
                                          (PHYSOP-MULTF C CAR_V))
                                         (T (POLY-MULTF C CAR_V))))
                                (COND
                                 (VV
                                  (PROGN
                                   (SETQ C
                                           (ADDF C
                                                 (NEGF
                                                  (COND
                                                   (*PHYSOP-LOADED
                                                    (PHYSOP-MULTF CAR_U
                                                     (CAR VV)))
                                                   (T
                                                    (POLY-MULTF CAR_U
                                                                (CAR VV)))))))
                                   (SETQ VV (CDR VV)))))
                                C))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ K (DIFFERENCE K 1)))
        (GO WHILELABEL))
      (COND
       (Q0
        (PROGN
         (SETQ Q (CONS (CAR Q0) NIL))
         (SETQ VV 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (SETQ Q0 (CDR Q0))) (RETURN NIL)))
           (SETQ Q
                   (CONS
                    ((LAMBDA (G565)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR Q0) G565))
                             (T (POLY-MULTF (CAR Q0) G565))))
                     (SETQ VV
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF VV CAR_V))
                                   (T (POLY-MULTF VV CAR_V)))))
                    Q))
           (GO WHILELABEL)))))
      (RETURN (CONS (COEFFS-TO-FORM Q VAR) (COEFFS-TO-FORM U VAR))))) 
(PUT 'COEFFS-TO-FORM 'NUMBER-OF-ARGS 2) 
(PUT 'COEFFS-TO-FORM 'DEFINED-ON-LINE '189) 
(PUT 'COEFFS-TO-FORM 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'COEFFS-TO-FORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFFS-TO-FORM (COEFF_LIST VAR)
    (AND COEFF_LIST
         (COEFFS-TO-FORM1 COEFF_LIST VAR (DIFFERENCE (LENGTH COEFF_LIST) 1)))) 
(PUT 'COEFFS-TO-FORM1 'NUMBER-OF-ARGS 3) 
(PUT 'COEFFS-TO-FORM1 'DEFINED-ON-LINE '195) 
(PUT 'COEFFS-TO-FORM1 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'COEFFS-TO-FORM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFFS-TO-FORM1 (COEFF_LIST VAR D)
    (COND
     ((GREATERP D 0)
      ((LAMBDA (REDUCTUM)
         (COND
          ((CAR COEFF_LIST)
           (CONS (CONS (CONS VAR D) (CAR COEFF_LIST)) REDUCTUM))
          (T REDUCTUM)))
       (COEFFS-TO-FORM1 (CDR COEFF_LIST) VAR (DIFFERENCE D 1))))
     (T (CAR COEFF_LIST)))) 
(PUT 'DOMAINP_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'DOMAINP_LIST 'DEFINED-ON-LINE '204) 
(PUT 'DOMAINP_LIST 'DEFINED-IN-FILE 'POLY/POLYDIV.RED) 
(PUT 'DOMAINP_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DOMAINP_LIST (COEFF_LIST)
    (OR (NULL COEFF_LIST)
        (AND (OR (ATOM (CAR COEFF_LIST)) (ATOM (CAR (CAR COEFF_LIST))))
             (DOMAINP_LIST (CDR COEFF_LIST))))) 
(ENDMODULE) 