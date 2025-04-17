(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYCONV)) 
(EXPORTS (LIST 'PREPTAYLOR** 'PREPTAYLOR* 'PREPTAYLOR*1 'TAYLOR-GEN-BIG-O)) 
(IMPORTS
 (LIST 'EQCAR 'LASTPAIR 'PREPSQ* 'REPLUS 'RETIMES 'REVAL 'PREPTAYEXP 'TAYCFPL
       'TAYCFSQ 'TAYCOEFFLIST 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELPOINT
       'TAYTPELVARS)) 
(FLUID '(CONVERT-TAYLOR* TAYLORPRINTTERMS TAYLOR-TRUNCATION-FLAG)) 
(PUT 'PREPTAYLOR*1 'NUMBER-OF-ARGS 3) 
(PUT 'PREPTAYLOR*1 'DEFINED-ON-LINE '53) 
(PUT 'PREPTAYLOR*1 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'PREPTAYLOR*1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREPTAYLOR*1 (COEFFLIST TEMPLATE NO-OF-TERMS)
    (REPLUS
     (PROG (CC FORALL-RESULT FORALL-ENDPTR)
       (SETQ CC COEFFLIST)
      STARTOVER
       (COND ((NULL CC) (RETURN NIL)))
       (SETQ FORALL-RESULT
               ((LAMBDA (CC)
                  (PROG (X)
                    (COND (TAYLOR-TRUNCATION-FLAG (RETURN NIL)))
                    (SETQ X (PREPTAYLOR*2 CC TEMPLATE))
                    (COND ((OR (NULL X) (NULL NO-OF-TERMS)) (RETURN X)))
                    (SETQ NO-OF-TERMS (DIFFERENCE NO-OF-TERMS 1))
                    (COND
                     ((LESSP NO-OF-TERMS 0)
                      (PROGN (SETQ TAYLOR-TRUNCATION-FLAG T) (RETURN NIL))))
                    (RETURN X)))
                (CAR CC)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ CC (CDR CC))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL CC) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               ((LAMBDA (CC)
                  (PROG (X)
                    (COND (TAYLOR-TRUNCATION-FLAG (RETURN NIL)))
                    (SETQ X (PREPTAYLOR*2 CC TEMPLATE))
                    (COND ((OR (NULL X) (NULL NO-OF-TERMS)) (RETURN X)))
                    (SETQ NO-OF-TERMS (DIFFERENCE NO-OF-TERMS 1))
                    (COND
                     ((LESSP NO-OF-TERMS 0)
                      (PROGN (SETQ TAYLOR-TRUNCATION-FLAG T) (RETURN NIL))))
                    (RETURN X)))
                (CAR CC)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ CC (CDR CC))
       (GO LOOPLABEL)))) 
(PUT 'PREPTAYLOR*2 'NUMBER-OF-ARGS 2) 
(PUT 'PREPTAYLOR*2 'DEFINED-ON-LINE '66) 
(PUT 'PREPTAYLOR*2 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'PREPTAYLOR*2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREPTAYLOR*2 (COEFF TEMPLATE)
    ((LAMBDA (PC)
       (COND ((EQUAL PC 0) NIL)
             (T
              (LIST
               (RETIMES
                (CONS
                 (COND
                  ((AND (EQCAR PC 'QUOTIENT) (EQCAR (CADR PC) 'MINUS))
                   (LIST 'MINUS (LIST 'QUOTIENT (CADR (CADR PC)) (CADDR PC))))
                  (T PC))
                 (PREPTAYCOEFF (CAR COEFF) TEMPLATE)))))))
     (PREPSQ* (CDR COEFF)))) 
(PUT 'CHECKDIFFERENCE 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKDIFFERENCE 'DEFINED-ON-LINE '76) 
(PUT 'CHECKDIFFERENCE 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'CHECKDIFFERENCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKDIFFERENCE (VAR VAR0)
    (COND ((EQUAL VAR0 0) VAR)
          ((AND (NUMBERP VAR0) (LESSP VAR0 0)) (LIST 'PLUS VAR (MINUS VAR0)))
          ((AND (EQCAR VAR0 'QUOTIENT) (EQCAR (CADR VAR0) 'MINUS))
           (LIST 'PLUS VAR (LIST 'QUOTIENT (CADR (CADR VAR0)) (CADDR VAR0))))
          (T (LIST 'DIFFERENCE VAR VAR0)))) 
(PUT 'CHECKEXP 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKEXP 'DEFINED-ON-LINE '84) 
(PUT 'CHECKEXP 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'CHECKEXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKEXP (BAS EXP)
    (COND ((EQUAL EXP 0) 1) ((EQUAL EXP 1) BAS)
          (T (LIST 'EXPT BAS (PREPTAYEXP EXP))))) 
(DE CHECKPOWER (VAR VAR0 N)
    (COND
     ((EQ VAR0 'INFINITY)
      (COND ((EQUAL N 0) 1) (T (LIST 'QUOTIENT 1 (CHECKEXP VAR N)))))
     (T (CHECKEXP (CHECKDIFFERENCE VAR (REVAL1 VAR0 T)) N)))) 
(PUT 'CHECKPOWER 'NUMBER-OF-ARGS 3) 
(PUT 'CHECKPOWER 'DEFINED-ON-LINE '89) 
(PUT 'CHECKPOWER 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'CHECKPOWER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'CHECKPOWER 'INLINE
      '(LAMBDA (VAR VAR0 N)
         (COND
          ((EQ VAR0 'INFINITY)
           (COND ((EQUAL N 0) 1) (T (LIST 'QUOTIENT 1 (CHECKEXP VAR N)))))
          (T (CHECKEXP (CHECKDIFFERENCE VAR (REVAL1 VAR0 T)) N))))) 
(PUT 'PREPTAYCOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'PREPTAYCOEFF 'DEFINED-ON-LINE '95) 
(PUT 'PREPTAYCOEFF 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'PREPTAYCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREPTAYCOEFF (CC TEMPLATE)
    (PROG (RESULT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL TEMPLATE))) (RETURN NIL)))
        (PROG (CCL)
          (SETQ CCL (CAR CC))
          (PROG (VAR)
            (SETQ VAR (CAR (CAR TEMPLATE)))
           LAB
            (COND ((NULL VAR) (RETURN NIL)))
            ((LAMBDA (VAR)
               (PROGN
                (SETQ RESULT
                        (CONS
                         ((LAMBDA (G162)
                            (COND
                             ((EQ G162 'INFINITY)
                              (COND ((EQUAL (CAR CCL) 0) 1)
                                    (T
                                     (LIST 'QUOTIENT 1
                                           (CHECKEXP VAR (CAR CCL))))))
                             (T
                              (CHECKEXP (CHECKDIFFERENCE VAR (REVAL1 G162 T))
                               (CAR CCL)))))
                          (CADR (CAR TEMPLATE)))
                         RESULT))
                (SETQ CCL (CDR CCL))))
             (CAR VAR))
            (SETQ VAR (CDR VAR))
            (GO LAB))
          (SETQ CC (CDR CC))
          (SETQ TEMPLATE (CDR TEMPLATE)))
        (GO WHILELABEL))
      (RETURN RESULT))) 
(PUT 'TAYLOR* 'PREPFN2 'PREPTAYLOR**) 
(PUT 'PREPTAYLOR** 'NUMBER-OF-ARGS 1) 
(PUT 'PREPTAYLOR** 'DEFINED-ON-LINE '111) 
(PUT 'PREPTAYLOR** 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'PREPTAYLOR** 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPTAYLOR** (U) (COND ((NULL CONVERT-TAYLOR*) U) (T (PREPTAYLOR* U)))) 
(PUT 'PREPTAYLOR* 'NUMBER-OF-ARGS 1) 
(PUT 'PREPTAYLOR* 'DEFINED-ON-LINE '114) 
(PUT 'PREPTAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'PREPTAYLOR* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPTAYLOR* (U) (PREPTAYLOR*1 (CADR U) (CADDR U) NIL)) 
(PUT 'TAYLOR-GEN-BIG-O 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLOR-GEN-BIG-O 'DEFINED-ON-LINE '117) 
(PUT 'TAYLOR-GEN-BIG-O 'DEFINED-IN-FILE 'TAYLOR/TAYCONV.RED) 
(PUT 'TAYLOR-GEN-BIG-O 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLOR-GEN-BIG-O (TP)
    (CONS "O"
          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
            (SETQ EL TP)
            (COND ((NULL EL) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (COND
                                 ((NULL (CDR (CAR EL)))
                                  ((LAMBDA (G164 G166)
                                     (COND
                                      ((EQ (CADR EL) 'INFINITY)
                                       (COND ((EQUAL G166 0) 1)
                                             (T
                                              (LIST 'QUOTIENT 1
                                                    (CHECKEXP G164 G166)))))
                                      (T
                                       (CHECKEXP
                                        (CHECKDIFFERENCE G164
                                         (REVAL1 (CADR EL) T))
                                        G166))))
                                   (CAR (CAR EL)) (CADDDR EL)))
                                 (T
                                  (PROG (VAR0)
                                    (SETQ VAR0 (REVAL1 (CADR EL) T))
                                    (RETURN
                                     (COND
                                      ((EQ VAR0 'INFINITY)
                                       (LIST 'QUOTIENT 1
                                             (CHECKEXP (CONS 'LIST (CAR EL))
                                              (CADDDR EL))))
                                      (T
                                       (CHECKEXP
                                        (CONS 'LIST
                                              (PROG (KRNL FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ KRNL (CAR EL))
                                                (COND
                                                 ((NULL KRNL) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA
                                                                      (KRNL)
                                                                    (CHECKDIFFERENCE
                                                                     KRNL
                                                                     VAR0))
                                                                  (CAR KRNL))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ KRNL (CDR KRNL))
                                                (COND
                                                 ((NULL KRNL)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (KRNL)
                                                            (CHECKDIFFERENCE
                                                             KRNL VAR0))
                                                          (CAR KRNL))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL)))
                                        (CADDDR EL)))))))))
                              (CAR EL))
                             NIL)))
           LOOPLABEL
            (SETQ EL (CDR EL))
            (COND ((NULL EL) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (EL)
                        (COND
                         ((NULL (CDR (CAR EL)))
                          ((LAMBDA (G164 G166)
                             (COND
                              ((EQ (CADR EL) 'INFINITY)
                               (COND ((EQUAL G166 0) 1)
                                     (T
                                      (LIST 'QUOTIENT 1
                                            (CHECKEXP G164 G166)))))
                              (T
                               (CHECKEXP
                                (CHECKDIFFERENCE G164 (REVAL1 (CADR EL) T))
                                G166))))
                           (CAR (CAR EL)) (CADDDR EL)))
                         (T
                          (PROG (VAR0)
                            (SETQ VAR0 (REVAL1 (CADR EL) T))
                            (RETURN
                             (COND
                              ((EQ VAR0 'INFINITY)
                               (LIST 'QUOTIENT 1
                                     (CHECKEXP (CONS 'LIST (CAR EL))
                                      (CADDDR EL))))
                              (T
                               (CHECKEXP
                                (CONS 'LIST
                                      (PROG (KRNL FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ KRNL (CAR EL))
                                        (COND ((NULL KRNL) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (KRNL)
                                                            (CHECKDIFFERENCE
                                                             KRNL VAR0))
                                                          (CAR KRNL))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ KRNL (CDR KRNL))
                                        (COND
                                         ((NULL KRNL) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (KRNL)
                                                    (CHECKDIFFERENCE KRNL
                                                     VAR0))
                                                  (CAR KRNL))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                                (CADDDR EL)))))))))
                      (CAR EL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(ENDMODULE) 