(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TPSCONV)) 
(EXPORTS (LIST '|PREP:PS| '|PS:PRINT1| '|PS:PRINT| '|PS:PRINT0|)) 
(FLUID '(|PS:EXP-LIM| *FORT *NAT *PSPRINTORDER)) 
(PUT '|PREP:PS| 'NUMBER-OF-ARGS 2) 
(PUT '|PREP:PS| 'DEFINED-ON-LINE '39) 
(PUT '|PREP:PS| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PREP:PS| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PREP:PS| (PS HIGHEST-ORDER)
    (PROG (X VAR INV)
      (SETQ VAR (|PS:MKVAR| PS))
      (SETQ INV (EQUAL (|PS:EXPANSION-POINT| PS) '|PS:INF|))
      (RETURN
       (REPLUS
        (PROG (J FORALL-RESULT FORALL-ENDPTR)
          (SETQ J (|PS:FIND-ORDER| PS))
         STARTOVER
          (COND ((MINUSP (DIFFERENCE HIGHEST-ORDER J)) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (PROGN
                   (SETQ X (PREPSQ* (|PS:EVALUATE| PS J)))
                   (COND ((EQUAL X 0) NIL)
                         (T
                          (LIST
                           (RETIMES
                            (CONS
                             (COND
                              ((AND (EQCAR X 'QUOTIENT)
                                    (EQCAR (CADR X) 'MINUS))
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (CADR (CADR X))
                                           (CADDR X))))
                              (T X))
                             (|PS:MKPOW| VAR J INV))))))))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
          (SETQ J (PLUS2 J 1))
          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
         LOOPLABEL
          (COND ((MINUSP (DIFFERENCE HIGHEST-ORDER J)) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (PROGN
                   (SETQ X (PREPSQ* (|PS:EVALUATE| PS J)))
                   (COND ((EQUAL X 0) NIL)
                         (T
                          (LIST
                           (RETIMES
                            (CONS
                             (COND
                              ((AND (EQCAR X 'QUOTIENT)
                                    (EQCAR (CADR X) 'MINUS))
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (CADR (CADR X))
                                           (CADDR X))))
                              (T X))
                             (|PS:MKPOW| VAR J INV))))))))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
          (SETQ J (PLUS2 J 1))
          (GO LOOPLABEL)))))) 
(PUT '|PS:MKPOW| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:MKPOW| 'DEFINED-ON-LINE '71) 
(PUT '|PS:MKPOW| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:MKPOW| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:MKPOW| (BAS EXP INVERTED)
    (COND ((EQUAL EXP 0) (LIST 1))
          (T
           ((LAMBDA (X)
              (COND (INVERTED (LIST (LIST 'QUOTIENT 1 X))) (T (LIST X))))
            (COND ((EQUAL EXP 1) BAS) (T (LIST 'EXPT BAS EXP))))))) 
(PUT '|PS:MKVAR| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:MKVAR| 'DEFINED-ON-LINE '77) 
(PUT '|PS:MKVAR| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:MKVAR| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:MKVAR| (PS)
    (PROG (VAR0 VAR)
      (SETQ VAR (|PS:DEPVAR| PS))
      (SETQ VAR0 (|PS:EXPANSION-POINT| PS))
      (COND ((OR (EQUAL VAR0 0) (EQUAL VAR0 '|PS:INF|)) (RETURN VAR))
            ((AND (NUMBERP VAR0) (LESSP VAR0 0))
             (RETURN (LIST 'PLUS VAR (MINUS VAR0))))
            ((EQCAR VAR0 'MINUS) (RETURN (LIST 'PLUS VAR (CADR VAR0))))
            ((AND (EQCAR VAR0 'QUOTIENT) (EQCAR (CADR VAR0) 'MINUS))
             (RETURN
              (LIST 'PLUS VAR (LIST 'QUOTIENT (CADADR VAR0) (CADDR VAR0)))))
            (T (RETURN (LIST 'DIFFERENCE VAR VAR0)))))) 
(PUT '|PS:BIG-O| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:BIG-O| 'DEFINED-ON-LINE '93) 
(PUT '|PS:BIG-O| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:BIG-O| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:BIG-O| (PS ORD)
    (CONS '|o|
          (|PS:MKPOW| (|PS:MKVAR| PS) ORD
           (EQUAL (|PS:EXPANSION-POINT| PS) '|PS:INF|)))) 
(PUT '|PS:PRINT1| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:PRINT1| 'DEFINED-ON-LINE '100) 
(PUT '|PS:PRINT1| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:PRINT1| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:PRINT1| (U)
    (PROG (PREPEXPR REST)
      (SETQ PREPEXPR (|PREP:PS| U |PS:EXP-LIM|))
      (COND
       (*PSPRINTORDER (SETQ REST (LIST (|PS:BIG-O| U (PLUS |PS:EXP-LIM| 1)))))
       (T (SETQ REST (LIST '|...|))))
      (RETURN
       (COND
        ((AND (EQUAL (|PS:EXPRESSION| U) 'FULL)
              (GEQ |PS:EXP-LIM| (|PS:LAST-TERM| U)))
         PREPEXPR)
        ((NOT (EQCAR PREPEXPR 'PLUS)) (CONS 'PLUS (CONS (OR PREPEXPR 0) REST)))
        (T (NCONC PREPEXPR REST)))))) 
(PUT '|:PS:| 'FANCY-REFORM '|PS:PRINT1|) 
(PUT '|PS:PRINT| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:PRINT| 'DEFINED-ON-LINE '119) 
(PUT '|PS:PRINT| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:PRINT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:PRINT| (U P)
    (COND (*FORT (FMPRINT (|PREP:PS| U |PS:EXP-LIM|) P))
          ((NULL *NAT)
           (MAPRINT
            (CONS 'PS
                  (CONS (|PS:VALUE| U)
                        (CONS (|PS:DEPVAR| U)
                              (LIST
                               ((LAMBDA (ABOUT)
                                  (COND ((EQUAL ABOUT '|PS:INF|) 'INFINITY)
                                        (T ABOUT)))
                                (|PS:EXPANSION-POINT| U))))))
            P))
          (T (MAPRINT (|PS:PRINT1| U) P)))) 
(PUT '|PS:PRINT0| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:PRINT0| 'DEFINED-ON-LINE '130) 
(PUT '|PS:PRINT0| 'DEFINED-IN-FILE 'TPS/TPSCONV.RED) 
(PUT '|PS:PRINT0| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:PRINT0| (U) (|PS:PRINT| U 0)) 
(ENDMODULE) 