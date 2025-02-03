(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPLOG)) 
(FLUID '(*INTFLAG* *NONEGLOGS *EXPANDLOGS)) 
(GLOBAL '(DOMAINLIST*)) 
(EXPORTS (LIST 'SIMPLOG 'SIMPLOGB 'SIMPLOGBI 'SIMPLOGBSQ 'SIMPLOGI 'SIMPLOGSQ)) 
(IMPORTS
 (LIST 'ADDF 'ADDSQ 'COMFAC 'QUOTF 'PREPF 'MKSP 'SIMP* '*MULTSQ 'SIMPTIMES
       'MINUSF 'NEGF 'NEGSQ 'MK*SQ 'CARX 'MULTSQ 'RESIMP 'SIMPIDEN 'SIMPPLUS
       'PREPD 'MKSQ 'RERROR 'ZFACTOR 'SFCHK)) 
(DE GET-LOG-BASE (U) (COND ((EQ (CAR U) 'LOG10) 10) (T NIL))) 
(PUT 'GET-LOG-BASE 'NUMBER-OF-ARGS 1) 
(PUT 'GET-LOG-BASE 'DEFINED-ON-LINE '57) 
(PUT 'GET-LOG-BASE 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'GET-LOG-BASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'GET-LOG-BASE 'INLINE
      '(LAMBDA (U) (COND ((EQ (CAR U) 'LOG10) 10) (T NIL)))) 
(PUT 'SIMPLOG 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLOG 'DEFINED-ON-LINE '60) 
(PUT 'SIMPLOG 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLOG (U)
    (COND
     ((OR (NULL (CDR U)) (CDDR U))
      (RERROR 'ALG 5 (LIST "Wrong number of arguments to" (CAR U))))
     (*EXPANDLOGS
      ((LAMBDA (*EXPANDLOGS)
         (RESIMP
          (SIMPLOGBI (REVAL1 (CADR U) NIL)
                     (COND ((EQ (CAR U) 'LOG10) 10) (T NIL)))))
       NIL))
     (T
      ((LAMBDA (X)
         (COND ((EQUAL X 0) (RERROR 'ALG 210 (LIST (CAR U) "0 formed")))
               ((EQUAL X 1) (CONS NIL 1))
               ((AND (EQ X 'E) (NOT (EQ (CAR U) 'LOG10))) (CONS 1 1))
               ((AND (FIXP X) (EQ (CAR U) 'LOG10)
                     (NOT (AND DMODE* (GET 'LOG10 DMODE*))))
                (SIMPLOGBN X (COND ((EQ (CAR U) 'LOG10) 10) (T NIL)) T))
               ((AND (EQCAR X 'EXPT)
                     (EQUAL (CADR X) (COND ((EQ (CAR U) 'LOG10) 10) (T 'E))))
                (SIMP (CADDR X)))
               ((AND (EQCAR X 'QUOTIENT) (EQUAL (CADR X) 1)
                     (OR (NULL *PRECISE) (REALVALUEDP (CADDR X))))
                (NEGSQ (SIMPIDEN (CONS (CAR U) (CDDR X)))))
               (T (SIMPIDEN U))))
       (REVAL1 (CADR U) T))))) 
(PUT 'SIMPLOGB 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLOGB 'DEFINED-ON-LINE '79) 
(PUT 'SIMPLOGB 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLOGB (U)
    (COND
     ((OR (NULL (CDR U)) (NULL (CDDR U)) (CDDDR U))
      (RERROR 'ALG 5 "Wrong number of arguments to logb"))
     (*EXPANDLOGS
      ((LAMBDA (*EXPANDLOGS) (SIMPLOGBI (REVAL1 (CADR U) NIL) (CADDR U))) NIL))
     (T
      ((LAMBDA (X)
         (COND ((EQUAL X 0) (RERROR 'ALG 210 "Logb(0,...) formed"))
               ((EQUAL X 1) (CONS NIL 1))
               ((EQUAL (REVAL1 (LIST 'DIFFERENCE X (CADDR U)) T) 0) (CONS 1 1))
               ((EQUAL (CADDR U) 'E) (SIMPLOG (LIST 'LOG X)))
               ((EQUAL (REVAL1 (LIST 'DIFFERENCE 10 (CADDR U)) T) 0)
                (SIMPLOG (LIST 'LOG10 X)))
               ((AND (FIXP X) (NOT DMODE*)) (SIMPLOGBN X (CADDR U) NIL))
               ((AND (EQCAR X 'EXPT)
                     (EQUAL (REVAL1 (LIST 'DIFFERENCE (CADR X) (CADDR U)) T)
                            0))
                (SIMP (CADDR X)))
               ((AND (EQCAR X 'QUOTIENT) (EQUAL (CADR X) 1)
                     (OR (NULL *PRECISE) (REALVALUEDP (CADDR X))))
                (NEGSQ (SIMPIDEN (LIST (CAR U) (CADDR X) (CADDR U)))))
               (T (SIMPIDEN U))))
       (REVAL1 (CADR U) T))))) 
(PUT 'LOG 'SIMPFN 'SIMPLOG) 
(PUT 'LOG10 'SIMPFN 'SIMPLOG) 
(PUT 'LOGB 'SIMPFN 'SIMPLOGB) 
(FLAG '(LOG LOG10 LOGB) 'FULL) 
(PUT 'EXPANDLOGS 'SIMPFG '((NIL (RMSUBS)) (T (RMSUBS)))) 
(PUT 'COMBINELOGS 'SIMPFG '((NIL (RMSUBS)) (T (RMSUBS)))) 
(DE MK-LOG-ARG (ARG BASE)
    (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG ARG))
          ((EQUAL BASE 10) (LIST 'LOG10 ARG)) (T (LIST 'LOGB ARG BASE)))) 
(PUT 'MK-LOG-ARG 'NUMBER-OF-ARGS 2) 
(PUT 'MK-LOG-ARG 'DEFINED-ON-LINE '113) 
(PUT 'MK-LOG-ARG 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'MK-LOG-ARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'MK-LOG-ARG 'INLINE
      '(LAMBDA (ARG BASE)
         (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG ARG))
               ((EQUAL BASE 10) (LIST 'LOG10 ARG)) (T (LIST 'LOGB ARG BASE))))) 
(PUT 'SIMPLOGI 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLOGI 'DEFINED-ON-LINE '118) 
(PUT 'SIMPLOGI 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLOGI (SQ) (SIMPLOGBI SQ NIL)) 
(PUT 'SIMPLOGBI 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPLOGBI 'DEFINED-ON-LINE '121) 
(PUT 'SIMPLOGBI 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGBI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPLOGBI (SQ BASE)
    (COND ((ATOM SQ) (SIMPLOGBSQ (SIMP* SQ) BASE))
          ((MEMQ (CAR SQ) DOMAINLIST*)
           (SIMPIDEN
            (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG SQ))
                  ((EQUAL BASE 10) (LIST 'LOG10 SQ))
                  (T (LIST 'LOGB SQ BASE)))))
          ((EQ (CAR SQ) 'TIMES)
           (COND
            ((OR (NULL *PRECISE) (ONE_COMPLEXLIST (CDR SQ)))
             (SIMPPLUS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U (CDR SQ))
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (MK*SQ (SIMPLOGBI U BASE)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (MK*SQ (SIMPLOGBI U BASE))) (CAR U))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))
            (T
             (CONS
              (LIST
               (CONS
                (MKSP
                 (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG SQ))
                       ((EQUAL BASE 10) (LIST 'LOG10 SQ))
                       (T (LIST 'LOGB SQ BASE)))
                 1)
                1))
              1))))
          ((AND (EQ (CAR SQ) 'QUOTIENT)
                (OR (NULL *PRECISE) (ONE_COMPLEXLIST (CDR SQ))))
           (ADDSQ (SIMPLOGBI (CADR SQ) BASE)
                  (NEGSQ (SIMPLOGBI (CADDR SQ) BASE))))
          ((EQ (CAR SQ) 'EXPT)
           (SIMPTIMES (LIST (CADDR SQ) (MK*SQ (SIMPLOGBI (CADR SQ) BASE)))))
          ((EQ (CAR SQ) 'NTHROOT)
           (MULTSQ* (CONS 1 (CADDR SQ)) (SIMPLOGBI (CADR SQ) BASE)))
          ((EQ (CAR SQ) 'SQRT) (MULTSQ* (CONS 1 2) (SIMPLOGBI (CADR SQ) BASE)))
          ((EQUAL (CAR SQ) '*SQ) (SIMPLOGBSQ (CADR SQ) BASE))
          (T (SIMPLOGBSQ (SIMP* SQ) BASE)))) 
(PUT 'ONE_COMPLEXLIST 'NUMBER-OF-ARGS 1) 
(PUT 'ONE_COMPLEXLIST 'DEFINED-ON-LINE '143) 
(PUT 'ONE_COMPLEXLIST 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'ONE_COMPLEXLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ONE_COMPLEXLIST (U)
    (COND ((NULL U) T) ((REALVALUEDP (CAR U)) (ONE_COMPLEXLIST (CDR U)))
          (T (OR (NULL (CDR U)) (REALVALUEDLIST (CDR U)))))) 
(PUT 'MULTSQ* 'NUMBER-OF-ARGS 2) 
(PUT 'MULTSQ* 'DEFINED-ON-LINE '149) 
(PUT 'MULTSQ* 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'MULTSQ* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTSQ* (U V) (COND (*INTFLAG* (*MULTSQ U V)) (T (MULTSQ U V)))) 
(PUT 'SIMPLOGSQ 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLOGSQ 'DEFINED-ON-LINE '152) 
(PUT 'SIMPLOGSQ 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLOGSQ (SQ) (SIMPLOGBSQ SQ NIL)) 
(PUT 'SIMPLOGBSQ 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPLOGBSQ 'DEFINED-ON-LINE '155) 
(PUT 'SIMPLOGBSQ 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPLOGBSQ (SQ BASE)
    (COND ((NULL (CAR SQ)) (RERROR 'ALG 210 "Log 0 formed"))
          (T
           (PROG (N)
             (SETQ N 0)
             (COND
              ((AND (EQUAL (CDR SQ) 1)
                    (OR (ATOM (CAR SQ)) (ATOM (CAR (CAR SQ)))))
               (PROGN
                (COND ((|:ONEP| (CAR SQ)) (RETURN (CONS NIL 1)))
                      ((AND (SETQ N (INT-EQUIV-CHK (CAR SQ))) (FIXP N))
                       (RETURN (SIMPLOGBN N BASE NIL)))
                      ((AND (EQCAR (CAR SQ) '|:RN:|)
                            (NOT (|:MINUSP| (CAR SQ))))
                       (RETURN
                        (ADDSQ (SIMPLOGB2 (CADR (CAR SQ)) BASE)
                               (NEGSQ (SIMPLOGB2 (CDDR (CAR SQ)) BASE))))))))
              ((AND (FIXP (CDR SQ)) (OR (ATOM (CAR SQ)) (ATOM (CAR (CAR SQ)))))
               (PROGN
                (COND
                 ((AND (SETQ N (INT-EQUIV-CHK (CAR SQ))) (FIXP N))
                  (RETURN
                   (ADDSQ (SIMPLOGBN N BASE NIL)
                          (NEGSQ (SIMPLOGBN (CDR SQ) BASE NIL)))))))))
             (COND
              ((AND *PRECISE (NOT (REALVALUEDP-SF (CDR SQ)))
                    (NOT (REALVALUEDP-SF (CAR SQ))))
               (RETURN
                (CONS
                 (LIST
                  (CONS
                   (MKSP
                    ((LAMBDA (G580)
                       (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G580))
                             ((EQUAL BASE 10) (LIST 'LOG10 G580))
                             (T (LIST 'LOGB G580 BASE))))
                     (PREPSQ SQ))
                    1)
                   1))
                 1)))
              (T
               (RETURN
                (ADDSQ (SIMPLOGB2 (CAR SQ) BASE)
                       (NEGSQ (SIMPLOGB2 (CDR SQ) BASE)))))))))) 
(PUT 'SIMPLOGB2 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPLOGB2 'DEFINED-ON-LINE '175) 
(PUT 'SIMPLOGB2 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGB2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPLOGB2 (SF BASE)
    (COND
     ((ATOM SF)
      (COND ((NULL SF) (RERROR 'ALG 21 "Log 0 formed"))
            ((NUMBERP SF)
             (COND ((IEQUAL SF 1) (CONS NIL 1))
                   ((IEQUAL SF 0) (RERROR 'ALG 22 "Log 0 formed"))
                   (T (SIMPLOGBN SF BASE NIL))))
            (T (FORMLOG SF BASE))))
     ((OR (ATOM SF) (ATOM (CAR SF)))
      (MKSQ
       ((LAMBDA (G582)
          (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G582))
                ((EQUAL BASE 10) (LIST 'LOG10 G582))
                (T (LIST 'LOGB G582 BASE))))
        (PREPD SF))
       1))
     (T
      (PROG (FORM)
        (SETQ FORM (COMFAC SF))
        (COND
         ((NOT (NULL (CAR FORM)))
          (COND
           ((OR (NULL *PRECISE) (REALVALUEDP-SF (CONS FORM NIL)))
            (RETURN
             (ADDSQ (FORMLOG (CONS FORM NIL) BASE)
                    (SIMPLOGB2 ((LAMBDA (*EXP) (QUOTF1 SF (CONS FORM NIL))) T)
                               BASE))))
           (T (RETURN (FORMLOG SF BASE))))))
        (SETQ FORM (CDR FORM))
        (COND
         ((NEQ FORM 1)
          (RETURN
           (ADDSQ (SIMPLOGB2 FORM BASE)
                  (SIMPLOGB2 ((LAMBDA (*EXP) (QUOTF1 SF FORM)) T) BASE)))))
        (RETURN (FORMLOG SF BASE)))))) 
(PUT 'SIMPLOGN 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLOGN 'DEFINED-ON-LINE '201) 
(PUT 'SIMPLOGN 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLOGN (U) (SIMPLOGBN U NIL NIL)) 
(PUT 'SIMPLOGBN 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLOGBN 'DEFINED-ON-LINE '210) 
(PUT 'SIMPLOGBN 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'SIMPLOGBN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLOGBN (U BASE FLG)
    (COND ((EQUAL U 1) (CONS NIL 1))
          (T
           (PROG (Y Z)
             (SETQ Y (ZFACTOR U))
             (COND
              ((EQUAL BASE 10)
               (PROG (TWOS FIVES)
                 (SETQ TWOS 0)
                 (SETQ FIVES 0)
                 (SETQ TWOS (ASSOC 2 Y))
                 (SETQ FIVES (ASSOC 5 Y))
                 (COND
                  ((AND TWOS FIVES)
                   (PROGN
                    (SETQ Y (DELETE TWOS Y))
                    (SETQ Y (DELETE FIVES Y))
                    (COND ((EQUAL (CDR TWOS) (CDR FIVES)) (SETQ Z (CDR TWOS)))
                          ((LESSP (CDR TWOS) (CDR FIVES))
                           (PROGN
                            (SETQ Z (CDR TWOS))
                            (SETQ Y
                                    (APPEND Y
                                            (LIST
                                             (CONS 5
                                                   (DIFFERENCE (CDR FIVES)
                                                               (CDR
                                                                TWOS))))))))
                          (T
                           (PROGN
                            (SETQ Z (CDR FIVES))
                            (SETQ Y
                                    (APPEND Y
                                            (LIST
                                             (CONS 2
                                                   (DIFFERENCE (CDR TWOS)
                                                               (CDR
                                                                FIVES)))))))))))))))
             (COND
              (FLG
               (RETURN
                (CONS
                 (COND ((NULL Y) Z)
                       (T
                        (LIST
                         (CONS
                          (MKSP
                           (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG U))
                                 ((EQUAL BASE 10) (LIST 'LOG10 U))
                                 (T (LIST 'LOGB U BASE)))
                           1)
                          1))))
                 1))))
             (COND
              ((AND (NOT (ATOM Y)) (EQUAL (CAR Y) '(-1 . 1))
                    (NULL (SETQ Y (MERGEMINUS (CDR Y)))))
               (RETURN
                (CONS
                 (LIST
                  (CONS
                   (MKSP
                    (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG U))
                          ((EQUAL BASE 10) (LIST 'LOG10 U))
                          (T (LIST 'LOGB U BASE)))
                    1)
                   1))
                 1))))
             (PROG (X)
               (SETQ X Y)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (COND ((EQUAL (CAR X) BASE) (SETQ Z (ADDF (CDR X) Z)))
                        (T
                         (SETQ Z
                                 (ADDF
                                  (CONS
                                   (CONS
                                    (GETPOWER
                                     (FKERN
                                      (COND
                                       ((OR (NULL BASE) (EQ BASE 'E))
                                        (LIST 'LOG (CAR X)))
                                       ((EQUAL BASE 10) (LIST 'LOG10 (CAR X)))
                                       (T (LIST 'LOGB (CAR X) BASE))))
                                     1)
                                    (CDR X))
                                   NIL)
                                  Z)))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN (CONS Z 1)))))) 
(PUT 'MERGEMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'MERGEMINUS 'DEFINED-ON-LINE '237) 
(PUT 'MERGEMINUS 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'MERGEMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MERGEMINUS (U)
    (PROG (X)
     A
      (COND ((NULL U) (RETURN NIL))
            ((EQUAL (REMAINDER (CDAR U) 2) 1)
             (RETURN
              (REVERSIP2 X (CONS (CONS (MINUS (CAAR U)) (CDAR U)) (CDR U)))))
            (T (PROGN (SETQ X (CONS (CAR U) X)) (SETQ U (CDR U)) (GO A)))))) 
(PUT 'FORMLOG 'NUMBER-OF-ARGS 2) 
(PUT 'FORMLOG 'DEFINED-ON-LINE '245) 
(PUT 'FORMLOG 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'FORMLOG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMLOG (SF BASE)
    (COND ((NULL (CDR SF)) (FORMLOGTERM SF BASE))
          (T (CONS (FORMLOG2 SF BASE) 1)))) 
(PUT 'FORMLOGTERM 'NUMBER-OF-ARGS 2) 
(PUT 'FORMLOGTERM 'DEFINED-ON-LINE '256) 
(PUT 'FORMLOGTERM 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'FORMLOGTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMLOGTERM (SF BASE)
    (PROG (U)
      (SETQ U (CAAAR SF))
      (COND
       ((AND (NOT (ATOM U)) (MEMBER (CAR U) '(TIMES SQRT EXPT NTHROOT)))
        (SETQ U
                (ADDSQ (SIMPLOGB2 (CDAR SF) BASE)
                       (MULTSQ* (SIMPLOGBI U BASE) (SIMP* (CDAAR SF))))))
       ((AND (IEQUAL (CDAR SF) 1) (IEQUAL (CDAAR SF) 1))
        (SETQ U
                (CONS
                 (CONS
                  (CONS
                   (GETPOWER
                    (FKERN
                     ((LAMBDA (G584)
                        (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G584))
                              ((EQUAL BASE 10) (LIST 'LOG10 G584))
                              (T (LIST 'LOGB G584 BASE))))
                      (SFCHK U)))
                    1)
                   1)
                  NIL)
                 1)))
       ((AND (OR (ATOM (CDAR SF)) (ATOM (CAR (CDAR SF))))
             (|:MINUSP| (CDAR SF)))
        (SETQ U
                (CONS
                 (CONS
                  (CONS
                   (GETPOWER
                    (FKERN
                     ((LAMBDA (G586)
                        (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G586))
                              ((EQUAL BASE 10) (LIST 'LOG10 G586))
                              (T (LIST 'LOGB G586 BASE))))
                      (PREPF SF)))
                    1)
                   1)
                  NIL)
                 1)))
       ((AND *PRECISE
             (NOT (COND ((SFP U) (REALVALUEDP-SF U)) (T (REALVALUEDP U))))
             (NOT (REALVALUEDP-SF (CDAR SF))))
        (SETQ U
                (CONS
                 (CONS
                  (CONS
                   (GETPOWER
                    (FKERN
                     ((LAMBDA (G588)
                        (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G588))
                              ((EQUAL BASE 10) (LIST 'LOG10 G588))
                              (T (LIST 'LOGB G588 BASE))))
                      (PREPF SF)))
                    1)
                   1)
                  NIL)
                 1)))
       (T
        (SETQ U
                (ADDSQ
                 (SIMPTIMES
                  (LIST
                   ((LAMBDA (G590)
                      (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G590))
                            ((EQUAL BASE 10) (LIST 'LOG10 G590))
                            (T (LIST 'LOGB G590 BASE))))
                    (SFCHK U))
                   (CDAAR SF)))
                 (SIMPLOGB2 (CDAR SF) BASE)))))
      (RETURN U))) 
(PUT 'FORMLOG2 'NUMBER-OF-ARGS 2) 
(PUT 'FORMLOG2 'DEFINED-ON-LINE '278) 
(PUT 'FORMLOG2 'DEFINED-IN-FILE 'ALG/SIMPLOG.RED) 
(PUT 'FORMLOG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMLOG2 (SF BASE)
    (CONS
     (CONS
      (GETPOWER
       (FKERN
        ((LAMBDA (G592)
           (COND ((OR (NULL BASE) (EQ BASE 'E)) (LIST 'LOG G592))
                 ((EQUAL BASE 10) (LIST 'LOG10 G592))
                 (T (LIST 'LOGB G592 BASE))))
         (PREPF SF)))
       1)
      1)
     NIL)) 
(ENDMODULE) 