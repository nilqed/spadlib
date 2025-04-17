(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYFRONT)) 
(EXPORTS
 (LIST 'TAYLORCOMBINE 'TAYLORORIGINAL 'TAYLORPRINTORDER 'TAYLORSERIESP
       'TAYLORTEMPLATE 'TAYLORTOSTANDARD)) 
(IMPORTS
 (LIST 'EQCAR 'MK*SQ 'MVAR 'NUMR 'PREPSQ 'SIMP* 'TYPERR 'TAYLOR-KERNEL-SQ-P
       'TAYORIG 'TAYTEMPLATE 'TAYTPELORDER 'TAYTPELPOINT 'TAYTPELVARS
       'TAYLOR-ERROR 'TAYSIMPSQ)) 
(PUT 'TAYLORSERIESP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORSERIESP 'DEFINED-ON-LINE '54) 
(PUT 'TAYLORSERIESP 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORSERIESP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORSERIESP (U)
    ((LAMBDA (SQ) (AND (KERNP SQ) (EQCAR (CAAAR (CAR SQ)) 'TAYLOR*)))
     (SIMP* U))) 
(PUT 'TAYLORCOMBINE 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORCOMBINE 'DEFINED-ON-LINE '58) 
(PUT 'TAYLORCOMBINE 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORCOMBINE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORCOMBINE (U) (MK*SQ (TAYSIMPSQ (SIMP* U)))) 
(PUT 'TAYLORTOSTANDARD 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORTOSTANDARD 'DEFINED-ON-LINE '61) 
(PUT 'TAYLORTOSTANDARD 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORTOSTANDARD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORTOSTANDARD (U)
    ((LAMBDA (CONVERT-TAYLOR* ALGLIST*) (MK*SQ (RESIMP (SIMP* U)))) T
     (CONS NIL NIL))) 
(PUT 'TAYLORORIGINAL 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORORIGINAL 'DEFINED-ON-LINE '65) 
(PUT 'TAYLORORIGINAL 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORORIGINAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORORIGINAL (U)
    ((LAMBDA (SQ)
       (COND
        ((NOT (AND (KERNP SQ) (EQCAR (CAAAR (CAR SQ)) 'TAYLOR*)))
         (TYPERR U "Taylor kernel"))
        (T
         ((LAMBDA (TAY)
            (COND ((CADDDR TAY) (MK*SQ (CADDDR TAY)))
                  (T (TAYLOR-ERROR 'NO-ORIGINAL 'TAYLORORIGINAL))))
          (CAAAR (CAR SQ))))))
     (SIMP* U))) 
(PUT 'TAYLORTEMPLATE 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORTEMPLATE 'DEFINED-ON-LINE '73) 
(PUT 'TAYLORTEMPLATE 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORTEMPLATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORTEMPLATE (U)
    ((LAMBDA (SQ)
       (COND
        ((NOT (AND (KERNP SQ) (EQCAR (CAAAR (CAR SQ)) 'TAYLOR*)))
         (TYPERR U "Taylor kernel"))
        (T
         (CONS 'LIST
               (PROG (QUARTET FORALL-RESULT FORALL-ENDPTR)
                 (SETQ QUARTET (CADDR (CAAAR (CAR SQ))))
                 (COND ((NULL QUARTET) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (QUARTET)
                                     (LIST 'LIST
                                           (COND
                                            ((NULL (CDR (CAR QUARTET)))
                                             (CAR (CAR QUARTET)))
                                            (T (CONS 'LIST (CAR QUARTET))))
                                           (CADR QUARTET) (CADDR QUARTET)))
                                   (CAR QUARTET))
                                  NIL)))
                LOOPLABEL
                 (SETQ QUARTET (CDR QUARTET))
                 (COND ((NULL QUARTET) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (QUARTET)
                             (LIST 'LIST
                                   (COND
                                    ((NULL (CDR (CAR QUARTET)))
                                     (CAR (CAR QUARTET)))
                                    (T (CONS 'LIST (CAR QUARTET))))
                                   (CADR QUARTET) (CADDR QUARTET)))
                           (CAR QUARTET))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
     (SIMP* U))) 
(PUT 'TAYLORCOEFFLIST 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLORCOEFFLIST 'DEFINED-ON-LINE '85) 
(PUT 'TAYLORCOEFFLIST 'DEFINED-IN-FILE 'TAYLOR/TAYFRONT.RED) 
(PUT 'TAYLORCOEFFLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORCOEFFLIST (U)
    ((LAMBDA (SQ)
       (COND
        ((NOT (AND (KERNP SQ) (EQCAR (CAAAR (CAR SQ)) 'TAYLOR*)))
         (TYPERR U "Taylor kernel"))
        (T
         (CONS 'LIST
               (PROG (CF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ CF (CADR (CAAAR (CAR SQ))))
                 (COND ((NULL CF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (CF)
                                     (LIST 'LIST
                                           (CONS 'LIST
                                                 (PROG (LL FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ LL (CAR CF))
                                                   (COND
                                                    ((NULL LL) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (LL)
                                                                       (COND
                                                                        ((NULL
                                                                          (CDR
                                                                           LL))
                                                                         (PREPTAYEXP
                                                                          (CAR
                                                                           LL)))
                                                                        (T
                                                                         (CONS
                                                                          'LIST
                                                                          (PROG (N
                                                                                 FORALL-RESULT
                                                                                 FORALL-ENDPTR)
                                                                            (SETQ N
                                                                                    LL)
                                                                            (COND
                                                                             ((NULL
                                                                               N)
                                                                              (RETURN
                                                                               NIL)))
                                                                            (SETQ FORALL-RESULT
                                                                                    (SETQ FORALL-ENDPTR
                                                                                            (CONS
                                                                                             ((LAMBDA
                                                                                                  (
                                                                                                   N)
                                                                                                (PREPTAYEXP
                                                                                                 N))
                                                                                              (CAR
                                                                                               N))
                                                                                             NIL)))
                                                                           LOOPLABEL
                                                                            (SETQ N
                                                                                    (CDR
                                                                                     N))
                                                                            (COND
                                                                             ((NULL
                                                                               N)
                                                                              (RETURN
                                                                               FORALL-RESULT)))
                                                                            (RPLACD
                                                                             FORALL-ENDPTR
                                                                             (CONS
                                                                              ((LAMBDA
                                                                                   (
                                                                                    N)
                                                                                 (PREPTAYEXP
                                                                                  N))
                                                                               (CAR
                                                                                N))
                                                                              NIL))
                                                                            (SETQ FORALL-ENDPTR
                                                                                    (CDR
                                                                                     FORALL-ENDPTR))
                                                                            (GO
                                                                             LOOPLABEL))))))
                                                                     (CAR LL))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ LL (CDR LL))
                                                   (COND
                                                    ((NULL LL)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (LL)
                                                               (COND
                                                                ((NULL
                                                                  (CDR LL))
                                                                 (PREPTAYEXP
                                                                  (CAR LL)))
                                                                (T
                                                                 (CONS 'LIST
                                                                       (PROG (N
                                                                              FORALL-RESULT
                                                                              FORALL-ENDPTR)
                                                                         (SETQ N
                                                                                 LL)
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            NIL)))
                                                                         (SETQ FORALL-RESULT
                                                                                 (SETQ FORALL-ENDPTR
                                                                                         (CONS
                                                                                          ((LAMBDA
                                                                                               (
                                                                                                N)
                                                                                             (PREPTAYEXP
                                                                                              N))
                                                                                           (CAR
                                                                                            N))
                                                                                          NIL)))
                                                                        LOOPLABEL
                                                                         (SETQ N
                                                                                 (CDR
                                                                                  N))
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            FORALL-RESULT)))
                                                                         (RPLACD
                                                                          FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 N)
                                                                              (PREPTAYEXP
                                                                               N))
                                                                            (CAR
                                                                             N))
                                                                           NIL))
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CDR
                                                                                  FORALL-ENDPTR))
                                                                         (GO
                                                                          LOOPLABEL))))))
                                                             (CAR LL))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                           (MK*SQ (CDR CF))))
                                   (CAR CF))
                                  NIL)))
                LOOPLABEL
                 (SETQ CF (CDR CF))
                 (COND ((NULL CF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (CF)
                             (LIST 'LIST
                                   (CONS 'LIST
                                         (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ LL (CAR CF))
                                           (COND ((NULL LL) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (LL)
                                                               (COND
                                                                ((NULL
                                                                  (CDR LL))
                                                                 (PREPTAYEXP
                                                                  (CAR LL)))
                                                                (T
                                                                 (CONS 'LIST
                                                                       (PROG (N
                                                                              FORALL-RESULT
                                                                              FORALL-ENDPTR)
                                                                         (SETQ N
                                                                                 LL)
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            NIL)))
                                                                         (SETQ FORALL-RESULT
                                                                                 (SETQ FORALL-ENDPTR
                                                                                         (CONS
                                                                                          ((LAMBDA
                                                                                               (
                                                                                                N)
                                                                                             (PREPTAYEXP
                                                                                              N))
                                                                                           (CAR
                                                                                            N))
                                                                                          NIL)))
                                                                        LOOPLABEL
                                                                         (SETQ N
                                                                                 (CDR
                                                                                  N))
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            FORALL-RESULT)))
                                                                         (RPLACD
                                                                          FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 N)
                                                                              (PREPTAYEXP
                                                                               N))
                                                                            (CAR
                                                                             N))
                                                                           NIL))
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CDR
                                                                                  FORALL-ENDPTR))
                                                                         (GO
                                                                          LOOPLABEL))))))
                                                             (CAR LL))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ LL (CDR LL))
                                           (COND
                                            ((NULL LL) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (LL)
                                                       (COND
                                                        ((NULL (CDR LL))
                                                         (PREPTAYEXP (CAR LL)))
                                                        (T
                                                         (CONS 'LIST
                                                               (PROG (N
                                                                      FORALL-RESULT
                                                                      FORALL-ENDPTR)
                                                                 (SETQ N LL)
                                                                 (COND
                                                                  ((NULL N)
                                                                   (RETURN
                                                                    NIL)))
                                                                 (SETQ FORALL-RESULT
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CONS
                                                                                  ((LAMBDA
                                                                                       (
                                                                                        N)
                                                                                     (PREPTAYEXP
                                                                                      N))
                                                                                   (CAR
                                                                                    N))
                                                                                  NIL)))
                                                                LOOPLABEL
                                                                 (SETQ N
                                                                         (CDR
                                                                          N))
                                                                 (COND
                                                                  ((NULL N)
                                                                   (RETURN
                                                                    FORALL-RESULT)))
                                                                 (RPLACD
                                                                  FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (N)
                                                                      (PREPTAYEXP
                                                                       N))
                                                                    (CAR N))
                                                                   NIL))
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CDR
                                                                          FORALL-ENDPTR))
                                                                 (GO
                                                                  LOOPLABEL))))))
                                                     (CAR LL))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                   (MK*SQ (CDR CF))))
                           (CAR CF))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
     (SIMP* U))) 
(FLAG
 '(TAYLORSERIESP TAYLORCOMBINE TAYLORTOSTANDARD TAYLORORIGINAL TAYLORTEMPLATE
   TAYLORCOEFFLIST)
 'OPFN) 
(FLAG '(TAYLORSERIESP) 'BOOLEAN) 
(ENDMODULE) 