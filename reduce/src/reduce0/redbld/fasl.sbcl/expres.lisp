(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EXPRES)) 
(GLOBAL '(*OUTP)) 
(FLUID '(*WRCHRI ORIG* POSN*)) 
(SWITCH (LIST 'WRCHRI)) 
(GLOBAL '(OLDDIMENSION* DIMENSION* COORDINDX* CYCLIC1* CYCLIC2* SFPROD* NSCAL*)) 
(FLAG '(SHARE) 'EVAL) 
(SHARE
 (LIST 'OLDDIMENSION* 'DIMENSION* 'COORDINDX* 'CYCLIC1* 'CYCLIC2* 'SFPROD*
       'NSCAL*)) 
(SETQ *PRECISE NIL) 
(SETQ NSCAL* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 0)) 
(PUT 'TENSOR 'TAG 'TENS) 
(PUT 'TENSOR 'FN 'TENSFN) 
(PUT 'TENSOR 'EVFN 'EXPRES) 
(PUT 'TENS 'PRIFN 'TENSPRI) 
(FLAG '(TENSOR) 'SPRIFN) 
(PUT 'TENS 'SETPRIFN 'SETTENSPRI) 
(PUT 'TENSOR 'TYPELETFN 'TENSLET) 
(PUT 'PTENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'PTENSOR 'DEFINED-ON-LINE '57) 
(PUT 'PTENSOR 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'PTENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PTENSOR (X) 'TENSOR) 
(PUT 'POPTENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'POPTENSOR 'DEFINED-ON-LINE '60) 
(PUT 'POPTENSOR 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'POPTENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POPTENSOR (U) (COND ((NULL U) 'TENSOR) (T NIL))) 
(DEFLIST
 '((TENSOR PTENSOR) (TENSOP POPTENSOR) (DF GETRTYPECAR) (DIFF GETRTYPECAR)
   (& PTENSOR) (|#| PTENSOR) (? PTENSOR) (GRAD PTENSOR) (DIV PTENSOR)
   (LAPL PTENSOR) (CURL PTENSOR) (VECT PTENSOR) (DYAD PTENSOR) (DIRDF PTENSOR))
 'RTYPEFN) 
(PUT 'CONS 'RTYPEFN 'GETRTYPECONS) 
(PUT 'RCONS 'EVFN 'EVRCONS) 
(REMPROP 'CONS 'PSOPFN) 
(PUT 'GETRTYPECONS 'NUMBER-OF-ARGS 1) 
(PUT 'GETRTYPECONS 'DEFINED-ON-LINE '81) 
(PUT 'GETRTYPECONS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'GETRTYPECONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETRTYPECONS (U)
    (COND ((EQ (GETRTYPE (CAR U)) 'TENSOR) 'TENSOR) (T 'RCONS))) 
(PUT 'EVRCONS 'NUMBER-OF-ARGS 2) 
(PUT 'EVRCONS 'DEFINED-ON-LINE '85) 
(PUT 'EVRCONS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'EVRCONS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVRCONS (U V) (RCONS (CDR U))) 
(PUT 'TENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'TENSOR 'DEFINED-ON-LINE '88) 
(PUT 'TENSOR 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENSOR (U)
    (PROG (A)
      (SETQ A U)
     LAB
      (COND ((NULL A) (RETURN NIL)))
      ((LAMBDA (A)
         (PROGN
          (PUT A 'RTYPE 'TENSOP)
          (PUT A 'AVALUE (LIST 'TENSOR (MKTENSOR 0 (CONS 0 1))))))
       (CAR A))
      (SETQ A (CDR A))
      (GO LAB))) 
(DEFLIST '((TENSOR RLIS)) 'STAT) 
(PUT 'TENSLET 'NUMBER-OF-ARGS 5) 
(PUT 'TENSLET 'DEFINED-ON-LINE '95) 
(PUT 'TENSLET 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSLET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TENSLET (U V TYPU B TYPV)
    (COND ((NOT (ATOM U)) (LPRIE (LIST " Non atom tensor variable " U)))
          (B
           (PROGN
            (COND
             ((NOT (EQCAR V 'TENSOR))
              (SETQ V
                      (MKTENSOR 0
                       (COND ((EQCAR V '*SQ) (CADR V)) (T (SIMP* V)))))))
            (RMSUBS)
            (PUT U 'RTYPE 'TENSOP)
            (PUT U 'AVALUE (LIST 'TENSOR V))))
          (T (PROGN (REMPROP U 'RTYPE) (REMPROP U 'AVALUE))))) 
(DE TENSRNK (U) (CAR U)) 
(PUT 'TENSRNK 'NUMBER-OF-ARGS 1) 
(PUT 'TENSRNK 'DEFINED-ON-LINE '128) 
(PUT 'TENSRNK 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSRNK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'TENSRNK 'INLINE '(LAMBDA (U) (CAR U))) 
(DE TENSVAL (U) (CADR U)) 
(PUT 'TENSVAL 'NUMBER-OF-ARGS 1) 
(PUT 'TENSVAL 'DEFINED-ON-LINE '132) 
(PUT 'TENSVAL 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'TENSVAL 'INLINE '(LAMBDA (U) (CADR U))) 
(PUT 'MKTENSOR 'NUMBER-OF-ARGS 2) 
(PUT 'MKTENSOR 'DEFINED-ON-LINE '136) 
(PUT 'MKTENSOR 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'MKTENSOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKTENSOR (RNK U)
    (CONS 'TENSOR
          (CONS NSCAL*
                (CONS RNK (CONS U (COND (*RESUBS *SQVAR*) (T (LIST NIL)))))))) 
(PUT 'SETTENSPRI 'NUMBER-OF-ARGS 2) 
(PUT 'SETTENSPRI 'DEFINED-ON-LINE '139) 
(PUT 'SETTENSPRI 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'SETTENSPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETTENSPRI (U V)
    (COND ((NOT (ATOM U)) (LPRIE (LIST " Non-atom tensor variable " U)))
          (T (PROGN (PRIN2* U) (PRIN2* (GET 'SETQ 'PRTCH)) (TENSPRI V))))) 
(PUT 'TENSPRI 'NUMBER-OF-ARGS 1) 
(PUT 'TENSPRI 'DEFINED-ON-LINE '145) 
(PUT 'TENSPRI 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENSPRI (U)
    (PROG (RNK)
      (SETQ U (CDDR U))
      (SETQ RNK (CAR U))
      (SETQ U (CADR U))
      (COND ((EQUAL RNK 0) (PROGN (PMAPRIN U) (TERPRI* T)))
            ((EQUAL RNK 1) (PROGN (TENSPRI1 U) (SETQ ORIG* 0) (TERPRI* T)))
            ((EQUAL RNK 2)
             (PROGN
              (PRIN2* " ( ")
              (TENSPRI1 (CAR U))
              (PROG (I)
                (SETQ I (CDR U))
               LAB
                (COND ((NULL I) (RETURN NIL)))
                ((LAMBDA (I) (PROGN (PRIN2* " , ") (TERPRI* T) (TENSPRI1 I)))
                 (CAR I))
                (SETQ I (CDR I))
                (GO LAB))
              (PRIN2* " ) ")
              (SETQ ORIG* 0)
              (TERPRI* T)))
            (T (LPRIE (LIST " Can't print tensor " U " with rank " RNK)))))) 
(PUT 'TENSPRI1 'NUMBER-OF-ARGS 1) 
(PUT 'TENSPRI1 'DEFINED-ON-LINE '171) 
(PUT 'TENSPRI1 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TENSPRI1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENSPRI1 (U)
    (PROGN
     (PRIN2* " ( ")
     (SETQ ORIG* POSN*)
     (PMAPRIN (CAR U))
     (PROG (I)
       (SETQ I (CDR U))
      LAB
       (COND ((NULL I) (RETURN NIL)))
       ((LAMBDA (I) (PROGN (PRIN2* " , ") (TERPRI* T) (PMAPRIN I))) (CAR I))
       (SETQ I (CDR I))
       (GO LAB))
     (SETQ ORIG* (DIFFERENCE ORIG* 3))
     (PRIN2* " ) "))) 
(PUT 'PMAPRIN 'NUMBER-OF-ARGS 1) 
(PUT 'PMAPRIN 'DEFINED-ON-LINE '182) 
(PUT 'PMAPRIN 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'PMAPRIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PMAPRIN (U) (MAPRIN (SETQ *OUTP (PREPSQ* U)))) 
(PUT 'UPDATEDIMEN 'NUMBER-OF-ARGS 0) 
(PUT 'UPDATEDIMEN 'DEFINED-ON-LINE '185) 
(PUT 'UPDATEDIMEN 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'UPDATEDIMEN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE UPDATEDIMEN NIL
    (COND ((EQUAL OLDDIMENSION* DIMENSION*) T)
          (T
           (PROGN
            (COND
             ((EQUAL DIMENSION* 2)
              (PROGN
               (SETQ COORDINDX* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(2 1)))
               (SETQ CYCLIC1* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(1 2)))
               (SETQ CYCLIC2* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(2 1)))))
             ((EQUAL DIMENSION* 3)
              (PROGN
               (SETQ COORDINDX*
                       (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(3 2 1)))
               (SETQ CYCLIC1* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(2 3 1)))
               (SETQ CYCLIC2*
                       (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(3 1 2)))))
             (T (LPRIE (LIST " Can't handle dimension = " DIMENSION*))))
            (SETQ OLDDIMENSION*
                    (PROGN (SETQ ALGLIST* (CONS NIL NIL)) DIMENSION*)))))) 
(PUT 'EXPRES 'NUMBER-OF-ARGS 2) 
(PUT 'EXPRES 'DEFINED-ON-LINE '198) 
(PUT 'EXPRES 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'EXPRES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPRES (EXPN V) (EXPRESS EXPN)) 
(PUT 'RESIMPTENS 'NUMBER-OF-ARGS 1) 
(PUT 'RESIMPTENS 'DEFINED-ON-LINE '201) 
(PUT 'RESIMPTENS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'RESIMPTENS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESIMPTENS (U)
    (MKTENSOR (CADDR U)
     (COND ((EQUAL (CADDR U) 0) (RESIMP (CADDDR U)))
           ((EQUAL (CADDR U) 1)
            (PROG (A FORALL-RESULT FORALL-ENDPTR)
              (SETQ A (CADDDR U))
              (COND ((NULL A) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (A) (RESIMP A)) (CAR A)) NIL)))
             LOOPLABEL
              (SETQ A (CDR A))
              (COND ((NULL A) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (A) (RESIMP A)) (CAR A)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           ((EQUAL (CADDR U) 2)
            (PROG (A FORALL-RESULT FORALL-ENDPTR)
              (SETQ A (CADDDR U))
              (COND ((NULL A) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (A)
                                  (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ B A)
                                    (COND ((NULL B) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (B) (RESIMP B))
                                                      (CAR B))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ B (CDR B))
                                    (COND ((NULL B) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (B) (RESIMP B)) (CAR B))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR A))
                               NIL)))
             LOOPLABEL
              (SETQ A (CDR A))
              (COND ((NULL A) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (A)
                          (PROG (B FORALL-RESULT FORALL-ENDPTR)
                            (SETQ B A)
                            (COND ((NULL B) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (B) (RESIMP B)) (CAR B))
                                             NIL)))
                           LOOPLABEL
                            (SETQ B (CDR B))
                            (COND ((NULL B) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (B) (RESIMP B)) (CAR B))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                        (CAR A))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           (T
            (LPRIE (LIST "Can't handle tensor " U " with rank " (CADDR U))))))) 
(PUT 'EXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'EXPRESS 'DEFINED-ON-LINE '211) 
(PUT 'EXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'EXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPRESS (EXPN)
    (PROG (LST MATRX RNK OPEXPRESS)
      (COND ((NOT (ATOM EXPN)) (GO OP)))
      (COND
       ((AND (EQ (GET EXPN 'RTYPE) 'TENSOP) (SETQ RNK (GET EXPN 'AVALUE))
             (MEMQ (CAR RNK) '(TENSOR TENSOP)) (SETQ RNK (CADR RNK)))
        (RETURN
         (COND
          ((EQUAL (CADR RNK) NSCAL*)
           (COND ((CAR (CDDDDR RNK)) RNK) (T (RESIMPTENS RNK))))
          (T
           (LPRIE
            (LIST " You must rebind tensor " EXPN
                  " in the new coordinate system")))))))
      (RETURN (MKTENSOR 0 (SIMP* EXPN)))
     OP
      (COND
       ((EQUAL (CAR EXPN) 'VECT)
        (RETURN
         (MKTENSOR 1
          (TESTDIM1
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A (CDR EXPN))
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (A) (SIMP* A)) (CAR A)) NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (SIMP* A)) (CAR A)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))))
       ((EQUAL (CAR EXPN) 'DYAD)
        (RETURN
         (MKTENSOR 2
          (TESTDIM2
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A (CDR EXPN))
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (A)
                                 (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ B A)
                                   (COND ((NULL B) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (B) (SIMP* B))
                                                     (CAR B))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ B (CDR B))
                                   (COND ((NULL B) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B) (SIMP* B)) (CAR B))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR A))
                              NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (A)
                         (PROG (B FORALL-RESULT FORALL-ENDPTR)
                           (SETQ B A)
                           (COND ((NULL B) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B) (SIMP* B)) (CAR B))
                                            NIL)))
                          LOOPLABEL
                           (SETQ B (CDR B))
                           (COND ((NULL B) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (B) (SIMP* B)) (CAR B)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR A))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))))
       ((EQ (CAR EXPN) 'TENSOR)
        (RETURN
         (COND
          ((EQUAL (CADR EXPN) NSCAL*)
           (COND ((CAR (CDDDDR EXPN)) EXPN) (T (RESIMPTENS EXPN))))
          (T
           (LPRIE
            (LIST " You must rebind tensor " EXPN
                  " in the new coordinate system")))))))
      (SETQ LST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (CDR EXPN))
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (A) (CDDR (EXPRESS A))) (CAR A))
                                      NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (CDDR (EXPRESS A))) (CAR A)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((SETQ OPEXPRESS (GET (CAR EXPN) 'EXPRESS))
        (PROGN
         (SETQ RNK (EVAL (CONS OPEXPRESS (LIST (MKQUOTE LST)))))
         (RETURN (MKTENSOR (CAR RNK) (CADR RNK))))))
      (COND
       ((GET (CAR EXPN) 'SIMPFN)
        (RETURN
         (MKTENSOR 0
          (SIMP
           (CONS (CAR EXPN)
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A LST)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A)
                                       (COND
                                        ((EQUAL (CAR A) 0)
                                         (LIST '*SQ (CDR A) NIL))
                                        (T
                                         (TYPERR EXPN
                                                 " well formed scalar "))))
                                     (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (A)
                               (COND
                                ((EQUAL (CAR A) 0) (LIST '*SQ (CDR A) NIL))
                                (T (TYPERR EXPN " well formed scalar "))))
                             (CAR A))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))))
      (SETQ LST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A LST)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (COND ((EQUAL (CAR A) 0) (PREPSQ (CDR A)))
                                          (T
                                           (TYPERR EXPN
                                                   " well formed tensor"))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (COND ((EQUAL (CAR A) 0) (PREPSQ (CDR A)))
                                  (T (TYPERR EXPN " well formed tensor"))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (MKTENSOR 0 (CONS (LIST (CONS (CONS (CONS (CAR EXPN) LST) 1) 1)) 1))))) 
(PUT 'TESTDIM1 'NUMBER-OF-ARGS 1) 
(PUT 'TESTDIM1 'DEFINED-ON-LINE '248) 
(PUT 'TESTDIM1 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TESTDIM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTDIM1 (U)
    (COND ((EQUAL (LENGTH U) DIMENSION*) U)
          (T (PROGN (LPRIE "Bad number of vector components") U)))) 
(PUT 'TESTDIM2 'NUMBER-OF-ARGS 1) 
(PUT 'TESTDIM2 'DEFINED-ON-LINE '252) 
(PUT 'TESTDIM2 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TESTDIM2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTDIM2 (U)
    (PROG (X)
      (COND ((EQUAL (LENGTH U) DIMENSION*) T) (T (GO ER)))
      (SETQ X U)
     A
      (COND ((EQUAL (LENGTH (CAR U)) DIMENSION*) T) (T (GO ER)))
      (SETQ X (CDR X))
      (COND (X (GO A)))
      (RETURN U)
     ER
      (LPRIE "Bad number of dyad components")
      (RETURN U))) 
(PUT 'VECTORS 'NUMBER-OF-ARGS 1) 
(PUT 'VECTORS 'DEFINED-ON-LINE '271) 
(PUT 'VECTORS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'VECTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTORS (ARGLIST)
    (PROG (I)
      (SETQ I ARGLIST)
     LAB
      (COND ((NULL I) (RETURN NIL)))
      ((LAMBDA (I)
         (PROGN
          (PUT I 'RTYPE 'TENSOP)
          (PUT I 'SIMPFN 'SIMPIDEN)
          (PUT I 'AVALUE
               (LIST 'TENSOP
                     (MKTENSOR 1
                      (REVERSE
                       (PROG (A FORALL-RESULT FORALL-ENDPTR)
                         (SETQ A COORDINDX*)
                         (COND ((NULL A) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (A) (SIMP (LIST I A)))
                                           (CAR A))
                                          NIL)))
                        LOOPLABEL
                         (SETQ A (CDR A))
                         (COND ((NULL A) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (A) (SIMP (LIST I A))) (CAR A))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))))
       (CAR I))
      (SETQ I (CDR I))
      (GO LAB))) 
(DEFLIST '((VECTORS RLIS)) 'STAT) 
(PUT 'DYADS 'NUMBER-OF-ARGS 1) 
(PUT 'DYADS 'DEFINED-ON-LINE '282) 
(PUT 'DYADS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DYADS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DYADS (ARGLIST)
    (PROG (I)
      (SETQ I ARGLIST)
     LAB
      (COND ((NULL I) (RETURN NIL)))
      ((LAMBDA (I)
         (PROGN
          (PUT I 'RTYPE 'TENSOP)
          (PUT I 'SIMPFN 'SIMPIDEN)
          (PUT I 'AVALUE
               (LIST 'TENSOP
                     (MKTENSOR 2
                      (REVERSE
                       (PROG (A FORALL-RESULT FORALL-ENDPTR)
                         (SETQ A COORDINDX*)
                         (COND ((NULL A) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (A)
                                             (REVERSE
                                              (PROG (B FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ B COORDINDX*)
                                                (COND ((NULL B) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (B)
                                                                    (SIMP
                                                                     (LIST I A
                                                                           B)))
                                                                  (CAR B))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ B (CDR B))
                                                (COND
                                                 ((NULL B)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (B)
                                                            (SIMP
                                                             (LIST I A B)))
                                                          (CAR B))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                           (CAR A))
                                          NIL)))
                        LOOPLABEL
                         (SETQ A (CDR A))
                         (COND ((NULL A) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (REVERSE
                                      (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ B COORDINDX*)
                                        (COND ((NULL B) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (B)
                                                            (SIMP
                                                             (LIST I A B)))
                                                          (CAR B))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ B (CDR B))
                                        (COND
                                         ((NULL B) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (B)
                                                    (SIMP (LIST I A B)))
                                                  (CAR B))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))))
                                   (CAR A))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))))
       (CAR I))
      (SETQ I (CDR I))
      (GO LAB))) 
(DEFLIST '((DYADS RLIS)) 'STAT) 
(PUT 'PLUSEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'PLUSEXPRESS 'DEFINED-ON-LINE '295) 
(PUT 'PLUSEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'PLUSEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLUSEXPRESS (U)
    (PROG (Z)
      (SETQ Z (CAR U))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN Z)))
      (SETQ Z (PLUS2EX Z (CAR U)))
      (GO A))) 
(PUT 'PLUS 'EXPRESS 'PLUSEXPRESS) 
(PUT 'PLUS2EX 'NUMBER-OF-ARGS 2) 
(PUT 'PLUS2EX 'DEFINED-ON-LINE '307) 
(PUT 'PLUS2EX 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'PLUS2EX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLUS2EX (X Y)
    (PROG (MTX MTY SLX SLY RNK ANS ANS1)
      (SETQ RNK (CAR X))
      (COND ((NOT (EQUAL RNK (CAR Y))) (LPRIE "Tensor mishmash")))
      (COND ((EQUAL RNK 0) (RETURN (LIST RNK (ADDSQ (CADR X) (CADR Y)))))
            ((EQUAL RNK 1)
             (PROGN
              (SETQ SLX (CADR X))
              (SETQ SLY (CADR Y))
              (PROG ()
               WHILELABEL
                (COND ((NOT SLX) (RETURN NIL)))
                (PROGN
                 (SETQ ANS (CONS (ADDSQ (CAR SLX) (CAR SLY)) ANS))
                 (SETQ SLX (CDR SLX))
                 (SETQ SLY (CDR SLY)))
                (GO WHILELABEL))
              (SETQ ANS (LIST 1 (REVERSE ANS)))))
            ((EQUAL RNK 2)
             (PROGN
              (SETQ MTX (CADR X))
              (SETQ MTY (CADR Y))
              (PROG ()
               WHILELABEL
                (COND ((NOT MTX) (RETURN NIL)))
                (PROGN
                 (SETQ SLX (CAR MTX))
                 (SETQ SLY (CAR MTY))
                 (SETQ ANS1 NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT SLX) (RETURN NIL)))
                   (PROGN
                    (SETQ ANS1 (CONS (ADDSQ (CAR SLX) (CAR SLY)) ANS1))
                    (SETQ SLX (CDR SLX))
                    (SETQ SLY (CDR SLY)))
                   (GO WHILELABEL))
                 (SETQ ANS (CONS (REVERSE ANS1) ANS))
                 (SETQ MTX (CDR MTX))
                 (SETQ MTY (CDR MTY)))
                (GO WHILELABEL))
              (SETQ ANS (LIST 2 (REVERSE ANS))))))
      (RETURN ANS))) 
(PUT 'TIMESEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'TIMESEXPRESS 'DEFINED-ON-LINE '339) 
(PUT 'TIMESEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TIMESEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TIMESEXPRESS (U)
    (PROG (Z)
      (SETQ Z (CAR U))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN Z)))
      (SETQ Z (TIMES2EX Z (CAR U)))
      (GO A))) 
(PUT 'TIMES 'EXPRESS 'TIMESEXPRESS) 
(PUT 'TIMES2EX 'NUMBER-OF-ARGS 2) 
(PUT 'TIMES2EX 'DEFINED-ON-LINE '351) 
(PUT 'TIMES2EX 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TIMES2EX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TIMES2EX (X Y)
    (PROG (RNKX RNKY)
      (SETQ RNKX (CAR X))
      (SETQ RNKY (CAR Y))
      (RETURN
       (COND ((EQUAL RNKX 0) (LIST RNKY (TIMES0EX (CADR X) (CADR Y) RNKY)))
             ((EQUAL RNKY 0) (LIST RNKX (TIMES0EX (CADR Y) (CADR X) RNKX)))
             (T (LPRIE " Tensor mishmash ")))))) 
(PUT 'TIMES0EX 'NUMBER-OF-ARGS 3) 
(PUT 'TIMES0EX 'DEFINED-ON-LINE '362) 
(PUT 'TIMES0EX 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'TIMES0EX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TIMES0EX (X Y RNK)
    (COND ((EQUAL RNK 0) (MULTSQ X Y))
          ((EQUAL RNK 1)
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A Y)
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (A) (MULTSQ X A)) (CAR A)) NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (A) (MULTSQ X A)) (CAR A)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          ((EQUAL RNK 2)
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A Y)
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (A)
                                 (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ B A)
                                   (COND ((NULL B) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (B) (MULTSQ X B))
                                                     (CAR B))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ B (CDR B))
                                   (COND ((NULL B) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B) (MULTSQ X B)) (CAR B))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR A))
                              NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (A)
                         (PROG (B FORALL-RESULT FORALL-ENDPTR)
                           (SETQ B A)
                           (COND ((NULL B) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B) (MULTSQ X B)) (CAR B))
                                            NIL)))
                          LOOPLABEL
                           (SETQ B (CDR B))
                           (COND ((NULL B) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (B) (MULTSQ X B)) (CAR B))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR A))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          (T (LPRIE " Tensor mishmash ")))) 
(PUT 'MINUSEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'MINUSEXPRESS 'DEFINED-ON-LINE '371) 
(PUT 'MINUSEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'MINUSEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINUSEXPRESS (EXPN)
    (TIMESEXPRESS (LIST (LIST 0 (CONS (MINUS 1) 1)) (CAR EXPN)))) 
(PUT 'MINUS 'EXPRESS 'MINUSEXPRESS) 
(PUT 'DIFFERENCEEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'DIFFERENCEEXPRESS 'DEFINED-ON-LINE '376) 
(PUT 'DIFFERENCEEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DIFFERENCEEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFFERENCEEXPRESS (EXPN)
    (PLUSEXPRESS (LIST (CAR EXPN) (MINUSEXPRESS (LIST (CADR EXPN)))))) 
(PUT 'DIFFERENCE 'EXPRESS 'DIFFERENCEEXPRESS) 
(PUT 'QUOTIENTEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTIENTEXPRESS 'DEFINED-ON-LINE '381) 
(PUT 'QUOTIENTEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'QUOTIENTEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTIENTEXPRESS (EXPN)
    (COND
     ((EQUAL (CAR (CADR EXPN)) 0)
      (TIMES2EX (LIST 0 (SIMP* (LIST 'RECIP (PREPSQ (CADR (CADR EXPN))))))
       (CAR EXPN)))
     (T (LPRIE " Tensor mishmash ")))) 
(PUT 'QUOTIENT 'EXPRESS 'QUOTIENTEXPRESS) 
(PUT 'EXPTEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'EXPTEXPRESS 'DEFINED-ON-LINE '389) 
(PUT 'EXPTEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'EXPTEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPTEXPRESS (EXPN)
    (COND
     ((AND (EQUAL (CAR (CAR EXPN)) 0) (EQUAL (CAR (CADR EXPN)) 0))
      (LIST 0
            (SIMP*
             (LIST 'EXPT (PREPSQ (CADR (CAR EXPN)))
                   (PREPSQ (CADR (CADR EXPN)))))))
     (T (LPRIE " Tensor mishmash ")))) 
(PUT 'EXPT 'EXPRESS 'EXPTEXPRESS) 
(PUT 'RECIPEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'RECIPEXPRESS 'DEFINED-ON-LINE '398) 
(PUT 'RECIPEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'RECIPEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RECIPEXPRESS (EXPN)
    (COND
     ((EQUAL (CAR (CAR EXPN)) 0)
      (LIST 0 (SIMP* (LIST 'RECIP (PREPSQ (CADR (CAR EXPN)))))))
     (T (LPRIE " Tensor mishmash ")))) 
(PUT 'RECIP 'EXPRESS 'RECIPEXPRESS) 
(PUT 'INPRODEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'INPRODEXPRESS 'DEFINED-ON-LINE '406) 
(PUT 'INPRODEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'INPRODEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INPRODEXPRESS (EXPN)
    (PROG (ARG1 ARG2 RNK1 RNK2)
      (SETQ ARG1 (CADR (CAR EXPN)))
      (SETQ ARG2 (CADR (CADR EXPN)))
      (SETQ RNK1 (CAR (CAR EXPN)))
      (SETQ RNK2 (CAR (CADR EXPN)))
      (RETURN
       (COND ((EQUAL RNK1 1) (INPROD1EX ARG1 ARG2 RNK2))
             ((EQUAL RNK1 2) (INPROD2EX ARG1 ARG2 RNK2))
             (T (LPRIE " Tensor mishmash ")))))) 
(PUT 'CONS 'EXPRESS 'INPRODEXPRESS) 
(PUT 'INPROD1EX 'NUMBER-OF-ARGS 3) 
(PUT 'INPROD1EX 'DEFINED-ON-LINE '421) 
(PUT 'INPROD1EX 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'INPROD1EX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INPROD1EX (X Y RNK)
    (PROG (LSTX LSTY MTY Z ZZ)
      (SETQ LSTX X)
      (SETQ LSTY Y)
      (COND
       ((EQUAL RNK 1)
        (PROGN
         (SETQ Z (CONS NIL 1))
         (PROG ()
          WHILELABEL
           (COND ((NOT LSTX) (RETURN NIL)))
           (PROGN
            (SETQ Z (ADDSQ Z (MULTSQ (CAR LSTX) (CAR LSTY))))
            (SETQ LSTX (CDR LSTX))
            (SETQ LSTY (CDR LSTY)))
           (GO WHILELABEL))
         (SETQ Z (LIST 0 Z))))
       ((EQUAL RNK 2)
        (PROGN
         (SETQ Z NIL)
         (SETQ LSTY (SETQ MTY (COPY2 Y T)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (CAR MTY)) (RETURN NIL)))
           (PROGN
            (SETQ LSTX X)
            (SETQ LSTY MTY)
            (SETQ ZZ (CONS NIL 1))
            (PROG ()
             WHILELABEL
              (COND ((NOT LSTX) (RETURN NIL)))
              (PROGN
               (SETQ ZZ (ADDSQ ZZ (MULTSQ (CAR LSTX) (CAAR LSTY))))
               (RPLACA LSTY (CDAR LSTY))
               (SETQ LSTY (CDR LSTY))
               (SETQ LSTX (CDR LSTX)))
              (GO WHILELABEL))
            (SETQ Z (NCONC Z (LIST ZZ))))
           (GO WHILELABEL))
         (SETQ Z (LIST 1 Z))))
       (T (LPRIE " Tensor mishmash ")))
      (RETURN Z))) 
(PUT 'INPROD2EX 'NUMBER-OF-ARGS 3) 
(PUT 'INPROD2EX 'DEFINED-ON-LINE '451) 
(PUT 'INPROD2EX 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'INPROD2EX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INPROD2EX (X Y RNK)
    (PROG (MTX Z)
      (SETQ MTX X)
      (COND
       ((EQUAL RNK 1)
        (PROG ()
         WHILELABEL
          (COND ((NOT MTX) (RETURN NIL)))
          (PROGN
           (SETQ Z (NCONC Z (CDR (INPROD1EX (CAR MTX) Y 1))))
           (SETQ MTX (CDR MTX)))
          (GO WHILELABEL)))
       ((EQUAL RNK 2)
        (PROG ()
         WHILELABEL
          (COND ((NOT MTX) (RETURN NIL)))
          (PROGN
           (SETQ Z (NCONC Z (CDR (INPROD1EX (CAR MTX) (COPY2 Y T) 2))))
           (SETQ MTX (CDR MTX)))
          (GO WHILELABEL)))
       (T (LPRIE " Tensor mishmash ")))
      (RETURN (LIST RNK Z)))) 
(PUT 'OUTEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'OUTEXPRESS 'DEFINED-ON-LINE '467) 
(PUT 'OUTEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'OUTEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTEXPRESS (EXPN)
    (PROG (X Y Z)
      (SETQ X (CADR (CAR EXPN)))
      (SETQ Y (CADR (CADR EXPN)))
      (COND
       ((AND (EQUAL (CAR (CAR EXPN)) 1) (EQUAL (CAR (CADR EXPN)) 1)
             (NULL (CDDR EXPN)))
        (PROG (I)
          (SETQ I X)
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (SETQ Z
                     (CONS
                      (PROG (A FORALL-RESULT FORALL-ENDPTR)
                        (SETQ A Y)
                        (COND ((NULL A) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (A) (MULTSQ A I)) (CAR A))
                                         NIL)))
                       LOOPLABEL
                        (SETQ A (CDR A))
                        (COND ((NULL A) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (A) (MULTSQ A I)) (CAR A)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      Z)))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB)))
       (T (LPRIE (LIST " Outer product of " EXPN))))
      (RETURN (LIST 2 (REVERSE Z))))) 
(PUT '& 'EXPRESS 'OUTEXPRESS) 
(FLAG '(&) 'TENSFN) 
(PUT 'COPY2 'NUMBER-OF-ARGS 2) 
(PUT 'COPY2 'DEFINED-ON-LINE '481) 
(PUT 'COPY2 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'COPY2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COPY2 (X P)
    (COND ((NULL X) NIL) (P (CONS (COPY2 (CAR X) NIL) (COPY2 (CDR X) T)))
          (T (CONS (CAR X) (COPY2 (CDR X) NIL))))) 
(PUT 'LISTAR 'NUMBER-OF-ARGS 2) 
(PUT 'LISTAR 'DEFINED-ON-LINE '486) 
(PUT 'LISTAR 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'LISTAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTAR (ARG J)
    (COND ((EQUAL J 1) (CAR ARG)) ((EQUAL J 2) (CADR ARG))
          ((EQUAL J 3) (CADDR ARG)) (T (LPRIE (LIST " LISTAR " ARG J))))) 
(PUT 'LISTARSQ 'NUMBER-OF-ARGS 2) 
(PUT 'LISTARSQ 'DEFINED-ON-LINE '492) 
(PUT 'LISTARSQ 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'LISTARSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTARSQ (ARG J) (PREPSQ (LISTAR ARG J))) 
(PUT 'DINPROD 'NUMBER-OF-ARGS 1) 
(PUT 'DINPROD 'DEFINED-ON-LINE '495) 
(PUT 'DINPROD 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DINPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DINPROD (EXPN)
    (PROG (X Y Z XX YY)
      (SETQ X (CADR (CAR EXPN)))
      (SETQ Y (COPY2 (CADR (CADR EXPN)) T))
      (SETQ Z (CONS NIL 1))
      (COND
       ((NOT
         (AND (EQUAL (CAR (CAR EXPN)) 2) (EQUAL (CAR (CADR EXPN)) 2)
              (NULL (CDDR EXPN))))
        (LPRIE (LIST " D-scalar product of " EXPN))))
     A
      (COND ((AND (NULL X) (NULL Y)) (GO D)) ((OR (NULL X) (NULL Y)) (GO ER)))
      (SETQ XX (CAR X))
      (SETQ YY (CAR Y))
     B
      (COND ((AND (NULL XX) (NULL YY)) (GO C))
            ((OR (NULL XX) (NULL YY)) (GO ER)))
      (SETQ Z (ADDSQ Z (MULTSQ (CAR XX) (CAR YY))))
      (SETQ XX (CDR XX))
      (SETQ YY (CDR YY))
      (GO B)
     C
      (SETQ X (CDR X))
      (SETQ Y (CDR Y))
      (GO A)
     D
      (RETURN (LIST 0 Z))
     ER
      (LPRIE (LIST " EXPRESS error " EXPN " D-S dyads of dif. size")))) 
(PUT '|#| 'EXPRESS 'DINPROD) 
(PUT 'HASH 'EXPRESS 'DINPROD) 
(PUT 'HASH 'RTYPEFN 'PTENSOR) 
(PUT 'ANTISYMSUM 'NUMBER-OF-ARGS 2) 
(PUT 'ANTISYMSUM 'DEFINED-ON-LINE '524) 
(PUT 'ANTISYMSUM 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'ANTISYMSUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ANTISYMSUM (U V)
    (COND ((EQUAL DIMENSION* 2) (DIFMUL (CAR U) (CADR U) (CADR V) (CAR V)))
          ((EQUAL DIMENSION* 3)
           (LIST (DIFMUL (CADR U) (CADDR U) (CADDR V) (CADR V))
                 (DIFMUL (CADDR U) (CAR U) (CAR V) (CADDR V))
                 (DIFMUL (CAR U) (CADR U) (CADR V) (CAR V))))
          (T (LPRIE (LIST " ANTISYMSUM " U V))))) 
(PUT 'DIFMUL 'NUMBER-OF-ARGS 4) 
(PUT 'DIFMUL 'DEFINED-ON-LINE '532) 
(PUT 'DIFMUL 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DIFMUL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFMUL (A B C D) (ADDSQ (MULTSQ A C) (NEGSQ (MULTSQ B D)))) 
(PUT 'VECTPROD 'NUMBER-OF-ARGS 1) 
(PUT 'VECTPROD 'DEFINED-ON-LINE '536) 
(PUT 'VECTPROD 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'VECTPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTPROD (EXPN)
    (PROG (X Y RNX RNY)
      (SETQ X (CADR (CAR EXPN)))
      (SETQ Y (CADR (CADR EXPN)))
      (SETQ RNX (CAR (CAR EXPN)))
      (SETQ RNY (CAR (CADR EXPN)))
      (COND
       ((AND (EQUAL RNX 1) (EQUAL RNY 1))
        (RETURN (LIST (DIFFERENCE DIMENSION* 2) (ANTISYMSUM X Y))))
       ((AND (EQUAL RNX 2) (EQUAL RNY 1))
        (RETURN
         (LIST (DIFFERENCE DIMENSION* 1)
               (PROG (A FORALL-RESULT FORALL-ENDPTR)
                 (SETQ A X)
                 (COND ((NULL A) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (A) (ANTISYMSUM A Y)) (CAR A))
                                       NIL)))
                LOOPLABEL
                 (SETQ A (CDR A))
                 (COND ((NULL A) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (A) (ANTISYMSUM A Y)) (CAR A)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
       ((AND (EQUAL RNX 1) (EQUAL RNY 2))
        (RETURN
         (LIST (DIFFERENCE DIMENSION* 1)
               (COND
                ((EQUAL DIMENSION* 3)
                 (TP1
                  (COPY2
                   (PROG (A FORALL-RESULT FORALL-ENDPTR)
                     (SETQ A (TP1 (COPY2 Y T)))
                     (COND ((NULL A) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (A) (ANTISYMSUM X A)) (CAR A))
                                      NIL)))
                    LOOPLABEL
                     (SETQ A (CDR A))
                     (COND ((NULL A) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (A) (ANTISYMSUM X A)) (CAR A))
                                   NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))
                   T)))
                (T
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A (TP1 (COPY2 Y T)))
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (A) (ANTISYMSUM X A)) (CAR A))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (A) (ANTISYMSUM X A)) (CAR A)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))
       (T (LPRIE (LIST " VECTPROD of " EXPN)))))) 
(PUT '? 'EXPRESS 'VECTPROD) 
(AEVAL (OPERATOR (LIST 'DIFF))) 
(PUT 'GRADEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'GRADEXPRESS 'DEFINED-ON-LINE '561) 
(PUT 'GRADEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'GRADEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRADEXPRESS (EXPN)
    (PROG (ARG VT ANS ROW Z)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (COND
       ((EQUAL VT 0)
        (PROG (I)
          (SETQ I COORDINDX*)
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (SETQ ANS
                     (CONS
                      (SIMP*
                       (LIST 'QUOTIENT
                             (LIST 'DIFF (LIST '*SQ ARG NIL)
                                   (GETEL (LIST 'COORDINATS I)))
                             (GETEL (LIST 'SF I))))
                      ANS)))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB)))
       ((EQUAL VT 1)
        (PROG (I)
          (SETQ I COORDINDX*)
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ ROW NIL)
              (PROG (J)
                (SETQ J COORDINDX*)
               LAB
                (COND ((NULL J) (RETURN NIL)))
                ((LAMBDA (J)
                   (PROGN
                    (SETQ Z
                            (LIST 'DIFF (LISTARSQ ARG J)
                                  (GETEL (LIST 'COORDINATS I))))
                    (SETQ Z (LIST (LIST 'QUOTIENT Z (GETEL (LIST 'SF I)))))
                    (PROG (K)
                      (SETQ K COORDINDX*)
                     LAB
                      (COND ((NULL K) (RETURN NIL)))
                      ((LAMBDA (K)
                         (SETQ Z
                                 (CONS
                                  (LIST 'TIMES
                                        (GETEL (LIST 'CHRISTOFFEL I J K))
                                        (LISTARSQ ARG K))
                                  Z)))
                       (CAR K))
                      (SETQ K (CDR K))
                      (GO LAB))
                    (SETQ ROW (CONS (SIMP* (CONS 'PLUS Z)) ROW))))
                 (CAR J))
                (SETQ J (CDR J))
                (GO LAB))
              (SETQ ANS (CONS ROW ANS))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB)))
       (T (LPRIE (LIST " GRAD of " EXPN))))
      (RETURN (LIST (PLUS VT 1) ANS)))) 
(PUT 'GRAD 'EXPRESS 'GRADEXPRESS) 
(PUT 'DIVEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'DIVEXPRESS 'DEFINED-ON-LINE '595) 
(PUT 'DIVEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DIVEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIVEXPRESS (EXPN)
    (PROG (ARG VT ANS Z)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (COND
       ((EQUAL VT 1)
        (PROGN
         (PROG (I)
           (SETQ I COORDINDX*)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (SETQ Z
                      (CONS
                       (LIST 'DIFF
                             (LIST 'TIMES SFPROD* (LISTARSQ ARG I)
                                   (LIST 'RECIP (GETEL (LIST 'SF I))))
                             (GETEL (LIST 'COORDINATS I)))
                       Z)))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (SETQ Z (CONS 'PLUS Z))
         (SETQ Z (LIST 'QUOTIENT Z SFPROD*))
         (SETQ ANS (SIMP* Z))))
       ((EQUAL VT 2)
        (PROG (I)
          (SETQ I COORDINDX*)
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ Z NIL)
              (PROG (J)
                (SETQ J 1)
               LAB
                (COND ((MINUSP (DIFFERENCE DIMENSION* J)) (RETURN NIL)))
                (PROGN
                 (SETQ Z
                         (CONS
                          (LIST 'QUOTIENT
                                (LIST 'DIFF
                                      (LIST 'TIMES (LISTARSQ (LISTAR ARG J) I)
                                            SFPROD*
                                            (LIST 'RECIP (GETEL (LIST 'SF J))))
                                      (GETEL (LIST 'COORDINATS J)))
                                SFPROD*)
                          Z))
                 (PROG (L)
                   (SETQ L 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE DIMENSION* L)) (RETURN NIL)))
                   (SETQ Z
                           (CONS
                            (LIST 'TIMES (LISTARSQ (LISTAR ARG J) L)
                                  (GETEL (LIST 'CHRISTOFFEL J I L)))
                            Z))
                   (SETQ L (PLUS2 L 1))
                   (GO LAB)))
                (SETQ J (PLUS2 J 1))
                (GO LAB))
              (SETQ ANS (CONS (SIMP* (CONS 'PLUS Z)) ANS))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB)))
       (T (LPRIE (LIST " DIV of " EXPN))))
      (RETURN (LIST (DIFFERENCE VT 1) ANS)))) 
(PUT 'DIV 'EXPRESS 'DIVEXPRESS) 
(PUT 'LAPLEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'LAPLEXPRESS 'DEFINED-ON-LINE '638) 
(PUT 'LAPLEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'LAPLEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAPLEXPRESS (EXPN)
    (PROG (ARG VT ANS)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (COND
       ((EQUAL VT 0)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE DIMENSION* I)) (RETURN NIL)))
           (SETQ ANS
                   (CONS
                    (LIST 'DIFF
                          (LIST 'TIMES SFPROD*
                                (LIST 'EXPT (GETEL (LIST 'SF I)) (MINUS 2))
                                (LIST 'DIFF (LIST '*SQ ARG NIL)
                                      (GETEL (LIST 'COORDINATS I))))
                          (GETEL (LIST 'COORDINATS I)))
                    ANS))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ ANS
                 (LIST 0 (SIMP* (LIST 'QUOTIENT (CONS 'PLUS ANS) SFPROD*))))))
       ((EQUAL VT 1) (SETQ ANS (DIVEXPRESS (LIST (GRADEXPRESS EXPN)))))
       (T (LPRIE (LIST " LAPLACIAN of " EXPN))))
      (RETURN ANS))) 
(PUT 'LAPL 'EXPRESS 'LAPLEXPRESS) 
(PUT 'CURLEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'CURLEXPRESS 'DEFINED-ON-LINE '666) 
(PUT 'CURLEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'CURLEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CURLEXPRESS (EXPN)
    (PROG (ARG VT ANS IC1 IC2)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (COND
       ((EQUAL VT 1)
        (PROG (I)
          (SETQ I (COND ((EQUAL DIMENSION* 3) COORDINDX*) (T '(1))))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ IC1 (LISTAR CYCLIC1* I))
              (SETQ IC2 (LISTAR CYCLIC2* I))
              (SETQ ANS
                      (CONS
                       (SIMP*
                        (LIST 'TIMES (GETEL (LIST 'SF I)) (LIST 'RECIP SFPROD*)
                              (LIST 'DIFFERENCE
                                    (LIST 'DIFF
                                          (LIST 'TIMES (GETEL (LIST 'SF IC2))
                                                (LISTARSQ ARG IC2))
                                          (GETEL (LIST 'COORDINATS IC1)))
                                    (LIST 'DIFF
                                          (LIST 'TIMES (GETEL (LIST 'SF IC1))
                                                (LISTARSQ ARG IC1))
                                          (GETEL (LIST 'COORDINATS IC2))))))
                       ANS))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB)))
       (T (LPRIE (LIST " CURL of " EXPN))))
      (RETURN
       (COND ((EQUAL DIMENSION* 3) (LIST 1 ANS)) (T (LIST 0 (CAR ANS))))))) 
(PUT 'CURL 'EXPRESS 'CURLEXPRESS) 
(FLAG '(CONS GRAD DIV LAPL CURL TENS VECT DYAD DIRDF & |#| ?) 'TENSFN) 
(PUT 'EXSCALVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EXSCALVAL 'DEFINED-ON-LINE '700) 
(PUT 'EXSCALVAL 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'EXSCALVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXSCALVAL (U)
    (PROG (FCE ARGS)
      (SETQ FCE (CAR U))
      (SETQ ARGS
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (CDR U))
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (A) (CDDR (EXPRESS A))) (CAR A))
                                      NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (CDDR (EXPRESS A))) (CAR A)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FCE (EVAL (CONS (GET FCE 'EXPRESS) (LIST (MKQUOTE ARGS)))))
      (COND ((EQUAL (CAR FCE) 0) (RETURN (CADR FCE)))
            (T (TYPERR U " is not scalar ")))
      (RETURN (CONS NIL 1)))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(MKOP 'HASH) 
(MKOP '?) 
(MKOP '&) 
(INFIX (LIST 'HASH '? '&)) 
(PRECEDENCE (LIST 'CONS 'EXPT)) 
(PRECEDENCE (LIST 'HASH 'CONS)) 
(PRECEDENCE (LIST '? 'HASH)) 
(PRECEDENCE (LIST '& '?)) 
(FLAG '(CONS |#| ? DIV LAPL CURL DIRDF) 'FULL) 
(PROG (A)
  (SETQ A '(CONS |#| ? DIV LAPL CURL DIRDF))
 LAB
  (COND ((NULL A) (RETURN NIL)))
  ((LAMBDA (A) (PUT A 'SIMPFN 'EXSCALVAL)) (CAR A))
  (SETQ A (CDR A))
  (GO LAB)) 
(PUT 'SCALEFACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'SCALEFACTORS 'DEFINED-ON-LINE '723) 
(PUT 'SCALEFACTORS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'SCALEFACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCALEFACTORS (TRANSF)
    (PROG (VAR)
      (SETQ DIMENSION* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CAR TRANSF)))
      (SETQ TRANSF (CDR TRANSF))
      (COND
       ((EQUAL DIMENSION* 2)
        (PROGN
         (SETQ VAR (CDDR TRANSF))
         (SETQ TRANSF (LIST (CAR TRANSF) (CADR TRANSF)))))
       ((SETQ DIMENSION* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 3))
        (PROGN
         (SETQ VAR (CDDDR TRANSF))
         (SETQ TRANSF (LIST (CAR TRANSF) (CADR TRANSF) (CADDR TRANSF)))))
       (T (LPRIE (LIST " Can't handle dimension = " DIMENSION*))))
      (COND ((EQUAL DIMENSION* (LENGTH VAR)) T)
            (T (LPRIE (LIST " Transformation " TRANSF VAR))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE DIMENSION* I)) (RETURN NIL)))
        (SETEL (LIST 'COORDINATS I) (LISTAR VAR I))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE DIMENSION* ROW)) (RETURN NIL)))
        (PROG (COL)
          (SETQ COL 1)
         LAB
          (COND ((MINUSP (DIFFERENCE DIMENSION* COL)) (RETURN NIL)))
          (SETEL (LIST 'JACOBIAN ROW COL)
                 (REVAL1
                  (LIST 'DF (LISTAR TRANSF COL) (GETEL (LIST 'COORDINATS ROW)))
                  NIL))
          (SETQ COL (PLUS2 COL 1))
          (GO LAB))
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (UPDATEDIMEN)
      (RSCALE))) 
(DEFLIST '((SCALEFACTORS RLIS)) 'STAT) 
(FLAG '(REMD) 'EVAL) 
(REMD 'JACOBIAN) 
(REMPROP 'JACOBIAN 'OPFN) 
(ARRAYFN 'ALGEBRAIC
         (LIST (LIST 'JACOBIAN 3 3) (LIST 'COORDINATS 3) (LIST 'SF 3)
               (LIST 'CHRISTOFFEL 3 3 3))) 
(PUT 'RSCALE 'NUMBER-OF-ARGS 0) 
(FLAG '(RSCALE) 'OPFN) 
(PUT 'RSCALE 'DEFINED-ON-LINE '756) 
(PUT 'RSCALE 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'RSCALE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RSCALE NIL
    (PROG ()
      (SETQ SFPROD* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (AEVAL 1)))
      (SETQ NSCAL*
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (AEVAL (LIST 'PLUS NSCAL* 1))))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DIMENSION*) ROW))
          (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE ROW 1) COL)) (RETURN NIL)))
           (PROGN
            (SETEL (LIST 'SF ROW) (AEVAL* (LIST 'GCOV ROW COL)))
            (COND ((EVALEQUAL (AEVAL* (LIST 'SF ROW)) 0) (AEVAL* 'NIL))
                  (T
                   (PROGN
                    (ASSGNPRI
                     (AEVAL* " WARNING: Coordinate system is nonorthogonal")
                     NIL 'FIRST)
                    (ASSGNPRI (AEVAL* " unless following simplifies to zero: ")
                              NIL NIL)
                    (ASSGNPRI (AEVAL* (LIST 'SF ROW)) NIL 'LAST)))))
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         (SETEL (LIST 'SF ROW) (AEVAL* (LIST 'SQRT (LIST 'GCOV ROW ROW))))
         (SETQ SFPROD*
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (AEVAL* (LIST 'TIMES SFPROD* (LIST 'SF ROW))))))
        (SETQ ROW
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 ROW))
        (GO LAB))
      (AEVAL (ON (LIST 'NERO)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DIMENSION*) I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DIMENSION*) J))
            (RETURN NIL)))
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND
             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DIMENSION*) K))
              (RETURN NIL)))
            (PROG ()
              (SETEL (LIST 'CHRISTOFFEL I J K)
                     (AEVAL*
                      (LIST 'QUOTIENT
                            (LIST 'DIFFERENCE
                                  (COND
                                   ((EQUAL I J)
                                    (AEVAL*
                                     (LIST 'DF (LIST 'SF J)
                                           (LIST 'COORDINATS K))))
                                   (T 0))
                                  (COND
                                   ((EQUAL I K)
                                    (AEVAL*
                                     (LIST 'DF (LIST 'SF K)
                                           (LIST 'COORDINATS J))))
                                   (T 0)))
                            (LIST 'TIMES (LIST 'SF J) (LIST 'SF K)))))
              (COND
               ((EVALEQUAL (AEVAL* (LIST 'WRCHRI 'A)) 0)
                (ASSGNPRI
                 (SETEL (LIST 'CHRISTOFFEL I J K)
                        (AEVAL* (LIST 'CHRISTOFFEL I J K)))
                 (LIST (LIST 'CHRISTOFFEL I J K)) 'ONLY))))
            (SETQ K
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     K))
            (GO LAB))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (AEVAL (OFF (LIST 'NERO))))) 
(PUT 'GCOV 'NUMBER-OF-ARGS 2) 
(FLAG '(GCOV) 'OPFN) 
(PUT 'GCOV 'DEFINED-ON-LINE '780) 
(PUT 'GCOV 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'GCOV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCOV (J K)
    (PROG (L FORALL-RESULT)
      (SETQ L 1)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DIMENSION*) L))
        (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (AEVAL*
               (LIST 'PLUS
                     (AEVAL*
                      (LIST 'TIMES (LIST 'JACOBIAN J L) (LIST 'JACOBIAN K L)))
                     FORALL-RESULT)))
      (SETQ L
              ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
               L))
      (GO LAB1))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'SIMPWRCHRI 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPWRCHRI 'DEFINED-ON-LINE '785) 
(PUT 'SIMPWRCHRI 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'SIMPWRCHRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPWRCHRI (U) (COND (*WRCHRI (CONS NIL 1)) (T (CONS 1 1)))) 
(PUT 'WRCHRI 'SIMPFN 'SIMPWRCHRI) 
(PUT 'RMAT 'NUMBER-OF-ARGS 0) 
(PUT 'RMAT 'DEFINED-ON-LINE '791) 
(PUT 'RMAT 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'RMAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RMAT NIL (CONS 'DYAD (CDR (MATSTAT)))) 
(PUT 'FORMDYAD 'NUMBER-OF-ARGS 3) 
(PUT 'FORMDYAD 'DEFINED-ON-LINE '794) 
(PUT 'FORMDYAD 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'FORMDYAD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMDYAD (U V M) (CONS 'LIST (CONS (MKQUOTE 'DYAD) (CDDR (FORMMAT U V M))))) 
(PUT 'DYAD 'STAT 'RMAT) 
(PUT 'DYAD 'FORMFN 'FORMDYAD) 
(PUT 'DIRDFEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'DIRDFEXPRESS 'DEFINED-ON-LINE '800) 
(PUT 'DIRDFEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DIRDFEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIRDFEXPRESS (EXPN)
    (PROG (ARG VT DIREC ANS Z DJ DI ARGJ SFJ SFI COOJ)
      (SETQ ARG (CADR EXPN))
      (SETQ VT (CAR ARG))
      (SETQ DIREC (CAR EXPN))
      (COND
       ((NOT (EQUAL (CAR DIREC) 1))
        (RETURN (LPRIE (LIST " Direction in DIRDF is not a vector " EXPN)))))
      (COND
       ((EQUAL VT 0)
        (RETURN (INPRODEXPRESS (LIST DIREC (GRADEXPRESS (LIST ARG)))))))
      (SETQ ARG (CADR ARG))
      (SETQ DIREC (CADR DIREC))
      (COND
       ((NOT (EQUAL VT 1))
        (RETURN (LPRIE (LIST " Argument of DIRDF is dyadic " EXPN)))))
      (PROG (I)
        (SETQ I COORDINDX*)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ Z NIL)
            (SETQ DI (LISTARSQ DIREC I))
            (SETQ SFI (GETEL (LIST 'SF I)))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE DIMENSION* J)) (RETURN NIL)))
              (PROGN
               (SETQ DJ (LISTARSQ DIREC J))
               (SETQ ARGJ (LISTARSQ ARG J))
               (SETQ SFJ (GETEL (LIST 'SF J)))
               (SETQ COOJ (GETEL (LIST 'COORDINATS J)))
               (SETQ Z
                       (CONS
                        (LIST 'TIMES DJ (LIST 'RECIP SFJ)
                              (LIST 'DIFF (LISTARSQ ARG I) COOJ))
                        Z))
               (SETQ Z
                       (CONS
                        (LIST 'TIMES DI ARGJ (LIST 'RECIP SFI)
                              (LIST 'RECIP SFJ) (LIST 'DF SFI COOJ))
                        Z))
               (SETQ Z
                       (CONS
                        (LIST 'MINUS
                              (LIST 'TIMES DJ ARGJ (LIST 'RECIP SFI)
                                    (LIST 'RECIP SFJ)
                                    (LIST 'DF SFJ
                                          (GETEL (LIST 'COORDINATS I)))))
                        Z)))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (SETQ Z (CONS 'PLUS Z))
            (SETQ Z (SIMP* Z))
            (SETQ ANS (CONS Z ANS))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN (LIST 1 ANS)))) 
(PUT 'DIRDF 'EXPRESS 'DIRDFEXPRESS) 
(PUT 'DFEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'DFEXPRESS 'DEFINED-ON-LINE '852) 
(PUT 'DFEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DFEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DFEXPRESS (EXPN)
    (PROG (ARG VT REST)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (SETQ REST (CDR EXPN))
      (SETQ REST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A REST)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (COND
                                     ((EQUAL (CAR A) 0)
                                      (COND ((ATOM (CADR A)) (CADR A))
                                            ((AND (EQUAL (CDR (CADR A)) 1)
                                                  (NUMBERP (CAR (CADR A))))
                                             (CAR (CADR A)))
                                            (T (*Q2K (CADR A)))))
                                     (T
                                      (LPRIE (LIST " Bad arg of DF " EXPN)))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (COND
                             ((EQUAL (CAR A) 0)
                              (COND ((ATOM (CADR A)) (CADR A))
                                    ((AND (EQUAL (CDR (CADR A)) 1)
                                          (NUMBERP (CAR (CADR A))))
                                     (CAR (CADR A)))
                                    (T (*Q2K (CADR A)))))
                             (T (LPRIE (LIST " Bad arg of DF " EXPN)))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EQUAL VT 0) (RETURN (LIST 0 (SIMPDF (CONS (LIST '*SQ ARG T) REST)))))
       ((EQUAL VT 1)
        (RETURN
         (LIST 1
               (PROG (A FORALL-RESULT FORALL-ENDPTR)
                 (SETQ A ARG)
                 (COND ((NULL A) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (SIMPDF (CONS (LIST '*SQ A T) REST)))
                                   (CAR A))
                                  NIL)))
                LOOPLABEL
                 (SETQ A (CDR A))
                 (COND ((NULL A) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A) (SIMPDF (CONS (LIST '*SQ A T) REST)))
                           (CAR A))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
       ((EQUAL VT 2)
        (RETURN
         (LIST 2
               (PROG (A FORALL-RESULT FORALL-ENDPTR)
                 (SETQ A ARG)
                 (COND ((NULL A) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ B A)
                                       (COND ((NULL B) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (B)
                                                           (SIMPDF
                                                            (CONS
                                                             (LIST '*SQ B T)
                                                             REST)))
                                                         (CAR B))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ B (CDR B))
                                       (COND ((NULL B) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (SIMPDF
                                                    (CONS (LIST '*SQ B T)
                                                          REST)))
                                                 (CAR B))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR A))
                                  NIL)))
                LOOPLABEL
                 (SETQ A (CDR A))
                 (COND ((NULL A) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A)
                             (PROG (B FORALL-RESULT FORALL-ENDPTR)
                               (SETQ B A)
                               (COND ((NULL B) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (SIMPDF
                                                    (CONS (LIST '*SQ B T)
                                                          REST)))
                                                 (CAR B))
                                                NIL)))
                              LOOPLABEL
                               (SETQ B (CDR B))
                               (COND ((NULL B) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (B)
                                           (SIMPDF
                                            (CONS (LIST '*SQ B T) REST)))
                                         (CAR B))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                           (CAR A))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
       (T (LPRIE (LIST " Bad tensor in DF " EXPN)))))) 
(PUT 'DF 'EXPRESS 'DFEXPRESS) 
(PUT 'DIFFEXPRESS 'NUMBER-OF-ARGS 1) 
(PUT 'DIFFEXPRESS 'DEFINED-ON-LINE '876) 
(PUT 'DIFFEXPRESS 'DEFINED-IN-FILE 'FIDE/EXPRES.RED) 
(PUT 'DIFFEXPRESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFFEXPRESS (EXPN)
    (PROG (ARG VT REST)
      (SETQ ARG (CADR (CAR EXPN)))
      (SETQ VT (CAR (CAR EXPN)))
      (SETQ REST (CDR EXPN))
      (SETQ REST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A REST)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (COND
                                     ((EQUAL (CAR A) 0)
                                      (COND ((ATOM (CADR A)) (CADR A))
                                            ((AND (EQUAL (CDR (CADR A)) 1)
                                                  (NUMBERP (CAR (CADR A))))
                                             (CAR (CADR A)))
                                            (T (*Q2K (CADR A)))))
                                     (T
                                      (LPRIE
                                       (LIST " Bad arg of DIFF " EXPN)))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (COND
                             ((EQUAL (CAR A) 0)
                              (COND ((ATOM (CADR A)) (CADR A))
                                    ((AND (EQUAL (CDR (CADR A)) 1)
                                          (NUMBERP (CAR (CADR A))))
                                     (CAR (CADR A)))
                                    (T (*Q2K (CADR A)))))
                             (T (LPRIE (LIST " Bad arg of DIFF " EXPN)))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EQUAL VT 0)
        (RETURN (LIST 0 (SIMP (CONS 'DIFF (CONS (PREPSQ ARG) REST))))))
       ((EQUAL VT 1)
        (RETURN
         (LIST 1
               (PROG (A FORALL-RESULT FORALL-ENDPTR)
                 (SETQ A ARG)
                 (COND ((NULL A) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (SIMP
                                      (CONS 'DIFF (CONS (PREPSQ A) REST))))
                                   (CAR A))
                                  NIL)))
                LOOPLABEL
                 (SETQ A (CDR A))
                 (COND ((NULL A) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A)
                             (SIMP (CONS 'DIFF (CONS (PREPSQ A) REST))))
                           (CAR A))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
       ((EQUAL VT 2)
        (RETURN
         (LIST 2
               (PROG (A FORALL-RESULT FORALL-ENDPTR)
                 (SETQ A ARG)
                 (COND ((NULL A) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ B A)
                                       (COND ((NULL B) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (B)
                                                           (SIMP
                                                            (CONS 'DIFF
                                                                  (CONS
                                                                   (PREPSQ B)
                                                                   REST))))
                                                         (CAR B))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ B (CDR B))
                                       (COND ((NULL B) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (SIMP
                                                    (CONS 'DIFF
                                                          (CONS (PREPSQ B)
                                                                REST))))
                                                 (CAR B))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR A))
                                  NIL)))
                LOOPLABEL
                 (SETQ A (CDR A))
                 (COND ((NULL A) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A)
                             (PROG (B FORALL-RESULT FORALL-ENDPTR)
                               (SETQ B A)
                               (COND ((NULL B) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (SIMP
                                                    (CONS 'DIFF
                                                          (CONS (PREPSQ B)
                                                                REST))))
                                                 (CAR B))
                                                NIL)))
                              LOOPLABEL
                               (SETQ B (CDR B))
                               (COND ((NULL B) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (B)
                                           (SIMP
                                            (CONS 'DIFF
                                                  (CONS (PREPSQ B) REST))))
                                         (CAR B))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                           (CAR A))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))
       (T (LPRIE (LIST " Bad tensor in DIFF " EXPN)))))) 
(PUT 'DIFF 'EXPRESS 'DIFFEXPRESS) 
(REMPROP 'DIFF 'NUMBER-OF-ARGS) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SCALEFACTORS (LIST 3 'X 'Y 'Z 'X 'Y 'Z)) 
(ENDMODULE) 