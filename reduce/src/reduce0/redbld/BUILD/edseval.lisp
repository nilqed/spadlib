(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSEVAL)) 
(FLUID '(CFRMCOB* CFRMCRD* CFRMDRV* CFRMRSX* XVARS* KORD*)) 
(GLOBAL '(*SQVAR*)) 
(PUT 'EDS 'TAG '!EDS!) 
(PUT '!EDS! 'RTYPEFN 'QUOTEEDS) 
(PUT 'QUOTEEDS 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTEEDS 'DEFINED-ON-LINE '58) 
(PUT 'QUOTEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'QUOTEEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTEEDS (U) 'EDS) 
(COND
 ((NOT (MEMQ (GET 'LIST 'RTYPEFN) (LIST 'QUOTELIST 'EDSORLIST)))
  (LPRIM (LIST "Changing list rtypefn from" (GET 'LIST 'RTYPEFN))))) 
(PUT 'LIST 'RTYPEFN 'EDSORLIST) 
(PUT 'EDSORLIST 'NUMBER-OF-ARGS 1) 
(PUT 'EDSORLIST 'DEFINED-ON-LINE '66) 
(PUT 'EDSORLIST 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSORLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSORLIST (U)
    (COND ((AND U (EQUAL (GETRTYPE (CAR U)) 'EDS)) 'EDS) (T 'LIST))) 
(PUT 'EDS 'EVFN 'EDSEVAL) 
(PUT 'EDSEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'EDSEVAL 'DEFINED-ON-LINE '78) 
(PUT 'EDSEVAL 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSEVAL (U V)
    (COND
     ((ATOM U)
      (EDSEVAL (COND ((FLAGP U 'SHARE) (EVAL U)) (T (CADR (GET U 'AVALUE))))
       V))
     ((EDSP U) (RESIMPEDS* U))
     ((XEDSP U)
      (MKXEDS
       (CONS 'LIST
             (PROG (S FORALL-RESULT FORALL-ENDPTR)
               (SETQ S (MKXEDS0 U))
               (COND ((NULL S) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (S) (RESIMPEDS* S)) (CAR S))
                                     NIL)))
              LOOPLABEL
               (SETQ S (CDR S))
               (COND ((NULL S) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (S) (RESIMPEDS* S)) (CAR S)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
     ((SETQ V (GET (CAR U) 'EDSFN))
      (MKXEDS
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F (EDSEXPAND (REVLIS (CDR U))))
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (F)
                                   (COND
                                    ((FLAGP (CAR U) 'NOSPREAD)
                                     (EDSPROTECT (LIST V F)))
                                    (T (EDSPROTECT (CONS V F)))))
                                 (CAR F))
                                NIL)))
              LOOPLABEL
               (SETQ F (CDR F))
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (F)
                           (COND
                            ((FLAGP (CAR U) 'NOSPREAD) (EDSPROTECT (LIST V F)))
                            (T (EDSPROTECT (CONS V F)))))
                         (CAR F))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
     (T (RERROR 'EDS 0 (LIST "Illegal operation on EDS"))))) 
(PUT 'RESIMPEDS* 'NUMBER-OF-ARGS 1) 
(PUT 'RESIMPEDS* 'DEFINED-ON-LINE '100) 
(PUT 'RESIMPEDS* 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'RESIMPEDS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESIMPEDS* (S)
    (COND (((LAMBDA (V) (AND V (CAR V))) (GETEDS S 'SQVAR)) S)
          (T (RESIMPEDS S)))) 
(PUT 'EDSEXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'EDSEXPAND 'DEFINED-ON-LINE '107) 
(PUT 'EDSEXPAND 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSEXPAND (U)
    (COND ((NULL U) (LIST U))
          ((NOT (XEDSP (CAR U)))
           (PROG (W FORALL-RESULT FORALL-ENDPTR)
             (SETQ W (EDSEXPAND (CDR U)))
             (COND ((NULL W) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (W) (CONS (CAR U) W)) (CAR W))
                                   NIL)))
            LOOPLABEL
             (SETQ W (CDR W))
             (COND ((NULL W) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (W) (CONS (CAR U) W)) (CAR W)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          (T
           (PROG (S FORALL-RESULT FORALL-ENDPTR)
             (SETQ S (MKXEDS0 (CAR U)))
            STARTOVER
             (COND ((NULL S) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (S)
                        (PROG (W FORALL-RESULT FORALL-ENDPTR)
                          (SETQ W (EDSEXPAND (CDR U)))
                          (COND ((NULL W) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (W) (CONS S W)) (CAR W))
                                           NIL)))
                         LOOPLABEL
                          (SETQ W (CDR W))
                          (COND ((NULL W) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (W) (CONS S W)) (CAR W)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR S)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ S (CDR S))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL S) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (S)
                        (PROG (W FORALL-RESULT FORALL-ENDPTR)
                          (SETQ W (EDSEXPAND (CDR U)))
                          (COND ((NULL W) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (W) (CONS S W)) (CAR W))
                                           NIL)))
                         LOOPLABEL
                          (SETQ W (CDR W))
                          (COND ((NULL W) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (W) (CONS S W)) (CAR W)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR S)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ S (CDR S))
             (GO LOOPLABEL))))) 
(PUT 'EDSEXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'EDSEXPAND 'DEFINED-ON-LINE '119) 
(PUT 'EDSEXPAND 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSEXPAND (U)
    (COND ((OR (NULL U) (NOT (XEDSP (CAR U)))) (LIST U))
          (T
           (PROG (S FORALL-RESULT FORALL-ENDPTR)
             (SETQ S (MKXEDS0 (CAR U)))
             (COND ((NULL S) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (S) (CONS S (CDR U))) (CAR S))
                                   NIL)))
            LOOPLABEL
             (SETQ S (CDR S))
             (COND ((NULL S) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (S) (CONS S (CDR U))) (CAR S)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'EDSPROTECT 'NUMBER-OF-ARGS 1) 
(PUT 'EDSPROTECT 'DEFINED-ON-LINE '128) 
(PUT 'EDSPROTECT 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSPROTECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSPROTECT (U)
    (PROG (M OK OD XVARS*)
      (PROG (V)
        (SETQ V (CDR U))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((AND (NULL M) (EDSP V)) (SETQ M V)))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ OK KORD*)
      (SETQ OD (APPEND (GET 'D 'KVALUE) NIL))
      (COND (M (SETQ M (SETCFRM (EDS_CFRM* M)))))
      (SETQ U
              (ERRORSET*
               (CONS (CAR U)
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CDR U))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (MKQUOTE J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (MKQUOTE J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               T))
      (COND (M (SETCFRM M)))
      (SETKORDER OK)
      (COND (OD (PUT 'D 'KVALUE OD)) (T (REMPROP 'D 'KVALUE)))
      (COND ((ERRORP U) (ERROR1)) (T (RETURN (CAR U)))))) 
(PUT 'EDS_CFRM* 'NUMBER-OF-ARGS 1) 
(PUT 'EDS_CFRM* 'DEFINED-ON-LINE '149) 
(PUT 'EDS_CFRM* 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDS_CFRM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDS_CFRM* (S)
    (PROG (M)
      (SETQ M (COPYCFRM (CADDR (CDR S))))
      (SETCAR (CDDR M) (SETDIFF (CADDR M) (EDSINDCRD S)))
      (RETURN M))) 
(PUT 'EDSCOB 'NUMBER-OF-ARGS 1) 
(PUT 'EDSCOB 'DEFINED-ON-LINE '159) 
(PUT 'EDSCOB 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSCOB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSCOB (S) (CADR (CADDR (CDR S)))) 
(PUT 'EDSCRD 'NUMBER-OF-ARGS 1) 
(PUT 'EDSCRD 'DEFINED-ON-LINE '165) 
(PUT 'EDSCRD 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSCRD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSCRD (S) (CADDR (CADDR (CDR S)))) 
(PUT 'EDSINDCRD 'NUMBER-OF-ARGS 1) 
(PUT 'EDSINDCRD 'DEFINED-ON-LINE '170) 
(PUT 'EDSINDCRD 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSINDCRD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSINDCRD (S)
    (PROG (I J)
      (SETQ I (INDKRNS S))
      (SETQ J
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K I)
               STARTOVER
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                         (CAR K)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ K (CDR K))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (K) (COND ((EXACT K) (LIST (CADR K)))))
                         (CAR K)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ K (CDR K))
                (GO LOOPLABEL)))
      (COND ((EQUAL (LENGTH J) (LENGTH I)) (RETURN J)))
      (SETQ J
              (APPEND J
                      (PROG (C FORALL-RESULT FORALL-ENDPTR)
                        (SETQ C (SETDIFF (EDSCRD S) J))
                       STARTOVER
                        (COND ((NULL C) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (C)
                                   (COND ((MEMQ (CAAR (EXDFK C)) I) (LIST C))))
                                 (CAR C)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ C (CDR C))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL C) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (C)
                                   (COND ((MEMQ (CAAR (EXDFK C)) I) (LIST C))))
                                 (CAR C)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ C (CDR C))
                        (GO LOOPLABEL))))
      (COND ((EQUAL (LENGTH J) (LENGTH I)) (RETURN J)))
      (RETURN
       (COND
        ((GREATERP (LENGTH J) (LENGTH I))
         (REVERSE
          (PNTH (REVERSE J) (PLUS 1 (DIFFERENCE (LENGTH J) (LENGTH I))))))
        (T J))))) 
(PUT 'LIST 'EDSFN 'LISTEDS) 
(FLAG '(LIST) 'NOSPREAD) 
(PUT 'LISTEDS 'NUMBER-OF-ARGS 1) 
(PUT 'LISTEDS 'DEFINED-ON-LINE '194) 
(PUT 'LISTEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'LISTEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTEDS (U) (CONS 'LIST U)) 
(PUT 'MKEDS 'NUMBER-OF-ARGS 1) 
(PUT 'MKEDS 'DEFINED-ON-LINE '202) 
(PUT 'MKEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'MKEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKEDS (U) (CONS '!EDS! U)) 
(PUT 'MKXEDS 'NUMBER-OF-ARGS 1) 
(PUT 'MKXEDS 'DEFINED-ON-LINE '207) 
(PUT 'MKXEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'MKXEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKXEDS (U)
    (COND ((EQUAL (LENGTH (SETQ U (MKXEDS0 U))) 1) (CAR U)) (T (CONS 'LIST U)))) 
(PUT 'MKXEDS0 'NUMBER-OF-ARGS 1) 
(PUT 'MKXEDS0 'DEFINED-ON-LINE '214) 
(PUT 'MKXEDS0 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'MKXEDS0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKXEDS0 (U)
    (COND ((EDSP U) (LIST U))
          ((EQCAR U 'LIST)
           (PROG (V FORALL-RESULT FORALL-ENDPTR)
             (SETQ V (CDR U))
            STARTOVER
             (COND ((NULL V) (RETURN NIL)))
             (SETQ FORALL-RESULT ((LAMBDA (V) (MKXEDS0 V)) (CAR V)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ V (CDR V))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL V) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR ((LAMBDA (V) (MKXEDS0 V)) (CAR V)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ V (CDR V))
             (GO LOOPLABEL)))
          (T (TYPERR U 'EDS)))) 
(PUT 'EMPTYEDS 'NUMBER-OF-ARGS 0) 
(PUT 'EMPTYEDS 'DEFINED-ON-LINE '223) 
(PUT 'EMPTYEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EMPTYEDS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EMPTYEDS NIL
    (MKEDS
     (LIST (LIST (CONS (CONS 1 (CONS 1 1)) NIL)) (LIST) (EMPTYCFRM) (LIST)))) 
(PUT 'EMPTYEDSP 'NUMBER-OF-ARGS 1) 
(PUT 'EMPTYEDSP 'DEFINED-ON-LINE '228) 
(PUT 'EMPTYEDSP 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EMPTYEDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EMPTYEDSP (S) (MEMBER (CONS (CONS 1 (CONS 1 1)) NIL) (CADR S))) 
(PUT 'EDSP 'NUMBER-OF-ARGS 1) 
(PUT 'EDSP 'DEFINED-ON-LINE '233) 
(PUT 'EDSP 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSP (U) (EQCAR U '!EDS!)) 
(PUT 'XEDSP 'NUMBER-OF-ARGS 1) 
(PUT 'XEDSP 'DEFINED-ON-LINE '238) 
(PUT 'XEDSP 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'XEDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XEDSP (U) (OR (EDSP U) (AND (EQCAR U 'LIST) (CDR U) (XEDSP (CADR U))))) 
(PUT 'PURGEXEDS 'NUMBER-OF-ARGS 1) 
(PUT 'PURGEXEDS 'DEFINED-ON-LINE '244) 
(PUT 'PURGEXEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'PURGEXEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGEXEDS (S)
    (PROG ()
      (SETQ S
              (PROG (S0 FORALL-RESULT FORALL-ENDPTR)
                (SETQ S0 (MKXEDS0 S))
               STARTOVER
                (COND ((NULL S0) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S0) (COND ((NOT (EMPTYEDSP S0)) (LIST S0))))
                         (CAR S0)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S0 (CDR S0))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S0) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S0) (COND ((NOT (EMPTYEDSP S0)) (LIST S0))))
                         (CAR S0)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S0 (CDR S0))
                (GO LOOPLABEL)))
      (RETURN
       (COND ((NULL S) (EMPTYEDS)) ((EQUAL (LENGTH S) 1) (CAR S))
             (T (CONS 'LIST S)))))) 
(PUT 'EDS 'RTYPEFN 'QUOTEEDS) 
(PUT 'EDS 'EDSFN '*A2EDS) 
(FLAG '(EDS) 'NOSPREAD) 
(PUT '*A2EDS 'NUMBER-OF-ARGS 1) 
(PUT '*A2EDS 'DEFINED-ON-LINE '264) 
(PUT '*A2EDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT '*A2EDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2EDS (S)
    (PROG (SYS IND CFRM PROPS)
      (COND
       ((OR (LESSP (LENGTH S) 2) (GREATERP (LENGTH S) 4))
        (RERROR 'EDS 0 (LIST "Wrong number of arguments to EDS"))))
      (SETQ SYS (*A2SYS (CAR S)))
      (COND ((EQCAR (CADR S) 'LIST) (SETQ IND (*A2SYS (CADR S))))
            ((GETRTYPE (CADR S)) (TYPERR (CADR S) "independence form"))
            ((NULL (SETQ IND (XDECOMPOSEPF (XPARTITOP (CADR S)))))
             (TYPERR (CADR S) "independence form (not decomposable)")))
      (PROG (L)
        (SETQ L (CDDR S))
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (COND ((CFRMP L) (SETQ CFRM L))
                 ((AND (EQCAR L 'LIST) (EDSPROPSP (CDR L)))
                  (SETQ PROPS (CDR L)))
                 (T (RERROR 'EDS 0 "Badly formed EDS"))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (SETQ IND
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F IND)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (COND ((EQUAL (DEGREEPF F) 1) F)
                                          (T
                                           (TYPERR F "independence 1-form"))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (COND ((EQUAL (DEGREEPF F) 1) F)
                                  (T (TYPERR F "independence 1-form"))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL CFRM) (SETQ CFRM (*SYS2CFRM (APPEND SYS IND)))))
      (SETQ PROPS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X PROPS)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (COND
                                     ((NOT (IDP (CADR X)))
                                      (RERROR 'EDS 0
                                              "Badly formed properties in EDS"))
                                     (T
                                      (CONS (CADR X)
                                            (COND
                                             ((EQCAR (CADDR X) 'LIST)
                                              (REVLIS
                                               (CDR
                                                (INDEXEXPANDEVAL
                                                 (LIST (CADDR X))))))
                                             (T (CADDR X)))))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (COND
                             ((NOT (IDP (CADR X)))
                              (RERROR 'EDS 0 "Badly formed properties in EDS"))
                             (T
                              (CONS (CADR X)
                                    (COND
                                     ((EQCAR (CADDR X) 'LIST)
                                      (REVLIS
                                       (CDR
                                        (INDEXEXPANDEVAL (LIST (CADDR X))))))
                                     (T (CADDR X)))))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ S (MKEDS (LIST SYS IND CFRM PROPS)))
      (RETURN (EDSPROTECT (LIST 'CHECKEDS S))))) 
(PUT 'EDSPROPSP 'NUMBER-OF-ARGS 1) 
(PUT 'EDSPROPSP 'DEFINED-ON-LINE '296) 
(PUT 'EDSPROPSP 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSPROPSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSPROPSP (U) (OR (NULL U) (AND (EQEXPR (CAR U)) (EDSPROPSP (CDR U))))) 
(PUT '!EDS! 'PRIFN 'EDSPRINT) 
(PUT '!EDS! 'FANCY-REFORM '*EDS2A) 
(PUT 'EDS 'TEXPRIFN 'TEXPRIEDS) 
(PUT 'EDSPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'EDSPRINT 'DEFINED-ON-LINE '311) 
(PUT 'EDSPRINT 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSPRINT (S) (MAPRIN (*EDS2A S))) 
(PUT '*EDS2A 'NUMBER-OF-ARGS 1) 
(PUT '*EDS2A 'DEFINED-ON-LINE '317) 
(PUT '*EDS2A 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT '*EDS2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *EDS2A (S) (EDSPROTECT (LIST '*EDS2A1 S))) 
(PUT '*EDS2A1 'NUMBER-OF-ARGS 1) 
(PUT '*EDS2A1 'DEFINED-ON-LINE '322) 
(PUT '*EDS2A1 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT '*EDS2A1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *EDS2A1 (S)
    (COND
     (*NAT
      (CONS "EDS"
            (LIST
             (CONS 'LIST
                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                     (SETQ F (CADR S))
                     (COND ((NULL F) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (F) (PREPPF (REPARTIT F)))
                                       (CAR F))
                                      NIL)))
                    LOOPLABEL
                     (SETQ F (CDR F))
                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                   NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
             (COND
              ((CADDR S)
               (MKNWEDGE
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F (CADDR S))
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
              (T (CONS 'LIST NIL))))))
     (T
      (CONS "eds"
            (LIST
             (CONS 'LIST
                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                     (SETQ F (CADR S))
                     (COND ((NULL F) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (F) (PREPPF (REPARTIT F)))
                                       (CAR F))
                                      NIL)))
                    LOOPLABEL
                     (SETQ F (CDR F))
                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                   NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
             (COND
              ((CADDR S)
               (MKNWEDGE
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F (CADDR S))
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (F) (PREPPF (REPARTIT F))) (CAR F))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
              (T (CONS 'LIST NIL)))
             (*CFRM2A (CADDR (CDR S))) (EDSPROPERTIES S)))))) 
(PUT 'TEXPRIEDS 'NUMBER-OF-ARGS 3) 
(PUT 'TEXPRIEDS 'DEFINED-ON-LINE '353) 
(PUT 'TEXPRIEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'TEXPRIEDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TEXPRIEDS (U V W)
    (COND
     ((EDSP U)
      (TEXVARPRI
       (COND ((GET 'HODGE 'TEXNAME) (*EDS2A U))
             (T (CONS 'TEXPRIEDSOP (*EDS2A U))))
       V W))
     (T
      (TEXVARPRI
       (CONS 'LIST
             (PROG (S FORALL-RESULT FORALL-ENDPTR)
               (SETQ S (GETRLIST U))
               (COND ((NULL S) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (S)
                                   (COND ((GET 'HODGE 'TEXNAME) (*EDS2A S))
                                         (T (CONS 'TEXPRIEDSOP (*EDS2A S)))))
                                 (CAR S))
                                NIL)))
              LOOPLABEL
               (SETQ S (CDR S))
               (COND ((NULL S) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (S)
                           (COND ((GET 'HODGE 'TEXNAME) (*EDS2A S))
                                 (T (CONS 'TEXPRIEDSOP (*EDS2A S)))))
                         (CAR S))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
       V W)))) 
(PUT 'TEXPRIEDSOP 'SIMPFN 'SIMPTEXPRIEDSOP) 
(PUT 'SIMPTEXPRIEDSOP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTEXPRIEDSOP 'DEFINED-ON-LINE '369) 
(PUT 'SIMPTEXPRIEDSOP 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'SIMPTEXPRIEDSOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTEXPRIEDSOP (U) (CONS (LIST (CONS (CONS U 1) 1)) 1)) 
(PUT 'SYSTEM 'FORMFN 'FORMSYSTEM) 
(PUT 'FORMSYSTEM 'NUMBER-OF-ARGS 3) 
(PUT 'FORMSYSTEM 'DEFINED-ON-LINE '380) 
(PUT 'FORMSYSTEM 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'FORMSYSTEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMSYSTEM (U V MODE)
    (PROG (X)
      (SETQ X (FORMLIS (CDR U) V MODE))
      (RETURN
       (COND ((EQUAL MODE 'SYMBOLIC) (CONS 'SYSTEM X))
             ((AND X (STRINGP (CAR X)))
              (CONS 'LIST (CONS (MKQUOTE 'SYSTEM) X)))
             (T (CONS 'LIST (CONS (MKQUOTE 'SYSTEMEDS) X))))))) 
(PUT 'SYSTEMEDS 'RTYPEFN 'QUOTELIST) 
(PUT 'SYSTEMEDS 'LISTFN 'SYSEVAL) 
(PUT 'SYSEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'SYSEVAL 'DEFINED-ON-LINE '396) 
(PUT 'SYSEVAL 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'SYSEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYSEVAL (S V)
    (COND ((NOT (XEDSP (SETQ S (REVAL1 (CAR S) T)))) (TYPERR S 'EDS))
          ((EDSP S) (*SYS2A1 (CADR S) V))
          (T
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (*SYS2A1 (CADR X) V)) (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (*SYS2A1 (CADR X) V)) (CAR X))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'INDEPENDENCE 'RTYPEFN 'QUOTELIST) 
(PUT 'INDEPENDENCE 'LISTFN 'INDEVAL) 
(PUT 'INDEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'INDEVAL 'DEFINED-ON-LINE '408) 
(PUT 'INDEVAL 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'INDEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDEVAL (S V)
    (COND ((NOT (XEDSP (SETQ S (REVAL1 (CAR S) T)))) (TYPERR S 'EDS))
          ((EDSP S)
           (CONS 'LIST
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F (CADDR S))
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F))
                                         NIL)))
                  LOOPLABEL
                   (SETQ F (CDR F))
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (CONS 'LIST
                                             (PROG (F FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ F (CADDR X))
                                               (COND ((NULL F) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (F)
                                                                   (*PF2A1 F
                                                                           V))
                                                                 (CAR F))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ F (CDR F))
                                               (COND
                                                ((NULL F)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (F)
                                                           (*PF2A1 F V))
                                                         (CAR F))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (CONS 'LIST
                                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ F (CADDR X))
                                       (COND ((NULL F) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (F)
                                                           (*PF2A1 F V))
                                                         (CAR F))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ F (CDR F))
                                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (F) (*PF2A1 F V))
                                                 (CAR F))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'PROPERTIES 'RTYPEFN 'QUOTELIST) 
(PUT 'PROPERTIES 'LISTFN 'PROPERTIESEVAL) 
(PUT 'PROPERTIESEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'PROPERTIESEVAL 'DEFINED-ON-LINE '420) 
(PUT 'PROPERTIESEVAL 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'PROPERTIESEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROPERTIESEVAL (S V)
    (COND ((NOT (XEDSP (SETQ S (REVAL1 (CAR S) T)))) (TYPERR S 'EDS))
          ((EDSP S) (EDSPROPERTIES S))
          (T
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (EDSPROPERTIES X)) (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (EDSPROPERTIES X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'EDSPROPERTIES 'NUMBER-OF-ARGS 1) 
(PUT 'EDSPROPERTIES 'DEFINED-ON-LINE '428) 
(PUT 'EDSPROPERTIES 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSPROPERTIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSPROPERTIES (S)
    (CONS 'LIST
          (PROG (P FORALL-RESULT FORALL-ENDPTR)
            (SETQ P (CADDR (CDDR S)))
           STARTOVER
            (COND ((NULL P) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    ((LAMBDA (P)
                       (COND
                        ((NOT (FLAGP (CAR P) 'HIDDEN))
                         (LIST
                          (LIST 'EQUAL (CAR P)
                                (COND ((PAIRP (CDR P)) (CONS 'LIST (CDR P)))
                                      (T (CDR P))))))))
                     (CAR P)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
            (SETQ P (CDR P))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND ((NULL P) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    ((LAMBDA (P)
                       (COND
                        ((NOT (FLAGP (CAR P) 'HIDDEN))
                         (LIST
                          (LIST 'EQUAL (CAR P)
                                (COND ((PAIRP (CDR P)) (CONS 'LIST (CDR P)))
                                      (T (CDR P))))))))
                     (CAR P)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ P (CDR P))
            (GO LOOPLABEL)))) 
(PUT 'EDS 'LENGTHFN 'EDSLENGTH) 
(PUT 'EDSLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'EDSLENGTH 'DEFINED-ON-LINE '437) 
(PUT 'EDSLENGTH 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSLENGTH (S) (COND ((EDSP S) 1) (T (LENGTH (CDR S))))) 
(PUT 'EDSPART 'NUMBER-OF-ARGS 2) 
(PUT 'EDSPART 'DEFINED-ON-LINE '443) 
(PUT 'EDSPART 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSPART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSPART (S N)
    (COND ((EQUAL N 0) 'EDS) ((EQUAL N 1) (*SYS2A (CADR S)))
          ((EQUAL N 2) (*SYS2A (CADDR S))) ((EQUAL N 3) (CADDR (CDR S)))
          ((EQUAL N 4) (EDSPROPERTIES S)) (T (PARTERR S N)))) 
(PUT '!EDS! 'PARTOP 'EDSPART) 
(PUT 'EDSSETPART 'NUMBER-OF-ARGS 3) 
(PUT 'EDSSETPART 'DEFINED-ON-LINE '455) 
(PUT 'EDSSETPART 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'EDSSETPART 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EDSSETPART (S L R) (RERROR 'EDS 0 "Part setting disabled on EDS operator")) 
(PUT '!EDS! 'SETPARTOP 'EDSSETPART) 
(PUT 'MAPEDS 'NUMBER-OF-ARGS 2) 
(PUT 'MAPEDS 'DEFINED-ON-LINE '462) 
(PUT 'MAPEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'MAPEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAPEDS (FN S)
    (PROG ()
      (SETQ S (COPYEDS S))
      (SETCAR (CDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (XPARTITOP (APPLY1 FN (*PF2A F))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F) (XPARTITOP (APPLY1 FN (*PF2A F))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR S)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (XPARTITOP (APPLY1 FN (*PF2A F))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F) (XPARTITOP (APPLY1 FN (*PF2A F))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (EDSPROTECT (LIST 'CHECKEDS S))))) 
(PUT '!EDS! 'MAPFN 'MAPEDS) 
(PUT 'CHECKEDS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECKEDS 'DEFINED-ON-LINE '481) 
(PUT 'CHECKEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'CHECKEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECKEDS (S)
    (PROG (M N)
      (SETQ S (PURGEEDS S))
      (SETQ N (*SYS2CFRM (APPEND (CADDR S) (CADR S))))
      (SETQ M (COPYCFRM (CADDR (CDR S))))
      (COND
       ((NOT (SUBSETP (CADDR N) (CADDR M)))
        (RERROR 'EDS 0 "EDS not expressed in terms of coframing coordinates")))
      (COND
       ((NOT (SUBSETP (CADR N) (CADR M)))
        (RERROR 'EDS 0 "EDS not expressed in terms of coframing cobasis")))
      (SETCAR (CDDR (CDDR M)) (UNION (CADDR (CDDR N)) (CADDR (CDDR M))))
      (SETCAR (CDDR (CDR M)) (UNION (CADDR (CDR N)) (CADDR (CDR M))))
      (SETCAR (CDDR (CDR S)) (PURGECFRM M))
      (PUTEDS S 'SQVAR *SQVAR*)
      (RETURN (NORMALEDS S)))) 
(PUT 'RESIMPEDS 'NUMBER-OF-ARGS 1) 
(PUT 'RESIMPEDS 'DEFINED-ON-LINE '504) 
(PUT 'RESIMPEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'RESIMPEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESIMPEDS (S)
    (PROG (R OK)
      (SETQ R (COPYEDS S))
      (SETQ OK (CFRMSWAPKORD (EDSCOB R) (LIST)))
      (SETCAR (CDR R)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR R))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR R)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR R))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR (CDR R)) (RESIMPCFRM (CADDR (CDR R))))
      (COND
       ((NEQ (REVLIS (GETEDS R 'JET0)) (GETEDS S 'JET0)) (REMPROPEDS R 'JET0)))
      (SETKORDER OK)
      (COND ((EQUAL R S) (PROGN (PUTEDS S 'SQVAR *SQVAR*) (RETURN S))))
      (RETURN (EDSPROTECT (LIST 'CHECKEDS R))))) 
(FLAG '(SQVAR) 'HIDDEN) 
(PUT 'CLEANUP 'RTYPEFN 'GETRTYPECAR) 
(PUT 'CLEANUP 'EDSFN 'CLEANEDS) 
(PUT 'CLEANEDS 'NUMBER-OF-ARGS 1) 
(PUT 'CLEANEDS 'DEFINED-ON-LINE '530) 
(PUT 'CLEANEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'CLEANEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEANEDS (S)
    (PROG (R J)
      (SETQ S (COPYEDS S))
      (SETQ J (GETEDS S 'JET0))
      (SETCAR (CDDR (CDDR S)) (LIST))
      (COND (J (PUTEDS S 'JET0 J)))
      (SETQ R (RESIMPEDS S))
      (RETURN (COND ((EQ R S) (EDSPROTECT (LIST 'CHECKEDS S))) (T R))))) 
(PUT 'PURGEEDS 'NUMBER-OF-ARGS 1) 
(PUT 'PURGEEDS 'DEFINED-ON-LINE '544) 
(PUT 'PURGEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'PURGEEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGEEDS (S)
    (PROG ()
      (SETQ S (COPYEDS S))
      (SETCAR (CDDR (CDDR S))
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CADDR (CDDR S)))
               STARTOVER
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (P)
                           (COND ((NOT (FLAGP (CAR P) 'HIDDEN)) (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ P (CDR P))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (P)
                           (COND ((NOT (FLAGP (CAR P) 'HIDDEN)) (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ P (CDR P))
                (GO LOOPLABEL)))
      (RETURN S))) 
(PUT 'PURGEEDS* 'NUMBER-OF-ARGS 1) 
(PUT 'PURGEEDS* 'DEFINED-ON-LINE '555) 
(PUT 'PURGEEDS* 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'PURGEEDS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGEEDS* (S)
    (PROG ()
      (SETQ S (COPYEDS S))
      (SETCAR (CDDR (CDDR S))
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CADDR (CDDR S)))
               STARTOVER
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (P)
                           (COND
                            ((OR (MEMQ (CAR P) (LIST 'SOLVED 'REDUCED 'SQVAR))
                                 (NOT (FLAGP (CAR P) 'HIDDEN)))
                             (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ P (CDR P))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (P)
                           (COND
                            ((OR (MEMQ (CAR P) (LIST 'SOLVED 'REDUCED 'SQVAR))
                                 (NOT (FLAGP (CAR P) 'HIDDEN)))
                             (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ P (CDR P))
                (GO LOOPLABEL)))
      (RETURN S))) 
(PUT 'PUTEDS 'NUMBER-OF-ARGS 3) 
(PUT 'PUTEDS 'DEFINED-ON-LINE '571) 
(PUT 'PUTEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'PUTEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PUTEDS (S K V)
    (COND
     ((NOT (EDSP S))
      (ERRDHH (LIST "Attempt to do puteds on" S "which is not an EDS")))
     ((NOT (IDP K))
      (ERRDHH (LIST "Attempt to do puteds with" K "which is not an id")))
     (T
      (PROG (P)
        (COND
         ((SETQ P (ASSOC K (CADDR (CDDR S))))
          (SETCAR (CDDR (CDDR S))
                  (CONS (CONS K V) (DELETE P (CADDR (CDDR S))))))
         (T (SETCAR (CDDR (CDDR S)) (CONS (CONS K V) (CADDR (CDDR S))))))
        (RETURN V))))) 
(PUT 'REMPROPEDS 'NUMBER-OF-ARGS 2) 
(PUT 'REMPROPEDS 'DEFINED-ON-LINE '587) 
(PUT 'REMPROPEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'REMPROPEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMPROPEDS (S K)
    (COND ((OR (NOT (EDSP S)) (NOT (IDP K))) NIL)
          (T
           (PROG (P)
             (COND
              ((SETQ P (ASSOC K (CADDR (CDDR S))))
               (SETCAR (CDDR (CDDR S)) (DELETE P (CADDR (CDDR S))))))
             (RETURN (COND (P (CDR P)))))))) 
(PUT 'GETEDS 'NUMBER-OF-ARGS 2) 
(PUT 'GETEDS 'DEFINED-ON-LINE '598) 
(PUT 'GETEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'GETEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETEDS (S K)
    (COND ((OR (NOT (EDSP S)) (NOT (IDP K))) NIL)
          (T ((LAMBDA (P) (COND (P (CDR P)))) (ASSOC K (CADDR (CDDR S))))))) 
(PUT 'FLAGTRUEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'FLAGTRUEEDS 'DEFINED-ON-LINE '607) 
(PUT 'FLAGTRUEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'FLAGTRUEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FLAGTRUEEDS (S K) (PROGN (PUTEDS S K 1) NIL)) 
(PUT 'KNOWNTRUEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'KNOWNTRUEEDS 'DEFINED-ON-LINE '612) 
(PUT 'KNOWNTRUEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'KNOWNTRUEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KNOWNTRUEEDS (S K) (EQUAL (GETEDS S K) 1)) 
(PUT 'REMTRUEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'REMTRUEEDS 'DEFINED-ON-LINE '617) 
(PUT 'REMTRUEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'REMTRUEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMTRUEEDS (S K) (PROGN (COND ((KNOWNTRUEEDS S K) (REMPROPEDS S K))) NIL)) 
(PUT 'FLAGFALSEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'FLAGFALSEEDS 'DEFINED-ON-LINE '622) 
(PUT 'FLAGFALSEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'FLAGFALSEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FLAGFALSEEDS (S K) (PROGN (PUTEDS S K 0) NIL)) 
(PUT 'KNOWNFALSEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'KNOWNFALSEEDS 'DEFINED-ON-LINE '627) 
(PUT 'KNOWNFALSEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'KNOWNFALSEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KNOWNFALSEEDS (S K) (EQUAL (GETEDS S K) 0)) 
(PUT 'REMFALSEEDS 'NUMBER-OF-ARGS 2) 
(PUT 'REMFALSEEDS 'DEFINED-ON-LINE '632) 
(PUT 'REMFALSEEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'REMFALSEEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMFALSEEDS (S K) (PROGN (COND ((KNOWNFALSEEDS S K) (REMPROPEDS S K))) NIL)) 
(PUT 'KNOWNEDS 'NUMBER-OF-ARGS 2) 
(PUT 'KNOWNEDS 'DEFINED-ON-LINE '637) 
(PUT 'KNOWNEDS 'DEFINED-IN-FILE 'EDS/EDSEVAL.RED) 
(PUT 'KNOWNEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KNOWNEDS (S K) (GETEDS S K)) 
(ENDMODULE) 