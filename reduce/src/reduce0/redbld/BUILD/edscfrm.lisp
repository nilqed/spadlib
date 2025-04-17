(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSCFRM)) 
(FLUID '(CFRMCOB* CFRMCRD* CFRMDRV* CFRMRSX* XVARS* KORD*)) 
(GLOBAL '(*SQVAR*)) 
(PUT 'CFRM 'TAG '!CFRM!) 
(PUT '!CFRM! 'RTYPEFN 'QUOTECFRM) 
(PUT 'QUOTECFRM 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTECFRM 'DEFINED-ON-LINE '59) 
(PUT 'QUOTECFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'QUOTECFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTECFRM (U) 'CFRM) 
(PUT 'CFRM 'EVFN 'CFRMEVAL) 
(PUT 'CFRMEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMEVAL 'DEFINED-ON-LINE '67) 
(PUT 'CFRMEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMEVAL (U V)
    (COND
     ((ATOM U)
      (CFRMEVAL (COND ((FLAGP U 'SHARE) (EVAL U)) (T (CADR (GET U 'AVALUE))))
       V))
     ((CFRMP U) U)
     ((SETQ V (GET (CAR U) 'CFRMFN))
      (COND ((FLAGP (CAR U) 'NOSPREAD) (CFRMPROTECT (LIST V (REVLIS (CDR U)))))
            (T (CFRMPROTECT (CONS V (REVLIS (CDR U)))))))
     (T (RERROR 'EDS 0 (LIST "Illegal operation on coframings"))))) 
(PUT 'CFRMPROTECT 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMPROTECT 'DEFINED-ON-LINE '84) 
(PUT 'CFRMPROTECT 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMPROTECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMPROTECT (U)
    (PROG (M OK OD XVARS*)
      (PROG (V)
        (SETQ V (CDR U))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((CFRMP V) (SETQ M V)))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ OK KORD*)
      (SETQ OD (APPEND (GET 'D 'KVALUE) NIL))
      (COND (M (SETQ M (SETCFRM M))))
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
(PUT 'MKCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'MKCFRM 'DEFINED-ON-LINE '107) 
(PUT 'MKCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'MKCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKCFRM (U) (CONS '!CFRM! U)) 
(PUT 'COPYCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'COPYCFRM 'DEFINED-ON-LINE '112) 
(PUT 'COPYCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'COPYCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPYCFRM (U)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P U)
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (P) P) (CAR P)) NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) P) (CAR P)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CFRMP 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMP 'DEFINED-ON-LINE '117) 
(PUT 'CFRMP 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMP (U) (EQCAR U '!CFRM!)) 
(PUT 'EMPTYCFRM 'NUMBER-OF-ARGS 0) 
(PUT 'EMPTYCFRM 'DEFINED-ON-LINE '122) 
(PUT 'EMPTYCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'EMPTYCFRM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EMPTYCFRM NIL (MKCFRM (LIST (LIST) (LIST) (LIST) (LIST)))) 
(PUT 'SET_COFRAMING 'PSOPFN 'SETCFRMEVAL) 
(PUT 'SETCFRMEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SETCFRMEVAL 'DEFINED-ON-LINE '132) 
(PUT 'SETCFRMEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'SETCFRMEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETCFRMEVAL (U)
    (PROG (M)
      (SETQ U
              (COND ((OR (NULL U) (EQUAL U (LIST NIL))) (SETCFRM (EMPTYCFRM)))
                    ((CFRMP (SETQ M (REVAL1 (CAR U) T))) (SETCFRM M))
                    ((EDSP M) (SETCFRM (CADDR (CDR M)))) (T (TYPERR U 'CFRM))))
      (RMSUBS)
      (RETURN U))) 
(PUT 'SETCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'SETCFRM 'DEFINED-ON-LINE '145) 
(PUT 'SETCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'SETCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETCFRM (M)
    (PROG (N)
      (SETQ N (GETCFRM))
      (COND ((EQUAL M N) (RETURN N)))
      (CFRMSWAPKORD (CADR M) (CADR N))
      (CFRMSWAPRULES (CADDR (CDR M)) (CADDR (CDR N)))
      (SETQ CFRMCOB* (CADR M))
      (SETQ CFRMCRD* (CADDR M))
      (SETQ CFRMDRV* (CADDR (CDR M)))
      ((LAMBDA (XVARS*)
         (SETQ CFRMRSX*
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (CADDR (CDDR M)))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (P) (XPARTITOP P)) (CAR P))
                                         NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (P) (XPARTITOP P)) (CAR P)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
       (CADDR M))
      (RETURN N))) 
(PUT 'CFRMSWAPKORD 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMSWAPKORD 'DEFINED-ON-LINE '163) 
(PUT 'CFRMSWAPKORD 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMSWAPKORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMSWAPKORD (NEW OLD)
    (SETKORDER (APPEND NEW (SETDIFF KORD* (APPEND NEW OLD))))) 
(PUT 'CFRMSWAPRULES 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMSWAPRULES 'DEFINED-ON-LINE '170) 
(PUT 'CFRMSWAPRULES 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMSWAPRULES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMSWAPRULES (NEW OLD)
    (PROG (SQ)
      (COND ((EQUAL NEW OLD) (RETURN NIL)))
      (SETQ SQ *SQVAR*)
      (COND (OLD (RULE-LIST OLD NIL)))
      (COND (NEW (RULE-LIST NEW T)))
      (SETQ *SQVAR* SQ)
      (SETCAR *SQVAR* T))) 
(PUT 'GETCFRM 'NUMBER-OF-ARGS 0) 
(PUT 'GETCFRM 'DEFINED-ON-LINE '185) 
(PUT 'GETCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'GETCFRM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GETCFRM NIL
    (MKCFRM
     (LIST CFRMCOB* CFRMCRD* CFRMDRV*
           (PROG (F FORALL-RESULT FORALL-ENDPTR)
             (SETQ F CFRMRSX*)
             (COND ((NULL F) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (F) (*PF2A F)) (CAR F)) NIL)))
            LOOPLABEL
             (SETQ F (CDR F))
             (COND ((NULL F) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (F) (*PF2A F)) (CAR F)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'COFRAMING 'RTYPEFN 'QUOTECFRM) 
(PUT 'COFRAMING 'CFRMFN '*A2CFRM) 
(FLAG '(COFRAMING) 'NOSPREAD) 
(PUT '*A2CFRM 'NUMBER-OF-ARGS 1) 
(PUT '*A2CFRM 'DEFINED-ON-LINE '200) 
(PUT '*A2CFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT '*A2CFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2CFRM (U)
    (COND ((NULL U) (GETCFRM))
          ((EQUAL (LENGTH U) 1)
           (COND ((CFRMP (CAR U)) (CAR U))
                 ((EDSP (CAR U)) (CADDR (CDR (CAR U))))
                 ((XEDSP (CAR U))
                  (CONS 'LIST
                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                          (SETQ S (GETRLIST (CAR U)))
                          (COND ((NULL S) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S) (CADDR (CDR S)))
                                            (CAR S))
                                           NIL)))
                         LOOPLABEL
                          (SETQ S (CDR S))
                          (COND ((NULL S) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (S) (CADDR (CDR S))) (CAR S))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
                 (T (*SYS2CFRM (*A2SYS (CAR U))))))
          (T (*A2CFRM1 U)))) 
(PUT '*A2CFRM1 'NUMBER-OF-ARGS 1) 
(PUT '*A2CFRM1 'DEFINED-ON-LINE '221) 
(PUT '*A2CFRM1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT '*A2CFRM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2CFRM1 (U)
    (PROG (COB CRD DRV RSX)
      (PROG (L)
        (SETQ L U)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (COND ((NULL (SETQ L (GETRLIST (INDEXEXPANDEVAL (LIST L))))) NIL)
                 ((EQEXPR (CAR L)) (SETQ DRV L))
                 ((EQCAR (CAR L) 'NEQ) (SETQ RSX L))
                 ((EQUAL (XDEGREE (CAR L)) 1) (SETQ COB L))
                 ((EQUAL (XDEGREE (CAR L)) 0) (SETQ CRD L))
                 (T (RERROR 'EDS 0 "Badly formed coframing"))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (SETQ COB
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K COB)
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (COND
                                     ((EQUAL (XDEGREE (SETQ K (*A2K K))) 1) K)
                                     (T (TYPERR K "cobasis element"))))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (COND ((EQUAL (XDEGREE (SETQ K (*A2K K))) 1) K)
                                  (T (TYPERR K "cobasis element"))))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CRD
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K CRD)
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K)
                                    (COND
                                     (((LAMBDA (XVARS*)
                                         (AND
                                          (EQUAL (XDEGREE (SETQ K (*A2K K))) 0)
                                          (XVARP K)))
                                       T)
                                      K)
                                     (T (TYPERR K "coordinate"))))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (K)
                            (COND
                             (((LAMBDA (XVARS*)
                                 (AND (EQUAL (XDEGREE (SETQ K (*A2K K))) 0)
                                      (XVARP K)))
                               T)
                              K)
                             (T (TYPERR K "coordinate"))))
                          (CAR K))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DRV
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R DRV)
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R)
                                    (COND ((EQEXPR R) R)
                                          (T (TYPERR R "structure equation"))))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (COND ((EQEXPR R) R)
                                  (T (TYPERR R "structure equation"))))
                          (CAR R))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RSX
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F RSX)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (COND
                                     ((EQCAR F 'NEQ)
                                      (REVAL1
                                       (LIST 'DIFFERENCE (CADR F) (CADDR F))
                                       NIL))
                                     (T
                                      (TYPERR F
                                              "restriction (only neq allowed)"))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (COND
                             ((EQCAR F 'NEQ)
                              (REVAL1 (LIST 'DIFFERENCE (CADR F) (CADDR F))
                                      NIL))
                             (T (TYPERR F "restriction (only neq allowed)"))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CHECKCFRM (MKCFRM (LIST COB CRD DRV RSX)))))) 
(PUT '*SYS2CFRM 'NUMBER-OF-ARGS 1) 
(PUT '*SYS2CFRM 'DEFINED-ON-LINE '255) 
(PUT '*SYS2CFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT '*SYS2CFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *SYS2CFRM (S)
    (PROG (CRD COB DRV RSX)
      (PROG ()
       WHILELABEL
        (COND ((NOT S) (RETURN NIL)))
        (PROG (NEW)
          (PROG (K)
            (SETQ K (KERNELSPF (CAR S)))
           LAB
            (COND ((NULL K) (RETURN NIL)))
            ((LAMBDA (K)
               (COND
                ((AND (NOT (OR (MEMQ K CRD) (MEMQ K COB))) (EXFORMP K))
                 (COND
                  ((EQUAL (XDEGREE K) 0)
                   (COND
                    ((OR (ASSOC K DEPL*) (EQCAR K 'PARTDF)
                         (NOT ((LAMBDA (XVARS*) (XVARP K)) T)))
                     (PROG (P)
                       (SETQ P (XPOWS (EXDFK K)))
                      LAB
                       (COND ((NULL P) (RETURN NIL)))
                       ((LAMBDA (P)
                          (SETQ NEW (CONS (CONS (CONS P (CONS 1 1)) NIL) NEW)))
                        (CAR P))
                       (SETQ P (CDR P))
                       (GO LAB)))
                    (T
                     (PROGN
                      (SETQ CRD (CONS K CRD))
                      (SETQ NEW (CONS (EXDFK K) NEW))
                      (COND
                       ((NEQ (CAR NEW)
                             (CONS (CONS (LIST 'D K) (CONS 1 1)) NIL))
                        (SETQ DRV
                                (CONS
                                 (LIST 'REPLACEBY (LIST 'D K)
                                       (*PF2A (CAR NEW)))
                                 DRV))))))))
                  ((EQUAL (XDEGREE K) 1)
                   (PROGN
                    (SETQ COB (CONS K COB))
                    (COND
                     ((NOT (EXACT K))
                      (PROGN
                       (SETQ NEW (CONS (EXDFK K) NEW))
                       (COND
                        ((NEQ (CAR NEW)
                              (CONS (CONS (LIST 'D K) (CONS 1 1)) NIL))
                         (SETQ DRV
                                 (CONS
                                  (LIST 'REPLACEBY (LIST 'D K)
                                        (*PF2A (CAR NEW)))
                                  DRV)))
                        (T (SETQ NEW (CDR NEW))))))
                     ((NOT (MEMQ (CADR K) CRD))
                      (SETQ CRD (CONS (CADR K) CRD))))))
                  (T (TYPERR K "0-form or 1-form"))))))
             (CAR K))
            (SETQ K (CDR K))
            (GO LAB))
          (PROG (Q)
            (SETQ Q (XCOEFFS (CAR S)))
           LAB
            (COND ((NULL Q) (RETURN NIL)))
            ((LAMBDA (Q)
               (COND
                ((NOT (FREEOFFL (CDR Q) CRD))
                 (SETQ RSX (CONS (MK*SQ (CONS (CDR Q) 1)) RSX)))))
             (CAR Q))
            (SETQ Q (CDR Q))
            (GO LAB))
          (SETQ S (APPEND (CDR S) NEW)))
        (GO WHILELABEL))
      (RETURN
       (PURGECFRM
        (MKCFRM (LIST (SORT COB 'TERMORDP) (SORT CRD 'TERMORDP) DRV RSX)))))) 
(PUT '!CFRM! 'PRIFN 'CFRMPRINT) 
(PUT '!CFRM! 'FANCY-REFORM '*CFRM2A) 
(PUT 'CFRM 'TEXPRIFN 'TEXPRICFRM) 
(PUT 'CFRMPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMPRINT 'DEFINED-ON-LINE '304) 
(PUT 'CFRMPRINT 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMPRINT (M) (MAPRIN (*CFRM2A M))) 
(PUT '*CFRM2A 'NUMBER-OF-ARGS 1) 
(PUT '*CFRM2A 'DEFINED-ON-LINE '310) 
(PUT '*CFRM2A 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT '*CFRM2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *CFRM2A (M)
    (CONS "coframing"
          (LIST (CONS 'LIST (CADR M)) (CONS 'LIST (CADDR M))
                (CONS 'LIST
                      (PROG (R FORALL-RESULT FORALL-ENDPTR)
                        (SETQ R (CADDR (CDR M)))
                        (COND ((NULL R) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (R) (*RULE2PREFIX R))
                                          (CAR R))
                                         NIL)))
                       LOOPLABEL
                        (SETQ R (CDR R))
                        (COND ((NULL R) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (R) (*RULE2PREFIX R)) (CAR R))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                (CONS 'LIST
                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                        (SETQ F (CADDR (CDDR M)))
                        (COND ((NULL F) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (F)
                                            (LIST 'NEQ (REVAL1 F T) 0))
                                          (CAR F))
                                         NIL)))
                       LOOPLABEL
                        (SETQ F (CDR F))
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F) (LIST 'NEQ (REVAL1 F T) 0))
                                  (CAR F))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))) 
(PUT '*RULE2PREFIX 'NUMBER-OF-ARGS 1) 
(PUT '*RULE2PREFIX 'DEFINED-ON-LINE '319) 
(PUT '*RULE2PREFIX 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT '*RULE2PREFIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *RULE2PREFIX (R)
    (CONS (CAR R)
          (PROG (A FORALL-RESULT FORALL-ENDPTR)
            (SETQ A (CDR R))
            (COND ((NULL A) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (A)
                                (COND ((EQCAR A '*SQ) (PREPSQ* (CADR A)))
                                      (T A)))
                              (CAR A))
                             NIL)))
           LOOPLABEL
            (SETQ A (CDR A))
            (COND ((NULL A) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (A)
                        (COND ((EQCAR A '*SQ) (PREPSQ* (CADR A))) (T A)))
                      (CAR A))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'TEXPRICFRM 'NUMBER-OF-ARGS 3) 
(PUT 'TEXPRICFRM 'DEFINED-ON-LINE '329) 
(PUT 'TEXPRICFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'TEXPRICFRM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TEXPRICFRM (U V W)
    (TEXVARPRI
     (COND ((GET 'HODGE 'TEXNAME) (*CFRM2A U))
           (T (CONS 'TEXPRIEDSOP (*CFRM2A U))))
     V W)) 
(PUT 'COBASIS 'RTYPEFN 'QUOTELIST) 
(PUT 'COBASIS 'LISTFN 'COBEVAL) 
(PUT 'COBEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'COBEVAL 'DEFINED-ON-LINE '343) 
(PUT 'COBEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'COBEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COBEVAL (S V) (COND ((NULL V) (REVAL1 (COBEVAL1 S) NIL)) (T (COBEVAL1 S)))) 
(PUT 'COBEVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'COBEVAL1 'DEFINED-ON-LINE '349) 
(PUT 'COBEVAL1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'COBEVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COBEVAL1 (S)
    (COND ((CFRMP (SETQ S (REVAL1 (CAR S) T))) (CONS 'LIST (CADR S)))
          ((EDSP S) (CONS 'LIST (EDSCOB S)))
          ((XEDSP S)
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (CONS 'LIST (EDSCOB X)))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (CONS 'LIST (EDSCOB X))) (CAR X))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (EDSPARTERR S "cobasis")))) 
(PUT 'COORDINATES 'RTYPEFN 'QUOTELIST) 
(PUT 'COORDINATES 'LISTFN 'CRDEVAL) 
(PUT 'CRDEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'CRDEVAL 'DEFINED-ON-LINE '364) 
(PUT 'CRDEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CRDEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CRDEVAL (S V) (COND ((NULL V) (REVAL1 (CRDEVAL1 S) NIL)) (T (CRDEVAL1 S)))) 
(PUT 'CRDEVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'CRDEVAL1 'DEFINED-ON-LINE '370) 
(PUT 'CRDEVAL1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CRDEVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CRDEVAL1 (S)
    (COND ((CFRMP (SETQ S (REVAL1 (CAR S) T))) (CONS 'LIST (CADDR S)))
          ((EDSP S) (CONS 'LIST (CADDR (CADDR (CDR S)))))
          ((XEDSP S)
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (CONS 'LIST (CADDR (CADDR (CDR X)))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (CONS 'LIST (CADDR (CADDR (CDR X)))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQCAR S 'LIST)
           (CONS 'LIST
                 (PURGE
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X (GETRLIST S))
                   STARTOVER
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (X) (GETRLIST (ALLCOORDS X))) (CAR X)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ X (CDR X))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (X) (GETRLIST (ALLCOORDS X))) (CAR X)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ X (CDR X))
                    (GO LOOPLABEL)))))
          ((NULL (GETRTYPE S)) (ALLCOORDS S)) (T (EDSPARTERR S "coordinates")))) 
(PUT 'STRUCTURE_EQUATIONS 'RTYPEFN 'QUOTELIST) 
(PUT 'STRUCTURE_EQUATIONS 'LISTFN 'DRVEVAL) 
(PUT 'DRVEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'DRVEVAL 'DEFINED-ON-LINE '389) 
(PUT 'DRVEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'DRVEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DRVEVAL (S V) (REVAL1 (DRVEVAL1 S) V)) 
(PUT 'DRVEVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'DRVEVAL1 'DEFINED-ON-LINE '395) 
(PUT 'DRVEVAL1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'DRVEVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DRVEVAL1 (S)
    (COND
     ((CFRMP (CAR (SETQ S (REVLIS S)))) (CONS 'LIST (CADDR (CDR (CAR S)))))
     ((EDSP (CAR S)) (CONS 'LIST (CADDR (CDR (CADDR (CDR (CAR S)))))))
     ((XEDSP (CAR S))
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (GETRLIST (CAR S)))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS 'LIST (CADDR (CDR (CADDR (CDR X))))))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (CONS 'LIST (CADDR (CDR (CADDR (CDR X))))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((AND (EQCAR (CAR S) 'LIST) (CDR (CAR S)) (EQEXPR (CADR (CAR S))))
      (XFORMDRVEVAL S))
     (T (EDSPARTERR S "structure equations")))) 
(PUT 'RESTRICTIONS 'RTYPEFN 'QUOTELIST) 
(PUT 'RESTRICTIONS 'LISTFN 'RSXEVAL) 
(PUT 'RSXEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'RSXEVAL 'DEFINED-ON-LINE '413) 
(PUT 'RSXEVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'RSXEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RSXEVAL (S V)
    (COND
     ((CFRMP (SETQ S (REVAL1 (CAR S) T)))
      (CONS 'LIST
            (PROG (R FORALL-RESULT FORALL-ENDPTR)
              (SETQ R (CADDR (CDDR S)))
              (COND ((NULL R) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (R) (LIST 'NEQ (REVAL1 R V) 0))
                                (CAR R))
                               NIL)))
             LOOPLABEL
              (SETQ R (CDR R))
              (COND ((NULL R) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (R) (LIST 'NEQ (REVAL1 R V) 0)) (CAR R))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((EDSP S)
      (CONS 'LIST
            (PROG (R FORALL-RESULT FORALL-ENDPTR)
              (SETQ R (CADDR (CDDR (CADDR (CDR S)))))
              (COND ((NULL R) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (R) (LIST 'NEQ (REVAL1 R V) 0))
                                (CAR R))
                               NIL)))
             LOOPLABEL
              (SETQ R (CDR R))
              (COND ((NULL R) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (R) (LIST 'NEQ (REVAL1 R V) 0)) (CAR R))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((XEDSP S)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (CDR S))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS 'LIST
                                        (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ R
                                                  (CADDR
                                                   (CDDR (CADDR (CDR X)))))
                                          (COND ((NULL R) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (R)
                                                              (LIST 'NEQ
                                                                    (REVAL1 R
                                                                            V)
                                                                    0))
                                                            (CAR R))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ R (CDR R))
                                          (COND
                                           ((NULL R) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (R)
                                                      (LIST 'NEQ (REVAL1 R V)
                                                            0))
                                                    (CAR R))
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
                                (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ R (CADDR (CDDR (CADDR (CDR X)))))
                                  (COND ((NULL R) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (R)
                                                      (LIST 'NEQ (REVAL1 R V)
                                                            0))
                                                    (CAR R))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ R (CDR R))
                                  (COND ((NULL R) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (R)
                                              (LIST 'NEQ (REVAL1 R V) 0))
                                            (CAR R))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (EDSPARTERR S "restrictions")))) 
(PUT 'EDSPARTERR 'NUMBER-OF-ARGS 2) 
(PUT 'EDSPARTERR 'DEFINED-ON-LINE '428) 
(PUT 'EDSPARTERR 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'EDSPARTERR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSPARTERR (U V) (MSGPRI NIL U (LIST "has no" V) NIL T)) 
(PUT 'CFRMPART 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMPART 'DEFINED-ON-LINE '434) 
(PUT 'CFRMPART 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMPART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMPART (M N)
    (COND ((EQUAL N 0) 'COFRAMING) ((EQUAL N 1) (CONS 'LIST (CADR M)))
          ((EQUAL N 2) (CONS 'LIST (CADDR M)))
          ((EQUAL N 3) (CONS 'LIST (CADDR (CDR M))))
          ((EQUAL N 4)
           (CONS 'LIST
                 (PROG (R FORALL-RESULT FORALL-ENDPTR)
                   (SETQ R (CADDR (CDDR M)))
                   (COND ((NULL R) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (R) (LIST 'NEQ R 0)) (CAR R))
                                         NIL)))
                  LOOPLABEL
                   (SETQ R (CDR R))
                   (COND ((NULL R) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (R) (LIST 'NEQ R 0)) (CAR R)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (PARTERR M N)))) 
(PUT '!CFRM! 'PARTOP 'CFRMPART) 
(PUT 'CFRMSETPART 'NUMBER-OF-ARGS 3) 
(PUT 'CFRMSETPART 'DEFINED-ON-LINE '447) 
(PUT 'CFRMSETPART 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMSETPART 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CFRMSETPART (M L R)
    (RERROR 'EDS 0 "Part setting disabled on coframing operator")) 
(PUT '!CFRM! 'SETPARTOP 'CFRMSETPART) 
(PUT 'CHECKCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'CHECKCFRM 'DEFINED-ON-LINE '457) 
(PUT 'CHECKCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CHECKCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECKCFRM (M) (CFRMPROTECT (LIST 'CHECKCFRM1 M))) 
(PUT 'CHECKCFRM1 'NUMBER-OF-ARGS 1) 
(PUT 'CHECKCFRM1 'DEFINED-ON-LINE '466) 
(PUT 'CHECKCFRM1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CHECKCFRM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECKCFRM1 (M)
    (PROG (N U DRV)
      (SETQ M (COPYCFRM M))
      (SETQ N (*SYS2CFRM (*A2SYS (CONS 'LIST (APPEND (CADR M) (CADDR M))))))
      (COND
       ((NEQ (CADR N) (CADR M)) (RERROR 'EDS 0 "Missing cobasis elements")))
      (SETCAR (CDDR (CDDR N)) (UNION (CADDR (CDDR M)) (CADDR (CDDR N))))
      (SETQ DRV
              (PROG (D FORALL-RESULT FORALL-ENDPTR)
                (SETQ D (CADDR (CDR N)))
                (COND ((NULL D) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (D) (CADR D)) (CAR D)) NIL)))
               LOOPLABEL
                (SETQ D (CDR D))
                (COND ((NULL D) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (D) (CADR D)) (CAR D)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (K)
        (SETQ K (CADR N))
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (COND
            ((AND (NOT (EXACT K)) (NOT (MEMBER (LIST 'D K) DRV)))
             (SETQ U (CONS K U)))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (COND (U (EDSVERBOSE "Missing structure equations" (REVERSE U) 'COB)))
      (RETURN (PURGECFRM N)))) 
(PUT 'RESIMPCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'RESIMPCFRM 'DEFINED-ON-LINE '490) 
(PUT 'RESIMPCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'RESIMPCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESIMPCFRM (S)
    (PROG (R)
      (SETQ R (COPYCFRM S))
      (SETCAR (CDR R)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR R)
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR S))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR (CDR R))
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR (CDR S)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDDR (CDDR R))
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDR (CDDR S)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REVAL1 F NIL)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REVAL1 F NIL)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (COND ((EQUAL R S) S) (T (CHECKCFRM R)))))) 
(PUT 'REORDER 'PSOPFN 'REORDEREVAL) 
(PUT 'REORDEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'REORDEREVAL 'DEFINED-ON-LINE '505) 
(PUT 'REORDEREVAL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'REORDEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REORDEREVAL (S)
    (COND ((CFRMP (SETQ S (REVAL1 (CAR S) T))) (REORDERCFRM S))
          ((EDSP S) (REORDEREDS S))
          ((XEDSP S)
           (CONS 'LIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR S))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (REORDEREDS X)) (CAR X))
                                         NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (REORDEREDS X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (MSGPRI NIL NIL "Don't know how to reorder" S T)))) 
(PUT 'REORDERCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'REORDERCFRM 'DEFINED-ON-LINE '516) 
(PUT 'REORDERCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'REORDERCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REORDERCFRM (S)
    (PROG (R)
      (SETQ R (COPYCFRM S))
      (SETCAR (CDR R) (SORT (CADR S) 'TERMORDP))
      (SETCAR (CDDR R) (SORT (CADDR S) 'TERMORDP))
      (SETCAR (CDDR (CDR R))
              (SORT (CADDR (CDR S))
                    '(LAMBDA (X Y) (TERMORDP (CADR X) (CADR Y)))))
      (SETCAR (CDDR (CDDR R)) (SORT (CADDR (CDDR S)) 'ORDOP))
      (RETURN (COND ((EQUAL R S) S) (T R))))) 
(PUT 'CLEANUP 'RTYPEFN 'GETRTYPECAR) 
(PUT 'CLEANUP 'CFRMFN 'CLEANCFRM) 
(PUT 'CLEANCFRM 'NUMBER-OF-ARGS 1) 
(PUT 'CLEANCFRM 'DEFINED-ON-LINE '532) 
(PUT 'CLEANCFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CLEANCFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEANCFRM (M)
    (PROG (N)
      (SETQ N (RESIMPCFRM M))
      (RETURN (COND ((EQ N M) (CHECKCFRM M)) (T N))))) 
(PUT 'PURGECFRM 'NUMBER-OF-ARGS 1) 
(PUT 'PURGECFRM 'DEFINED-ON-LINE '543) 
(PUT 'PURGECFRM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'PURGECFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGECFRM (M)
    (PROG (CFRMCRD* CFRMCOB*)
      (SETQ M (COPYCFRM M))
      (SETQ CFRMCOB* (CADR M))
      (SETQ CFRMCRD* (CADDR M))
      (SETCAR (CDDR (CDR M)) (PURGEDRV (CADDR (CDR M))))
      (SETCAR (CDDR (CDDR M)) (PURGERSX (CADDR (CDDR M))))
      (RETURN M))) 
(PUT 'PURGEDRV 'NUMBER-OF-ARGS 1) 
(PUT 'PURGEDRV 'DEFINED-ON-LINE '557) 
(PUT 'PURGEDRV 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'PURGEDRV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGEDRV (X)
    (PROG (DRV DL DR R2)
      (PROG (R)
        (SETQ R X)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (COND
            ((AND (EXACT (SETQ DL (CADR R)))
                  (OR (MEMBER (CADR DL) CFRMCOB*) (MEMBER (CADR DL) CFRMCRD*))
                  (NOT
                   (AND (KERNP (SETQ DR (SIMP* (CADDR R))))
                        (EQUAL DL (CAAAR (CAR DR))))))
             (COND
              ((NULL (SETQ R2 (ASSOC DL DRV)))
               (SETQ DRV (CONS (CONS DL DR) DRV)))
              ((AND (NEQ (CDR R2) DR) (NEQ (RESIMP (CDR R2)) (RESIMP DR)))
               (PROGN
                (EDSDEBUG "Inconsistent structure equations"
                 (CONS 'LIST
                       (LIST (LIST 'REPLACEBY DL (MK*SQ DR))
                             (LIST 'REPLACEBY (CAR R2) (MK*SQ (CDR R2)))))
                 'PREFIX)
                (RERROR 'EDS 0 "Inconsistent structure equations")))))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (SETQ DRV
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (REVERSIP DRV))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (LIST 'REPLACEBY (CAR P) (MK*SQ (CDR P))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (LIST 'REPLACEBY (CAR P) (MK*SQ (CDR P))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (SORT DRV '(LAMBDA (X Y) (TERMORDP (CADR X) (CADR Y))))))) 
(PUT 'PURGERSX 'NUMBER-OF-ARGS 1) 
(PUT 'PURGERSX 'DEFINED-ON-LINE '582) 
(PUT 'PURGERSX 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'PURGERSX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PURGERSX (X)
    (PROG (RSX)
      (PROG (F)
        (SETQ F (REVERSE (PURGE X)))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (SETQ RSX (ADDRSX (CAR (SIMP* F)) RSX))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN RSX))) 
(PUT 'ADDRSX 'NUMBER-OF-ARGS 2) 
(PUT 'ADDRSX 'DEFINED-ON-LINE '591) 
(PUT 'ADDRSX 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'ADDRSX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDRSX (X RSX)
    (PROG ()
      (COND
       ((AND (NOT (CFRMCONSTANT X)) (NOT (MEMBER (MK*SQ (CONS X 1)) RSX)))
        (PROG (F)
          (SETQ F (CDR (FCTRF (REORDER X))))
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F)
             (COND
              ((AND (NOT (CFRMCONSTANT (CAR F)))
                    (NOT (MEMBER (SETQ F (MK*SQ (CONS (CAR F) 1))) RSX)))
               (SETQ RSX (CONS F RSX)))))
           (CAR F))
          (SETQ F (CDR F))
          (GO LAB))))
      (RETURN RSX))) 
(INFIX (LIST 'CROSS)) 
(PRECEDENCE (LIST 'CROSS 'TIMES)) 
(PUT 'CROSS 'RTYPEFN 'GETRTYPECAR) 
(PUT 'CROSS 'EDSFN 'EXTENDEDS) 
(PUT 'CROSS 'CFRMFN 'CFRMPROD) 
(FLAG '(CROSS) 'NOSPREAD) 
(FLAG '(CROSS) 'NARY) 
(PUT 'EXTENDEDS 'NUMBER-OF-ARGS 1) 
(PUT 'EXTENDEDS 'DEFINED-ON-LINE '618) 
(PUT 'EXTENDEDS 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'EXTENDEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTENDEDS (U)
    (PROG (S JET0)
      (COND ((NULL (CDR U)) (RETURN (CAR U))))
      (SETQ S (COPYEDS (CAR U)))
      (SETQ U (CFRMPROD (CDR U)))
      (COND
       ((SETQ JET0 (GETEDS S 'JET0))
        (PUTEDS S 'JET0
         (PURGEJET0 (APPEND JET0 (SETDIFF (CADDR U) (EDSCRD S)))
          (UNIQIDS (INDKRNS S))))))
      (SETCAR (CDDR (CDR S)) (CFRMPROD2 (CADDR (CDR S)) U))
      (REMKRNS S)
      (RETURN (NORMALEDS (PURGEEDS* S))))) 
(PUT 'PURGEJET0 'NUMBER-OF-ARGS 2) 
(PUT 'PURGEJET0 'DEFINED-ON-LINE '634) 
(PUT 'PURGEJET0 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'PURGEJET0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PURGEJET0 (CRD IDXL)
    (PROG (J J0)
      (SETQ IDXL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I (FLATINDXL IDXL))
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (I) (LIST 'MINUS I)) (CAR I))
                                      NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (LIST 'MINUS I)) (CAR I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (C)
        (SETQ C CRD)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ J J0)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND J (NOT (JETPRL C (CAR J) IDXL)))) (RETURN NIL)))
              (SETQ J (CDR J))
              (GO WHILELABEL))
            (COND
             ((NULL J)
              (SETQ J0
                      (CONS C
                            (PROG (C0 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ C0 J0)
                             STARTOVER
                              (COND ((NULL C0) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (C0)
                                         (COND
                                          ((NOT (JETPRL C0 C IDXL))
                                           (LIST C0))))
                                       (CAR C0)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ C0 (CDR C0))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL C0) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (C0)
                                         (COND
                                          ((NOT (JETPRL C0 C IDXL))
                                           (LIST C0))))
                                       (CAR C0)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ C0 (CDR C0))
                              (GO LOOPLABEL))))))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (RETURN J0))) 
(PUT 'JETPRL 'NUMBER-OF-ARGS 3) 
(PUT 'JETPRL 'DEFINED-ON-LINE '646) 
(PUT 'JETPRL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'JETPRL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JETPRL (C C0 IDXL)
    (COND ((SETQ C (SPLITOFFINDICES C0 C)) (SUBSETP (CDR C) IDXL)))) 
(PUT 'CFRMPROD 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMPROD 'DEFINED-ON-LINE '650) 
(PUT 'CFRMPROD 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMPROD (U)
    ((LAMBDA (M)
       (COND ((NOT (CFRMP M)) (TYPERR (CAR U) "coframing"))
             ((EQUAL (LENGTH U) 1) M)
             (T (CFRMPROTECT (LIST 'CFRMPROD2 M (CFRMPROD (CDR U)))))))
     (*A2CFRM (LIST (CAR U))))) 
(PUT 'CFRMPROD2 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMPROD2 'DEFINED-ON-LINE '659) 
(PUT 'CFRMPROD2 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMPROD2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMPROD2 (M N)
    (COND
     ((OR (XNP (CADR M) (CADR N)) (XNP (CADDR M) (CADDR N))) (CFRMBPROD M N))
     (T
      (MKCFRM
       (LIST (APPEND (CADR M) (CADR N)) (APPEND (CADDR M) (CADDR N))
             (APPEND (CADDR (CDR M)) (CADDR (CDR N)))
             (APPEND (CADDR (CDDR M)) (CADDR (CDDR N)))))))) 
(PUT 'CFRMBPROD 'NUMBER-OF-ARGS 2) 
(PUT 'CFRMBPROD 'DEFINED-ON-LINE '670) 
(PUT 'CFRMBPROD 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMBPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CFRMBPROD (M N)
    (PROG (Z U V)
      (SETQ Z
              (*A2SYS
               (CONS 'LIST
                     (APPEND (INTERSECTION (CADR M) (CADR N))
                             (INTERSECTION (CADDR M) (CADDR N))))))
      (SETCFRM M)
      (SETQ U (*SYS2CFRM Z))
      (SETCFRM N)
      (SETQ V (*SYS2CFRM Z))
      (COND
       ((NOT (EQUALCFRM U V))
        (RERROR 'EDS 0
                "Cannot form coframing product: overlap cannot be factored")))
      (RETURN
       (RESIMPCFRM
        (MKCFRM
         (LIST (APPEND (SETDIFF (CADR M) (CADR U)) (CADR N))
               (APPEND (SETDIFF (CADDR M) (CADDR U)) (CADDR N))
               (APPEND (SETDIFF (CADDR (CDR M)) (CADDR (CDR U)))
                       (CADDR (CDR N)))
               (APPEND (SETDIFF (CADDR (CDDR M)) (CADDR (CDDR U)))
                       (CADDR (CDDR N))))))))) 
(PUT 'DIM 'SIMPFN 'SIMPDIM) 
(PUT 'SIMPDIM 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDIM 'DEFINED-ON-LINE '697) 
(PUT 'SIMPDIM 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'SIMPDIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDIM (U)
    (COND ((CFRMP (SETQ U (REVAL1 (CAR U) T))) (CONS (LENGTH (CADR U)) 1))
          ((EDSP U) (CONS (LENGTH (EDSCOB U)) 1))
          (T (EDSPARTERR U "dimension")))) 
(PUT 'CFRMNOWHEREZERO 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMNOWHEREZERO 'DEFINED-ON-LINE '717) 
(PUT 'CFRMNOWHEREZERO 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMNOWHEREZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMNOWHEREZERO (X)
    ((LAMBDA (XVARS*)
       (OR (OR (ATOM X) (ATOM (CAR X)))
           (CFRMNOWHEREZERO1 (XPARTITSQ (CONS X 1)))
           (COND
            ((AND (SETQ X (CDR (FCTRF X)))
                  (OR (GREATERP (LENGTH X) 1) (GREATERP (CDAR X) 1)))
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND X (CFRMNOWHEREZERO1 (XPARTITSQ (CONS (CAAR X) 1)))))
                  (RETURN NIL)))
                (SETQ X (CDR X))
                (GO WHILELABEL))
              (NULL X))))))
     CFRMCRD*)) 
(PUT 'CFRMNOWHEREZERO1 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMNOWHEREZERO1 'DEFINED-ON-LINE '732) 
(PUT 'CFRMNOWHEREZERO1 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMNOWHEREZERO1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMNOWHEREZERO1 (X)
    (COND ((EQUAL (CAAR X) 1) (CFRMCONSTANT (CAR (CDAR X))))
          (T (CFRMVIOLATESRSX X)))) 
(PUT 'CFRMCONSTANT 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMCONSTANT 'DEFINED-ON-LINE '742) 
(PUT 'CFRMCONSTANT 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMCONSTANT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMCONSTANT (X) (FREEOFFL X CFRMCRD*)) 
(PUT 'FREEOFFL 'NUMBER-OF-ARGS 2) 
(PUT 'FREEOFFL 'DEFINED-ON-LINE '747) 
(PUT 'FREEOFFL 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'FREEOFFL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FREEOFFL (X V) (OR (NULL V) (AND (FREEOFF X (CAR V)) (FREEOFFL X (CDR V))))) 
(PUT 'FREEOFF 'NUMBER-OF-ARGS 2) 
(PUT 'FREEOFF 'DEFINED-ON-LINE '753) 
(PUT 'FREEOFF 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'FREEOFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FREEOFF (X V)
    (COND ((OR (ATOM X) (ATOM (CAR X))) T)
          ((SFP (CAAAR X))
           (AND (FREEOFF (CAAAR X) V) (FREEOFF (CDAR X) V)
                (FREEOFF (CDR X) V)))
          (T
           (AND (NOT (NDEPENDS (CAAAR X) V)) (FREEOFF (CDAR X) V)
                (FREEOFF (CDR X) V))))) 
(PUT 'CFRMVIOLATESRSX 'NUMBER-OF-ARGS 1) 
(PUT 'CFRMVIOLATESRSX 'DEFINED-ON-LINE '764) 
(PUT 'CFRMVIOLATESRSX 'DEFINED-IN-FILE 'EDS/EDSCFRM.RED) 
(PUT 'CFRMVIOLATESRSX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFRMVIOLATESRSX (X)
    (PROG (RSX)
      (SETQ RSX CFRMRSX*)
      (SETQ X (LIST X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND RSX (XREDUCE (CAR RSX) X))) (RETURN NIL)))
        (SETQ RSX (CDR RSX))
        (GO WHILELABEL))
      (RETURN (NOT (NULL RSX))))) 
(ENDMODULE) 