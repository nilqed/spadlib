(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SOLVELNR)) 
(FLUID
 '(*CRAMER *EXP *SOLVESINGULAR ASYMPLIS* WTL* *ARBVARS *TRSPARSE *VAROPT
   BAREISS-STEP-SIZE*)) 
(PUT 'SOLVELNRSYS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVELNRSYS 'DEFINED-ON-LINE '45) 
(PUT 'SOLVELNRSYS 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVELNRSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVELNRSYS (EXLIS VARLIS)
    (PROG (W X)
      (COND ((SETQ W (SOLVESPARSECHECK EXLIS VARLIS)) (SETQ EXLIS W))
            (T (SETQ EXLIS (CONS EXLIS VARLIS))))
      (COND
       ((AND (NULL *CRAMER)
             (NULL
              (ERRORP
               ((LAMBDA (BAREISS-STEP-SIZE*)
                  (SETQ X
                          (ERRORSET2
                           (LIST 'SOLVEBAREISS (MKQUOTE (CAR EXLIS))
                                 (MKQUOTE (CDR EXLIS))))))
                (COND (W 4) (T 2))))))
        (SETQ EXLIS (CAR X)))
       (T (SETQ EXLIS (SOLVECRAMER (CAR EXLIS) (CDR EXLIS)))))
      (RETURN (SOLVESYSPOST EXLIS VARLIS)))) 
(PUT 'EXPTEXPFLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'EXPTEXPFLISTP 'DEFINED-ON-LINE '68) 
(PUT 'EXPTEXPFLISTP 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'EXPTEXPFLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPTEXPFLISTP (U) (AND U (OR (EXPTEXPFP (CAR U)) (EXPTEXPFLISTP (CDR U))))) 
(PUT 'EXPTEXPFP 'NUMBER-OF-ARGS 1) 
(PUT 'EXPTEXPFP 'DEFINED-ON-LINE '72) 
(PUT 'EXPTEXPFP 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'EXPTEXPFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPTEXPFP (U)
    (AND (NOT (OR (ATOM U) (ATOM (CAR U))))
         ((LAMBDA (X)
            (OR (EQCAR X 'EXPT) (EXPTEXPFP (CDAR U)) (EXPTEXPFP (CDR U))))
          (CAAAR U)))) 
(PUT 'SOLVESYSPOST 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVESYSPOST 'DEFINED-ON-LINE '78) 
(PUT 'SOLVESYSPOST 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVESYSPOST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVESYSPOST (EXLIS VARLIS)
    (CONS (CAR EXLIS)
          (PROG (S FORALL-RESULT FORALL-ENDPTR)
            (SETQ S (CDR EXLIS))
            (COND ((NULL S) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (S)
                                (COND ((AND (CAR S) (NULL (CADR S))) S)
                                      (T
                                       (PROG (ARBVARS Z)
                                         (COND
                                          ((OR *ARBVARS
                                               (AND (NULL (CADR S))
                                                    (EQUAL (LENGTH VARLIS) 1)))
                                           (SETQ ARBVARS
                                                   (PROG (V FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ V
                                                             (SETDIFF VARLIS
                                                                      (CADR
                                                                       S)))
                                                     (COND
                                                      ((NULL V) (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (V)
                                                                         (CONS
                                                                          V
                                                                          (CAAAR
                                                                           (MAKEARBCOMPLEX))))
                                                                       (CAR V))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ V (CDR V))
                                                     (COND
                                                      ((NULL V)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (V)
                                                                 (CONS V
                                                                       (CAAAR
                                                                        (MAKEARBCOMPLEX))))
                                                               (CAR V))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL))))
                                          (T
                                           (SETQ VARLIS
                                                   (INTERSECTION VARLIS
                                                                 (CADR S)))))
                                         (SETQ Z
                                                 (PAIR (CADR S)
                                                       (SUBLIS ARBVARS
                                                               (CAR S))))
                                         (SETQ Z
                                                 (APPEND Z
                                                         (PROG (P FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ P ARBVARS)
                                                           (COND
                                                            ((NULL P)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  P)
                                                                               (CONS
                                                                                (CAR
                                                                                 P)
                                                                                (CONS
                                                                                 (LIST
                                                                                  (CONS
                                                                                   (CONS
                                                                                    (CDR
                                                                                     P)
                                                                                    1)
                                                                                   1))
                                                                                 1)))
                                                                             (CAR
                                                                              P))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ P (CDR P))
                                                           (COND
                                                            ((NULL P)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (P)
                                                                (CONS (CAR P)
                                                                      (CONS
                                                                       (LIST
                                                                        (CONS
                                                                         (CONS
                                                                          (CDR
                                                                           P)
                                                                          1)
                                                                         1))
                                                                       1)))
                                                              (CAR P))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL))))
                                         (RETURN
                                          (LIST
                                           (PROG (V FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ V VARLIS)
                                             (COND ((NULL V) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (V)
                                                                 (REORDSQ
                                                                  (CDR
                                                                   (ATSOC V
                                                                          Z))))
                                                               (CAR V))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ V (CDR V))
                                             (COND
                                              ((NULL V)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (V)
                                                         (REORDSQ
                                                          (CDR (ATSOC V Z))))
                                                       (CAR V))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL))
                                           VARLIS (CADDR S)))))))
                              (CAR S))
                             NIL)))
           LOOPLABEL
            (SETQ S (CDR S))
            (COND ((NULL S) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (S)
                        (COND ((AND (CAR S) (NULL (CADR S))) S)
                              (T
                               (PROG (ARBVARS Z)
                                 (COND
                                  ((OR *ARBVARS
                                       (AND (NULL (CADR S))
                                            (EQUAL (LENGTH VARLIS) 1)))
                                   (SETQ ARBVARS
                                           (PROG (V FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ V (SETDIFF VARLIS (CADR S)))
                                             (COND ((NULL V) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (V)
                                                                 (CONS V
                                                                       (CAAAR
                                                                        (MAKEARBCOMPLEX))))
                                                               (CAR V))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ V (CDR V))
                                             (COND
                                              ((NULL V)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (V)
                                                         (CONS V
                                                               (CAAAR
                                                                (MAKEARBCOMPLEX))))
                                                       (CAR V))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL))))
                                  (T
                                   (SETQ VARLIS
                                           (INTERSECTION VARLIS (CADR S)))))
                                 (SETQ Z
                                         (PAIR (CADR S)
                                               (SUBLIS ARBVARS (CAR S))))
                                 (SETQ Z
                                         (APPEND Z
                                                 (PROG (P FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ P ARBVARS)
                                                   (COND
                                                    ((NULL P) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (P)
                                                                       (CONS
                                                                        (CAR P)
                                                                        (CONS
                                                                         (LIST
                                                                          (CONS
                                                                           (CONS
                                                                            (CDR
                                                                             P)
                                                                            1)
                                                                           1))
                                                                         1)))
                                                                     (CAR P))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ P (CDR P))
                                                   (COND
                                                    ((NULL P)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (P)
                                                               (CONS (CAR P)
                                                                     (CONS
                                                                      (LIST
                                                                       (CONS
                                                                        (CONS
                                                                         (CDR
                                                                          P)
                                                                         1)
                                                                        1))
                                                                      1)))
                                                             (CAR P))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))))
                                 (RETURN
                                  (LIST
                                   (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ V VARLIS)
                                     (COND ((NULL V) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (V)
                                                         (REORDSQ
                                                          (CDR (ATSOC V Z))))
                                                       (CAR V))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ V (CDR V))
                                     (COND ((NULL V) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (V)
                                                 (REORDSQ (CDR (ATSOC V Z))))
                                               (CAR V))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))
                                   VARLIS (CADDR S)))))))
                      (CAR S))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'SOLVECRAMER 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVECRAMER 'DEFINED-ON-LINE '98) 
(PUT 'SOLVECRAMER 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVECRAMER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVECRAMER (EXLIS VARLIS) (GLNRSOLVE EXLIS VARLIS)) 
(PUT 'SOLVESPARSECHECK 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVESPARSECHECK 'DEFINED-ON-LINE '104) 
(PUT 'SOLVESPARSECHECK 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVESPARSECHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVESPARSECHECK (SYS VL)
    (PROG (VL1 XL SYS1 Q X Y SP)
      (SETQ SP 0)
      (SETQ SP 0)
      (SETQ VL1
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X VL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (CONS X (CONS 0 NIL))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CONS X (CONS 0 NIL))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (Q)
        (SETQ Q SYS)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROG (X)
             (SETQ X (SETQ XL (INTERSECTION (TOPKERNS Q) VL)))
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (PROGN
                 (SETQ Y (ASSOC X VL1))
                 (SETCDR Y (CONS (PLUS (CADR Y) 1) (UNION XL (CDDR Y))))
                 (SETQ SP (PLUS SP 1))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB)))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (PROG (P)
        (SETQ P VL1)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETCDR (CDR P) (DIFFERENCE (LENGTH (CDDR P)) 1)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       ((GREATERP SP (TIMES (LENGTH SYS) (LENGTH VL) 0.8))
        (PROGN
         (COND (*TRSPARSE (PRIN2T "System is not very sparse")))
         (RETURN NIL))))
      (COND (*TRSPARSE (SOLVESPARSEPRINT "Original sparse system" SYS VL)))
      (COND
       (*VAROPT
        (PROGN
         (SETQ VL1 (SORT VL1 (FUNCTION SOLVEVARORDP)))
         (SETQ VL1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X VL1)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ VL1 (SOLVEVARADJUST VL1))
         (PROG (K)
           (SETQ K (REVERSE VL1))
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K) (UPDKORDER K)) (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (SETQ SYS
                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Q SYS)
                   (COND ((NULL Q) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (Q) (REORDER Q)) (CAR Q))
                                         NIL)))
                  LOOPLABEL
                   (SETQ Q (CDR Q))
                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (Q) (REORDER Q)) (CAR Q)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T
        (SETQ VL1
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X VL1)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ SYS1
              (CONS (CONS NIL NIL)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X VL1)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (X) (CONS X NIL)) (CAR X))
                                            NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS X NIL)) (CAR X)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (PROG (Q)
        (SETQ Q SYS)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (COND
             ((OR (OR (ATOM Q) (ATOM (CAR Q))) (NOT (MEMBER (CAAAR Q) VL1)))
              (SETQ Y (ASSOC NIL SYS1)))
             (T (SETQ Y (ASSOC (CAAAR Q) SYS1))))
            (SETCDR Y (CONS Q (CDR Y)))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (PROG (P)
        (SETQ P (CDR SYS1))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((CDR P) (SETCDR P (SORT (CDR P) (FUNCTION SOLVESPARSESORT))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ SYS
              (NCONC
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P SYS1)
                STARTOVER
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (P) (COND ((CDR P) (LIST (CADR P)))))
                          (CAR P)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ P (CDR P))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (P) (COND ((CDR P) (LIST (CADR P)))))
                          (CAR P)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ P (CDR P))
                 (GO LOOPLABEL))
               (REVERSIP
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P SYS1)
                 STARTOVER
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (P) (COND ((CDR P) (CDDR P)))) (CAR P)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ P (CDR P))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (P) (COND ((CDR P) (CDDR P)))) (CAR P)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ P (CDR P))
                  (GO LOOPLABEL)))))
      (COND
       (*TRSPARSE
        (SOLVESPARSEPRINT "Variables and/or equations rearranged" SYS VL1)))
      (RETURN (CONS SYS VL1)))) 
(PUT 'SOLVEVARORDP 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEVARORDP 'DEFINED-ON-LINE '167) 
(PUT 'SOLVEVARORDP 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVEVARORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEVARORDP (X Y)
    (OR (LESSP (CADR X) (CADR Y))
        (AND (EQUAL (CADR X) (CADR Y)) (LESSP (CDDR X) (CDDR Y))))) 
(PUT 'SOLVEVARADJUST 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEVARADJUST 'DEFINED-ON-LINE '171) 
(PUT 'SOLVEVARADJUST 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVEVARADJUST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEVARADJUST (U)
    (PROG (V Y)
      (COND ((NULL U) (RETURN NIL)))
      (SETQ V
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ Y (ASSOC X DEPL*))
                            (COND
                             ((OR (NULL Y) (NULL (XNP (CDR Y) U))) (LIST X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (PROGN
                            (SETQ Y (ASSOC X DEPL*))
                            (COND
                             ((OR (NULL Y) (NULL (XNP (CDR Y) U))) (LIST X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (NCONC (SOLVEVARADJUST (SETDIFF U V)) V)))) 
(PUT 'SOLVESPARSEPRINT 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVESPARSEPRINT 'DEFINED-ON-LINE '183) 
(PUT 'SOLVESPARSEPRINT 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVESPARSEPRINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVESPARSEPRINT (TEXT SYS VL)
    (PROGN
     (TERPRI)
     (PRIN2T TEXT)
     (PROG (E)
       (SETQ E SYS)
      LAB
       (COND ((NULL E) (RETURN NIL)))
       ((LAMBDA (E)
          (PROGN
           (SETQ E (TOPKERNS E))
           (PROG (X)
             (SETQ X VL)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X) (COND ((MEMQ X E) (PRIN2 "*")) (T (PRIN2 "-"))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (TERPRI)))
        (CAR E))
       (SETQ E (CDR E))
       (GO LAB)))) 
(PUT 'TOPKERNS 'NUMBER-OF-ARGS 1) 
(PUT 'TOPKERNS 'DEFINED-ON-LINE '191) 
(PUT 'TOPKERNS 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'TOPKERNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOPKERNS (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T (CONS (CAAAR U) (TOPKERNS (CDR U)))))) 
(PUT 'SOLVESPARSESORT 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVESPARSESORT 'DEFINED-ON-LINE '196) 
(PUT 'SOLVESPARSESORT 'DEFINED-IN-FILE 'SOLVE/SOLVELNR.RED) 
(PUT 'SOLVESPARSESORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVESPARSESORT (X Y)
    (COND ((OR (ATOM X) (ATOM (CAR X))) NIL) ((OR (ATOM Y) (ATOM (CAR Y))) T)
          ((EQUAL (CAAAR X) (CAAAR Y)) (SOLVESPARSESORT (CDR Y) (CDR X)))
          ((ORDOP (CAAAR X) (CAAAR Y)) T) (T NIL))) 
(ENDMODULE) 