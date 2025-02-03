(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSSOLVE)) 
(FLUID
 '(*EDSVERBOSE *EDSDEBUG *ARBVARS *VAROPT *GROEBOPT *SOLVEINCONSISTENT DEPL*
   CFRMCRD* CFRMRSX* XVARS* *EDSSLOPPY *EDSDISJOINT)) 
(PUT 'EDSSOLVEGRADED 'NUMBER-OF-ARGS 3) 
(PUT 'EDSSOLVEGRADED 'DEFINED-ON-LINE '49) 
(PUT 'EDSSOLVEGRADED 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSSOLVEGRADED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EDSSOLVEGRADED (XL VL RSX)
    ((LAMBDA (DEPL*)
       (PROG (FL Z)
         (SETQ Z
                 (PROG (L FORALL-RESULT FORALL-ENDPTR)
                   (SETQ L VL)
                  STARTOVER
                   (COND ((NULL L) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (L) (APPEND L (LIST))) (CAR L)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ L (CDR L))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL L) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (L) (APPEND L (LIST))) (CAR L)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ L (CDR L))
                   (GO LOOPLABEL)))
         (COND
          ((AND VL
                (SEMILINEARSQL XL (CAR VL) (COND (*EDSSLOPPY (CAR VL)) (T Z))))
           (RETURN (EDSSOLVESEMI XL VL RSX))))
         (PROG (L)
           (SETQ L VL)
          LAB
           (COND ((NULL L) (RETURN NIL)))
           (PROG (Y)
             (SETQ Y (CAR L))
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y)
                (PROG (LL)
                  (SETQ LL (CDR L))
                 LAB
                  (COND ((NULL LL) (RETURN NIL)))
                  ((LAMBDA (LL)
                     (PROG (X)
                       (SETQ X LL)
                      LAB
                       (COND ((NULL X) (RETURN NIL)))
                       ((LAMBDA (X) (DEPEND1 Y X T)) (CAR X))
                       (SETQ X (CDR X))
                       (GO LAB)))
                   (CAR LL))
                  (SETQ LL (CDR LL))
                  (GO LAB)))
              (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB))
           (SETQ L (CDR L))
           (GO LAB))
         (SETQ XL (EDSSOLVE XL Z))
         (SETQ FL (LIST))
         (SETQ XL
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S XL)
                  STARTOVER
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (S)
                              (COND
                               ((NULL (CAR S))
                                (PROGN (SETQ FL (CONS S FL)) NIL))
                               ((NOT
                                 (MEMBER 0
                                         (SETQ Z (PULLBACKRSX RSX (CADR S)))))
                                (LIST
                                 (LIST (CADR S)
                                       (PURGERSX (APPEND Z (CADDR S))))))))
                            (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ S (CDR S))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (S)
                              (COND
                               ((NULL (CAR S))
                                (PROGN (SETQ FL (CONS S FL)) NIL))
                               ((NOT
                                 (MEMBER 0
                                         (SETQ Z (PULLBACKRSX RSX (CADR S)))))
                                (LIST
                                 (LIST (CADR S)
                                       (PURGERSX (APPEND Z (CADDR S))))))))
                            (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ S (CDR S))
                   (GO LOOPLABEL)))
         (RETURN
          (APPEND (REVERSE FL)
                  (PROG (S FORALL-RESULT FORALL-ENDPTR)
                    (SETQ S (EDSDISJOIN XL))
                    (COND ((NULL S) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (S) (CONS T S)) (CAR S))
                                          NIL)))
                   LOOPLABEL
                    (SETQ S (CDR S))
                    (COND ((NULL S) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (S) (CONS T S)) (CAR S)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))
     DEPL*)) 
(PUT 'SEMILINEARSQL 'NUMBER-OF-ARGS 3) 
(PUT 'SEMILINEARSQL 'DEFINED-ON-LINE '69) 
(PUT 'SEMILINEARSQL 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SEMILINEARSQL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEMILINEARSQL (XL VL KL)
    (OR (NULL XL)
        (AND (SEMILINEARSQ (CAR XL) VL KL) (SEMILINEARSQL (CDR XL) VL KL)))) 
(PUT 'SEMILINEARSQ 'NUMBER-OF-ARGS 3) 
(PUT 'SEMILINEARSQ 'DEFINED-ON-LINE '75) 
(PUT 'SEMILINEARSQ 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SEMILINEARSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEMILINEARSQ (X VL KL)
    ((LAMBDA (XVARS*) (SEMILINEARPF0 (XPARTITSQ X) VL KL)) VL)) 
(PUT 'SEMILINEARPF0 'NUMBER-OF-ARGS 3) 
(PUT 'SEMILINEARPF0 'DEFINED-ON-LINE '82) 
(PUT 'SEMILINEARPF0 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SEMILINEARPF0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEMILINEARPF0 (F VL KL)
    (OR (NULL F)
        (AND
         (OR
          (AND (EQUAL (CAAR F) 1) (FREEOFFL (CDR (CDAR F)) VL)
               (FREEOFFL (CAR (CDAR F)) VL))
          (AND (EQUAL (LENGTH (WEDGEFAX (CAAR F))) 1)
               (FREEOFFL (CDR (CDAR F)) KL) (FREEOFFL (CAR (CDAR F)) KL)))
         (SEMILINEARPF0 (CDR F) VL KL)))) 
(PUT 'EDSSOLVESEMI 'NUMBER-OF-ARGS 3) 
(PUT 'EDSSOLVESEMI 'DEFINED-ON-LINE '100) 
(PUT 'EDSSOLVESEMI 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSSOLVESEMI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EDSSOLVESEMI (XL VL RSX)
    (PROG (SL NL)
      (SETQ XL (EDSPARTSOLVE XL (CAR VL)))
      (PROG (X)
        (SETQ X (CAR XL))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((NOT (MEMQ (CAR X) (CADR XL)))
             (SETQ SL
                     (CONS
                      (CONS (CAR X)
                            (MK*SQ (SUBS2 (SUBSQ (SIMP* (CDR X)) (CADDR XL)))))
                      SL)))
            ((CAR
              (PROGN
               (SETQ X
                       (ADDSQ (NEGSQ (CONS (LIST (CONS (CONS (CAR X) 1) 1)) 1))
                              (SIMP* (CDR X))))
               (SETQ X (SUBS2 (SUBSQ X (CADDR XL))))))
             (SETQ NL (CONS X NL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ SL (*MAP2RMAP (REVERSIP SL)))
      (COND ((MEMBER 0 (SETQ RSX (PULLBACKRSX RSX (CAR SL)))) (RETURN NIL)))
      (SETQ RSX (PURGERSX (APPEND RSX (CADR SL))))
      (SETQ SL (CAR SL))
      (COND ((NULL NL) (RETURN (LIST (CONS T (LIST SL RSX))))))
      (SETQ XL (EDSSOLVEGRADED NL (CDR VL) RSX))
      (RETURN
       (PROG (S FORALL-RESULT FORALL-ENDPTR)
         (SETQ S XL)
         (COND ((NULL S) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (S)
                             (COND
                              ((NULL (CAR S))
                               (CONS NIL
                                     (APPEND (CDR S)
                                             (PROG (X FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ X SL)
                                               (COND ((NULL X) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (X)
                                                                   (ADDSQ
                                                                    (NEGSQ
                                                                     (CONS
                                                                      (LIST
                                                                       (CONS
                                                                        (CONS
                                                                         (CAR
                                                                          X)
                                                                         1)
                                                                        1))
                                                                      1))
                                                                    (SIMP*
                                                                     (CDR X))))
                                                                 (CAR X))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ X (CDR X))
                                               (COND
                                                ((NULL X)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (X)
                                                           (ADDSQ
                                                            (NEGSQ
                                                             (CONS
                                                              (LIST
                                                               (CONS
                                                                (CONS (CAR X)
                                                                      1)
                                                                1))
                                                              1))
                                                            (SIMP* (CDR X))))
                                                         (CAR X))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))))
                              (T
                               (CONS T
                                     (LIST
                                      (APPEND (PULLBACKMAP SL (CADR S))
                                              (CADR S))
                                      (CADDR S))))))
                           (CAR S))
                          NIL)))
        LOOPLABEL
         (SETQ S (CDR S))
         (COND ((NULL S) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (S)
                     (COND
                      ((NULL (CAR S))
                       (CONS NIL
                             (APPEND (CDR S)
                                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ X SL)
                                       (COND ((NULL X) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (X)
                                                           (ADDSQ
                                                            (NEGSQ
                                                             (CONS
                                                              (LIST
                                                               (CONS
                                                                (CONS (CAR X)
                                                                      1)
                                                                1))
                                                              1))
                                                            (SIMP* (CDR X))))
                                                         (CAR X))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ X (CDR X))
                                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (X)
                                                   (ADDSQ
                                                    (NEGSQ
                                                     (CONS
                                                      (LIST
                                                       (CONS (CONS (CAR X) 1)
                                                             1))
                                                      1))
                                                    (SIMP* (CDR X))))
                                                 (CAR X))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))))
                      (T
                       (CONS T
                             (LIST (APPEND (PULLBACKMAP SL (CADR S)) (CADR S))
                                   (CADDR S))))))
                   (CAR S))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'EDSSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'EDSSOLVE 'DEFINED-ON-LINE '127) 
(PUT 'EDSSOLVE 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSSOLVE (XL VL)
    (PROG (KL SL MSG *ARBVARS)
      (SETQ MSG
              (LIST "Solving" (LENGTH XL) "equations in" (LENGTH VL)
                    "variables"))
      (PROG (Q)
        (SETQ Q XL)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q) (SETQ KL (UNION (KERNELS (CAR Q)) KL))) (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (SETQ KL (EXPANDDEPENDENCE KL))
      (SETQ VL (INTERSECTION VL KL))
      (SETQ MSG (APPEND MSG (LIST (LIST (LENGTH VL) "present"))))
      (EDSDEBUG MSG NIL NIL)
      (SETQ SL
              (EDSSOLVESYS
               (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                 (SETQ Q XL)
                 (COND ((NULL Q) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (Q) (CAR Q)) (CAR Q)) NIL)))
                LOOPLABEL
                 (SETQ Q (CDR Q))
                 (COND ((NULL Q) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (Q) (CAR Q)) (CAR Q)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               VL))
      (COND ((EQCAR SL 'INCONSISTENT) (SETQ SL (LIST)))
            ((OR (EQCAR SL 'FAILED) (EQCAR SL NIL))
             (SETQ SL (LIST (CONS NIL XL))))
            ((EQCAR SL T)
             (SETQ SL
                     (PROG (S FORALL-RESULT FORALL-ENDPTR)
                       (SETQ S (CDR SL))
                      STARTOVER
                       (COND ((NULL S) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (S)
                                  (COND
                                   ((NOT (EXPLICITSOLUTIONP S))
                                    (LIST
                                     (CONS NIL
                                           (PROG (PR FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ PR (PAIR (CADR S) (CAR S)))
                                             (COND ((NULL PR) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (PR)
                                                                 (ADDSQ
                                                                  (NEGSQ
                                                                   (CONS
                                                                    (LIST
                                                                     (CONS
                                                                      (CONS
                                                                       (CAR PR)
                                                                       1)
                                                                      1))
                                                                    1))
                                                                  (CDR PR)))
                                                               (CAR PR))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ PR (CDR PR))
                                             (COND
                                              ((NULL PR)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (PR)
                                                         (ADDSQ
                                                          (NEGSQ
                                                           (CONS
                                                            (LIST
                                                             (CONS
                                                              (CONS (CAR PR) 1)
                                                              1))
                                                            1))
                                                          (CDR PR)))
                                                       (CAR PR))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                   (T
                                    (LIST
                                     (CONS T
                                           (*MAP2RMAP
                                            (PAIR (CADR S)
                                                  (PROG (Q FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ Q (CAR S))
                                                    (COND
                                                     ((NULL Q) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (Q)
                                                                        (MK*SQ
                                                                         (REORDSQ
                                                                          Q)))
                                                                      (CAR Q))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ Q (CDR Q))
                                                    (COND
                                                     ((NULL Q)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (Q)
                                                                (MK*SQ
                                                                 (REORDSQ Q)))
                                                              (CAR Q))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))))))))
                                (CAR S)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ S (CDR S))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL S) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (S)
                                  (COND
                                   ((NOT (EXPLICITSOLUTIONP S))
                                    (LIST
                                     (CONS NIL
                                           (PROG (PR FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ PR (PAIR (CADR S) (CAR S)))
                                             (COND ((NULL PR) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (PR)
                                                                 (ADDSQ
                                                                  (NEGSQ
                                                                   (CONS
                                                                    (LIST
                                                                     (CONS
                                                                      (CONS
                                                                       (CAR PR)
                                                                       1)
                                                                      1))
                                                                    1))
                                                                  (CDR PR)))
                                                               (CAR PR))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ PR (CDR PR))
                                             (COND
                                              ((NULL PR)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (PR)
                                                         (ADDSQ
                                                          (NEGSQ
                                                           (CONS
                                                            (LIST
                                                             (CONS
                                                              (CONS (CAR PR) 1)
                                                              1))
                                                            1))
                                                          (CDR PR)))
                                                       (CAR PR))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                   (T
                                    (LIST
                                     (CONS T
                                           (*MAP2RMAP
                                            (PAIR (CADR S)
                                                  (PROG (Q FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ Q (CAR S))
                                                    (COND
                                                     ((NULL Q) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (Q)
                                                                        (MK*SQ
                                                                         (REORDSQ
                                                                          Q)))
                                                                      (CAR Q))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ Q (CDR Q))
                                                    (COND
                                                     ((NULL Q)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (Q)
                                                                (MK*SQ
                                                                 (REORDSQ Q)))
                                                              (CAR Q))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))))))))
                                (CAR S)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ S (CDR S))
                       (GO LOOPLABEL))))
            (T (ERRDHH (LIST "Unexpected result from solvesys:" SL))))
      (RETURN SL))) 
(PUT 'EXPANDDEPENDENCE 'NUMBER-OF-ARGS 1) 
(PUT 'EXPANDDEPENDENCE 'DEFINED-ON-LINE '162) 
(PUT 'EXPANDDEPENDENCE 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EXPANDDEPENDENCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPANDDEPENDENCE (KL)
    (COND ((NULL KL) NIL)
          ((ATOMF (CAR KL)) (CONS (CAR KL) (EXPANDDEPENDENCE (CDR KL))))
          (T (CONS (CAR KL) (EXPANDDEPENDENCE (APPEND (CDAR KL) (CDR KL))))))) 
(PUT 'EDSSOLVESYS 'NUMBER-OF-ARGS 2) 
(PUT 'EDSSOLVESYS 'DEFINED-ON-LINE '173) 
(PUT 'EDSSOLVESYS 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSSOLVESYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSSOLVESYS (XL VL) (SOLVESYS XL VL)) 
(PUT 'EXPLICITSOLUTIONP 'NUMBER-OF-ARGS 1) 
(PUT 'EXPLICITSOLUTIONP 'DEFINED-ON-LINE '179) 
(PUT 'EXPLICITSOLUTIONP 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EXPLICITSOLUTIONP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPLICITSOLUTIONP (S)
    (AND (NOT (SMEMBER 'ROOT_OF S)) (NOT (SMEMBER 'ONE_OF S)))) 
(PUT 'EDSPARTSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'EDSPARTSOLVE 'DEFINED-ON-LINE '184) 
(PUT 'EDSPARTSOLVE 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSPARTSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSPARTSOLVE (XL VL)
    ((LAMBDA (DEPL*)
       (PROG (AL IL L)
         (SETQ XL (SPLITLINEAREQUATIONS XL VL))
         (SETQ XL
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P XL)
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P)
                                       (COND ((NULL (CAR (CDR P))) (CAR P))
                                             ((SETQ L
                                                      (MEMBER (MK*SQ (CDR P))
                                                              IL))
                                              (ADDSQ (CAR P)
                                                     (CONS
                                                      (LIST
                                                       (CONS
                                                        (CONS
                                                         (NTH AL
                                                              (PLUS 1
                                                                    (DIFFERENCE
                                                                     (LENGTH
                                                                      IL)
                                                                     (LENGTH
                                                                      L))))
                                                         1)
                                                        1))
                                                      1)))
                                             (T
                                              (PROGN
                                               (SETQ AL
                                                       (CONS (INTERN (GENSYM))
                                                             AL))
                                               (SETQ IL
                                                       (CONS (MK*SQ (CDR P))
                                                             IL))
                                               (ADDSQ (CAR P)
                                                      (CONS
                                                       (LIST
                                                        (CONS (CONS (CAR AL) 1)
                                                              1))
                                                       1))))))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P)
                               (COND ((NULL (CAR (CDR P))) (CAR P))
                                     ((SETQ L (MEMBER (MK*SQ (CDR P)) IL))
                                      (ADDSQ (CAR P)
                                             (CONS
                                              (LIST
                                               (CONS
                                                (CONS
                                                 (NTH AL
                                                      (PLUS 1
                                                            (DIFFERENCE
                                                             (LENGTH IL)
                                                             (LENGTH L))))
                                                 1)
                                                1))
                                              1)))
                                     (T
                                      (PROGN
                                       (SETQ AL (CONS (INTERN (GENSYM)) AL))
                                       (SETQ IL (CONS (MK*SQ (CDR P)) IL))
                                       (ADDSQ (CAR P)
                                              (CONS
                                               (LIST
                                                (CONS (CONS (CAR AL) 1) 1))
                                               1))))))
                             (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ AL (REVERSIP AL))
         (SETQ IL (REVERSIP IL))
         (PROG (Y)
           (SETQ Y VL)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROG (Z)
                (SETQ Z AL)
               LAB
                (COND ((NULL Z) (RETURN NIL)))
                ((LAMBDA (Z) (DEPEND1 Y Z T)) (CAR Z))
                (SETQ Z (CDR Z))
                (GO LAB)))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (SETQ XL (EDSSOLVE XL (APPEND VL AL)))
         (COND
          ((OR (NEQ (LENGTH XL) 1) (NULL (CAR (SETQ XL (CAR XL)))))
           (ERRDHH (LIST "Bad solution in edspartsolve" XL))))
         (RETURN (LIST (CADR XL) AL (PAIR AL IL)))))
     DEPL*)) 
(PUT 'SPLITLINEAREQUATIONS 'NUMBER-OF-ARGS 2) 
(PUT 'SPLITLINEAREQUATIONS 'DEFINED-ON-LINE '216) 
(PUT 'SPLITLINEAREQUATIONS 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SPLITLINEAREQUATIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPLITLINEAREQUATIONS (V C)
    (PROG (OK G H *EXP)
      (SETQ *EXP T)
      (SETQ OK (SETKORDER C))
      (SETQ V
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q V)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Q)
                                    ((LAMBDA (F D)
                                       (PROGN
                                        (SETQ G (SETQ H NIL))
                                        (PROG ()
                                         WHILELABEL
                                          (COND
                                           ((NOT
                                             (AND
                                              (NOT
                                               (OR (ATOM F) (ATOM (CAR F))))
                                              (MEMQ (CAAAR F) C)))
                                            (RETURN NIL)))
                                          (PROGN
                                           (SETQ G (CONS (CAR F) G))
                                           (SETQ F (CDR F)))
                                          (GO WHILELABEL))
                                        (PROG (U)
                                          (SETQ U G)
                                         LAB
                                          (COND ((NULL U) (RETURN NIL)))
                                          ((LAMBDA (U) (SETQ H (CONS U H)))
                                           (CAR U))
                                          (SETQ U (CDR U))
                                          (GO LAB))
                                        (CONS (CANCEL (CONS H D))
                                              (CANCEL (CONS F D)))))
                                     (REORDER (CAR Q)) (REORDER (CDR Q))))
                                  (CAR Q))
                                 NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Q)
                            ((LAMBDA (F D)
                               (PROGN
                                (SETQ G (SETQ H NIL))
                                (PROG ()
                                 WHILELABEL
                                  (COND
                                   ((NOT
                                     (AND (NOT (OR (ATOM F) (ATOM (CAR F))))
                                          (MEMQ (CAAAR F) C)))
                                    (RETURN NIL)))
                                  (PROGN
                                   (SETQ G (CONS (CAR F) G))
                                   (SETQ F (CDR F)))
                                  (GO WHILELABEL))
                                (PROG (U)
                                  (SETQ U G)
                                 LAB
                                  (COND ((NULL U) (RETURN NIL)))
                                  ((LAMBDA (U) (SETQ H (CONS U H))) (CAR U))
                                  (SETQ U (CDR U))
                                  (GO LAB))
                                (CONS (CANCEL (CONS H D))
                                      (CANCEL (CONS F D)))))
                             (REORDER (CAR Q)) (REORDER (CDR Q))))
                          (CAR Q))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETKORDER OK)
      (RETURN
       (PROG (P FORALL-RESULT FORALL-ENDPTR)
         (SETQ P V)
         (COND ((NULL P) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (P)
                             (CONS (REORDSQ (CAR P)) (REORDSQ (CDR P))))
                           (CAR P))
                          NIL)))
        LOOPLABEL
         (SETQ P (CDR P))
         (COND ((NULL P) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (P) (CONS (REORDSQ (CAR P)) (REORDSQ (CDR P))))
                   (CAR P))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'EDSGRADECOORDS 'NUMBER-OF-ARGS 2) 
(PUT 'EDSGRADECOORDS 'DEFINED-ON-LINE '238) 
(PUT 'EDSGRADECOORDS 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'EDSGRADECOORDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSGRADECOORDS (CRD JET0)
    (COND ((NULL JET0) (LIST CRD))
          (T
           (PROG (U)
             (PROG (C)
               (SETQ C CRD)
              LAB
               (COND ((NULL C) (RETURN NIL)))
               ((LAMBDA (C)
                  (PROG (J0 C0)
                    (SETQ J0 JET0)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND J0
                              (NULL (SETQ C0 (SPLITOFFINDICES (CAR J0) C)))))
                        (RETURN NIL)))
                      (SETQ J0 (CDR J0))
                      (GO WHILELABEL))
                    (COND ((SETQ J0 (ASSOC (LENGTH C0) U)) (NCONC J0 (LIST C)))
                          (T (SETQ U (CONS (CONS (LENGTH C0) (LIST C)) U))))))
                (CAR C))
               (SETQ C (CDR C))
               (GO LAB))
             (RETURN
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (SORT U (FUNCTION GREATERPCAR)))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (CDR V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CDR V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'SOLVEPFSYS 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEPFSYS 'DEFINED-ON-LINE '262) 
(PUT 'SOLVEPFSYS 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SOLVEPFSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEPFSYS (SYS) (SOLVEPFSYS1 SYS (LIST))) 
(PUT 'SOLVEPFSYS1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEPFSYS1 'DEFINED-ON-LINE '267) 
(PUT 'SOLVEPFSYS1 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SOLVEPFSYS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEPFSYS1 (SYS VARS)
    (PROG (OK SL NL)
      (SETQ OK (UPDKORDL NIL))
      (SETQ NL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F SYS)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (SUBS2PF* F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (SUBS2PF* F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (NL
        (PROG ()
          (SETQ NL (SOLVEPFSYSEASY NL VARS 'CFRMCONSTANT))
          (COND
           ((CAR NL)
            (EDSDEBUG "Solved with constant coefficients:" (CAR NL) 'SYS)))
          (COND
           ((CADR NL)
            (EDSDEBUG "Unsolved with constant coefficients:" (CADR NL) 'SYS)))
          (SETQ SL (APPEND SL (CAR NL)))
          (SETQ NL (CADR NL)))))
      (COND
       (NL
        (PROG ()
          (SETQ NL (SOLVEPFSYSEASY NL VARS 'CFRMNOWHEREZERO))
          (COND
           ((CAR NL)
            (EDSDEBUG "Solved with nonzero coefficients:" (CAR NL) 'SYS)))
          (COND
           ((CADR NL)
            (EDSDEBUG "Unsolved with nonzero coefficients:" (CADR NL) 'SYS)))
          (SETQ SL (APPEND SL (CAR NL)))
          (SETQ NL (CADR NL)))))
      (COND
       (NL
        (PROG ()
          (SETQ NL (SOLVEPFSYSHARD NL VARS))
          (COND
           ((CAR NL) (EDSDEBUG "Solved with Cramer's rule:" (CAR NL) 'SYS)))
          (COND
           ((CADR NL)
            (EDSDEBUG "Unsolved with Cramer's rule:" (CADR NL) 'SYS)))
          (SETQ SL (APPEND SL (CAR NL)))
          (SETQ NL (CADR NL)))))
      (SETKORDER OK)
      (UPDKORDL (LPOWS SL))
      (SETQ SL (XAUTOREDUCE1 (XREORDERSYS SL)))
      (SETQ NL (XREORDERSYS NL))
      (SETQ SL (SORTSYS SL OK))
      (UPDKORDL (LPOWS SL))
      (RETURN (LIST SL NL)))) 
(PUT 'SOLVEPFSYSEASY 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVEPFSYSEASY 'DEFINED-ON-LINE '324) 
(PUT 'SOLVEPFSYSEASY 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SOLVEPFSYSEASY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEPFSYSEASY (SYS VARS TEST)
    (PROG (SL NL KL)
      (SETQ KL
              (PURGE
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F SYS)
                STARTOVER
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (F) (XSOLVEABLES F TEST)) (CAR F)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ F (CDR F))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (F) (XSOLVEABLES F TEST)) (CAR F)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ F (CDR F))
                 (GO LOOPLABEL))))
      (SETQ KL (SORT (COND (VARS (INTERSECTION KL VARS)) (T KL)) 'TERMORDP))
      (COND ((NULL KL) (RETURN (LIST (LIST) SYS))))
      (UPDKORDL KL)
      (PROG (F)
        (SETQ F SYS)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((AND (SETQ F (XREDUCE (XREORDER F) SL))
                  (APPLY1 TEST (CAR (CDAR F))))
             (SETQ SL (CONS (XNORMALISE F) SL)))
            (F (SETQ NL (CONS F NL)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ SL (REVERSIP SL))
      (SETQ NL
              (REVERSIP
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F NL)
                STARTOVER
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (F)
                            (COND
                             ((SETQ F (SUBS2PF* (XREDUCE F SL))) (LIST F))))
                          (CAR F)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ F (CDR F))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (F)
                            (COND
                             ((SETQ F (SUBS2PF* (XREDUCE F SL))) (LIST F))))
                          (CAR F)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ F (CDR F))
                 (GO LOOPLABEL))))
      (COND ((OR (NULL SL) (NULL NL)) (RETURN (LIST SL NL))))
      (SETQ NL (SOLVEPFSYSEASY NL VARS TEST))
      (RETURN (LIST (APPEND SL (CAR NL)) (CADR NL))))) 
(PUT 'XSOLVEABLES 'NUMBER-OF-ARGS 2) 
(PUT 'XSOLVEABLES 'DEFINED-ON-LINE '350) 
(PUT 'XSOLVEABLES 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'XSOLVEABLES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XSOLVEABLES (F TEST)
    (COND ((NULL F) NIL)
          ((APPLY1 TEST (CAR (CDAR F)))
           (CONS (CAAR F) (XSOLVEABLES (CDR F) TEST)))
          (T (XSOLVEABLES (CDR F) TEST)))) 
(PUT 'SOLVEPFSYSHARD 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEPFSYSHARD 'DEFINED-ON-LINE '358) 
(PUT 'SOLVEPFSYSHARD 'DEFINED-IN-FILE 'EDS/EDSSOLVE.RED) 
(PUT 'SOLVEPFSYSHARD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEPFSYSHARD (SYS VARS)
    (PROG (SL NL KL W)
      (COND ((NULL SYS) (RETURN (LIST (LIST) (LIST)))))
      (COND (VARS (UPDKORDL VARS)))
      (SETQ W (CONS (CONS 1 (CONS 1 1)) NIL))
      (PROG (F)
        (SETQ F SYS)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((SETQ F (SUBS2PF (WEDGEPF (COND (VARS (XREORDER F)) (T F)) W)))
             (COND
              ((OR (NULL VARS) (SUBSETP (WEDGEFAX (CAAR F)) VARS)) (SETQ W F))
              (T
               (PROGN
                (COND
                 ((NEQ (DEGREEPF F) 1)
                  (SETQ F (XCOEFF F (INTERSECTION (WEDGEFAX (CAAR F)) VARS)))))
                (SETQ NL (CONS (MULTPFSQ F (INVSQ (CDAR W))) NL))))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ KL (XSOLVEABLES W 'CFRMNOWHEREZERO))
      (COND ((AND (NULL KL) *EDSSLOPPY) (SETQ KL (XPOWS W))))
      (COND
       (VARS
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (AND KL (NOT (SUBSETP (WEDGEFAX (CAR KL)) VARS))))
            (RETURN NIL)))
          (SETQ KL (CDR KL))
          (GO WHILELABEL))))
      (COND ((OR (NULL KL) (EQUAL (CAR KL) 1)) (RETURN (LIST (LIST) SYS))))
      (SETQ KL (WEDGEFAX (CAR KL)))
      (COND
       (*EDSSLOPPY
        ((LAMBDA (XVARS*)
           (SETQ CFRMRSX*
                   (CONS (XPARTITSQ (CONS (CAR (CDAR (XCOEFF W KL))) 1))
                         CFRMRSX*)))
         CFRMCRD*)))
      (SETQ SL
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K KL)
                (COND ((NULL K) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (K) (XCOEFF W (DELETE K KL)))
                                  (CAR K))
                                 NIL)))
               LOOPLABEL
                (SETQ K (CDR K))
                (COND ((NULL K) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (K) (XCOEFF W (DELETE K KL))) (CAR K))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (UPDKORDL KL)
      (RETURN
       (LIST
        (PROG (F FORALL-RESULT FORALL-ENDPTR)
          (SETQ F SL)
          (COND ((NULL F) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F) (XNORMALISE (XREORDER F))) (CAR F))
                           NIL)))
         LOOPLABEL
          (SETQ F (CDR F))
          (COND ((NULL F) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (F) (XNORMALISE (XREORDER F))) (CAR F)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        (PROG (F FORALL-RESULT FORALL-ENDPTR)
          (SETQ F NL)
          (COND ((NULL F) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F)) NIL)))
         LOOPLABEL
          (SETQ F (CDR F))
          (COND ((NULL F) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (F) (XREPARTIT* F)) (CAR F)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(ENDMODULE) 