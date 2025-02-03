(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYSUBST)) 
(EXPORTS (LIST 'SUBSUBTAYLOR)) 
(IMPORTS
 (LIST 'ADDSQ 'DENR 'DEPENDS 'EXPTSQ 'INVSQ 'MULTSQ 'NLIST 'NTH 'NUMR 'PREPSQ
       'REPLACE-NTH-NTH 'REVAL 'REVERSIP 'SIMP* 'SORT 'SUBEVAL1 'SUBS2* 'SUBSQ
       'SUBTRSQ 'TYPERR 'MAKE-TAYLOR* 'SET-TAYCFSQ 'TAYCFPL 'TAYCFSQ
       'TAYCOEFFLIST 'TAYFLAGS '|TAYLOR:| 'TAYLOR-ERROR 'TAYVARS 'TAYMAKECOEFF
       'TAYORIG 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER 'TAYTPELPOINT
       'TAYTPELVARS 'CONSTANT-SQ-P 'DELETE-NTH 'DELETE-NTH-NTH 'REPLACE-NTH
       'TAYLOR-ERROR 'TAYLOR-ERROR* 'VAR-IS-NTH 'ENTER-SORTED 'RAT-KERN-POW)) 
(FLUID '(*TAYLORKEEPORIGINAL)) 
(PUT 'TAYLOR* 'SUBFUNC 'SUBSUBTAYLOR) 
(PUT 'SUBSUBTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSUBTAYLOR 'DEFINED-ON-LINE '62) 
(PUT 'SUBSUBTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'SUBSUBTAYLOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSUBTAYLOR (L V)
    (PROG (X CLIST DELETE_LIST TP PL *MCD)
      (SETQ *MCD T)
      (SETQ CLIST
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U (CADR V))
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (CONS (CAR U) (SUBSQ (CDR U) L)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U) (CONS (CAR U) (SUBSQ (CDR U) L)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TP (CADDR V))
      (SETQ TP
              (PROG (QUARTET FORALL-RESULT FORALL-ENDPTR)
                (SETQ QUARTET TP)
                (COND ((NULL QUARTET) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (QUARTET)
                                    (LIST (CAR QUARTET)
                                          (REVAL1 (SUBEVAL1 L (CADR QUARTET))
                                                  T)
                                          (CADDR QUARTET) (CADDDR QUARTET)))
                                  (CAR QUARTET))
                                 NIL)))
               LOOPLABEL
                (SETQ QUARTET (CDR QUARTET))
                (COND ((NULL QUARTET) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (QUARTET)
                            (LIST (CAR QUARTET)
                                  (REVAL1 (SUBEVAL1 L (CADR QUARTET)) T)
                                  (CADDR QUARTET) (CADDDR QUARTET)))
                          (CAR QUARTET))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PL
              (PROG (QUARTET FORALL-RESULT FORALL-ENDPTR)
                (SETQ QUARTET TP)
                (COND ((NULL QUARTET) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (QUARTET)
                                    (NLIST NIL (LENGTH (CAR QUARTET))))
                                  (CAR QUARTET))
                                 NIL)))
               LOOPLABEL
                (SETQ QUARTET (CDR QUARTET))
                (COND ((NULL QUARTET) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (QUARTET) (NLIST NIL (LENGTH (CAR QUARTET))))
                          (CAR QUARTET))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (P)
        (SETQ P L)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((MEMBER (CAR P)
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CADDR V))
                      STARTOVER
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ X (CDR X))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ X (CDR X))
                       (GO LOOPLABEL)))
             (PROG (TEMP W ABOUT POS POS1)
               (SETQ POS 0)
               (SETQ POS1 0)
               (SETQ TEMP (SIMP* (CDR P)))
               (SETQ W (VAR-IS-NTH TP (CAR P)))
               (SETQ POS (CAR W))
               (SETQ POS1 (CDR W))
               (SETQ ABOUT (CADR (NTH TP POS)))
               (COND
                ((OR (CONSTANT-SQ-P TEMP) (EQUAL ABOUT (CDR P)))
                 (PROG (LL Y Z)
                   (COND
                    ((NOT (NULL (NTH (NTH PL POS) POS1)))
                     (TAYLOR-ERROR 'INVALID-SUBST
                      "multiple substitution for same variable")))
                   (SETQ PL (REPLACE-NTH-NTH PL POS POS1 0))
                   (COND
                    ((EQ ABOUT 'INFINITY)
                     (COND
                      ((NULL (CAR TEMP))
                       (TAYLOR-ERROR* 'ZERO-DENOM "Taylor Substitution"))
                      (T (SETQ TEMP (INVSQ TEMP)))))
                    (T (SETQ TEMP (ADDSQ TEMP (NEGSQ (SIMP* ABOUT))))))
                   (PROG (PP)
                     (SETQ PP DELETE_LIST)
                    LAB
                     (COND ((NULL PP) (RETURN NIL)))
                     ((LAMBDA (PP)
                        (COND
                         ((LESSP (CAR PP) POS) (SETQ POS (DIFFERENCE POS 1)))))
                      (CAR PP))
                     (SETQ PP (CDR PP))
                     (GO LAB))
                   (SETQ DELETE_LIST (CONS (CONS POS POS1) DELETE_LIST))
                   (PROG (CC)
                     (SETQ CC CLIST)
                    LAB
                     (COND ((NULL CC) (RETURN NIL)))
                     ((LAMBDA (CC)
                        (PROG (EXPONENT)
                          (SETQ W (NTH (CAR CC) POS))
                          (SETQ W
                                  (COND
                                   ((NULL (CDR W)) (DELETE-NTH (CAR CC) POS))
                                   (T (DELETE-NTH-NTH (CAR CC) POS POS1))))
                          (SETQ EXPONENT (NTH (NTH (CAR CC) POS) POS1))
                          (SETQ Z
                                  (COND ((EQUAL EXPONENT 0) (CDR CC))
                                        ((AND (TAYEXP-LESSP EXPONENT 0)
                                              (NULL (CAR TEMP)))
                                         (TAYLOR-ERROR* 'ZERO-DENOM
                                          "Taylor Substitution"))
                                        (T
                                         (MULTSQ (CDR CC)
                                                 (EXPTSQ TEMP EXPONENT)))))
                          (SETQ Y (ASSOC W LL))
                          (COND (Y (RPLACD Y (SUBS2* (ADDSQ (CDR Y) Z))))
                                ((NOT (NULL (CAR (SETQ Z (SUBS2* Z)))))
                                 (SETQ LL (CONS (CONS W Z) LL))))))
                      (CAR CC))
                     (SETQ CC (CDR CC))
                     (GO LAB))
                   (SETQ CLIST NIL)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT LL) (RETURN NIL)))
                     (PROGN
                      (COND
                       ((NOT (NULL (CAR (CDR (CAR LL)))))
                        (SETQ CLIST (ENTER-SORTED (CAR LL) CLIST))))
                      (SETQ LL (CDR LL)))
                     (GO WHILELABEL))))
                ((NOT
                  (AND (EQUAL (CDR TEMP) 1)
                       (SETQ TEMP (RAT-KERN-POW (CAR TEMP) T))))
                 (TYPERR (LIST 'REPLACEBY (CAR P) (CDR P))
                         "Taylor substitution"))
                (T
                 (PROG (W EXPO POS POS1)
                   (SETQ POS 0)
                   (SETQ POS1 0)
                   (SETQ EXPO (CDR TEMP))
                   (SETQ TEMP (CAR TEMP))
                   (PROG (EL)
                     (SETQ EL
                             (DELETE (CAR P)
                                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ X (CADDR V))
                                      STARTOVER
                                       (COND ((NULL X) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               ((LAMBDA (X)
                                                  (APPEND (CAR X) NIL))
                                                (CAR X)))
                                       (SETQ FORALL-ENDPTR
                                               (LASTPAIR FORALL-RESULT))
                                       (SETQ X (CDR X))
                                       (COND
                                        ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                      LOOPLABEL
                                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               ((LAMBDA (X)
                                                  (APPEND (CAR X) NIL))
                                                (CAR X)))
                                       (SETQ FORALL-ENDPTR
                                               (LASTPAIR FORALL-ENDPTR))
                                       (SETQ X (CDR X))
                                       (GO LOOPLABEL))))
                    LAB
                     (COND ((NULL EL) (RETURN NIL)))
                     ((LAMBDA (EL)
                        (COND
                         ((DEPENDS TEMP EL)
                          (TAYLOR-ERROR 'INVALID-SUBST
                           (LIST "dependent variables" (CDR P) EL)))))
                      (CAR EL))
                     (SETQ EL (CDR EL))
                     (GO LAB))
                   (COND
                    ((NOT (EQUAL EXPO 1))
                     (PROGN
                      (SETQ W (VAR-IS-NTH TP (CAR P)))
                      (SETQ POS (CAR W))
                      (SETQ POS1 (CDR W))
                      (COND
                       ((NOT (NULL (NTH (NTH PL POS) POS1)))
                        (TAYLOR-ERROR 'INVALID-SUBST
                         "different powers in homogeneous template")))
                      (SETQ PL (REPLACE-NTH-NTH PL POS POS1 EXPO)))))
                   (SETQ X (CONS (CONS (CAR P) TEMP) X)))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (PP)
        (SETQ PP (SORT DELETE_LIST (FUNCTION SORTPRED)))
       LAB
        (COND ((NULL PP) (RETURN NIL)))
        ((LAMBDA (PP)
           ((LAMBDA (U)
              (PROGN
               (COND
                ((NULL (CDR (CAR U)))
                 (PROGN
                  (SETQ TP (DELETE-NTH TP (CAR PP)))
                  (SETQ PL (DELETE-NTH PL (CAR PP)))))
                (T
                 (PROGN
                  (SETQ TP
                          (REPLACE-NTH TP (CAR PP)
                           (LIST (DELETE-NTH (CAR U) (CDR PP)) (CADR U)
                                 (CADDR U) (CADDDR U))))
                  (SETQ PL (DELETE-NTH-NTH PL (CAR PP) (CDR PP))))))))
            (NTH TP (CAR PP))))
         (CAR PP))
        (SETQ PP (CDR PP))
        (GO LAB))
      (COND
       ((NULL TP)
        (RETURN (COND ((NULL CLIST) 0) (T (PREPSQ (CDR (CAR CLIST))))))))
      (SETQ X (REVERSIP X))
      (SETQ PL (CHECK-PL PL))
      (COND
       ((NULL PL)
        (TAYLOR-ERROR 'INVALID-SUBST
         "different powers in homogeneous template")))
      (RETURN
       (COND
        ((EQUAL PL (NLIST 1 (LENGTH TP)))
         (LIST 'TAYLOR* CLIST (SUBLIS X TP)
               (COND
                ((AND *TAYLORKEEPORIGINAL (CADDDR V)) (SUBSQ (CADDDR V) L))
                (T NIL))
               (CAR (CDDDDR V))))
        (T
         (LIST 'TAYLOR* (CHANGE-COEFFLIST CLIST PL)
               (CHANGE-TP (SUBLIS X TP) PL)
               (COND
                ((AND *TAYLORKEEPORIGINAL (CADDDR V)) (SUBSQ (CADDDR V) L))
                (T NIL))
               (CAR (CDDDDR V)))))))) 
(PUT 'SORTPRED 'NUMBER-OF-ARGS 2) 
(PUT 'SORTPRED 'DEFINED-ON-LINE '198) 
(PUT 'SORTPRED 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'SORTPRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORTPRED (U V)
    (OR (GREATERP (CAR U) (CAR V))
        (AND (EQUAL (CAR U) (CAR V)) (GREATERP (CDR U) (CDR V))))) 
(PUT 'CHECK-PL 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-PL 'DEFINED-ON-LINE '201) 
(PUT 'CHECK-PL 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'CHECK-PL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-PL (PL)
    (COND ((NULL PL) NIL)
          (T
           ((LAMBDA (N)
              (COND ((EQUAL N 0) (CHECK-PL (CDR PL)))
                    ((AND N (TAYEXP-LESSP N 0)) NIL)
                    (T (CONS N (CHECK-PL (CDR PL))))))
            (CHECK-PL0 (CAR (CAR PL)) (CDR (CAR PL))))))) 
(PUT 'CHECK-PL0 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK-PL0 'DEFINED-ON-LINE '209) 
(PUT 'CHECK-PL0 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'CHECK-PL0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK-PL0 (N NL)
    (COND ((NULL NL) N) (T (AND (EQUAL N (CAR NL)) (CHECK-PL0 N (CDR NL)))))) 
(PUT 'CHANGE-COEFFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'CHANGE-COEFFLIST 'DEFINED-ON-LINE '212) 
(PUT 'CHANGE-COEFFLIST 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'CHANGE-COEFFLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHANGE-COEFFLIST (CFLIST PL)
    (PROG (CF FORALL-RESULT FORALL-ENDPTR)
      (SETQ CF CFLIST)
      (COND ((NULL CF) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (CF) (CONS (CHANGE-PL (CAR CF) PL) (CDR CF)))
                        (CAR CF))
                       NIL)))
     LOOPLABEL
      (SETQ CF (CDR CF))
      (COND ((NULL CF) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (CF) (CONS (CHANGE-PL (CAR CF) PL) (CDR CF))) (CAR CF))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CHANGE-TP 'NUMBER-OF-ARGS 2) 
(PUT 'CHANGE-TP 'DEFINED-ON-LINE '216) 
(PUT 'CHANGE-TP 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'CHANGE-TP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHANGE-TP (TP PL)
    (COND ((NULL TP) NIL)
          (T
           (CONS
            (COND ((NULL (CAR PL)) (CAR TP))
                  (T
                   (LIST (CAR (CAR TP)) (CADR (CAR TP))
                         (TAYEXP-TIMES (CADDR (CAR TP)) (CAR PL))
                         (TAYEXP-TIMES (CADDDR (CAR TP)) (CAR PL)))))
            (CDR TP))))) 
(PUT 'CHANGE-PL 'NUMBER-OF-ARGS 2) 
(PUT 'CHANGE-PL 'DEFINED-ON-LINE '225) 
(PUT 'CHANGE-PL 'DEFINED-IN-FILE 'TAYLOR/TAYSUBST.RED) 
(PUT 'CHANGE-PL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHANGE-PL (PL PL0)
    (COND ((NULL PL) NIL)
          (T
           (CONS
            (COND ((NULL (CAR PL0)) (CAR PL))
                  (T
                   (PROG (N FORALL-RESULT FORALL-ENDPTR)
                     (SETQ N (CAR PL))
                     (COND ((NULL N) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (N) (TAYEXP-TIMES (CAR PL0) N))
                                       (CAR N))
                                      NIL)))
                    LOOPLABEL
                     (SETQ N (CDR N))
                     (COND ((NULL N) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (N) (TAYEXP-TIMES (CAR PL0) N)) (CAR N))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
            (CHANGE-PL (CDR PL) (CDR PL0)))))) 
(ENDMODULE) 