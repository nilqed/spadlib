(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBF)) 
(PUT 'LEXEFGB 'SIMPFG '((T (PUT 'CALI 'EFGB 'LEX)) (NIL (REMPROP 'CALI 'EFGB)))) 
(PUT 'GROEBF=PROBLEMSORT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=PROBLEMSORT 'DEFINED-ON-LINE '74) 
(PUT 'GROEBF=PROBLEMSORT 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=PROBLEMSORT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=PROBLEMSORT (A B)
    (OR (LESSP (NTH A 4) (NTH B 4))
        (AND (EQUAL (NTH A 4) (NTH B 4))
             (LEQ (LENGTH (SECOND A)) (LENGTH (SECOND B)))))) 
(PUT 'GROEBF=RESULTSORT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=RESULTSORT 'DEFINED-ON-LINE '79) 
(PUT 'GROEBF=RESULTSORT 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=RESULTSORT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=RESULTSORT (A B) (GREATERP (THIRD A) (THIRD B))) 
(PUT 'GROEBFACTOR 'PSOPFN 'INTF=GROEBFACTOR) 
(PUT 'INTF=GROEBFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'INTF=GROEBFACTOR 'DEFINED-ON-LINE '85) 
(PUT 'INTF=GROEBFACTOR 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'INTF=GROEBFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTF=GROEBFACTOR (M)
    (PROG (BAS CON)
      (SETQ BAS (DPMAT_FROM_A (REVAL1 (FIRST M) T)))
      (COND ((EQUAL (LENGTH M) 1) (SETQ CON NIL))
            ((EQUAL (LENGTH M) 2)
             (SETQ CON
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CDR (REVAL1 (SECOND M) T)))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (DP_FROM_A X)) (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (DP_FROM_A X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T (REDERR "Syntax : GROEBFACTOR(base list [,constraint list])")))
      (RETURN
       (CONS 'LIST
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (GROEBFACTOR* BAS CON))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X) (DPMAT_2A (FIRST X))) (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (DPMAT_2A (FIRST X))) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(FLAG '(LISTGROEBFACTOR) 'OPFN) 
(PUT 'LISTGROEBFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'LISTGROEBFACTOR 'DEFINED-ON-LINE '97) 
(PUT 'LISTGROEBFACTOR 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'LISTGROEBFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTGROEBFACTOR (L)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X
                      (LISTGROEBFACTOR*
                       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Y (CDR (REVAL1 L T)))
                         (COND ((NULL Y) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Y) (DPMAT_FROM_A Y))
                                           (CAR Y))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Y (CDR Y))
                         (COND ((NULL Y) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (Y) (DPMAT_FROM_A Y)) (CAR Y))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (LISTGROEBFACTOR* L)))) 
(PUT 'LISTGROEBFACTOR* 'NUMBER-OF-ARGS 1) 
(PUT 'LISTGROEBFACTOR* 'DEFINED-ON-LINE '106) 
(PUT 'LISTGROEBFACTOR* 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'LISTGROEBFACTOR* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTGROEBFACTOR* (L)
    (PROG (GBS)
      (SETQ GBS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X
                        (GROEBF=PREPROCESS NIL
                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                           (SETQ X L)
                           (COND ((NULL X) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (X) (LIST X NIL)) (CAR X))
                                            NIL)))
                          LOOPLABEL
                           (SETQ X (CDR X))
                           (COND ((NULL X) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (LIST X NIL)) (CAR X))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GBS (SORT GBS (FUNCTION GROEBF=PROBLEMSORT)))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (GROEBF=MASTERPROCESS GBS NIL))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (FIRST X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (FIRST X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'GROEBFACTOR* 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBFACTOR* 'DEFINED-ON-LINE '116) 
(PUT 'GROEBFACTOR* 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBFACTOR* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBFACTOR* (BAS POLY)
    (COND
     ((GREATERP (DPMAT_COLS BAS) 0)
      (REDERR "GROEBFACTOR only for ideal bases"))
     ((NULL *NOETHERIAN)
      (REDERR "GROEBFACTOR only for noetherian term orders"))
     ((DPMAT_ZERO? BAS) (LIST (LIST BAS POLY)))
     (T
      (PROG (GBS)
        (COND
         ((GREATERP (CALI_TRACE) 5)
          (PROGN
           (PROGN (PRIN2 "GROEBFACTOR the system ") NIL)
           (DPMAT_PRINT BAS))))
        (SETQ GBS
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (GROEBF=PREPROCESS NIL (LIST (LIST BAS POLY))))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ GBS (SORT GBS (FUNCTION GROEBF=PROBLEMSORT)))
        (RETURN (GROEBF=MASTERPROCESS GBS NIL)))))) 
(PUT 'EXTENDEDGROEBFACTOR 'PSOPFN 'INTF=EXTENDEDGROEBFACTOR) 
(PUT 'INTF=EXTENDEDGROEBFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'INTF=EXTENDEDGROEBFACTOR 'DEFINED-ON-LINE '135) 
(PUT 'INTF=EXTENDEDGROEBFACTOR 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'INTF=EXTENDEDGROEBFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTF=EXTENDEDGROEBFACTOR (M)
    (PROG (BAS CON)
      (SETQ BAS (DPMAT_FROM_A (REVAL1 (FIRST M) T)))
      (COND ((EQUAL (LENGTH M) 1) (SETQ CON NIL))
            ((EQUAL (LENGTH M) 2)
             (SETQ CON
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CDR (REVAL1 (SECOND M) T)))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (DP_FROM_A X)) (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (DP_FROM_A X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T
             (REDERR
              "Syntax : EXTENDEDGROEBFACTOR(base list [,constraint list])")))
      (RETURN
       (CONS 'LIST
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (EXTENDEDGROEBFACTOR* BAS CON))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (CONS 'LIST
                                         (LIST (FIRST X)
                                               (CONS 'LIST (SECOND X))
                                               (CONS 'LIST (THIRD X)))))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (CONS 'LIST
                                 (LIST (FIRST X) (CONS 'LIST (SECOND X))
                                       (CONS 'LIST (THIRD X)))))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'EXTENDEDGROEBFACTOR* 'NUMBER-OF-ARGS 2) 
(PUT 'EXTENDEDGROEBFACTOR* 'DEFINED-ON-LINE '148) 
(PUT 'EXTENDEDGROEBFACTOR* 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'EXTENDEDGROEBFACTOR* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXTENDEDGROEBFACTOR* (BAS POLY)
    (COND
     ((GREATERP (DPMAT_COLS BAS) 0)
      (REDERR "EXTENDEDGROEBFACTOR only for ideal bases"))
     ((NULL *NOETHERIAN)
      (REDERR "EXTENDEDGROEBFACTOR only for noetherian term orders"))
     ((DPMAT_ZERO? BAS)
      (LIST (LIST (DPMAT_2A BAS) NIL (RING_NAMES CALI=BASERING))))
     (T
      (PROG (GBS)
        (COND
         ((GREATERP (CALI_TRACE) 5)
          (PROGN
           (PROGN (PRIN2 "EXTENDEDGROEBFACTOR the system ") NIL)
           (DPMAT_PRINT BAS))))
        (SETQ GBS
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (GROEBF=PREPROCESS NIL (LIST (LIST BAS POLY))))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (RETURN (GROEBF=EXTENDEDMASTERPROCESS GBS)))))) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS 'DEFINED-ON-LINE '167) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=EXTENDEDMASTERPROCESS (GBS)
    (PROG (RES RES1 U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (OR GBS RES)) (RETURN NIL)))
        (COND
         (GBS
          (PROGN
           (SETQ GBS (SORT GBS (FUNCTION GROEBF=PROBLEMSORT)))
           (SETQ RES
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (GROEBF=MASTERPROCESS GBS RES))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (COND ((EQUAL (LENGTH X) 3) X)
                                               (T
                                                (LIST (FIRST X) (SECOND X)
                                                      (DIM* (FIRST X))))))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (COND ((EQUAL (LENGTH X) 3) X)
                                       (T
                                        (LIST (FIRST X) (SECOND X)
                                              (DIM* (FIRST X))))))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ RES (SORT RES (FUNCTION GROEBF=RESULTSORT)))
           (SETQ GBS NIL)))
         (T
          (PROGN
           (SETQ U (GROEBF=POSTPROCESS2 (CAR RES)))
           (SETQ RES (CDR RES))
           (SETQ GBS
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (GROEBF=PREPROCESS RES (SECOND U)))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ RES1 (NCONC (FIRST U) RES1))
           NIL)))
        (GO WHILELABEL))
      (RETURN RES1))) 
(PUT 'EXTENDEDGROEBFACTOR1 'PSOPFN 'INTF=EXTENDEDGROEBFACTOR1) 
(PUT 'INTF=EXTENDEDGROEBFACTOR1 'NUMBER-OF-ARGS 1) 
(PUT 'INTF=EXTENDEDGROEBFACTOR1 'DEFINED-ON-LINE '200) 
(PUT 'INTF=EXTENDEDGROEBFACTOR1 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'INTF=EXTENDEDGROEBFACTOR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTF=EXTENDEDGROEBFACTOR1 (M)
    (PROG (BAS CON)
      (SETQ BAS (DPMAT_FROM_A (REVAL1 (FIRST M) T)))
      (COND ((EQUAL (LENGTH M) 1) (SETQ CON NIL))
            ((EQUAL (LENGTH M) 2)
             (SETQ CON
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CDR (REVAL1 (SECOND M) T)))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (DP_FROM_A X)) (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (DP_FROM_A X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T
             (REDERR
              "Syntax : EXTENDEDGROEBFACTOR1(base list [,constraint list])")))
      (RETURN
       (CONS 'LIST
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (EXTENDEDGROEBFACTOR1* BAS CON))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (CONS 'LIST
                                         (LIST (FIRST X)
                                               (CONS 'LIST (SECOND X))
                                               (CONS 'LIST (THIRD X)))))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (CONS 'LIST
                                 (LIST (FIRST X) (CONS 'LIST (SECOND X))
                                       (CONS 'LIST (THIRD X)))))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'EXTENDEDGROEBFACTOR1* 'NUMBER-OF-ARGS 2) 
(PUT 'EXTENDEDGROEBFACTOR1* 'DEFINED-ON-LINE '213) 
(PUT 'EXTENDEDGROEBFACTOR1* 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'EXTENDEDGROEBFACTOR1* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXTENDEDGROEBFACTOR1* (BAS POLY)
    (COND
     ((GREATERP (DPMAT_COLS BAS) 0)
      (REDERR "EXTENDEDGROEBFACTOR1 only for ideal bases"))
     ((NULL *NOETHERIAN)
      (REDERR "EXTENDEDGROEBFACTOR1 only for noetherian term orders"))
     ((DPMAT_ZERO? BAS)
      (LIST (LIST (DPMAT_2A BAS) NIL (RING_NAMES CALI=BASERING))))
     (T
      (PROG (GBS)
        (COND
         ((GREATERP (CALI_TRACE) 5)
          (PROGN
           (PROGN (PRIN2 "EXTENDEDGROEBFACTOR1 the system ") NIL)
           (DPMAT_PRINT BAS))))
        (SETQ GBS
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (GROEBF=PREPROCESS NIL (LIST (LIST BAS POLY))))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (RETURN
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X (GROEBF=EXTENDEDMASTERPROCESS1 GBS))
           (COND ((NULL X) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (NTH X 4)) (CAR X)) NIL)))
          LOOPLABEL
           (SETQ X (CDR X))
           (COND ((NULL X) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (NTH X 4)) (CAR X)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS1 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS1 'DEFINED-ON-LINE '233) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS1 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=EXTENDEDMASTERPROCESS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=EXTENDEDMASTERPROCESS1 (GBS)
    (PROG (RES U V P)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (OR GBS
               (SETQ P
                       (LISTTEST RES NIL
                                 (FUNCTION
                                  (LAMBDA (X Y) (LESSP (LENGTH X) 4)))))))
          (RETURN NIL)))
        (COND
         (GBS
          (PROGN
           (SETQ GBS (SORT GBS (FUNCTION GROEBF=PROBLEMSORT)))
           (SETQ RES
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (GROEBF=MASTERPROCESS GBS RES))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (COND ((GREATERP (LENGTH X) 2) X)
                                               (T
                                                (LIST (FIRST X) (SECOND X)
                                                      (DIM* (FIRST X))))))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (COND ((GREATERP (LENGTH X) 2) X)
                                       (T
                                        (LIST (FIRST X) (SECOND X)
                                              (DIM* (FIRST X))))))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ RES (SORT RES (FUNCTION GROEBF=RESULTSORT)))
           (SETQ GBS NIL)))
         (T
          (PROGN
           (SETQ U (GROEBF=POSTPROCESS2 P))
           (SETQ RES (DELETE P RES))
           (SETQ V
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (FIRST U))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (LIST (GROEBF=POSTPROCESS3 X) NIL
                                               (LENGTH (THIRD X)) X))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (LIST (GROEBF=POSTPROCESS3 X) NIL
                                       (LENGTH (THIRD X)) X))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (PROG (Y)
             (SETQ Y V)
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y)
                (COND
                 ((NOT (GROEBF=REDTEST RES Y))
                  (SETQ RES
                          (MERGE (LIST Y) (GROEBF=SIEVE RES Y)
                                 (FUNCTION GROEBF=RESULTSORT))))))
              (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB))
           (SETQ GBS
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (GROEBF=PREPROCESS RES (SECOND U)))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           NIL)))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'GROEBF=MASTERPROCESS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=MASTERPROCESS 'DEFINED-ON-LINE '274) 
(PUT 'GROEBF=MASTERPROCESS 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=MASTERPROCESS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=MASTERPROCESS (GBS RES)
    (PROG (U V)
      (PROG ()
       WHILELABEL
        (COND ((NOT GBS) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (CALI_TRACE) 10)
           (PRINT
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X GBS)
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (NTH X 4)) (CAR X)) NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (NTH X 4)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))
         (SETQ U (GROEBF=SLAVE (CAR GBS)))
         (SETQ GBS (CDR GBS))
         (COND
          (U
           (PROGN
            (SETQ V
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (SECOND U))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (GROEBF=POSTPROCESS1 RES X))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X) (GROEBF=POSTPROCESS1 RES X))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ U
                    (NCONC (CAR U)
                           (PROG (X FORALL-RESULT FORALL-ENDPTR)
                             (SETQ X V)
                            STARTOVER
                             (COND ((NULL X) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     ((LAMBDA (X) (CAR X)) (CAR X)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                             (SETQ X (CDR X))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND ((NULL X) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     ((LAMBDA (X) (CAR X)) (CAR X)))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ X (CDR X))
                             (GO LOOPLABEL))))
            (SETQ V
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X V)
                     STARTOVER
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT ((LAMBDA (X) (SECOND X)) (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ X (CDR X))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR ((LAMBDA (X) (SECOND X)) (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ X (CDR X))
                      (GO LOOPLABEL)))
            (PROG (Y)
              (SETQ Y V)
             LAB
              (COND ((NULL Y) (RETURN NIL)))
              ((LAMBDA (Y)
                 (COND
                  ((GREATERP (CALI_TRACE) 5)
                   (PROGN
                    (PROGN (PRIN2 "partial result :") NIL)
                    (TERPRI)
                    (DPMAT_PRINT (CAR Y))
                    (PRIN2 "constraints : ")
                    (PROG (X)
                      (SETQ X (SECOND Y))
                     LAB
                      (COND ((NULL X) (RETURN NIL)))
                      ((LAMBDA (X) (DP_PRINT2 X)) (CAR X))
                      (SETQ X (CDR X))
                      (GO LAB))
                    NIL))))
               (CAR Y))
              (SETQ Y (CDR Y))
              (GO LAB))
            (PROG (Y)
              (SETQ Y V)
             LAB
              (COND ((NULL Y) (RETURN NIL)))
              ((LAMBDA (Y)
                 (COND
                  ((NOT (GROEBF=REDTEST RES Y))
                   (SETQ RES (CONS Y (GROEBF=SIEVE RES Y))))))
               (CAR Y))
              (SETQ Y (CDR Y))
              (GO LAB))
            (PROG (X)
              (SETQ X U)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (COND
                  ((NOT (GROEBF=REDTEST RES X))
                   (SETQ GBS
                           (MERGE (LIST X) (GROEBF=SIEVE GBS X)
                                  (FUNCTION GROEBF=PROBLEMSORT))))))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (COND
             ((GREATERP (CALI_TRACE) 20)
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 (LENGTH GBS))
                (PRIN2 " remaining branches. ")
                (PRIN2 (LENGTH RES))
                (PRIN2 " partial results")
                NIL)
               (TERPRI))))
            NIL))
          ((GREATERP (CALI_TRACE) 20) (PRINT "Branch discarded")))
         NIL)
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'GROEBF=INITPROBLEM 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=INITPROBLEM 'DEFINED-ON-LINE '316) 
(PUT 'GROEBF=INITPROBLEM 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=INITPROBLEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=INITPROBLEM (X)
    (LIST (CAR X) (SECOND X) (GROEB_MAKEPAIRLIST (DPMAT_LIST (CAR X)) T)
          (EASYDIM* (CAR X)))) 
(PUT 'GROEBF=REDTEST 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=REDTEST 'DEFINED-ON-LINE '324) 
(PUT 'GROEBF=REDTEST 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=REDTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=REDTEST (A C)
    (PROG (U)
      (SETQ U
              (LISTTEST A C
                        (FUNCTION
                         (LAMBDA (X Y) (SUBMODULEP* (CAR X) (CAR Y))))))
      (COND
       (U (SETCDR U (CONS (INTERSECTION (SECOND U) (SECOND C)) (CDDR U)))))
      (RETURN U))) 
(PUT 'GROEBF=SIEVE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=SIEVE 'DEFINED-ON-LINE '333) 
(PUT 'GROEBF=SIEVE 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=SIEVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=SIEVE (A C)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X A)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X)
                 (COND ((NOT (SUBMODULEP* (CAR C) (CAR X))) (LIST X))
                       (T
                        (PROGN
                         (SETCDR C
                                 (CONS (INTERSECTION (SECOND X) (SECOND C))
                                       (CDDR C)))
                         NIL))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X)
                 (COND ((NOT (SUBMODULEP* (CAR C) (CAR X))) (LIST X))
                       (T
                        (PROGN
                         (SETCDR C
                                 (CONS (INTERSECTION (SECOND X) (SECOND C))
                                       (CDDR C)))
                         NIL))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'GROEBF=TEST 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=TEST 'DEFINED-ON-LINE '339) 
(PUT 'GROEBF=TEST 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=TEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=TEST (CON M)
    (COND ((NULL M) T) ((DP_UNIT? (BAS_DPOLY (FIRST M))) NIL) ((NULL CON) T)
          (T
           (PROG (P)
             (SETQ P T)
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P CON)) (RETURN NIL)))
               (PROGN
                (SETQ P
                        (AND P
                             (BAS_DPOLY
                              (CAR (RED_REDPOL M (BAS_MAKE 0 (CAR CON)))))))
                (SETQ CON (CDR CON)))
               (GO WHILELABEL))
             (RETURN P))))) 
(PUT 'GROEBF=NEWCON 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=NEWCON 'DEFINED-ON-LINE '352) 
(PUT 'GROEBF=NEWCON 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=NEWCON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=NEWCON (R D)
    (PROG (M C U)
      (SETQ M (FIRST R))
      (SETQ C (SECOND R))
      (RETURN
       (PROG (P FORALL-RESULT FORALL-ENDPTR)
         (SETQ P D)
        STARTOVER
         (COND ((NULL P) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (P)
                    (COND
                     ((NOT (MEMBER P C))
                      (PROGN
                       (SETQ U
                               (LIST (MATSUM* (LIST M (DPMAT_FROM_DPOLY P)))
                                     C))
                       (SETQ C (CONS P C))
                       (LIST U)))))
                  (CAR P)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ P (CDR P))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL P) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (P)
                    (COND
                     ((NOT (MEMBER P C))
                      (PROGN
                       (SETQ U
                               (LIST (MATSUM* (LIST M (DPMAT_FROM_DPOLY P)))
                                     C))
                       (SETQ C (CONS P C))
                       (LIST U)))))
                  (CAR P)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ P (CDR P))
         (GO LOOPLABEL))))) 
(PUT 'GROEBF=PREPROCESS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=PREPROCESS 'DEFINED-ON-LINE '362) 
(PUT 'GROEBF=PREPROCESS 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=PREPROCESS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=PREPROCESS (A1 B)
    (PROG (A C D BACK U)
      (COND ((GREATERP (CALI_TRACE) 20) (PRIN2 "preprocessing started")))
      (PROG ()
       WHILELABEL
        (COND ((NOT B) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (CALI_TRACE) 20)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 (LENGTH A)) (PRIN2 " ready. ") NIL)
            (PROGN (PRIN2 (LENGTH B)) (PRIN2 " left.") NIL)
            (TERPRI))))
         (SETQ C (CAR B))
         (SETQ B (CDR B))
         (COND
          ((NOT
            (OR (NULL (GROEBF=TEST (SECOND C) (DPMAT_LIST (CAR C))))
                (GROEBF=REDTEST A1 C) (GROEBF=REDTEST A C)))
           (PROGN
            (SETQ D (DPMAT_LIST (CAR C)))
            (SETQ BACK NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND D (NOT BACK))) (RETURN NIL)))
              (PROGN
               (SETQ U
                       ((LAMBDA (*FACTOR)
                          (FCTRF (CAR (SIMP (DP_2A (BAS_DPOLY (CAR D)))))))
                        T))
               (COND
                ((OR (GREATERP (LENGTH U) 2) (GREATERP (CDADR U) 1))
                 (PROGN
                  (SETQ BACK T)
                  (SETQ B
                          (APPEND
                           (GROEBF=NEWCON C
                            (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Y (CDR U))
                              (COND ((NULL Y) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y)
                                                  (DP_FROM_A (PREPF (CAR Y))))
                                                (CAR Y))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Y (CDR Y))
                              (COND ((NULL Y) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (DP_FROM_A (PREPF (CAR Y))))
                                        (CAR Y))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                           B))
                  NIL))
                (T (SETQ D (CDR D)))))
              (GO WHILELABEL))
            (COND
             ((NOT BACK)
              (PROGN
               (COND
                ((GREATERP (CALI_TRACE) 20)
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "Subproblem :") NIL)
                  (DPMAT_PRINT (CAR C)))))
               (COND
                ((NOT (GROEBF=REDTEST A C))
                 (SETQ A (CONS C (GROEBF=SIEVE A C)))))
               NIL)))))))
        (GO WHILELABEL))
      (COND ((GREATERP (CALI_TRACE) 20) (PRIN2 "preprocessing finished...")))
      (RETURN A))) 
(PUT 'GROEBF=SLAVE 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=SLAVE 'DEFINED-ON-LINE '401) 
(PUT 'GROEBF=SLAVE 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=SLAVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=SLAVE (C)
    (PROG (BE BACK P U V A B GB PL NR POL CON)
      (SETQ BACK NIL)
      (SETQ GB (BAS_SORT (DPMAT_LIST (FIRST C))))
      (SETQ CON (SECOND C))
      (SETQ PL (THIRD C))
      (SETQ NR (LENGTH GB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PL (NOT BACK))) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PL))
         (SETQ PL (CDR PL))
         (COND ((GREATERP (CALI_TRACE) 10) (GROEB_PRINTPAIR P PL)))
         (SETQ POL (GROEB_SPOL P))
         (COND
          ((GREATERP (CALI_TRACE) 70)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "S.-pol : ") NIL)
            (DP_PRINT2 (BAS_DPOLY POL)))))
         (SETQ POL (BAS_DPOLY (CAR (RED_REDPOL GB POL))))
         (COND
          ((GREATERP (CALI_TRACE) 70)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "Reduced S.-pol. : ") NIL)
            (DP_PRINT2 POL))))
         (COND
          (POL
           (PROGN
            (COND (*BCSIMP (SETQ POL (CAR (DP_SIMP POL)))))
            (COND
             ((DP_UNIT? POL)
              (PROGN
               (COND ((GREATERP (CALI_TRACE) 20) (PRINT "unit ideal")))
               (SETQ BACK T)))
             (T
              (PROGN
               (SETQ U ((LAMBDA (*FACTOR) (FCTRF (CAR (SIMP (DP_2A POL))))) T))
               (SETQ NR (PLUS NR 1))
               (COND
                ((EQUAL (LENGTH (CDR U)) 1)
                 (PROGN
                  (SETQ POL (DP_FROM_A (PREPF (CAADR U))))
                  (SETQ BE (BAS_MAKE NR POL))
                  (SETQ U (CONS BE GB))
                  (COND
                   ((NULL (GROEBF=TEST CON U))
                    (PROGN
                     (SETQ BACK T)
                     (COND
                      ((GREATERP (CALI_TRACE) 20) (PRINT " zero constraint")))
                     NIL))
                   (T
                    (PROGN
                     (COND
                      ((GREATERP (CALI_TRACE) 20)
                       (PROGN
                        (TERPRI)
                        (PROGN (PRIN2 NR) (PRIN2 ". ") NIL)
                        (DP_PRINT2 POL))))
                     (SETQ PL (GROEB_UPDATEPL PL GB BE T))
                     (COND
                      ((GREATERP (CALI_TRACE) 30)
                       (PROGN (TERPRI) (GROEB_PRINTPAIRLIST PL))))
                     (SETQ GB (MERGE (LIST BE) GB (FUNCTION RED_BETTER)))
                     NIL)))))
                (T
                 (PROGN
                  (PROG (X)
                    (SETQ X (CDR U))
                   LAB
                    (COND ((NULL X) (RETURN NIL)))
                    ((LAMBDA (X)
                       (PROGN
                        (SETQ POL (DP_FROM_A (PREPF (CAR X))))
                        (SETQ BE (BAS_MAKE NR POL))
                        (SETQ A (CONS BE GB))
                        (COND
                         ((GROEBF=TEST CON A)
                          (PROGN
                           (COND
                            ((GREATERP (CALI_TRACE) 20)
                             (PROGN
                              (TERPRI)
                              (PROGN (PRIN2 NR) NIL)
                              (PROGN (PRIN2 ". ") NIL)
                              (DP_PRINT2 POL))))
                           (SETQ P (GROEB_UPDATEPL (APPEND PL NIL) GB BE T))
                           (COND
                            ((GREATERP (CALI_TRACE) 30)
                             (PROGN (TERPRI) (GROEB_PRINTPAIRLIST P))))
                           (SETQ B
                                   (MERGE (LIST BE) (APPEND GB NIL)
                                          (FUNCTION RED_BETTER)))
                           (SETQ B (DPMAT_MAKE (LENGTH B) 0 B NIL NIL))
                           (SETQ V (CONS (LIST B CON P) V))
                           NIL))
                         ((GREATERP (CALI_TRACE) 20)
                          (PRINT " zero constraint")))
                        (COND
                         ((NOT (MEMBER POL CON)) (SETQ CON (CONS POL CON))))
                        NIL))
                     (CAR X))
                    (SETQ X (CDR X))
                    (GO LAB))
                  (COND
                   ((NULL V)
                    (PROGN
                     (COND
                      ((GREATERP (CALI_TRACE) 20) (PRINT "Branch canceled")))
                     (SETQ BACK T)))
                   ((EQUAL (LENGTH V) 1)
                    (PROGN
                     (SETQ C (CAR V))
                     (SETQ GB (DPMAT_LIST (FIRST C)))
                     (SETQ CON (SECOND C))
                     (SETQ PL (THIRD C))
                     (SETQ V NIL)
                     NIL))
                   (T
                    (PROGN
                     (SETQ BACK T)
                     (COND
                      ((GREATERP (CALI_TRACE) 20)
                       (PROGN
                        (PROGN
                         (PRIN2 " Branching into ")
                         (PRIN2 (LENGTH V))
                         (PRIN2 " parts ")
                         NIL)
                        (TERPRI)
                        NIL)))
                     NIL)))
                  NIL)))
               NIL)))
            NIL)))
         NIL)
        (GO WHILELABEL))
      (COND
       ((NOT BACK)
        (RETURN
         (LIST NIL
               (LIST
                (LIST (GROEB_MINGB (DPMAT_MAKE (LENGTH GB) 0 GB NIL T))
                      CON)))))
       (V
        (RETURN
         (LIST
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X V)
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (LIST (FIRST X) (SECOND X) (THIRD X)
                                      (EASYDIM* (FIRST X))))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X)
                        (LIST (FIRST X) (SECOND X) (THIRD X)
                              (EASYDIM* (FIRST X))))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          NIL)))
       (T (RETURN NIL))))) 
(PUT 'GROEBF=POSTPROCESS1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=POSTPROCESS1 'DEFINED-ON-LINE '494) 
(PUT 'GROEBF=POSTPROCESS1 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=POSTPROCESS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=POSTPROCESS1 (RES X)
    (PROG (P R V)
      (COND
       (*RED_TOTAL
        (PROGN
         (SETQ V
                 (GROEBF=PREPROCESS RES
                  (LIST
                   (LIST
                    (DPMAT_MAKE (DPMAT_ROWS (CAR X)) 0
                     (RED_STRAIGHT (DPMAT_LIST (CAR X))) NIL
                     (DPMAT_GBTAG (CAR X)))
                    (SECOND X)))))
         (COND ((AND (EQUAL (LENGTH V) 1) (DPMAT_GBTAG (CAAR V))) (SETQ R V))
               (T
                (SETQ P
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X V)
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))))
         NIL))
       (T (SETQ R (LIST X))))
      (RETURN (LIST P R)))) 
(PUT 'GROEBF=POSTPROCESS2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=POSTPROCESS2 'DEFINED-ON-LINE '515) 
(PUT 'GROEBF=POSTPROCESS2 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=POSTPROCESS2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=POSTPROCESS2 (M)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (D VARS U V C1 M1 M1A M2 P CON)
         (SETQ CON (SECOND M))
         (SETQ D (THIRD M))
         (SETQ M (FIRST M))
         (SETQ V (MOID_GOODINDEPVARSET M))
         (COND
          ((NEQ (LENGTH V) D)
           (REDERR "In POSTPROCESS2 the dimension is wrong")))
         (COND
          ((NULL V)
           (RETURN
            (LIST
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (GROEBF=ZEROSOLVE M CON))
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (LIST X NIL NIL)) (CAR X))
                                     NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LIST X NIL NIL)) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             NIL))))
         (SETQ VARS (RING_NAMES (SETQ C1 CALI=BASERING)))
         (SETQ U (SETDIFF VARS V))
         (COND ((EQUAL (GET 'CALI 'EFGB) 'LEX) (SETRING* (RING_LP C1 U)))
               (T (SETRING* (RING_RLP C1 U))))
         (SETQ M1
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U
                           (GROEBFACTOR* (DPMAT_NEWORDER M NIL)
                            (PROG (X FORALL-RESULT FORALL-ENDPTR)
                              (SETQ X CON)
                              (COND ((NULL X) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (X) (DP_NEWORDER X))
                                                (CAR X))
                                               NIL)))
                             LOOPLABEL
                              (SETQ X (CDR X))
                              (COND ((NULL X) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X) (DP_NEWORDER X)) (CAR X))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U)
                                       (LIST (FIRST U) (SECOND U)
                                             (DIM* (FIRST U))))
                                     (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (U)
                               (LIST (FIRST U) (SECOND U) (DIM* (FIRST U))))
                             (CAR U))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (X)
           (SETQ X M1)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (COND
               ((AND (EQUAL (THIRD X) D) (MEMBER V (INDEPVARSETS* (CAR X))))
                (SETQ M1A (CONS X M1A)))
               (T (SETQ M2 (CONS X M2)))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (SETQ M1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1A)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST (DPMAT_2A (FIRST X))
                                             (PROG (P FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ P (SECOND X))
                                               (COND ((NULL P) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (P)
                                                                   (DP_2A P))
                                                                 (CAR P))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ P (CDR P))
                                               (COND
                                                ((NULL P)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P) (DP_2A P))
                                                         (CAR P))
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
                               (LIST (DPMAT_2A (FIRST X))
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P (SECOND X))
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P) (DP_2A P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P) (DP_2A P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((EQUAL (GET 'CALI 'EFGB) 'LEX)
           (SETRING*
            (RING_DEFINE U NIL 'LEX
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X U)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
          (T
           (SETRING*
            (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X U)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))
         (SETQ M1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST
                                        (GROEB_MINGB (DPMAT_FROM_A (FIRST X)))
                                        (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ P (SECOND X))
                                          (COND ((NULL P) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (P)
                                                              (DP_FROM_A P))
                                                            (CAR P))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ P (CDR P))
                                          (COND
                                           ((NULL P) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P) (DP_FROM_A P))
                                                    (CAR P))
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
                               (LIST (GROEB_MINGB (DPMAT_FROM_A (FIRST X)))
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P (SECOND X))
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P)
                                                           (DP_FROM_A P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P) (DP_FROM_A P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ M1A
                 (PAIR M1A
                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                         (SETQ X M1)
                         (COND ((NULL X) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X)
                                             (GROEBF=ELCBE (FIRST X)))
                                           (CAR X))
                                          NIL)))
                        LOOPLABEL
                         (SETQ X (CDR X))
                         (COND ((NULL X) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X) (GROEBF=ELCBE (FIRST X)))
                                   (CAR X))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ M1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X)
                              (GROEBF=ZEROSOLVE (FIRST X) (SECOND X)))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X)
                              (GROEBF=ZEROSOLVE (FIRST X) (SECOND X)))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (SETQ M1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST X
                                             (GROEBF=ELCBE (DPMAT_FROM_A X))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (LIST X (GROEBF=ELCBE (DPMAT_FROM_A X))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETRING* C1)
         (SETQ M1
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (LIST (FIRST X) (SECOND X) V))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (LIST (FIRST X) (SECOND X) V))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ M2
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M2)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST (DPMAT_NEWORDER (FIRST X) NIL)
                                             (PROG (Y FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ Y (SECOND X))
                                               (COND ((NULL Y) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (Y)
                                                                   (DP_NEWORDER
                                                                    Y))
                                                                 (CAR Y))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ Y (CDR Y))
                                               (COND
                                                ((NULL Y)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (Y)
                                                           (DP_NEWORDER Y))
                                                         (CAR Y))
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
                               (LIST (DPMAT_NEWORDER (FIRST X) NIL)
                                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ Y (SECOND X))
                                       (COND ((NULL Y) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (Y)
                                                           (DP_NEWORDER Y))
                                                         (CAR Y))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ Y (CDR Y))
                                       (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (Y) (DP_NEWORDER Y))
                                                 (CAR Y))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ M1A
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X M1A)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X)
                              (GROEBF=NEWCON
                               (LIST (DPMAT_NEWORDER (FIRST (CAR X)) NIL)
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P (SECOND (CAR X)))
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P)
                                                           (DP_NEWORDER P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P) (DP_NEWORDER P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ P (CDR X))
                                 (COND ((NULL P) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (P) (DP_FROM_A P))
                                                   (CAR P))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ P (CDR P))
                                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (P) (DP_FROM_A P)) (CAR P))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X)
                              (GROEBF=NEWCON
                               (LIST (DPMAT_NEWORDER (FIRST (CAR X)) NIL)
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P (SECOND (CAR X)))
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P)
                                                           (DP_NEWORDER P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P) (DP_NEWORDER P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ P (CDR X))
                                 (COND ((NULL P) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (P) (DP_FROM_A P))
                                                   (CAR P))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ P (CDR P))
                                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (P) (DP_FROM_A P)) (CAR P))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (RETURN (LIST M1 (NCONC M1A M2)))))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'GROEBF=ELCBE 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=ELCBE 'DEFINED-ON-LINE '604) 
(PUT 'GROEBF=ELCBE 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=ELCBE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=ELCBE (M)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (DPMAT_LIST M))
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X)
                 (COND
                  (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                    (DP_LC (BAS_DPOLY X)))
                   (LIST))
                  (T (LIST (CALI_BC_2A (DP_LC (BAS_DPOLY X)))))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X)
                 (COND
                  (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                    (DP_LC (BAS_DPOLY X)))
                   (LIST))
                  (T (LIST (CALI_BC_2A (DP_LC (BAS_DPOLY X)))))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'GROEBF=POSTPROCESS3 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=POSTPROCESS3 'DEFINED-ON-LINE '611) 
(PUT 'GROEBF=POSTPROCESS3 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=POSTPROCESS3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=POSTPROCESS3 (U)
    (MATQQUOT* (DPMAT_FROM_A (FIRST U))
     (GROEBF=PROD
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (SECOND U))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (DP_FROM_A X)) (CAR X)) NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (DP_FROM_A X)) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'GROEBF=PROD 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBF=PROD 'DEFINED-ON-LINE '617) 
(PUT 'GROEBF=PROD 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=PROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBF=PROD (L)
    (PROG (P)
      (SETQ P (DP_FI 1))
      (SETQ L
              (LISTMINIMIZE
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X L)
                STARTOVER
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT ((LAMBDA (X) (DP_FACTOR X)) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ X (CDR X))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR ((LAMBDA (X) (DP_FACTOR X)) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ X (CDR X))
                 (GO LOOPLABEL))
               (FUNCTION EQUAL)))
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ P (DP_PROD X P))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN P))) 
(PUT 'GROEBF=ZEROSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF=ZEROSOLVE 'DEFINED-ON-LINE '624) 
(PUT 'GROEBF=ZEROSOLVE 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF=ZEROSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF=ZEROSOLVE (M CON)
    (PROG (U)
      (SETQ CON
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X CON)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((NOT (DP_UNIT? X)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((NOT (DP_UNIT? X)) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (SETQ U (GROEBF_ZEROPRIMES1 M CON))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X U)
        STARTOVER
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (X)
                    (COND ((EQUAL (GET 'CALI 'EFGB) 'LEX) (ZEROSOLVE* X))
                          (T (ZEROSOLVE1* X))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ X (CDR X))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (X)
                    (COND ((EQUAL (GET 'CALI 'EFGB) 'LEX) (ZEROSOLVE* X))
                          (T (ZEROSOLVE1* X))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ X (CDR X))
         (GO LOOPLABEL))))) 
(PUT 'GROEBF_ZEROPRIMES1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBF_ZEROPRIMES1 'DEFINED-ON-LINE '640) 
(PUT 'GROEBF_ZEROPRIMES1 'DEFINED-IN-FILE 'CALI/GROEBF.RED) 
(PUT 'GROEBF_ZEROPRIMES1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBF_ZEROPRIMES1 (M CON)
    (PROG (M1 M2 P U L)
      (SETQ L (LIST (LIST M CON)))
      (PROG (X)
        (SETQ X (RING_NAMES CALI=BASERING))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ M1 (SETQ M2 NIL))
            (PROG (Y)
              (SETQ Y L)
             LAB
              (COND ((NULL Y) (RETURN NIL)))
              ((LAMBDA (Y)
                 (COND
                  ((NOT
                    (MEMBER X
                            (PROG (V FORALL-RESULT FORALL-ENDPTR)
                              (SETQ V (DPMAT_LIST (FIRST Y)))
                             STARTOVER
                              (COND ((NULL V) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (V)
                                         (LIST
                                          (MO_LINEAR (DP_LMON (BAS_DPOLY V)))))
                                       (CAR V)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ V (CDR V))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL V) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (V)
                                         (LIST
                                          (MO_LINEAR (DP_LMON (BAS_DPOLY V)))))
                                       (CAR V)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ V (CDR V))
                              (GO LOOPLABEL))))
                   (PROGN
                    (SETQ P (ODIM_UP X (FIRST Y)))
                    (SETQ U (DP_FACTOR P))
                    (COND
                     ((OR (GREATERP (LENGTH U) 1) (NOT (EQUAL (FIRST U) P)))
                      (SETQ M1 (NCONC (GROEBF=NEWCON Y U) M1)))
                     (T (SETQ M2 (CONS Y M2))))
                    NIL))
                  (T (SETQ M2 (CONS Y M2)))))
               (CAR Y))
              (SETQ Y (CDR Y))
              (GO LAB))
            (SETQ L
                    (GROEBF=MASTERPROCESS
                     (SORT
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X M1)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X) (GROEBF=INITPROBLEM X))
                                          (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (GROEBF=INITPROBLEM X)) (CAR X))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      (FUNCTION GROEBF=PROBLEMSORT))
                     M2))
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X L)
        STARTOVER
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (X)
                    (COND
                     ((SECOND X)
                      (LIST (MATQQUOT* (FIRST X) (GROEBF=PROD (SECOND X)))))
                     (T (LIST (FIRST X)))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ X (CDR X))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (X)
                    (COND
                     ((SECOND X)
                      (LIST (MATQQUOT* (FIRST X) (GROEBF=PROD (SECOND X)))))
                     (T (LIST (FIRST X)))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ X (CDR X))
         (GO LOOPLABEL))))) 
(ENDMODULE) 