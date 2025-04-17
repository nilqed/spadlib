(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEB)) 
(PUT 'CALI 'GROEB=RF 'GROEB=RF1) 
(FLAG '(GBTESTVERSION) 'OPFN) 
(PUT 'GBTESTVERSION 'NUMBER-OF-ARGS 1) 
(PUT 'GBTESTVERSION 'DEFINED-ON-LINE '76) 
(PUT 'GBTESTVERSION 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GBTESTVERSION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBTESTVERSION (N)
    (COND ((MEMBER N (LIST 1 2 3)) (PUT 'CALI 'GROEB=RF (MKID 'GROEB=RF N))))) 
(PUT 'GROEB=POSTPROCESS 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=POSTPROCESS 'DEFINED-ON-LINE '80) 
(PUT 'GROEB=POSTPROCESS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=POSTPROCESS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=POSTPROCESS (POL)
    (PROG ()
      (COND (*BCSIMP (SETQ POL (CAR (BAS_SIMPELEMENT POL)))))
      (COND
       ((NOT *NOETHERIAN)
        (COND (*FACTORUNITS (SETQ POL (BAS_FACTORUNITS POL)))
              (*DETECTUNITS (SETQ POL (BAS_DETECTUNITS POL))))))
      (COND
       (CALI=MONSET
        (SETQ POL
                (BAS_MAKE (BAS_NR POL)
                 (CAR (DP_MONDELETE (BAS_DPOLY POL) CALI=MONSET))))))
      (RETURN POL))) 
(PUT 'GROEB_STBASIS 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB_STBASIS 'DEFINED-ON-LINE '93) 
(PUT 'GROEB_STBASIS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_STBASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB_STBASIS (BAS COMP_MGB COMP_CH COMP_SYZ)
    (GROEB=CHOOSE_DRIVER BAS COMP_MGB COMP_CH COMP_SYZ
     (FUNCTION GROEB=GENERALDRIVER))) 
(PUT 'GROEB=CHOOSE_DRIVER 'NUMBER-OF-ARGS 5) 
(PUT 'GROEB=CHOOSE_DRIVER 'DEFINED-ON-LINE '98) 
(PUT 'GROEB=CHOOSE_DRIVER 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=CHOOSE_DRIVER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB=CHOOSE_DRIVER (BAS COMP_MGB COMP_CH COMP_SYZ DRIVER)
    (COND
     ((DPMAT_ZERO? BAS)
      (LIST BAS (DPMAT_UNIT (DPMAT_ROWS BAS) NIL)
            (DPMAT_UNIT (DPMAT_ROWS BAS) NIL)))
     (T
      ((LAMBDA (CALI=DEGREES *FACTORUNITS *DETECTUNITS CALI=MONSET)
         (PROG (U GB SYZ CHANGE SYZ1)
           (COND
            (COMP_SYZ
             (PROGN
              (SETQ U
                      (SETDIFF
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND
                          ((MINUSP (DIFFERENCE (DPMAT_ROWS BAS) I))
                           (RETURN NIL)))
                         (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                        LOOPLABEL
                         (SETQ I (PLUS2 I 1))
                         (COND
                          ((MINUSP (DIFFERENCE (DPMAT_ROWS BAS) I))
                           (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR (CONS I NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                         (SETQ X (BAS_ZERODELETE (DPMAT_LIST BAS)))
                         (COND ((NULL X) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X) (BAS_NR X)) (CAR X))
                                          NIL)))
                        LOOPLABEL
                         (SETQ X (CDR X))
                         (COND ((NULL X) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (BAS_NR X)) (CAR X)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
              (SETQ SYZ1
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X U)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (BAS_MAKE 0 (DP_FROM_EI X)))
                                          (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (BAS_MAKE 0 (DP_FROM_EI X)))
                                  (CAR X))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              NIL)))
           (SETQ GB (BAS_ZERODELETE (DPMAT_LIST BAS)))
           (COND
            ((OR COMP_CH COMP_SYZ)
             (PROGN
              (SETQ *FACTORUNITS (SETQ *DETECTUNITS (SETQ CALI=MONSET NIL)))
              (BAS_SETRELATIONS GB)
              NIL)))
           (COND
            ((GREATERP (CALI_TRACE) 5)
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 " Compute GBasis of") NIL)
              (BAS_PRINT GB)))
            ((GREATERP (CALI_TRACE) 0)
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 " Computing GBasis  ") NIL)
              (TERPRI))))
           (SETQ U
                   (APPLY DRIVER
                          (LIST (DPMAT_ROWS BAS) (DPMAT_COLS BAS) GB
                                COMP_SYZ)))
           (SETQ SYZ (SECOND U))
           (COND
            (COMP_MGB
             (PROGN
              (SETQ U (GROEB_MINGB (CAR U)))
              (COND
               (*RED_TOTAL
                (SETQ U
                        (DPMAT_MAKE (DPMAT_ROWS U) (DPMAT_COLS U)
                         (RED_STRAIGHT (DPMAT_LIST U)) CALI=DEGREES T))))
              NIL))
            (T (SETQ U (CAR U))))
           (SETQ CALI=DEGREES (DPMAT_ROWDEGREES BAS))
           (COND
            (COMP_CH
             (SETQ CHANGE
                     (DPMAT_MAKE (DPMAT_ROWS U) (DPMAT_ROWS BAS)
                      (BAS_NEWORDER (BAS_GETRELATIONS (DPMAT_LIST U)))
                      CALI=DEGREES NIL))))
           (BAS_REMOVERELATIONS (DPMAT_LIST U))
           (COND
            (COMP_SYZ
             (PROGN
              (SETQ SYZ (NCONC SYZ SYZ1))
              (SETQ SYZ
                      (DPMAT_MAKE (LENGTH SYZ) (DPMAT_ROWS BAS)
                       (BAS_NEWORDER (BAS_RENUMBER SYZ)) CALI=DEGREES NIL))
              NIL)))
           (SETQ CALI=DEGREES (DPMAT_COLDEGS U))
           (RETURN (LIST U CHANGE SYZ))))
       (DPMAT_COLDEGS BAS) *FACTORUNITS *DETECTUNITS CALI=MONSET)))) 
(PUT 'GROEB=GENERALDRIVER 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB=GENERALDRIVER 'DEFINED-ON-LINE '188) 
(PUT 'GROEB=GENERALDRIVER 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=GENERALDRIVER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB=GENERALDRIVER (R C GB COMP_SYZ)
    (PROG (U Q SYZ P PL POL TRACE RETURN_BY_UNIT SIMP RF CCRIT)
      (SETQ CCRIT (AND (NOT COMP_SYZ) (LESSP C 2)))
      (SETQ SIMP
              (SORT (LISTMINIMIZE GB (FUNCTION RED=CANCELSIMP))
                    (FUNCTION RED_BETTER)))
      (SETQ PL (GROEB_MAKEPAIRLIST GB CCRIT))
      (SETQ RF (GET 'CALI 'GROEB=RF))
      (COND ((GREATERP (CALI_TRACE) 30) (GROEB_PRINTPAIRLIST PL)))
      (COND
       ((GREATERP (CALI_TRACE) 5)
        (PROGN (TERPRI) (PROGN (PRIN2 " New base elements :") NIL) (TERPRI))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PL (NOT RETURN_BY_UNIT))) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PL))
         (SETQ PL (CDR PL))
         (COND ((GREATERP (CALI_TRACE) 10) (GROEB_PRINTPAIR P PL)))
         (SETQ U (APPLY2 RF (GROEB_SPOL P) SIMP))
         (SETQ POL (FIRST U))
         (SETQ SIMP (SECOND U))
         (COND
          ((GREATERP (CALI_TRACE) 70)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 " Reduced S.-pol. : ") NIL)
            (DP_PRINT2 (BAS_DPOLY POL)))))
         (COND
          ((BAS_DPOLY POL)
           (PROGN
            (SETQ POL (GROEB=POSTPROCESS POL))
            (SETQ R (PLUS R 1))
            (SETQ POL (BAS_NEWNUMBER R POL))
            (SETQ Q (BAS_DPOLY POL))
            (SETQ TRACE
                    (CONS (LIST (GROEB=I P) (GROEB=J P) R (DP_LMON Q)) TRACE))
            (COND
             ((GREATERP (CALI_TRACE) 20)
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 R) (PRIN2 ". ---> ") NIL)
               (DP_PRINT2 Q))))
            (COND ((AND CCRIT (DP_UNIT? Q)) (SETQ RETURN_BY_UNIT T)))
            (COND
             ((NOT RETURN_BY_UNIT)
              (PROGN
               (SETQ PL (GROEB_UPDATEPL PL GB POL CCRIT))
               (COND
                ((GREATERP (CALI_TRACE) 30)
                 (PROGN (TERPRI) (GROEB_PRINTPAIRLIST PL))))
               (SETQ GB (CONS POL GB))
               (SETQ SIMP (RED_UPDATE SIMP POL))
               NIL)))
            NIL))
          (COMP_SYZ
           (SETQ SYZ
                   (CONS (CAR (BAS_SIMPELEMENT (BAS_MAKE 0 (BAS_REP POL))))
                         SYZ)))))
        (GO WHILELABEL))
      (COND
       ((GREATERP (CALI_TRACE) 0)
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 " Simplifier list has length ")
          (PRIN2 (LENGTH SIMP))
          NIL))))
      (COND
       (RETURN_BY_UNIT
        (RETURN (LIST (DPMAT_FROM_DPOLY POL) NIL (REVERSIP TRACE)))))
      (SETQ GB (DPMAT_MAKE (LENGTH GB) C GB CALI=DEGREES T))
      (RETURN (LIST GB SYZ (REVERSIP TRACE))))) 
(PUT 'GROEB=RF1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=RF1 'DEFINED-ON-LINE '255) 
(PUT 'GROEB=RF1 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=RF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=RF1 (POL SIMP) (LIST (RED_TOTALRED SIMP POL) SIMP)) 
(PUT 'GROEB=RF2 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=RF2 'DEFINED-ON-LINE '257) 
(PUT 'GROEB=RF2 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=RF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=RF2 (POL SIMP)
    (COND ((OR (NULL (BAS_DPOLY POL)) (NULL SIMP)) (LIST POL SIMP))
          (T
           (PROG (V Q)
             (SETQ POL (RED_TOPREDBE SIMP POL))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (SETQ Q (BAS_DPOLY POL))
                       (SETQ V (RED_DIVTEST SIMP (DP_LMON Q)))))
                 (RETURN NIL)))
               (PROGN
                (SETQ V (RED_SUBST POL V))
                (SETQ SIMP (RED_UPDATE SIMP POL))
                (SETQ POL (RED_TOPREDBE SIMP V))
                NIL)
               (GO WHILELABEL))
             (COND
              ((AND *RED_TOTAL (BAS_DPOLY POL))
               (SETQ POL (RED_TAILRED SIMP POL))))
             (RETURN (LIST POL SIMP)))))) 
(PUT 'GROEB=RF3 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=RF3 'DEFINED-ON-LINE '278) 
(PUT 'GROEB=RF3 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=RF3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=RF3 (POL SIMP)
    (COND ((OR (NULL (BAS_DPOLY POL)) (NULL SIMP)) (LIST POL SIMP))
          (T
           (PROG ()
             (SETQ POL (RED_TOPREDBE SIMP POL))
             (COND
              ((BAS_DPOLY POL)
               (SETQ POL
                       (RED_TAILREDDRIVER SIMP POL (FUNCTION RED_TOPREDBE)))))
             (RETURN (LIST POL SIMP)))))) 
(PUT 'GROEB_LAZYSTBASIS 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB_LAZYSTBASIS 'DEFINED-ON-LINE '299) 
(PUT 'GROEB_LAZYSTBASIS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_LAZYSTBASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB_LAZYSTBASIS (BAS COMP_MGB COMP_CH COMP_SYZ)
    (GROEB=CHOOSE_DRIVER BAS COMP_MGB COMP_CH COMP_SYZ
     (FUNCTION GROEB=LAZYDRIVER))) 
(PUT 'GROEB=LAZYMOCOMPARE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=LAZYMOCOMPARE 'DEFINED-ON-LINE '303) 
(PUT 'GROEB=LAZYMOCOMPARE 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=LAZYMOCOMPARE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=LAZYMOCOMPARE (A B) (LESSP (MO_ECART A) (MO_ECART B))) 
(PUT 'GROEB=QUEUESORT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=QUEUESORT 'DEFINED-ON-LINE '308) 
(PUT 'GROEB=QUEUESORT 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=QUEUESORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=QUEUESORT (A B)
    (GROEB=LAZYMOCOMPARE (DP_LMON (BAS_DPOLY A)) (DP_LMON (BAS_DPOLY B)))) 
(PUT 'GROEB=NEXTSPOL 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=NEXTSPOL 'DEFINED-ON-LINE '312) 
(PUT 'GROEB=NEXTSPOL 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=NEXTSPOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=NEXTSPOL (PL QUEUE)
    (COND ((NULL QUEUE) T) ((NULL PL) NIL)
          (T
           (GROEB=LAZYMOCOMPARE (NTH (CAR PL) 3)
            (DP_LMON (BAS_DPOLY (CAR QUEUE))))))) 
(PUT 'GROEB=LAZYDRIVER 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB=LAZYDRIVER 'DEFINED-ON-LINE '318) 
(PUT 'GROEB=LAZYDRIVER 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=LAZYDRIVER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB=LAZYDRIVER (R C GB COMP_SYZ)
    (PROG (SYZ CCRIT QUEUE V SIMP P PL POL RETURN_BY_UNIT)
      (SETQ SIMP
              (SORT (LISTMINIMIZE GB (FUNCTION RED=CANCELSIMP))
                    (FUNCTION RED_BETTER)))
      (SETQ CCRIT (AND (NOT COMP_SYZ) (LESSP C 2)))
      (SETQ PL (GROEB_MAKEPAIRLIST GB CCRIT))
      (COND ((GREATERP (CALI_TRACE) 30) (GROEB_PRINTPAIRLIST PL)))
      (COND
       ((GREATERP (CALI_TRACE) 5)
        (PROGN (TERPRI) (PROGN (PRIN2 " New base elements :") NIL) (TERPRI))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (OR PL QUEUE) (NOT RETURN_BY_UNIT))) (RETURN NIL)))
        (COND
         ((GROEB=NEXTSPOL PL QUEUE)
          (PROGN
           (SETQ P (CAR PL))
           (SETQ PL (CDR PL))
           (COND ((GREATERP (CALI_TRACE) 10) (GROEB_PRINTPAIR P PL)))
           (SETQ POL (GROEB_SPOL P))
           (COND
            ((BAS_DPOLY POL)
             (COND
              ((AND CCRIT (DP_UNIT? (BAS_DPOLY POL))) (SETQ RETURN_BY_UNIT T))
              (T
               (SETQ QUEUE
                       (MERGE (LIST POL) QUEUE (FUNCTION GROEB=QUEUESORT))))))
            (COMP_SYZ
             (SETQ SYZ
                     (CONS (BAS_SIMPELEMENT (BAS_MAKE 0 (BAS_REP POL))) SYZ))))
           NIL))
         (T
          (PROGN
           (SETQ POL (CAR QUEUE))
           (SETQ QUEUE (CDR QUEUE))
           (COND
            ((SETQ V
                     (RED_DIVTESTBE SIMP (DP_LMON (BAS_DPOLY POL))
                      (BAS_DPECART POL)))
             NIL)
            ((SETQ V (RED_DIVTEST SIMP (DP_LMON (BAS_DPOLY POL))))
             (SETQ SIMP (RED_UPDATE SIMP POL))))
           (COND
            (V
             (PROGN
              (SETQ POL (RED_SUBST POL V))
              (COND
               ((BAS_DPOLY POL)
                (SETQ QUEUE
                        (MERGE (LIST POL) QUEUE (FUNCTION GROEB=QUEUESORT))))
               (COMP_SYZ
                (SETQ SYZ
                        (CONS (BAS_SIMPELEMENT (BAS_MAKE 0 (BAS_REP POL)))
                              SYZ))))
              NIL))
            (T
             (PROGN
              (SETQ POL
                      (GROEB=POSTPROCESS
                       (COND
                        (*RED_TOTAL
                         (RED_TAILREDDRIVER GB POL (FUNCTION RED_TOPREDBE)))
                        (T POL))))
              (COND ((DP_UNIT? (BAS_DPOLY POL)) (SETQ RETURN_BY_UNIT T))
                    (T
                     (PROGN
                      (SETQ R (PLUS R 1))
                      (SETQ POL (BAS_NEWNUMBER R POL))
                      (COND
                       ((GREATERP (CALI_TRACE) 20)
                        (PROGN
                         (TERPRI)
                         (PROGN (PRIN2 R) (PRIN2 ". --> ") NIL)
                         (DP_PRINT2 (BAS_DPOLY POL)))))
                      (SETQ PL (GROEB_UPDATEPL PL GB POL CCRIT))
                      (SETQ SIMP (RED_UPDATE SIMP POL))
                      (SETQ GB (CONS POL GB))
                      NIL)))))))))
        (GO WHILELABEL))
      (COND
       ((GREATERP (CALI_TRACE) 0)
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 " Simplifier list has length ")
          (PRIN2 (LENGTH SIMP))
          NIL))))
      (COND (RETURN_BY_UNIT (RETURN (LIST (DPMAT_FROM_DPOLY POL) NIL NIL)))
            (T
             (RETURN
              (LIST (DPMAT_MAKE (LENGTH SIMP) C SIMP CALI=DEGREES T) SYZ
                    NIL)))))) 
(PUT 'GROEB=CRITA 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=CRITA 'DEFINED-ON-LINE '393) 
(PUT 'GROEB=CRITA 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=CRITA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=CRITA (P) (LISTMINIMIZE P (FUNCTION GROEB=TESTA))) 
(PUT 'GROEB=TESTA 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=TESTA 'DEFINED-ON-LINE '399) 
(PUT 'GROEB=TESTA 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=TESTA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=TESTA (P Q) (MO_DIVIDES? (NTH P 3) (NTH Q 3))) 
(PUT 'GROEB=CRITB 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=CRITB 'DEFINED-ON-LINE '401) 
(PUT 'GROEB=CRITB 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=CRITB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=CRITB (E P)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X) (COND ((NOT (GROEB=TESTB E X)) (LIST X)))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X) (COND ((NOT (GROEB=TESTB E X)) (LIST X)))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'GROEB=TESTB 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=TESTB 'DEFINED-ON-LINE '405) 
(PUT 'GROEB=TESTB 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=TESTB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=TESTB (E A)
    (AND (EQUAL (COND ((NULL (CAR E)) 0) (T (CAAR E))) (CAR A))
         (MO_DIVIDES? E (NTH A 3))
         (NOT (MO_EQUAL? (MO_LCM (DP_LMON (BAS_DPOLY (NTH A 5))) E) (NTH A 3)))
         (NOT
          (MO_EQUAL? (MO_LCM (DP_LMON (BAS_DPOLY (NTH A 4))) E) (NTH A 3))))) 
(PUT 'GROEB=CRITC 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=CRITC 'DEFINED-ON-LINE '415) 
(PUT 'GROEB=CRITC 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=CRITC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=CRITC (P)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X) (COND ((NOT (GROEB=TESTC1 X)) (LIST X)))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X) (COND ((NOT (GROEB=TESTC1 X)) (LIST X)))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'GROEB=TESTC1 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=TESTC1 'DEFINED-ON-LINE '419) 
(PUT 'GROEB=TESTC1 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=TESTC1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=TESTC1 (EL)
    (MO_EQUAL?
     (MO_SUM (DP_LMON (BAS_DPOLY (NTH EL 5))) (DP_LMON (BAS_DPOLY (NTH EL 4))))
     (NTH EL 3))) 
(PUT 'GROEB_UPDATEPL 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB_UPDATEPL 'DEFINED-ON-LINE '425) 
(PUT 'GROEB_UPDATEPL 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_UPDATEPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB_UPDATEPL (P GB BE CCRIT)
    (PROG (P1 K A N)
      (SETQ N (NEQ (BAS_NR BE) 0))
      (SETQ A (DP_LMON (BAS_DPOLY BE)))
      (SETQ K (COND ((NULL (CAR A)) 0) (T (CAAR A))))
      (PROG (B)
        (SETQ B GB)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (COND
            ((AND
              (EQUAL K
                     ((LAMBDA (V) (COND ((NULL (CAR V)) 0) (T (CAAR V))))
                      (DP_LMON (BAS_DPOLY B))))
              (OR N (NEQ (BAS_NR B) 0)))
             (SETQ P1 (CONS (GROEB=NEWPAIR K B BE) P1)))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (SETQ P1 (GROEB=CRITA (SORT P1 (FUNCTION GROEB=BETTER))))
      (COND (CCRIT (SETQ P1 (GROEB=CRITC P1))))
      (RETURN (MERGE P1 (GROEB=CRITB A P) (FUNCTION GROEB=BETTER))))) 
(PUT 'GROEB_MAKEPAIRLIST 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB_MAKEPAIRLIST 'DEFINED-ON-LINE '442) 
(PUT 'GROEB_MAKEPAIRLIST 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_MAKEPAIRLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB_MAKEPAIRLIST (GB CCRIT)
    (PROG (NEWGB P)
      (PROG ()
       WHILELABEL
        (COND ((NOT GB) (RETURN NIL)))
        (PROGN
         (SETQ P (GROEB_UPDATEPL P NEWGB (CAR GB) CCRIT))
         (SETQ NEWGB (CONS (CAR GB) NEWGB))
         (SETQ GB (CDR GB)))
        (GO WHILELABEL))
      (RETURN P))) 
(PUT 'GROEB=I 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=I 'DEFINED-ON-LINE '453) 
(PUT 'GROEB=I 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=I 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=I (P) (BAS_NR (NTH P 4))) 
(PUT 'GROEB=J 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB=J 'DEFINED-ON-LINE '455) 
(PUT 'GROEB=J 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=J 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB=J (P) (BAS_NR (NTH P 5))) 
(PUT 'GROEB=BETTER 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=BETTER 'DEFINED-ON-LINE '457) 
(PUT 'GROEB=BETTER 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=BETTER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=BETTER (A B)
    (COND ((LESSP (CADR A) (CADR B)) T)
          ((EQUAL (CADR A) (CADR B)) (LEQ (MO_COMPARE (NTH A 3) (NTH B 3)) 0))
          (T NIL))) 
(PUT 'GROEB=WEIGHT 'NUMBER-OF-ARGS 3) 
(PUT 'GROEB=WEIGHT 'DEFINED-ON-LINE '463) 
(PUT 'GROEB=WEIGHT 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=WEIGHT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB=WEIGHT (LCM P1 P2)
    (PLUS (MO_ECART LCM) (MIN2 (BAS_DPECART P1) (BAS_DPECART P2)))) 
(PUT 'GROEB=NEWPAIR 'NUMBER-OF-ARGS 3) 
(PUT 'GROEB=NEWPAIR 'DEFINED-ON-LINE '466) 
(PUT 'GROEB=NEWPAIR 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=NEWPAIR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB=NEWPAIR (K P1 P2)
    ((LAMBDA (LCM) (LIST K (GROEB=WEIGHT LCM P1 P2) LCM P1 P2))
     (MO_LCM (DP_LMON (BAS_DPOLY P1)) (DP_LMON (BAS_DPOLY P2))))) 
(PUT 'GROEB_PRINTPAIRLIST 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB_PRINTPAIRLIST 'DEFINED-ON-LINE '471) 
(PUT 'GROEB_PRINTPAIRLIST 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_PRINTPAIRLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB_PRINTPAIRLIST (P)
    (PROG ()
      (PROG (X)
        (SETQ X P)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (PROGN (PRIN2 (GROEB=I X)) (PRIN2 ".") (PRIN2 (GROEB=J X)) NIL)
            (PRINT_LF " | ")))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI))) 
(PUT 'GROEB_PRINTPAIR 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB_PRINTPAIR 'DEFINED-ON-LINE '478) 
(PUT 'GROEB_PRINTPAIR 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_PRINTPAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB_PRINTPAIR (PP P)
    (PROG ()
      (TERPRI)
      (PROGN
       (PRIN2 "Investigate (")
       (PRIN2 (GROEB=I PP))
       (PRIN2 ".")
       (PRIN2 (GROEB=J PP))
       (PRIN2 ")  ")
       (PRIN2 "Pair list has length ")
       (PRIN2 (LENGTH P))
       NIL)
      (TERPRI))) 
(PUT 'GROEB_SPOL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB_SPOL 'DEFINED-ON-LINE '486) 
(PUT 'GROEB_SPOL 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_SPOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB_SPOL (PP)
    (PROG (PI_ PJ RI RJ ZI ZJ LCM MI MJ A B)
      (SETQ A (NTH PP 4))
      (SETQ B (NTH PP 5))
      (SETQ LCM (NTH PP 3))
      (SETQ PI_ (BAS_DPOLY A))
      (SETQ PJ (BAS_DPOLY B))
      (SETQ RI (BAS_REP A))
      (SETQ RJ (BAS_REP B))
      (SETQ MI (MO_DIFF LCM (DP_LMON PI_)))
      (SETQ MJ (MO_DIFF LCM (DP_LMON PJ)))
      (SETQ ZI (DP_LC PJ))
      (SETQ ZJ (CALI_BC_NEG (DP_LC PI_)))
      (SETQ A
              (DP_SUM (DP_TIMES_BCMO ZI MI (CDR PI_))
               (DP_TIMES_BCMO ZJ MJ (CDR PJ))))
      (SETQ B (DP_SUM (DP_TIMES_BCMO ZI MI RI) (DP_TIMES_BCMO ZJ MJ RJ)))
      (SETQ A (BAS_MAKE1 0 A B))
      (COND (*BCSIMP (SETQ A (CAR (BAS_SIMPELEMENT A)))))
      (COND
       ((GREATERP (CALI_TRACE) 70)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 " S.-pol : ") NIL)
         (DP_PRINT2 (BAS_DPOLY A)))))
      (RETURN A))) 
(PUT 'GROEB_MINGB 'NUMBER-OF-ARGS 1) 
(PUT 'GROEB_MINGB 'DEFINED-ON-LINE '522) 
(PUT 'GROEB_MINGB 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_MINGB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEB_MINGB (GB)
    (PROG (U)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CAR (RED_COLLECT (DPMAT_LIST GB))))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((GREATERP (BAS_NR X) 0) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((GREATERP (BAS_NR X) 0) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN
       (DPMAT_MAKE (LENGTH U) (DPMAT_COLS GB) (BAS_RENUMBER U)
        (DPMAT_COLDEGS GB) (DPMAT_GBTAG GB))))) 
(PUT 'GROEB=DELETE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB=DELETE 'DEFINED-ON-LINE '535) 
(PUT 'GROEB=DELETE 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB=DELETE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB=DELETE (L BAS)
    (PROG (B)
      (PROG ()
       WHILELABEL
        (COND ((NOT BAS) (RETURN NIL)))
        (PROGN
         (COND ((NOT (MEMQ (BAS_NR (CAR BAS)) L)) (SETQ B (CONS (CAR BAS) B))))
         (SETQ BAS (CDR BAS)))
        (GO WHILELABEL))
      (RETURN (REVERSE B)))) 
(PUT 'GROEB_MINIMIZE 'NUMBER-OF-ARGS 2) 
(PUT 'GROEB_MINIMIZE 'DEFINED-ON-LINE '546) 
(PUT 'GROEB_MINIMIZE 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_MINIMIZE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEB_MINIMIZE (BAS SYZ)
    ((LAMBDA (CALI=DEGREES)
       (PROG (DROWS DCOLS S S1 I J P Q Y)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS SYZ))
         (SETQ S1 (DPMAT_LIST SYZ))
         (SETQ J 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT (LESSP J (DPMAT_ROWS SYZ))) (RETURN NIL)))
           (PROGN
            (SETQ J (PLUS J 1))
            (COND
             ((SETQ Q (BAS_DPOLY (BAS_GETELEMENT J S1)))
              (PROGN
               (SETQ I 0)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT
                    (AND (LEQ I (DPMAT_COLS SYZ))
                         (OR (MEMQ I DCOLS)
                             (NOT (DP_UNIT? (SETQ P (DP_COMP I Q)))))))
                   (RETURN NIL)))
                 (SETQ I (PLUS I 1))
                 (GO WHILELABEL))
               (COND
                ((LEQ I (DPMAT_COLS SYZ))
                 (PROGN
                  (SETQ DROWS (CONS J DROWS))
                  (SETQ DCOLS (CONS I DCOLS))
                  (SETQ S1
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X S1)
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (X)
                                                (COND
                                                 ((MEMQ (BAS_NR X) DROWS) X)
                                                 (T
                                                  ((LAMBDA (Y)
                                                     (BAS_MAKE (BAS_NR X)
                                                      (DP_DIFF (DP_PROD Y P)
                                                       (DP_PROD Q
                                                        (DP_COMP I Y)))))
                                                   (BAS_DPOLY X)))))
                                              (CAR X))
                                             NIL)))
                           LOOPLABEL
                            (SETQ X (CDR X))
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X)
                                        (COND ((MEMQ (BAS_NR X) DROWS) X)
                                              (T
                                               ((LAMBDA (Y)
                                                  (BAS_MAKE (BAS_NR X)
                                                   (DP_DIFF (DP_PROD Y P)
                                                    (DP_PROD Q
                                                     (DP_COMP I Y)))))
                                                (BAS_DPOLY X)))))
                                      (CAR X))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                  NIL)))))))
           (GO WHILELABEL))
         (SETQ S1 (BAS_RENUMBER (BAS_SIMP (GROEB=DELETE DROWS S1))))
         (SETQ S1
                 (DPMAT_MAKE (LENGTH S1) (DPMAT_COLS SYZ) S1 CALI=DEGREES NIL))
         (SETQ S
                 (DPMAT_RENUMBER
                  (DPMAT_MAKE (DPMAT_ROWS BAS) (DPMAT_COLS BAS)
                   (GROEB=DELETE DCOLS (DPMAT_LIST BAS)) (DPMAT_COLDEGS BAS)
                   NIL)))
         (SETQ S1 (DPMAT_MULT S1 (DPMAT_TRANSPOSE (CDR S))))
         (SETQ S (CAR S))
         (SETQ CALI=DEGREES (DPMAT_ROWDEGREES S))
         (SETQ S1
                 (INTERREDUCE*
                  (DPMAT_MAKE (DPMAT_ROWS S1) (DPMAT_COLS S1)
                   (BAS_NEWORDER (DPMAT_LIST S1)) CALI=DEGREES NIL)))
         (RETURN (CONS S S1))))
     CALI=DEGREES)) 
(PUT 'GROEB_HOMSTBASIS 'NUMBER-OF-ARGS 4) 
(PUT 'GROEB_HOMSTBASIS 'DEFINED-ON-LINE '592) 
(PUT 'GROEB_HOMSTBASIS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'GROEB_HOMSTBASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEB_HOMSTBASIS (M COMP_MGB COMP_CH COMP_SYZ)
    ((LAMBDA (CALI=BASERING CALI=MONSET CALI=DEGREES)
       (PROG (V C U)
         (SETQ C CALI=BASERING)
         (SETQ V (LIST (MAKE_CALI_VARNAME)))
         (COND
          ((NOT (OR COMP_CH COMP_SYZ))
           (SETQ CALI=MONSET (APPEND V CALI=MONSET))))
         (SETRING* (RING_SUM C (RING_DEFINE V NIL 'LEX '(1))))
         (SETQ CALI=DEGREES (MO_DEGNEWORDER (DPMAT_COLDEGS M)))
         (COND ((GREATERP (CALI_TRACE) 0) (PRINT " Homogenize input ")))
         (SETQ U
                 ((LAMBDA (*NOETHERIAN)
                    (GROEB_STBASIS (MATHOMOGENIZE* M (CAR V)) COMP_MGB COMP_CH
                     COMP_SYZ))
                  T))
         (COND ((GREATERP (CALI_TRACE) 0) (PRINT " Dehomogenize output ")))
         (SETQ U
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X U)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (COND (X (MATDEHOMOGENIZE* X (CAR V)))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (COND (X (MATDEHOMOGENIZE* X (CAR V)))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETRING* C)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (RETURN
          (LIST (COND ((FIRST U) (DPMAT_NEWORDER (FIRST U) T)))
                (COND ((SECOND U) (DPMAT_NEWORDER (SECOND U) NIL)))
                (COND ((THIRD U) (DPMAT_NEWORDER (THIRD U) NIL)))))))
     CALI=BASERING CALI=MONSET CALI=DEGREES)) 
(FLAG '(HOMSTBASIS) 'OPFN) 
(PUT 'HOMSTBASIS 'NUMBER-OF-ARGS 1) 
(PUT 'HOMSTBASIS 'DEFINED-ON-LINE '616) 
(PUT 'HOMSTBASIS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'HOMSTBASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HOMSTBASIS (M)
    (COND ((EQUAL *MODE 'ALGEBRAIC) (DPMAT_2A (HOMSTBASIS* (DPMAT_FROM_A M))))
          (T (HOMSTBASIS* M)))) 
(PUT 'HOMSTBASIS* 'NUMBER-OF-ARGS 1) 
(PUT 'HOMSTBASIS* 'DEFINED-ON-LINE '620) 
(PUT 'HOMSTBASIS* 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'HOMSTBASIS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HOMSTBASIS* (M) (GROEB_MINGB (CAR (GROEB_HOMSTBASIS M T NIL NIL)))) 
(FLAG '(LAZYSTBASIS) 'OPFN) 
(PUT 'LAZYSTBASIS 'NUMBER-OF-ARGS 1) 
(PUT 'LAZYSTBASIS 'DEFINED-ON-LINE '624) 
(PUT 'LAZYSTBASIS 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'LAZYSTBASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAZYSTBASIS (M)
    (COND ((EQUAL *MODE 'ALGEBRAIC) (DPMAT_2A (LAZYSTBASIS* (DPMAT_FROM_A M))))
          (T (LAZYSTBASIS* M)))) 
(PUT 'LAZYSTBASIS* 'NUMBER-OF-ARGS 1) 
(PUT 'LAZYSTBASIS* 'DEFINED-ON-LINE '628) 
(PUT 'LAZYSTBASIS* 'DEFINED-IN-FILE 'CALI/GROEB.RED) 
(PUT 'LAZYSTBASIS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAZYSTBASIS* (M) (CAR (GROEB_LAZYSTBASIS M T NIL NIL))) 
(ENDMODULE) 