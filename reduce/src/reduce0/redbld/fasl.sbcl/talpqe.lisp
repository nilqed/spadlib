(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TALPQE)) 
(REVISION 'TALPQE "$Id: talpqe.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'TALPQE "(c) 2004-2009 A. Dolzmann, T. Sturm, 2016 T. Sturm") 
(PUT 'TALP_QE 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_QE 'DEFINED-ON-LINE '32) 
(PUT 'TALP_QE 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_QE (PHI THEO)
    (PROGN (SETQ PHI (CL_SIMPL (CL_PNF PHI) NIL (MINUS 1))) (TALP_QE1 PHI))) 
(PUT 'TALP_QEA 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_QEA 'DEFINED-ON-LINE '41) 
(PUT 'TALP_QEA 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_QEA (PHI THEO)
    (PROG (*RLQEANS)
      (SETQ *RLQEANS T)
      (SETQ PHI (CL_SIMPL (CL_PNF PHI) NIL (MINUS 1)))
      (RETURN (TALP_QE1 PHI)))) 
(PUT 'TALP_QE1 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_QE1 'DEFINED-ON-LINE '54) 
(PUT 'TALP_QE1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_QE1 (PHI)
    (PROG (SPLIT SPHI)
      (SETQ SPHI (TALP_TRY PHI))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM SPHI) SPHI) (T (CAR SPHI)))))
        (RETURN (COND (*RLQEANS (LIST (CONS SPHI NIL))) (T SPHI)))))
      (SETQ SPLIT (CL_SPLT PHI))
      (RETURN (TALP_QE2 (CAR SPLIT) (CADR SPLIT))))) 
(PUT 'TALP_QE2 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_QE2 'DEFINED-ON-LINE '67) 
(PUT 'TALP_QE2 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_QE2 (QBL MTRX)
    (PROG (RESULT TMP BVARL QTF QB)
      (SETQ TMP MTRX)
      (PROG ()
       WHILELABEL
        (COND ((NOT QBL) (RETURN NIL)))
        (PROGN
         (SETQ QB (CAR QBL))
         (SETQ QTF (CAR QB))
         (SETQ BVARL (CDR QB))
         (SETQ QBL (CDR QBL))
         (COND
          (*RLVERBOSE
           (IOTO_PRIN2T (LIST "+++ eliminate block " QTF (REVERSE BVARL)))))
         (SETQ TMP (TALP_QEBLOCK QTF BVARL TMP (AND *RLQEANS (NULL QBL))))
         (SETQ RESULT TMP))
        (GO WHILELABEL))
      (RETURN (COND (*RLQEANS (TALP_GETANS RESULT)) (T RESULT))))) 
(PUT 'TALP_QEBLOCK 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_QEBLOCK 'DEFINED-ON-LINE '88) 
(PUT 'TALP_QEBLOCK 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_QEBLOCK (QTF BVARL MTRX ANSP)
    (PROG (TMP ANSWER NEWMTRX L STOP BLK RESULT NOSP)
      (SETQ NEWMTRX (COND ((EQ QTF 'EX) MTRX) (T (CL_NNFNOT MTRX))))
      (SETQ L
              (COND
               ((EQ (COND ((ATOM NEWMTRX) NEWMTRX) (T (CAR NEWMTRX))) 'OR)
                (CDR NEWMTRX))
               (T (LIST NEWMTRX))))
      (SETQ NOSP (LENGTH L))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L (NOT STOP))) (RETURN NIL)))
        (PROGN
         (SETQ BLK (CAR L))
         (SETQ L (CDR L))
         (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "    [" NOSP "] subproblems"))))
         (SETQ TMP (TALP_QEBLOCK1 BVARL BLK ANSP))
         (COND
          (ANSP
           (PROGN
            (COND
             ((EQ (CDR TMP) 'TRUE)
              (PROGN (SETQ STOP T) (SETQ RESULT (CAR TMP))))
             ((NEQ (CDR TMP) 'FALSE)
              (SETQ RESULT
                      (COND (RESULT (APPEND RESULT (CAR TMP)))
                            (T (CAR TMP))))))
            NIL))
          (T
           (SETQ RESULT
                   (CL_SIMPL
                    (COND (RESULT (LIST 'OR RESULT (CDR TMP))) (T (CDR TMP)))
                    NIL (MINUS 1)))))
         (COND ((EQ RESULT 'TRUE) (SETQ STOP T)))
         (SETQ NOSP (DIFFERENCE NOSP 1)))
        (GO WHILELABEL))
      (COND
       ((NULL RESULT)
        (SETQ RESULT (COND (ANSP (LIST (CONS NIL 'FALSE))) (T 'FALSE)))))
      (COND
       ((EQ QTF 'ALL)
        (COND
         (ANSP
          (PROGN
           (SETQ ANSWER
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X RESULT)
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (CONS (CAR X) (CL_NNFNOT (CDR X))))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (CONS (CAR X) (CL_NNFNOT (CDR X))))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN ANSWER)))
         (T (RETURN (CL_NNFNOT RESULT)))))
       (T (RETURN RESULT))))) 
(PUT 'TALP_QEBLOCK1 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_QEBLOCK1 'DEFINED-ON-LINE '125) 
(PUT 'TALP_QEBLOCK1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEBLOCK1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_QEBLOCK1 (BVARL MTRX ANSP)
    (PROG (BVL)
      (SETQ BVL
              (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                (SETQ VAR BVARL)
               STARTOVER
                (COND ((NULL VAR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (VAR)
                           (COND ((TALP_CONTAINS MTRX VAR) (LIST VAR))))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ VAR (CDR VAR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (VAR)
                           (COND ((TALP_CONTAINS MTRX VAR) (LIST VAR))))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ VAR (CDR VAR))
                (GO LOOPLABEL)))
      (COND (*TALPQP (SETQ BVL (TALP_PERMBVARL BVL MTRX))))
      (COND ((NULL BVL) (RETURN (CONS NIL MTRX))))
      (COND
       ((AND *RLVERBOSE (CDR BVL) *TALPQP)
        (IOTO_PRIN2T
         (LIST "    new order for processing bound variables: " BVL))))
      (RETURN (TALP_QEEXBLOCK BVL MTRX NIL ANSP)))) 
(PUT 'TALP_QEEXBLOCK 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_QEEXBLOCK 'DEFINED-ON-LINE '139) 
(PUT 'TALP_QEEXBLOCK 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEEXBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_QEEXBLOCK (BVL MTRX ANS ANSP)
    (PROG (NEW TMP TMP2 RESULT STOP ASET APS NOSP)
      (SETQ TMP (TALP_QEVAR (CAR BVL) MTRX ANSP))
      (COND
       (ANSP
        (PROGN
         (SETQ APS (TALP_GETPAIRS (CAR TMP) (CDR TMP)))
         (COND (ANS (SETQ APS (TALP_INSERTEQ APS ANS))))
         NIL)))
      (COND ((NULL (CDR BVL)) (RETURN (CONS APS (CDR TMP)))))
      (COND
       ((NULL ANSP) (RETURN (TALP_QEEXBLOCK (CDR BVL) (CDR TMP) ANS ANSP))))
      (SETQ TMP APS)
      (SETQ NOSP (LENGTH TMP))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMP (NOT STOP))) (RETURN NIL)))
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "   [" NOSP "] answers"))))
         (SETQ NOSP (DIFFERENCE NOSP 1))
         (SETQ NEW (CAR TMP))
         (SETQ TMP (CDR TMP))
         (SETQ TMP2 (TALP_QEEXBLOCK (CDR BVL) (CDR NEW) (CAR NEW) ANSP))
         (COND
          ((EQ (CDR TMP2) 'TRUE)
           (PROGN (SETQ STOP T) (SETQ RESULT 'TRUE) (SETQ ASET (CAR TMP2))))
          ((NEQ (CDR TMP2) 'FALSE)
           (SETQ ASET (COND (ASET (APPEND ASET (CAR TMP2))) (T (CAR TMP2))))))
         NIL)
        (GO WHILELABEL))
      (COND ((NULL RESULT) (SETQ RESULT (CDR TMP2))))
      (COND ((NULL ASET) (SETQ ASET (CAR TMP2))))
      (RETURN (CONS ASET RESULT)))) 
(PUT 'TALP_QEVAR 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_QEVAR 'DEFINED-ON-LINE '178) 
(PUT 'TALP_QEVAR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_QEVAR (BVAR MTRX ANSP)
    (PROG (TMP TMP2 NOSP STOP NEW FVARL MAXD NOFT LTYPE RESULT ASET)
      (SETQ TMP
              (COND
               ((EQ (COND ((ATOM MTRX) MTRX) (T (CAR MTRX))) 'OR) (CDR MTRX))
               (T (LIST MTRX))))
      (SETQ NOSP (LENGTH TMP))
      (SETQ LTYPE (TALP_GETTYPE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMP (NOT STOP))) (RETURN NIL)))
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "++ [" NOSP "] subproblems"))))
         (SETQ NOSP (DIFFERENCE NOSP 1))
         (SETQ NEW (CAR TMP))
         (SETQ TMP (CDR TMP))
         (COND (*TALPQEGAUSS (SETQ TMP2 (TALP_TRYQEGAUSS BVAR MTRX ANSP))))
         (COND
          ((OR (NULL *TALPQEGAUSS) (EQ TMP2 'FAILED))
           (PROGN
            (SETQ FVARL (SETDIFF (CL_FVARL NEW) (LIST BVAR)))
            (SETQ MAXD (TALP_DEPTHBOUND NEW LTYPE))
            (SETQ NOFT (TALP_NUMBERBOUND FVARL MAXD LTYPE))
            (COND
             (*RLVERBOSE
              (PROGN
               (COND (*TALPQEGAUSS (IOTO_PRIN2T "failed")))
               (IOTO_PRIN2
                (LIST (COND (*TALPQEGAUSS "  ") (T "+")) "standard QE for "
                      BVAR ": substitute max " "[" NOFT "] terms of depth <= "
                      MAXD " ... "))
               NIL)))
            (SETQ TMP2 (TALP_QEVAR1 BVAR NEW ANSP FVARL MAXD)))))
         (COND
          ((EQ (CDR TMP2) 'TRUE)
           (PROGN (SETQ STOP T) (SETQ ASET (CAR TMP2)) (SETQ RESULT 'TRUE)))
          ((NEQ (CDR TMP2) 'FALSE)
           (PROGN
            (COND
             (ANSP
              (SETQ ASET
                      (COND (ASET (APPEND ASET (CAR TMP2))) (T (CAR TMP2)))))
             (T
              (PROGN
               (SETQ RESULT
                       (CL_SIMPL
                        (COND (RESULT (LIST 'OR (CDR TMP2) RESULT))
                              (T (CDR TMP2)))
                        NIL (MINUS 1)))
               (COND ((EQ RESULT 'TRUE) (SETQ STOP T))))))))))
        (GO WHILELABEL))
      (COND ((NULL RESULT) (SETQ RESULT (CDR TMP2))))
      (COND ((NULL ASET) (SETQ ASET (CAR TMP2))))
      (RETURN (CONS ASET RESULT)))) 
(PUT 'TALP_QEVAR1 'NUMBER-OF-ARGS 5) 
(PUT 'TALP_QEVAR1 'DEFINED-ON-LINE '228) 
(PUT 'TALP_QEVAR1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QEVAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_QEVAR1 (BVAR MTRX ANSP FVARL MAXD)
    (PROG (RESULT ANSWER TMP T2SUB STOP SUBPAIR NEW)
      (SETQ T2SUB (TALP_NEXTT NIL MAXD FVARL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND T2SUB (NULL STOP))) (RETURN NIL)))
        (PROGN
         (SETQ SUBPAIR (CONS BVAR T2SUB))
         (SETQ TMP (TALP_TRY (CL_SUBFOF (LIST SUBPAIR) MTRX)))
         (COND
          ((EQ TMP 'TRUE)
           (PROGN
            (COND (ANSP (SETQ ANSWER SUBPAIR)))
            (SETQ RESULT TMP)
            (SETQ STOP T)))
          ((NEQ TMP 'FALSE)
           (PROGN
            (COND
             (ANSP
              (PROGN
               (SETQ ANSWER (CONS SUBPAIR ANSWER))
               (SETQ RESULT (CONS TMP RESULT))))
             (T
              (PROGN
               (SETQ RESULT
                       (CL_SIMPL (COND (RESULT (LIST 'OR RESULT TMP)) (T TMP))
                                 NIL (MINUS 1)))
               (COND ((EQ RESULT 'TRUE) (SETQ STOP T)))))))))
         (SETQ NEW (TALP_COPY T2SUB))
         (SETQ T2SUB (TALP_NEXTT NEW MAXD FVARL)))
        (GO WHILELABEL))
      (COND (*RLVERBOSE (IOTO_PRIN2T "succeeded")))
      (COND ((NULL RESULT) (SETQ RESULT 'FALSE)))
      (RETURN (CONS ANSWER RESULT)))) 
(PUT 'TALP_PERMBVARL 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_PERMBVARL 'DEFINED-ON-LINE '264) 
(PUT 'TALP_PERMBVARL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_PERMBVARL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_PERMBVARL (BVARL MTRX)
    (PROG (TMP NEWBVARL N)
      (COND
       ((OR (OR (EQ MTRX 'TRUE) (EQ MTRX 'FALSE)) (NULL BVARL))
        (RETURN BVARL)))
      (SETQ N (PLUS (TIMES 2 (LENGTH (RL_ATL MTRX))) 1))
      (SETQ TMP
              (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                (SETQ VAR BVARL)
               STARTOVER
                (COND ((NULL VAR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (VAR)
                           (COND
                            ((TALP_CONTAINS MTRX VAR)
                             (COND
                              ((AND *TALPQEGAUSS
                                    (NOT
                                     (OR
                                      (MEMQ (TALP_TRYGAUSSVAR VAR MTRX NIL)
                                            '(FAILED IGNORE))
                                      (MEMQ
                                       (TALP_TRYGAUSSVAR VAR (TALP_RNF MTRX)
                                        NIL)
                                       '(FAILED IGNORE)))))
                               (LIST (CONS VAR N)))
                              (T (LIST (CONS VAR (TALP_COCC MTRX VAR))))))))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ VAR (CDR VAR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (VAR)
                           (COND
                            ((TALP_CONTAINS MTRX VAR)
                             (COND
                              ((AND *TALPQEGAUSS
                                    (NOT
                                     (OR
                                      (MEMQ (TALP_TRYGAUSSVAR VAR MTRX NIL)
                                            '(FAILED IGNORE))
                                      (MEMQ
                                       (TALP_TRYGAUSSVAR VAR (TALP_RNF MTRX)
                                        NIL)
                                       '(FAILED IGNORE)))))
                               (LIST (CONS VAR N)))
                              (T (LIST (CONS VAR (TALP_COCC MTRX VAR))))))))
                         (CAR VAR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ VAR (CDR VAR))
                (GO LOOPLABEL)))
      (COND
       (TMP
        (PROGN
         (SETQ TMP (TALP_MERGESORT TMP))
         (SETQ NEWBVARL
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X TMP)
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
         NIL))
       (T (RETURN NIL)))
      (RETURN NEWBVARL))) 
(PUT 'TALP_COCC 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_COCC 'DEFINED-ON-LINE '287) 
(PUT 'TALP_COCC 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_COCC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_COCC (F VAR)
    (PROG (NOCCS)
      (SETQ NOCCS 0)
      (SETQ NOCCS 0)
      (COND
       ((PAIRP F)
        (PROG (X)
          (SETQ X F)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND ((PAIRP X) (SETQ NOCCS (PLUS NOCCS (TALP_COCC X VAR))))
                   (T (COND ((EQ X VAR) (SETQ NOCCS (PLUS NOCCS 1)))))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB)))
       ((EQ VAR F) (RETURN 1)))
      (RETURN NOCCS))) 
(PUT 'TALP_MERGESORT 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_MERGESORT 'DEFINED-ON-LINE '301) 
(PUT 'TALP_MERGESORT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_MERGESORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_MERGESORT (L)
    (PROG (CRIT S1 S2)
      (COND ((OR (NULL L) (NULL (CDR L))) (RETURN L)))
      (SETQ CRIT (CAR L))
      (PROG (ENTRY)
        (SETQ ENTRY (CDR L))
       LAB
        (COND ((NULL ENTRY) (RETURN NIL)))
        ((LAMBDA (ENTRY)
           (COND ((GREATERP (CDR ENTRY) (CDR CRIT)) (SETQ S1 (CONS ENTRY S1)))
                 (T (SETQ S2 (CONS ENTRY S2)))))
         (CAR ENTRY))
        (SETQ ENTRY (CDR ENTRY))
        (GO LAB))
      (RETURN
       (NCONC (TALP_MERGESORT (REVERSIP S1))
              (CONS CRIT (TALP_MERGESORT (REVERSIP S2))))))) 
(PUT 'TALP_GETPAIRS 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_GETPAIRS 'DEFINED-ON-LINE '317) 
(PUT 'TALP_GETPAIRS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GETPAIRS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_GETPAIRS (ANSWERSET RESULTSET)
    (PROG (TMP ANSWER RESULT)
      (COND ((ATOM RESULTSET) (RETURN (LIST (CONS ANSWERSET RESULTSET)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT RESULTSET) (RETURN NIL)))
        (PROGN
         (SETQ ANSWER (CAR ANSWERSET))
         (SETQ RESULT (CAR RESULTSET))
         (SETQ TMP
                 (COND (TMP (CONS (CONS ANSWER RESULT) TMP))
                       (T (LIST (CONS ANSWER RESULT)))))
         (SETQ RESULTSET (CDR RESULTSET))
         (SETQ ANSWERSET (CDR ANSWERSET)))
        (GO WHILELABEL))
      (RETURN TMP))) 
(PUT 'TALP_COPY 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_COPY 'DEFINED-ON-LINE '335) 
(PUT 'TALP_COPY 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_COPY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_COPY (L)
    (PROG (NL)
      (COND ((ATOM L) (SETQ NL L))
            (T
             (SETQ NL
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X L)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X)
                                           (COND ((ATOM X) X)
                                                 (T (TALP_COPY X))))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (COND ((ATOM X) X) (T (TALP_COPY X))))
                                 (CAR X))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (RETURN NL))) 
(PUT 'TALP_INSERTEQ 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_INSERTEQ 'DEFINED-ON-LINE '347) 
(PUT 'TALP_INSERTEQ 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_INSERTEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_INSERTEQ (ANS EQUA)
    (PROG (TMP EQUS)
      (COND ((EQ (CDAR ANS) 'FALSE) (RETURN ANS)))
      (SETQ TMP ANS)
      (PROG (ELEM)
        (SETQ ELEM TMP)
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (PROGN
            (SETQ EQUS
                    (TALP_UPDATEINFO ELEM
                     (COND ((ATOM (CAR EQUA)) (LIST EQUA)) (T EQUA))))
            (SETCAR ELEM
                    (COND ((ATOM (CAAR ELEM)) (CONS (CAR ELEM) EQUS))
                          (T (APPEND (CAR ELEM) EQUS))))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (RETURN TMP))) 
(PUT 'TALP_UPDATEINFO 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_UPDATEINFO 'DEFINED-ON-LINE '363) 
(PUT 'TALP_UPDATEINFO 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_UPDATEINFO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_UPDATEINFO (ANS EQUS)
    (PROG (TMP RESULT)
      (SETQ TMP (CAR ANS))
      (SETQ RESULT
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM EQUS)
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (CONS (CAR ELEM)
                                          (TALP_SIMPLT
                                           (TALP_SPECSUBT (CAR TMP) (CDR TMP)
                                            (CDR ELEM)))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM)
                            (CONS (CAR ELEM)
                                  (TALP_SIMPLT
                                   (TALP_SPECSUBT (CAR TMP) (CDR TMP)
                                    (CDR ELEM)))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RESULT))) 
(PUT 'TALP_GETANS 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_GETANS 'DEFINED-ON-LINE '375) 
(PUT 'TALP_GETANS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GETANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_GETANS (ANSINFO)
    (PROG (ANSWER RESULT EQUA EQUAT INFO)
      (COND ((NULL (CAR ANSINFO)) (RETURN (LIST (LIST (CDR ANSINFO) NIL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT ANSINFO) (RETURN NIL)))
        (PROGN
         (SETQ INFO (CAR ANSINFO))
         (SETQ EQUAT (CAR INFO))
         (SETQ RESULT (CDR INFO))
         (SETQ EQUA
                 (COND
                  (EQUAT (COND ((ATOM (CAR EQUAT)) (LIST EQUAT)) (T EQUAT)))))
         (SETQ ANSWER (CONS (CONS RESULT EQUA) ANSWER))
         (SETQ ANSINFO (CDR ANSINFO)))
        (GO WHILELABEL))
      (RETURN ANSWER))) 
(PUT 'TALP_GETTYPE 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_GETTYPE 'DEFINED-ON-LINE '397) 
(PUT 'TALP_GETTYPE 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GETTYPE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_GETTYPE NIL
    (PROG (LANG UNACOUNT DONE)
      (SETQ UNACOUNT 0)
      (SETQ LANG TALP_LANG*)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LANG (NOT DONE))) (RETURN NIL)))
        (PROGN
         (COND ((GREATERP (CDAR LANG) 1) (SETQ DONE T)))
         (COND ((EQ (CDAR LANG) 1) (SETQ UNACOUNT (PLUS UNACOUNT 1))))
         (SETQ LANG (CDR LANG)))
        (GO WHILELABEL))
      (RETURN (COND (DONE 'NN) ((GREATERP UNACOUNT 1) 'UN) (T 'U1))))) 
(PUT 'TALP_DEPTHBOUND 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_DEPTHBOUND 'DEFINED-ON-LINE '412) 
(PUT 'TALP_DEPTHBOUND 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_DEPTHBOUND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_DEPTHBOUND (F LANGTYPE)
    (COND
     ((EQ LANGTYPE 'NN)
      (PLUS (TIMES (TALP_MAXD F) (PLUS (RL_ATNUM F) 1))
            (CEILING (LOG (PLUS (RL_ATNUM F) 1)))))
     ((EQ LANGTYPE 'UN)
      (PLUS (TIMES 2 (TALP_MAXD F)) (CEILING (LOG (PLUS (RL_ATNUM F) 1)))))
     (T (PLUS (TIMES 2 (TALP_MAXD F)) (RL_ATNUM F))))) 
(PUT 'TALP_NUMBERBOUND 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_NUMBERBOUND 'DEFINED-ON-LINE '423) 
(PUT 'TALP_NUMBERBOUND 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NUMBERBOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_NUMBERBOUND (FVARL DEPTH LANGTYPE)
    (COND ((EQ LANGTYPE 'U1) (TALP_NBU1 FVARL DEPTH))
          ((EQ LANGTYPE 'UN) (TALP_NBUN FVARL DEPTH))
          (T (TALP_NBNN FVARL DEPTH)))) 
(PUT 'TALP_NBU1 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_NBU1 'DEFINED-ON-LINE '436) 
(PUT 'TALP_NBU1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NBU1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_NBU1 (FVARL DEPTH)
    (PROG (NOFT NOCTS NOVARS TMP)
      (SETQ NOCTS (TALP_GETNOFCTS))
      (SETQ NOVARS (LENGTH FVARL))
      (SETQ NOFT (PLUS NOCTS NOVARS))
      (SETQ TMP (PLUS NOFT NOVARS))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP DEPTH 0)) (RETURN NIL)))
        (PROGN
         (SETQ NOFT (PLUS TMP NOFT))
         (SETQ TMP (PLUS TMP NOVARS))
         (SETQ DEPTH (DIFFERENCE DEPTH 1)))
        (GO WHILELABEL))
      (RETURN NOFT))) 
(PUT 'TALP_NBUN 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_NBUN 'DEFINED-ON-LINE '454) 
(PUT 'TALP_NBUN 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NBUN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_NBUN (FVARL DEPTH)
    (EXPT (PLUS (LENGTH FVARL) (LENGTH TALP_EXTLANG*))
          (COND ((GREATERP DEPTH 0) DEPTH) (T 1)))) 
(PUT 'TALP_NBNN 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_NBNN 'DEFINED-ON-LINE '461) 
(PUT 'TALP_NBNN 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NBNN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_NBNN (FVARL DEPTH)
    (PROG (MA)
      (SETQ MA (TALP_GETMAXAR))
      (RETURN
       (EXPT (EXPT (PLUS (LENGTH TALP_EXTLANG*) (LENGTH FVARL)) MA)
             (COND ((GREATERP DEPTH 0) DEPTH) (T 1)))))) 
(PUT 'TALP_GETNOFCTS 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_GETNOFCTS 'DEFINED-ON-LINE '472) 
(PUT 'TALP_GETNOFCTS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GETNOFCTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_GETNOFCTS NIL
    (PROG (X FORALL-RESULT)
      (SETQ X TALP_LANG*)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (X) (COND ((EQ (CDR X) 0) 1) (T 0))) (CAR X))
                    FORALL-RESULT))
      (SETQ X (CDR X))
      (GO LAB1))) 
(PUT 'TALP_GETMAXAR 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_GETMAXAR 'DEFINED-ON-LINE '478) 
(PUT 'TALP_GETMAXAR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GETMAXAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_GETMAXAR NIL
    (PROG (ARITY)
      (SETQ ARITY 0)
      (PROG (X)
        (SETQ X TALP_LANG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((GREATERP (CDR X) ARITY) (SETQ ARITY (CDR X)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN ARITY))) 
(PUT 'TALP_MAXD 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_MAXD 'DEFINED-ON-LINE '489) 
(PUT 'TALP_MAXD 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_MAXD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_MAXD (F)
    (PROG (TMP MD MDTMP)
      (SETQ MD 0)
      (SETQ MDTMP 0)
      (COND ((ATOM F) (RETURN 0)))
      (SETQ TMP (RL_ATL F))
      (PROG ()
       WHILELABEL
        (COND ((NOT TMP) (RETURN NIL)))
        (PROGN
         (SETQ MDTMP
                 (MAX2 (TALP_TD (CADR (CAR TMP))) (TALP_TD (CADDR (CAR TMP)))))
         (COND ((GREATERP MDTMP MD) (SETQ MD MDTMP)))
         (SETQ TMP (CDR TMP))
         NIL)
        (GO WHILELABEL))
      (RETURN MD))) 
(PUT 'TALP_TD 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_TD 'DEFINED-ON-LINE '504) 
(PUT 'TALP_TD 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_TD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_TD (TERM)
    (COND ((ATOM TERM) 0)
          (T
           (PLUS 1
                 (LTO_MAX
                  (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ARG (CDR TERM))
                    (COND ((NULL ARG) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ARG) (TALP_TD ARG)) (CAR ARG))
                                     NIL)))
                   LOOPLABEL
                    (SETQ ARG (CDR ARG))
                    (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (ARG) (TALP_TD ARG)) (CAR ARG))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))) 
(PUT 'TALP_CONTAINS 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_CONTAINS 'DEFINED-ON-LINE '511) 
(PUT 'TALP_CONTAINS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_CONTAINS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_CONTAINS (F VAR)
    (PROG (CV)
      (COND
       ((PAIRP F)
        (PROG (X)
          (SETQ X F)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND ((AND (PAIRP X) (NOT CV)) (SETQ CV (TALP_CONTAINS X VAR)))
                   (T (COND ((EQ X VAR) (SETQ CV T))))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB)))
       ((EQ VAR F) (RETURN T)))
      (RETURN CV))) 
(PUT 'TALP_NEXTT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_NEXTT 'DEFINED-ON-LINE '527) 
(PUT 'TALP_NEXTT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NEXTT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_NEXTT (LAST MD VL)
    (PROG (CL CVV VV FV IFV)
      (SETQ FV
              (TALP_LIST2VEC
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X TALP_LANG*)
                STARTOVER
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (X)
                            (COND
                             ((EQ (CDR X) 0)
                              (PROGN (SETQ CL (CONS (CAR X) CL)) NIL))
                             (T (TALP_MK-INVS VL X))))
                          (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ X (CDR X))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (X)
                            (COND
                             ((EQ (CDR X) 0)
                              (PROGN (SETQ CL (CONS (CAR X) CL)) NIL))
                             (T (TALP_MK-INVS VL X))))
                          (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ X (CDR X))
                 (GO LOOPLABEL))))
      (SETQ IFV
              (TALP_LIST2VEC
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I 0)
                STARTOVER
                 (COND ((MINUSP (DIFFERENCE (UPBV FV) I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (Y) (COND ((TALP_INVP Y) (LIST Y))))
                          (GETV FV I)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ I (PLUS2 I 1))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND
                  ((MINUSP (DIFFERENCE (UPBV FV) I)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (Y) (COND ((TALP_INVP Y) (LIST Y))))
                          (GETV FV I)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ I (PLUS2 I 1))
                 (GO LOOPLABEL))))
      (SETQ CVV (TALP_LIST2VEC (NCONC (REVERSIP CL) VL)))
      (SETQ VV (TALP_LIST2VEC VL))
      (RETURN
       (COND
        (LAST
         ((LAMBDA (Z) (COND ((CAR Z) (CDR Z))))
          (TALP_NEXTT1 LAST 0 MD CVV VV FV IFV NIL)))
        ((GREATERP (UPBV CVV) (MINUS 1)) (GETV CVV 0)))))) 
(PUT 'TALP_NEXTT1 'NUMBER-OF-ARGS 8) 
(PUT 'TALP_NEXTT1 'DEFINED-ON-LINE '546) 
(PUT 'TALP_NEXTT1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NEXTT1 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TALP_NEXTT1 (LAST CD MD CVV VV FV IFV INVP)
    (PROG (DONE RESET TEMP)
      (COND
       ((ATOM LAST) (RETURN (TALP_NEXTT-ATOM LAST CD MD CVV VV FV IFV INVP))))
      (COND
       ((TALP_INVP LAST)
        ((LAMBDA (X)
           (COND
            ((CAR X)
             (PROGN (SETCDR LAST (LIST (CDR X))) (RETURN (CONS T LAST))))))
         (TALP_NEXTT1 (CAR (CDR LAST)) (PLUS CD 1) MD CVV VV FV IFV T)))
       (T
        (PROGN
         (SETQ TEMP (CDR LAST))
         (SETQ RESET (GETV CVV 0))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND TEMP (NOT DONE))) (RETURN NIL)))
           (PROGN
            ((LAMBDA (X)
               (COND ((CAR X) (PROGN (SETQ DONE T) (SETCAR TEMP (CDR X))))
                     (T (SETCAR TEMP RESET))))
             (TALP_NEXTT1 (CAR TEMP) (PLUS CD 1) MD CVV VV FV IFV NIL))
            (SETQ TEMP (CDR TEMP)))
           (GO WHILELABEL)))))
      (COND
       ((NOT DONE)
        (COND
         (INVP
          ((LAMBDA (I)
             (COND
              ((LESSP I (UPBV IFV))
               (RETURN (CONS T (TALP_GET-MINFCT (PLUS I 1) IFV VV CVV))))))
           (TALP_GET-IDX LAST IFV)))
         (T
          ((LAMBDA (I)
             (COND
              ((LESSP I (UPBV FV))
               (RETURN (CONS T (TALP_GET-MINFCT (PLUS I 1) FV VV CVV))))))
           (TALP_GET-IDX LAST FV))))))
      (RETURN (CONS DONE LAST)))) 
(PUT 'TALP_NEXTT-ATOM 'NUMBER-OF-ARGS 8) 
(PUT 'TALP_NEXTT-ATOM 'DEFINED-ON-LINE '578) 
(PUT 'TALP_NEXTT-ATOM 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_NEXTT-ATOM 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TALP_NEXTT-ATOM (LAST CD MD CVV VV FV IFV INVP)
    (COND
     (INVP
      (COND
       ((LESSP (TALP_GET-IDX LAST VV) (UPBV VV))
        (CONS T (GETV VV (PLUS (TALP_GET-IDX LAST VV) 1))))
       ((LESSP CD MD) (CONS T (TALP_GET-MINFCT 0 IFV VV CVV)))
       (T (CONS NIL LAST))))
     ((LESSP (TALP_GET-IDX LAST CVV) (UPBV CVV))
      (CONS T (GETV CVV (PLUS (TALP_GET-IDX LAST CVV) 1))))
     ((LESSP CD MD) (CONS T (TALP_GET-MINFCT 0 FV VV CVV)))
     (T (CONS NIL LAST)))) 
(PUT 'TALP_LIST2VEC 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_LIST2VEC 'DEFINED-ON-LINE '589) 
(PUT 'TALP_LIST2VEC 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_LIST2VEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_LIST2VEC (L)
    (PROG (VEC)
      (SETQ VEC (MKVECT (DIFFERENCE (LENGTH L) 1)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV VEC) I)) (RETURN NIL)))
        (PROGN (PUTV VEC I (CAR L)) (SETQ L (CDR L)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN VEC))) 
(PUT 'TALP_MK-INVS 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_MK-INVS 'DEFINED-ON-LINE '598) 
(PUT 'TALP_MK-INVS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_MK-INVS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_MK-INVS (VP F)
    (COND
     (VP
      (CONS F
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 1)
              (COND ((MINUSP (DIFFERENCE (CDR F) I)) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS (CONS (TALP_MKINVOP (CAR F) I) 1) NIL)))
             LOOPLABEL
              (SETQ I (PLUS2 I 1))
              (COND ((MINUSP (DIFFERENCE (CDR F) I)) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS (CONS (TALP_MKINVOP (CAR F) I) 1) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (LIST F)))) 
(PUT 'TALP_GET-IDX 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_GET-IDX 'DEFINED-ON-LINE '603) 
(PUT 'TALP_GET-IDX 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GET-IDX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_GET-IDX (LAST VEC)
    (PROG (FOUND POS)
      (SETQ POS 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (LEQ POS (UPBV VEC)) (NOT FOUND))) (RETURN NIL)))
        (COND
         ((ATOM LAST)
          (COND ((EQ (GETV VEC POS) LAST) (SETQ FOUND T))
                (T (SETQ POS (PLUS POS 1)))))
         ((PAIRP (CAR LAST))
          ((LAMBDA (X)
             (COND
              ((AND (PAIRP (CAR X))
                    (EQ (TALP_INVF (CAR X)) (TALP_INVF (CAR LAST)))
                    (EQ (TALP_INVN (CAR X)) (TALP_INVN (CAR LAST))))
               (SETQ FOUND T))
              (T (SETQ POS (PLUS POS 1)))))
           (GETV VEC POS)))
         ((EQ (CAR (GETV VEC POS)) (CAR LAST)) (SETQ FOUND T))
         (T (SETQ POS (PLUS POS 1))))
        (GO WHILELABEL))
      (RETURN (COND (FOUND POS) (T (MINUS 1)))))) 
(PUT 'TALP_GET-MINFCT 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_GET-MINFCT 'DEFINED-ON-LINE '622) 
(PUT 'TALP_GET-MINFCT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GET-MINFCT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_GET-MINFCT (IDX FV VV CV)
    ((LAMBDA (X)
       (COND ((TALP_INVP X) (CONS (CAR X) (LIST (GETV VV 0))))
             (T
              (CONS (CAR X)
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND ((MINUSP (DIFFERENCE (CDR X) I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (GETV CV 0) NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE (CDR X) I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (GETV CV 0) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))
     (GETV FV IDX))) 
(PUT 'TALP_TRYQEGAUSS 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_TRYQEGAUSS 'DEFINED-ON-LINE '635) 
(PUT 'TALP_TRYQEGAUSS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_TRYQEGAUSS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TRYQEGAUSS (BVAR MTRX ANSP)
    (PROG (GAUSS)
      (COND
       (*RLVERBOSE
        (IOTO_PRIN2 (LIST "+ try gauss elimination for " BVAR " ... "))))
      (SETQ GAUSS (TALP_TRYGAUSS BVAR MTRX ANSP))
      (COND
       ((EQ GAUSS 'FAILED)
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_PRIN2T "failed")
            (IOTO_PRIN2
             (LIST "  try gauss elimination for " BVAR
                   " with transformed input formula ... "))
            NIL)))
         (SETQ MTRX (TALP_RNF MTRX))
         (SETQ GAUSS (TALP_TRYGAUSS BVAR MTRX ANSP)))))
      (COND
       ((NEQ GAUSS 'FAILED)
        (PROGN (COND (*RLVERBOSE (IOTO_PRIN2T "succeeded"))) (RETURN GAUSS))))
      (RETURN 'FAILED))) 
(PUT 'TALP_TRYGAUSS 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_TRYGAUSS 'DEFINED-ON-LINE '663) 
(PUT 'TALP_TRYGAUSS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_TRYGAUSS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TRYGAUSS (BV F ANSP)
    (PROG (TMP ELIMSET)
      (SETQ TMP (TALP_TRYGAUSSVAR BV F ANSP))
      (COND ((OR (EQ TMP 'FAILED) (EQ TMP 'IGNORE)) (RETURN 'FAILED)))
      (SETQ ELIMSET
              (COND ((AND (LISTP TMP) (NULL (ATOM (CAR TMP)))) TMP)
                    (T (LIST TMP))))
      (RETURN (TALP_TRYGAUSS1 ELIMSET F ANSP)))) 
(PUT 'TALP_TRYGAUSS1 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_TRYGAUSS1 'DEFINED-ON-LINE '676) 
(PUT 'TALP_TRYGAUSS1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_TRYGAUSS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TRYGAUSS1 (ES F ANSP)
    (PROG (SUBPAIR RESULT ANSWER TMP STOP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ES (NULL STOP))) (RETURN NIL)))
        (PROGN
         (SETQ SUBPAIR (CAR ES))
         (SETQ TMP (TALP_TRY (CL_SUBFOF (LIST SUBPAIR) F)))
         (COND
          ((EQ TMP 'TRUE)
           (PROGN
            (COND (ANSP (SETQ ANSWER SUBPAIR)))
            (SETQ RESULT TMP)
            (SETQ STOP T)))
          ((NEQ TMP 'FALSE)
           (PROGN
            (COND
             (ANSP
              (PROGN
               (SETQ ANSWER (CONS SUBPAIR ANSWER))
               (SETQ RESULT (CONS TMP RESULT))))
             (T
              (PROGN
               (SETQ RESULT
                       (CL_SIMPL (COND (RESULT (LIST 'OR RESULT TMP)) (T TMP))
                                 NIL (MINUS 1)))
               (COND ((EQ RESULT 'TRUE) (SETQ STOP T)))))))))
         (SETQ ES (CDR ES)))
        (GO WHILELABEL))
      (COND ((NULL RESULT) (SETQ RESULT 'FALSE)))
      (RETURN (CONS ANSWER RESULT)))) 
(PUT 'TALP_TRYGAUSSVAR 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_TRYGAUSSVAR 'DEFINED-ON-LINE '706) 
(PUT 'TALP_TRYGAUSSVAR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_TRYGAUSSVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TRYGAUSSVAR (BV F ANSP)
    (COND ((TALP_ATFP F) (TALP_QESOLSET BV F))
          ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
           (TALP_GAUSSAND BV (CDR F) ANSP))
          ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
           (TALP_GAUSSOR BV (CDR F) ANSP))
          (T 'FAILED))) 
(PUT 'TALP_QESOLSET 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_QESOLSET 'DEFINED-ON-LINE '721) 
(PUT 'TALP_QESOLSET 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_QESOLSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_QESOLSET (V ATF)
    (PROG (SUBS LHS RHS)
      (SETQ LHS (CADR ATF))
      (SETQ RHS (CADDR ATF))
      (COND
       ((NOT (OR (TALP_CONTAINS LHS V) (TALP_CONTAINS RHS V)))
        (RETURN 'IGNORE)))
      (COND
       ((NEQ (COND ((ATOM ATF) ATF) (T (CAR ATF))) 'EQUAL) (RETURN 'FAILED)))
      (COND
       ((AND (TALP_CONTAINS LHS V) (TALP_CONTAINS RHS V)) (RETURN 'FAILED)))
      (COND ((AND (NEQ LHS V) (NEQ RHS V)) (RETURN 'FAILED)))
      (SETQ SUBS (COND ((EQ LHS V) RHS) (T LHS)))
      (RETURN (CONS V SUBS)))) 
(PUT 'TALP_GAUSSAND 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_GAUSSAND 'DEFINED-ON-LINE '741) 
(PUT 'TALP_GAUSSAND 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GAUSSAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_GAUSSAND (V FL ANSP)
    (PROG (W CURR STOP)
      (SETQ CURR (TALP_TRYGAUSSVAR V (CAR FL) ANSP))
      (COND
       ((OR (EQ CURR 'FAILED) (EQ CURR 'IGNORE))
        (PROGN
         (SETQ FL (CDR FL))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND FL (NOT STOP))) (RETURN NIL)))
           (PROGN
            (SETQ W (TALP_TRYGAUSSVAR V (CAR FL) ANSP))
            (COND
             ((AND (NEQ W 'FAILED) (NEQ W 'IGNORE))
              (PROGN (SETQ STOP T) (SETQ CURR W))))
            (SETQ FL (CDR FL)))
           (GO WHILELABEL)))))
      (RETURN (COND (CURR CURR) (T 'FAILED))))) 
(PUT 'TALP_GAUSSOR 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_GAUSSOR 'DEFINED-ON-LINE '760) 
(PUT 'TALP_GAUSSOR 'DEFINED-IN-FILE 'REDLOG/TALP/TALPQE.RED) 
(PUT 'TALP_GAUSSOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_GAUSSOR (V FL ANSP)
    (PROG (W CURR STOP)
      (SETQ CURR (TALP_TRYGAUSSVAR V (CAR FL) ANSP))
      (COND
       ((NEQ CURR 'FAILED)
        (PROGN
         (SETQ FL (CDR FL))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND FL (NOT STOP))) (RETURN NIL)))
           (PROGN
            (SETQ W (TALP_TRYGAUSSVAR V (CAR FL) ANSP))
            (COND ((EQ W 'FAILED) (PROGN (SETQ CURR NIL) (SETQ STOP T) NIL))
                  ((NEQ W 'IGNORE)
                   (SETQ CURR
                           (COND ((NEQ CURR 'IGNORE) (CONS W (LIST CURR)))
                                 (T W)))))
            (SETQ FL (CDR FL))
            NIL)
           (GO WHILELABEL)))))
      (RETURN (COND (CURR CURR) (T 'FAILED))))) 
(ENDMODULE) 