(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ZMODULE)) 
(FLUID
 '(*GALOIS *TRA *TRFIELD *TRMIN BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS
   COMMONDEN GAUSSIANI LISTOFALLSQRTS LISTOFNEWSQRTS SQRT-PLACES-ALIST
   TAYLORASSLIST)) 
(EXPORTS (LIST 'ZMODULE)) 
(IMPORTS
 (LIST '*MULTF 'SQRTSINSQL 'SORTSQRTS 'SIMP '*Q2F 'ACTUALSIMPSQRT 'PRINTSF)) 
(IMPORTS (LIST 'PREPF 'SUBSTITUTESQ 'PRINTSQ 'MAPPLY '*MULTSQ 'MKILIST)) 
(IMPORTS (LIST 'MKVECF2Q 'MKVEC 'MKIDENM 'INVSQ 'MULTSQ 'NEGSQ 'ADDSQ 'GCDN)) 
(IMPORTS (LIST '*INVSQ 'PREPSQ)) 
(PUT 'ZMODULE 'NUMBER-OF-ARGS 1) 
(PUT 'ZMODULE 'DEFINED-ON-LINE '49) 
(PUT 'ZMODULE 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'ZMODULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZMODULE (W)
    (PROG (RESLIST DENLIST U COMMONDEN BASIS P1 P2 HCF)
      (PROG (V)
        (SETQ V W)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ U (CDR V))
            (SETQ RESLIST (CONS U RESLIST))
            (SETQ DENLIST (CONS (CDR U) DENLIST))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ BASIS (SQRTSINSQL RESLIST NIL))
      (COND ((OR (NULL U) (NULL (CDR U)) *GALOIS) (GO NOCHANGE)))
      (SETQ RESLIST (CHECK-SQRTS-DEPENDENCE RESLIST BASIS))
      (SETQ DENLIST
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U RESLIST)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CDR U)) (CAR U)) NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (CDR U)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
     NOCHANGE
      (SETQ COMMONDEN
              (CONS
               (MAPPLY
                (FUNCTION
                 (LAMBDA (U V)
                   ((LAMBDA (G587)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF U G587))
                            (T (POLY-MULTF U G587))))
                    (QUOTF-FAIL V (GCDF U V)))))
                DENLIST)
               1))
      (SETQ U NIL)
      (PROG (V)
        (SETQ V RESLIST)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ U (CONS (CAR (*MULTSQ V COMMONDEN)) U))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ RESLIST U)
      (SETQ U (BEXPRN RESLIST))
      (SETQ BASIS (CAR U))
      (SETQ RESLIST (CDR U))
      (SETQ DENLIST NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT BASIS) (RETURN NIL)))
        (PROGN
         (SETQ P1 RESLIST)
         (SETQ P2 W)
         (SETQ U NIL)
         (SETQ HCF 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT P1) (RETURN NIL)))
           (PROGN
            (COND
             ((NEQ 0 (CAAR P1))
              (PROGN
               (SETQ U (CONS (CONS (CAAR P2) (CAAR P1)) U))
               (SETQ HCF (GCDN HCF (CAAR P1))))))
            (SETQ P1 (CDR P1))
            (SETQ P2 (CDR P2)))
           (GO WHILELABEL))
         (COND
          ((NEQ HCF 1)
           (SETQ U
                   (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                     (SETQ UU U)
                     (COND ((NULL UU) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (UU)
                                         (CONS (CAR UU)
                                               (QUOTIENT (CDR UU) HCF)))
                                       (CAR UU))
                                      NIL)))
                    LOOPLABEL
                     (SETQ UU (CDR UU))
                     (COND ((NULL UU) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (UU)
                                 (CONS (CAR UU) (QUOTIENT (CDR UU) HCF)))
                               (CAR UU))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (SETQ DENLIST
                 (CONS
                  (CONS
                   (PREPSQ
                    (*MULTSQ (CAR BASIS)
                             (MULTSQ (CONS HCF 1) (*INVSQ COMMONDEN))))
                   U)
                  DENLIST))
         (SETQ BASIS (CDR BASIS))
         (SETQ RESLIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J RESLIST)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (GO WHILELABEL))
      (RETURN DENLIST))) 
(PUT 'BEXPRN 'NUMBER-OF-ARGS 1) 
(PUT 'BEXPRN 'DEFINED-ON-LINE '100) 
(PUT 'BEXPRN 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'BEXPRN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BEXPRN (WLIST)
    (PROG (BASIS REPLIST W W2 W3 P1 P2)
      (SETQ W (REVERSE WLIST))
      (SETQ REPLIST NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (PROGN
         (SETQ W2 (SF2DF (CAR W)))
         (SETQ W3 W2)
         (PROG ()
          WHILELABEL
           (COND ((NOT W3) (RETURN NIL)))
           (PROGN
            (COND
             ((NOT (MEMBER (CAAR W3) BASIS))
              (PROGN
               (SETQ BASIS (CONS (CAAR W3) BASIS))
               (SETQ REPLIST (MAPCONS REPLIST 0)))))
            (SETQ W3 (CDR W3)))
           (GO WHILELABEL))
         (SETQ REPLIST (CONS (MKILIST BASIS 0) REPLIST))
         (SETQ W3 W2)
         (PROG ()
          WHILELABEL
           (COND ((NOT W3) (RETURN NIL)))
           (PROGN
            (SETQ P1 BASIS)
            (SETQ P2 (CAR REPLIST))
            (PROG ()
             WHILELABEL
              (COND ((NOT P1) (RETURN NIL)))
              (PROGN
               (COND ((EQUAL (CAAR W3) (CAR P1)) (RPLACA P2 (CDAR W3))))
               (SETQ P1 (CDR P1))
               (SETQ P2 (CDR P2)))
              (GO WHILELABEL))
            (SETQ W3 (CDR W3)))
           (GO WHILELABEL))
         (SETQ W (CDR W)))
        (GO WHILELABEL))
      (RETURN (MKBASIS BASIS REPLIST)))) 
(PUT 'MKBASIS 'NUMBER-OF-ARGS 2) 
(PUT 'MKBASIS 'DEFINED-ON-LINE '136) 
(PUT 'MKBASIS 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'MKBASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKBASIS (BASIS RESLIST)
    (PROG (ROW NBASIS NRESLIST U V)
      (SETQ BASIS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U BASIS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CONS U 1)) (CAR U)) NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (CONS U 1)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NRESLIST (MKILIST RESLIST NIL))
     TRYNEWLOOP
      (SETQ ROW (MAPOVERCAR RESLIST))
      (SETQ RESLIST
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J RESLIST)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((OBVINDEP ROW NRESLIST) (SETQ U NIL))
            (T (SETQ U (LINDEP ROW NRESLIST))))
      (COND
       (U
        (PROGN
         (SETQ V NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT NBASIS) (RETURN NIL)))
           (PROGN
            (SETQ V
                    (CONS (ADDSQ (CAR NBASIS) (*MULTSQ (CAR BASIS) (CAR U)))
                          V))
            (SETQ NBASIS (CDR NBASIS))
            (SETQ U (CDR U)))
           (GO WHILELABEL))
         (SETQ NBASIS (REVERSIP V))))
       (T
        (PROGN
         (SETQ NRESLIST (PAIR ROW NRESLIST))
         (SETQ NBASIS (CONS (CAR BASIS) NBASIS)))))
      (SETQ BASIS (CDR BASIS))
      (COND (BASIS (GO TRYNEWLOOP)))
      (RETURN (CONS NBASIS NRESLIST)))) 
(PUT 'OBVINDEP 'NUMBER-OF-ARGS 2) 
(PUT 'OBVINDEP 'DEFINED-ON-LINE '172) 
(PUT 'OBVINDEP 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'OBVINDEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OBVINDEP (ROW MATRX)
    (PROG (U)
      (COND ((NULL (CAR MATRX)) (RETURN T)))
     NEXTTRY
      (COND ((NULL ROW) (RETURN NIL)))
      (COND ((IEQUAL 0 (CAR ROW)) (GO NOUSE)))
      (SETQ U (CAR MATRX))
     TESTLOOP
      (COND ((NEQ 0 (CAR U)) (GO NOUSE)))
      (SETQ U (CDR U))
      (COND (U (GO TESTLOOP)))
      (RETURN T)
     NOUSE
      (SETQ ROW (CDR ROW))
      (SETQ MATRX (CDR MATRX))
      (GO NEXTTRY))) 
(PUT 'SF2DF 'NUMBER-OF-ARGS 1) 
(PUT 'SF2DF 'DEFINED-ON-LINE '199) 
(PUT 'SF2DF 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'SF2DF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SF2DF (SF)
    (COND ((NULL SF) NIL) ((NUMBERP SF) (CONS (CONS 1 SF) NIL))
          (T
           (PROG (A B C)
             (SETQ A (SF2DF (CDAR SF)))
             (SETQ B (CONS (CONS (CAAR SF) 1) NIL))
             (PROG ()
              WHILELABEL
               (COND ((NOT A) (RETURN NIL)))
               (PROGN
                (SETQ C (CONS (CONS (*MULTF (CAAR A) B) (CDAR A)) C))
                (SETQ A (CDR A)))
               (GO WHILELABEL))
             (RETURN (NCONC C (SF2DF (CDR SF)))))))) 
(PUT 'CHECK-SQRTS-DEPENDENCE 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK-SQRTS-DEPENDENCE 'DEFINED-ON-LINE '218) 
(PUT 'CHECK-SQRTS-DEPENDENCE 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'CHECK-SQRTS-DEPENDENCE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK-SQRTS-DEPENDENCE (SQL SQRTL)
    (PROG (*GALOIS SUBLIST SQRTSAVELIST CHANGEFLAG)
      (SETQ SQRTSAVELIST (CONS LISTOFALLSQRTS LISTOFNEWSQRTS))
      (SETQ LISTOFNEWSQRTS (LIST (CAAAR GAUSSIANI)))
      (SETQ LISTOFALLSQRTS (LIST (CONS (CADR (CAAAR GAUSSIANI)) GAUSSIANI)))
      (SETQ *GALOIS T)
      (PROG (U)
        (SETQ U (SORTSQRTS SQRTL NIL))
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROG (V UU)
             (SETQ UU (*Q2F (SIMP (CADR U))))
             (SETQ V (ACTUALSIMPSQRT UU))
             (SETQ LISTOFALLSQRTS (CONS (CONS UU V) LISTOFALLSQRTS))
             (COND
              ((OR (OR (ATOM V) (ATOM (CAR V))) (NEQ (CAAAR V) U))
               (PROGN
                (COND
                 ((OR *TRA *TRFIELD)
                  (PROGN
                   (PROGN (PRIN2 U) (TERPRI) U)
                   (PROGN (PRIN2 "re-expressed as") (TERPRI) "re-expressed as")
                   (PRINTSF V))))
                (SETQ V (PREPF V))
                (SETQ SUBLIST (CONS (CONS U V) SUBLIST))
                (SETQ CHANGEFLAG T))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (COND
       (CHANGEFLAG
        (PROGN
         (SETQ SQL
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U SQL)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (SUBSTITUTESQ U SUBLIST))
                                     (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (U) (SUBSTITUTESQ U SUBLIST)) (CAR U))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ TAYLORASSLIST NIL)
         (SETQ SQRT-PLACES-ALIST NIL)
         (SETQ BASIC-LISTOFALLSQRTS LISTOFALLSQRTS)
         (SETQ BASIC-LISTOFNEWSQRTS LISTOFNEWSQRTS)
         (COND
          ((OR *TRA *TRMIN)
           (PROGN
            (PROGN
             (PRIN2 "New set of residues are")
             (TERPRI)
             "New set of residues are")
            (MAPC SQL (FUNCTION PRINTSQ)))))))
       (T
        (PROGN
         (SETQ LISTOFALLSQRTS (CAR SQRTSAVELIST))
         (SETQ LISTOFNEWSQRTS (CDR SQRTSAVELIST)))))
      (RETURN SQL))) 
(PUT 'LINDEP 'NUMBER-OF-ARGS 2) 
(PUT 'LINDEP 'DEFINED-ON-LINE '261) 
(PUT 'LINDEP 'DEFINED-IN-FILE 'ALGINT/ZMODULE.RED) 
(PUT 'LINDEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINDEP (ROW MATRX)
    (PROG (M M1 N U V INVERSE ROWSINUSE FAILURE)
      (SETQ M1 (LENGTH (CAR MATRX)))
      (SETQ M (ISUB1 M1))
      (SETQ N (ISUB1 (LENGTH MATRX)))
      (SETQ ROW (MKVECF2Q ROW))
      (SETQ MATRX
              (MKVEC
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J MATRX)
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (MKVECF2Q J)) (CAR J))
                                       NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (MKVECF2Q J)) (CAR J)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ INVERSE (MKIDENM M1))
      (SETQ ROWSINUSE (MKVECT M))
      (SETQ FAILURE T)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROG ()
          (SETQ U NIL)
          (PROG (J)
            (SETQ J 0)
           LAB
            (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
            (PROGN
             (COND
              ((AND (NULL U) (NULL (GETV ROWSINUSE J))
                    (CAR (GETV (GETV MATRX I) J)))
               (SETQ U J))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (COND ((NULL U) (GO NULLU)))
          (PUTV ROWSINUSE U T)
          (COND ((IEQUAL U M) (GO NONETOKILL)))
          (PROG (J)
            (SETQ J (IADD1 U))
           LAB
            (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
            (COND
             ((CAR (GETV (GETV MATRX I) J))
              (PROGN
               (SETQ V
                       (NEGSQ
                        (MULTSQ (GETV (GETV MATRX I) J)
                                (INVSQ (GETV (GETV MATRX I) U)))))
               (PROG (K)
                 (SETQ K 0)
                LAB
                 (COND ((MINUSP (DIFFERENCE M1 K)) (RETURN NIL)))
                 (PUTV (GETV INVERSE K) J
                       (ADDSQ (GETV (GETV INVERSE K) J)
                              (MULTSQ V (GETV (GETV INVERSE K) U))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               (PROG (K)
                 (SETQ K 0)
                LAB
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                 (PUTV (GETV MATRX K) J
                       (ADDSQ (GETV (GETV MATRX K) J)
                              (MULTSQ V (GETV (GETV MATRX K) U))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB)))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
         NONETOKILL
          (COND ((NULL (CAR (GETV ROW I))) (GO NOROWOP)))
          (SETQ V
                  (NEGSQ
                   (MULTSQ (GETV ROW I) (INVSQ (GETV (GETV MATRX I) U)))))
          (PROG (K)
            (SETQ K 0)
           LAB
            (COND ((MINUSP (DIFFERENCE M1 K)) (RETURN NIL)))
            (PUTV (GETV INVERSE K) M1
                  (ADDSQ (GETV (GETV INVERSE K) M1)
                         (MULTSQ V (GETV (GETV INVERSE K) U))))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (PROG (K)
            (SETQ K 0)
           LAB
            (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
            (PUTV ROW K
                  (ADDSQ (GETV ROW K) (MULTSQ V (GETV (GETV MATRX K) U))))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (SETQ U NIL)
          (PROG (K)
            (SETQ K 0)
           LAB
            (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
            (COND ((CAR (GETV ROW K)) (SETQ U T)))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (COND ((NULL U) (PROGN (SETQ N (MINUS 1)) (SETQ FAILURE NIL))))
         NOROWOP
          (COND
           (*TRA
            (PROGN
             (PRINC "At end of cycle")
             (PROGN (PRIN2 ROW) (TERPRI) ROW)
             (PROGN (PRIN2 MATRX) (TERPRI) MATRX)
             (PROGN (PRIN2 INVERSE) (TERPRI) INVERSE))))
          (RETURN NIL)
         NULLU
          (COND ((CAR (GETV ROW I)) (SETQ N (MINUS 1)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND (FAILURE (RETURN NIL)))
      (SETQ V NIL)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
        (SETQ V (CONS (NEGSQ (GETV (GETV INVERSE (DIFFERENCE M I)) M1)) V))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN V))) 
(ENDMODULE) 