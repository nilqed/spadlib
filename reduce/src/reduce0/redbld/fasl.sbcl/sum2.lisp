(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUM2)) 
(FLUID '(*TRSUM)) 
(FLUID '(SUM_LAST_ATTEMPT_RULES*)) 
(SWITCH (LIST 'TRSUM)) 
(PUT 'SIMP-SUM0 'NUMBER-OF-ARGS 2) 
(PUT 'SIMP-SUM0 'DEFINED-ON-LINE '48) 
(PUT 'SIMP-SUM0 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SIMP-SUM0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMP-SUM0 (U Y)
    (PROG (V UPPER LOWER LOWER1 DIF)
      (COND
       ((NOT (ATOM (CDR Y)))
        (PROGN
         (SETQ LOWER (CADR Y))
         (SETQ LOWER1
                 (COND ((NUMBERP LOWER) (DIFFERENCE LOWER 1))
                       (T (LIST 'PLUS LOWER (MINUS 1)))))
         (SETQ UPPER (COND ((NOT (ATOM (CDDR Y))) (CADDR Y)) (T (CAR Y))))
         (SETQ DIF (ADDSQ (SIMP* UPPER) (NEGSQ (SIMP* LOWER))))
         (COND
          ((EQUAL (CDR DIF) 1)
           (COND
            ((NULL (CAR DIF))
             (RETURN (SUBSQ U (LIST (CONS (*A2K (CAR Y)) UPPER)))))
            ((FIXP (CAR DIF)) (SETQ DIF (CAR DIF))) (T (SETQ DIF NIL))))
          (T (SETQ DIF NIL)))
         (COND ((AND DIF (LEQ DIF 0)) (RETURN (CONS NIL 1))))
         (COND ((ATOM (CDDR Y)) (SETQ UPPER NIL))))))
      (SETQ V (*A2K (CAR Y)))
      (RETURN (SIMP-SUM1 U V Y UPPER LOWER LOWER1 DIF)))) 
(PUT 'SIMP-SUM1 'NUMBER-OF-ARGS 7) 
(PUT 'SIMP-SUM1 'DEFINED-ON-LINE '68) 
(PUT 'SIMP-SUM1 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SIMP-SUM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SIMP-SUM1 (U V Y UPPER LOWER LOWER1 DIF)
    (PROG (W LST X Z FLG)
      (SETQ LST (SUM-SPLIT-LOG U V))
      (SETQ W (CAR LST))
      (SETQ LST (CDR LST))
      (SETQ U (CONS NIL 1))
     A
      (COND ((NULL W) (GO B)))
      (SETQ X
              (MULTSQ (CAAR W)
                      (SIMP-PROD1 (CDAR W) V Y UPPER LOWER LOWER1 DIF)))
      (SETQ U (ADDSQ U (SIMP* (LIST 'LOG (PREPSQ X)))))
      (SETQ W (CDR W))
      (GO A)
     B
      (COND ((NULL LST) (RETURN U)))
      (SETQ FLG NIL)
      (SETQ Z (CAR LST))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "Summation ")
         (SQPRINT Z)
         (PRIN2* " w.r.t ")
         (XPRINF (LIST (CONS (CONS V 1) 1)) NIL NIL)
         (TERPRI* T))))
      (SETQ W (SUM-SQ Z V))
      (COND
       ((EQUAL W 'FAILED)
        (PROGN
         (COND
          (*TRSUM
           (PROGN (PRIN2* "UMM-SQ failed. Trying SUM-TRIG") (TERPRI* T))))
         (SETQ W (SUM-TRIG Z V))
         (COND
          ((EQUAL W 'FAILED)
           (PROGN
            (COND (*TRSUM (PROGN (PRIN2* "SUM-TRIG failed.") (TERPRI* T))))
            (SETQ W (SUM-UNKNOWN Z V Y LOWER DIF))
            (SETQ FLG (CAR W))
            (SETQ W (CDR W))))))))
      (COND (*TRSUM (PROGN (PRIN2* "Result  = ") (SQPRINT W) (TERPRI* T))))
      (COND (FLG (GO C)))
      (COND
       (UPPER
        (SETQ W
                (ADDSQ (SUBSQ W (LIST (CONS V UPPER)))
                       (NEGSQ (SUBSQ W (LIST (CONS V LOWER1)))))))
       (LOWER (SETQ W (ADDSQ W (NEGSQ (SUBSQ W (LIST (CONS V LOWER1))))))))
     C
      (SETQ U (ADDSQ U W))
      (SETQ LST (CDR LST))
      (GO B))) 
(GLOBAL '(*TRIG-TO-EXP)) 
(PUT 'SUM-TRIG 'NUMBER-OF-ARGS 2) 
(PUT 'SUM-TRIG 'DEFINED-ON-LINE '127) 
(PUT 'SUM-TRIG 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-TRIG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUM-TRIG (U V)
    (PROG (LST W)
      (SETQ *TRIG-TO-EXP NIL)
      (SETQ U (TRIG-TO-EXPSQ U V))
      (COND ((NOT *TRIG-TO-EXP) (RETURN 'FAILED)))
      (SETQ LST (SUM-TERM-SPLIT U V))
      (SETQ U (CONS NIL 1))
     A
      (COND ((NULL LST) (RETURN (EXP-TO-TRIGSQ U))))
      (SETQ W (SUM-SQ (CAR LST) V))
      (COND ((EQUAL W 'FAILED) (RETURN 'FAILED)))
      (SETQ U (ADDSQ U W))
      (SETQ LST (CDR LST))
      (GO A))) 
(SETQ SUM_LAST_ATTEMPT_RULES*
        (PROGN
         (AEVAL
          (LIST 'LIST
                (LIST 'REPLACEBY
                      (LIST 'SUM (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'G))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'PLUS (LIST 'SUM 'F 'N 'ANF 'ENDE)
                                  (LIST 'SUM 'G 'N 'ANF 'ENDE))
                            (LIST 'OR
                                  (LIST 'NEQ
                                        (LIST 'PART
                                              (LIST 'SUM 'F 'N 'ANF 'ENDE) 0)
                                        'SUM)
                                  (LIST 'NEQ
                                        (LIST 'PART
                                              (LIST 'SUM 'G 'N 'ANF 'ENDE) 0)
                                        'SUM))))
                (LIST 'REPLACEBY
                      (LIST 'SUM
                            (LIST 'QUOTIENT
                                  (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'G))
                                  (LIST '~ 'DD))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'PLUS
                                  (LIST 'SUM (LIST 'QUOTIENT 'F 'DD) 'N 'ANF
                                        'ENDE)
                                  (LIST 'SUM (LIST 'QUOTIENT 'G 'DD) 'N 'ANF
                                        'ENDE))
                            (LIST 'OR
                                  (LIST 'NEQ
                                        (LIST 'PART
                                              (LIST 'SUM
                                                    (LIST 'QUOTIENT 'F 'DD) 'N
                                                    'ANF 'ENDE)
                                              0)
                                        'SUM)
                                  (LIST 'NEQ
                                        (LIST 'PART
                                              (LIST 'SUM
                                                    (LIST 'QUOTIENT 'G 'DD) 'N
                                                    'ANF 'ENDE)
                                              0)
                                        'SUM))))
                (LIST 'REPLACEBY
                      (LIST 'SUM (LIST 'TIMES (LIST '~ 'C) (LIST '~ 'F))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN (LIST 'TIMES 'C (LIST 'SUM 'F 'N 'ANF 'ENDE))
                            (LIST 'AND (LIST 'FREEOF 'C 'N) (LIST 'NEQ 'C 1))))
                (LIST 'REPLACEBY
                      (LIST 'SUM (LIST 'QUOTIENT (LIST '~ 'C) (LIST '~ 'F))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'TIMES 'C
                                  (LIST 'SUM (LIST 'QUOTIENT 1 'F) 'N 'ANF
                                        'ENDE))
                            (LIST 'AND (LIST 'FREEOF 'C 'N) (LIST 'NEQ 'C 1))))
                (LIST 'REPLACEBY
                      (LIST 'SUM
                            (LIST 'TIMES (LIST '~ 'C)
                                  (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G)))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'TIMES 'C
                                  (LIST 'SUM (LIST 'QUOTIENT 'F 'G) 'N 'ANF
                                        'ENDE))
                            (LIST 'AND (LIST 'FREEOF 'C 'N)
                                  (LIST 'NEQ 'C 1)))))))) 
(PUT 'SUM-UNKNOWN 'NUMBER-OF-ARGS 5) 
(PUT 'SUM-UNKNOWN 'DEFINED-ON-LINE '164) 
(PUT 'SUM-UNKNOWN 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-UNKNOWN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUM-UNKNOWN (U V Y LOWER DIF)
    (PROG (Z W)
      (COND
       ((NULL DIF)
        (PROGN
         (SETQ Z (CONS 'SUM (CONS (PREPSQ U) (LIST (CAR Y)))))
         (COND ((SETQ W (OPMTCH Z)) (RETURN (CONS NIL (SIMP W))))
               ((NULL (CDR Y)) (RETURN (CONS T (MKSQ Z 1)))))
         (SETQ Z (CONS 'SUM (CONS (PREPSQ U) Y)))
         (LET (LIST SUM_LAST_ATTEMPT_RULES*))
         (SETQ W (OPMTCH Z))
         (RULE-LIST (LIST SUM_LAST_ATTEMPT_RULES*) NIL)
         (RETURN (CONS T (COND (W (SIMP W)) (T (MKSQ Z 1))))))))
      (SETQ Z (CONS NIL 1))
     A
      (COND ((LESSP DIF 0) (RETURN (CONS T Z))))
      (SETQ Z (ADDSQ Z (SUBSQ U (LIST (CONS V (LIST 'PLUS LOWER DIF))))))
      (SETQ DIF (DIFFERENCE DIF 1))
      (GO A))) 
(PUT 'SUM-SUBST 'NUMBER-OF-ARGS 3) 
(PUT 'SUM-SUBST 'DEFINED-ON-LINE '184) 
(PUT 'SUM-SUBST 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-SUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUM-SUBST (U X A)
    (COND ((EQUAL U X) A) ((ATOM U) U)
          (T (CONS (SUM-SUBST (CAR U) X A) (SUM-SUBST (CDR U) X A))))) 
(PUT 'SUM-DF 'NUMBER-OF-ARGS 2) 
(PUT 'SUM-DF 'DEFINED-ON-LINE '189) 
(PUT 'SUM-DF 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUM-DF (U Y)
    (PROG (W Z UPPER LOWER DIF)
      (SETQ DIF NIL)
      (COND
       ((EQUAL (LENGTH Y) 3)
        (PROGN
         (SETQ LOWER (CADR Y))
         (SETQ UPPER (CADDR Y))
         (SETQ DIF (ADDSQ (SIMP* UPPER) (NEGSQ (SIMP* LOWER))))
         (COND
          ((EQUAL (CDR DIF) 1)
           (COND
            ((NULL (CAR DIF)) (RETURN (SIMP* (SUM-SUBST U (CAR Y) UPPER))))
            ((FIXP (CAR DIF)) (SETQ DIF (CAR DIF))) (T (SETQ DIF NIL))))
          (T (SETQ DIF NIL)))
         (COND ((AND DIF (LEQ DIF 0)) (RETURN (CONS NIL 1)))))))
      (COND
       ((NULL DIF)
        (PROGN
         (SETQ Z (CONS 'SUM (CONS U Y)))
         (LET (LIST SUM_LAST_ATTEMPT_RULES*))
         (SETQ W (OPMTCH Z))
         (RULE-LIST (LIST SUM_LAST_ATTEMPT_RULES*) NIL)
         (RETURN (COND (W (SIMP W)) (T (MKSQ Z 1)))))))
      (SETQ Z (CONS NIL 1))
     A
      (COND ((LESSP DIF 0) (RETURN Z)))
      (SETQ Z (ADDSQ Z (SIMP* (SUM-SUBST U (CAR Y) (LIST 'PLUS LOWER DIF)))))
      (SETQ DIF (DIFFERENCE DIF 1))
      (GO A))) 
(PUT 'SUM-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'SUM-SQ 'DEFINED-ON-LINE '221) 
(PUT 'SUM-SQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUM-SQ (U V)
    (PROG (GN FN PN RN QN Z K X)
      (COND ((NULL (CAR U)) (RETURN (CONS NIL 1))))
      (SETQ X (SETKORDER (LIST V)))
      (SETQ Z (REORDER (CAR U)))
      (SETQ U (CONS Z (REORDER (CDR U))))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T " *** Summation by Gosper's algorithm ***")
         (PRIN2* "    A(n) = ")
         (SQPRINT U)
         (TERPRI* T)
         (TERPRI* T))))
      (COND
       ((OR (OR (ATOM Z) (ATOM (CAR Z))) (NOT (EQ (CAAAR Z) V)) (CDR Z))
        (PROGN (SETQ PN 1) (SETQ Z U)))
       (T
        (PROGN
         (SETQ PN (LIST (CONS (CAAR Z) 1)))
         (SETQ Z (CONS (CDAR Z) (CDR U))))))
      (SETQ Z (MULTSQ Z (INVSQ (NSUBSQ Z V (MINUS 1)))))
      (SETQ GN (GCDF* (CAR Z) (CDR Z)))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "A(n)/A(n-1) = ")
         (SQPRINT Z)
         (TERPRI* T)
         (PRIN2* "GN = ")
         (XPRINF GN NIL NIL)
         (TERPRI* T))))
      (SETQ QN (QUOTF* (CAR Z) GN))
      (SETQ RN (QUOTF* (CDR Z) GN))
      (COND ((OR (NONPOLYP QN V) (NONPOLYP RN V)) (GO FAIL)))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "Initial qn, rn and pn are ")
         (TERPRI* T)
         (PRIN2* "QN = ")
         (XPRINF QN NIL NIL)
         (TERPRI* T)
         (PRIN2* "RN = ")
         (XPRINF RN NIL NIL)
         (TERPRI* T)
         (PRIN2* "PN = ")
         (XPRINF PN NIL NIL)
         (TERPRI* T))))
      (SETQ K (COMPRESS (EXPLODE '+J)))
      (SETQ Z (INTEGER-ROOT (RESULTANT QN (NSUBSF RN V K) V) K))
      (COND
       (*TRSUM
        (PROGN (PRIN2 "Root of resultant(q(n),r(n+j)) are ") (PRIN2T Z))))
      (PROG ()
       WHILELABEL
        (COND ((NOT Z) (RETURN NIL)))
        (PROGN
         (SETQ K (CAR Z))
         (SETQ GN (GCDF* QN (NSUBSF RN V K)))
         (SETQ QN (QUOTF* QN GN))
         (SETQ RN (QUOTF* RN (NSUBSF GN V (MINUS K))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (GEQ (SETQ K (DIFFERENCE K 1)) 0)) (RETURN NIL)))
           (SETQ PN
                   ((LAMBDA (G127)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF PN G127))
                            (T (POLY-MULTF PN G127))))
                    (NSUBSF GN V (MINUS K))))
           (GO WHILELABEL))
         (SETQ Z (CDR Z)))
        (GO WHILELABEL))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "Shift free  qn, rn and pn are")
         (TERPRI* T)
         (PRIN2* "QN = ")
         (XPRINF QN NIL NIL)
         (TERPRI* T)
         (PRIN2* "RN = ")
         (XPRINF RN NIL NIL)
         (TERPRI* T)
         (PRIN2* "PN = ")
         (XPRINF PN NIL NIL)
         (TERPRI* T))))
      (SETQ QN (NSUBSF QN V 1))
      (COND
       ((LESSP (SETQ K (DEGREE-BOUND PN (ADDF QN RN) (ADDF QN (NEGF RN)) V)) 0)
        (GO FAIL)))
      (COND (*TRSUM (PROGN (PRIN2 "DEGREE BOUND is ") (PRIN2T K))))
      (COND ((NOT (SETQ FN (SOLVE-FN K PN QN RN V))) (GO FAIL)))
      (COND (*TRSUM (PROGN (PRIN2* "FN = ") (SQPRINT FN) (TERPRI* T))))
      (SETQ U (MULTSQ (MULTSQ (CONS QN 1) FN) (MULTSQ U (CONS 1 PN))))
      (SETQ Z (GCDF* (CAR U) (CDR U)))
      (SETQ U (CONS (QUOTF* (CAR U) Z) (QUOTF* (CDR U) Z)))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T " *** Gosper's algorithm completed ***")
         (PRIN2* "    S(n) = ")
         (SQPRINT U)
         (TERPRI* T)
         (TERPRI* T))))
      (SETKORDER X)
      (RETURN (CONS (REORDER (CAR U)) (REORDER (CDR U))))
     FAIL
      (COND
       (*TRSUM
        (PROGN (PRIN2T " *** Gosper's algorithm failed ***") (TERPRI* T))))
      (SETKORDER X)
      (RETURN 'FAILED))) 
(PUT 'INTEGER-ROOT 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGER-ROOT 'DEFINED-ON-LINE '297) 
(PUT 'INTEGER-ROOT 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'INTEGER-ROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGER-ROOT (U V)
    (PROG (X ROOT N W)
      (SETQ X (SETKORDER (LIST V)))
      (SETQ U (REORDER U))
      (COND ((OR (OR (ATOM U) (ATOM (CAR U))) (NOT (EQ (CAAAR U) V))) (GO A)))
      (SETQ U (CAR (CANCEL (CONS U (CDAR U)))))
      (SETQ W U)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM W) (ATOM (CAR W)))) (EQ (CAAAR W) V) (CDR W)))
          (RETURN NIL)))
        (SETQ W (CDR W))
        (GO WHILELABEL))
      (COND
       ((GREATERP (SETQ N (DEGR W V)) 0)
        (PROGN
         (SETQ W (CDAR W))
         (PROG ()
          WHILELABEL
           (COND ((NOT (GREATERP N 0)) (RETURN NIL)))
           (PROGN (SETQ ROOT (CONS 0 ROOT)) (SETQ N (DIFFERENCE N 1)))
           (GO WHILELABEL)))))
      (SETQ N (DFACTORS (LOWCOEF W)))
      (SETQ W (CONS (CONS V 1) 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT N) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (TESTDIVIDE U V (CAR N)))
           (PROGN
            (SETQ ROOT (CONS (CAR N) ROOT))
            (SETQ U (QUOTF* U (CONS W (MINUS (CAR N)))))))
          (T (SETQ N (CDR N)))))
        (GO WHILELABEL))
     A
      (SETKORDER X)
      (RETURN ROOT))) 
(PUT 'LOWCOEF 'NUMBER-OF-ARGS 1) 
(PUT 'LOWCOEF 'DEFINED-ON-LINE '325) 
(PUT 'LOWCOEF 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'LOWCOEF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOWCOEF (U)
    (PROG (LST M)
      (SETQ LST (DCOEFL U))
      (SETQ M 0)
     A
      (COND ((NULL LST) (RETURN M)))
      (SETQ M (GCDN M (CAR LST)))
      (COND ((EQUAL M 1) (RETURN 1)))
      (SETQ LST (CDR LST))
      (GO A))) 
(PUT 'DCOEFL 'NUMBER-OF-ARGS 1) 
(PUT 'DCOEFL 'DEFINED-ON-LINE '338) 
(PUT 'DCOEFL 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DCOEFL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DCOEFL (U)
    (COND
     ((OR (ATOM U) (ATOM (CAR U))) (COND ((FIXP U) (LIST (ABS U))) (T NIL)))
     (T (NCONC (DCOEFL (CDAR U)) (DCOEFL (CDR U)))))) 
(PUT 'TESTDIVIDE 'NUMBER-OF-ARGS 3) 
(PUT 'TESTDIVIDE 'DEFINED-ON-LINE '342) 
(PUT 'TESTDIVIDE 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TESTDIVIDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TESTDIVIDE (U V N)
    (PROG (X)
     A
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NOT (EQ (CAAAR U) V)))
        (RETURN (ADDF U X))))
      (SETQ X (ADDF (MULTD (EXPT N (CDAAR U)) (CDAR U)) X))
      (COND ((SETQ U (CDR U)) (GO A)))
      (RETURN X))) 
(PUT 'DEGREE-BOUND 'NUMBER-OF-ARGS 4) 
(PUT 'DEGREE-BOUND 'DEFINED-ON-LINE '356) 
(PUT 'DEGREE-BOUND 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DEGREE-BOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEGREE-BOUND (PN U V KERN)
    (PROG (LP L+ L- X M K)
      (SETQ X (SETKORDER (LIST KERN)))
      (SETQ U (REORDER U))
      (SETQ V (REORDER V))
      (SETQ PN (REORDER PN))
      (SETQ L+ (COND (U (DEGR U KERN)) (T (MINUS 1))))
      (SETQ L- (COND (V (DEGR V KERN)) (T (MINUS 1))))
      (SETQ LP (COND (PN (DEGR PN KERN)) (T (MINUS 1))))
      (COND ((LEQ L+ L-) (PROGN (SETQ K (DIFFERENCE LP L-)) (GO A))))
      (SETQ K (PLUS (DIFFERENCE LP L+) 1))
      (COND ((GREATERP L+ 0) (SETQ U (CDAR U))))
      (COND ((GREATERP L- 0) (SETQ V (CDAR V))))
      (COND
       ((AND (EQUAL L+ (PLUS L- 1))
             (FIXP (SETQ M (QUOTF1 (MULTD (MINUS 2) V) U))))
        (SETQ K (MAX M K)))
       ((EQUAL LP L-) (SETQ K (MAX K 0))))
     A
      (SETKORDER X)
      (RETURN K))) 
(PUT 'SOLVE-FN 'NUMBER-OF-ARGS 5) 
(PUT 'SOLVE-FN 'DEFINED-ON-LINE '385) 
(PUT 'SOLVE-FN 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SOLVE-FN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-FN (K PN QN RN V)
    (PROG (I FN X Y Z U W C CLST FLST)
      (SETQ C (MAKEVAR 'C 0))
      (SETQ CLST (LIST C))
      (SETQ FN (LIST (CONS (CONS C 1) 1)))
      (SETQ I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ (SETQ I (PLUS I 1)) K)) (RETURN NIL)))
        (PROGN
         (SETQ C (MAKEVAR 'C I))
         (SETQ CLST (CONS C CLST))
         (SETQ FN (CONS (CONS (CONS V I) (LIST (CONS (CONS C 1) 1))) FN)))
        (GO WHILELABEL))
      (SETQ Z
              (ADDF PN
                    (ADDF
                     (NEGF
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF QN FN))
                            (T (POLY-MULTF QN FN))))
                     ((LAMBDA (G129)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF RN G129))
                              (T (POLY-MULTF RN G129))))
                      (NSUBSF FN V (MINUS 1))))))
      (SETQ X (SETKORDER (CONS V CLST)))
      (SETQ Z (REORDER Z))
      (SETQ C CLST)
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "C Equation is")
         (TERPRI* T)
         (XPRINF Z NIL NIL)
         (TERPRI* T))))
     A
      (COND
       ((OR (OR (ATOM Z) (ATOM (CAR Z)))
            ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
             (SETQ Y (COND ((EQ (CAAAR Z) V) (CDAR Z)) (T Z)))))
        (GO FAIL)))
      (SETQ W (CAAAR Y))
      (COND ((NOT (MEMQ W CLST)) (GO FAIL)))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "C Equation to solve is ")
         (XPRINF Y NIL NIL)
         (TERPRI* T)
         (PRIN2* " w.r.t ")
         (XPRINF (LIST (CONS (CONS W 1) 1)) NIL NIL)
         (TERPRI* T))))
      (SETQ U (GCDF* (CDR Y) (CDAR Y)))
      (SETQ U (CONS (QUOTF* (NEGF (CDR Y)) U) (QUOTF* (CDAR Y) U)))
      (SETQ FLST (CONS (CONS W U) FLST))
      (SETQ Z (SUBST-CN Z W U))
      (COND
       (*TRSUM
        (PROGN
         (XPRINF (LIST (CONS (CONS W 1) 1)) NIL NIL)
         (PRIN2* " := ")
         (SQPRINT U)
         (TERPRI* T))))
      (SETQ CLST (DELETEQ CLST W))
      (COND (Z (GO A)))
      (SETKORDER C)
      (SETQ FN (REORDER FN))
      (SETQ U 1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (ATOM FN) (ATOM (CAR FN)))) (MEMQ (CAAAR FN) C)))
          (RETURN NIL)))
        (PROGN
         (SETQ W (CAAAR FN))
         (SETQ Z (ATSOC W FLST))
         (SETQ FN (SUBST-CN FN W (COND (Z (CDR Z)))))
         (COND
          (Z
           (SETQ U
                   ((LAMBDA (G131)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF U G131))
                            (T (POLY-MULTF U G131))))
                    (CDR (CDR Z))))))
         (SETQ Z (GCDF* FN U))
         (SETQ FN (QUOTF* FN Z))
         (SETQ U (QUOTF* U Z)))
        (GO WHILELABEL))
      (SETKORDER X)
      (RETURN (CANCEL (CONS (REORDER FN) (REORDER U))))
     FAIL
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T "Fail to solve C equation.")
         (PRIN2* "Z := ")
         (XPRINF Z NIL NIL)
         (TERPRI* T))))
      (SETKORDER X)
      (RETURN NIL))) 
(PUT 'SUBST-CN 'NUMBER-OF-ARGS 3) 
(PUT 'SUBST-CN 'DEFINED-ON-LINE '444) 
(PUT 'SUBST-CN 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUBST-CN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBST-CN (U V X)
    (PROG (Z)
      (SETQ Z (SETKORDER (LIST V)))
      (SETQ U (REORDER U))
      (COND
       ((AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (EQ (CAAAR U) V))
        (COND
         (X
          (SETQ U
                  (ADDF
                   ((LAMBDA (G133)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) G133))
                            (T (POLY-MULTF (CDAR U) G133))))
                    (REORDER (CAR X)))
                   ((LAMBDA (G135)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR U) G135))
                            (T (POLY-MULTF (CDR U) G135))))
                    (REORDER (CDR X))))))
         (T (SETQ U (CDR U))))))
      (SETKORDER Z)
      (RETURN (REORDER U)))) 
(PUT 'MAKEVAR 'NUMBER-OF-ARGS 2) 
(PUT 'MAKEVAR 'DEFINED-ON-LINE '458) 
(PUT 'MAKEVAR 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'MAKEVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKEVAR (ID N) (COMPRESS (NCONC (EXPLODE ID) (EXPLODE N)))) 
(PUT 'DELETEQ 'NUMBER-OF-ARGS 2) 
(PUT 'DELETEQ 'DEFINED-ON-LINE '461) 
(PUT 'DELETEQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DELETEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DELETEQ (U X)
    (COND ((NULL U) NIL) ((EQ (CAR U) X) (CDR U))
          (T (CONS (CAR U) (DELETEQ (CDR U) X))))) 
(PUT 'NSUBSF 'NUMBER-OF-ARGS 3) 
(PUT 'NSUBSF 'DEFINED-ON-LINE '467) 
(PUT 'NSUBSF 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'NSUBSF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NSUBSF (U KERN I)
    (PROG (X Y Z N)
      (COND ((OR (NULL I) (EQUAL I 0)) (RETURN U)))
      (SETQ X (SETKORDER (LIST KERN)))
      (SETQ U (REORDER U))
      (SETQ Y
              (ADDF (LIST (CONS (CONS KERN 1) 1))
                    (COND ((FIXP I) I) (T (LIST (CONS (CONS I 1) 1))))))
      (SETQ Z NIL)
     A
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NOT (EQ (CAAAR U) KERN))) (GO B)))
      (SETQ Z (ADDF Z (CDAR U)))
      (SETQ N (DIFFERENCE (DEGR U KERN) (DEGR (CDR U) KERN)))
      (SETQ U (CDR U))
     A1
      (COND ((LEQ N 0) (GO A)))
      (SETQ Z (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z Y)) (T (POLY-MULTF Z Y))))
      (SETQ N (DIFFERENCE N 1))
      (GO A1)
     B
      (SETQ Z (ADDF Z U))
      (SETKORDER X)
      (RETURN (REORDER Z)))) 
(PUT 'NSUBSQ 'NUMBER-OF-ARGS 3) 
(PUT 'NSUBSQ 'DEFINED-ON-LINE '496) 
(PUT 'NSUBSQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'NSUBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NSUBSQ (U KERN I) (SUBSQ U (LIST (CONS KERN (LIST 'PLUS KERN I))))) 
(PUT 'NONPOLYP 'NUMBER-OF-ARGS 2) 
(PUT 'NONPOLYP 'DEFINED-ON-LINE '508) 
(PUT 'NONPOLYP 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'NONPOLYP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONPOLYP (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (OR (AND (NOT (EQ (CAAAR U) V)) (DEPEND-P (CAAAR U) V))
               (NONPOLYP (CDAR U) V) (NONPOLYP (CDR U) V))))) 
(PUT 'DEPEND-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'DEPEND-SQ 'DEFINED-ON-LINE '515) 
(PUT 'DEPEND-SQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DEPEND-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPEND-SQ (U V) (OR (DEPEND-F (CAR U) V) (DEPEND-F (CDR U) V))) 
(PUT 'DEPEND-F 'NUMBER-OF-ARGS 2) 
(PUT 'DEPEND-F 'DEFINED-ON-LINE '519) 
(PUT 'DEPEND-F 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DEPEND-F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPEND-F (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (OR (DEPEND-P (CAAAR U) V) (DEPEND-F (CDAR U) V)
               (DEPEND-F (CDR U) V))))) 
(PUT 'DEPEND-P 'NUMBER-OF-ARGS 2) 
(PUT 'DEPEND-P 'DEFINED-ON-LINE '526) 
(PUT 'DEPEND-P 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DEPEND-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPEND-P (U V)
    (COND ((EQ U V) T) ((ATOM U) NIL) ((NOT (ATOM (CAR U))) (DEPEND-F U V))
          ((EQ (CAR U) '*SQ) (DEPEND-SQ (CADR U) V)) (T (DEPEND-L (CDR U) V)))) 
(PUT 'DEPEND-L 'NUMBER-OF-ARGS 2) 
(PUT 'DEPEND-L 'DEFINED-ON-LINE '533) 
(PUT 'DEPEND-L 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'DEPEND-L 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPEND-L (U V)
    (COND ((NULL U) NIL) ((DEPEND-SQ (SIMP (CAR U)) V) T)
          (T (DEPEND-L (CDR U) V)))) 
(PUT 'SUM-TERM-SPLIT 'NUMBER-OF-ARGS 2) 
(PUT 'SUM-TERM-SPLIT 'DEFINED-ON-LINE '543) 
(PUT 'SUM-TERM-SPLIT 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'SUM-TERM-SPLIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUM-TERM-SPLIT (U V)
    (PROG (Y Z KLST LST X)
      (SETQ X (SETKORDER (LIST V)))
      (SETQ Z (QREMF (REORDER (CAR U)) (SETQ Y (REORDER (CDR U)))))
      (SETQ KLST (KERN-LIST (CAR Z) V))
      (SETQ LST (TERMLST (CAR Z) (CONS 1 1) KLST))
      (SETQ KLST (KERN-LIST (CDR Z) V))
      (COND ((DEPEND-F Y V) (SETQ KLST (DELETEQ KLST V))))
      (SETQ LST (APPEND LST (TERMLST (CDR Z) (CONS 1 Y) KLST)))
      (SETKORDER X)
      (RETURN LST))) 
(PUT 'KERN-LIST 'NUMBER-OF-ARGS 2) 
(PUT 'KERN-LIST 'DEFINED-ON-LINE '557) 
(PUT 'KERN-LIST 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'KERN-LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KERN-LIST (U V)
    (PROG (X)
      (PROG (J)
        (SETQ J (KERNELS U))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (COND ((DEPEND-P J V) (SETQ X (CONS J X))))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN X))) 
(PUT 'TERMLST 'NUMBER-OF-ARGS 3) 
(PUT 'TERMLST 'DEFINED-ON-LINE '564) 
(PUT 'TERMLST 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TERMLST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMLST (U V KLST)
    (PROG (X KERN LST)
      (COND ((NULL U) (RETURN NIL))
            ((OR (NULL KLST) (OR (ATOM U) (ATOM (CAR U))))
             (RETURN (LIST (MULTSQ V (CONS U 1))))))
      (SETQ KERN (CAR KLST))
      (SETQ KLST (CDR KLST))
      (SETQ X (SETKORDER (LIST KERN)))
      (SETQ U (REORDER U))
      (SETQ V (CONS (REORDER (CAR V)) (REORDER (CDR V))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (ATOM U) (ATOM (CAR U)))) (EQ (CAAAR U) KERN)))
          (RETURN NIL)))
        (PROGN
         (SETQ LST
                 (NCONC
                  (TERMLST (CDAR U)
                   (MULTSQ (CONS (LIST (CONS (CAAR U) 1)) 1) V) KLST)
                  LST))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (COND (U (SETQ LST (NCONC (TERMLST U V KLST) LST))))
      (SETKORDER X)
      (RETURN LST))) 
(PUT 'TRIG-TO-EXPSQ 'NUMBER-OF-ARGS 2) 
(PUT 'TRIG-TO-EXPSQ 'DEFINED-ON-LINE '589) 
(PUT 'TRIG-TO-EXPSQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TRIG-TO-EXPSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIG-TO-EXPSQ (U V)
    (MULTSQ (TRIG-TO-EXPF (CAR U) V) (INVSQ (TRIG-TO-EXPF (CDR U) V)))) 
(PUT 'TRIG-TO-EXPF 'NUMBER-OF-ARGS 2) 
(PUT 'TRIG-TO-EXPF 'DEFINED-ON-LINE '594) 
(PUT 'TRIG-TO-EXPF 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TRIG-TO-EXPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIG-TO-EXPF (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (CONS U 1))
          (T
           (ADDSQ (MULTSQ (TRIG-TO-EXPP (CAAR U) V) (TRIG-TO-EXPF (CDAR U) V))
                  (TRIG-TO-EXPF (CDR U) V))))) 
(PUT 'TRIG-TO-EXPP 'NUMBER-OF-ARGS 2) 
(PUT 'TRIG-TO-EXPP 'DEFINED-ON-LINE '601) 
(PUT 'TRIG-TO-EXPP 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TRIG-TO-EXPP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIG-TO-EXPP (U V)
    (PROG (*COMBINEEXPT W X Z N WI)
      (SETQ N (CDR U))
      (SETQ Z (CAR U))
      (COND
       ((OR (ATOM Z) (NOT (ATOM (SETQ X (CAR Z)))) (NOT (DEPEND-P Z V)))
        (RETURN (CONS (LIST (CONS U 1)) 1))))
      (COND
       ((MEMQ X '(SIN COS TAN SEC COSEC COT))
        (PROGN
         (SETQ *TRIG-TO-EXP T)
         (SETQ W
                 (MULTSQ (CONS (LIST (CONS (CONS 'I 1) 1)) 1)
                         (SIMP* (CADR Z))))
         (SETQ W (SIMP* (LIST 'EXPT 'E (MK*SQ W))))
         (SETQ WI (INVSQ W))
         (COND
          ((EQ X 'SIN)
           (SETQ W
                   (MULTSQ (ADDSQ W (NEGSQ WI))
                           (CONS 1 (LIST (CONS (CONS 'I 1) 2))))))
          ((EQ X 'COS) (SETQ W (MULTSQ (ADDSQ W WI) (CONS 1 2))))
          ((EQ X 'TAN)
           (SETQ W (MULTSQ (ADDSQ W (NEGSQ WI)) (INVSQ (ADDSQ W WI)))))
          ((EQ X 'SEC) (SETQ W (MULTSQ (CONS 2 1) (INVSQ (ADDSQ W WI)))))
          ((EQ X 'COSEC)
           (SETQ W
                   (MULTSQ (LIST (CONS (CONS 'I 1) 2))
                           (INVSQ (ADDSQ W (NEGSQ WI))))))
          (T (SETQ W (MULTSQ (ADDSQ W WI) (INVSQ (ADDSQ W (NEGSQ WI)))))))))
       ((MEMQ X '(SINH COSH TANH SECH COSECH COTH))
        (PROGN
         (SETQ *TRIG-TO-EXP T)
         (SETQ W (SIMP* (LIST 'EXPT 'E (CADR Z))))
         (SETQ WI (INVSQ W))
         (COND ((EQ X 'SINH) (SETQ W (MULTSQ (ADDSQ W (NEGSQ WI)) (CONS 1 2))))
               ((EQ X 'COSH) (SETQ W (MULTSQ (ADDSQ W WI) (CONS 1 2))))
               ((EQ X 'TANH)
                (SETQ W (MULTSQ (ADDSQ W (NEGSQ WI)) (INVSQ (ADDSQ W WI)))))
               ((EQ X 'SECH) (SETQ W (MULTSQ (CONS 2 1) (INVSQ (ADDSQ W WI)))))
               ((EQ X 'COSECH)
                (SETQ W (MULTSQ (CONS 2 1) (INVSQ (ADDSQ W (NEGSQ WI))))))
               (T
                (SETQ W (MULTSQ (ADDSQ W WI) (INVSQ (ADDSQ W (NEGSQ WI)))))))))
       (T (RETURN (CONS (LIST (CONS U 1)) 1))))
      (RETURN (EXPTSQ W N)))) 
(PUT 'EXP-TO-TRIGSQ 'NUMBER-OF-ARGS 1) 
(PUT 'EXP-TO-TRIGSQ 'DEFINED-ON-LINE '662) 
(PUT 'EXP-TO-TRIGSQ 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'EXP-TO-TRIGSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXP-TO-TRIGSQ (U)
    (MULTSQ (EXP-TO-TRIGF (CAR U)) (INVSQ (EXP-TO-TRIGF (CDR U))))) 
(PUT 'EXP-TO-TRIGF 'NUMBER-OF-ARGS 1) 
(PUT 'EXP-TO-TRIGF 'DEFINED-ON-LINE '666) 
(PUT 'EXP-TO-TRIGF 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'EXP-TO-TRIGF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXP-TO-TRIGF (U)
    (PROG (V V1 X Y N)
      (SETQ U (TERMLST1 U 1 (CONS NIL 1)))
      (SETQ V NIL)
     A
      (COND ((NULL U) (GO B)))
      (SETQ X (CAAR U))
      (SETQ Y (CDAR U))
      (SETQ U (CDR U))
     A1
      (COND
       ((AND U (EQUAL Y (CDAR U)))
        (PROGN (SETQ X (ADDF X (CAAR U))) (SETQ U (CDR U)) (GO A1))))
      (SETQ V (CONS (CONS X Y) V))
      (GO A)
     B
      (SETQ V1 (REVERSE V))
      (SETQ N (LENGTH V))
      (SETQ U (CONS NIL 1))
     C
      (COND ((EQUAL N 0) (RETURN U))
            ((EQUAL N 1)
             (RETURN
              (ADDSQ U
                     (MULTSQ (CONS (CAAR V) 1)
                             (SIMP* (LIST 'EXPT 'E (MK*SQ (CDAR V)))))))))
      (SETQ U (ADDSQ U (EXP-TO-TRIGL (CAAR V1) (CAAR V) (CDAR V1) (CDAR V))))
      (SETQ V (CDR V))
      (SETQ V1 (CDR V1))
      (SETQ N (DIFFERENCE N 2))
      (GO C))) 
(PUT 'EXP-TO-TRIGL 'NUMBER-OF-ARGS 4) 
(PUT 'EXP-TO-TRIGL 'DEFINED-ON-LINE '699) 
(PUT 'EXP-TO-TRIGL 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'EXP-TO-TRIGL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXP-TO-TRIGL (A B C D)
    (PROG (X Y Z)
      (SETQ X (CONS (ADDF A B) 1))
      (SETQ Y (CONS (ADDF A (NEGF B)) 1))
      (SETQ Z (MULTSQ (ADDSQ C (NEGSQ D)) (CONS 1 2)))
      (SETQ Z (REAL-IMAG-SINCOS Z))
      (RETURN
       (MULTSQ (SIMP* (LIST 'EXPT 'E (MK*SQ (MULTSQ (ADDSQ C D) (CONS 1 2)))))
               (ADDSQ (MULTSQ X (CDR Z)) (MULTSQ Y (CAR Z))))))) 
(PUT 'TERMLST1 'NUMBER-OF-ARGS 3) 
(PUT 'TERMLST1 'DEFINED-ON-LINE '717) 
(PUT 'TERMLST1 'DEFINED-IN-FILE 'SUM/SUM2.RED) 
(PUT 'TERMLST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMLST1 (U V W)
    (PROG (X Y)
      (COND ((NULL U) (RETURN NIL))
            ((OR (ATOM U) (ATOM (CAR U)))
             (RETURN
              (LIST
               (CONS
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF U V)) (T (POLY-MULTF U V)))
                W)))))
      (SETQ X (CAAAR U))
      (SETQ Y
              (COND
               ((OR (ATOM X) (NOT (EQ (CAR X) 'EXPT)) (NOT (EQ (CADR X) 'E)))
                (TERMLST1 (CDAR U)
                 ((LAMBDA (G136)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF G136 V))
                          (T (POLY-MULTF G136 V))))
                  (LIST (CONS (CAAR U) 1)))
                 W))
               (T
                (TERMLST1 (CDAR U) V
                 (ADDSQ W (MULTSQ (SIMP* (CADDR X)) (CONS (CDAAR U) 1)))))))
      (RETURN (NCONC Y (TERMLST1 (CDR U) V W))))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(LET
 '((LIST
    (REPLACEBY (SUM (QUOTIENT (SIN (TIMES (~ N) (~ TT))) N) (~ N) 1 INFINITY)
     (QUOTIENT (DIFFERENCE PI TT) 2))
    (REPLACEBY
     (SUM (QUOTIENT (SIN (TIMES (~ N) (~ TT))) (EXPT (~ N) 3)) (~ N) 1
      INFINITY)
     (PLUS
      (DIFFERENCE (TIMES (EXPT PI 2) (QUOTIENT TT 6))
                  (TIMES PI (QUOTIENT (EXPT TT 2) 4)))
      (QUOTIENT (EXPT TT 3) 12)))
    (REPLACEBY
     (SUM (QUOTIENT (SIN (TIMES (~ N) (~ TT))) (EXPT (~ N) 5)) (~ N) 1
      INFINITY)
     (PLUS
      (DIFFERENCE (TIMES (EXPT PI 4) (QUOTIENT TT 90))
                  (TIMES (EXPT PI 2) (QUOTIENT (EXPT TT 3) 36)))
      (DIFFERENCE (TIMES PI (QUOTIENT (EXPT TT 4) 48))
                  (QUOTIENT (EXPT TT 5) 240))))))) 
(LET
 '((LIST
    (REPLACEBY
     (SUM (QUOTIENT (COS (TIMES (~ N) (~ TT))) (~ N)) (~ N) 1 INFINITY)
     (MINUS (LOG (TIMES 2 (SIN (QUOTIENT TT 2))))))
    (REPLACEBY
     (SUM (QUOTIENT (COS (TIMES (~ N) (~ TT))) (EXPT (~ N) 2)) (~ N) 1
      INFINITY)
     (PLUS (DIFFERENCE (QUOTIENT (EXPT PI 2) 6) (TIMES PI (QUOTIENT TT 2)))
           (QUOTIENT (EXPT TT 2) 4)))
    (REPLACEBY
     (SUM (QUOTIENT (COS (TIMES (~ N) (~ TT))) (EXPT (~ N) 4)) (~ N) 1
      INFINITY)
     (PLUS
      (DIFFERENCE (QUOTIENT (EXPT PI 4) 90)
                  (TIMES (EXPT PI 2) (QUOTIENT (EXPT TT 2) 12)))
      (DIFFERENCE (TIMES PI (QUOTIENT (EXPT TT 3) 12))
                  (QUOTIENT (EXPT TT 4) 48))))))) 
(ENDMODULE) 