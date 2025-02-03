(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PROD)) 
(FLUID '(*TRSUM PROD_LAST_ATTEMPT_RULES*)) 
(PUT 'SIMP-PROD 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROD 'DEFINED-ON-LINE '37) 
(PUT 'SIMP-PROD 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'SIMP-PROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROD (U)
    (PROG (V Y UPPER LOWER LOWER1 DIF)
      (COND
       ((NULL U) (RERROR 'SUM 1 (LIST "Wrong number of arguments to prod"))))
      (SETQ Y (CDR U))
      (SETQ U (SIMP* (CAR U)))
      (COND ((NULL (CAR U)) (RETURN (CONS 1 1))) ((ATOM Y) (RETURN U)))
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
         (COND ((AND DIF (LEQ DIF 0)) (RETURN (CONS 1 1))))
         (COND ((ATOM (CDDR Y)) (SETQ UPPER NIL))))))
      (SETQ V (*A2K (CAR Y)))
      (RETURN (SIMP-PROD1 U V Y UPPER LOWER LOWER1 DIF)))) 
(PUT 'SIMP-PROD1 'NUMBER-OF-ARGS 7) 
(PUT 'SIMP-PROD1 'DEFINED-ON-LINE '68) 
(PUT 'SIMP-PROD1 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'SIMP-PROD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SIMP-PROD1 (U V Y UPPER LOWER LOWER1 DIF)
    (PROG (W LST X Z FLG)
      (SETQ LST (PROD-SPLIT-EXP U V))
      (SETQ W (CAR LST))
      (SETQ LST (CDR LST))
      (SETQ U (CONS 1 1))
     A
      (COND ((NULL W) (GO B)))
      (SETQ X (SIMP-SUM1 (CDAR W) V Y UPPER LOWER LOWER1 DIF))
      (SETQ U
              (MULTSQ U
                      ((LAMBDA (U)
                         (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U))
                               (T (BASIC-SIMPEXPT U))))
                       (LIST (CAAR W) (PREPSQ X)))))
      (SETQ W (CDR W))
      (GO A)
     B
      (COND ((NULL LST) (RETURN U)))
      (SETQ FLG NIL)
      (SETQ Z (CAR LST))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2* "Product ")
         (SQPRINT Z)
         (PRIN2* " w.r.t ")
         (XPRINF (LIST (CONS (CONS V 1) 1)) NIL NIL)
         (TERPRI* T))))
      (SETQ W (PROD-SQ Z V))
      (COND
       ((EQUAL W 'FAILED)
        (PROGN
         (COND (*TRSUM (PROGN (PRIN2* "PROD-SQ failed.") (TERPRI* T))))
         (SETQ W (PROD-UNKNOWN Z V Y LOWER DIF))
         (SETQ FLG (CAR W))
         (SETQ W (CDR W)))))
      (COND (*TRSUM (PROGN (PRIN2* "Result  = ") (SQPRINT W) (TERPRI* T))))
      (COND (FLG (GO C)))
      (COND
       (UPPER
        (SETQ W
                (MULTSQ (SUBSQ W (LIST (CONS V UPPER)))
                        (INVSQ (SUBSQ W (LIST (CONS V LOWER1)))))))
       (LOWER (SETQ W (MULTSQ W (INVSQ (SUBSQ W (LIST (CONS V LOWER1))))))))
     C
      (SETQ U (MULTSQ U W))
      (SETQ LST (CDR LST))
      (GO B))) 
(PUT 'PROD 'SIMPFN 'SIMP-PROD) 
(PUT 'PROD-UNKNOWN 'NUMBER-OF-ARGS 5) 
(PUT 'PROD-UNKNOWN 'DEFINED-ON-LINE '117) 
(PUT 'PROD-UNKNOWN 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'PROD-UNKNOWN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROD-UNKNOWN (U V Y LOWER DIF)
    (PROG (Z W UU)
      (COND
       ((NULL DIF)
        (PROGN
         (SETQ Z (CONS 'PROD (CONS (PREPSQ U) (LIST (CAR Y)))))
         (COND ((SETQ W (OPMTCH Z)) (RETURN (CONS NIL (SIMP W))))
               ((NULL (CDR Y)) (RETURN (CONS T (MKSQ Z 1)))))
         (LOAD_PACKAGE (LIST 'FACTOR))
         (SETQ UU (OLD_FACTORIZE (PREPF (CAR U))))
         (COND
          ((GREATERP (LENGTH UU) 2)
           (PROGN
            (SETQ Z
                    (CONS 'TIMES
                          (PROG (UUU FORALL-RESULT FORALL-ENDPTR)
                            (SETQ UUU (CDR UU))
                            (COND ((NULL UUU) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (UUU)
                                                (CONS 'PROD
                                                      (CONS
                                                       (PREPSQ
                                                        (COND
                                                         ((AND (PAIRP UUU)
                                                               (EQ (CAR UUU)
                                                                   '*SQ))
                                                          (CADR UUU))
                                                         (T (SIMP UUU))))
                                                       Y)))
                                              (CAR UUU))
                                             NIL)))
                           LOOPLABEL
                            (SETQ UUU (CDR UUU))
                            (COND ((NULL UUU) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (UUU)
                                        (CONS 'PROD
                                              (CONS
                                               (PREPSQ
                                                (COND
                                                 ((AND (PAIRP UUU)
                                                       (EQ (CAR UUU) '*SQ))
                                                  (CADR UUU))
                                                 (T (SIMP UUU))))
                                               Y)))
                                      (CAR UUU))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
            (SETQ Z (LIST 'QUOTIENT Z (CONS 'PROD (CONS (PREPF (CDR U)) Y))))
            (RETURN (CONS T (SIMP Z))))))
         (SETQ Z (CONS 'PROD (CONS (PREPSQ U) Y)))
         (LET (LIST PROD_LAST_ATTEMPT_RULES*))
         (SETQ W (OPMTCH Z))
         (RULE-LIST (LIST PROD_LAST_ATTEMPT_RULES*) NIL)
         (RETURN (CONS T (COND (W (SIMP W)) (T (MKSQ Z 1))))))))
      (SETQ Z (CONS 1 1))
     A
      (COND ((LESSP DIF 0) (RETURN (CONS T Z))))
      (SETQ Z (MULTSQ Z (SUBSQ U (LIST (CONS V (LIST 'PLUS LOWER DIF))))))
      (SETQ DIF (DIFFERENCE DIF 1))
      (GO A))) 
(SETQ PROD_LAST_ATTEMPT_RULES*
        (PROGN
         (AEVAL
          (LIST 'LIST
                (LIST 'REPLACEBY
                      (LIST 'PROD (LIST 'TIMES (LIST '~ 'F) (LIST '~ 'G))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'TIMES (LIST 'PROD 'F 'N 'ANF 'ENDE)
                                  (LIST 'PROD 'G 'N 'ANF 'ENDE))
                            (LIST 'AND (LIST 'NEQ 'G 1)
                                  (LIST 'OR
                                        (LIST 'NUMBERP
                                              (LIST 'PROD 'F 'N 'ANF 'ENDE))
                                        (LIST 'NEQ
                                              (LIST 'PART
                                                    (LIST 'PROD 'F 'N 'ANF
                                                          'ENDE)
                                                    0)
                                              'PROD)
                                        (LIST 'NEQ
                                              (LIST 'PART
                                                    (LIST 'PROD 'G 'N 'ANF
                                                          'ENDE)
                                                    0)
                                              'PROD)))))
                (LIST 'REPLACEBY
                      (LIST 'PROD (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'QUOTIENT (LIST 'PROD 'F 'N 'ANF 'ENDE)
                                  (LIST 'PROD 'G 'N 'ANF 'ENDE))
                            (LIST 'AND (LIST 'NEQ 'G 1)
                                  (LIST 'OR
                                        (LIST 'NUMBERP
                                              (LIST 'PROD 'F 'N 'ANF 'ENDE))
                                        (LIST 'NEQ
                                              (LIST 'PART
                                                    (LIST 'PROD 'F 'N 'ANF
                                                          'ENDE)
                                                    0)
                                              'PROD)
                                        (LIST 'NEQ
                                              (LIST 'PART
                                                    (LIST 'PROD 'G 'N 'ANF
                                                          'ENDE)
                                                    0)
                                              'PROD)))))
                (LIST 'REPLACEBY
                      (LIST 'PROD (LIST 'EXPT (LIST '~ 'F) (LIST '~ 'K))
                            (LIST '~ 'N) (LIST '~ 'ANF) (LIST '~ 'ENDE))
                      (LIST 'WHEN
                            (LIST 'PROG (LIST 'II 'FORALL-RESULT)
                                  (LIST 'SETQ 'II 1)
                                  (LIST 'SETQ 'FORALL-RESULT 1) 'LAB1
                                  (LIST 'COND
                                        (LIST
                                         (LIST '|AMINUSP:|
                                               (LIST 'LIST ''DIFFERENCE
                                                     (LIST 'AEVAL* ''K) 'II))
                                         (LIST 'RETURN 'FORALL-RESULT)))
                                  (LIST 'SETQ 'FORALL-RESULT
                                        (LIST 'AEVAL*
                                              (LIST 'LIST ''TIMES
                                                    (LIST 'AEVAL*
                                                          (LIST 'LIST ''PROD
                                                                ''F ''N ''ANF
                                                                ''ENDE))
                                                    'FORALL-RESULT)))
                                  (LIST 'SETQ 'II
                                        (LIST
                                         (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                               (LIST 'AEVAL*
                                                     (LIST 'LIST ''PLUS
                                                           'FORALL-RESULT 1)))
                                         'II))
                                  (LIST 'GO 'LAB1))
                            (LIST 'NEQ
                                  (LIST 'PART (LIST 'PROD 'F 'N 'ANF 'ENDE) 0)
                                  'PROD))))))) 
(AEVAL (LET '((REPLACEBY (PROD (~ N) N 1 (~ K)) (FACTORIAL K))))) 
(AEVAL 'NIL) 
(PUT 'PROD-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'PROD-SQ 'DEFINED-ON-LINE '180) 
(PUT 'PROD-SQ 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'PROD-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROD-SQ (U V)
    (PROG (GN P1N P2N RN QN Z K X Y)
      (COND ((NULL (CAR U)) (RETURN (CONS 1 1))))
      (SETQ X (SETKORDER (LIST V)))
      (SETQ QN (REORDER (CAR U)))
      (SETQ RN (REORDER (CDR U)))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T " *** Product of A(n) = qn/rn with ***")
         (PRIN2* "QN = ")
         (XPRINF QN NIL NIL)
         (TERPRI* T)
         (PRIN2* "RN = ")
         (XPRINF RN NIL NIL)
         (TERPRI* T))))
      (COND ((OR (NONPOLYP QN V) (NONPOLYP RN V)) (GO FAIL)))
      (SETQ K (COMPRESS (EXPLODE '+J)))
      (SETQ Z (INTEGER-ROOT2 (RESULTANT QN (NSUBSF RN V K) V) K))
      (COND
       (*TRSUM
        (PROGN (PRIN2 "Root of resultant(q(n),r(n+j)) are ") (PRIN2T Z))))
      (SETQ P2N (SETQ P1N 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT Z) (RETURN NIL)))
        (PROGN
         (SETQ K (CAR Z))
         (SETQ GN (GCDF* QN (NSUBSF RN V K)))
         (SETQ QN (QUOTF* QN GN))
         (SETQ RN (QUOTF* RN (NSUBSF GN V (MINUS K))))
         (COND
          ((GREATERP K 0)
           (PROG ()
            WHILELABEL
             (COND ((NOT (GEQ (SETQ K (DIFFERENCE K 1)) 0)) (RETURN NIL)))
             (PROGN
              (SETQ P1N
                      ((LAMBDA (G140)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF P1N G140))
                               (T (POLY-MULTF P1N G140))))
                       (NSUBSF GN V (MINUS K))))
              (COND
               ((SETQ Y (PROD-NSUBSF GN V (MINUS K)))
                (SETQ P2N
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF P2N Y))
                              (T (POLY-MULTF P2N Y)))))))
             (GO WHILELABEL)))
          ((LESSP K 0)
           (PROG ()
            WHILELABEL
             (COND ((NOT (LESSP K 0)) (RETURN NIL)))
             (PROGN
              (SETQ P2N
                      ((LAMBDA (G142)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF P2N G142))
                               (T (POLY-MULTF P2N G142))))
                       (NSUBSF GN V (MINUS K))))
              (COND
               ((SETQ Y (PROD-NSUBSF GN V (MINUS K)))
                (SETQ P1N
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF P1N Y))
                              (T (POLY-MULTF P1N Y))))))
              (SETQ K (PLUS K 1)))
             (GO WHILELABEL))))
         (SETQ Z (CDR Z)))
        (GO WHILELABEL))
      (COND ((OR (DEPEND-F QN V) (DEPEND-F RN V)) (GO FAIL)))
      (SETQ U
              (MULTSQ (CONS P1N P2N)
                      ((LAMBDA (U)
                         (COND (*QSUM-SIMPEXPT (QSUM-SIMPEXPT U))
                               (T (BASIC-SIMPEXPT U))))
                       (LIST (PREPSQ (CONS QN RN)) V))))
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T " *** Product of rational function calculated ***")
         (PRIN2* "    P(n) = ")
         (SQPRINT U)
         (TERPRI* T)
         (TERPRI* T))))
      (SETKORDER X)
      (RETURN (CONS (REORDER (CAR U)) (REORDER (CDR U))))
      (RETURN U)
     FAIL
      (COND
       (*TRSUM
        (PROGN
         (PRIN2T " *** Product of rational function failed ***")
         (TERPRI* T))))
      (SETKORDER X)
      (RETURN 'FAILED))) 
(PUT 'PROD-NSUBSF 'NUMBER-OF-ARGS 3) 
(PUT 'PROD-NSUBSF 'DEFINED-ON-LINE '233) 
(PUT 'PROD-NSUBSF 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'PROD-NSUBSF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROD-NSUBSF (U KERN I)
    (PROG (X Z N)
      (SETQ X (SETKORDER (LIST KERN)))
      (SETQ U (REORDER U))
      (SETQ Z NIL)
     A
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NOT (EQ (CAAAR U) KERN))) (GO B)))
      (SETQ Z (ADDF Z (CDAR U)))
      (SETQ N (DIFFERENCE (DEGR U KERN) (DEGR (CDR U) KERN)))
      (SETQ U (CDR U))
      (COND ((EQUAL I 0) (COND ((EQUAL N 0) NIL) (T (SETQ Z NIL))))
            (T
             (SETQ Z
                     ((LAMBDA (G144)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z G144))
                              (T (POLY-MULTF Z G144))))
                      (EXPT I N)))))
      (GO A)
     B
      (SETQ Z (ADDF Z U))
      (SETKORDER X)
      (RETURN (REORDER Z)))) 
(PUT 'INTEGER-ROOT2 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGER-ROOT2 'DEFINED-ON-LINE '260) 
(PUT 'INTEGER-ROOT2 'DEFINED-IN-FILE 'SUM/PROD.RED) 
(PUT 'INTEGER-ROOT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGER-ROOT2 (U V)
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
          ((NOT (TESTDIVIDE U V (MINUS (CAR N))))
           (PROGN
            (SETQ ROOT (CONS (MINUS (CAR N)) ROOT))
            (SETQ U (QUOTF* U (CONS W (CAR N))))))
          (T (SETQ N (CDR N)))))
        (GO WHILELABEL))
     A
      (SETKORDER X)
      (RETURN ROOT))) 
(ENDMODULE) 