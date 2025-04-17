(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFDECDEG)) 
(REVISION 'OFSFDECDEG
          "$Id: ofsfdecdeg.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFDECDEG
           "(c) 1995-2009 A. Dolzmann, T. Sturm, 2010-2014 T. Sturm") 
(PUT 'OFSF_DECDEG 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_DECDEG 'DEFINED-ON-LINE '32) 
(PUT 'OFSF_DECDEG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_DECDEG (F) (CAR (OFSF_DECDEG0 (CL_RENAME-VARS F)))) 
(PUT 'OFSF_DECDEG0 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_DECDEG0 'DEFINED-ON-LINE '38) 
(PUT 'OFSF_DECDEG0 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEG0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_DECDEG0 (F)
    (PROG (OP W GAMMA NEWMAT DVL NARGL)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (PROGN
         (SETQ NARGL
                 (PROG (SUBFO FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SUBFO (CDR F))
                   (COND ((NULL SUBFO) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SUBFO)
                                       (PROGN
                                        (SETQ W (OFSF_DECDEG0 SUBFO))
                                        (SETQ DVL (NCONC DVL (CDR W)))
                                        (CAR W)))
                                     (CAR SUBFO))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SUBFO (CDR SUBFO))
                   (COND ((NULL SUBFO) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBFO)
                               (PROGN
                                (SETQ W (OFSF_DECDEG0 SUBFO))
                                (SETQ DVL (NCONC DVL (CDR W)))
                                (CAR W)))
                             (CAR SUBFO))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN (CONS (CONS OP NARGL) DVL)))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (SETQ W (OFSF_DECDEG0 (CADDR F)))
         (SETQ DVL (CDR W))
         (SETQ W (OFSF_DECDEG1 (CAR W) (LIST (CADR F))))
         (SETQ DVL (NCONC DVL (CDR W)))
         (SETQ NEWMAT
                 (COND
                  ((OR (NULL (CDR W)) (NOT (EVENP (CDR (CAR (CDR W))))))
                   (CAR W))
                  (T
                   (PROGN
                    (SETQ GAMMA
                            (LIST 'GEQ (CAR (SIMP (CAR (CAR (CDR W))))) NIL))
                    (CONS (COND ((EQ OP 'EX) 'AND) (T 'IMPL))
                          (LIST GAMMA (CAR W)))))))
         (RETURN (CONS (LIST OP (CADR F) NEWMAT) DVL)))))
      (RETURN (CONS F NIL)))) 
(PUT 'OFSF_DECDEG1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DECDEG1 'DEFINED-ON-LINE '72) 
(PUT 'OFSF_DECDEG1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEG1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DECDEG1 (F VL)
    (PROG (POSP DVL N)
      (SETQ N 0)
      (COND ((EQ VL 'FVARL) (SETQ VL (CL_FVARL1 F))))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ POSP (OFSF_POSVARP F V))
            (SETQ N (OFSF_DECDEG2 F V POSP))
            (COND
             ((GREATERP N 1)
              (PROGN
               (SETQ F (OFSF_DECDEG3 F V N POSP))
               (SETQ DVL (CONS (CONS V N) DVL)))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS F DVL)))) 
(PUT 'OFSF_DECDEG2 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_DECDEG2 'DEFINED-ON-LINE '97) 
(PUT 'OFSF_DECDEG2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEG2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DECDEG2 (F V POSP)
    (PROG (A W ATL *GCD ODDP DGCD)
      (SETQ DGCD 0)
      (SETQ *GCD T)
      (COND (*RLBRKCXK (SETQ DGCD (OFSF_CXKDGCD F V))))
      (SETQ ATL (CL_ATL1 F))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ATL (NOT (EQN DGCD 1)))) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR ATL) (SETQ ATL (CDR ATL))))
         (SETQ W (OFSF_IGNSHIFT A V POSP))
         (COND ((AND (EQ W 'ODD) (NULL ODDP)) (SETQ ODDP 'ODD))
               ((NULL W)
                (PROGN
                 (SETQ A (SFTO_REORDER (CADR A) V))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND (NOT (OR (ATOM A) (ATOM (CAR A)))) (EQ (CAAAR A) V)
                           (NEQ DGCD 1)))
                     (RETURN NIL)))
                   (PROGN (SETQ DGCD (GCDF DGCD (CDAAR A))) (SETQ A (CDR A)))
                   (GO WHILELABEL)))))
         (COND
          ((AND (GREATERP DGCD 0) (EQ ODDP 'ODD))
           (PROGN
            (SETQ ODDP T)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (SETQ W ((LAMBDA (*EXP) (QUOTF1 DGCD 2)) T)))
                (RETURN NIL)))
              (SETQ DGCD W)
              (GO WHILELABEL))))))
        (GO WHILELABEL))
      (COND ((EQUAL DGCD 0) (RETURN 1)))
      (RETURN DGCD))) 
(PUT 'OFSF_CXKDGCD 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CXKDGCD 'DEFINED-ON-LINE '136) 
(PUT 'OFSF_CXKDGCD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_CXKDGCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CXKDGCD (F V)
    ((LAMBDA (*RLBRKCXK) (OFSF_CXKDGCD1 (CL_FVARL1 F) V 0)) NIL)) 
(PUT 'OFSF_CXKDGCD1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_CXKDGCD1 'DEFINED-ON-LINE '142) 
(PUT 'OFSF_CXKDGCD1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_CXKDGCD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CXKDGCD1 (KL V DGCD)
    (PROG (U)
      (PROG (K)
        (SETQ K KL)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (COND
            ((PAIRP K)
             (PROG (ARG)
               (SETQ ARG (CDR K))
              LAB
               (COND ((NULL ARG) (RETURN NIL)))
               ((LAMBDA (ARG)
                  (PROGN
                   (SETQ U (CAR (SIMP ARG)))
                   (SETQ DGCD (OFSF_CXKDGCD1 (KERNELS U) V DGCD))
                   (SETQ U (SFTO_REORDER U V))
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT
                        (AND (NOT (OR (ATOM U) (ATOM (CAR U))))
                             (EQ (CAAAR U) V)))
                       (RETURN NIL)))
                     (PROGN (SETQ DGCD (GCDF (CDAAR U) DGCD)) (SETQ U (CDR U)))
                     (GO WHILELABEL))))
                (CAR ARG))
               (SETQ ARG (CDR ARG))
               (GO LAB)))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (RETURN DGCD))) 
(PUT 'OFSF_TRANSFORM 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_TRANSFORM 'DEFINED-ON-LINE '163) 
(PUT 'OFSF_TRANSFORM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_TRANSFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_TRANSFORM (V F VL AN THEO ANS BVL)
    (PROG (POSP DGCD NF V_SHIFT W)
      (SETQ POSP (OFSF_POSVARP F V))
      (SETQ DGCD (OFSF_DECDEG2 F V POSP))
      (COND ((EQUAL DGCD 1) (RETURN NIL)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 (LIST "(" V "^" DGCD ")"))))
      (SETQ NF (OFSF_DECDEG3 F V DGCD POSP))
      (COND
       ((EVENP DGCD)
        (SETQ NF (CONS 'AND (LIST (LIST 'GEQ (CAR (SIMP V)) NIL) NF)))))
      (COND
       (ANS
        (PROGN
         (PROG ()
          REPEATLABEL
           (SETQ V_SHIFT (INTERN (GENSYM)))
           (COND ((NOT (NOT (FLAGP V_SHIFT 'USED*))) (GO REPEATLABEL))))
         (FLAG (LIST V_SHIFT) 'RL_QEANSVAR)
         (SETQ NF (CL_SUBFOF (LIST (CONS V V_SHIFT)) NF))
         (SETQ VL
                 (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                   (SETQ VV VL)
                   (COND ((NULL VV) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (VV)
                                       (COND ((EQ VV V) V_SHIFT) (T VV)))
                                     (CAR VV))
                                    NIL)))
                  LOOPLABEL
                   (SETQ VV (CDR VV))
                   (COND ((NULL VV) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (VV) (COND ((EQ VV V) V_SHIFT) (T VV)))
                             (CAR VV))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ W (SIMP (LIST 'EXPT V_SHIFT (LIST 'QUOTIENT 1 DGCD))))
         (SETQ AN
                 (CL_UPDANS V 'OFSF_SHIFT-INDICATOR (LIST 'DUMMY W DGCD) F AN
                            ANS)))))
      (RETURN (LIST NF VL AN THEO ANS BVL)))) 
(PUT 'OFSF_IGNSHIFT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_IGNSHIFT 'DEFINED-ON-LINE '191) 
(PUT 'OFSF_IGNSHIFT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_IGNSHIFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_IGNSHIFT (AT V POSP)
    (PROG (W)
      (SETQ W (SFTO_REORDER (CADR AT) V))
      (COND
       ((AND (NOT (OR (ATOM W) (ATOM (CAR W)))) (NULL (CDR W))
             (EQ (CAAAR W) V))
        (COND
         ((OR *RLPOS POSP (MEMQ (CAR AT) '(EQUAL NEQ)) (EVENP (CDAAR W)))
          (RETURN 'IGNORE))
         (T (RETURN 'ODD))))))) 
(PUT 'OFSF_DECDEG3 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_DECDEG3 'DEFINED-ON-LINE '203) 
(PUT 'OFSF_DECDEG3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEG3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DECDEG3 (F V N POSP) (CL_APPLY2ATS1 F 'OFSF_DECDEGAT (LIST V N POSP))) 
(PUT 'OFSF_DECDEGAT 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_DECDEGAT 'DEFINED-ON-LINE '208) 
(PUT 'OFSF_DECDEGAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_DECDEGAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DECDEGAT (ATF V N POSP)
    (COND ((OFSF_IGNSHIFT ATF V POSP) ATF)
          (T (LIST (CAR ATF) (SFTO_DECDEGF (CADR ATF) V N) NIL)))) 
(PUT 'OFSF_RETRANSFORM 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_RETRANSFORM 'DEFINED-ON-LINE '217) 
(PUT 'OFSF_RETRANSFORM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDECDEG.RED) 
(PUT 'OFSF_RETRANSFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_RETRANSFORM (F V DGCD)
    (PROG (POSP)
      (SETQ POSP (OFSF_POSVARP F V))
      (SETQ F (OFSF_DECDEG3 F V DGCD POSP))
      (COND
       ((EVENP DGCD)
        (SETQ F (CONS 'AND (LIST (LIST 'GEQ (CAR (SIMP V)) NIL) F)))))
      (RETURN F))) 
(ENDMODULE) 