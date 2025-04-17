(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFMISC)) 
(REVISION 'PASFMISC
          "$Id: pasfmisc.red 5964 2021-08-23 16:00:17Z thomas-sturm $") 
(COPYRIGHT 'PASFMISC
           "(c) 2002-2009 A. Dolzmann, A. Seidl, T. Sturm, 2010-2017 T. Sturm") 
(PUT 'PASF_ATF2IV 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ATF2IV 'DEFINED-ON-LINE '34) 
(PUT 'PASF_ATF2IV 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ATF2IV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ATF2IV (ATF)
    (PROG (DC NOM DEN FLOOR CEIL EUCD)
      (SETQ DC (REPR_ATFNEW ATF (CAR (CL_FVARL ATF)) NIL))
      (SETQ NOM (REPR_A DC))
      (SETQ DEN (REPR_N DC))
      (SETQ FLOOR (PASF_FLOOR NOM DEN))
      (SETQ CEIL (PASF_CEIL NOM DEN))
      (COND
       ((EQ (REPR_OP DC) 'EQUAL)
        (COND ((EQN DEN 1) (RETURN (IV_NEW FLOOR FLOOR)))
              (T (RETURN (LIST))))))
      (COND ((EQ (REPR_OP DC) 'LEQ) (RETURN (IV_NEW 'MINF FLOOR))))
      (COND
       ((EQ (REPR_OP DC) 'LESSP) (RETURN (IV_NEW 'MINF (ADDF CEIL (NEGF 1))))))
      (COND ((EQ (REPR_OP DC) 'GEQ) (RETURN (IV_NEW CEIL 'PINF))))
      (COND
       ((EQ (REPR_OP DC) 'GREATERP) (RETURN (IV_NEW (ADDF FLOOR 1) 'PINF))))
      (COND
       ((EQ (REPR_OP DC) 'NEQ)
        (RETURN
         (IV_MERGE (IV_NEW 'MINF (ADDF CEIL (NEGF 1)))
          (IV_NEW (ADDF FLOOR 1) 'PINF)))))
      (COND
       ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
        (COND
         ((EQ (GCDF DEN (CDAR ATF)) 1)
          (PROGN
           (SETQ EUCD (SFTO_EXTEUCD DEN (CDAR ATF)))
           (RETURN
            (IV_NEWCONG (REPR_OP DC)
             (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CADR EUCD) NOM))
                   (T (POLY-MULTF (CADR EUCD) NOM)))))))
         (T (RETURN (LIST))))))
      (REDERR (LIST "pasf_atf2iv: illegal operator " (CAR ATF))))) 
(PUT 'PASF_QFF2IVL 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_QFF2IVL 'DEFINED-ON-LINE '72) 
(PUT 'PASF_QFF2IVL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_QFF2IVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_QFF2IVL (F)
    (COND
     ((PASF_UPRAP F)
      (REDERR
       (LIST "pasf_qff2ivl : uniform Presburger arithmetic formula in input")))
     (T (PASF_QFF2IVL1 (PASF_DNF F))))) 
(PUT 'PASF_QFF2IVL1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_QFF2IVL1 'DEFINED-ON-LINE '81) 
(PUT 'PASF_QFF2IVL1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_QFF2IVL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_QFF2IVL1 (F)
    (PROG (FS CNG)
      (COND
       ((OR (EQ F 'TRUE) (EQ F 'FALSE))
        (COND
         ((EQ F 'TRUE)
          (REDERR (LIST "pasf_qff2ivl1 : true as a bound is invalid")))
         (T (RETURN NIL)))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
        (PROGN
         (PROG (PF)
           (SETQ PF (CDR F))
          LAB
           (COND ((NULL PF) (RETURN NIL)))
           ((LAMBDA (PF)
              (COND
               ((AND (PAIRP PF) (PAIRP (CAR PF))
                     (MEMQ (CAAR PF) '(CONG NCONG)))
                (SETQ CNG (CONS (CAR (PASF_ATF2IV PF)) CNG)))
               (T (SETQ FS (CONS (PASF_ATF2IV PF) FS)))))
            (CAR PF))
           (SETQ PF (CDR PF))
           (GO LAB))
         (RETURN (IV_CUTCONGS (IV_CUTN FS) CNG))))
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (RETURN
         (IV_MERGEN
          (PROG (PF FORALL-RESULT FORALL-ENDPTR)
            (SETQ PF (CDR F))
            (COND ((NULL PF) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (PF) (PASF_QFF2IVL1 PF)) (CAR PF))
                                  NIL)))
           LOOPLABEL
            (SETQ PF (CDR PF))
            (COND ((NULL PF) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (PF) (PASF_QFF2IVL1 PF)) (CAR PF)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND
       ((EQ
         (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) F) ((PAIRP (CAR F)) (CAAR F))
               (T (CAR F)))
         'EQUAL)
        (RETURN (PASF_ATF2IV F))))
      (REDERR (LIST "pasf_qff2ivl1 : abused procedure call with" F)))) 
(PUT 'PASF_IVL2QFF 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_IVL2QFF 'DEFINED-ON-LINE '120) 
(PUT 'PASF_IVL2QFF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_IVL2QFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_IVL2QFF (IVL VAR)
    (COND
     ((NOT (IV_EMPTY IVL))
      ((LAMBDA (G127)
         (COND ((AND G127 (CDR G127)) (CONS 'OR G127))
               ((NULL G127) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
               (T (CAR G127))))
       (PROG (IV FORALL-RESULT FORALL-ENDPTR)
         (SETQ IV IVL)
         (COND ((NULL IV) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (IV)
                             (PASF_MKRNG (CAR (SIMP VAR)) (CAR (SIMP (CAR IV)))
                                         (CAR (SIMP (CDR IV)))))
                           (CAR IV))
                          NIL)))
        LOOPLABEL
         (SETQ IV (CDR IV))
         (COND ((NULL IV) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (IV)
                     (PASF_MKRNG (CAR (SIMP VAR)) (CAR (SIMP (CAR IV)))
                                 (CAR (SIMP (CDR IV)))))
                   (CAR IV))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     (T 'FALSE))) 
(PUT 'PASF_BSATP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_BSATP 'DEFINED-ON-LINE '131) 
(PUT 'PASF_BSATP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_BSATP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_BSATP (F VAR)
    (PROG (FLG R ARGN ARGNA A)
      (COND ((NEQ (CL_FVARL F) (LIST VAR)) (RETURN NIL)))
      (SETQ F (PASF_DNF F))
      (COND ((EQ F 'FALSE) (RETURN NIL)))
      (COND ((EQ F 'TRUE) (REDERR (LIST "pasf_bsatp: infinite bound"))))
      (SETQ ARGN
              (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR) (CDR F))
                    (T (LIST F))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ARGN (NULL R))) (RETURN NIL)))
        (PROGN
         (SETQ ARGNA
                 (COND
                  ((EQ
                    (COND ((ATOM (CAR ARGN)) (CAR ARGN)) (T (CAR (CAR ARGN))))
                    'AND)
                   (CDR (CAR ARGN)))
                  (T (LIST (CAR ARGN)))))
         (SETQ FLG NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND ARGNA (NOT FLG))) (RETURN NIL)))
           (PROGN
            (SETQ FLG
                    (AND (PAIRP (CAR ARGNA)) (PAIRP (CAR (CAR ARGNA)))
                         (MEMQ (CAAR (CAR ARGNA)) '(CONG NCONG))))
            (SETQ ARGNA (CDR ARGNA)))
           (GO WHILELABEL))
         (COND
          ((AND (NULL FLG) (NOT (IV_EMPTY (PASF_QFF2IVL (CAR ARGN)))))
           (SETQ R T)))
         (SETQ ARGN (CDR ARGN)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PASF_RMAX 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_RMAX 'DEFINED-ON-LINE '158) 
(PUT 'PASF_RMAX 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_RMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_RMAX (RNG1 RNG2)
    (CONS (PASF_MIN (CAR RNG1) (CAR RNG2)) (PASF_MAX (CDR RNG1) (CDR RNG2)))) 
(PUT 'PASF_BRNG 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_BRNG 'DEFINED-ON-LINE '164) 
(PUT 'PASF_BRNG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_BRNG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_BRNG (B VAR)
    (PROG (TMP BMAX)
      (COND
       ((GREATERP (LENGTH (CL_FVARL B)) 1)
        (REDERR (LIST "pasf_brng called with parametric bound"))))
      (SETQ TMP (CL_SIMPL (PASF_DNF B) NIL (MINUS 1)))
      (COND
       ((EQ TMP 'FALSE) (REDERR (LIST "Not satisfiable bound in pasf_brng"))))
      (COND ((EQ TMP 'TRUE) (REDERR (LIST "Tautological bound in pasf_brng"))))
      (SETQ BMAX (CONS 'PINF 'MINF))
      (COND
       ((EQ (COND ((ATOM TMP) TMP) (T (CAR TMP))) 'OR)
        (PROG (SF)
          (SETQ SF (CDR TMP))
         LAB
          (COND ((NULL SF) (RETURN NIL)))
          ((LAMBDA (SF) (SETQ BMAX (PASF_RMAX (PASF_BRNG1 SF VAR) BMAX)))
           (CAR SF))
          (SETQ SF (CDR SF))
          (GO LAB)))
       (T (SETQ BMAX (PASF_BRNG1 TMP VAR))))
      (RETURN BMAX))) 
(PUT 'PASF_BRNG1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_BRNG1 'DEFINED-ON-LINE '188) 
(PUT 'PASF_BRNG1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_BRNG1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_BRNG1 (B VAR)
    (PROG (TMP)
      (SETQ TMP
              (PASF_QFF2IVL
               ((LAMBDA (G129)
                  (COND ((AND G129 (CDR G129)) (CONS 'AND G129))
                        ((NULL G129) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G129))))
                (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                  (SETQ ATF (CL_ATL B))
                 STARTOVER
                  (COND ((NULL ATF) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (ATF)
                             (COND
                              ((NOT
                                (AND (PAIRP ATF) (PAIRP (CAR ATF))
                                     (MEMQ (CAAR ATF) '(CONG NCONG))))
                               (LIST ATF))))
                           (CAR ATF)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ ATF (CDR ATF))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (ATF)
                             (COND
                              ((NOT
                                (AND (PAIRP ATF) (PAIRP (CAR ATF))
                                     (MEMQ (CAAR ATF) '(CONG NCONG))))
                               (LIST ATF))))
                           (CAR ATF)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ ATF (CDR ATF))
                  (GO LOOPLABEL)))))
      (COND
       ((NULL TMP)
        (REDERR
         (LIST "pasf_brng1 : Something is wrong, empty bound solution set"))))
      (RETURN (CONS (CAAR TMP) (CDAR (REVERSE TMP)))))) 
(PUT 'PASF_ORDATP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_ORDATP 'DEFINED-ON-LINE '207) 
(PUT 'PASF_ORDATP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ORDATP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_ORDATP (A1 A2)
    (PROG (LHS1 LHS2)
      (SETQ LHS1 (CADR A1))
      (SETQ LHS2 (CADR A2))
      (COND ((NEQ LHS1 LHS2) (RETURN (ORDP LHS1 LHS2))))
      (RETURN
       (PASF_ORDRELP
        (COND ((OR (EQ A1 'TRUE) (EQ A1 'FALSE)) A1)
              ((PAIRP (CAR A1)) (CAAR A1)) (T (CAR A1)))
        (COND ((OR (EQ A2 'TRUE) (EQ A2 'FALSE)) A2)
              ((PAIRP (CAR A2)) (CAAR A2)) (T (CAR A2))))))) 
(PUT 'PASF_ORDRELP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_ORDRELP 'DEFINED-ON-LINE '218) 
(PUT 'PASF_ORDRELP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ORDRELP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_ORDRELP (R1 R2)
    (NOT
     (NOT (MEMQ R2 (MEMQ R1 '(EQUAL NEQ LEQ LESSP GEQ GREATERP CONG NCONG)))))) 
(PUT 'PASF_DEC 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_DEC 'DEFINED-ON-LINE '224) 
(PUT 'PASF_DEC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_DEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_DEC (U)
    (PROG (ABSV)
      (SETQ ABSV U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM ABSV) (ATOM (CAR ABSV))))) (RETURN NIL)))
        (SETQ ABSV (CDR ABSV))
        (GO WHILELABEL))
      (RETURN (CONS (ADDF U (NEGF ABSV)) ABSV)))) 
(PUT 'PASF_DECI 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_DECI 'DEFINED-ON-LINE '235) 
(PUT 'PASF_DECI 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_DECI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_DECI (U)
    (PROG (R)
      (SETQ R (PASF_DEC U))
      (RETURN (CONS (CAR R) (COND ((NULL (CDR R)) 0) (T (CDR R))))))) 
(PUT 'PASF_VARLAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_VARLAT 'DEFINED-ON-LINE '245) 
(PUT 'PASF_VARLAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_VARLAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_VARLAT (ATF)
    (PROG (VL)
      (SETQ VL
              (APPEND (KERNELS (CADR ATF))
                      (COND
                       ((AND (PAIRP ATF) (PAIRP (CAR ATF))
                             (MEMQ (CAAR ATF) '(CONG NCONG)))
                        (KERNELS (CDAR ATF)))
                       (T NIL))))
      (COND
       (*RLBRKCXK
        (SETQ VL
                (PROG (V FORALL-RESULT FORALL-ENDPTR)
                  (SETQ V VL)
                 STARTOVER
                  (COND ((NULL V) (RETURN NIL)))
                  (SETQ FORALL-RESULT ((LAMBDA (V) (LTO_LPVARL V)) (CAR V)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ V (CDR V))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL V) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR ((LAMBDA (V) (LTO_LPVARL V)) (CAR V)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ V (CDR V))
                  (GO LOOPLABEL)))))
      (RETURN VL))) 
(PUT 'PASF_VARSUBSTAT 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_VARSUBSTAT 'DEFINED-ON-LINE '257) 
(PUT 'PASF_VARSUBSTAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_VARSUBSTAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_VARSUBSTAT (ATF NEW OLD)
    (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
          (T
           (LIST
            (COND
             ((AND (PAIRP ATF) (PAIRP (CAR ATF))
                   (MEMQ (CAAR ATF) '(CONG NCONG)))
              (CONS
               (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                     ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
               (CAR (SUBF (CDAR ATF) (LIST (CONS OLD NEW))))))
             (T (CAR ATF)))
            (CAR (SUBF (CADR ATF) (LIST (CONS OLD NEW)))) NIL)))) 
(PUT 'PASF_NEGATEAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_NEGATEAT 'DEFINED-ON-LINE '273) 
(PUT 'PASF_NEGATEAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_NEGATEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_NEGATEAT (ATF)
    (COND
     ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
      (COND ((EQ ATF 'FALSE) 'TRUE) (T 'FALSE)))
     ((MEMQ
       (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
             ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
       '(CONG NCONG))
      (LIST
       (PASF_MKOP
        (PASF_LNEGREL
         (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
               ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF))))
        (CDAR ATF))
       (CADR ATF) (CADDR ATF)))
     (T
      (LIST
       (PASF_LNEGREL
        (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
              ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF))))
       (CADR ATF) (CADDR ATF))))) 
(PUT 'PASF_LNEGREL 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_LNEGREL 'DEFINED-ON-LINE '284) 
(PUT 'PASF_LNEGREL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_LNEGREL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_LNEGREL (R)
    (COND ((EQ R 'EQUAL) 'NEQ) ((EQ R 'NEQ) 'EQUAL) ((EQ R 'LEQ) 'GREATERP)
          ((EQ R 'LESSP) 'GEQ) ((EQ R 'GEQ) 'LESSP) ((EQ R 'GREATERP) 'LEQ)
          ((EQ R 'CONG) 'NCONG) ((EQ R 'NCONG) 'CONG)
          (T (REDERR (LIST "pasf_lnegrel: unknown operator" R))))) 
(PUT 'PASF_ANEGATEAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ANEGATEAT 'DEFINED-ON-LINE '298) 
(PUT 'PASF_ANEGATEAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ANEGATEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ANEGATEAT (ATF)
    (COND
     ((MEMQ
       (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
             ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
       '(CONG NCONG))
      (LIST
       (PASF_MKOP
        (PASF_ANEGREL
         (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
               ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF))))
        (CDAR ATF))
       (NEGF (CADR ATF)) (NEGF (CADDR ATF))))
     (T
      (LIST
       (PASF_ANEGREL
        (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
              ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF))))
       (NEGF (CADR ATF)) (NEGF (CADDR ATF)))))) 
(PUT 'PASF_ANEGREL 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ANEGREL 'DEFINED-ON-LINE '309) 
(PUT 'PASF_ANEGREL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ANEGREL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ANEGREL (R)
    (OR
     (CDR
      (ATSOC R
             '((EQUAL . EQUAL) (NEQ . NEQ) (LEQ . GEQ) (GEQ . LEQ)
               (LESSP . GREATERP) (GREATERP . LESSP) (CONG . CONG)
               (NCONG . NCONG))))
     (REDERR (LIST "pasf_anegrel: unknown operator " R)))) 
(PUT 'PASF_SUBAT 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SUBAT 'DEFINED-ON-LINE '317) 
(PUT 'PASF_SUBAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SUBAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SUBAT (AL F)
    (PROG (NLHS NLHS1)
      (PROG (A)
        (SETQ A AL)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NULL (EQN (CDR (SIMP (CDR A))) 1))
             (REDERR "pasf_subat: only presburger terms can be substituted"))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (COND
       ((AND (PAIRP F) (PAIRP (CAR F)) (MEMQ (CAAR F) '(CONG NCONG)))
        (PROGN
         (SETQ NLHS (SUBF (CADR F) AL))
         (SETQ NLHS1 (SUBF (CDAR F) AL))
         (COND
          ((OR (NOT (OR (ATOM (CDR NLHS)) (ATOM (CAR (CDR NLHS)))))
               (NOT (OR (ATOM (CDR NLHS1)) (ATOM (CAR (CDR NLHS1))))))
           (REDERR "pasf_subat: parametric denominator after substitution")))
         (RETURN
          (LIST
           (CONS
            (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) F)
                  ((PAIRP (CAR F)) (CAAR F)) (T (CAR F)))
            (CAR NLHS1))
           (CAR NLHS) NIL)))))
      (SETQ NLHS (SUBF (CADR F) AL))
      (COND
       ((NOT (OR (ATOM (CDR NLHS)) (ATOM (CAR (CDR NLHS)))))
        (REDERR "pasf_subat: parametric denominator after substitution")))
      (RETURN (LIST (CAR F) (CAR NLHS) NIL)))) 
(PUT 'PASF_MKSTRICT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_MKSTRICT 'DEFINED-ON-LINE '338) 
(PUT 'PASF_MKSTRICT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_MKSTRICT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_MKSTRICT (R) (COND ((EQ R 'LEQ) 'LESSP) ((EQ R 'GEQ) 'GREATERP) (T R))) 
(PUT 'PASF_SUBALCHK 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SUBALCHK 'DEFINED-ON-LINE '343) 
(PUT 'PASF_SUBALCHK 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SUBALCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SUBALCHK (AL)
    (PROG (X)
      (SETQ X AL)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X)
         (COND
          ((NOT
            ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CDR (SIMP (CDR X)))))
           (REDERR
            "pasf_subalchk: parametric denominator in substituted term"))))
       (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'PASF_EQNRHSKERNELS 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_EQNRHSKERNELS 'DEFINED-ON-LINE '350) 
(PUT 'PASF_EQNRHSKERNELS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EQNRHSKERNELS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_EQNRHSKERNELS (X)
    ((LAMBDA (W) (NCONC (KERNELS (CAR W)) (KERNELS (CDR W)))) (SIMP (CDR X)))) 
(PUT 'PASF_FLOOR 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_FLOOR 'DEFINED-ON-LINE '355) 
(PUT 'PASF_FLOOR 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_FLOOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_FLOOR (NOM DEN)
    (COND
     ((AND (OR (ATOM NOM) (ATOM (CAR NOM))) (OR (ATOM DEN) (ATOM (CAR DEN))))
      (COND ((NULL NOM) NIL)
            (T
             (CAR
              (SIMP
               (COND ((EQUAL (REMAINDER NOM DEN) 0) (QUOTIENT NOM DEN))
                     ((GREATERP (TIMES NOM DEN) 0) (QUOTIENT NOM DEN))
                     (T (DIFFERENCE (QUOTIENT NOM DEN) 1))))))))
     (T (REDERR (LIST "pasf_floor: not a domain valued sf in input" NOM DEN))))) 
(PUT 'PASF_CEIL 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_CEIL 'DEFINED-ON-LINE '374) 
(PUT 'PASF_CEIL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_CEIL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_CEIL (NOM DEN)
    (COND
     ((AND (OR (ATOM NOM) (ATOM (CAR NOM))) (OR (ATOM DEN) (ATOM (CAR DEN))))
      (COND ((NULL NOM) NIL)
            (T
             (CAR
              (SIMP
               (COND ((EQUAL (REMAINDER NOM DEN) 0) (QUOTIENT NOM DEN))
                     ((GREATERP (TIMES NOM DEN) 0) (PLUS (QUOTIENT NOM DEN) 1))
                     (T (QUOTIENT NOM DEN))))))))
     (T (REDERR (LIST "pasf_ceil: not a domain valued sf in input" NOM DEN))))) 
(PUT 'PASF_CONST 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CONST 'DEFINED-ON-LINE '393) 
(PUT 'PASF_CONST 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_CONST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CONST (EX)
    (COND ((OR (ATOM EX) (ATOM (CAR EX))) EX) (T (PASF_CONST (CDR EX))))) 
(PUT 'PASF_FCTRAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_FCTRAT 'DEFINED-ON-LINE '401) 
(PUT 'PASF_FCTRAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_FCTRAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_FCTRAT (ATF)
    (COND
     ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
      (NCONC (CDR (FCTRF (CADR ATF))) (CDR (FCTRF (CDAR ATF)))))
     (T (CDR (FCTRF (CADR ATF)))))) 
(PUT 'PASF_TERMMLAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_TERMMLAT 'DEFINED-ON-LINE '412) 
(PUT 'PASF_TERMMLAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_TERMMLAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_TERMMLAT (ATF) (COND ((CADR ATF) (LIST (CONS (CADR ATF) 1))))) 
(PUT 'PASF_MAX 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_MAX 'DEFINED-ON-LINE '418) 
(PUT 'PASF_MAX 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_MAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_MAX (A B) (COND ((PASF_LEQP A B) B) (T A))) 
(PUT 'PASF_MIN 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_MIN 'DEFINED-ON-LINE '424) 
(PUT 'PASF_MIN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_MIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_MIN (A B) (COND ((PASF_LEQP A B) A) (T B))) 
(PUT 'PASF_LEQP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_LEQP 'DEFINED-ON-LINE '430) 
(PUT 'PASF_LEQP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_LEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_LEQP (C1 C2)
    (PROG ()
      (COND ((NULL C1) (SETQ C1 0)))
      (COND ((NULL C2) (SETQ C2 0)))
      (RETURN
       (COND
        ((OR (EQ C1 'MINF) (EQ C2 'PINF)
             (AND (NEQ C1 'PINF) (NEQ C2 'MINF) (LEQ C1 C2)))
         T))))) 
(PUT 'PASF_LEQ 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_LEQ 'DEFINED-ON-LINE '443) 
(PUT 'PASF_LEQ 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_LEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_LEQ (C1 C2)
    (PROG ()
      (COND ((NULL C1) (SETQ C1 0)))
      (COND ((NULL C2) (SETQ C2 0)))
      (RETURN
       (COND
        ((OR (EQ C1 'MINF) (EQ C2 'PINF)
             (AND (NEQ C1 'PINF) (NEQ C2 'PINF) (NEQ C2 'MINF) (LESSP C1 C2)))
         T))))) 
(PUT 'PASF_EXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_EXPAND 'DEFINED-ON-LINE '456) 
(PUT 'PASF_EXPAND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_EXPAND (F)
    (PROG (FDEC FLAG TMP)
      (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ Entering pasf_expand"))))
      (COND (*RLVERBOSE (IOTO_PRIN2T " (exprng)")))
      (RETURN (CL_SIMPL (PASF_EXPRNG F) NIL (MINUS 1)))
      (SETQ FDEC (FDEC_NEW (PASF_PNF F) NIL))
      (PROG (B)
        (SETQ B (FDEC_BVL FDEC))
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ TMP (CL_FVARL (CAR B)))
            (COND
             ((OR (GREATERP (LENGTH TMP) 1)
                  (AND (EQUAL (LENGTH TMP) 1) (NEQ (CDR B) (CAR TMP))))
              (SETQ FLAG T)))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (RETURN
       (COND
        (FLAG
         (PROGN
          (COND (*RLVERBOSE (IOTO_PRIN2T " (regular)")))
          (CL_SIMPL (PASF_EXPRNG1 F) NIL (MINUS 1))))
        (T
         (PROGN
          (COND (*RLVERBOSE (IOTO_PRIN2T " (smart)")))
          (CL_SIMPL (PASF_EXPRNG2 F) NIL (MINUS 1)))))))) 
(PUT 'PASF_EXPRNG1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_EXPRNG1 'DEFINED-ON-LINE '484) 
(PUT 'PASF_EXPRNG1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_EXPRNG1 (F)
    (PROG (OP)
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         ((LAMBDA (G131)
            (COND ((AND G131 (CDR G131)) (CONS OP G131))
                  ((NULL G131) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G131))))
          (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
            (SETQ ARG (CDR F))
            (COND ((NULL ARG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (ARG) (PASF_EXPRNG1 ARG)) (CAR ARG))
                                  NIL)))
           LOOPLABEL
            (SETQ ARG (CDR ARG))
            (COND ((NULL ARG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (ARG) (PASF_EXPRNG1 ARG)) (CAR ARG)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (PASF_EXPRNG1 (CADDR F))))))
      (COND
       ((EQ OP 'BALL)
        (RETURN
         (PASF_EXPRNG1-GAND OP (CADR F) (CADDDR F) (CADDR F) 'AND 'TRUE
          'FALSE))))
      (COND
       ((EQ OP 'BEX)
        (RETURN
         (PASF_EXPRNG1-GAND OP (CADR F) (CADDDR F) (CADDR F) 'OR 'FALSE
          'TRUE))))
      (RETURN F))) 
(PUT 'PASF_EXPRNG1-GAND 'NUMBER-OF-ARGS 7) 
(PUT 'PASF_EXPRNG1-GAND 'DEFINED-ON-LINE '505) 
(PUT 'PASF_EXPRNG1-GAND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG1-GAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PASF_EXPRNG1-GAND (OP V B M GAND GTRUE GFALSE)
    (PROG (W MATJ TERML J C RESL)
      (SETQ W (CL_FVARL B))
      (COND
       ((OR (NOT (EQCAR W V)) (CDR W))
        (REDERR
         (LIST "Expanding a parametric bounded formula is impossible"))))
      (SETQ TERML (PASF_B2TERML B V))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" OP "," V "," (LENGTH TERML)))))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C TERML)) (RETURN NIL)))
        (PROGN
         (SETQ J (CAR TERML))
         (SETQ TERML (CDR TERML))
         (SETQ MATJ
                 (CL_SIMPL (PASF_EXPRNG1 (PASF_SUBFOF V J M)) NIL (MINUS 1)))
         (COND
          ((EQ MATJ GFALSE)
           (PROGN
            (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "!"))))
            (SETQ SECONDVALUE* (CONS (CONS V J) SECONDVALUE*))
            (SETQ C NIL)))
          ((NEQ MATJ GTRUE) (SETQ RESL (CONS MATJ RESL)))))
        (GO WHILELABEL))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "]"))))
      (RETURN
       (COND
        (C
         (CL_SIMPL
          (COND ((AND RESL (CDR RESL)) (CONS GAND RESL))
                ((NULL RESL) (COND ((EQ GAND 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR RESL)))
          NIL (MINUS 1)))
        (T GFALSE))))) 
(PUT 'PASF_EXPRNG2 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_EXPRNG2 'DEFINED-ON-LINE '528) 
(PUT 'PASF_EXPRNG2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_EXPRNG2 (F)
    (PROG (TERML EVALTYPE MATR TMP RES)
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (COND
       (((LAMBDA (X)
           (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
               (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV))))
         (COND ((ATOM F) F) (T (CAR F))))
        (RETURN
         ((LAMBDA (G132 G133)
            (COND ((AND G133 (CDR G133)) (CONS G132 G133))
                  ((NULL G133) (COND ((EQ G132 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G133))))
          (COND ((ATOM F) F) (T (CAR F)))
          (PROG (SF FORALL-RESULT FORALL-ENDPTR)
            (SETQ SF (CDR F))
            (COND ((NULL SF) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (SF)
                                (CL_SIMPL (PASF_EXPRNG2 SF) NIL (MINUS 1)))
                              (CAR SF))
                             NIL)))
           LOOPLABEL
            (SETQ SF (CDR SF))
            (COND ((NULL SF) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (SF) (CL_SIMPL (PASF_EXPRNG2 SF) NIL (MINUS 1)))
                      (CAR SF))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND
       (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
         (COND ((ATOM F) F) (T (CAR F))))
        (PROGN
         (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'BEX) (SETQ EVALTYPE 'OR))
               ((EQ (COND ((ATOM F) F) (T (CAR F))) 'BALL)
                (SETQ EVALTYPE 'AND))
               (T
                (REDERR
                 (LIST "pasf_expand : unknown or illegal quantifier"
                       (COND ((ATOM F) F) (T (CAR F)))))))
         (SETQ TMP (CL_FVARL (CADDDR F)))
         (COND
          ((OR (CDR TMP) (NOT (EQCAR TMP (CADR F))))
           (REDERR
            (LIST "Expanding a parametric bounded formula is impossible"))))
         (SETQ TERML (PASF_B2TERML (CADDDR F) (CADR F)))
         (SETQ MATR (PASF_EXPRNG2 (CADDR F)))
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T
            (LIST "---- (" (COND ((ATOM F) F) (T (CAR F))) " " (CADR F) ")"))))
         (SETQ RES (LIST))
         (PROG (J FORALL-RESULT FORALL-ENDPTR)
           (SETQ J TERML)
           (COND ((NULL J) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J)
                               (PROGN
                                (COND
                                 (*RLVERBOSE (IOTO_PRIN2 (LIST "[" J "]"))))
                                (SETQ RES
                                        (CONS
                                         (CL_SIMPL
                                          (PASF_SUBFOF (CADR F) J MATR) NIL
                                          (MINUS 1))
                                         RES))))
                             (CAR J))
                            NIL)))
          LOOPLABEL
           (SETQ J (CDR J))
           (COND ((NULL J) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (J)
                       (PROGN
                        (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" J "]"))))
                        (SETQ RES
                                (CONS
                                 (CL_SIMPL (PASF_SUBFOF (CADR F) J MATR) NIL
                                           (MINUS 1))
                                 RES))))
                     (CAR J))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))
         (IOTO_PRIN2T (LIST ""))
         (RETURN
          (COND ((AND RES (CDR RES)) (CONS EVALTYPE RES))
                ((NULL RES) (COND ((EQ EVALTYPE 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR RES)))))))
      (COND
       (((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
         (COND ((ATOM F) F) (T (CAR F))))
        (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F)
              (PASF_EXPRNG2 (CADDR F)))))
      (RETURN F))) 
(PUT 'PASF_EXPRNG 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_EXPRNG 'DEFINED-ON-LINE '566) 
(PUT 'PASF_EXPRNG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_EXPRNG (F)
    (PROG (OP W)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((EQ OP 'AND) (RETURN (PASF_EXPRNG-GAND 'AND (CDR F) 'TRUE 'FALSE))))
      (COND ((EQ OP 'OR) (RETURN (PASF_EXPRNG-GAND 'OR (CDR F) 'FALSE 'TRUE))))
      (COND
       ((EQ OP 'BALL)
        (RETURN
         (PASF_EXPRNG-GBALL (CADR F) (CADDDR F) (CADDR F) 'AND 'TRUE 'FALSE))))
      (COND
       ((EQ OP 'BEX)
        (RETURN
         (PASF_EXPRNG-GBALL (CADR F) (CADDDR F) (CADDR F) 'OR 'FALSE 'TRUE))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (PROGN
         (SETQ W
                 (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SUBF (CDR F))
                   (COND ((NULL SUBF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SUBF) (PASF_EXPRNG SUBF))
                                     (CAR SUBF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SUBF (CDR SUBF))
                   (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBF) (PASF_EXPRNG SUBF)) (CAR SUBF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN
          (RL_SIMPLBASIC
           (COND ((AND W (CDR W)) (CONS OP W))
                 ((NULL W) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE))) (T (CAR W)))
           NIL (MINUS 1))))))
      (RETURN F))) 
(PUT 'PASF_EXPRNG-GAND 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_EXPRNG-GAND 'DEFINED-ON-LINE '589) 
(PUT 'PASF_EXPRNG-GAND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG-GAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_EXPRNG-GAND (GAND ARGL GTRUE GFALSE)
    (PROG (C A W NARGL)
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C ARGL)) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR ARGL) (SETQ ARGL (CDR ARGL))))
         (SETQ W (PASF_EXPRNG A))
         (COND ((EQ W GFALSE) (SETQ C NIL))
               ((NEQ W GTRUE) (SETQ NARGL (CONS W NARGL)))))
        (GO WHILELABEL))
      (COND ((NOT C) (RETURN GFALSE)))
      (RETURN
       (COND ((AND NARGL (CDR NARGL)) (CONS GAND NARGL))
             ((NULL NARGL) (COND ((EQ GAND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR NARGL)))))) 
(PUT 'PASF_B2TERML 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_B2TERML 'DEFINED-ON-LINE '604) 
(PUT 'PASF_B2TERML 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_B2TERML 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_B2TERML (B VAR)
    (PROG (IVL)
      (SETQ IVL (PASF_QFF2IVL B))
      (RETURN
       (PROG (IV FORALL-RESULT FORALL-ENDPTR)
         (SETQ IV IVL)
        STARTOVER
         (COND ((NULL IV) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (IV)
                    (COND
                     ((AND (NUMBERP (CAR IV)) (NUMBERP (CDR IV)))
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I (CAR IV))
                        (COND ((MINUSP (DIFFERENCE (CDR IV) I)) (RETURN NIL)))
                        (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                       LOOPLABEL
                        (SETQ I (PLUS2 I 1))
                        (COND
                         ((MINUSP (DIFFERENCE (CDR IV) I))
                          (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR (CONS I NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                     (T
                      (REDERR
                       (LIST
                        "pasf_b2terml : trying to expand infinite bound")))))
                  (CAR IV)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ IV (CDR IV))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL IV) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (IV)
                    (COND
                     ((AND (NUMBERP (CAR IV)) (NUMBERP (CDR IV)))
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I (CAR IV))
                        (COND ((MINUSP (DIFFERENCE (CDR IV) I)) (RETURN NIL)))
                        (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                       LOOPLABEL
                        (SETQ I (PLUS2 I 1))
                        (COND
                         ((MINUSP (DIFFERENCE (CDR IV) I))
                          (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR (CONS I NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                     (T
                      (REDERR
                       (LIST
                        "pasf_b2terml : trying to expand infinite bound")))))
                  (CAR IV)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ IV (CDR IV))
         (GO LOOPLABEL))))) 
(SWITCH (LIST 'RLEXPRNGNATURAL)) 
(OFF1 'RLEXPRNGNATURAL) 
(PUT 'PASF_EXPRNG-GBALL 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_EXPRNG-GBALL 'DEFINED-ON-LINE '629) 
(PUT 'PASF_EXPRNG-GBALL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPRNG-GBALL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_EXPRNG-GBALL (V B M GAND GTRUE GFALSE)
    (PROG (C U W ARGL IVL IV)
      (SETQ W (CL_FVARL B))
      (COND
       ((OR (NOT (EQCAR W V)) (CDR W))
        (REDERR (LIST "pasf_exprng: bad bound " B " with free variables " W))))
      (COND (*RLEXPRNGNATURAL (SETQ M (PASF_EXPRNG M))))
      (SETQ IVL (PASF_QFF2IVL B))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C IVL)) (RETURN NIL)))
        (PROGN
         (SETQ IV (PROG1 (CAR IVL) (SETQ IVL (CDR IVL))))
         (SETQ U (CAR IV))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C (LEQ U (CDR IV)))) (RETURN NIL)))
           (PROGN
            (SETQ W (PASF_SISUB M V U))
            (COND ((NOT *RLEXPRNGNATURAL) (SETQ W (PASF_EXPRNG W))))
            (COND ((EQ W GFALSE) (SETQ C NIL))
                  (T
                   (PROGN
                    (COND ((NEQ W GTRUE) (SETQ ARGL (CONS W ARGL))))
                    (SETQ U (PLUS U 1))))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (COND ((NOT C) (RETURN GFALSE)))
      (RETURN
       (COND ((AND ARGL (CDR ARGL)) (CONS GAND ARGL))
             ((NULL ARGL) (COND ((EQ GAND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR ARGL)))))) 
(PUT 'PASF_SISUB 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SISUB 'DEFINED-ON-LINE '658) 
(PUT 'PASF_SISUB 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SISUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SISUB (F V N)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (PASF_SISUB (CADDR F) V N)))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (RETURN
         (LIST OP (CADR F) (PASF_SISUB (CADDR F) V N)
               (PASF_SISUB (CADDDR F) V N)))))
      (COND
       ((EQ OP 'AND) (RETURN (PASF_SISUB-GAND 'AND (CDR F) V N 'TRUE 'FALSE))))
      (COND
       ((EQ OP 'OR) (RETURN (PASF_SISUB-GAND 'OR (CDR F) V N 'FALSE 'TRUE))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         ((LAMBDA (G135)
            (COND ((AND G135 (CDR G135)) (CONS OP G135))
                  ((NULL G135) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G135))))
          (PROG (SF FORALL-RESULT FORALL-ENDPTR)
            (SETQ SF (CDR F))
            (COND ((NULL SF) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (SF) (PASF_SISUB SF V N)) (CAR SF))
                                  NIL)))
           LOOPLABEL
            (SETQ SF (CDR SF))
            (COND ((NULL SF) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (SF) (PASF_SISUB SF V N)) (CAR SF)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (RETURN
       (PASF_SIMPLAT1
        (LIST (CAR F) (CAR (SUBF (CADR F) (LIST (CONS V N)))) NIL) OP)))) 
(PUT 'PASF_SISUB-GAND 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_SISUB-GAND 'DEFINED-ON-LINE '682) 
(PUT 'PASF_SISUB-GAND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SISUB-GAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SISUB-GAND (GAND ARGL V N GTRUE GFALSE)
    (PROG (C W A NARGL)
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C ARGL)) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR ARGL) (SETQ ARGL (CDR ARGL))))
         (SETQ W (PASF_SISUB A V N))
         (COND ((EQ W GFALSE) (SETQ C NIL))
               ((NEQ W GTRUE) (SETQ NARGL (CONS W NARGL)))))
        (GO WHILELABEL))
      (COND ((NOT C) (RETURN GFALSE)))
      (RETURN
       (COND ((AND NARGL (CDR NARGL)) (CONS GAND NARGL))
             ((NULL NARGL) (COND ((EQ GAND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR NARGL)))))) 
(SWITCH (LIST 'HACK)) 
(PUT 'PASF_EXPANDA 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_EXPANDA 'DEFINED-ON-LINE '699) 
(PUT 'PASF_EXPANDA 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_EXPANDA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_EXPANDA (ANSW PHI)
    (PROG (GUARD W BADL GOODL GDIS NRANGEL)
      (PROG (A)
        (SETQ A ANSW)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ SECONDVALUE* NIL)
            (SETQ GUARD (PASF_EXPAND (CAR A)))
            (SETQ W SECONDVALUE*)
            (PROG (G136)
              (SETQ G136 (PASF_FINDSAMPLE (CADR A) (CADDR A) W))
              (SETQ NRANGEL (CAR G136))
              (SETQ ANSW (CDR G136))
              (RETURN G136))
            (COND
             (NRANGEL
              (SETQ BADL
                      (LTO_INSERT
                       (CONS GUARD
                             (REVERSIP
                              (CONS (CONS 'IMPLICIT (CONS 'LIST NRANGEL))
                                    (REVERSE ANSW))))
                       BADL)))
             (T (SETQ GOODL (LTO_INSERT (CONS GUARD ANSW) GOODL))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ GDIS
              (CL_SIMPL
               ((LAMBDA (G138)
                  (COND ((AND G138 (CDR G138)) (CONS 'OR G138))
                        ((NULL G138) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G138))))
                (PROG (GP FORALL-RESULT FORALL-ENDPTR)
                  (SETQ GP GOODL)
                  (COND ((NULL GP) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (GP) (CAR GP)) (CAR GP))
                                        NIL)))
                 LOOPLABEL
                  (SETQ GP (CDR GP))
                  (COND ((NULL GP) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (GP) (CAR GP)) (CAR GP)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               NIL (MINUS 1)))
      (COND
       (*RLQEASRI
        (SETQ BADL
                (PROG (GP FORALL-RESULT FORALL-ENDPTR)
                  (SETQ GP BADL)
                 STARTOVER
                  (COND ((NULL GP) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (GP)
                             (COND
                              ((PASF_SRIP (CAR GP) GDIS)
                               (PROGN
                                (COND (*RLVERBOSE (IOTO_PRIN2 "(SRI) ")))
                                NIL))
                              (T (LIST GP))))
                           (CAR GP)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ GP (CDR GP))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL GP) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (GP)
                             (COND
                              ((PASF_SRIP (CAR GP) GDIS)
                               (PROGN
                                (COND (*RLVERBOSE (IOTO_PRIN2 "(SRI) ")))
                                NIL))
                              (T (LIST GP))))
                           (CAR GP)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ GP (CDR GP))
                  (GO LOOPLABEL)))))
      (RETURN (NCONC (REVERSIP GOODL) (REVERSIP BADL))))) 
(PUT 'PASF_SRIP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SRIP 'DEFINED-ON-LINE '727) 
(PUT 'PASF_SRIP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SRIP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SRIP (PREM CONCL)
    (EQ (CL_SIMPL (LIST 'IMPL PREM CONCL) NIL (MINUS 1)) 'TRUE)) 
(PUT 'PASF_FINDSAMPLE 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_FINDSAMPLE 'DEFINED-ON-LINE '732) 
(PUT 'PASF_FINDSAMPLE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_FINDSAMPLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_FINDSAMPLE (RANGEL POINTS HITL)
    (PROG (W ANSW NRANGEL)
      (SETQ ANSW
              (PROG (POINT FORALL-RESULT FORALL-ENDPTR)
                (SETQ POINT POINTS)
                (COND ((NULL POINT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (POINT)
                                    (CONS (CAR POINT)
                                          (PREPSQ
                                           (SUBSQ (SIMP (CDR POINT)) HITL))))
                                  (CAR POINT))
                                 NIL)))
               LOOPLABEL
                (SETQ POINT (CDR POINT))
                (COND ((NULL POINT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (POINT)
                            (CONS (CAR POINT)
                                  (PREPSQ (SUBSQ (SIMP (CDR POINT)) HITL))))
                          (CAR POINT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NRANGEL
              (PROG (RANGE FORALL-RESULT FORALL-ENDPTR)
                (SETQ RANGE RANGEL)
               STARTOVER
                (COND ((NULL RANGE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (RANGE)
                           (PROGN
                            (SETQ W
                                    (CL_SIMPL (CL_SUBFOF HITL RANGE) NIL
                                              (MINUS 1)))
                            (COND
                             ((NOT (OR (EQ W 'TRUE) (EQ W 'FALSE)))
                              (LIST (RL_PREPFOF W))))))
                         (CAR RANGE)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ RANGE (CDR RANGE))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL RANGE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (RANGE)
                           (PROGN
                            (SETQ W
                                    (CL_SIMPL (CL_SUBFOF HITL RANGE) NIL
                                              (MINUS 1)))
                            (COND
                             ((NOT (OR (EQ W 'TRUE) (EQ W 'FALSE)))
                              (LIST (RL_PREPFOF W))))))
                         (CAR RANGE)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ RANGE (CDR RANGE))
                (GO LOOPLABEL)))
      (RETURN (CONS NRANGEL ANSW)))) 
(PUT 'PASF_ZSIMPL 'NUMBER-OF-ARGS 1) 
(DE PASF_ZSIMPL (F)
    (PROG (W Z FL FB BEST GLEQ GLESSP GONE)
      (SETQ W (CL_FVARL F))
      (COND
       ((CDR W) (REDERR (LIST "pasf_zsimpl: more than one variable: " W))))
      (SETQ Z (CAR W))
      (SETQ F (CL_DNF F))
      (SETQ FL
              (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR) (CDR F))
                    (T (LIST F))))
      (SETQ FB (PASF_ZSIMPL-FIRSTBOUND FL))
      (COND
       ((OR (EQ FB 'LESSP) (EQ FB 'LEQ))
        (PROGN (SETQ GLEQ 'LEQ) (SETQ GLESSP 'LESSP) (SETQ GONE 1)))
       ((OR (EQ FB 'GREATERP) (EQ FB 'GEQ))
        (PROGN (SETQ GLEQ 'GEQ) (SETQ GLESSP 'GREATERP) (SETQ GONE (MINUS 1))))
       (T (REDERR "pasf_zsimpl: cannot determine direction")))
      (PROG (ARG)
        (SETQ ARG FL)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG) (SETQ BEST (PASF_IMPROVE Z BEST ARG GLEQ GLESSP GONE)))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (RETURN (LIST GLEQ (CONS (CONS (CONS Z 1) 1) (MINUS BEST)) NIL)))) 
(PUT 'PASF_ZSIMPL-FIRSTBOUND 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ZSIMPL-FIRSTBOUND 'DEFINED-ON-LINE '775) 
(PUT 'PASF_ZSIMPL-FIRSTBOUND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_ZSIMPL-FIRSTBOUND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ZSIMPL-FIRSTBOUND (FL)
    (PROG (F OP FB ATL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT FB) FL)) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FL))
         (SETQ FL (CDR FL))
         (SETQ ATL
                 (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND) (CDR F))
                       (T (LIST F))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (NOT FB) ATL)) (RETURN NIL)))
           (PROGN
            (SETQ OP (COND ((ATOM (CAR ATL)) (CAR ATL)) (T (CAR (CAR ATL)))))
            (SETQ ATL (CDR ATL))
            (COND ((MEMQ OP '(LESSP LEQ GREATERP GEQ)) (SETQ FB OP))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN FB))) 
(PUT 'PASF_IMPROVE 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_IMPROVE 'DEFINED-ON-LINE '791) 
(PUT 'PASF_IMPROVE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_IMPROVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_IMPROVE (Z BEST ARG GLEQ GLESSP GONE)
    (PROG (OP ARGL TYPE CAND CONGL CONG)
      (SETQ ARGL
              (COND ((EQ (COND ((ATOM ARG) ARG) (T (CAR ARG))) 'AND) (CDR ARG))
                    (T (LIST ARG))))
      (PROG (AT)
        (SETQ AT ARGL)
       LAB
        (COND ((NULL AT) (RETURN NIL)))
        ((LAMBDA (AT)
           (PROGN
            (COND
             ((AND (PAIRP AT) (PAIRP (CAR AT)) (MEMQ (CAAR AT) '(CONG NCONG)))
              (SETQ CONGL (CONS AT CONGL)))
             (T
              (PROGN
               (SETQ OP (COND ((ATOM AT) AT) (T (CAR AT))))
               (COND
                ((EQ OP GLEQ)
                 (PROGN
                  (COND
                   (TYPE
                    (REDERR (LIST "pasf_improve: too many bounds in" ARG))))
                  (SETQ CAND (PASF_IMPROVE-GETVAL Z (CADR AT)))))
                ((EQ OP GLESSP)
                 (PROGN
                  (COND
                   (TYPE
                    (REDERR (LIST "pasf_improve: too many bounds in" ARG))))
                  (SETQ CAND
                          (DIFFERENCE (PASF_IMPROVE-GETVAL Z (CADR AT))
                                      GONE))))
                ((EQ OP 'EQUAL)
                 (PROGN
                  (COND
                   (TYPE
                    (REDERR (LIST "pasf_improve: too many bounds in" ARG))))
                  (SETQ CAND (PASF_IMPROVE-GETVAL Z (CADR AT)))))
                (T (REDERR (LIST "pasf_improve: unexpected operator" OP))))
               (SETQ TYPE OP))))))
         (CAR AT))
        (SETQ AT (CDR AT))
        (GO LAB))
      (COND ((AND BEST (EVAL (LIST GLEQ CAND BEST))) (RETURN BEST)))
      (SETQ CONG
              (COND ((AND CONGL (CDR CONGL)) (CONS 'AND CONGL))
                    ((NULL CONGL) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR CONGL))))
      (COND
       ((EQ TYPE 'EQUAL)
        (RETURN (COND ((PASF_IMPROVE-CONGP Z CAND CONG) CAND) (T BEST)))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (OR (NULL BEST) (EVAL (LIST GLESSP BEST CAND)))
                (NOT (PASF_IMPROVE-CONGP Z CAND CONG))))
          (RETURN NIL)))
        (SETQ CAND (DIFFERENCE CAND GONE))
        (GO WHILELABEL))
      (RETURN
       (COND ((OR (NULL BEST) (EVAL (LIST GLESSP BEST CAND))) CAND) (T BEST))))) 
(PUT 'PASF_IMPROVE-GETVAL 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_IMPROVE-GETVAL 'DEFINED-ON-LINE '823) 
(PUT 'PASF_IMPROVE-GETVAL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_IMPROVE-GETVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_IMPROVE-GETVAL (Z U)
    (PROGN
     (COND
      ((OR (NEQ (CAAAR U) Z) (NEQ (CDAAR U) 1) (NEQ (CDAR U) 1))
       (REDERR (LIST "pasf_improve: unexpected term " U))))
     (MINUS (CDR U)))) 
(PUT 'PASF_IMPROVE-CONGP 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_IMPROVE-CONGP 'DEFINED-ON-LINE '830) 
(PUT 'PASF_IMPROVE-CONGP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_IMPROVE-CONGP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_IMPROVE-CONGP (Z CAND CONG)
    (EQ (CL_SIMPL (CL_SUBFOF (LIST (CONS Z CAND)) CONG) NIL (MINUS 1)) 'TRUE)) 
(PUT 'PASF_PDP 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PDP 'DEFINED-ON-LINE '833) 
(PUT 'PASF_PDP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_PDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PDP (TERM)
    (PROG (C R)
      (COND
       ((OR (ATOM TERM) (ATOM (CAR TERM)))
        (RETURN
         (COND ((NULL TERM) 'INDEF) ((LESSP TERM 0) 'NDEF)
               ((GREATERP TERM 0) 'PDEF) (T 'INDEF)))))
      (COND
       ((EVENP (CDAAR TERM))
        (PROGN
         (SETQ C (PASF_PDP (CDAR TERM)))
         (SETQ R (PASF_PDP (CDR TERM)))
         (COND
          ((AND (NULL R) (OR (EQ C 'PSDEF) (EQ C 'PDEF))) (RETURN 'PSDEF)))
         (COND
          ((AND (NULL R) (OR (EQ C 'NSDEF) (EQ C 'NDEF))) (RETURN 'NSDEF)))
         (COND
          ((AND (EQ R 'PDEF) (OR (EQ C 'PSDEF) (EQ C 'PDEF))) (RETURN 'PDEF)))
         (COND
          ((AND (EQ R 'NDEF) (OR (EQ C 'NSDEF) (EQ C 'NDEF)))
           (RETURN 'NDEF))))))
      (RETURN 'INDEF))) 
(PUT 'PASF_SUBFOF 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SUBFOF 'DEFINED-ON-LINE '855) 
(PUT 'PASF_SUBFOF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SUBFOF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SUBFOF (VAR EX F) (CL_APPLY2ATS1 F 'PASF_SUBFOF1 (LIST VAR EX))) 
(PUT 'PASF_SUBFOF1 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SUBFOF1 'DEFINED-ON-LINE '862) 
(PUT 'PASF_SUBFOF1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SUBFOF1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SUBFOF1 (ATF VAR EX)
    (LIST
     (COND
      ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
       (CONS
        (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
              ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
        (CAR (SUBF (CDAR ATF) (LIST (CONS VAR EX))))))
      (T
       (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
             ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))))
     (CAR (SUBF (CADR ATF) (LIST (CONS VAR EX))))
     (CAR (SUBF (CADDR ATF) (LIST (CONS VAR EX)))))) 
(DE PASF_NEWVAR (F)
    ((LAMBDA (W) (PROGN (COND (*RLGENSYMINTERN (INTERN W)) (T (REMOB W))) W))
     (COMPRESS
      (CONS '!
            (CONS '_
                  (CONS 'K
                        (EXPLODE
                         (SETCDR RLGENSYMFAST*
                                 (PLUS (CDR RLGENSYMFAST*) 1))))))))) 
(PUT 'PASF_NEWVAR 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_NEWVAR 'DEFINED-ON-LINE '874) 
(PUT 'PASF_NEWVAR 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_NEWVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'PASF_NEWVAR 'INLINE
      '(LAMBDA (F)
         ((LAMBDA (W)
            (PROGN (COND (*RLGENSYMINTERN (INTERN W)) (T (REMOB W))) W))
          (COMPRESS
           (CONS '!
                 (CONS '_
                       (CONS 'K
                             (EXPLODE
                              (SETCDR RLGENSYMFAST*
                                      (PLUS (CDR RLGENSYMFAST*) 1)))))))))) 
(PUT 'PASF_NEWVAR1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_NEWVAR1 'DEFINED-ON-LINE '879) 
(PUT 'PASF_NEWVAR1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_NEWVAR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_NEWVAR1 (F)
    (PROG (VARL VARV EXPLD L)
      (SETQ VARL (CL_VARL F))
      (SETQ VARV 0)
      (PROG (VAR)
        (SETQ VAR (APPEND (CAR VARL) (CDR VARL)))
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROGN
            (SETQ EXPLD (EXPLODE VAR))
            (COND
             ((EQ (CAR EXPLD) 'K)
              (PROGN
               (SETQ L (IMPLODE (CDR EXPLD)))
               (COND ((GEQ L VARV) (SETQ VARV (PLUS L 1)))))))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (RETURN (IMPLODE (CONS 'K (EXPLODE VARV)))))) 
(PUT 'PASF_CAUCHYBND 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_CAUCHYBND 'DEFINED-ON-LINE '898) 
(PUT 'PASF_CAUCHYBND 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_CAUCHYBND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_CAUCHYBND (P X)
    (PROG (CL RES)
      (SETQ CL (PASF_COEFLST P X))
      (PROG (P)
        (SETQ P (CDR CL))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ RES (ADDF RES (EXPTF (CAR P) 2)))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (ADDF RES 1)))) 
(PUT 'PASF_CAUCHYBNDCL 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CAUCHYBNDCL 'DEFINED-ON-LINE '909) 
(PUT 'PASF_CAUCHYBNDCL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_CAUCHYBNDCL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CAUCHYBNDCL (CL)
    (PROG (RES)
      (PROG (P)
        (SETQ P (CDR CL))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ RES (ADDF RES (EXPTF (CAR P) 2)))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (ADDF RES 1)))) 
(PUT 'PASF_COEFLST 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_COEFLST 'DEFINED-ON-LINE '921) 
(PUT 'PASF_COEFLST 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_COEFLST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_COEFLST (P X)
    (PROG (OLDKORD NEXPR RES)
      (SETQ OLDKORD (SETKORDER (LIST X)))
      (SETQ NEXPR (REORDER P))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM NEXPR) (ATOM (CAR NEXPR))))
                (EQ (CAAAR NEXPR) X)))
          (RETURN NIL)))
        (PROGN
         (SETQ RES (CONS (CONS (CDAR NEXPR) (CDAAR NEXPR)) RES))
         (SETQ NEXPR (CDR NEXPR)))
        (GO WHILELABEL))
      (SETKORDER OLDKORD)
      (RETURN (REVERSIP (CONS (CONS (NEGF NEXPR) 0) RES))))) 
(PUT 'REPR_NEW 'NUMBER-OF-ARGS 4) 
(PUT 'REPR_NEW 'DEFINED-ON-LINE '947) 
(PUT 'REPR_NEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_NEW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPR_NEW (POS OP CL TN)
    (LIST POS OP CL TN
          (COND
           ((NULL CL) (REDERR (LIST "repr_new : invalid coefficient list")))
           (T (CDAR CL))))) 
(PUT 'REPR_EQ 'NUMBER-OF-ARGS 2) 
(PUT 'REPR_EQ 'DEFINED-ON-LINE '958) 
(PUT 'REPR_EQ 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_EQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPR_EQ (REPR1 REPR2) (EQ (CDR REPR1) (CDR REPR2))) 
(PUT 'REPR_POS 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_POS 'DEFINED-ON-LINE '964) 
(PUT 'REPR_POS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_POS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_POS (REPR) (CAR REPR)) 
(PUT 'REPR_SETPOS 'NUMBER-OF-ARGS 2) 
(PUT 'REPR_SETPOS 'DEFINED-ON-LINE '969) 
(PUT 'REPR_SETPOS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_SETPOS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REPR_SETPOS (REPR POS) (CONS POS (CDR REPR))) 
(PUT 'REPR_OP 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_OP 'DEFINED-ON-LINE '975) 
(PUT 'REPR_OP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_OP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_OP (REPR) (CADR REPR)) 
(PUT 'REPR_LDEG 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_LDEG 'DEFINED-ON-LINE '980) 
(PUT 'REPR_LDEG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_LDEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_LDEG (REPR) (CAR (CDDDDR REPR))) 
(PUT 'REPR_N 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_N 'DEFINED-ON-LINE '985) 
(PUT 'REPR_N 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_N 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_N (REPR)
    (COND
     ((NULL (CADDR REPR)) (REDERR (LIST "repr_n : invalid REPR structure")))
     ((EQUAL (CAR (CDDDDR REPR)) 0) NIL) (T (CAAR (CADDR REPR))))) 
(PUT 'REPR_R 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_R 'DEFINED-ON-LINE '999) 
(PUT 'REPR_R 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_R 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_R (REPR) (CAAR (REVERSE (CADDR REPR)))) 
(PUT 'REPR_CL 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_CL 'DEFINED-ON-LINE '1004) 
(PUT 'REPR_CL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_CL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_CL (REPR) (CADDR REPR)) 
(PUT 'REPR_T 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_T 'DEFINED-ON-LINE '1009) 
(PUT 'REPR_T 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_T 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_T (REPR) (CADDDR REPR)) 
(PUT 'REPR_A 'NUMBER-OF-ARGS 1) 
(PUT 'REPR_A 'DEFINED-ON-LINE '1014) 
(PUT 'REPR_A 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPR_A (REPR) (ADDF (CAAR (REVERSE (CADDR REPR))) (CADDDR REPR))) 
(PUT 'REPR_ATFNEW 'NUMBER-OF-ARGS 3) 
(PUT 'REPR_ATFNEW 'DEFINED-ON-LINE '1019) 
(PUT 'REPR_ATFNEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_ATFNEW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPR_ATFNEW (ATF X POS)
    (PROG (OP CL)
      (SETQ OP (CAR ATF))
      (SETQ CL (PASF_COEFLST (CADR ATF) X))
      (COND
       ((MINUSF (CAAR CL))
        (PROGN
         (SETQ OP
                 (COND
                  ((AND (PAIRP ATF) (PAIRP (CAR ATF))
                        (MEMQ (CAAR ATF) '(CONG NCONG)))
                   (CONS (PASF_ANEGREL (CAR OP)) (CDR OP)))
                  (T (PASF_ANEGREL OP))))
         (SETQ CL
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C CL)
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (CONS
                                        ((LAMBDA (G140)
                                           (COND
                                            (*PHYSOP-LOADED
                                             (PHYSOP-MULTF (CAR C) G140))
                                            (T (POLY-MULTF (CAR C) G140))))
                                         (MINUS 1))
                                        (CDR C)))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C)
                               (CONS
                                ((LAMBDA (G140)
                                   (COND
                                    (*PHYSOP-LOADED
                                     (PHYSOP-MULTF (CAR C) G140))
                                    (T (POLY-MULTF (CAR C) G140))))
                                 (MINUS 1))
                                (CDR C)))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         NIL)))
      (RETURN (REPR_NEW POS OP CL NIL)))) 
(PUT 'REPR_ATFBNEW 'NUMBER-OF-ARGS 4) 
(PUT 'REPR_ATFBNEW 'DEFINED-ON-LINE '1040) 
(PUT 'REPR_ATFBNEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'REPR_ATFBNEW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPR_ATFBNEW (ATF X POS BVL)
    (PROG (RP R TM TMP)
      (COND
       ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE))
        (RETURN (REPR_NEW POS ATF NIL NIL))))
      (SETQ RP (REPR_ATFNEW ATF X POS))
      (SETQ R (REPR_A RP))
      (PROG (V)
        (SETQ V BVL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ TMP (PASF_COEFLST R (CDR V)))
            (COND
             ((GREATERP (LENGTH TMP) 1)
              (SETQ TM
                      (ADDF TM
                            ((LAMBDA (G141)
                               (COND
                                (*PHYSOP-LOADED (PHYSOP-MULTF G141 (CAAR TMP)))
                                (T (POLY-MULTF G141 (CAAR TMP)))))
                             (CAR (SIMP (CDR V))))))))
            (SETQ R (CAR (SUBF R (LIST (CONS (CDR V) NIL)))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN
       (REPR_NEW POS (REPR_OP RP)
        (REVERSIP (CONS (CONS R 0) (CDR (REVERSE (REPR_CL RP))))) TM)))) 
(PUT 'FDEC_NEW 'NUMBER-OF-ARGS 2) 
(PUT 'FDEC_NEW 'DEFINED-ON-LINE '1068) 
(PUT 'FDEC_NEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'FDEC_NEW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FDEC_NEW (F X)
    (PROG (BVL POS BTL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
            (COND ((ATOM F) F) (T (CAR F)))))
          (RETURN NIL)))
        (PROGN
         (COND
          ((MEMQ X (RL_FVARL (CADDDR F)))
           (REDERR
            (LIST "Quantified variable" X
                  "is not allowed inside formula's bound"))))
         (SETQ BVL (CONS (CONS (CADDDR F) (CADR F)) BVL))
         (SETQ POS (APPEND POS (LIST 0)))
         (SETQ BTL (CONS (COND ((ATOM F) F) (T (CAR F))) BTL))
         (SETQ F (CADDR F)))
        (GO WHILELABEL))
      (RETURN (LIST F POS BVL BTL)))) 
(PUT 'FDEC_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'FDEC_MAT 'DEFINED-ON-LINE '1088) 
(PUT 'FDEC_MAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'FDEC_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FDEC_MAT (FDEC) (CAR FDEC)) 
(PUT 'FDEC_POS 'NUMBER-OF-ARGS 1) 
(PUT 'FDEC_POS 'DEFINED-ON-LINE '1093) 
(PUT 'FDEC_POS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'FDEC_POS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FDEC_POS (FDEC) (CADR FDEC)) 
(PUT 'FDEC_BVL 'NUMBER-OF-ARGS 1) 
(PUT 'FDEC_BVL 'DEFINED-ON-LINE '1099) 
(PUT 'FDEC_BVL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'FDEC_BVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FDEC_BVL (FDEC) (CADDR FDEC)) 
(PUT 'FDEC_BOPL 'NUMBER-OF-ARGS 1) 
(PUT 'FDEC_BOPL 'DEFINED-ON-LINE '1105) 
(PUT 'FDEC_BOPL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'FDEC_BOPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FDEC_BOPL (FDEC) (CADDDR FDEC)) 
(PUT 'ELIMPT_NEW 'NUMBER-OF-ARGS 6) 
(PUT 'ELIMPT_NEW 'DEFINED-ON-LINE '1115) 
(PUT 'ELIMPT_NEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_NEW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELIMPT_NEW (POS GUARD NOM DEN BVL UNIF) (LIST POS GUARD NOM DEN BVL UNIF)) 
(PUT 'ELIMPT_POS 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_POS 'DEFINED-ON-LINE '1125) 
(PUT 'ELIMPT_POS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_POS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_POS (ELIMPT) (CAR ELIMPT)) 
(PUT 'ELIMPT_CPOS 'NUMBER-OF-ARGS 2) 
(PUT 'ELIMPT_CPOS 'DEFINED-ON-LINE '1130) 
(PUT 'ELIMPT_CPOS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_CPOS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIMPT_CPOS (ELIMPT1 ELIMPT2)
    (PROG (POS P1 P2)
      (SETQ P1 (CAR ELIMPT1))
      (SETQ P2 (CAR ELIMPT2))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P1 P2 (EQ (CAR P1) (CAR P2)))) (RETURN NIL)))
        (PROGN
         (SETQ POS (CONS (CAR P1) POS))
         (SETQ P1 (CDR P1))
         (SETQ P2 (CDR P2)))
        (GO WHILELABEL))
      (RETURN (REVERSE POS)))) 
(PUT 'ELIMPT_GUARD 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_GUARD 'DEFINED-ON-LINE '1145) 
(PUT 'ELIMPT_GUARD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_GUARD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_GUARD (ELIMPT) (CADR ELIMPT)) 
(PUT 'ELIMPT_NOM 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_NOM 'DEFINED-ON-LINE '1150) 
(PUT 'ELIMPT_NOM 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_NOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_NOM (ELIMPT) (CADDR ELIMPT)) 
(PUT 'ELIMPT_DEN 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_DEN 'DEFINED-ON-LINE '1155) 
(PUT 'ELIMPT_DEN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_DEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_DEN (ELIMPT) (CADDDR ELIMPT)) 
(PUT 'ELIMPT_BVL 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_BVL 'DEFINED-ON-LINE '1160) 
(PUT 'ELIMPT_BVL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_BVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_BVL (ELIMPT) (CAR (CDDDDR ELIMPT))) 
(PUT 'ELIMPT_UNIF 'NUMBER-OF-ARGS 1) 
(PUT 'ELIMPT_UNIF 'DEFINED-ON-LINE '1165) 
(PUT 'ELIMPT_UNIF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ELIMPT_UNIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIMPT_UNIF (ELIMPT) (CADR (CDDDDR ELIMPT))) 
(PUT 'ANSW_NEW 'NUMBER-OF-ARGS 3) 
(PUT 'ANSW_NEW 'DEFINED-ON-LINE '1174) 
(PUT 'ANSW_NEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ANSW_NEW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ANSW_NEW (F BL TL) (LIST F BL TL)) 
(PUT 'ANSW_F 'NUMBER-OF-ARGS 1) 
(PUT 'ANSW_F 'DEFINED-ON-LINE '1180) 
(PUT 'ANSW_F 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ANSW_F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANSW_F (ANSW) (CAR ANSW)) 
(PUT 'ANSW_BL 'NUMBER-OF-ARGS 1) 
(PUT 'ANSW_BL 'DEFINED-ON-LINE '1185) 
(PUT 'ANSW_BL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ANSW_BL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANSW_BL (ANSW) (CADR ANSW)) 
(PUT 'ANSW_TL 'NUMBER-OF-ARGS 1) 
(PUT 'ANSW_TL 'DEFINED-ON-LINE '1190) 
(PUT 'ANSW_TL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ANSW_TL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANSW_TL (ANSW) (CADDR ANSW)) 
(PUT 'ANSW_BACKSUBST 'NUMBER-OF-ARGS 2) 
(PUT 'ANSW_BACKSUBST 'DEFINED-ON-LINE '1195) 
(PUT 'ANSW_BACKSUBST 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'ANSW_BACKSUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ANSW_BACKSUBST (ANSW1 ANSW2)
    (PROG (RES SUB VAR)
      (COND ((AND (NULL ANSW2) ANSW1) (RETURN ANSW1))
            ((NULL ANSW1) (REDERR (LIST "incorrect ANSW structure"))))
      (SETQ SUB
              (LIST
               (CONS (PREPF (CADR (CAADDR ANSW1)))
                     (PREPSQ (CADDR (CAADDR ANSW1))))))
      (SETQ RES
              (PROG (EQN FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQN (CADDR ANSW2))
                (COND ((NULL EQN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQN)
                                    (LIST 'EQUAL (CADR EQN)
                                          (SUBSQ (CADDR EQN) SUB)))
                                  (CAR EQN))
                                 NIL)))
               LOOPLABEL
                (SETQ EQN (CDR EQN))
                (COND ((NULL EQN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQN)
                            (LIST 'EQUAL (CADR EQN) (SUBSQ (CADDR EQN) SUB)))
                          (CAR EQN))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (LIST (CAR ANSW1) (APPEND (CADR ANSW1) (CADR ANSW2))
             (CONS (CAADDR ANSW1) RES))))) 
(PUT 'IV_NEW 'NUMBER-OF-ARGS 2) 
(PUT 'IV_NEW 'DEFINED-ON-LINE '1217) 
(PUT 'IV_NEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_NEW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_NEW (LB RB) (LIST (CONS (COND (LB LB) (T 0)) (COND (RB RB) (T 0))))) 
(PUT 'IV_NEWCONG 'NUMBER-OF-ARGS 2) 
(PUT 'IV_NEWCONG 'DEFINED-ON-LINE '1223) 
(PUT 'IV_NEWCONG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_NEWCONG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_NEWCONG (OP CLASS) (LIST (CONS OP (COND (CLASS CLASS) (T 0))))) 
(PUT 'IV_CONGP 'NUMBER-OF-ARGS 1) 
(PUT 'IV_CONGP 'DEFINED-ON-LINE '1230) 
(PUT 'IV_CONGP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CONGP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_CONGP (IVL) (COND (IVL (OR (PAIRP (CAAR IVL)) (IV_CONGP (CDR IVL)))))) 
(PUT 'IV_EMPTY 'NUMBER-OF-ARGS 1) 
(PUT 'IV_EMPTY 'DEFINED-ON-LINE '1236) 
(PUT 'IV_EMPTY 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_EMPTY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_EMPTY (IVL) (NOT IVL)) 
(PUT 'IV_CONGSPLITL 'NUMBER-OF-ARGS 1) 
(PUT 'IV_CONGSPLITL 'DEFINED-ON-LINE '1241) 
(PUT 'IV_CONGSPLITL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CONGSPLITL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_CONGSPLITL (IVL)
    (PROG (SPLIT REST)
      (COND (IVL (RETURN (CONS NIL NIL))))
      (SETQ SPLIT (IV_CONGSPLIT (CAR IVL)))
      (SETQ REST (IV_CONGSPLITL (CDR IVL)))
      (RETURN
       (CONS (CONS (CAR SPLIT) (CAR REST)) (CONS (CDR SPLIT) (CDR REST)))))) 
(PUT 'IV_CONGSPLIT 'NUMBER-OF-ARGS 1) 
(PUT 'IV_CONGSPLIT 'DEFINED-ON-LINE '1254) 
(PUT 'IV_CONGSPLIT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CONGSPLIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_CONGSPLIT (IV)
    (COND
     (IV
      (COND
       ((IV_CONGP (LIST (CAR IV)))
        (CONS (CAR (IV_CONGSPLIT (CDR IV)))
              (CONS (CAR IV) (CDR (IV_CONGSPLIT (CDR IV))))))
       (T
        (CONS (CONS (CAR IV) (CAR (IV_CONGSPLIT (CDR IV))))
              (CDR (IV_CONGSPLIT (CDR IV)))))))
     (T (CONS NIL NIL)))) 
(PUT 'IV_CUTN 'NUMBER-OF-ARGS 1) 
(PUT 'IV_CUTN 'DEFINED-ON-LINE '1266) 
(PUT 'IV_CUTN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CUTN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_CUTN (IVL)
    (COND ((CDR IVL) (IV_CUT (CAR IVL) (IV_CUTN (CDR IVL)))) (T (CAR IVL)))) 
(PUT 'IV_CUT 'NUMBER-OF-ARGS 2) 
(PUT 'IV_CUT 'DEFINED-ON-LINE '1275) 
(PUT 'IV_CUT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CUT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_CUT (IV1 IV2)
    (PROG (CURR LOWER RES)
      (COND ((OR (IV_EMPTY IV1) (IV_EMPTY IV2)) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (AND (IV_EMPTY IV1) (IV_EMPTY IV2)))) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (IV_EMPTY IV2)
               (AND (NOT (IV_EMPTY IV1)) (PASF_LEQP (CAAR IV1) (CAAR IV2))))
           (PROGN (SETQ LOWER (CAR IV1)) (SETQ IV1 (CDR IV1))))
          (T (PROGN (SETQ LOWER (CAR IV2)) (SETQ IV2 (CDR IV2)))))
         (COND ((NULL CURR) (SETQ CURR LOWER))
               ((PASF_LEQ (CDR CURR) (CAR LOWER)) (SETQ CURR LOWER))
               ((PASF_LEQP (CDR CURR) (CDR LOWER))
                (PROGN
                 (SETQ RES (CONS (CONS (CAR LOWER) (CDR CURR)) RES))
                 (SETQ CURR LOWER)))
               (T (SETQ RES (CONS LOWER RES)))))
        (GO WHILELABEL))
      (RETURN (REVERSE RES)))) 
(PUT 'IV_CUTCONGS 'NUMBER-OF-ARGS 2) 
(PUT 'IV_CUTCONGS 'DEFINED-ON-LINE '1313) 
(PUT 'IV_CUTCONGS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CUTCONGS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_CUTCONGS (IVL CONGS)
    (PROG (CURR RES)
      (COND ((NOT CONGS) (RETURN IVL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (IV_EMPTY IVL))) (RETURN NIL)))
        (PROGN
         (PROG (I)
           (SETQ I (CAAR IVL))
          LAB
           (COND ((MINUSP (DIFFERENCE (CDAR IVL) I)) (RETURN NIL)))
           (PROGN
            (IV_CUTCONGS1 I CONGS)
            (COND
             ((IV_CUTCONGS1 I CONGS)
              (COND (CURR (SETQ CURR (CONS (CAR CURR) I)))
                    (T (SETQ CURR (CONS I I)))))
             (CURR (PROGN (SETQ RES (CONS CURR RES)) (SETQ CURR NIL)))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((AND (NULL (CDR IVL)) CURR)
           (SETQ RES (CONS (CONS (CAR CURR) (CDAR IVL)) RES))))
         (SETQ IVL (CDR IVL)))
        (GO WHILELABEL))
      (RETURN (REVERSE RES)))) 
(PUT 'IV_CUTCONGS1 'NUMBER-OF-ARGS 2) 
(PUT 'IV_CUTCONGS1 'DEFINED-ON-LINE '1342) 
(PUT 'IV_CUTCONGS1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CUTCONGS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_CUTCONGS1 (VAL CONGS)
    (COND
     (CONGS
      (AND (IV_CUTCONGS2 VAL (CAR CONGS)) (IV_CUTCONGS1 VAL (CDR CONGS))))
     (T T))) 
(PUT 'IV_CUTCONGS2 'NUMBER-OF-ARGS 2) 
(PUT 'IV_CUTCONGS2 'DEFINED-ON-LINE '1351) 
(PUT 'IV_CUTCONGS2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_CUTCONGS2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_CUTCONGS2 (VAL CONG)
    (COND
     ((EQ (CAAR CONG) 'CONG)
      (EQUAL (REMAINDER (DIFFERENCE (CDR CONG) VAL) (CDAR CONG)) 0))
     (T (NOT (EQUAL (REMAINDER (DIFFERENCE (CDR CONG) VAL) (CDAR CONG)) 0))))) 
(PUT 'IV_MERGEN 'NUMBER-OF-ARGS 1) 
(PUT 'IV_MERGEN 'DEFINED-ON-LINE '1360) 
(PUT 'IV_MERGEN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_MERGEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_MERGEN (IVL)
    (COND ((CDR IVL) (IV_MERGE (CAR IVL) (IV_MERGEN (CDR IVL)))) (T (CAR IVL)))) 
(PUT 'IV_MERGE 'NUMBER-OF-ARGS 2) 
(PUT 'IV_MERGE 'DEFINED-ON-LINE '1369) 
(PUT 'IV_MERGE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'IV_MERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IV_MERGE (IV1 IV2)
    (PROG (CURR LOWER RES)
      (COND
       ((OR (IV_CONGP IV1) (IV_CONGP IV2))
        (REDERR (LIST "iv_merge : merging a congruence not possible }"))))
      (COND ((AND (IV_EMPTY IV1) (IV_EMPTY IV2)) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (AND (IV_EMPTY IV1) (IV_EMPTY IV2)))) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (IV_EMPTY IV2)
               (AND (NOT (IV_EMPTY IV1)) (PASF_LEQP (CAAR IV1) (CAAR IV2))))
           (PROGN (SETQ LOWER (CAR IV1)) (SETQ IV1 (CDR IV1))))
          (T (PROGN (SETQ LOWER (CAR IV2)) (SETQ IV2 (CDR IV2)))))
         (COND ((NOT CURR) (SETQ CURR LOWER))
               ((PASF_LEQ (CDR CURR) (CAR LOWER))
                (PROGN (SETQ RES (CONS CURR RES)) (SETQ CURR LOWER)))
               ((PASF_LEQP (CDR CURR) (CDR LOWER))
                (SETQ CURR (CONS (CAR CURR) (CDR LOWER))))))
        (GO WHILELABEL))
      (RETURN (REVERSE (CONS CURR RES))))) 
(PUT 'PASF_STEX 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_STEX 'DEFINED-ON-LINE '1410) 
(PUT 'PASF_STEX 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_STEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_STEX (F)
    (CL_APPLY2ATS1 F (FUNCTION PASF_STEXAT) (LIST (CONS NIL NIL)))) 
(PUT 'PASF_STEXAT 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_STEXAT 'DEFINED-ON-LINE '1413) 
(PUT 'PASF_STEXAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_STEXAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_STEXAT (AT RNDALPAIR)
    (PROG (AL LHS W)
      (SETQ AL (CAR RNDALPAIR))
      (SETQ LHS (CADR AT))
      (SETQ W (PASF_STEXF LHS AL))
      (SETCAR RNDALPAIR (CDR W))
      (RETURN (LIST (CAR AT) (CAR W) NIL)))) 
(PUT 'PASF_STEXF 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_STEXF 'DEFINED-ON-LINE '1422) 
(PUT 'PASF_STEXF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_STEXF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_STEXF (U AL)
    (PROG (W C R)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (CONS U AL))))
      (SETQ W (PASF_STEXF (CDAR U) AL))
      (SETQ AL (CDR W))
      (SETQ C (CAR W))
      (SETQ W (PASF_STEXF (CDR U) AL))
      (SETQ AL (CDR W))
      (SETQ R (CAR W))
      (SETQ W (PASF_STEXK (CAAAR U) AL))
      (RETURN
       (CONS
        (ADDF
         ((LAMBDA (G144)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF C G144))
                  (T (POLY-MULTF C G144))))
          (EXPTF (CAR W) (CDAAR U)))
         R)
        (CDR W))))) 
(PUT 'PASF_STEXK 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_STEXK 'DEFINED-ON-LINE '1436) 
(PUT 'PASF_STEXK 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_STEXK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_STEXK (K AL)
    (PROG (W)
      (COND ((IDP K) (RETURN (CONS (LIST (CONS (CONS K 1) 1)) AL))))
      (SETQ W (ATSOC (CADDR K) AL))
      (COND (W (RETURN (CONS (CDR W) AL))))
      (COND
       ((NOT (OR (ATOM (CADR K)) (ATOM (CAR (CADR K)))))
        (REDERR (LIST "pasf_stexk:" (CADR K) "is not a number"))))
      (SETQ W (RANDOM (PLUS (CADR K) 1)))
      (RETURN (CONS W (CONS (CONS (CADDR K) W) AL))))) 
(PUT 'PASF_STRUCTAT 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_STRUCTAT 'DEFINED-ON-LINE '1450) 
(PUT 'PASF_STRUCTAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_STRUCTAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_STRUCTAT (AT AL)
    (PROG (LHS)
      (SETQ LHS (CADR AT))
      (COND ((OR (ATOM LHS) (ATOM (CAR LHS))) (RETURN AT)))
      (RETURN (LIST (CAR AT) (CAR (SIMP (CDR (ASSOC LHS AL)))) NIL)))) 
(PUT 'PASF_SMT2PRINTQF 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SMT2PRINTQF 'DEFINED-ON-LINE '1463) 
(PUT 'PASF_SMT2PRINTQF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTQF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SMT2PRINTQF (F FNAME LINEL)
    (PROG (VL)
      (COND (FNAME (OUT (LIST FNAME))))
      (PRIN2T "(set-logic QF_AUFLIA)")
      (COND
       (LINEL
        (PROG (LINE)
          (SETQ LINE LINEL)
         LAB
          (COND ((NULL LINE) (RETURN NIL)))
          ((LAMBDA (LINE) (PRIN2T LINE)) (CAR LINE))
          (SETQ LINE (CDR LINE))
          (GO LAB)))
       (T (PRIN2T "(set-info :source | automatically generated by REDLOG |)")))
      (SETQ VL (CL_VARL1 F))
      (COND
       ((CDR VL) (REDERR "pasf_smt2PrintQf: Formula is NOT quantifier-free!")))
      (SETQ VL (CAR VL))
      (PROG ()
       WHILELABEL
        (COND ((NOT VL) (RETURN NIL)))
        (PROGN (PASF_SMT2PRINTV (CAR VL)) (SETQ VL (CDR VL)))
        (GO WHILELABEL))
      (PASF_SMT2PRINT1 F)
      (PRIN2T "(check-sat)")
      (COND (FNAME (SHUT (LIST FNAME)))))) 
(PUT 'PASF_SMT2PRINTV 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2PRINTV 'DEFINED-ON-LINE '1490) 
(PUT 'PASF_SMT2PRINTV 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2PRINTV (V)
    (PROGN
     (PRIN2 "(declare-fun ")
     (PRIN2
      (COMPRESS (CONS '|"| (REVERSIP (CONS '|"| (REVERSIP (EXPLODE V)))))))
     (PRIN2T " () Int)")
     NIL)) 
(PUT 'PASF_SMT2PRINT 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SMT2PRINT 'DEFINED-ON-LINE '1497) 
(PUT 'PASF_SMT2PRINT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SMT2PRINT (F FNAME LINEL)
    (PROGN
     (COND (FNAME (OUT (LIST FNAME))))
     (PASF_SMT2PRINTLOGIC)
     (COND
      (LINEL
       (PROG (LINE)
         (SETQ LINE LINEL)
        LAB
         (COND ((NULL LINE) (RETURN NIL)))
         ((LAMBDA (LINE) (PRIN2T LINE)) (CAR LINE))
         (SETQ LINE (CDR LINE))
         (GO LAB)))
      (T (PRIN2T "(set-info :source | automatically generated by REDLOG |)")))
     (PASF_SMT2PRINT1 F)
     (PRIN2T "(check-sat)")
     (COND (FNAME (SHUT (LIST FNAME)))))) 
(PUT 'PASF_SMT2PRINTLOGIC 'NUMBER-OF-ARGS 0) 
(PUT 'PASF_SMT2PRINTLOGIC 'DEFINED-ON-LINE '1515) 
(PUT 'PASF_SMT2PRINTLOGIC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTLOGIC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PASF_SMT2PRINTLOGIC NIL (PRIN2T "(set-logic AUFLIA)")) 
(PUT 'PASF_SMT2PRINT1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2PRINT1 'DEFINED-ON-LINE '1518) 
(PUT 'PASF_SMT2PRINT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2PRINT1 (F)
    (PROG (VL) (PRIN2 "(assert ") (PASF_SMT2PRINT2 F) (PRIN2T ")"))) 
(PUT 'PASF_SMT2PRINT2 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2PRINT2 'DEFINED-ON-LINE '1530) 
(PUT 'PASF_SMT2PRINT2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINT2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2PRINT2 (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((MEMQ OP '(BALL BEX))
        (REDERR (LIST "pasf_smt2Print2: bounded quantifiers not supported!")))
       ((MEMQ OP '(ALL EX)) (PASF_SMT2PREFIXPRINTQ OP (CADR F) (CADDR F)))
       ((EQ OP 'IMPL) (PASF_SMT2PREFIXPRINT "=>" (CDR F)))
       ((EQ OP 'REPL) (PASF_SMT2PREFIXPRINT "=>" (LIST (CADDR F) (CADR F))))
       ((EQ OP 'EQUIV)
        (PASF_SMT2PRINT2
         (CONS 'AND (LIST (CONS 'IMPL (CDR F)) (CONS 'REPL (CDR F))))))
       ((MEMQ OP '(NOT AND OR)) (PASF_SMT2PREFIXPRINT OP (CDR F)))
       ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (PRIN2 F)) (T (PASF_SMT2PRINTAT F))))) 
(PUT 'PASF_SMT2PREFIXPRINT 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SMT2PREFIXPRINT 'DEFINED-ON-LINE '1552) 
(PUT 'PASF_SMT2PREFIXPRINT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PREFIXPRINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SMT2PREFIXPRINT (OP ARGL)
    (PROGN
     (PRIN2 "(")
     (PRIN2 OP)
     (PRIN2 " ")
     (PROG (RARGL)
       (SETQ RARGL ARGL)
      LAB
       (COND ((NULL RARGL) (RETURN NIL)))
       (PROGN (PASF_SMT2PRINT2 (CAR RARGL)) (COND ((CDR RARGL) (PRIN2 " "))))
       (SETQ RARGL (CDR RARGL))
       (GO LAB))
     (PRIN2 ")"))) 
(PUT 'PASF_SMT2PREFIXPRINTQ 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SMT2PREFIXPRINTQ 'DEFINED-ON-LINE '1565) 
(PUT 'PASF_SMT2PREFIXPRINTQ 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PREFIXPRINTQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SMT2PREFIXPRINTQ (QUANT VAR ARG)
    (PROG (QUANTAL W)
      (PRIN2 "(")
      (SETQ QUANTAL '((ALL . "forall") (EX . "exists")))
      (PRIN2 (COND ((SETQ W (ATSOC QUANT QUANTAL)) (CDR W)) (T QUANT)))
      (PRIN2 " ((")
      (PRIN2 VAR)
      (PRIN2 " Int)) ")
      (PASF_SMT2PRINT2 ARG)
      (PRIN2 ")"))) 
(PUT 'PASF_SMT2PRINTAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2PRINTAT 'DEFINED-ON-LINE '1577) 
(PUT 'PASF_SMT2PRINTAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2PRINTAT (F)
    (PROG (OPAL OP LHS W)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (SETQ LHS (PREPF (CADR F)))
      (COND
       ((EQ OP 'NEQ) (PASF_SMT2PRINT2 (LIST 'NOT (LIST 'EQUAL (CADR F) NIL))))
       ((AND (PAIRP F) (PAIRP (CAR F)) (MEMQ (CAAR F) '(CONG NCONG)))
        (PROGN
         (PRIN2 "(= ")
         (PASF_SMT2PRINTT (LIST 'MODC LHS (CDAR F)))
         (PRIN2 " 0)")))
       (T
        (PROGN
         (SETQ OPAL
                 '((LESSP . "<") (LEQ . "<=") (GREATERP . ">") (GEQ . ">=")
                   (EQUAL . "=")))
         (PRIN2 "(")
         (PRIN2 (COND ((SETQ W (ATSOC OP OPAL)) (CDR W)) (T OP)))
         (PRIN2 " ")
         (PASF_SMT2PRINTT LHS)
         (PRIN2 " 0)")))))) 
(PUT 'PASF_SMT2PRINTT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2PRINTT 'DEFINED-ON-LINE '1599) 
(PUT 'PASF_SMT2PRINTT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2PRINTT (U)
    (COND ((OR (NUMBERP U) (IDP U)) (PRIN2 (COND ((NULL U) 0) (T U))))
          (T (PASF_SMT2PRINTT1 (CAR U) (CDR U))))) 
(PUT 'PASF_SMT2PRINTT1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_SMT2PRINTT1 'DEFINED-ON-LINE '1605) 
(PUT 'PASF_SMT2PRINTT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2PRINTT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_SMT2PRINTT1 (OP ARGL)
    (COND
     ((EQ OP 'MODC)
      (PROGN
       (PRIN2 "(mod ")
       (PASF_SMT2PRINTT (CAR ARGL))
       (PRIN2 " ")
       (PASF_SMT2PRINTT (CADR ARGL))
       (PRIN2 ")")))
     ((EQ OP 'DIVC)
      (PROGN
       (PRIN2 "(div ")
       (PASF_SMT2PRINTT (CAR ARGL))
       (PRIN2 " ")
       (PASF_SMT2PRINTT (CADR ARGL))
       (PRIN2 ")")))
     ((EQ OP 'DIFFERENCE)
      (PROGN
       (PRIN2 "(- ")
       (PASF_SMT2PRINTT (CAR ARGL))
       (PRIN2 " ")
       (PASF_SMT2PRINTT (CADR ARGL))
       (PRIN2 ")")))
     ((EQ OP 'MINUS)
      (PROGN (PRIN2 "(- ") (PASF_SMT2PRINTT (CAR ARGL)) (PRIN2 ")")))
     ((EQ OP 'PLUS)
      (PROGN
       (PRIN2 "(+ ")
       (PROG (RARGL)
         (SETQ RARGL ARGL)
        LAB
         (COND ((NULL RARGL) (RETURN NIL)))
         (PROGN (PASF_SMT2PRINTT (CAR RARGL)) (COND ((CDR RARGL) (PRIN2 " "))))
         (SETQ RARGL (CDR RARGL))
         (GO LAB))
       (PRIN2 ")")))
     ((EQ OP 'TIMES)
      (PROGN
       (PRIN2 "(* ")
       (PROG (RARGL)
         (SETQ RARGL ARGL)
        LAB
         (COND ((NULL RARGL) (RETURN NIL)))
         (PROGN (PASF_SMT2PRINTT (CAR RARGL)) (COND ((CDR RARGL) (PRIN2 " "))))
         (SETQ RARGL (CDR RARGL))
         (GO LAB))
       (PRIN2 ")")))
     ((EQ OP 'EXPT)
      (PASF_SMT2PRINTT1 'TIMES
       (PROG (I FORALL-RESULT FORALL-ENDPTR)
         (SETQ I 1)
         (COND ((MINUSP (DIFFERENCE (CADR ARGL) I)) (RETURN NIL)))
         (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (CAR ARGL) NIL)))
        LOOPLABEL
         (SETQ I (PLUS2 I 1))
         (COND ((MINUSP (DIFFERENCE (CADR ARGL) I)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS (CAR ARGL) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'PASF_SMT2READAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2READAT 'DEFINED-ON-LINE '1647) 
(PUT 'PASF_SMT2READAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2READAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2READAT (FORM)
    (PROG (OP W LHS RHS)
      (SETQ OP (CAR FORM))
      (SETQ W
              (ATSOC OP
                     '((>= . GEQ) (<= . LEQ) (< . LESSP) (> . GREATERP)
                       (= . EQUAL))))
      (COND
       ((NOT W)
        (CL_SMT2READERROR
         (LIST "error: expecting logical symbol but found " OP))))
      (SETQ OP (CDR W))
      (SETQ LHS (PASF_SMT2READTERM (CADR FORM)))
      (SETQ RHS (PASF_SMT2READTERM (CADDR FORM)))
      (RETURN (LIST OP (CAR (ADDSQ LHS (NEGSQ RHS))) NIL)))) 
(PUT 'PASF_SMT2READTERM 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2READTERM 'DEFINED-ON-LINE '1660) 
(PUT 'PASF_SMT2READTERM 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2READTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2READTERM (U) (SIMP (PASF_SMT2READTERM1 U))) 
(PUT 'PASF_SMT2READTERM1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_SMT2READTERM1 'DEFINED-ON-LINE '1663) 
(PUT 'PASF_SMT2READTERM1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFMISC.RED) 
(PUT 'PASF_SMT2READTERM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_SMT2READTERM1 (U)
    (PROG (OP W)
      (COND ((OR (ATOM U) (EQCAR U '|:DN:|) (EQCAR U '_)) (RETURN U)))
      (SETQ OP (CAR U))
      (SETQ W (ATSOC OP '((+ . PLUS) (- . MINUS) (* . TIMES) (/ . QUOTIENT))))
      (COND
       ((NOT W)
        (CL_SMT2READERROR
         (LIST "error: expecting arithmetic symbol but found " OP))))
      (SETQ OP (CDR W))
      (COND ((AND (EQ OP 'MINUS) (CDDR U)) (SETQ OP 'DIFFERENCE)))
      (RETURN
       (CONS OP
             (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
               (SETQ ARG (CDR U))
               (COND ((NULL ARG) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ARG) (PASF_SMT2READTERM1 ARG))
                                 (CAR ARG))
                                NIL)))
              LOOPLABEL
               (SETQ ARG (CDR ARG))
               (COND ((NULL ARG) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (ARG) (PASF_SMT2READTERM1 ARG)) (CAR ARG))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(ENDMODULE) 