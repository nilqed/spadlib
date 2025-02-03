(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GENSEP_LIN)) 
(PUT 'QUICK_GEN_SEPARATION 'NUMBER-OF-ARGS 1) 
(PUT 'QUICK_GEN_SEPARATION 'DEFINED-ON-LINE '33) 
(PUT 'QUICK_GEN_SEPARATION 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'QUICK_GEN_SEPARATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUICK_GEN_SEPARATION (ARGLIST)
    (COND
     (VL_
      (PROG (P L L1 PDES STP)
        (SETQ PDES (CAR ARGLIST))
        (COND
         (EXPERT_MODE
          (PROGN
           (SETQ L1 (SELECTPDES PDES 1))
           (COND ((GET (CAR L1) 'STARDE) (FLAG L1 'TO_GENSEP)))))
         (T (SETQ L1 (CADDDR ARGLIST))))
        (COND
         ((SETQ P (GET_GEN_SEPAR_PDE L1 T NIL))
          (PROGN
           (SETQ L (GENSEP P PDES NIL))
           (COND
            ((NEQ L 1)
             (PROGN
              (SETQ PDES (DELETE P PDES))
              (PROG (A)
                (SETQ A L)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A)
                   (PROGN
                    (SETQ PDES (EQINSERT A PDES))
                    (COND
                     ((AND (MEMBER A PDES) (SETQ STP (GET A 'STARDE)))
                      (SETQ TO_DO_LIST
                              (CONS
                               (LIST
                                (COND ((EQUAL (CAAR STP) 0) 'SEPARATION)
                                      (T 'GEN_SEPARATION))
                                (LIST A))
                               TO_DO_LIST))))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB)))))
           (SETQ L (LIST PDES (CADR ARGLIST))))))
        (RETURN L))))) 
(PUT 'GEN_SEPARATION 'NUMBER-OF-ARGS 1) 
(PUT 'GEN_SEPARATION 'DEFINED-ON-LINE '67) 
(PUT 'GEN_SEPARATION 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GEN_SEPARATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GEN_SEPARATION (ARGLIST) (INDIRECT_SEPARATION ARGLIST NIL)) 
(PUT 'CASE_GEN_SEPARATION 'NUMBER-OF-ARGS 1) 
(PUT 'CASE_GEN_SEPARATION 'DEFINED-ON-LINE '72) 
(PUT 'CASE_GEN_SEPARATION 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'CASE_GEN_SEPARATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASE_GEN_SEPARATION (ARGLIST)
    (COND ((NULL LIN_PROBLEM) (INDIRECT_SEPARATION ARGLIST T)))) 
(PUT 'INDIRECT_SEPARATION 'NUMBER-OF-ARGS 2) 
(PUT 'INDIRECT_SEPARATION 'DEFINED-ON-LINE '79) 
(PUT 'INDIRECT_SEPARATION 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'INDIRECT_SEPARATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDIRECT_SEPARATION (ARGLIST CASEGEN)
    (COND
     (VL_
      (PROG (P L L1 PDES STP)
        (SETQ PDES (CAR ARGLIST))
        (COND
         (EXPERT_MODE
          (PROGN
           (SETQ L1 (SELECTPDES PDES 1))
           (COND ((GET (CAR L1) 'STARDE) (FLAG L1 'TO_GENSEP)))))
         (T (SETQ L1 (CADDDR ARGLIST))))
        (COND
         ((SETQ P (GET_GEN_SEPAR_PDE L1 NIL CASEGEN))
          (PROGN
           (SETQ L (GENSEP P PDES CASEGEN))
           (COND
            ((NEQ L 1)
             (PROGN
              (SETQ PDES (DELETE P PDES))
              (PROG (A)
                (SETQ A L)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A)
                   (PROGN
                    (SETQ PDES (EQINSERT A PDES))
                    (COND
                     ((AND (MEMBER A PDES) (SETQ STP (GET A 'STARDE)))
                      (SETQ TO_DO_LIST
                              (CONS
                               (LIST
                                (COND ((EQUAL (CAAR STP) 0) 'SEPARATION)
                                      (T 'GEN_SEPARATION))
                                (LIST A))
                               TO_DO_LIST))))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB)))))
           (SETQ L (LIST PDES (CADR ARGLIST))))))
        (RETURN L))))) 
(PUT 'MAXNOARGS 'NUMBER-OF-ARGS 2) 
(PUT 'MAXNOARGS 'DEFINED-ON-LINE '114) 
(PUT 'MAXNOARGS 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'MAXNOARGS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXNOARGS (FL V)
    (PROG (F N M)
      (SETQ N 0)
      (PROG (F)
        (SETQ F FL)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ M (FCTARGS F))
            (SETQ M (COND ((AND M (NOT_INCLUDED V M)) (LENGTH M)) (T 0)))
            (COND ((LESSP N M) (SETQ N M)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN N))) 
(PUT 'GET_GEN_SEPAR_PDE 'NUMBER-OF-ARGS 3) 
(PUT 'GET_GEN_SEPAR_PDE 'DEFINED-ON-LINE '128) 
(PUT 'GET_GEN_SEPAR_PDE 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GET_GEN_SEPAR_PDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GET_GEN_SEPAR_PDE (PDES HIGH_PRIORITY CASEGEN)
    (PROG (P NV NF DW LEN H1 H2 H3 H4 NVMAX)
      (COND
       (HIGH_PRIORITY
        (PROGN
         (SETQ NVMAX 0)
         (PROG (P)
           (SETQ P PDES)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((GREATERP (SETQ H1 (GET P 'NVARS)) NVMAX) (SETQ NVMAX H1))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (SETQ P NIL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (COND
          ((AND
            (OR (FLAGP (CAR PDES) 'TO_GENSEP)
                (AND CASEGEN (FLAGP (CAR PDES) 'TO_CASEGENSEP)))
            (PROGN
             (SETQ H1 (GET (CAR PDES) 'LENGTH))
             (OR (NULL HIGH_PRIORITY) (EQUAL (GET (CAR PDES) 'NVARS) NVMAX)
                 (GREATERP LOW_GENSEP H1) (LESSP HIGH_GENSEP H1)))
            (SETQ H3 (GET (CAR PDES) 'STARDE)) (NEQ (CAAR H3) 0)
            (OR (SETQ H4 (FLAGP (CAR PDES) 'USED_)) (NULL DW))
            (OR (NULL P) (GREATERP LEN H1)
                (AND (EQUAL LEN H1)
                     (OR
                      (LESSP NV
                             (SETQ H2
                                     (MAXNOARGS (GET (CAR PDES) 'FCTS)
                                      (GET (CAR PDES) 'VARS))))
                      (AND (EQUAL NV H2)
                           (OR (AND (NULL DW) (FLAGP (CAR PDES) 'USED_))
                               (GREATERP NF (CAAR H3))
                               (AND (EQUAL NF (CAAR H3))
                                    (GREATERP LEN H1))))))))
           (PROGN
            (SETQ P (CAR PDES))
            (SETQ NV
                    (COND
                     ((OR (NULL NV) (NULL H2))
                      (MAXNOARGS (GET P 'FCTS) (GET (CAR PDES) 'VARS)))
                     (T H2)))
            (COND (H4 (SETQ DW H4)))
            (SETQ NF (CAAR H3))
            (SETQ LEN H1))))
         (SETQ PDES (CDR PDES))
         NIL)
        (GO WHILELABEL))
      (RETURN P))) 
(PUT 'GENSEP 'NUMBER-OF-ARGS 3) 
(PUT 'GENSEP 'DEFINED-ON-LINE '203) 
(PUT 'GENSEP 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GENSEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GENSEP (P PDES CASEGEN)
    (COND ((ZEROP (CAAR (GET P 'STARDE))) (SEPARATE P PDES))
          (T
           (PROG (A PL)
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN (PRIN2 "generalized separation of ") (PRIN2 P) NIL))))
             (COND
              (TR_GENSEP
               (PROGN
                (SETQ A (GET P 'STARDE))
                (TERPRI)
                (PROGN (PRIN2 "DE to be separated : ") NIL)
                (TYPEEQLIST (LIST P))
                (TERPRI)
                (PROGN (PRIN2 "occurences of variables in functions:") NIL)
                (TERPRI)
                (PROG (V)
                  (SETQ V A)
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V)
                     (PROGN
                      (PRIN2 (CDR V))
                      (PRIN2 ":")
                      (PRIN2 (CAR V))
                      (PRIN2 " times, ")
                      NIL))
                   (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB)))))
             (CP_SQ2P_VAL P)
             (SETQ PL
                     (PARTITN (GET P 'PVAL) NIL (GET P 'FCTS) (GET P 'VARS)
                      (GET P 'STARDE) CASEGEN))
             (COND
              (PL
               (PROGN
                (SETQ PL
                        (APPEND
                         (PROG (A FORALL-RESULT FORALL-ENDPTR)
                           (SETQ A (CAR PL))
                           (COND ((NULL A) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (A) (SIMP (CDR A)))
                                             (CAR A))
                                            NIL)))
                          LOOPLABEL
                           (SETQ A (CDR A))
                           (COND ((NULL A) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (SIMP (CDR A))) (CAR A))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         (PROG (A FORALL-RESULT FORALL-ENDPTR)
                           (SETQ A (CADR PL))
                           (COND ((NULL A) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (A) (SIMP A)) (CAR A))
                                            NIL)))
                          LOOPLABEL
                           (SETQ A (CDR A))
                           (COND ((NULL A) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (SIMP A)) (CAR A)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (SETQ PL
                        (MKEQSQLIST PL NIL NIL
                         (FCTSORT (UNION FTEM_ (GET P 'FCTS))) (GET P 'VARS)
                         (CONS 'TO_DROP ALLFLAGS_) T (GET P 'ORDERINGS) PDES))
                (DROP_PDE P NIL NIL)
                (FLAG PL 'USED_)
                (COND
                 (PRINT_
                  (PROGN
                   (TERPRI)
                   (SETQ A (LENGTH PL))
                   (PROGN
                    (PRIN2 "separation yields ")
                    (PRIN2 A)
                    (PRIN2 " new equation")
                    NIL)
                   (COND ((GREATERP A 1) (PROGN (PRIN2 "s") NIL)))
                   (PROGN (PRIN2 " : ") NIL)
                   (COND (TR_GENSEP (TYPEEQLIST PL)) (T (LISTPRINT PL)))
                   (TERPRI))))))
              (T
               (PROGN
                (COND (CASEGEN (REMFLAG (LIST P) 'TO_CASEGENSEP))
                      (T (REMFLAG (LIST P) 'TO_GENSEP)))
                (SETQ PL (LIST P)))))
             (RETURN PL))))) 
(PUT 'PARTITN 'NUMBER-OF-ARGS 6) 
(PUT 'PARTITN 'DEFINED-ON-LINE '258) 
(PUT 'PARTITN 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'PARTITN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARTITN (Q OLD_HISTY FTEM VL AV CASEGEN)
    (PROG (HISTY L1 L4 NV VL1 NV1 H X Y F FT AA BB CC KK RULI EXTRA_COND PAR
           CASES NEWF)
      (SETQ AV
              (REVERSE
               (PROG (NV FORALL-RESULT FORALL-ENDPTR)
                 (SETQ NV AV)
                 (COND ((NULL NV) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (NV) (CDR NV)) (CAR NV)) NIL)))
                LOOPLABEL
                 (SETQ NV (CDR NV))
                 (COND ((NULL NV) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (NV) (CDR NV)) (CAR NV)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ FT FTEM)
      (SETQ NV 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT FT) (RETURN NIL)))
        (PROGN
         (SETQ VL1 (FCTARGS (CAR FT)))
         (SETQ NV1 (COND (VL1 (LENGTH VL1)) (T 0)))
         (COND
          ((GREATERP NV1 NV)
           (PROGN
            (SETQ H (SETDIFF AV VL1))
            (COND
             (H
              (PROGN
               (SETQ X (CAR H))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND H (FREEOF Q (CAR H)))) (RETURN NIL)))
                 (SETQ H (CDR H))
                 (GO WHILELABEL))
               (COND (H (SETQ X (CAR H))))
               (SETQ F (CAR FT))
               (SETQ NV NV1)))))))
         (SETQ FT (CDR FT)))
        (GO WHILELABEL))
      (COND ((EQUAL NV 0) (SETQ X (CAR AV))))
      (COND
       (TR_GENSEP
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "--- The aim is to separate directly w.r.t. ")
          (PRIN2 X)
          NIL)
         (PROGN (PRIN2 " the expression : ") NIL)
         (DEPRINT (LIST Q)))))
      (SETQ FT NIL)
      (PROG (F)
        (SETQ F FTEM)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((AND (MEMBER X (FCTARGS F)) (NOT (FREEOF Q F)))
             (SETQ FT (CONS F FT)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ FT (FCTSORT (REVERSE FT)))
      (SETQ RULI (START_LET_RULES))
      (COND
       (TR_GENSEP
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "--- To separate w.r.t. ")
          (PRIN2 X)
          (PRIN2 " we need to get rid of functions ")
          (PRIN2 FT)
          (PRIN2 " . ")
          NIL)
         (TERPRI)
         NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FT) (RETURN NIL)))
        (PROGN
         (SETQ L1 (FELIM Q (CAR FT) FTEM VL))
         (COND ((NULL L1) (SETQ FT NIL))
               (T
                (PROGN
                 (PROG (H)
                   (SETQ H (CDADR L1))
                  LAB
                   (COND ((NULL H) (RETURN NIL)))
                   ((LAMBDA (H)
                      (COND
                       ((NULL (CAN_NOT_BECOME_ZEROSQ (SIMP (CAR H)) FTEM))
                        (SETQ CASES (CONS (CAR H) CASES)))))
                    (CAR H))
                   (SETQ H (CDR H))
                   (GO LAB))
                 (COND
                  (CASES
                   (PROGN
                    (COND
                     (CASEGEN
                      (SETQ TO_DO_LIST
                              (CONS (LIST 'SPLIT_INTO_CASES (SIMP (CAR CASES)))
                                    TO_DO_LIST)))
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2
                         "Generalized separation with the aim to separate wrt. ")
                        (PRIN2 X)
                        (PRIN2 " ")
                        (PRIN2
                         "needs to consider cases. To initiate this run module 84 ")
                        (PRIN2 "(case_gen_separation)")
                        NIL)
                       (TERPRI))))
                    (SETQ L1 NIL)
                    (SETQ FT NIL)))))))
         (COND
          (L1
           (PROGN
            (SETQ Q (CAR L1))
            (COND
             ((ZEROP Q)
              (PROGN
               (SETQ Y (CAR (CADR L1)))
               (SETQ L1 (CDR (CADR L1)))
               (SETQ CC NIL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT L1) (RETURN NIL)))
                 (PROGN
                  (SETQ CC
                          (PROG (HH FORALL-RESULT FORALL-ENDPTR)
                            (SETQ HH CC)
                            (COND ((NULL HH) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (HH)
                                                (CONS (CAR HH)
                                                      (REVAL1
                                                       (LIST 'TIMES
                                                             (CAR
                                                              (INTPDE (CDR HH)
                                                               FTEM VL Y NIL))
                                                             (CAAR L1))
                                                       T)))
                                              (CAR HH))
                                             NIL)))
                           LOOPLABEL
                            (SETQ HH (CDR HH))
                            (COND ((NULL HH) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (HH)
                                        (CONS (CAR HH)
                                              (REVAL1
                                               (LIST 'TIMES
                                                     (CAR
                                                      (INTPDE (CDR HH) FTEM VL
                                                       Y NIL))
                                                     (CAAR L1))
                                               T)))
                                      (CAR HH))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                  (SETQ KK (CDAR L1))
                  (PROG (HH)
                    (SETQ HH CC)
                   LAB
                    (COND ((NULL HH) (RETURN NIL)))
                    ((LAMBDA (HH)
                       (SETQ KK
                               (LIST 'DIFFERENCE KK
                                     (LIST 'TIMES (CAR HH) (CDR HH)))))
                     (CAR HH))
                    (SETQ HH (CDR HH))
                    (GO LAB))
                  (SETQ CC
                          (CONS
                           (CONS (REVAL1 (LIST 'QUOTIENT KK (CAAR L1)) T)
                                 (CAAR L1))
                           CC))
                  (SETQ L1 (CDR L1)))
                 (GO WHILELABEL))
               (COND
                (TR_GENSEP
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "Another division and differentiation gives zero.")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "Therefore eliminating function ")
                   (PRIN2 (CAR FT))
                   (PRIN2 " lead to a direct")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "separation and the vanishing of the following expressions:")
                   NIL)
                  (TERPRI)
                  (PROG (KK)
                    (SETQ KK CC)
                   LAB
                    (COND ((NULL KK) (RETURN NIL)))
                    ((LAMBDA (KK) NIL) (CAR KK))
                    (SETQ KK (CDR KK))
                    (GO LAB)))))
               (SETQ FT NIL)
               NIL))
             (T
              (PROGN
               (SETQ CC (CADR L1))
               (COND
                ((AND (PAIRP Q) (EQUAL (CAR Q) 'QUOTIENT))
                 (PROGN (SETQ BB (CADDR Q)) (SETQ Q (CADR Q))))
                (T (SETQ BB 1)))
               (COND
                (TR_GENSEP
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "The denominator taken off: ") NIL)
                  (MATHPRINT BB))))
               (SETQ HISTY (CONS (CONS BB CC) HISTY))
               (SETQ FTEM (SMEMBERL FTEM Q))
               (SETQ AA (SEP_VAR FTEM (ARGSET FTEM)))
               (COND ((OR (NOT AA) (ZEROP (CAAR AA))) (SETQ FT NIL))
                     (T (SETQ FT (SMEMBERL (CDR FT) FTEM)))))))))))
        (GO WHILELABEL))
      (STOP_LET_RULES RULI)
      (RETURN
       (COND
        ((NULL L1)
         (COND
          ((ZEROP Q)
           (PROGN
            (SETQ PAR (BACKINT CC OLD_HISTY HISTY FTEM VL))
            (COND (PAR (LIST (CAR PAR) (CADR PAR) CASES (CADDR PAR)))
                  (T NIL))))
          (TR_GENSEP
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "felim or newgensep gave nil!!") NIL)
            (TERPRI)
            (PROGN (PRIN2 "q=") (PRIN2 Q) NIL)
            (TERPRI)
            NIL))
          (T NIL)))
        (T
         (PROGN
          (COND
           (TR_GENSEP
            (PROGN
             (TERPRI)
             (PROGN (PRIN2 "Now ready for direct separation.") NIL))))
          (COND
           (TR_GENSEP
            (PROGN
             (TERPRI)
             (PROGN (PRIN2 "trying direct separation of ") NIL)
             (DEPRINT (LIST Q))
             NIL)))
          (SETQ L1 (SEPAR Q FTEM VL NIL NIL))
          (COND
           (TR_GENSEP
            (PROGN
             (TERPRI)
             (PROGN (PRIN2 "The result of direct separation: ") NIL)
             (DEPRINT
              (PROG (BB FORALL-RESULT FORALL-ENDPTR)
                (SETQ BB L1)
                (COND ((NULL BB) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (BB) (CDR BB)) (CAR BB)) NIL)))
               LOOPLABEL
                (SETQ BB (CDR BB))
                (COND ((NULL BB) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (BB) (CDR BB)) (CAR BB)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))
          (PROG ()
           WHILELABEL
            (COND ((NOT L1) (RETURN NIL)))
            (PROGN
             (SETQ H (CAR L1))
             (SETQ L1 (CDR L1))
             (SETQ FT (SMEMBERL FTEM (CDR H)))
             (SETQ VL1 (ARGSET FT))
             (COND
              ((OR (NULL (SETQ AA (SEP_VAR FT VL1))) (NOT (PAIRP FT))
                   (NEQ (CAR FT) 'PLUS))
               (SETQ L4 (CONS H L4)))
              (T
               (PROGN
                (SETQ PAR
                        (PARTITN (CDR H) (APPEND HISTY OLD_HISTY) FT VL1 AA
                         CASEGEN))
                (COND
                 (PAR
                  (PROGN
                   (SETQ L4
                           (APPEND L4
                                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ F (CAR PAR))
                                     (COND ((NULL F) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (F)
                                                         (CONS
                                                          (LIST 'TIMES (CAR H)
                                                                (CAR F))
                                                          (CDR F)))
                                                       (CAR F))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ F (CDR F))
                                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (F)
                                                 (CONS
                                                  (LIST 'TIMES (CAR H) (CAR F))
                                                  (CDR F)))
                                               (CAR F))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))))
                   (SETQ EXTRA_COND (APPEND EXTRA_COND (CADR PAR)))
                   (SETQ CASES (APPEND CASES (CADDR PAR)))
                   (SETQ NEWF (CADDDR PAR))
                   (SETQ FTEM (APPEND FTEM NEWF))))
                 (T (SETQ L1 NIL)))))))
            (GO WHILELABEL))
          (SETQ PAR (BACKINT L4 OLD_HISTY HISTY FTEM VL))
          (COND
           (PAR
            (PROGN
             (SETQ EXTRA_COND (APPEND EXTRA_COND (CADR PAR)))
             (LIST (CAR PAR) EXTRA_COND CASES (APPEND NEWF (CADDR PAR)))))
           (T NIL)))))))) 
(PUT 'FELIM 'NUMBER-OF-ARGS 4) 
(PUT 'FELIM 'DEFINED-ON-LINE '549) 
(PUT 'FELIM 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'FELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FELIM (Q F FTEM VL)
    (PROG (A B L L1 FT1 V PRFLAG F1 VLI)
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((NOT (MY_FREEOF Q V)) (SETQ L (CONS V L)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ VL L)
      (SETQ L NIL)
      (SETQ VLI (SETDIFF VL (FCTARGS F)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ V (CAR VLI))
         (SETQ VLI (CDR VLI))
         (SETQ FT1 NIL)
         (PROG (F1)
           (SETQ F1 FTEM)
          LAB
           (COND ((NULL F1) (RETURN NIL)))
           ((LAMBDA (F1) (COND ((MY_FREEOF F1 V) (SETQ FT1 (CONS F1 FT1)))))
            (CAR F1))
           (SETQ F1 (CDR F1))
           (GO LAB))
         (COND
          ((AND (PAIRP Q) (EQUAL (CAR Q) 'QUOTIENT) (SMEMBERL FT1 (CADDR Q)))
           (PROGN
            (COND
             (TR_GENSEP
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2
                 "The quotient of the expression to be separated contains one or more ")
                (PRIN2 V)
                (PRIN2 "-independent function: ")
                (PRIN2 (SMEMBERL FT1 (CADDR Q)))
                (PRIN2
                 " which prevents further continuation. (This should not occur anymore.)")
                NIL))))))
          (T
           (PROGN
            (SETQ PRFLAG PRINT_)
            (SETQ PRINT_ NIL)
            (SETQ L (SEPAR Q FT1 (LIST V) NIL NIL))
            (SETQ PRINT_ PRFLAG)
            (COND
             (L
              (COND
               ((AND (NULL (CDR L)) (MY_FREEOF (CDAR L) V))
                (SETQ L NIL)))))))))
        (COND ((NOT (OR L (NULL VLI))) (GO REPEATLABEL))))
      (COND
       ((NULL L)
        (RETURN
         (PROGN
          (PROGN (PRIN2 "##### The expression ") NIL)
          (MATHPRINT Q)
          (PROGN
           (PRIN2
            "is supposed to be an indirectly separable expression. But after ")
           NIL)
          (PROGN
           (PRIN2 "dropping the ")
           (PRIN2 V)
           (PRIN2 " dependent factor ")
           NIL)
          (MATHPRINT (CAAR L))
          (PROGN (PRIN2 "the remaining factor ") NIL)
          (MATHPRINT (CADR L))
          (PROGN
           (PRIN2 "is independent of all variables on which ")
           (PRIN2 F)
           (PRIN2 " does not depend on. ")
           NIL)
          (PROGN
           (PRIN2
            "Therefore this factor is not an indirectly separable expression ")
           NIL)
          (PROGN (PRIN2 "which is very strange #####.") NIL)
          NIL)))
       (T
        (PROGN
         (SETQ PRFLAG PRINT_)
         (SETQ PRINT_ NIL)
         (SETQ L (SEPAR Q FT1 (LIST V) NIL NIL))
         (SETQ PRINT_ PRFLAG)
         (COND
          (TR_GENSEP
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2 "To get rid of ")
             (PRIN2 F)
             (PRIN2 " we will differentiate w.r.t. : ")
             (PRIN2 V)
             NIL))))
         (SETQ L
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A
                           (REV_IDX_SORT
                            (PROG (B FORALL-RESULT FORALL-ENDPTR)
                              (SETQ B L)
                              (COND ((NULL B) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (B)
                                                  (CONS
                                                   (COND ((NUMBERP (CAR B)) 0)
                                                         (T
                                                          (DELENGTH (CAR B))))
                                                   B))
                                                (CAR B))
                                               NIL)))
                             LOOPLABEL
                              (SETQ B (CDR B))
                              (COND ((NULL B) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (B)
                                          (CONS
                                           (COND ((NUMBERP (CAR B)) 0)
                                                 (T (DELENGTH (CAR B))))
                                           B))
                                        (CAR B))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ L1 NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ A (CAR L))
            (COND
             ((AND (NOT (FREEOF (CDR A) F)) (NOT (ZEROP (CAR A))))
              (SETQ L1 (CONS (CAR A) L1))))
            (SETQ L (CDR L)))
           (GO WHILELABEL))
         (COND
          (TR_GENSEP
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2 V)
             (PRIN2 " - depending coefficients of terms containing ")
             (PRIN2 F)
             (PRIN2 " : ")
             NIL)
            (PROG (SS)
              (SETQ SS L1)
             LAB
              (COND ((NULL SS) (RETURN NIL)))
              ((LAMBDA (SS) (EQPRINT SS)) (CAR SS))
              (SETQ SS (CDR SS))
              (GO LAB)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT L1) (RETURN NIL)))
           (PROGN
            (SETQ B (REVAL1 (CAR L1) T))
            (SETQ L1 (CDR L1))
            (COND
             ((ZEROP B)
              (PROGN
               (PROGN
                (PRIN2 "#### Planned division through zero in gensep! ####")
                NIL)
               (TERPRI)))
             (T
              (PROGN
               (SETQ A (REVAL1 (LIST 'QUOTIENT Q B) T))
               (SETQ L (CONS (CONS B Q) L))
               (SETQ L1
                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                         (SETQ C L1)
                         (COND ((NULL C) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (C)
                                             (REVAL1
                                              (LIST 'DF (LIST 'QUOTIENT C B) V)
                                              T))
                                           (CAR C))
                                          NIL)))
                        LOOPLABEL
                         (SETQ C (CDR C))
                         (COND ((NULL C) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (C)
                                     (REVAL1 (LIST 'DF (LIST 'QUOTIENT C B) V)
                                             T))
                                   (CAR C))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ Q (REVAL1 (LIST 'DF A V) T))
               (COND
                (TR_GENSEP
                 (PROGN
                  (PROGN (PRIN2 "After division through ") NIL)
                  (EQPRINT B)
                  (PROGN
                   (PRIN2 "and differentiation wrt. ")
                   (PRIN2 V)
                   (PRIN2 " the new equation is: ")
                   NIL)
                  (EQPRINT Q)
                  (COND
                   (L1
                    (PROGN
                     (PROGN
                      (PRIN2
                       "The remaining coefficients to eliminate function ")
                      (PRIN2 F)
                      (PRIN2 " : ")
                      NIL)
                     (TERPRI)
                     (PROG (SS)
                       (SETQ SS L1)
                      LAB
                       (COND ((NULL SS) (RETURN NIL)))
                       ((LAMBDA (SS) (EQPRINT SS)) (CAR SS))
                       (SETQ SS (CDR SS))
                       (GO LAB))))))))))))
           (GO WHILELABEL))
         (COND
          (TR_GENSEP
           (COND
            ((ZEROP Q)
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2
                "This series of divisions and differentiations shows that the expression ")
               (PRIN2 "at the start of eliminating ")
               (PRIN2 F)
               (PRIN2 " can be directly separated, so no ")
               (PRIN2 "backintegration of the last step for eliminating ")
               (PRIN2 F)
               (PRIN2 " is needed.")
               NIL)
              (SETQ Q (CDAR L))
              (SETQ L (CDR L))))
            (T
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 "This new expression should either not depend on ")
               (PRIN2 F)
               (PRIN2 " or not depend ")
               (PRIN2 "on ")
               (PRIN2 V)
               (PRIN2
                " in which case a new differentiation variable is needed.")
               NIL)
              (EQPRINT Q)
              (COND
               (L
                (PROGN
                 (PROGN
                  (PRIN2 "To invert the last steps one has to integr. wrt. ")
                  (PRIN2 V)
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "each time before multiplying with the following factors:")
                  NIL)
                 (TERPRI)
                 (PROG (AA)
                   (SETQ AA L)
                  LAB
                   (COND ((NULL AA) (RETURN NIL)))
                   ((LAMBDA (AA) (EQPRINT (CAR AA))) (CAR AA))
                   (SETQ AA (CDR AA))
                   (GO LAB))))))))))
         (SETQ L1 (LIST Q (CONS V L))))))
      (RETURN L1))) 
(PUT 'BACKINT 'NUMBER-OF-ARGS 5) 
(PUT 'BACKINT 'DEFINED-ON-LINE '716) 
(PUT 'BACKINT 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'BACKINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE BACKINT (L4 OLD_HISTY HISTY FTEM VL)
    (PROG (SUCC FT Q L V V1 VF S1 S2 P F1 F2 FCTR CHECK_SUM ALLFNEW NEW_COND
           DENOMI LIN_PROBLEM_BAK)
      (SETQ SUCC T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND HISTY SUCC)) (RETURN NIL)))
        (PROGN
         (SETQ L (CAR HISTY))
         (SETQ HISTY (CDR HISTY))
         (SETQ DENOMI (CAR L))
         (SETQ V (CADR L))
         (SETQ L (CDDR L))
         (COND
          ((NEQ DENOMI 1)
           (SETQ L4
                   (PROG (H FORALL-RESULT FORALL-ENDPTR)
                     (SETQ H L4)
                     (COND ((NULL H) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (H)
                                         (CONS (CAR H)
                                               (LIST 'QUOTIENT (CDR H)
                                                     DENOMI)))
                                       (CAR H))
                                      NIL)))
                    LOOPLABEL
                     (SETQ H (CDR H))
                     (COND ((NULL H) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (H)
                                 (CONS (CAR H)
                                       (LIST 'QUOTIENT (CDR H) DENOMI)))
                               (CAR H))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (COND
          (TR_GENSEP
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "backward integration w.r.t. ") (PRIN2 V) NIL))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND L SUCC)) (RETURN NIL)))
           (PROGN
            (SETQ FCTR (CAAR L))
            (SETQ CHECK_SUM (CDAR L))
            (SETQ L (CDR L))
            (COND
             (TR_GENSEP
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2
                 "The integrals of the following partitioned subexpressions")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "added up should be equal to the original expression: ")
                NIL)
               (TERPRI)
               (EQPRINT CHECK_SUM))))
            (SETQ L4
                    (PROG (H FORALL-RESULT FORALL-ENDPTR)
                      (SETQ H L4)
                      (COND ((NULL H) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (H)
                                          (COND ((NULL (CAR H)) H)
                                                (T
                                                 (PROGN
                                                  (SETQ FT
                                                          (SMEMBERL FTEM
                                                           (CDR H)))
                                                  (SETQ FNEW_ NIL)
                                                  (COND
                                                   (TR_GENSEP
                                                    (PROGN
                                                     (TERPRI)
                                                     (PROGN
                                                      (PRIN2
                                                       "Backintegration of: ")
                                                      NIL)
                                                     (EQPRINT (CDR H))
                                                     (PROGN
                                                      (PRIN2 " w.r.t. ")
                                                      (PRIN2 V)
                                                      NIL)
                                                     (TERPRI))))
                                                  (SETQ LIN_PROBLEM_BAK
                                                          LIN_PROBLEM)
                                                  (SETQ LIN_PROBLEM T)
                                                  (SETQ Q
                                                          (INTEGRATEPDE (CDR H)
                                                           FT V NIL NIL))
                                                  (SETQ LIN_PROBLEM
                                                          LIN_PROBLEM_BAK)
                                                  (COND
                                                   ((NULL Q)
                                                    (PROGN
                                                     (SETQ SUCC NIL)
                                                     (COND
                                                      (PRINT_
                                                       (PROGN
                                                        (TERPRI)
                                                        (PROGN
                                                         (PRIN2
                                                          "#### Back integration of ")
                                                         NIL)
                                                        (EQPRINT (CDR H))
                                                        (PROGN
                                                         (PRIN2 " wrt ")
                                                         (PRIN2 V)
                                                         (PRIN2
                                                          " during generalized ")
                                                         (PRIN2
                                                          "separation was not successful ####.")
                                                         NIL)
                                                        (TERPRI)
                                                        (PROGN
                                                         (PRIN2
                                                          "The coeff. dropped in direct separation was ")
                                                         NIL)
                                                        (MATHPRINT
                                                         (CAR H)))))))
                                                   (T
                                                    (PROGN
                                                     (COND
                                                      (TR_GENSEP
                                                       (PROGN
                                                        (TERPRI)
                                                        (PROGN
                                                         (PRIN2
                                                          "Backintegration gives: ")
                                                         NIL)
                                                        (EQPRINT (CAR Q))
                                                        (TERPRI)
                                                        (PROGN
                                                         (PRIN2
                                                          "which is now multiplied with: ")
                                                         NIL)
                                                        (EQPRINT FCTR))))
                                                     (SETQ Q
                                                             (REVAL1
                                                              (LIST 'TIMES FCTR
                                                                    (CAR Q))
                                                              T))
                                                     (PROG (F1)
                                                       (SETQ F1 FNEW_)
                                                      LAB
                                                       (COND
                                                        ((NULL F1)
                                                         (RETURN NIL)))
                                                       ((LAMBDA (F1)
                                                          (PROGN
                                                           (SETQ F2 F1)
                                                           (SETQ VF
                                                                   (SETDIFF VL
                                                                            (FCTARGS
                                                                             F1)))
                                                           (PROG (S1)
                                                             (SETQ S1
                                                                     (REVERSE
                                                                      (APPEND
                                                                       HISTY
                                                                       OLD_HISTY)))
                                                            LAB
                                                             (COND
                                                              ((NULL S1)
                                                               (RETURN NIL)))
                                                             ((LAMBDA (S1)
                                                                (PROGN
                                                                 (SETQ V1
                                                                         (CADR
                                                                          S1))
                                                                 (SETQ S2
                                                                         (REVERSE
                                                                          (CDDR
                                                                           S1)))
                                                                 (PROG ()
                                                                  WHILELABEL
                                                                   (COND
                                                                    ((NOT S2)
                                                                     (RETURN
                                                                      NIL)))
                                                                   (PROGN
                                                                    (COND
                                                                     ((NOT
                                                                       (SMEMBERL
                                                                        VF
                                                                        (CAAR
                                                                         S2)))
                                                                      (SETQ F2
                                                                              (LIST
                                                                               'QUOTIENT
                                                                               F2
                                                                               (CAAR
                                                                                S2)))))
                                                                    (COND
                                                                     ((NOT
                                                                       (MY_FREEOF
                                                                        F1 V1))
                                                                      (SETQ F2
                                                                              (REVAL1
                                                                               (LIST
                                                                                'DF
                                                                                F2
                                                                                V1)
                                                                               T))))
                                                                    (SETQ S2
                                                                            (CDR
                                                                             S2)))
                                                                   (GO
                                                                    WHILELABEL))
                                                                 (COND
                                                                  ((NOT
                                                                    (SMEMBERL
                                                                     VF
                                                                     (CAR S1)))
                                                                   (SETQ F2
                                                                           (LIST
                                                                            'TIMES
                                                                            F2
                                                                            (CAR
                                                                             S1)))))
                                                                 NIL))
                                                              (CAR S1))
                                                             (SETQ S1 (CDR S1))
                                                             (GO LAB))
                                                           (COND
                                                            (HISTY
                                                             (PROGN
                                                              (SETQ S2
                                                                      (REVERSE
                                                                       L))
                                                              (PROG ()
                                                               WHILELABEL
                                                                (COND
                                                                 ((NOT S2)
                                                                  (RETURN
                                                                   NIL)))
                                                                (PROGN
                                                                 (COND
                                                                  ((NOT
                                                                    (SMEMBERL
                                                                     VF
                                                                     (CAAR
                                                                      S2)))
                                                                   (SETQ F2
                                                                           (LIST
                                                                            'QUOTIENT
                                                                            F2
                                                                            (CAAR
                                                                             S2)))))
                                                                 (COND
                                                                  ((NOT
                                                                    (MY_FREEOF
                                                                     F1 V1))
                                                                   (SETQ F2
                                                                           (REVAL1
                                                                            (LIST
                                                                             'DF
                                                                             F2
                                                                             V1)
                                                                            T))))
                                                                 (SETQ S2
                                                                         (CDR
                                                                          S2)))
                                                                (GO
                                                                 WHILELABEL))
                                                              NIL)))
                                                           (COND
                                                            ((NEQ F1 F2)
                                                             (PROGN
                                                              (COND
                                                               (TR_GENSEP
                                                                (PROGN
                                                                 (TERPRI)
                                                                 (PROGN
                                                                  (PRIN2 F1)
                                                                  (PRIN2
                                                                   " is replaced by ")
                                                                  NIL)
                                                                 (EQPRINT
                                                                  F2))))
                                                              (SETQ Q
                                                                      (SUBST F2
                                                                             F1
                                                                             Q))
                                                              NIL)))))
                                                        (CAR F1))
                                                       (SETQ F1 (CDR F1))
                                                       (GO LAB))
                                                     (SETQ ALLFNEW
                                                             (APPEND FNEW_
                                                                     ALLFNEW))
                                                     (SETQ FTEM
                                                             (UNION FNEW_
                                                                    FTEM))
                                                     (COND
                                                      (SUCC
                                                       (SETQ CHECK_SUM
                                                               (LIST
                                                                'DIFFERENCE
                                                                CHECK_SUM
                                                                (LIST 'TIMES Q
                                                                      (CAR
                                                                       H))))))
                                                     NIL)))
                                                  (CONS (CAR H) Q)))))
                                        (CAR H))
                                       NIL)))
                     LOOPLABEL
                      (SETQ H (CDR H))
                      (COND ((NULL H) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (H)
                                  (COND ((NULL (CAR H)) H)
                                        (T
                                         (PROGN
                                          (SETQ FT (SMEMBERL FTEM (CDR H)))
                                          (SETQ FNEW_ NIL)
                                          (COND
                                           (TR_GENSEP
                                            (PROGN
                                             (TERPRI)
                                             (PROGN
                                              (PRIN2 "Backintegration of: ")
                                              NIL)
                                             (EQPRINT (CDR H))
                                             (PROGN
                                              (PRIN2 " w.r.t. ")
                                              (PRIN2 V)
                                              NIL)
                                             (TERPRI))))
                                          (SETQ LIN_PROBLEM_BAK LIN_PROBLEM)
                                          (SETQ LIN_PROBLEM T)
                                          (SETQ Q
                                                  (INTEGRATEPDE (CDR H) FT V
                                                   NIL NIL))
                                          (SETQ LIN_PROBLEM LIN_PROBLEM_BAK)
                                          (COND
                                           ((NULL Q)
                                            (PROGN
                                             (SETQ SUCC NIL)
                                             (COND
                                              (PRINT_
                                               (PROGN
                                                (TERPRI)
                                                (PROGN
                                                 (PRIN2
                                                  "#### Back integration of ")
                                                 NIL)
                                                (EQPRINT (CDR H))
                                                (PROGN
                                                 (PRIN2 " wrt ")
                                                 (PRIN2 V)
                                                 (PRIN2 " during generalized ")
                                                 (PRIN2
                                                  "separation was not successful ####.")
                                                 NIL)
                                                (TERPRI)
                                                (PROGN
                                                 (PRIN2
                                                  "The coeff. dropped in direct separation was ")
                                                 NIL)
                                                (MATHPRINT (CAR H)))))))
                                           (T
                                            (PROGN
                                             (COND
                                              (TR_GENSEP
                                               (PROGN
                                                (TERPRI)
                                                (PROGN
                                                 (PRIN2
                                                  "Backintegration gives: ")
                                                 NIL)
                                                (EQPRINT (CAR Q))
                                                (TERPRI)
                                                (PROGN
                                                 (PRIN2
                                                  "which is now multiplied with: ")
                                                 NIL)
                                                (EQPRINT FCTR))))
                                             (SETQ Q
                                                     (REVAL1
                                                      (LIST 'TIMES FCTR
                                                            (CAR Q))
                                                      T))
                                             (PROG (F1)
                                               (SETQ F1 FNEW_)
                                              LAB
                                               (COND ((NULL F1) (RETURN NIL)))
                                               ((LAMBDA (F1)
                                                  (PROGN
                                                   (SETQ F2 F1)
                                                   (SETQ VF
                                                           (SETDIFF VL
                                                                    (FCTARGS
                                                                     F1)))
                                                   (PROG (S1)
                                                     (SETQ S1
                                                             (REVERSE
                                                              (APPEND HISTY
                                                                      OLD_HISTY)))
                                                    LAB
                                                     (COND
                                                      ((NULL S1) (RETURN NIL)))
                                                     ((LAMBDA (S1)
                                                        (PROGN
                                                         (SETQ V1 (CADR S1))
                                                         (SETQ S2
                                                                 (REVERSE
                                                                  (CDDR S1)))
                                                         (PROG ()
                                                          WHILELABEL
                                                           (COND
                                                            ((NOT S2)
                                                             (RETURN NIL)))
                                                           (PROGN
                                                            (COND
                                                             ((NOT
                                                               (SMEMBERL VF
                                                                (CAAR S2)))
                                                              (SETQ F2
                                                                      (LIST
                                                                       'QUOTIENT
                                                                       F2
                                                                       (CAAR
                                                                        S2)))))
                                                            (COND
                                                             ((NOT
                                                               (MY_FREEOF F1
                                                                V1))
                                                              (SETQ F2
                                                                      (REVAL1
                                                                       (LIST
                                                                        'DF F2
                                                                        V1)
                                                                       T))))
                                                            (SETQ S2 (CDR S2)))
                                                           (GO WHILELABEL))
                                                         (COND
                                                          ((NOT
                                                            (SMEMBERL VF
                                                             (CAR S1)))
                                                           (SETQ F2
                                                                   (LIST 'TIMES
                                                                         F2
                                                                         (CAR
                                                                          S1)))))
                                                         NIL))
                                                      (CAR S1))
                                                     (SETQ S1 (CDR S1))
                                                     (GO LAB))
                                                   (COND
                                                    (HISTY
                                                     (PROGN
                                                      (SETQ S2 (REVERSE L))
                                                      (PROG ()
                                                       WHILELABEL
                                                        (COND
                                                         ((NOT S2)
                                                          (RETURN NIL)))
                                                        (PROGN
                                                         (COND
                                                          ((NOT
                                                            (SMEMBERL VF
                                                             (CAAR S2)))
                                                           (SETQ F2
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    F2
                                                                    (CAAR
                                                                     S2)))))
                                                         (COND
                                                          ((NOT
                                                            (MY_FREEOF F1 V1))
                                                           (SETQ F2
                                                                   (REVAL1
                                                                    (LIST 'DF
                                                                          F2
                                                                          V1)
                                                                    T))))
                                                         (SETQ S2 (CDR S2)))
                                                        (GO WHILELABEL))
                                                      NIL)))
                                                   (COND
                                                    ((NEQ F1 F2)
                                                     (PROGN
                                                      (COND
                                                       (TR_GENSEP
                                                        (PROGN
                                                         (TERPRI)
                                                         (PROGN
                                                          (PRIN2 F1)
                                                          (PRIN2
                                                           " is replaced by ")
                                                          NIL)
                                                         (EQPRINT F2))))
                                                      (SETQ Q (SUBST F2 F1 Q))
                                                      NIL)))))
                                                (CAR F1))
                                               (SETQ F1 (CDR F1))
                                               (GO LAB))
                                             (SETQ ALLFNEW
                                                     (APPEND FNEW_ ALLFNEW))
                                             (SETQ FTEM (UNION FNEW_ FTEM))
                                             (COND
                                              (SUCC
                                               (SETQ CHECK_SUM
                                                       (LIST 'DIFFERENCE
                                                             CHECK_SUM
                                                             (LIST 'TIMES Q
                                                                   (CAR H))))))
                                             NIL)))
                                          (CONS (CAR H) Q)))))
                                (CAR H))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             (SUCC
              (PROGN
               (SETQ CHECK_SUM (REVAL1 CHECK_SUM T))
               (SETQ NEW_COND (CONS CHECK_SUM NEW_COND))
               (COND
                ((AND SUCC TR_GENSEP)
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "Consistency condition: ") NIL)
                  (EQPRINT CHECK_SUM))))))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (COND
       (SUCC
        (PROGN
         (PROG (F)
           (SETQ F ALLFNEW)
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (PROGN
               (SETQ FTEM_ (FCTINSERT F FTEM_))
               (SETQ FLIN_ (CONS F FLIN_))
               NIL))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_)))))
      (COND
       (TR_GENSEP
        (COND
         (SUCC
          (PROGN (TERPRI) (PROGN (PRIN2 "yields : ") NIL) (EQPRINT P) NIL))
         (T (PROGN (TERPRI) (PROGN (PRIN2 "was not successful.") NIL))))))
      (SETQ FNEW_ NIL)
      (RETURN (COND (SUCC (LIST L4 NEW_COND ALLFNEW)) (T NIL))))) 
(ENDMODULE) 
(MODULE (LIST 'GENSEP_NON_LIN)) 
(PUT 'MY_SMEMBERL 'NUMBER-OF-ARGS 2) 
(PUT 'MY_SMEMBERL 'DEFINED-ON-LINE '892) 
(PUT 'MY_SMEMBERL 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'MY_SMEMBERL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MY_SMEMBERL (P VL)
    (PROG (L V)
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((NOT (MY_FREEOF P V)) (SETQ L (CONS V L)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (REVERSE L)))) 
(PUT 'STRIPCOND 'NUMBER-OF-ARGS 1) 
(PUT 'STRIPCOND 'DEFINED-ON-LINE '901) 
(PUT 'STRIPCOND 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'STRIPCOND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRIPCOND (CONDS)
    (PROG (NEWCONDS CONDI)
      (SETQ NEWCONDS NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT CONDS) (RETURN NIL)))
        (PROGN
         (SETQ CONDI (CDAR CONDS))
         (SETQ CONDS (CDR CONDS))
         (COND ((EQUAL (LENGTH CONDI) 1) (SETQ CONDI (CAR CONDI)))
               (T (SETQ CONDI (CONS 'PLUS CONDI))))
         (SETQ NEWCONDS (CONS CONDI NEWCONDS)))
        (GO WHILELABEL))
      (RETURN NEWCONDS))) 
(PUT 'CHECKLI 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKLI 'DEFINED-ON-LINE '916) 
(PUT 'CHECKLI 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'CHECKLI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKLI (EXLIST CONDI)
    (PROG (OK ISINCONDI ISINEXLI EXCOPY N)
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CONDI OK)) (RETURN NIL)))
        (PROGN
         (SETQ ISINCONDI (CAR CONDI))
         (SETQ N (LENGTH ISINCONDI))
         (SETQ EXCOPY EXLIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND EXCOPY OK)) (RETURN NIL)))
           (PROGN
            (SETQ ISINEXLI (SMEMBERL ISINCONDI (CAR EXCOPY)))
            (COND
             (ISINEXLI (COND ((EQUAL (LENGTH ISINEXLI) N) (SETQ OK NIL)))))
            (SETQ EXCOPY (CDR EXCOPY)))
           (GO WHILELABEL))
         (SETQ CONDI (CDR CONDI)))
        (GO WHILELABEL))
      (RETURN OK))) 
(PUT 'LONGST 'NUMBER-OF-ARGS 1) 
(PUT 'LONGST 'DEFINED-ON-LINE '939) 
(PUT 'LONGST 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'LONGST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LONGST (EXLIST)
    (PROG (LO)
      (PROG ()
       WHILELABEL
        (COND ((NOT EXLIST) (RETURN NIL)))
        (PROGN
         (COND ((NOT LO) (SETQ LO (CAR EXLIST)))
               ((LESSP (LENGTH LO) (LENGTH (CAR EXLIST)))
                (SETQ LO (CAR EXLIST))))
         (SETQ EXLIST (CDR EXLIST)))
        (GO WHILELABEL))
      (RETURN LO))) 
(PUT 'STAREQU 'NUMBER-OF-ARGS 3) 
(PUT 'STAREQU 'DEFINED-ON-LINE '954) 
(PUT 'STAREQU 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'STAREQU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STAREQU (N ALINDEP BLINDEP)
    (PROG (I J CASES OLDCASES CASE AI BI CI OLDACONDS OLDBCONDS NEWACONDS
           NEWBCOND NEWBCONDS NEWACOND ILIST CONA CONB UNIN EL PRI)
      (SETQ I 0)
      (COND
       (ALINDEP
        (PROG (CONA)
          (SETQ CONA ALINDEP)
         LAB
          (COND ((NULL CONA) (RETURN NIL)))
          ((LAMBDA (CONA)
             (COND
              (BLINDEP
               (PROG (CONB)
                 (SETQ CONB BLINDEP)
                LAB
                 (COND ((NULL CONB) (RETURN NIL)))
                 ((LAMBDA (CONB)
                    (COND
                     ((GREATERP (SETQ J (LENGTH (UNION CONA CONB))) I)
                      (PROGN (SETQ AI CONA) (SETQ BI CONB) (SETQ I J)))
                     (T NIL)))
                  (CAR CONB))
                 (SETQ CONB (CDR CONB))
                 (GO LAB)))
              ((GREATERP (SETQ J (LENGTH CONA)) I)
               (PROGN (SETQ AI CONA) (SETQ I J)))
              (T NIL)))
           (CAR CONA))
          (SETQ CONA (CDR CONA))
          (GO LAB)))
       (BLINDEP
        (PROG (CONB)
          (SETQ CONB BLINDEP)
         LAB
          (COND ((NULL CONB) (RETURN NIL)))
          ((LAMBDA (CONB)
             (COND
              ((GREATERP (SETQ J (LENGTH CONB)) I)
               (PROGN (SETQ BI CONB) (SETQ I J)))))
           (CAR CONB))
          (SETQ CONB (CDR CONB))
          (GO LAB))))
      (SETQ ILIST
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS I NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (PRI (PROGN (PROGN (PRIN2 "222") NIL) (TERPRI))))
      (COND
       ((NEQ I 0)
        (PROGN
         (COND (AI (SETQ I (LENGTH AI))) (T (SETQ I 0)))
         (COND (BI (SETQ J (LENGTH BI))) (T (SETQ J 0)))
         (SETQ UNIN (UNION AI BI))
         (SETQ ILIST (SETDIFF ILIST UNIN))
         (COND
          ((GREATERP I J)
           (PROGN
            (PROG (EL)
              (SETQ EL (SETDIFF UNIN AI))
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL) (SETQ ILIST (CONS (MINUS EL) ILIST))) (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))
            (PROG (EL)
              (SETQ EL AI)
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL) (SETQ ILIST (CONS EL ILIST))) (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))))
          (T
           (PROGN
            (PROG (EL)
              (SETQ EL (SETDIFF UNIN BI))
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL) (SETQ ILIST (CONS EL ILIST))) (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))
            (PROG (EL)
              (SETQ EL BI)
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL) (SETQ ILIST (CONS (MINUS EL) ILIST))) (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB)))))
         (SETQ ILIST (REVERSE ILIST)))))
      (COND
       (PRI (PROGN (PROGN (PRIN2 "333 ilist=") (PRIN2 ILIST) NIL) (TERPRI))))
      (SETQ I (CAR ILIST))
      (COND ((LESSP I 0) (SETQ I (MINUS I))))
      (SETQ CI (MKID '_ I))
      (SETQ CASES
              (LIST (LIST (LIST (LIST CI)) NIL) (LIST NIL (LIST (LIST CI)))))
      (SETQ ILIST (CDR ILIST))
      (COND (PRI (PROGN (PROGN (PRIN2 "555") NIL) (TERPRI))))
      (PROG ()
       WHILELABEL
        (COND ((NOT ILIST) (RETURN NIL)))
        (PROGN
         (SETQ I (CAR ILIST))
         (SETQ ILIST (CDR ILIST))
         (COND (PRI (PROGN (PROGN (PRIN2 "iii=") (PRIN2 I) NIL) (TERPRI))))
         (COND ((GREATERP I 0) (SETQ CI (MKID '_ I)))
               (T (SETQ CI (MKID '_ (MINUS I)))))
         (COND
          (PRI
           (PROGN (PROGN (PRIN2 "666 car ilist=") (PRIN2 I) NIL) (TERPRI))))
         (SETQ OLDCASES CASES)
         (SETQ CASES NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT OLDCASES) (RETURN NIL)))
           (PROGN
            (SETQ CASE (CAR OLDCASES))
            (COND
             (PRI
              (PROGN (PROGN (PRIN2 "old case:") (PRIN2 CASE) NIL) (TERPRI))))
            (SETQ OLDCASES (CDR OLDCASES))
            (COND
             ((GREATERP I 0)
              (PROGN
               (SETQ OLDACONDS (CAR CASE))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "888 oldaconds=") (PRIN2 OLDACONDS) NIL)
                  (TERPRI))))
               (SETQ CASES
                       (CONS (CONS (CONS (LIST CI) OLDACONDS) (CDR CASE))
                             CASES))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "999 new case=") (PRIN2 (CAR CASES)) NIL)
                  (TERPRI))))
               (SETQ NEWACONDS NIL)
               (SETQ NEWBCOND (LIST CI))
               (PROG ()
                WHILELABEL
                 (COND ((NOT OLDACONDS) (RETURN NIL)))
                 (PROGN
                  (SETQ J (CAAR OLDACONDS))
                  (SETQ NEWACONDS
                          (CONS (CONS J (CONS CI (CDAR OLDACONDS))) NEWACONDS))
                  (SETQ NEWBCOND (CONS J NEWBCOND))
                  (SETQ OLDACONDS (CDR OLDACONDS)))
                 (GO WHILELABEL))
               (COND
                (PRI
                 (PROGN
                  (PROGN
                   (PRIN2 "newaconds=")
                   (PRIN2 NEWACONDS)
                   (PRIN2 " rev newbcond=")
                   (PRIN2 (REVERSE NEWBCOND))
                   NIL)
                  (TERPRI))))
               (SETQ CASES
                       (CONS
                        (LIST NEWACONDS (CONS (REVERSE NEWBCOND) (CADR CASE)))
                        CASES))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "000 new case=") (PRIN2 (CAR CASES)) NIL)
                  (TERPRI))))
               NIL))
             (T
              (PROGN
               (SETQ OLDBCONDS (CADR CASE))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "888 oldbconds=") (PRIN2 OLDBCONDS) NIL)
                  (TERPRI))))
               (SETQ CASES
                       (CONS (LIST (CAR CASE) (CONS (LIST CI) OLDBCONDS))
                             CASES))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "999 new case=") (PRIN2 (CAR CASES)) NIL)
                  (TERPRI))))
               (SETQ NEWBCONDS NIL)
               (SETQ NEWACOND (LIST CI))
               (PROG ()
                WHILELABEL
                 (COND ((NOT OLDBCONDS) (RETURN NIL)))
                 (PROGN
                  (SETQ J (CAAR OLDBCONDS))
                  (SETQ NEWBCONDS
                          (CONS (CONS J (CONS CI (CDAR OLDBCONDS))) NEWBCONDS))
                  (SETQ NEWACOND (CONS J NEWACOND))
                  (SETQ OLDBCONDS (CDR OLDBCONDS)))
                 (GO WHILELABEL))
               (SETQ CASES
                       (CONS
                        (LIST (CONS (REVERSE NEWACOND) (CAR CASE)) NEWBCONDS)
                        CASES))
               (COND
                (PRI
                 (PROGN
                  (PROGN (PRIN2 "000 new case=") (PRIN2 (CAR CASES)) NIL)
                  (TERPRI))))
               NIL))))
           (GO WHILELABEL))
         NIL)
        (GO WHILELABEL))
      (SETQ ALINDEP
              (PROG (CI FORALL-RESULT FORALL-ENDPTR)
                (SETQ CI ALINDEP)
                (COND ((NULL CI) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CI)
                                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ I CI)
                                      (COND ((NULL I) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (I)
                                                          (MKID '_ I))
                                                        (CAR I))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ I (CDR I))
                                      (COND ((NULL I) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (I) (MKID '_ I))
                                                (CAR I))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR CI))
                                 NIL)))
               LOOPLABEL
                (SETQ CI (CDR CI))
                (COND ((NULL CI) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CI)
                            (PROG (I FORALL-RESULT FORALL-ENDPTR)
                              (SETQ I CI)
                              (COND ((NULL I) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (I) (MKID '_ I))
                                                (CAR I))
                                               NIL)))
                             LOOPLABEL
                              (SETQ I (CDR I))
                              (COND ((NULL I) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (I) (MKID '_ I)) (CAR I))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR CI))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BLINDEP
              (PROG (CI FORALL-RESULT FORALL-ENDPTR)
                (SETQ CI BLINDEP)
                (COND ((NULL CI) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CI)
                                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ I CI)
                                      (COND ((NULL I) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (I)
                                                          (MKID '_ I))
                                                        (CAR I))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ I (CDR I))
                                      (COND ((NULL I) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (I) (MKID '_ I))
                                                (CAR I))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR CI))
                                 NIL)))
               LOOPLABEL
                (SETQ CI (CDR CI))
                (COND ((NULL CI) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CI)
                            (PROG (I FORALL-RESULT FORALL-ENDPTR)
                              (SETQ I CI)
                              (COND ((NULL I) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (I) (MKID '_ I))
                                                (CAR I))
                                               NIL)))
                             LOOPLABEL
                              (SETQ I (CDR I))
                              (COND ((NULL I) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (I) (MKID '_ I)) (CAR I))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR CI))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ OLDCASES NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT CASES) (RETURN NIL)))
        (PROGN
         (COND
          ((CHECKLI ALINDEP (CAAR CASES))
           (COND
            ((CHECKLI BLINDEP (CADAR CASES))
             (SETQ OLDCASES (CONS (CAR CASES) OLDCASES))))))
         (SETQ CASES (CDR CASES)))
        (GO WHILELABEL))
      (RETURN OLDCASES))) 
(PUT 'PICKFAC 'NUMBER-OF-ARGS 2) 
(PUT 'PICKFAC 'DEFINED-ON-LINE '1125) 
(PUT 'PICKFAC 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'PICKFAC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PICKFAC (EX INDX) (NTH EX (COMPRESS (CDR (EXPLODE INDX))))) 
(PUT 'FIND_COND 'NUMBER-OF-ARGS 2) 
(PUT 'FIND_COND 'DEFINED-ON-LINE '1131) 
(PUT 'FIND_COND 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'FIND_COND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND_COND (BCONS AI)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (PAIRP BCONS) (NEQ (CAAR BCONS) AI))) (RETURN NIL)))
        (SETQ BCONS (CDR BCONS))
        (GO WHILELABEL))
      (RETURN (COND ((PAIRP BCONS) (CAR BCONS)) (T NIL))))) 
(PUT 'STARSEP 'NUMBER-OF-ARGS 5) 
(PUT 'STARSEP 'DEFINED-ON-LINE '1144) 
(PUT 'STARSEP 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'STARSEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE STARSEP (EXX EX FTEM VL X)
    (PROG (CASES NEWCASES ACONS BCONS ACOND NEWCA ALINDEP BLINDEP ACO BCO AI BI
           CI A1 AVARS BVARS I ILI CILIST ALI N ADDEX BCOPY CNTR PRI)
      (SETQ ILI
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE (LENGTH EX) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (MKID '_ I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH EX) I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (MKID '_ I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N (LENGTH VL))
      (SETQ CNTR 0)
      (PROG (CI)
        (SETQ CI EX)
       LAB
        (COND ((NULL CI) (RETURN NIL)))
        ((LAMBDA (CI)
           (PROGN
            (SETQ CNTR (ADD1 CNTR))
            (COND
             (PRI
              (PROGN
               (PROGN (PRIN2 "a") (PRIN2 CNTR) (PRIN2 " = ") NIL)
               (MATHPRINT (CAR CI))
               (PROGN (PRIN2 "b") (PRIN2 CNTR) (PRIN2 " = ") NIL)
               (MATHPRINT (CDR CI))
               NIL)))
            (COND
             ((NULL (SMEMBERL FTEM (CAR CI)))
              (SETQ ALINDEP (CONS CNTR ALINDEP))))
            (COND
             ((NULL (SMEMBERL FTEM (CDR CI)))
              (SETQ BLINDEP (CONS CNTR BLINDEP))))
            NIL))
         (CAR CI))
        (SETQ CI (CDR CI))
        (GO LAB))
      (COND (ALINDEP (SETQ ALINDEP (LIST ALINDEP))))
      (COND (BLINDEP (SETQ BLINDEP (LIST BLINDEP))))
      (SETQ CASES (STAREQU (LENGTH EX) ALINDEP BLINDEP))
      (COND
       (PRI
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Returned from STAREQU: ") (PRIN2 CASES) NIL))))
      (SETQ NEWCASES NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT CASES) (RETURN NIL)))
        (PROGN
         (SETQ ACONS (CAAR CASES))
         (SETQ BCONS (CADAR CASES))
         (SETQ CASES (CDR CASES))
         (COND
          (PRI
           (PROGN
            (PROGN
             (PRIN2 "acons=")
             (PRIN2 ACONS)
             (PRIN2 "  bcons=")
             (PRIN2 BCONS)
             NIL)
            (TERPRI))))
         (SETQ NEWCA NIL)
         (SETQ CILIST NIL)
         (SETQ ADDEX NIL)
         (SETQ BCOPY NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT BCONS) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (LENGTH (CAR BCONS)) 1)
              (SETQ NEWCA (CONS (CDR (PICKFAC EX (CAAR BCONS))) NEWCA)))
             (T (SETQ BCOPY (CONS (CAR BCONS) BCOPY))))
            (SETQ BCONS (CDR BCONS)))
           (GO WHILELABEL))
         (SETQ BCONS BCOPY)
         (PROG ()
          WHILELABEL
           (COND ((NOT ACONS) (RETURN NIL)))
           (PROGN
            (SETQ ACO (CAR ACONS))
            (SETQ ACONS (CDR ACONS))
            (COND
             (PRI (PROGN (PROGN (PRIN2 "aco=") (PRIN2 ACO) NIL) (TERPRI))))
            (SETQ A1 (CAR ACO))
            (SETQ ACOND (LIST (CAR (PICKFAC EX A1))))
            (COND
             (PRI (PROGN (PROGN (PRIN2 "acond=") (PRIN2 ACOND) NIL) (TERPRI))))
            (COND
             ((EQUAL (LENGTH ACO) 1) (SETQ NEWCA (CONS (CAR ACOND) NEWCA)))
             (T
              (PROGN
               (SETQ ALI
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I ACO)
                         (COND ((NULL I) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (I) (CAR (PICKFAC EX I)))
                                           (CAR I))
                                          NIL)))
                        LOOPLABEL
                         (SETQ I (CDR I))
                         (COND ((NULL I) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (I) (CAR (PICKFAC EX I))) (CAR I))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ AVARS (MY_SMEMBERL ALI VL))
               (COND
                ((NEQ (LENGTH AVARS) N)
                 (PROGN
                  (SETQ ACO (CDR ACO))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT ACO) (RETURN NIL)))
                    (PROGN
                     (SETQ AI (CAR ACO))
                     (SETQ ACO (CDR ACO))
                     (SETQ BCO (FIND_COND BCOPY AI))
                     (COND
                      (PRI
                       (PROGN
                        (PROGN (PRIN2 "bco=") (PRIN2 BCO) NIL)
                        (TERPRI))))
                     (SETQ BVARS NIL)
                     (PROG (BI)
                       (SETQ BI BCO)
                      LAB
                       (COND ((NULL BI) (RETURN NIL)))
                       ((LAMBDA (BI)
                          (SETQ BVARS
                                  (UNION (MY_SMEMBERL (CDR (PICKFAC EX BI)) VL)
                                         BVARS)))
                        (CAR BI))
                       (SETQ BI (CDR BI))
                       (GO LAB))
                     (COND
                      (PRI
                       (PROGN
                        (PROGN (PRIN2 "bvars=") (PRIN2 BVARS) NIL)
                        (TERPRI))))
                     (SETQ CI (NEWFCT FNAME_ (INTERSECTION AVARS BVARS) NFCT_))
                     (SETQ CILIST (CONS CI CILIST))
                     (SETQ NFCT_ (PLUS NFCT_ 1))
                     (SETQ ACOND
                             (CONS
                              (LIST 'MINUS
                                    (LIST 'TIMES CI (CAR (PICKFAC EX AI))))
                              ACOND))
                     (COND
                      (PRI
                       (PROGN
                        (PROGN (PRIN2 "acond=") (PRIN2 ACOND) NIL)
                        (TERPRI))))
                     (COND
                      ((SETQ BCO (FIND_COND BCONS AI))
                       (PROGN
                        (SETQ BCONS
                                (SUBST (SUBST (LIST 'TIMES CI A1) A1 BCO) BCO
                                       BCONS))
                        (COND
                         (PRI
                          (PROGN
                           (PROGN (PRIN2 "bcons=") (PRIN2 BCONS) NIL)
                           (TERPRI))))
                        NIL))))
                    (GO WHILELABEL))
                  (SETQ ACOND (CONS 'PLUS ACOND))))
                (T
                 (PROGN
                  (SETQ ADDEX T)
                  (SETQ ALI (REVERSE ALI))
                  (SETQ ACO (REVERSE ACO))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (GREATERP (LENGTH ALI) 1)) (RETURN NIL)))
                    (PROGN
                     (COND
                      (PRI
                       (PROGN
                        (PROGN (PRIN2 "ali1=") (PRIN2 ALI) NIL)
                        (TERPRI))))
                     (COND (PRI (PROGN (PROGN (PRIN2 "###") NIL) (TERPRI))))
                     (SETQ ALI
                             (COND
                              ((NOT (ZEROP (CAR ALI)))
                               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ I (CDR ALI))
                                 (COND ((NULL I) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (I)
                                                     (REVAL1
                                                      (LIST 'DF
                                                            (LIST 'QUOTIENT I
                                                                  (CAR ALI))
                                                            X)
                                                      T))
                                                   (CAR I))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ I (CDR I))
                                 (COND ((NULL I) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (I)
                                             (REVAL1
                                              (LIST 'DF
                                                    (LIST 'QUOTIENT I
                                                          (CAR ALI))
                                                    X)
                                              T))
                                           (CAR I))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                              (T (CDR ALI))))
                     (COND
                      (PRI
                       (PROGN
                        (PROGN (PRIN2 "ali2=") (PRIN2 ALI) NIL)
                        (TERPRI))))
                     (COND
                      ((SETQ BCO (FIND_COND BCONS (CAR ACO)))
                       (SETQ BCONS (SETDIFF BCONS (LIST BCO)))))
                     (SETQ ACO (CDR ACO)))
                    (GO WHILELABEL))
                  (SETQ ACOND (CAR ALI))
                  (COND
                   ((AND (PAIRP ACOND) (EQUAL (CAR ACOND) 'QUOTIENT))
                    (SETQ ACOND (CADR ACOND))))
                  NIL)))
               (SETQ NEWCA (CONS ACOND NEWCA)))))
            (COND
             (PRI
              (PROGN (PROGN (PRIN2 "newca1=") (PRIN2 NEWCA) NIL) (TERPRI))))
            NIL)
           (GO WHILELABEL))
         (COND
          (PRI (PROGN (PROGN (PRIN2 "newca2=") (PRIN2 NEWCA) NIL) (TERPRI))))
         (PROG (BI)
           (SETQ BI ILI)
          LAB
           (COND ((NULL BI) (RETURN NIL)))
           ((LAMBDA (BI) (SETQ BCONS (SUBST (CDR (PICKFAC EX BI)) BI BCONS)))
            (CAR BI))
           (SETQ BI (CDR BI))
           (GO LAB))
         (PROG ()
          WHILELABEL
           (COND ((NOT BCONS) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (LENGTH (CAR BCONS)) 1)
              (SETQ NEWCA (CONS (CAAR BCONS) NEWCA)))
             (T (SETQ NEWCA (CONS (CONS 'PLUS (CAR BCONS)) NEWCA))))
            (SETQ BCONS (CDR BCONS)))
           (GO WHILELABEL))
         (COND (ADDEX (SETQ NEWCA (CONS EXX NEWCA))))
         (COND
          (PRI (PROGN (PROGN (PRIN2 "newca3=") (PRIN2 NEWCA) NIL) (TERPRI))))
         (SETQ NEWCA (CONS CILIST NEWCA))
         (COND
          (PRI (PROGN (PROGN (PRIN2 "cilist=") (PRIN2 CILIST) NIL) (TERPRI))))
         (SETQ NEWCASES (CONS NEWCA NEWCASES)))
        (GO WHILELABEL))
      (RETURN NEWCASES))) 
(PUT 'SEPARIZABLE 'NUMBER-OF-ARGS 3) 
(PUT 'SEPARIZABLE 'DEFINED-ON-LINE '1354) 
(PUT 'SEPARIZABLE 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'SEPARIZABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEPARIZABLE (P FTEM VL)
    (PROG (X FT F EX V A B VLCP ALLVARCAARA PRINT_BAK)
      (SETQ VLCP VL)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ X (CAR VL))
         (SETQ VL (CDR VL))
         (SETQ FT NIL)
         (PROG (F)
           (SETQ F FTEM)
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (COND
               ((AND (MEMBER X (FCTARGS F)) (NOT (MY_FREEOF P F)))
                (SETQ FT (CONS F FT)))))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (SETQ F (CAR (REVERSE (FCTSORT FT))))
         (SETQ V (CAR (SETDIFF VLCP (FCTARGS F))))
         (SETQ FT NIL)
         (PROG (F)
           (SETQ F FTEM)
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F) (COND ((MY_FREEOF F V) (SETQ FT (CONS F FT)))))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (SETQ PRINT_BAK PRINT_)
         (SETQ PRINT_ NIL)
         (SETQ EX (SEPAR2 P FT (LIST V)))
         (SETQ PRINT_ PRINT_BAK)
         (SETQ A EX)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND A
                   (PROGN
                    (SETQ B VLCP)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (AND B (NOT (MY_FREEOF (CAAR A) (CAR B)))))
                        (RETURN NIL)))
                      (SETQ B (CDR B))
                      (GO WHILELABEL))
                    B)))
             (RETURN NIL)))
           (SETQ A (CDR A))
           (GO WHILELABEL))
         (COND (A (SETQ ALLVARCAARA (CONS (CAAR A) ALLVARCAARA))))
         NIL)
        (COND ((NOT (OR (NULL A) (NULL VL))) (GO REPEATLABEL))))
      (SETQ VL VLCP)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND ALLVARCAARA
                (NOT (NOT_INCLUDED VLCP (SMEMBERL VLCP (CAR ALLVARCAARA))))))
          (RETURN NIL)))
        (PROGN (SETQ ALLVARCAARA (CDR ALLVARCAARA)) (SETQ VL (CDR VL)))
        (GO WHILELABEL))
      (RETURN
       (COND ((AND A (NULL ALLVARCAARA)) NIL)
             (A (LIST NIL (CAR ALLVARCAARA) (CAR VL)))
             (T
              (PROGN
               (COND
                (TR_GENSEP
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "To separate directly wrt. ") (PRIN2 X) NIL)
                  (PROGN (PRIN2 " the expression : ") NIL)
                  (DEPRINT (LIST P))
                  (PROGN
                   (PRIN2 "will be differentiated wrt. ")
                   (PRIN2 V)
                   (PRIN2 " to get rid of ")
                   (PRIN2 FT)
                   (PRIN2 " ")
                   NIL))))
               (LIST EX V))))))) 
(PUT 'NEWGENSEP 'NUMBER-OF-ARGS 4) 
(PUT 'NEWGENSEP 'DEFINED-ON-LINE '1414) 
(PUT 'NEWGENSEP 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'NEWGENSEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWGENSEP (P STARPRO FTEM VL)
    (PROG (PL V EX A B)
      (COND
       (PRINT_ (PROGN (TERPRI) (PROGN (PRIN2 "generalized separation ") NIL))))
      (COND
       (TR_GENSEP
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "DE to be separated : ") NIL)
         (DEPRINT (LIST P))
         (TERPRI)
         (PROGN (PRIN2 "occurences of variables in functions:") NIL)
         (TERPRI)
         (PROG (V)
           (SETQ V STARPRO)
          LAB
           (COND ((NULL V) (RETURN NIL)))
           ((LAMBDA (V)
              (PROGN
               (PRIN2 (CDR V))
               (PRIN2 ":")
               (PRIN2 (CAR V))
               (PRIN2 " times, ")
               NIL))
            (CAR V))
           (SETQ V (CDR V))
           (GO LAB)))))
      (PROG (V)
        (SETQ V STARPRO)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ VL (DELETE (CDR V) VL))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (V)
        (SETQ V (REVERSE STARPRO))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ VL (CONS (CDR V) VL))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ A (SEPARIZABLE P FTEM VL))
      (COND ((NULL A) (RETURN NIL))
            ((NULL (CAR A))
             (RETURN
              (PROGN
               (COND
                (PRINT_
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "In order to be separable with this procedure at first")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "one or more functions have to be eliminated through")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "differentiation and algebraic elimination, for example,")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "the functions: ")
                   (PRIN2 (SMEMBERL FTEM (CADR A)))
                   NIL)
                  (TERPRI)
                  NIL)))
               NIL)))
            (T (PROGN (SETQ EX (CAR A)) (SETQ V (CADR A)))))
      (PROG (A FORALL-RESULT FORALL-ENDPTR)
        (SETQ A
                (REV_IDX_SORT
                 (PROG (B FORALL-RESULT FORALL-ENDPTR)
                   (SETQ B EX)
                   (COND ((NULL B) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (B) (CONS (DELENGTH (CAR B)) B))
                                     (CAR B))
                                    NIL)))
                  LOOPLABEL
                   (SETQ B (CDR B))
                   (COND ((NULL B) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (B) (CONS (DELENGTH (CAR B)) B)) (CAR B))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (COND ((NULL A) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL)))
       LOOPLABEL
        (SETQ A (CDR A))
        (COND ((NULL A) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (COND
       (TR_GENSEP
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Return from SEPAR: ") NIL)
         (TERPRI)
         (PRETTYPRINT EX))))
      (SETQ PL (STARSEP P EX FTEM VL V))
      (COND
       (TR_GENSEP
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Return from STARSEP: ") NIL)
         (TERPRI)
         (PRETTYPRINT PL))))
      (RETURN PL))) 
(PUT 'GEN_SEPARATION2 'NUMBER-OF-ARGS 1) 
(PUT 'GEN_SEPARATION2 'DEFINED-ON-LINE '1508) 
(PUT 'GEN_SEPARATION2 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GEN_SEPARATION2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GEN_SEPARATION2 (ARGLIST)
    (PROG (P H FL L L1 PDES FORG N RESULT D CONTRAD NEWPDES)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE
        (PROGN
         (SETQ L1 (SELECTPDES PDES 1))
         (COND ((GET (CAR L1) 'STARDE) (FLAG L1 'TO_GENSEP)))))
       (T (SETQ L1 PDES)))
      (COND
       ((SETQ P (GET_GEN_SEPAR_PDE L1 NIL T))
        (COND
         ((PROGN
           (CP_SQ2P_VAL P)
           (SETQ L
                   (NEWGENSEP (GET P 'PVAL) (GET P 'STARDE) (GET P 'FCTS)
                    (GET P 'VARS))))
          (COND
           ((CDR L)
            (PROGN
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN
                 (PRIN2 "The indirect separation leads to ")
                 (PRIN2 (LENGTH L))
                 (PRIN2 " cases.")
                 NIL)
                (TERPRI)
                (COND
                 (KEEP_CASE_TREE
                  (PROGN
                   (PROGN
                    (PRIN2 "Comment: The case tree can no longer be updated.")
                    NIL)
                   (TERPRI)
                   (SETQ KEEP_CASE_TREE NIL)))))))
             (SETQ CONTRAD T)
             (SETQ N 0)
             (REMFLAG (LIST P) 'TO_GENSEP)
             (BACKUP_TO_FILE PDES FORG NIL)
             (PROG ()
              WHILELABEL
               (COND ((NOT L) (RETURN NIL)))
               (PROGN
                (SETQ D (CAR L))
                (SETQ L (CDR L))
                (COND
                 ((NOT (MEMBERL (CDR D) INEQ_))
                  (PROGN
                   (COND
                    ((NEQ N 0)
                     (PROGN
                      (SETQ H (RESTORE_AND_MERGE L1 PDES FORG))
                      (SETQ PDES (CAR H))
                      (SETQ FORG (CADR H))
                      NIL)))
                   (SETQ N (PLUS N 1))
                   (START_LEVEL N (LIST (LIST 'EQUAL 0 (CDR D))))
                   (COND
                    (PRINT_
                     (PROGN
                      (TERPRI)
                      (PROGN
                       (PRIN2 "CRACK is now called with the assumption : ")
                       NIL)
                      (DEPRINT (CDR D)))))
                   (PROG (H)
                     (SETQ H (CAR D))
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H)
                        (PROGN
                         (SETQ FTEM_ (FCTINSERT H FTEM_))
                         (SETQ FLIN_ (CONS H FLIN_))))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
                   (SETQ FL (APPEND (GET P 'FCTS) (CAR D)))
                   (SETQ NEWPDES PDES)
                   (PROG (H)
                     (SETQ H (CDR D))
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H)
                        (SETQ NEWPDES
                                (EQINSERT
                                 (MKEQSQ NIL NIL H FL VL_ ALLFLAGS_ T (LIST 0)
                                  NIL NEWPDES)
                                 NEWPDES)))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (SETQ RECYCLE_FCTS NIL)
                   (COND
                    (CONTRADICTION_ (PROGN (SETQ L1 NIL) (FINISH_LEVEL 0)))
                    (T (SETQ L1 (CRACKMAIN_IF_POSSIBLE_REMOTE NEWPDES FORG))))
                   (COND ((NOT CONTRADICTION_) (SETQ CONTRAD NIL)))
                   (COND
                    ((AND L1 (NOT CONTRADICTION_))
                     (SETQ RESULT (MERGE_CRACK_RETURNS L1 RESULT))))
                   (SETQ CONTRADICTION_ NIL)
                   NIL))))
               (GO WHILELABEL))
             (DELETE_BACKUP)
             (SETQ CONTRADICTION_ CONTRAD)
             (COND (CONTRADICTION_ (SETQ RESULT NIL)))
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN
                 (PRIN2 "This completes the investigation of all cases of an ")
                 (PRIN2 "indirect separation.")
                 NIL)
                (TERPRI)
                NIL)))
             (SETQ RESULT (LIST RESULT))))
           (T
            (PROGN
             (SETQ L (CAR L))
             (PROG (H)
               (SETQ H (CAR L))
              LAB
               (COND ((NULL H) (RETURN NIL)))
               ((LAMBDA (H)
                  (PROGN
                   (SETQ FTEM_ (FCTINSERT H FTEM_))
                   (SETQ FLIN_ (CONS H FLIN_))))
                (CAR H))
               (SETQ H (CDR H))
               (GO LAB))
             (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
             (SETQ FL (APPEND (GET P 'FCTS) (CAR L)))
             (SETQ PDES (DROP_PDE P PDES NIL))
             (PROG (H)
               (SETQ H (CDR L))
              LAB
               (COND ((NULL H) (RETURN NIL)))
               ((LAMBDA (H)
                  (SETQ PDES
                          (EQINSERT
                           (MKEQSQ NIL NIL H FL VL_ ALLFLAGS_ T (LIST 0) NIL
                            PDES)
                           PDES)))
                (CAR H))
               (SETQ H (CDR H))
               (GO LAB))
             (SETQ RESULT (LIST PDES FORG)))))))))
      (RETURN RESULT))) 
(ENDMODULE) 
(MODULE (LIST 'PARSEP_ALG)) 
(PUT 'GET_SPECIAL_ALG_SOL1 'NUMBER-OF-ARGS 1) 
(PUT 'GET_SPECIAL_ALG_SOL1 'DEFINED-ON-LINE '1611) 
(PUT 'GET_SPECIAL_ALG_SOL1 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GET_SPECIAL_ALG_SOL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_SPECIAL_ALG_SOL1 (ARGLIST) (ERR_CATCH_SPEC_ALG_SOL 1 ARGLIST)) 
(PUT 'GET_SPECIAL_ALG_SOL2 'NUMBER-OF-ARGS 1) 
(PUT 'GET_SPECIAL_ALG_SOL2 'DEFINED-ON-LINE '1614) 
(PUT 'GET_SPECIAL_ALG_SOL2 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'GET_SPECIAL_ALG_SOL2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_SPECIAL_ALG_SOL2 (ARGLIST) (ERR_CATCH_SPEC_ALG_SOL 2 ARGLIST)) 
(PUT 'ERR_CATCH_SPEC_ALG_SOL 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_SPEC_ALG_SOL 'DEFINED-ON-LINE '1617) 
(PUT 'ERR_CATCH_SPEC_ALG_SOL 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'ERR_CATCH_SPEC_ALG_SOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_SPEC_ALG_SOL (MODE ARGLIST)
    (PROG (H BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_SPEC_ALG_SOL))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_SPEC_ALG_SOL)
      (COND
       ((EQUAL MODE 1)
        ((LAMBDA (*PROTFG)
           (SETQ H (ERRORSET (LIST 'SPEC_ALG_SOL1 (MKQUOTE ARGLIST)) NIL NIL)))
         T)))
      (COND
       ((EQUAL MODE 2)
        ((LAMBDA (*PROTFG)
           (SETQ H (ERRORSET (LIST 'SPEC_ALG_SOL2 (MKQUOTE ARGLIST)) NIL NIL)))
         T)))
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((OR (NULL H) (ERRORP H)) NIL) (T (CAR H)))))) 
(PUT 'PRINT_PL 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_PL 'DEFINED-ON-LINE '1633) 
(PUT 'PRINT_PL 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'PRINT_PL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_PL (PDES)
    (PROG (P H HH)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PDES))
         (SETQ PDES (CDR PDES))
         (PROGN (PRIN2 P) (PRIN2 "(") (PRIN2 (GET P 'TERMS)) NIL)
         (COND
          ((SETQ H (GET P 'STARDE))
           (PROGN
            (PROG (HH)
              (SETQ HH 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (PLUS 1 (CAAR H)) HH)) (RETURN NIL)))
              (PROGN (PRIN2 "*") NIL)
              (SETQ HH (PLUS2 HH 1))
              (GO LAB))
            NIL)))
         (COND ((PAIRP (GET P 'FAC)) (PROGN (PRIN2 "#") NIL)))
         (COND ((GET P 'CASE2SEP) (PROGN (PRIN2 "!") NIL)))
         (COND
          ((AND FLIN_ (GET P 'ALLVARFCTS)
                (FREEOFLIST (GET P 'ALLVARFCTS) FLIN_))
           (PROGN (PRIN2 "a") NIL)))
         (COND
          ((AND (NULL LIN_PROBLEM) (GET P 'LINEAR_)) (PROGN (PRIN2 "l") NIL)))
         (PROGN (PRIN2 ")") NIL)
         (COND (PDES (PROGN (PRIN2 ",") NIL)))
         NIL)
        (GO WHILELABEL)))) 
(PUT 'SPEC_ALG_SOL1 'NUMBER-OF-ARGS 1) 
(PUT 'SPEC_ALG_SOL1 'DEFINED-ON-LINE '1651) 
(PUT 'SPEC_ALG_SOL1 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'SPEC_ALG_SOL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPEC_ALG_SOL1 (ARGLIST)
    (PROG (PDES PLI P F F2 CO FS FSCP CLI CONLI COP LINFLI ALLLINFLI H HH FCTZ
           PLICOP P2 P3 P4 RTRN NEWPDES VERBTM HIPO CPU GC)
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2
           "WARNING: If this module is successful, it will most likely lead")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "         to a loss of solutions admitted by the new system.")
          NIL)
         (TERPRI))))
      (SETQ PDES
              (COND (EXPERT_MODE (SELECTPDES (CAR ARGLIST) 1))
                    (T (REVERSE (CADDDR ARGLIST)))))
      (COND
       (PRINT_
        (PROGN
         (PROGN (PRIN2 "Available equations: ") NIL)
         (PRINT_PL PDES)
         (TERPRI))))
      (SETQ PLI NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PDES))
         (SETQ PDES (CDR PDES))
         (COND
          ((AND VERBTM PRINT_)
           (PROGN
            (PROGN (PRIN2 "Consideration of equation ") (PRIN2 P) NIL)
            (TERPRI))))
         (SETQ FS (GET P 'FCTS))
         (SETQ FSCP FS)
         (SETQ CLI NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT FS) (RETURN NIL)))
           (PROGN
            (SETQ F (CAR FS))
            (SETQ FS (CDR FS))
            (COND
             ((NOT (FREEOF (GET P 'NON_RAT_KERN) F))
              (COND
               (PRINT_
                (PROGN
                 (PROGN (PRIN2 F) (PRIN2 " occurs non-rationally") NIL)
                 (TERPRI)))
               (T NIL)))
             (T
              (PROGN
               (COND
                (*TIME
                 (PROGN
                  (SETQ CPU (TIME))
                  (SETQ GC (GCTIME))
                  (PROGN (PRIN2 "checking degree and coeff of ") (PRIN2 F) NIL)
                  (TERPRI)
                  NIL)))
               (SETQ HIPO (DEGREE_SF (CAR (GET P 'SQVAL)) F))
               (COND
                (*TIME
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "degree: ")
                   (PRIN2 HIPO)
                   (PRIN2 "   time: ")
                   (PRIN2 (DIFFERENCE (TIME) CPU))
                   (PRIN2 " ms,   GC time : ")
                   (PRIN2 (DIFFERENCE (GCTIME) GC))
                   (PRIN2 " ms")
                   NIL)
                  (SETQ CPU (TIME))
                  (SETQ GC (GCTIME)))))
               (SETQ CO
                       (CDR
                        (AEVAL* (LIST 'COEFF (LIST '*SQ (GET P 'SQVAL) T) F))))
               (COND
                (*TIME
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "time for coeff: ")
                   (PRIN2 (DIFFERENCE (TIME) CPU))
                   (PRIN2 " ms,   GC time : ")
                   (PRIN2 (DIFFERENCE (GCTIME) GC))
                   (PRIN2 " ms")
                   NIL)
                  NIL)))
               (COND
                ((GREATERP HIPOW* 0)
                 (COND
                  ((EQUAL HIPOW* 1)
                   (PROGN
                    (SETQ FS NIL)
                    (SETQ PDES NIL)
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2 "##### Equation ")
                        (PRIN2 P)
                        (PRIN2 " is linear in ")
                        (PRIN2 F)
                        (PRIN2 ". --> Use module 21")
                        NIL)
                       (COND
                        ((OR (NULL SUBST_3) (LESSP SUBST_3 1000000))
                         (PROGN
                          (PRIN2 " if necessary with 'as subst_3 1000000);'. ")
                          NIL)))
                       (TERPRI)))
                     (T NIL))))
                  (T
                   (PROGN
                    (SETQ CONLI (CDDR CO))
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND CONLI
                              (NEQ
                               (SIMPLIFYSQ (SIMP (CAR CONLI)) FTEM_ T NIL NIL)
                               (LIST (CONS 1 1)))))
                        (RETURN NIL)))
                      (SETQ CONLI (CDR CONLI))
                      (GO WHILELABEL))
                    (COND
                     (CONLI
                      (COND
                       ((AND VERBTM PRINT_)
                        (PROGN
                         (PROGN
                          (PRIN2 "a power of ")
                          (PRIN2 F)
                          (PRIN2 " has a non-vanishing coefficient")
                          NIL)
                         (TERPRI)))
                       (T NIL)))
                     (T
                      (PROGN
                       (SETQ CONLI (CDDR CO))
                       (SETQ ALLLINFLI NIL)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT CONLI) (RETURN NIL)))
                         (PROGN
                          (SETQ COP (CAR CONLI))
                          (SETQ CONLI (CDR CONLI))
                          (SETQ LINFLI NIL)
                          (SETQ FCTZ (FACTORIZE COP))
                          (PROG (FTR)
                            (SETQ FTR FCTZ)
                           LAB
                            (COND ((NULL FTR) (RETURN NIL)))
                            ((LAMBDA (FTR)
                               (PROG (F2)
                                 (SETQ F2 FSCP)
                                LAB
                                 (COND ((NULL F2) (RETURN NIL)))
                                 ((LAMBDA (F2)
                                    (PROGN
                                     (SETQ H (DEGREE_SF (CAR (SIMP FTR)) F2))
                                     (COND
                                      ((EQUAL H 1)
                                       (SETQ LINFLI
                                               (UNION (LIST F2) LINFLI))))))
                                  (CAR F2))
                                 (SETQ F2 (CDR F2))
                                 (GO LAB)))
                             (CAR FTR))
                            (SETQ FTR (CDR FTR))
                            (GO LAB))
                          (COND
                           ((NULL LINFLI)
                            (PROGN
                             (SETQ ALLLINFLI NIL)
                             (SETQ CONLI NIL)
                             (COND
                              ((AND VERBTM PRINT_)
                               (PROGN
                                (PROGN
                                 (PRIN2 "One coefficient of ")
                                 (PRIN2 F)
                                 (PRIN2 " has no linear function.")
                                 NIL)
                                (TERPRI))))))
                           (T (SETQ ALLLINFLI (UNION LINFLI ALLLINFLI)))))
                         (GO WHILELABEL))
                       (COND
                        (ALLLINFLI
                         (PROGN
                          (SETQ H
                                  (SOLVEEVAL
                                   (LIST (CONS 'LIST (CDDR CO))
                                         (CONS 'LIST ALLLINFLI))))
                          (COND
                           ((GREATERP (LENGTH H) 1)
                            (PROGN
                             (SETQ CLI
                                     (CONS (CONS F CO) CLI))))))))))))))))))))
           (GO WHILELABEL))
         (COND (CLI (SETQ PLI (CONS (CONS P CLI) PLI)))))
        (GO WHILELABEL))
     SELECTAGAIN
      (COND
       (PLI
        (PROGN
         (COND
          ((AND VERBTM PRINT_)
           (PROGN
            (PROGN (PRIN2 "Available linearizations: ") NIL)
            (TERPRI)
            (PROG (H)
              (SETQ H PLI)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (PROGN
                  (PROGN
                   (PRIN2 "equation: ")
                   (PRIN2 (CAR H))
                   (PRIN2 "(")
                   (PRIN2 (GET (CAR H) 'TERMS))
                   (PRIN2 ")")
                   NIL)
                  (TERPRI)
                  (PROG (HH)
                    (SETQ HH (CDR H))
                   LAB
                    (COND ((NULL HH) (RETURN NIL)))
                    ((LAMBDA (HH)
                       (PROGN
                        (PROGN
                         (PRIN2 "linearized function: ")
                         (PRIN2 (CAR HH))
                         NIL)
                        (TERPRI)
                        NIL))
                     (CAR HH))
                    (SETQ HH (CDR HH))
                    (GO LAB))))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB)))))
         (COND ((NULL EXPERT_MODE) (SETQ P (CAAR PLI)))
               (T
                (PROGN
                 (PROGN
                  (PRIN2
                   "If you do not want to proceed then enter ';' else input the equation name: ")
                  NIL)
                 (CHANGE_PROMPT_TO "")
                 (SETQ P (TERMREAD))
                 NIL)))
         (COND
          ((NEQ P '|;|)
           (PROGN
            (SETQ HH PLI)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND HH (NEQ (CAAR HH) P))) (RETURN NIL)))
              (SETQ HH (CDR HH))
              (GO WHILELABEL))
            (COND
             ((NULL HH)
              (PROGN
               (PROGN
                (PRIN2 "This is not one of the linearizable equations")
                NIL)
               (TERPRI)))
             (T
              (PROGN
               (COND ((NULL EXPERT_MODE) (SETQ F (CAAR (CDAR PLI))))
                     (T
                      (PROGN
                       (PROGN
                        (PRIN2 "What is the name of the linearized function: ")
                        NIL)
                       (SETQ F (TERMREAD))
                       NIL)))
               (COND
                ((AND VERBTM PRINT_)
                 (PROGN
                  (PROGN
                   (PRIN2 "Next try is equation ")
                   (PRIN2 P)
                   (PRIN2 "(")
                   (PRIN2 (GET P 'TERMS))
                   (PRIN2 ") splitted wrt ")
                   (PRIN2 F)
                   NIL)
                  (TERPRI))))
               (SETQ HH (CDAR HH))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND HH (NEQ (CAAR HH) F))) (RETURN NIL)))
                 (SETQ HH (CDR HH))
                 (GO WHILELABEL))
               (COND
                ((NULL HH)
                 (PROGN
                  (PROGN
                   (PRIN2 "This is not one of the linearizable unknowns")
                   NIL)
                  (TERPRI)))
                (T
                 (PROGN
                  (SETQ CO (CDAR HH))
                  (SETQ NEWPDES
                          (MKEQSQLIST
                           (CONS
                            (SIMP
                             (AEVAL
                              (LIST 'PLUS (CAR CO) (LIST 'TIMES F (CADR CO)))))
                            (PROG (HH FORALL-RESULT FORALL-ENDPTR)
                              (SETQ HH (CDDR CO))
                              (COND ((NULL HH) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (HH) (SIMP HH))
                                                (CAR HH))
                                               NIL)))
                             LOOPLABEL
                              (SETQ HH (CDR HH))
                              (COND ((NULL HH) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (HH) (SIMP HH)) (CAR HH))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                           NIL NIL (GET P 'FCTS) VL_ ALLFLAGS_ T (LIST 0)
                           (CAR ARGLIST)))
                  (COND
                   (CONTRADICTION_
                    (PROGN
                     (COND
                      ((AND VERBTM PRINT_)
                       (PROGN
                        (PROGN
                         (PRIN2
                          "This choice of equation and function lead to a contradiction with ")
                         (PRIN2
                          "previous inequalities. Choose a different equation or function.")
                         NIL)
                        (TERPRI)
                        NIL)))
                     (SETQ CONTRADICTION_ NIL)
                     (SETQ PLICOP NIL)
                     (PROG (P2)
                       (SETQ P2 PLI)
                      LAB
                       (COND ((NULL P2) (RETURN NIL)))
                       ((LAMBDA (P2)
                          (COND
                           ((NEQ (CAR P2) P) (SETQ PLICOP (CONS P2 PLICOP)))
                           (T
                            (PROGN
                             (SETQ P4 NIL)
                             (PROG (P3)
                               (SETQ P3 (CDR P2))
                              LAB
                               (COND ((NULL P3) (RETURN NIL)))
                               ((LAMBDA (P3)
                                  (COND
                                   ((NEQ (CAR P3) F) (SETQ P4 (CONS P3 P4)))))
                                (CAR P3))
                               (SETQ P3 (CDR P3))
                               (GO LAB))
                             (COND
                              (P4
                               (SETQ PLICOP
                                       (CONS (CONS (CAR P2) P4) PLICOP))))))))
                        (CAR P2))
                       (SETQ P2 (CDR P2))
                       (GO LAB))
                     (SETQ PLI (REVERSE PLICOP))
                     (SETQ PLICOP NIL)
                     (RESTORE_INTERACTIVE_PROMPT)
                     (SETQ RTRN NIL)
                     (GO SELECTAGAIN)))
                   (T
                    (PROGN
                     (DROP_PDE P NIL NIL)
                     (COND
                      (PRINT_
                       (PROGN
                        (PROGN (PRIN2 "Replaced equation: ") (PRIN2 P) NIL)
                        (TERPRI)
                        (PROGN (PRIN2 "New equations: ") NIL)
                        (PRINT_PL NEWPDES))))
                     (RESTORE_INTERACTIVE_PROMPT)
                     (SETQ RTRN
                             (LIST (APPEND NEWPDES (DELETE P (CAR ARGLIST)))
                                   (CADR ARGLIST)))))))))))))))
         NIL)))
      (RETURN RTRN))) 
(PUT 'SPEC_ALG_SOL2 'NUMBER-OF-ARGS 1) 
(PUT 'SPEC_ALG_SOL2 'DEFINED-ON-LINE '1925) 
(PUT 'SPEC_ALG_SOL2 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'SPEC_ALG_SOL2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPEC_ALG_SOL2 (ARGLIST)
    (PROG (PDES PLI P F CO FS H P2 RTRN VERBTM GC CPU BEST HIPO)
      (SETQ VERBTM T)
      (SETQ PDES
              (COND (EXPERT_MODE (SELECTPDES (CAR ARGLIST) 1))
                    (T (REVERSE (CADDDR ARGLIST)))))
      (COND
       (PRINT_
        (PROGN (PROGN (PRIN2 "Available equations: ") NIL) (PRINT_PL PDES))))
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PDES))
         (SETQ PDES (CDR PDES))
         (COND
          ((AND VERBTM PRINT_)
           (PROGN
            (PROGN (PRIN2 "Consideration of equation ") (PRIN2 P) NIL)
            (TERPRI))))
         (SETQ FS (GET P 'FCTS))
         (PROG ()
          WHILELABEL
           (COND ((NOT FS) (RETURN NIL)))
           (PROGN
            (SETQ F (CAR FS))
            (SETQ FS (CDR FS))
            (COND
             ((NOT (FREEOF (GET P 'NON_RAT_KERN) F))
              (COND
               (PRINT_
                (PROGN
                 (PROGN (PRIN2 F) (PRIN2 " occurs non-rationally") NIL)
                 (TERPRI)))
               (T NIL)))
             (T
              (PROGN
               (COND
                (*TIME
                 (PROGN
                  (SETQ CPU (TIME))
                  (SETQ GC (GCTIME))
                  (PROGN (PRIN2 "checking degree and coeff of ") (PRIN2 F) NIL)
                  (TERPRI)
                  NIL)))
               (SETQ HIPO (DEGREE_SF (CAR (GET P 'SQVAL)) F))
               (COND
                (*TIME
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "degree: ")
                   (PRIN2 HIPO)
                   (PRIN2 "   time: ")
                   (PRIN2 (DIFFERENCE (TIME) CPU))
                   (PRIN2 " ms,   GC time : ")
                   (PRIN2 (DIFFERENCE (GCTIME) GC))
                   (PRIN2 " ms")
                   NIL)
                  (SETQ CPU (TIME))
                  (SETQ GC (GCTIME)))))
               (SETQ CO
                       (CDR
                        (AEVAL* (LIST 'COEFF (LIST '*SQ (GET P 'SQVAL) T) F))))
               (COND
                (*TIME
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "time for coeff: ")
                   (PRIN2 (DIFFERENCE (TIME) CPU))
                   (PRIN2 " ms,   GC time : ")
                   (PRIN2 (DIFFERENCE (GCTIME) GC))
                   (PRIN2 " ms")
                   NIL)
                  NIL)))
               (COND
                ((GREATERP HIPOW* 0)
                 (COND
                  ((EQUAL HIPOW* 1)
                   (PROGN
                    (SETQ FS NIL)
                    (SETQ PDES NIL)
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2 "##### Equation ")
                        (PRIN2 P)
                        (PRIN2 " is linear in ")
                        (PRIN2 F)
                        (PRIN2 ". --> Use module 21")
                        NIL)
                       (COND
                        ((OR (NULL SUBST_3) (LESSP SUBST_3 1000000))
                         (PROGN
                          (PRIN2 " if necessary with 'as subst_3 1000000);'. ")
                          NIL)))
                       (TERPRI)))
                     (T NIL))))
                  ((NULL (CAR (CADR CO)))
                   (COND
                    ((AND VERBTM PRINT_)
                     (PROGN
                      (PROGN
                       (PRIN2 F)
                       (PRIN2 " does not come up linearly in ")
                       (PRIN2 P)
                       NIL)
                      (TERPRI)))
                    (T NIL)))
                  (T
                   (PROGN
                    (SETQ P2
                            (ADDSQ (SIMP (CAR CO))
                                   (MULTSQ (MKSQ F 1) (SIMP (CADR CO)))))
                    (COND
                     ((EQUAL (SIMPLIFYSQ P2 FTEM_ T NIL NIL) (LIST (CONS 1 1)))
                      (PROGN
                       (PROGN
                        (PRIN2 "The linear part of ")
                        (PRIN2 F)
                        (PRIN2 " in ")
                        (PRIN2 P)
                        (PRIN2 " is non-zero. ")
                        NIL)
                       (TERPRI)))
                     (T
                      (PROGN
                       (SETQ PLI
                               (CONS
                                (LIST P F P2 HIPOW*
                                      (NO_OF_TM_SF (CAR (SIMP (CAR CO))))
                                      (NO_OF_TM_SF (CAR (SIMP (CADR CO)))))
                                PLI))
                       (COND ((NULL BEST) (SETQ BEST (CAR PLI)))
                             ((OR (LESSP (CAR (CDDDAR PLI)) (CAR (CDDDR BEST)))
                                  (AND
                                   (EQUAL (CAR (CDDDAR PLI))
                                          (CAR (CDDDR BEST)))
                                   (OR
                                    (LESSP (CADR (CDDDAR PLI))
                                           (CADR (CDDDR BEST)))
                                    (AND
                                     (EQUAL (CADR (CDDDAR PLI))
                                            (CADR (CDDDR BEST)))
                                     (LESSP (CADDR (CDDDAR PLI))
                                            (CADDR (CDDDR BEST)))))))
                              (SETQ BEST (CAR PLI))))))))))))))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (COND
       ((AND VERBTM PRINT_)
        (PROGN
         (PROGN (PRIN2 "Available linearizations: ") NIL)
         (TERPRI)
         (PROG (H)
           (SETQ H PLI)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (PROGN
               (PROGN
                (PRIN2 "equation: ")
                (PRIN2 (CAR H))
                (PRIN2 "(")
                (PRIN2 (GET (CAR H) 'TERMS))
                (PRIN2 ")")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 " linearized function: ")
                (PRIN2 (CADR H))
                (PRIN2 " hipow: ")
                (PRIN2 (CADDDR H))
                (PRIN2 " #terms0: ")
                (PRIN2 (CAR (CDDDDR H)))
                (PRIN2 " #terms1: ")
                (PRIN2 (CADR (CDDDDR H)))
                NIL)
               (TERPRI)
               NIL))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB)))))
      (COND
       ((AND PLI EXPERT_MODE)
        (PROGN
         (PROGN
          (PRIN2
           "If you do not want to proceed then enter ';' else input the equation name: ")
          NIL)
         (CHANGE_PROMPT_TO "")
         (SETQ P (TERMREAD))
         (COND
          ((NEQ P '|;|)
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND PLI (NEQ (CAAR PLI) P))) (RETURN NIL)))
              (SETQ PLI (CDR PLI))
              (GO WHILELABEL))
            (COND
             ((NULL PLI)
              (PROGN
               (PROGN
                (PRIN2 "This is not one of the linearizable equations")
                NIL)
               (TERPRI)))
             (T
              (PROGN
               (PROGN
                (PRIN2 "What is the name of the linearized function: ")
                NIL)
               (SETQ F (TERMREAD))
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND PLI (OR (NEQ (CAAR PLI) P) (NEQ (CADAR PLI) F))))
                   (RETURN NIL)))
                 (SETQ PLI (CDR PLI))
                 (GO WHILELABEL))
               (COND
                ((NULL PLI)
                 (PROGN
                  (PROGN
                   (PRIN2 "This is not one of the linearizable unknowns")
                   NIL)
                  (TERPRI)))
                (T (SETQ BEST (CAR PLI))))))))))
         (RESTORE_INTERACTIVE_PROMPT)
         NIL)))
      (COND
       (BEST
        (PROGN
         (COND
          ((AND VERBTM PRINT_)
           (PROGN
            (PROGN
             (PRIN2 "Next try is equation ")
             (PRIN2 (CAR BEST))
             (PRIN2 "(")
             (PRIN2 (GET (CAR BEST) 'TERMS))
             (PRIN2 ") splitted wrt ")
             (PRIN2 (CADR BEST))
             NIL)
            (TERPRI))))
         (SETQ TO_DO_LIST
                 (CONS (LIST 'SPLIT_INTO_CASES (CADDR BEST)) TO_DO_LIST))
         (SETQ RTRN ARGLIST))))
      (RETURN RTRN))) 
(PUT 'QUIT_IF_NO_ALG_SOL 'NUMBER-OF-ARGS 1) 
(PUT 'QUIT_IF_NO_ALG_SOL 'DEFINED-ON-LINE '2087) 
(PUT 'QUIT_IF_NO_ALG_SOL 'DEFINED-IN-FILE 'CRACK/CRGENSEP.RED) 
(PUT 'QUIT_IF_NO_ALG_SOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUIT_IF_NO_ALG_SOL (ARGLIST)
    (PROG (PDES P RTRN)
      (SETQ PDES (CAR ARGLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PDES (NULL CONTRADICTION_))) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PDES))
         (SETQ PDES (CDR PDES))
         (COND
          ((AND (EQUAL 1 (LENGTH (GET P 'FCTS)))
                (EQUAL 1 (LENGTH (GET P 'KERN))) (NULL (GET P 'LINEAR_)))
           (COND
            ((EQUAL 2 (GET P 'FAC))
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 "Equation ")
               (PRIN2 P)
               (PRIN2 " has no rational solution: ")
               NIL)
              (TERPRI)
              (MATHPRINT (LIST '*SQ (GET P 'SQVAL) NIL))
              (SETQ CONTRADICTION_ T)))
            ((OR (NULL (GET P 'FAC)) (EQUAL 1 (GET P 'FAC)))
             (PROGN
              (SETQ RTRN ARGLIST)
              (COND
               (PRINT_
                (PROGN
                 (TERPRI)
                 (PROGN (PRIN2 P) (PRIN2 " is to be factorized next.") NIL)
                 (TERPRI))))
              (SETQ TO_DO_LIST
                      (CONS (LIST 'FIND_FACTORIZATION (LIST P)) TO_DO_LIST))
              NIL))
            (T
             (PROGN
              (SETQ RTRN ARGLIST)
              (COND
               (PRINT_
                (PROGN
                 (TERPRI)
                 (PROGN (PRIN2 P) (PRIN2 " is to be case splitted next.") NIL)
                 (TERPRI))))
              (SETQ TO_DO_LIST
                      (UNION (LIST (LIST 'FACTORIZE_ANY (LIST P)))
                             TO_DO_LIST))))))))
        (GO WHILELABEL))
      (COND (CONTRADICTION_ (SETQ RTRN NIL)))
      (RETURN RTRN))) 
(ENDMODULE) 