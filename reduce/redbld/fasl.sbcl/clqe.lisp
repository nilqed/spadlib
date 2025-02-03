(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLQE)) 
(REVISION 'CLQE "$Id: clqe.red 6043 2021-09-20 07:58:36Z thomas-sturm $") 
(COPYRIGHT 'CLQE "(c) 1995-2009 A. Dolzmann, T. Sturm, 2010-2017 T. Sturm") 
(PUT 'TAGGEDCONTAINERELEMENTL 'ASSERT_DYNTYPECHK 'TAGGEDCONTAINERELEMENTLP) 
(FLAG '(TAGGEDCONTAINERELEMENTL) 'ASSERT_DYNTYPE) 
(PUT 'CONTAINERELEMENTL 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(CONTAINERELEMENTL) 'ASSERT_DYNTYPE) 
(PUT 'CONTAINERELEMENT 'ASSERT_DYNTYPECHK 'CONTAINERELEMENTP) 
(FLAG '(CONTAINERELEMENT) 'ASSERT_DYNTYPE) 
(PUT 'VARLIST 'ASSERT_DYNTYPECHK 'VARLISTP) 
(FLAG '(VARLIST) 'ASSERT_DYNTYPE) 
(PUT 'ANSWER 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(ANSWER) 'ASSERT_DYNTYPE) 
(PUT 'SUBSTTRIPLET 'ASSERT_DYNTYPECHK 'SUBSTTRIPLETP) 
(FLAG '(SUBSTTRIPLET) 'ASSERT_DYNTYPE) 
(PUT 'TAGGEDCONTAINERELEMENTLP 'NUMBER-OF-ARGS 1) 
(PUT 'TAGGEDCONTAINERELEMENTLP 'DEFINED-ON-LINE '48) 
(PUT 'TAGGEDCONTAINERELEMENTLP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'TAGGEDCONTAINERELEMENTLP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAGGEDCONTAINERELEMENTLP (X)
    (AND (PAIRP X) (MEMQ (CAR X) '(ELIM FAILED LOCAL NONOCC)) (LISTP (CDR X)))) 
(PUT 'CONTAINERELEMENTP 'NUMBER-OF-ARGS 1) 
(PUT 'CONTAINERELEMENTP 'DEFINED-ON-LINE '51) 
(PUT 'CONTAINERELEMENTP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CONTAINERELEMENTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONTAINERELEMENTP (X) (EQCAR X 'CE)) 
(PUT 'VARLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'VARLISTP 'DEFINED-ON-LINE '54) 
(PUT 'VARLISTP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'VARLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARLISTP (X) (OR (EQ X 'BREAK) (LISTP X))) 
(PUT 'SUBSTTRIPLETP 'NUMBER-OF-ARGS 1) 
(PUT 'SUBSTTRIPLETP 'DEFINED-ON-LINE '57) 
(PUT 'SUBSTTRIPLETP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'SUBSTTRIPLETP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBSTTRIPLETP (X) (AND (LISTP X) (EQN (LENGTH X) 3))) 
(PUT 'CONTAINER 'ASSERT_DYNTYPECHK 'CONTAINERP) 
(FLAG '(CONTAINER) 'ASSERT_DYNTYPE) 
(PUT 'CONTAINERP 'NUMBER-OF-ARGS 1) 
(PUT 'CONTAINERP 'DEFINED-ON-LINE '66) 
(PUT 'CONTAINERP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CONTAINERP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONTAINERP (X) (OR (NULL X) (PAIRP X))) 
(PUT 'ELIMINATIONRESULT 'ASSERT_DYNTYPECHK 'PAIRP) 
(FLAG '(ELIMINATIONRESULT) 'ASSERT_DYNTYPE) 
(PUT 'EXTENDEDQERESULT 'ASSERT_DYNTYPECHK 'ALISTP) 
(FLAG '(EXTENDEDQERESULT) 'ASSERT_DYNTYPE) 
(PUT 'EQUATIONL 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(EQUATIONL) 'ASSERT_DYNTYPE) 
(DE CE_MK (VL F V ETERM AN) (LIST 'CE VL F V ETERM AN)) 
(PUT 'CE_MK 'NUMBER-OF-ARGS 5) 
(PUTC 'CE_MK 'INLINE '(LAMBDA (VL F V ETERM AN) (LIST 'CE VL F V ETERM AN))) 
(DE CE_VL (X) (CAR (CDR X))) 
(PUT 'CE_VL 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_VL 'INLINE '(LAMBDA (X) (CAR (CDR X)))) 
(DE CE_F (X) (CADR (CDR X))) 
(PUT 'CE_F 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_F 'INLINE '(LAMBDA (X) (CADR (CDR X)))) 
(DE CE_V (X) (CADDR (CDR X))) 
(PUT 'CE_V 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_V 'INLINE '(LAMBDA (X) (CADDR (CDR X)))) 
(DE CE_ETERM (X) (CADDDR (CDR X))) 
(PUT 'CE_ETERM 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_ETERM 'INLINE '(LAMBDA (X) (CADDDR (CDR X)))) 
(DE CE_ANS (X) (NTH (CDR X) 5)) 
(PUT 'CE_ANS 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_ANS 'INLINE '(LAMBDA (X) (NTH (CDR X) 5))) 
(PUT 'CO_NEW 'NUMBER-OF-ARGS 0) 
(DE CO_NEW NIL (CONS NIL NIL)) 
(DE CO_DATA (CO) (CAR CO)) 
(PUT 'CO_DATA 'NUMBER-OF-ARGS 1) 
(PUT 'CO_DATA 'DEFINED-ON-LINE '110) 
(PUT 'CO_DATA 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CO_DATA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'CO_DATA 'INLINE '(LAMBDA (CO) (CAR CO))) 
(DE CO_DYNL (CO) (CDR CO)) 
(PUT 'CO_DYNL 'NUMBER-OF-ARGS 1) 
(PUT 'CO_DYNL 'DEFINED-ON-LINE '113) 
(PUT 'CO_DYNL 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CO_DYNL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'CO_DYNL 'INLINE '(LAMBDA (CO) (CDR CO))) 
(DE CO_SETDATA (CO DATA) (RPLACA CO DATA)) 
(PUT 'CO_SETDATA 'NUMBER-OF-ARGS 2) 
(PUT 'CO_SETDATA 'DEFINED-ON-LINE '116) 
(PUT 'CO_SETDATA 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CO_SETDATA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CO_SETDATA 'INLINE '(LAMBDA (CO DATA) (RPLACA CO DATA))) 
(DE CO_SETDYNL (CO DYNL) (RPLACD CO DYNL)) 
(PUT 'CO_SETDYNL 'NUMBER-OF-ARGS 2) 
(PUT 'CO_SETDYNL 'DEFINED-ON-LINE '119) 
(PUT 'CO_SETDYNL 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CO_SETDYNL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CO_SETDYNL 'INLINE '(LAMBDA (CO DYNL) (RPLACD CO DYNL))) 
(PUT 'CO_SAVE 'NUMBER-OF-ARGS 2) 
(DE CO_SAVE (CO DOL)
    (COND ((AND *RLQEDFS *RLQEDYN) (CO_DYNPUSH CO DOL))
          (*RLQEDFS (CO_PUSH CO DOL)) (T (CO_ENQUEUE CO DOL)))) 
(PUT 'CO_DYNPUSH 'NUMBER-OF-ARGS 2) 
(DE CO_DYNPUSH (CO DOL)
    (PROGN
     (PROG (CE)
       (SETQ CE DOL)
      LAB
       (COND ((NULL CE) (RETURN NIL)))
       ((LAMBDA (CE) (SETQ CO (CO_DYNPUSH1 CO CE))) (CAR CE))
       (SETQ CE (CDR CE))
       (GO LAB))
     CO)) 
(PUT 'CO_DYNPUSH1 'NUMBER-OF-ARGS 2) 
(DE CO_DYNPUSH1 (CO CE)
    (PROG (F VL)
      (SETQ F (CADR (CDR CE)))
      (SETQ VL (CAR (CDR CE)))
      (COND
       ((LTO_HMEMBER (CONS VL F) (CDR CO) 'CO_HFN)
        (PROGN
         (COND ((AND *RLVERBOSE *RLQEVB *RLQEVBOLD) (IOTO_PRIN2 "@")))
         (RETURN CO))))
      (RPLACD CO (LTO_HINSERT (CONS VL F) (CDR CO) 'CO_HFN))
      (RPLACA CO (CONS CE (CAR CO)))
      (RETURN CO))) 
(PUT 'CO_HFN 'NUMBER-OF-ARGS 1) 
(DE CO_HFN (ITEM) (LIST (CL_FVARL1 (CDR ITEM)) (RL_ATNUM (CDR ITEM)))) 
(PUT 'CO_PUSH 'NUMBER-OF-ARGS 2) 
(DE CO_PUSH (CO DOL)
    (PROGN
     (PROG (CE)
       (SETQ CE DOL)
      LAB
       (COND ((NULL CE) (RETURN NIL)))
       ((LAMBDA (CE) (SETQ CO (CO_PUSH1 CO CE))) (CAR CE))
       (SETQ CE (CDR CE))
       (GO LAB))
     CO)) 
(PUT 'CO_PUSH1 'NUMBER-OF-ARGS 2) 
(DE CO_PUSH1 (CO CE) (RPLACA CO (CO_PUSH2 (CAR CO) CE))) 
(PUT 'CO_PUSH2 'NUMBER-OF-ARGS 2) 
(DE CO_PUSH2 (CO CE)
    (COND
     ((CO_MEMBER CE CO)
      (PROGN (COND ((AND *RLVERBOSE *RLQEVB *RLQEVBOLD) (IOTO_PRIN2 "@"))) CO))
     (T (CONS CE CO)))) 
(PUT 'CO_ENQUEUE 'NUMBER-OF-ARGS 2) 
(DE CO_ENQUEUE (CO DOL) (RPLACA CO (CO_ENQUEUE1 (CAR CO) DOL))) 
(PUT 'CO_ENQUEUE1 'NUMBER-OF-ARGS 2) 
(DE CO_ENQUEUE1 (CO DOL)
    (PROGN
     (COND
      ((AND (NULL CO) DOL)
       (PROGN
        (SETQ CO (LIST NIL (CAR DOL)))
        (SETCAR CO (CDR CO))
        (SETQ DOL (CDR DOL)))))
     (PROG (X)
       (SETQ X DOL)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (COND
           ((NOT (CO_MEMBER X (CDR CO)))
            (SETCAR CO (SETCDR (CAR CO) (LIST X))))))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     CO)) 
(PUT 'CO_GET 'NUMBER-OF-ARGS 1) 
(DE CO_GET (CO) (COND (*RLQEDFS (CO_POP CO)) (T (CO_DEQUEUE CO)))) 
(PUT 'CO_POP 'NUMBER-OF-ARGS 1) 
(DE CO_POP (CO)
    (PROG (A)
      (SETQ A (CAR (CAR CO)))
      (RPLACA CO (CDR (CAR CO)))
      (RETURN (CONS A CO)))) 
(PUT 'CO_DEQUEUE 'NUMBER-OF-ARGS 1) 
(DE CO_DEQUEUE (CO)
    (PROG (A D)
      (PROG (G143)
        (SETQ G143 (CO_DEQUEUE1 (CAR CO)))
        (SETQ A (CAR G143))
        (SETQ D (CDR G143))
        (RETURN G143))
      (RPLACA CO D)
      (RETURN (CONS A CO)))) 
(PUT 'CO_DEQUEUE1 'NUMBER-OF-ARGS 1) 
(DE CO_DEQUEUE1 (CO)
    (COND (CO (CONS (CADR CO) (COND ((CDDR CO) (CONS (CAR CO) (CDDR CO)))))))) 
(PUT 'CO_LENGTH 'NUMBER-OF-ARGS 1) 
(DE CO_LENGTH (CO)
    (COND ((OR *RLQEDFS (NULL (CAR CO))) (LENGTH (CAR CO)))
          (T (DIFFERENCE (LENGTH (CAR CO)) 1)))) 
(PUT 'CO_MEMBER 'NUMBER-OF-ARGS 2) 
(DE CO_MEMBER (CE L)
    (AND L
         (OR
          (AND (EQUAL (CAR (CDR CE)) (CAR (CDR (CAR L))))
               (EQUAL (CADR (CDR CE)) (CADR (CDR (CAR L)))))
          (CO_MEMBER CE (CDR L))))) 
(PUT 'CO_STAT 'NUMBER-OF-ARGS 1) 
(DE CO_STAT (CO)
    (PROG (AL W N)
      (SETQ N 0)
      (PROG (CE)
        (SETQ CE (CAR CO))
       LAB
        (COND ((NULL CE) (RETURN NIL)))
        ((LAMBDA (CE)
           (PROGN
            (SETQ N (LENGTH (CAR (CDR CE))))
            (SETQ W (ASSOC N AL))
            (COND (W (SETCDR W (PLUS (CDR W) 1)))
                  (T (SETQ AL (CONS (CONS N 1) AL))))))
         (CAR CE))
        (SETQ CE (CDR CE))
        (GO LAB))
      (RETURN (SORT AL (FUNCTION (LAMBDA (X Y) (GEQ (CAR X) (CAR Y)))))))) 
(PUT 'JUNCTIONL 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(JUNCTIONL) 'ASSERT_DYNTYPE) 
(PUT 'JUNCTION 'ASSERT_DYNTYPECHK 'PAIRP) 
(FLAG '(JUNCTION) 'ASSERT_DYNTYPE) 
(DE CL_MKJ (F AN) (CONS F AN)) 
(PUT 'CL_MKJ 'NUMBER-OF-ARGS 2) 
(PUTC 'CL_MKJ 'INLINE '(LAMBDA (F AN) (CONS F AN))) 
(DE CL_JF (J) (CAR J)) 
(PUT 'CL_JF 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_JF 'INLINE '(LAMBDA (J) (CAR J))) 
(DE CL_JA (J) (CDR J)) 
(PUT 'CL_JA 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_JA 'INLINE '(LAMBDA (J) (CDR J))) 
(DE CL_CO2J (X) (CONS (CADR (CDR X)) (NTH (CDR X) 5))) 
(PUT 'CL_CO2J 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_CO2J 'INLINE '(LAMBDA (X) (CONS (CADR (CDR X)) (NTH (CDR X) 5)))) 
(DE CL_ERTH (ER) (CAR ER)) 
(PUT 'CL_ERTH 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_ERTH 'INLINE '(LAMBDA (ER) (CAR ER))) 
(DE CL_EREQR (ER) (CDR ER)) 
(PUT 'CL_EREQR 'NUMBER-OF-ARGS 1) 
(PUTC 'CL_EREQR 'INLINE '(LAMBDA (ER) (CDR ER))) 
(DE CL_MKER (THEO EQR) (CONS THEO EQR)) 
(PUT 'CL_MKER 'NUMBER-OF-ARGS 2) 
(PUTC 'CL_MKER 'INLINE '(LAMBDA (THEO EQR) (CONS THEO EQR))) 
(DE CL_MK1EQR (F EQL) (LIST (CONS F EQL))) 
(PUT 'CL_MK1EQR 'NUMBER-OF-ARGS 2) 
(PUTC 'CL_MK1EQR 'INLINE '(LAMBDA (F EQL) (LIST (CONS F EQL)))) 
(RL_PROVIDESERVICE 'RL_GQE 'CL_GQE
                   '(RL_FBQE RL_SPECELIM RL_ESETUNION RL_BESTGAUSSP
                             RL_BETTERGAUSSP RL_QEFSOLSET RL_TRANSFORM
                             RL_QEMKANS RL_BETTERP RL_VARSEL RL_TRYGAUSS
                             RL_ELIMSET RL_TRANSLAT RL_NEGATEAT)) 
(PUT 'CL_GQE 'NUMBER-OF-ARGS 3) 
(DE CL_GQE (F THEO XBVL)
    (COND (*CLQENEW (CL_GQE_NEW F THEO XBVL))
          (T
           (PROG (ER THEO *RLQEGEN *RLSIPW *RLSIPO)
             (SETQ *RLSIPW (SETQ *RLQEGEN T))
             (SETQ ER (CL_QE1 F THEO XBVL))
             (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
             (SETQ THEO (RL_THSIMPL (CAR ER)))
             (RETURN (CONS THEO (RL_SIMPL (CAAR (CDR ER)) THEO (MINUS 1)))))))) 
(RL_PROVIDESERVICE 'RL_GQEA 'CL_GQEA
                   '(RL_SPECELIM RL_ESETUNION RL_BESTGAUSSP RL_BETTERGAUSSP
                                 RL_QEFSOLSET RL_TRANSFORM RL_QEMKANS
                                 RL_BETTERP RL_VARSEL RL_TRYGAUSS RL_ELIMSET
                                 RL_TRANSLAT RL_NEGATEAT)) 
(PUT 'CL_GQEA 'NUMBER-OF-ARGS 3) 
(DE CL_GQEA (F THEO XBVL)
    (COND (*CLQENEW (CL_GQEA_NEW F THEO XBVL))
          (T
           (PROG (ER THEO EQR *RLQEGEN *RLSIPW *RLSIPO *RLQEANS)
             (SETQ *RLSIPW (SETQ *RLQEGEN (SETQ *RLQEANS T)))
             (SETQ ER (CL_QE1 F THEO XBVL))
             (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
             (SETQ THEO (RL_THSIMPL (CAR ER)))
             (SETQ EQR
                     (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                       (SETQ PR (CDR ER))
                       (COND ((NULL PR) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (PR)
                                           (CONS
                                            (RL_SIMPL (CAR PR) THEO (MINUS 1))
                                            (CDR PR)))
                                         (CAR PR))
                                        NIL)))
                      LOOPLABEL
                       (SETQ PR (CDR PR))
                       (COND ((NULL PR) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (PR)
                                   (CONS (RL_SIMPL (CAR PR) THEO (MINUS 1))
                                         (CDR PR)))
                                 (CAR PR))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN (CONS THEO EQR)))))) 
(RL_PROVIDESERVICE 'RL_QE 'CL_QE
                   '(RL_FBQE RL_SPECELIM RL_ESETUNION RL_BESTGAUSSP
                             RL_BETTERGAUSSP RL_QEFSOLSET RL_TRANSFORM
                             RL_QEMKANS RL_BETTERP RL_VARSEL RL_TRYGAUSS
                             RL_ELIMSET RL_TRANSLAT RL_NEGATEAT)) 
(PUT 'CL_QE 'NUMBER-OF-ARGS 2) 
(DE CL_QE (F THEO)
    (COND (*CLQENEW (CL_QE_NEW F THEO))
          (T
           (PROG (ER *RLSIPW *RLSIPO)
             (SETQ *RLSIPW (SETQ *RLSIPO T))
             (SETQ ER (CL_QE1 F THEO NIL))
             (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
             (RETURN (CAAR (CDR ER))))))) 
(RL_PROVIDESERVICE 'RL_QEA 'CL_QEA
                   '(RL_SPECELIM RL_ESETUNION RL_BESTGAUSSP RL_BETTERGAUSSP
                                 RL_QEFSOLSET RL_TRANSFORM RL_QEMKANS
                                 RL_BETTERP RL_VARSEL RL_TRYGAUSS RL_ELIMSET
                                 RL_TRANSLAT RL_NEGATEAT)) 
(PUT 'CL_QEA 'NUMBER-OF-ARGS 2) 
(DE CL_QEA (F THEO)
    (COND (*CLQENEW (CL_QEA_NEW F THEO))
          (T
           (PROG (ER *RLSIPW *RLSIPO *RLQEANS)
             (SETQ *RLSIPW (SETQ *RLSIPO (SETQ *RLQEANS T)))
             (SETQ ER (CL_QE1 F THEO NIL))
             (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
             (RETURN (CDR ER)))))) 
(PUT 'CL_QE1 'NUMBER-OF-ARGS 3) 
(DE CL_QE1 (F THEO XBVL)
    (PROG (Q QL VARL VARLL BVL SVF RESULT W RVL JL N)
      (SETQ N 0)
      (COND (*RLQEPNF (SETQ F (RL_PNF F))))
      (SETQ F (RL_SIMPL F THEO (MINUS 1)))
      (COND ((EQ F 'INCTHEO) (RETURN (RL_EXCEPTION 'INCTHEO))))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN (CONS THEO (LIST (CONS F NIL))))))
      (PROG (G144 G145)
        (SETQ G144 (CL_SPLIT F))
        (SETQ G145 G144)
        (SETQ QL (CAR G144))
        (SETQ G144 (CDR G144))
        (SETQ VARLL (CAR G144))
        (SETQ G144 (CDR G144))
        (SETQ F (CAR G144))
        (SETQ G144 (CDR G144))
        (SETQ BVL (CAR G144))
        (SETQ G144 (CDR G144))
        (RETURN G145))
      (SETQ THEO
              (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                (SETQ ATF THEO)
               STARTOVER
                (COND ((NULL ATF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (ATF)
                           (COND
                            ((NULL (INTERSECTION (RL_VARLAT ATF) BVL))
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
                            ((NULL (INTERSECTION (RL_VARLAT ATF) BVL))
                             (LIST ATF))))
                         (CAR ATF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ ATF (CDR ATF))
                (GO LOOPLABEL)))
      (SETQ BVL (UNION BVL XBVL))
      (PROG (G146 G147)
        (SETQ G146 (CL_QE1-ITERATE QL VARLL F THEO BVL))
        (SETQ G147 G146)
        (SETQ QL (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ VARLL (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ Q (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ RVL (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ JL (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ THEO (CAR G146))
        (SETQ G146 (CDR G146))
        (SETQ SVF (CAR G146))
        (SETQ G146 (CDR G146))
        (RETURN G147))
      (SETQ JL (CL_QE1-REQUANTIFY QL VARLL Q RVL JL))
      (COND
       ((AND *RLQEANS (NULL QL))
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_TPRIN2 "+++ Postprocessing answer:")
            (SETQ N (LENGTH JL)))))
         (SETQ RESULT
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J JL)
                  STARTOVER
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (J)
                              (PROGN
                               (COND
                                (*RLVERBOSE
                                 (IOTO_PRIN2
                                  (LIST " [" (SETQ N (DIFFERENCE N 1))))))
                               (SETQ W
                                       (LIST
                                        (CONS (CAR J) (RL_QEMKANS (CDR J)))))
                               (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "]"))))
                               W))
                            (CAR J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ J (CDR J))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (J)
                              (PROGN
                               (COND
                                (*RLVERBOSE
                                 (IOTO_PRIN2
                                  (LIST " [" (SETQ N (DIFFERENCE N 1))))))
                               (SETQ W
                                       (LIST
                                        (CONS (CAR J) (RL_QEMKANS (CDR J)))))
                               (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "]"))))
                               W))
                            (CAR J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J (CDR J))
                   (GO LOOPLABEL)))
         NIL))
       (T
        (PROGN
         (SETQ F (CAR (CAR JL)))
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2
            (LIST "+++ Final simplification ... " (CL_ATNUM F) " -> "))))
         (SETQ F (RL_SIMPL F THEO (MINUS 1)))
         (COND (*RLVERBOSE (IOTO_PRIN2T (CL_ATNUM F))))
         (COND
          ((AND *RLQEFB RVL)
           (PROGN
            (COND
             ((NOT
               ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
                (COND ((ATOM F) F) (T (CAR F)))))
              (PROGN
               (COND
                (*RLVERBOSE
                 (IOTO_TPRIN2T
                  "++++ No more quantifiers after simplification")))
               (SETQ RESULT F)))
             (T
              (PROGN
               (COND
                (*RLVERBOSE
                 (IOTO_TPRIN2 (LIST "++++ Entering fallback QE: "))))
               (PROG (G148)
                 (SETQ G148 (RL_FBQE F THEO))
                 (SETQ THEO (CAR G148))
                 (SETQ RESULT (CDR G148))
                 (RETURN G148)))))))
          (T (SETQ RESULT F)))
         (SETQ RESULT (LIST (CONS RESULT NIL)))
         NIL)))
      (RETURN (CONS THEO RESULT)))) 
(PUT 'CL_SPLIT 'NUMBER-OF-ARGS 1) 
(DE CL_SPLIT (F)
    (PROG (Q OP QL VARL VARLL BVL)
      (SETQ Q (SETQ OP (COND ((ATOM F) F) (T (CAR F)))))
      (COND ((NOT (OR (EQ Q 'EX) (EQ Q 'ALL))) (RETURN (LIST NIL NIL F NIL))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((NEQ OP Q)
           (PROGN
            (PROGN (SETQ QL (CONS Q QL)) Q)
            (PROGN (SETQ VARLL (CONS VARL VARLL)) VARL)
            (SETQ Q OP)
            (SETQ VARL NIL))))
         (PROG (W1) (SETQ W1 (CADR F)) (SETQ VARL (CONS W1 VARL)) (RETURN W1))
         (PROG (W1) (SETQ W1 (CADR F)) (SETQ BVL (CONS W1 BVL)) (RETURN W1))
         (SETQ F (CADDR F)))
        (COND
         ((NOT
           (NOT
            ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
             (SETQ OP (COND ((ATOM F) F) (T (CAR F)))))))
          (GO REPEATLABEL))))
      (PROGN (SETQ QL (CONS Q QL)) Q)
      (PROGN (SETQ VARLL (CONS VARL VARLL)) VARL)
      (RETURN (LIST QL VARLL F BVL)))) 
(PUT 'CL_UNSPLIT 'NUMBER-OF-ARGS 3) 
(DE CL_UNSPLIT (QL VARLL F)
    (PROG (RES VARL)
      (SETQ RES F)
      (PROG (Q)
        (SETQ Q QL)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (SETQ VARL (PROG1 (CAR VARLL) (SETQ VARLL (CDR VARLL))))
            (PROG (V)
              (SETQ V VARL)
             LAB
              (COND ((NULL V) (RETURN NIL)))
              ((LAMBDA (V) (SETQ RES (LIST Q V RES))) (CAR V))
              (SETQ V (CDR V))
              (GO LAB))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (RETURN RES))) 
(PUT 'CL_QE1-ITERATE 'NUMBER-OF-ARGS 5) 
(DE CL_QE1-ITERATE (QL VARLL F THEO BVL)
    (PROG (SVRLIDENTIFY SVRLQEPRECISE SVRLQEAPRECISE Q VARL SVF RVL JL)
      (SETQ SVRLIDENTIFY *RLIDENTIFY)
      (SETQ JL (LIST (CONS F NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NULL RVL) QL)) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR (CAR JL)))
         (SETQ Q (PROG1 (CAR QL) (SETQ QL (CDR QL))))
         (SETQ VARL (PROG1 (CAR VARLL) (SETQ VARLL (CDR VARLL))))
         (COND ((AND *RLQEANS (NULL QL)) (SETQ SVF F)))
         (COND
          (*RLVERBOSE (IOTO_TPRIN2 (LIST "---- " (CONS Q (REVERSE VARL))))))
         (SETQ SVRLQEPRECISE *RLQEPRECISE)
         (SETQ SVRLQEAPRECISE *RLQEAPRECISE)
         (COND (QL (PROGN (OFF1 'RLQEPRECISE) (OFF1 'RLQEAPRECISE))))
         (PROG (G149 G150)
           (SETQ G149 (CL_QEBLOCK F Q VARL THEO (AND *RLQEANS (NULL QL)) BVL))
           (SETQ G150 G149)
           (SETQ RVL (CAR G149))
           (SETQ G149 (CDR G149))
           (SETQ JL (CAR G149))
           (SETQ G149 (CDR G149))
           (SETQ THEO (CAR G149))
           (SETQ G149 (CDR G149))
           (RETURN G150))
         (COND
          (QL
           (PROGN
            (ONOFF 'RLQEPRECISE SVRLQEPRECISE)
            (ONOFF 'RLQEAPRECISE SVRLQEAPRECISE))))
         NIL)
        (GO WHILELABEL))
      (ONOFF 'RLIDENTIFY SVRLIDENTIFY)
      (RETURN (LIST QL VARLL Q RVL JL THEO SVF)))) 
(PUT 'CL_QE1-REQUANTIFY 'NUMBER-OF-ARGS 5) 
(DE CL_QE1-REQUANTIFY (QL VARLL Q RVL JL)
    (PROG (XX XXV SCVARLL VARL)
      (COND ((NOT RVL) (RETURN JL)))
      (COND (*RLVERBOSE (IOTO_TPRIN2 "+++ Requantification ... ")))
      (SETQ JL
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J JL)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ XX (CAR J))
                                     (SETQ XXV (CL_FVARL XX))
                                     (PROG (V)
                                       (SETQ V RVL)
                                      LAB
                                       (COND ((NULL V) (RETURN NIL)))
                                       ((LAMBDA (V)
                                          (COND
                                           ((MEMQ V XXV)
                                            (SETQ XX (LIST Q V XX)))))
                                        (CAR V))
                                       (SETQ V (CDR V))
                                       (GO LAB))
                                     (SETQ SCVARLL VARLL)
                                     (PROG (Q)
                                       (SETQ Q QL)
                                      LAB
                                       (COND ((NULL Q) (RETURN NIL)))
                                       ((LAMBDA (Q)
                                          (PROGN
                                           (SETQ VARL (CAR SCVARLL))
                                           (SETQ SCVARLL (CDR SCVARLL))
                                           (PROG (V)
                                             (SETQ V VARL)
                                            LAB
                                             (COND ((NULL V) (RETURN NIL)))
                                             ((LAMBDA (V)
                                                (COND
                                                 ((MEMQ V XXV)
                                                  (SETQ XX (LIST Q V XX)))))
                                              (CAR V))
                                             (SETQ V (CDR V))
                                             (GO LAB))))
                                        (CAR Q))
                                       (SETQ Q (CDR Q))
                                       (GO LAB))
                                     (CONS XX (CDR J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ XX (CAR J))
                             (SETQ XXV (CL_FVARL XX))
                             (PROG (V)
                               (SETQ V RVL)
                              LAB
                               (COND ((NULL V) (RETURN NIL)))
                               ((LAMBDA (V)
                                  (COND
                                   ((MEMQ V XXV) (SETQ XX (LIST Q V XX)))))
                                (CAR V))
                               (SETQ V (CDR V))
                               (GO LAB))
                             (SETQ SCVARLL VARLL)
                             (PROG (Q)
                               (SETQ Q QL)
                              LAB
                               (COND ((NULL Q) (RETURN NIL)))
                               ((LAMBDA (Q)
                                  (PROGN
                                   (SETQ VARL (CAR SCVARLL))
                                   (SETQ SCVARLL (CDR SCVARLL))
                                   (PROG (V)
                                     (SETQ V VARL)
                                    LAB
                                     (COND ((NULL V) (RETURN NIL)))
                                     ((LAMBDA (V)
                                        (COND
                                         ((MEMQ V XXV)
                                          (SETQ XX (LIST Q V XX)))))
                                      (CAR V))
                                     (SETQ V (CDR V))
                                     (GO LAB))))
                                (CAR Q))
                               (SETQ Q (CDR Q))
                               (GO LAB))
                             (CONS XX (CDR J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*RLVERBOSE (IOTO_PRIN2T "done")))
      (RETURN JL))) 
(PUT 'CL_QEBLOCK 'NUMBER-OF-ARGS 6) 
(DE CL_QEBLOCK (F Q VARL THEO ANS BVL)
    (PROG (RVL JL)
      (COND
       ((EQ Q 'EX)
        (RETURN (CL_QEBLOCK1 (RL_SIMPL F THEO (MINUS 1)) VARL THEO ANS BVL))))
      (PROG (G151 G152)
        (SETQ G151
                (CL_QEBLOCK1 (RL_SIMPL (RL_NNFNOT F) THEO (MINUS 1)) VARL THEO
                 ANS BVL))
        (SETQ G152 G151)
        (SETQ RVL (CAR G151))
        (SETQ G151 (CDR G151))
        (SETQ JL (CAR G151))
        (SETQ G151 (CDR G151))
        (SETQ THEO (CAR G151))
        (SETQ G151 (CDR G151))
        (RETURN G152))
      (RETURN
       (LIST RVL
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X JL)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (CONS (RL_NNFNOT (CAR X)) (CDR X)))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X) (CONS (RL_NNFNOT (CAR X)) (CDR X)))
                         (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             THEO)))) 
(SWITCH (LIST 'OFSFVS)) 
(PUT 'CL_QEBLOCK1 'NUMBER-OF-ARGS 5) 
(DE CL_QEBLOCK1 (F VARL THEO ANS BVL)
    (COND
     ((AND *OFSFVS (EQ (RL_CNAME (CAR (RL_SET NIL))) 'OFSF))
      (VS_BLOCK F VARL THEO ANS BVL))
     (*RLQEHEU (CL_QEBLOCK2 F VARL THEO ANS BVL))
     (T (CL_QEBLOCK3 F VARL THEO ANS BVL)))) 
(PUT 'CL_QEBLOCK2 'NUMBER-OF-ARGS 5) 
(DE CL_QEBLOCK2 (F VARL THEO ANS BVL)
    (PROG (*RLQEDFS ATL)
      (SETQ ATL (CL_ATL1 F))
      (SETQ *RLQEDFS T)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (COND
         ((SETDIFF (RL_VARLAT (CAR ATL)) VARL) (SETQ *RLQEDFS (SETQ ATL NIL)))
         (T (SETQ ATL (CDR ATL))))
        (GO WHILELABEL))
      (RETURN (CL_QEBLOCK3 F VARL THEO ANS BVL)))) 
(PUT 'CL_QEBLOCK3 'NUMBER-OF-ARGS 5) 
(DE CL_QEBLOCK3 (F VARL THEO ANS BVL)
    (PROG (W VLV DPTH)
      (SETQ VLV 0)
      (SETQ DPTH 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (SETQ DPTH (LENGTH VARL))
         (COND
          (*RLQEDFS
           (PROGN
            (IOTO_PRIN2 (LIST " [DFS"))
            (COND (*RLQEDYN (IOTO_PRIN2 (LIST " DYN"))))
            (COND
             (*RLQEVBOLD
              (PROGN
               (SETQ VLV (QUOTIENT DPTH 4))
               (IOTO_PRIN2T
                (LIST ": depth " DPTH ", watching " (DIFFERENCE DPTH VLV)
                      "]"))))
             (T (IOTO_PRIN2T (LIST "]"))))))
          (T (IOTO_PRIN2T (LIST " [BFS: depth " DPTH "]")))))))
      (RETURN (CL_QEBLOCK4 F VARL THEO ANS BVL DPTH VLV)))) 
(PUT 'CL_QEBLOCK4 'NUMBER-OF-ARGS 7) 
(DE CL_QEBLOCK4 (F VARL THEO ANS BVL DPTH VLV)
    (PROG (W CO REMVL NEWJ CVL COE WW C COUNT DELC OLDCOL COMAX COMAXN)
      (SETQ C 0)
      (SETQ COUNT 0)
      (SETQ DELC 0)
      (SETQ OLDCOL 0)
      (SETQ COMAX 0)
      (SETQ COMAXN 0)
      (COND (*RLQEGSD (SETQ F (RL_GSN F THEO 'DNF))))
      (SETQ CVL VARL)
      (SETQ CO (CO_NEW))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (PROG (X)
          (SETQ X (CDR F))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (SETQ CO (CO_SAVE CO (LIST (LIST 'CE CVL X NIL NIL NIL)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB)))
       (T (SETQ CO (CO_SAVE CO (LIST (LIST 'CE CVL F NIL NIL NIL))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR CO)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND *RLVERBOSE *RLQEDFS (NOT *RLQEVBOLD))
           (PROGN
            (SETQ WW (CAR (CO_STAT CO)))
            (COND
             ((OR (EQUAL COMAX 0) (LESSP (CAR WW) COMAX)
                  (AND (EQUAL (CAR WW) COMAX) (LESSP (CDR WW) COMAXN)))
              (PROGN
               (SETQ COMAX (CAR WW))
               (SETQ COMAXN (CDR WW))
               (IOTO_PRIN2 (LIST "[" COMAX ":" COMAXN "] "))))))))
         (COND (*RLQEIDENTIFY (ON1 'RLIDENTIFY)))
         (PROG (G153)
           (SETQ G153 (CO_GET CO))
           (SETQ COE (CAR G153))
           (SETQ CO (CDR G153))
           (RETURN G153))
         (SETQ CVL (CAR (CDR COE)))
         (SETQ COUNT (PLUS COUNT 1))
         (COND
          (*RLVERBOSE
           (COND
            (*RLQEDFS
             (COND
              (*RLQEVBOLD
               (PROGN
                (COND
                 ((EQUAL VLV (LENGTH CVL))
                  (IOTO_TPRIN2T (LIST "-- crossing: " (DIFFERENCE DPTH VLV)))))
                (IOTO_PRIN2 (LIST "[" (DIFFERENCE DPTH (LENGTH CVL))))))))
            (T
             (PROGN
              (COND
               ((EQUAL C 0)
                (PROGN
                 (IOTO_TPRIN2T (LIST "-- left: " (LENGTH CVL) " " CVL))
                 (SETQ C (PLUS (CO_LENGTH CO) 1)))))
              (IOTO_NTERPRI (PLUS (LENGTH (EXPLODE C)) 4))
              (IOTO_PRIN2 (LIST "[" C))
              (SETQ C (DIFFERENCE C 1)))))))
         (PROG (G154)
           (SETQ G154
                   (CL_QEVAR (CADR (CDR COE)) (CAR (CDR COE)) (NTH (CDR COE) 5)
                    THEO ANS BVL))
           (SETQ W (CAR G154))
           (SETQ THEO (CDR G154))
           (RETURN G154))
         (COND
          ((CAR W)
           (PROGN
            (SETQ W (CDR W))
            (COND
             (W
              (COND
               ((EQ (CAR (CDR (CAR W))) 'BREAK)
                (PROGN
                 (SETQ CO (CO_NEW))
                 (SETQ NEWJ
                         (LIST
                          (CONS (CADR (CDR (CAR W))) (NTH (CDR (CAR W)) 5))))))
               ((CDR CVL)
                (PROGN
                 (COND (*RLVERBOSE (SETQ OLDCOL (CO_LENGTH CO))))
                 (SETQ CO (CO_SAVE CO W))
                 (COND
                  (*RLVERBOSE
                   (SETQ DELC
                           (PLUS DELC OLDCOL
                                 (DIFFERENCE (LENGTH W) (CO_LENGTH CO))))))))
               (T
                (PROG (X)
                  (SETQ X W)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (SETQ NEWJ
                             (LTO_INSERT (CONS (CADR (CDR X)) (NTH (CDR X) 5))
                                         NEWJ)))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))))))))
          (T
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (APPEND (CONS "[Failed:" (CDR W)) (LIST "] ")))))
            (SETQ REMVL (UNION CVL REMVL))
            (SETQ NEWJ
                    (LTO_INSERT (CONS (CADR (CDR COE)) (NTH (CDR COE) 5))
                                NEWJ)))))
         (COND
          ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD))
           (PROGN
            (IOTO_PRIN2 "] ")
            (COND ((AND *RLQEDFS (NULL CVL)) (IOTO_PRIN2 ". ")))))))
        (GO WHILELABEL))
      (COND
       (*RLVERBOSE
        (IOTO_PRIN2
         (IOTO_PRINTLISTTOSTRING
          (LIST "[DEL " DELC "/" (PLUS COUNT DELC) " = "
                (QUOTIENT (TIMES 100.0 DELC) (PLUS COUNT DELC)) "%]")))))
      (COND (ANS (RETURN (LIST REMVL NEWJ THEO))))
      (RETURN
       (LIST REMVL
             (LIST
              (CONS
               ((LAMBDA (G156)
                  (COND ((AND G156 (CDR G156)) (CONS 'OR G156))
                        ((NULL G156) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G156))))
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X NEWJ)
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
             THEO)))) 
(PUT 'CL_QEVAR 'NUMBER-OF-ARGS 6) 
(DE CL_QEVAR (F VL AN THEO ANS BVL)
    (PROG (W CANDVL STATUS LEN)
      (SETQ LEN 0)
      (COND
       ((SETQ W (CL_TRANSFORM F VL AN THEO ANS BVL))
        (PROG (G157 G158)
          (SETQ G157 W)
          (SETQ G158 G157)
          (SETQ F (CAR G157))
          (SETQ G157 (CDR G157))
          (SETQ VL (CAR G157))
          (SETQ G157 (CDR G157))
          (SETQ AN (CAR G157))
          (SETQ G157 (CDR G157))
          (SETQ THEO (CAR G157))
          (SETQ G157 (CDR G157))
          (SETQ ANS (CAR G157))
          (SETQ G157 (CDR G157))
          (SETQ BVL (CAR G157))
          (SETQ G157 (CDR G157))
          (RETURN G158))))
      (COND ((SETQ W (CL_GAUSS F VL AN THEO ANS BVL)) (RETURN W)))
      (COND
       ((NEQ (SETQ W (RL_SPECELIM F VL THEO ANS BVL)) 'FAILED) (RETURN W)))
      (SETQ CANDVL (CL_VARSEL F VL THEO))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD)
             (GREATERP (SETQ LEN (LENGTH CANDVL)) 1))
        (IOTO_PRIN2 (LIST "{" LEN ":"))))
      (PROG (G159)
        (SETQ G159 (CL_PROCESS-CANDVL F VL AN THEO ANS BVL CANDVL))
        (SETQ STATUS (CAR G159))
        (SETQ W (CDR G159))
        (RETURN G159))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD)
             (GREATERP LEN 1))
        (IOTO_PRIN2 (LIST "}"))))
      (COND ((EQ STATUS 'NONOCC) (RETURN (CONS (CONS T W) THEO))))
      (COND ((EQ STATUS 'FAILED) (RETURN (CONS (CONS NIL W) THEO))))
      (COND ((EQ STATUS 'ELIM) (RETURN (CONS (CONS T (CAR W)) (CDR W)))))
      (REDERR (LIST "cl_qevar: bad status" STATUS)))) 
(PUT 'CL_TRANSFORM 'NUMBER-OF-ARGS 6) 
(DE CL_TRANSFORM (F VL AN THEO ANS BVL)
    (PROG (W)
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ W (RL_TRANSFORM V F VL AN THEO ANS BVL))
            (COND
             (W
              (PROG (G160 G161)
                (SETQ G160 W)
                (SETQ G161 G160)
                (SETQ F (CAR G160))
                (SETQ G160 (CDR G160))
                (SETQ VL (CAR G160))
                (SETQ G160 (CDR G160))
                (SETQ AN (CAR G160))
                (SETQ G160 (CDR G160))
                (SETQ THEO (CAR G160))
                (SETQ G160 (CDR G160))
                (SETQ ANS (CAR G160))
                (SETQ G160 (CDR G160))
                (SETQ BVL (CAR G160))
                (SETQ G160 (CDR G160))
                (RETURN G161))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (LIST F VL AN THEO ANS BVL)))) 
(PUT 'CL_GAUSS 'NUMBER-OF-ARGS 6) 
(DE CL_GAUSS (F VL AN THEO ANS BVL)
    (PROG (W WW)
      (SETQ W (RL_TRYGAUSS F VL THEO ANS BVL))
      (COND
       ((NEQ W 'FAILED)
        (PROGN
         (SETQ THEO (CDR W))
         (SETQ W (CAR W))
         (COND
          ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD)) (IOTO_PRIN2 "g")))
         (SETQ VL (LTO_DELQ (CAR W) VL))
         (SETQ WW (CL_ESETSUBST F (CAR W) (CDR W) VL AN THEO ANS BVL))
         (RETURN (CONS (CONS T (CAR WW)) (CDR WW)))))))) 
(PUT 'CL_VARSEL 'NUMBER-OF-ARGS 3) 
(DE CL_VARSEL (F VL THEO)
    (PROG (CANDVL LEN)
      (SETQ LEN 0)
      (COND ((NULL (CDR VL)) (SETQ CANDVL VL))
            (*RLQEVARSEL (SETQ CANDVL (RL_VARSEL F VL THEO)))
            (T (SETQ CANDVL (LIST (CAR VL)))))
      (RETURN CANDVL))) 
(PUT 'CL_PROCESS-CANDVL 'NUMBER-OF-ARGS 7) 
(DE CL_PROCESS-CANDVL (F VL AN THEO ANS BVL CANDVL)
    (PROG (W WW V ALP HIT STATUS)
      (PROG ()
       WHILELABEL
        (COND ((NOT CANDVL) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR CANDVL) (SETQ CANDVL (CDR CANDVL))))
         (SETQ ALP (CL_QEATAL F V THEO ANS))
         (COND
          ((EQUAL ALP '(NIL))
           (PROGN
            (COND
             ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD))
              (IOTO_PRIN2 "*")))
            (SETQ W
                    (LIST
                     (LIST 'CE (LTO_DELQ V VL) F NIL NIL
                           (AND ANS (CL_UPDANS V 'ARBITRARY NIL NIL AN ANS)))))
            (SETQ STATUS 'NONOCC)
            (SETQ CANDVL NIL)))
          ((EQUAL (CAR ALP) 'FAILED)
           (COND ((NULL W) (PROGN (SETQ W (CDR ALP)) (SETQ STATUS 'FAILED)))))
          (T
           (PROGN
            (COND
             ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD))
              (IOTO_PRIN2 "e")))
            (SETQ WW
                    (CL_ESETSUBST F V (RL_ELIMSET V ALP) (LTO_DELQ V VL) AN
                     THEO ANS BVL))
            (COND
             ((RL_BETTERP WW W) (PROGN (SETQ W WW) (SETQ STATUS 'ELIM))))))))
        (GO WHILELABEL))
      (RETURN (CONS STATUS W)))) 
(PUT 'CL_ESETSUBST 'NUMBER-OF-ARGS 8) 
(DE CL_ESETSUBST (F V ESET VL AN THEO ANS BVL)
    (PROG (A D U ELIMRES JUNCT W)
      (PROG ()
       WHILELABEL
        (COND ((NOT ESET) (RETURN NIL)))
        (PROGN
         (PROG (G162)
           (SETQ G162 (PROG1 (CAR ESET) (SETQ ESET (CDR ESET))))
           (SETQ A (CAR G162))
           (SETQ D (CDR G162))
           (RETURN G162))
         (PROG ()
          WHILELABEL
           (COND ((NOT D) (RETURN NIL)))
           (PROGN
            (SETQ U (PROG1 (CAR D) (SETQ D (CDR D))))
            (SETQ W (APPLY A (CONS BVL (CONS THEO (CONS F (CONS V U))))))
            (SETQ THEO (UNION THEO (CAR W)))
            (SETQ ELIMRES (RL_SIMPL (CDR W) THEO (MINUS 1)))
            (COND (*RLQEGSD (SETQ ELIMRES (RL_GSN ELIMRES THEO 'DNF))))
            (COND
             ((EQ ELIMRES 'TRUE)
              (PROGN
               (SETQ AN (CL_UPDANS V A U F AN ANS))
               (PROG (VV)
                 (SETQ VV VL)
                LAB
                 (COND ((NULL VV) (RETURN NIL)))
                 ((LAMBDA (VV)
                    (SETQ AN (CL_UPDANS VV 'ARBITRARY NIL NIL AN ANS)))
                  (CAR VV))
                 (SETQ VV (CDR VV))
                 (GO LAB))
               (SETQ JUNCT (LIST (LIST 'CE 'BREAK ELIMRES NIL NIL AN)))
               (SETQ ESET (SETQ D NIL))))
             ((NEQ ELIMRES 'FALSE)
              (COND
               ((EQ (COND ((ATOM ELIMRES) ELIMRES) (T (CAR ELIMRES))) 'OR)
                (PROG (SUBF)
                  (SETQ SUBF (CDR ELIMRES))
                 LAB
                  (COND ((NULL SUBF) (RETURN NIL)))
                  ((LAMBDA (SUBF)
                     (SETQ JUNCT
                             (CONS
                              (LIST 'CE VL SUBF NIL NIL
                                    (CL_UPDANS V A U F AN ANS))
                              JUNCT)))
                   (CAR SUBF))
                  (SETQ SUBF (CDR SUBF))
                  (GO LAB)))
               (T
                (SETQ JUNCT
                        (CONS
                         (LIST 'CE VL ELIMRES NIL NIL
                               (CL_UPDANS V A U F AN ANS))
                         JUNCT))))))
            NIL)
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN (CONS JUNCT THEO)))) 
(PUT 'CL_UPDANS 'NUMBER-OF-ARGS 6) 
(PUT 'CL_UPDANS 'DEFINED-ON-LINE '795) 
(PUT 'CL_UPDANS 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_UPDANS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_UPDANS (V A U F AN ANS)
    (COND (ANS (CONS (LIST V A U (COND (*RLQESTDANS F))) AN)))) 
(PUT 'CL_QEATAL 'NUMBER-OF-ARGS 4) 
(PUT 'CL_QEATAL 'DEFINED-ON-LINE '798) 
(PUT 'CL_QEATAL 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEATAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QEATAL (F V THEO ANS) (CL_QEATAL1 F V THEO T ANS)) 
(SWITCH (LIST 'RLATALTHEO)) 
(ON1 'RLATALTHEO) 
(PUT 'CL_QEATAL1 'NUMBER-OF-ARGS 5) 
(PUT 'CL_QEATAL1 'DEFINED-ON-LINE '807) 
(PUT 'CL_QEATAL1 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEATAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QEATAL1 (F V THEO FLG ANS)
    (PROG (OP W WW)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (SETQ W
              (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (LIST (CONS NIL NIL)))
                    ((EQ OP 'NOT)
                     (LIST (CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)))
                    ((EQ OP 'AND)
                     (PROGN
                      (COND
                       (*RLATALTHEO
                        (PROG (SUBF)
                          (SETQ SUBF (CDR F))
                         LAB
                          (COND ((NULL SUBF) (RETURN NIL)))
                          ((LAMBDA (SUBF)
                             (COND
                              ((AND (CL_ATFP SUBF)
                                    (NOT (MEMQ V (RL_VARLAT SUBF))))
                               (SETQ THEO (LTO_INSERT SUBF THEO)))))
                           (CAR SUBF))
                          (SETQ SUBF (CDR SUBF))
                          (GO LAB))))
                      (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SUBF (CDR F))
                        (COND ((NULL SUBF) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SUBF)
                                            (CL_QEATAL1 SUBF V THEO FLG ANS))
                                          (CAR SUBF))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SUBF (CDR SUBF))
                        (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (SUBF)
                                    (CL_QEATAL1 SUBF V THEO FLG ANS))
                                  (CAR SUBF))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
                    ((EQ OP 'OR)
                     (PROGN
                      (COND
                       (*RLATALTHEO
                        (PROG (SUBF)
                          (SETQ SUBF (CDR F))
                         LAB
                          (COND ((NULL SUBF) (RETURN NIL)))
                          ((LAMBDA (SUBF)
                             (COND
                              ((AND (CL_ATFP SUBF)
                                    (NOT (MEMQ V (RL_VARLAT SUBF))))
                               (SETQ THEO
                                       (LTO_INSERT (RL_NEGATEAT SUBF) THEO)))))
                           (CAR SUBF))
                          (SETQ SUBF (CDR SUBF))
                          (GO LAB))))
                      (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SUBF (CDR F))
                        (COND ((NULL SUBF) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SUBF)
                                            (CL_QEATAL1 SUBF V THEO FLG ANS))
                                          (CAR SUBF))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SUBF (CDR SUBF))
                        (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (SUBF)
                                    (CL_QEATAL1 SUBF V THEO FLG ANS))
                                  (CAR SUBF))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
                    ((EQ OP 'IMPL)
                     (LIST (CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)
                           (CL_QEATAL1 (CADDR F) V THEO FLG ANS)))
                    ((EQ OP 'REPL)
                     (LIST (CL_QEATAL1 (CADR F) V THEO FLG ANS)
                           (CL_QEATAL1 (CADDR F) V THEO (NOT FLG) ANS)))
                    ((EQ OP 'EQUIV)
                     (LIST (CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)
                           (CL_QEATAL1 (CADDR F) V THEO FLG ANS)
                           (CL_QEATAL1 (CADR F) V THEO FLG ANS)
                           (CL_QEATAL1 (CADDR F) V THEO (NOT FLG) ANS)))
                    ((OR (EQ OP 'EX) (EQ OP 'ALL))
                     (REDERR "argument formula not prenex"))
                    (T (LIST (RL_TRANSLAT F V THEO FLG ANS)))))
      (COND ((SETQ WW (ATSOC 'FAILED W)) (RETURN WW)))
      (RETURN (CL_ALPUNION W)))) 
(PUT 'CL_ALPUNION 'NUMBER-OF-ARGS 1) 
(PUT 'CL_ALPUNION 'DEFINED-ON-LINE '845) 
(PUT 'CL_ALPUNION 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_ALPUNION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_ALPUNION (PL)
    (PROG (UALL PALL)
      (PROG (PAIR)
        (SETQ PAIR PL)
       LAB
        (COND ((NULL PAIR) (RETURN NIL)))
        ((LAMBDA (PAIR)
           (PROGN
            (SETQ UALL (CONS (CAR PAIR) UALL))
            (SETQ PALL (CONS (CDR PAIR) PALL))))
         (CAR PAIR))
        (SETQ PAIR (CDR PAIR))
        (GO LAB))
      (RETURN (CONS (LTO_ALUNION UALL) (LTO_ALMERGE PALL 'PLUS2))))) 
(PUT 'CL_BETTERP 'NUMBER-OF-ARGS 2) 
(PUT 'CL_BETTERP 'DEFINED-ON-LINE '856) 
(PUT 'CL_BETTERP 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_BETTERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_BETTERP (NEW OLD)
    (COND (*CLQENEW (CL_BETTERP_NEW NEW OLD))
          (T
           (PROG (ATN)
             (SETQ ATN 0)
             (SETQ ATN (CL_BETTERP-COUNT (CAR NEW)))
             (COND
              ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
               (IOTO_PRIN2 (LIST "(" ATN ")"))))
             (RETURN
              (OR (NULL OLD) (LESSP ATN (CL_BETTERP-COUNT (CAR OLD))))))))) 
(PUT 'CL_BETTERP-COUNT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_BETTERP-COUNT 'DEFINED-ON-LINE '867) 
(PUT 'CL_BETTERP-COUNT 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_BETTERP-COUNT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_BETTERP-COUNT (COELL)
    (PROG (X FORALL-RESULT)
      (SETQ X COELL)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (X) (RL_ATNUM (CADR (CDR X)))) (CAR X))
                    FORALL-RESULT))
      (SETQ X (CDR X))
      (GO LAB1))) 
(PUT 'CL_QEIPO 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QEIPO 'DEFINED-ON-LINE '871) 
(PUT 'CL_QEIPO 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEIPO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QEIPO (F THEO)
    (PROG (W *RLQEANS)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ W (CL_QEIPO1 (CL_APNF (RL_SIMPL F THEO (MINUS 1))) THEO))
         (SETQ F (CDR W)))
        (COND ((NOT (NOT (CAR W))) (GO REPEATLABEL))))
      (RETURN F))) 
(PUT 'CL_QEIPO1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QEIPO1 'DEFINED-ON-LINE '884) 
(PUT 'CL_QEIPO1 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEIPO1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QEIPO1 (F THEO)
    (PROG (OP NF A ARGL NTHEO)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (PROG (SUBF)
           (SETQ SUBF THEO)
          LAB
           (COND ((NULL SUBF) (RETURN NIL)))
           ((LAMBDA (SUBF)
              (COND
               ((NOT (MEMQ (CADR F) (RL_VARLAT SUBF)))
                (SETQ NTHEO (CONS SUBF NTHEO)))))
            (CAR SUBF))
           (SETQ SUBF (CDR SUBF))
           (GO LAB))
         (SETQ NF (CL_QEIPO1 (CADDR F) NTHEO))
         (COND ((CAR NF) (RETURN (CONS T (LIST OP (CADR F) (CDR NF))))))
         (SETQ A (RL_QE (LIST OP (CADR F) (CDR NF)) NTHEO))
         (COND
          (((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
            (COND ((ATOM A) A) (T (CAR A))))
           (REDERR "cl_qeipo1: Could not eliminate quantifier")))
         (RETURN (CONS T A)))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (PROGN
         (SETQ ARGL (CDR F))
         (COND
          ((EQ OP 'AND)
           (PROG (SUBF)
             (SETQ SUBF ARGL)
            LAB
             (COND ((NULL SUBF) (RETURN NIL)))
             ((LAMBDA (SUBF)
                (COND ((CL_ATFP SUBF) (SETQ THEO (CONS SUBF THEO)))))
              (CAR SUBF))
             (SETQ SUBF (CDR SUBF))
             (GO LAB))))
         (COND
          ((EQ OP 'OR)
           (PROG (SUBF)
             (SETQ SUBF ARGL)
            LAB
             (COND ((NULL SUBF) (RETURN NIL)))
             ((LAMBDA (SUBF)
                (COND
                 ((CL_ATFP SUBF) (SETQ THEO (CONS (RL_NEGATEAT SUBF) THEO)))))
              (CAR SUBF))
             (SETQ SUBF (CDR SUBF))
             (GO LAB))))
         (PROG ()
          WHILELABEL
           (COND ((NOT ARGL) (RETURN NIL)))
           (PROGN
            (SETQ A (CL_QEIPO1 (CAR ARGL) THEO))
            (SETQ NF (CONS (CDR A) NF))
            (SETQ ARGL (CDR ARGL))
            (COND
             ((CAR A)
              (PROGN (SETQ NF (NCONC (REVERSIP NF) ARGL)) (SETQ ARGL NIL)))))
           (GO WHILELABEL))
         (RETURN
          (COND ((CAR A) (CONS T (CONS OP NF)))
                (T (CONS NIL (CONS OP (REVERSIP NF)))))))))
      (RETURN (CONS NIL F)))) 
(PUT 'CL_QEWS 'NUMBER-OF-ARGS 2) 
(PUT 'CL_QEWS 'DEFINED-ON-LINE '927) 
(PUT 'CL_QEWS 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_QEWS (F THEO)
    (PROG (Q OP QL VARL VARLL *RLQEANS)
      (COND (*RLQEPNF (SETQ F (RL_PNF F))))
      (SETQ F (RL_SIMPL F THEO (MINUS 1)))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN F)))
      (PROG (G163 G164)
        (SETQ G163 (CL_SPLIT F))
        (SETQ G164 G163)
        (SETQ QL (CAR G163))
        (SETQ G163 (CDR G163))
        (SETQ VARLL (CAR G163))
        (SETQ G163 (CDR G163))
        (SETQ F (CAR G163))
        (SETQ G163 (CDR G163))
        (RETURN G164))
      (PROG ()
       WHILELABEL
        (COND ((NOT QL) (RETURN NIL)))
        (PROGN
         (SETQ Q (PROG1 (CAR QL) (SETQ QL (CDR QL))))
         (SETQ VARL (PROG1 (CAR VARLL) (SETQ VARLL (CDR VARLL))))
         (SETQ F
                 (COND ((EQ Q 'EX) (CL_QEWS1 VARL F THEO))
                       (T (RL_NNFNOT (CL_QEWS1 VARL (RL_NNFNOT F) THEO))))))
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'CL_QEWS1 'NUMBER-OF-ARGS 3) 
(PUT 'CL_QEWS1 'DEFINED-ON-LINE '951) 
(PUT 'CL_QEWS1 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_QEWS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_QEWS1 (VARL MTX THEO)
    (PROG (V W)
      (PROG ()
       WHILELABEL
        (COND ((NOT VARL) (RETURN NIL)))
        (PROGN
         (SETQ W (RL_TRYGAUSS MTX VARL THEO NIL NIL))
         (COND
          ((EQ W 'FAILED)
           (PROGN
            (SETQ V (RL_VARSEL MTX VARL THEO))
            (SETQ MTX (CL_QEIPO (LIST 'EX V MTX) THEO))))
          (T
           (PROGN (SETQ V (CAAR W)) (SETQ MTX (RL_QE (LIST 'EX V MTX) THEO)))))
         (SETQ VARL (DELETE V VARL)))
        (GO WHILELABEL))
      (RETURN MTX))) 
(PUT 'CL_TRYGAUSS 'NUMBER-OF-ARGS 5) 
(PUT 'CL_TRYGAUSS 'DEFINED-ON-LINE '978) 
(PUT 'CL_TRYGAUSS 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_TRYGAUSS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_TRYGAUSS (F VL THEO ANS BVL)
    (PROG (W)
      (SETQ W (CL_TRYGAUSS1 F VL THEO ANS BVL))
      (COND ((EQ W 'FAILED) (RETURN 'FAILED)))
      (RETURN (CONS (CAR W) (UNION (CDR W) THEO))))) 
(SWITCH (LIST 'RLGAUSSDEBUG)) 
(PUT 'CL_TRYGAUSS1 'NUMBER-OF-ARGS 5) 
(PUT 'CL_TRYGAUSS1 'DEFINED-ON-LINE '992) 
(PUT 'CL_TRYGAUSS1 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_TRYGAUSS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_TRYGAUSS1 (F VL THEO ANS BVL)
    (PROG (W V CSOL EV)
      (SETQ CSOL '(FAILED))
      (COND ((NULL *RLQEVARSEL) (SETQ VL (LIST (CAR VL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT VL) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (SETQ W (CL_TRYGAUSSVAR F V THEO ANS BVL))
         (COND
          ((AND (NEQ (CAR W) 'GIGNORE) (RL_BETTERGAUSSP W CSOL))
           (PROGN
            (SETQ CSOL W)
            (SETQ EV V)
            (COND ((RL_BESTGAUSSP CSOL) (SETQ VL NIL)))))))
        (GO WHILELABEL))
      (COND ((EQ (CAR CSOL) 'FAILED) (RETURN 'FAILED)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 (CAAR CSOL))))
      (COND
       (*RLGAUSSDEBUG
        (IOTO_TPRIN2T
         (LIST "DEBUG: cl_trygauss1 eliminates " EV " with verbose output "
               (CAAR CSOL)))))
      (RETURN (CONS (CONS EV (CADR CSOL)) (CDDR CSOL))))) 
(PUT 'CL_TRYGAUSSVAR 'NUMBER-OF-ARGS 5) 
(PUT 'CL_TRYGAUSSVAR 'DEFINED-ON-LINE '1021) 
(PUT 'CL_TRYGAUSSVAR 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_TRYGAUSSVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_TRYGAUSSVAR (F V THEO ANS BVL)
    (PROGN
     (COND ((CL_ATFP F) (RL_QEFSOLSET F V THEO ANS BVL))
           ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
            (CL_GAUSSAND (CDR F) V THEO ANS BVL))
           ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
            (CL_GAUSSOR (CDR F) V THEO ANS BVL))
           (T '(FAILED))))) 
(SWITCH (LIST 'RLGAUSSTHEO)) 
(ON1 'RLGAUSSTHEO) 
(PUT 'CL_GAUSSAND 'NUMBER-OF-ARGS 5) 
(PUT 'CL_GAUSSAND 'DEFINED-ON-LINE '1039) 
(PUT 'CL_GAUSSAND 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_GAUSSAND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_GAUSSAND (FL V THEO ANS BVL)
    (PROG (W CURR)
      (COND
       (*RLGAUSSTHEO
        (PROG (SUBF)
          (SETQ SUBF FL)
         LAB
          (COND ((NULL SUBF) (RETURN NIL)))
          ((LAMBDA (SUBF)
             (COND
              ((AND (CL_ATFP SUBF) (NOT (MEMQ V (RL_VARLAT SUBF))))
               (SETQ THEO (LTO_INSERT SUBF THEO)))))
           (CAR SUBF))
          (SETQ SUBF (CDR SUBF))
          (GO LAB))))
      (SETQ CURR (CL_TRYGAUSSVAR (CAR FL) V THEO ANS BVL))
      (SETQ FL (CDR FL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FL (NOT (RL_BESTGAUSSP CURR)))) (RETURN NIL)))
        (PROGN
         (SETQ W (CL_TRYGAUSSVAR (CAR FL) V THEO ANS BVL))
         (SETQ CURR (CL_GAUSSINTERSECTION W CURR))
         (SETQ FL (CDR FL)))
        (GO WHILELABEL))
      (RETURN CURR))) 
(PUT 'CL_GAUSSOR 'NUMBER-OF-ARGS 5) 
(PUT 'CL_GAUSSOR 'DEFINED-ON-LINE '1055) 
(PUT 'CL_GAUSSOR 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_GAUSSOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_GAUSSOR (FL V THEO ANS BVL)
    (PROG (W CURR)
      (COND
       (*RLGAUSSTHEO
        (PROG (SUBF)
          (SETQ SUBF FL)
         LAB
          (COND ((NULL SUBF) (RETURN NIL)))
          ((LAMBDA (SUBF)
             (COND
              ((AND (CL_ATFP SUBF) (NOT (MEMQ V (RL_VARLAT SUBF))))
               (SETQ THEO (LTO_INSERT (RL_NEGATEAT SUBF) THEO)))))
           (CAR SUBF))
          (SETQ SUBF (CDR SUBF))
          (GO LAB))))
      (SETQ CURR (CL_TRYGAUSSVAR (CAR FL) V THEO ANS BVL))
      (SETQ FL (CDR FL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FL (NEQ (CAR CURR) 'FAILED))) (RETURN NIL)))
        (PROGN
         (SETQ W (CL_TRYGAUSSVAR (CAR FL) V THEO ANS BVL))
         (SETQ FL (CDR FL))
         (SETQ CURR (CL_GAUSSUNION CURR W)))
        (GO WHILELABEL))
      (RETURN CURR))) 
(PUT 'CL_GAUSSUNION 'NUMBER-OF-ARGS 2) 
(PUT 'CL_GAUSSUNION 'DEFINED-ON-LINE '1071) 
(PUT 'CL_GAUSSUNION 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_GAUSSUNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_GAUSSUNION (GRV1 GRV2)
    (PROG (TAG ESET THEO)
      (COND
       ((OR (EQ (CAR GRV1) 'FAILED) (EQ (CAR GRV2) 'FAILED))
        (RETURN '(FAILED))))
      (SETQ TAG
              (COND ((EQ (CAR GRV1) 'GIGNORE) (CAR GRV2))
                    ((EQ (CAR GRV2) 'GIGNORE) (CAR GRV1))
                    ((RL_BETTERGAUSSP GRV1 GRV2) (CAR GRV2)) (T (CAR GRV1))))
      (SETQ ESET (RL_ESETUNION (CADR GRV1) (CADR GRV2)))
      (SETQ THEO (UNION (CDDR GRV1) (CDDR GRV2)))
      (RETURN (CONS TAG (CONS ESET THEO))))) 
(PUT 'CL_GAUSSINTERSECTION 'NUMBER-OF-ARGS 2) 
(PUT 'CL_GAUSSINTERSECTION 'DEFINED-ON-LINE '1088) 
(PUT 'CL_GAUSSINTERSECTION 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_GAUSSINTERSECTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_GAUSSINTERSECTION (GRV1 GRV2)
    (COND
     ((AND (EQ (CAR GRV1) 'GIGNORE) (EQ (CAR GRV2) 'GIGNORE))
      (COND ((LESSP (LENGTH (CDDR GRV1)) (LENGTH (CDDR GRV2))) GRV1) (T GRV2)))
     ((EQ (CAR GRV1) 'GIGNORE) GRV2) ((EQ (CAR GRV2) 'GIGNORE) GRV1)
     ((RL_BETTERGAUSSP GRV1 GRV2) GRV1) (T GRV2))) 
(PUT 'CL_SPECELIM 'NUMBER-OF-ARGS 5) 
(PUT 'CL_SPECELIM 'DEFINED-ON-LINE '1095) 
(PUT 'CL_SPECELIM 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_SPECELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SPECELIM (F VL THEO ANS BVL) 'FAILED) 
(PUT 'CL_FBQE 'NUMBER-OF-ARGS 1) 
(PUT 'CL_FBQE 'DEFINED-ON-LINE '1102) 
(PUT 'CL_FBQE 'DEFINED-IN-FILE 'REDLOG/CL/CLQE.RED) 
(PUT 'CL_FBQE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_FBQE (F)
    (PROGN (COND (*RLVERBOSE (IOTO_TPRIN2T "+++ no fallback QE specified"))) F)) 
(ENDMODULE) 