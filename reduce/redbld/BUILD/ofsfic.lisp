(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFIC)) 
(REVISION 'OFSFIC "$Id: ofsfic.red 6077 2021-10-03 16:10:30Z thomas-sturm $") 
(COPYRIGHT 'OFSFIC "(c) 2015 M. Jaroschek, 2017 M. Kosta, T. Sturm") 
(SWITCH (LIST 'RLQEINFCORE)) 
(PUT 'RLQEINFCORE 'SIMPFG
     '((T (RLQEINFCORE_SWITCH)) (NIL (RLQEINFCORE_SWITCH)))) 
(FLUID '(*RLQEINFCORE-OWAL)) 
(FLUID '(*RLQEINFCORE-DEFAL)) 
(FLUID '(SMT_UNSATCORE*)) 
(FLUID '(*RLQEICFALLBACK)) 
(SETQ *RLQEINFCORE-OWAL
        '((CE_MK . OFSFIC*CE_MK) (CO_PUSH2 . OFSFIC*CO_PUSH2)
          (CL_MK1EQR . OFSFIC*CL_MK1EQR) (CL_QEA . OFSFIC*CL_QEA)
          (CL_QE1 . OFSFIC*CL_QE1) (CL_QEBLOCK4 . OFSFIC*CL_QEBLOCK4)
          (CL_QEVAR . OFSFIC*CL_QEVAR)
          (CL_PROCESS-CANDVL . OFSFIC*CL_PROCESS-CANDVL)
          (CL_ESETSUBST . OFSFIC*CL_ESETSUBST) (CL_QEATAL1 . OFSFIC*CL_QEATAL1)
          (CL_SIMPL . OFSFIC*CL_SIMPL)
          (OFSF_EXPLOITKNOWL . OFSFIC*OFSF_EXPLOITKNOWL)
          (OFSF_QEMKANS . OFSFIC*OFSF_QEMKANS))) 
(PUT 'RLQEINFCORE_STOREDEFS 'NUMBER-OF-ARGS 0) 
(PUT 'RLQEINFCORE_STOREDEFS 'DEFINED-ON-LINE '80) 
(PUT 'RLQEINFCORE_STOREDEFS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'RLQEINFCORE_STOREDEFS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RLQEINFCORE_STOREDEFS NIL
    (PROG (PR)
      (SETQ PR *RLQEINFCORE-OWAL)
     LAB
      (COND ((NULL PR) (RETURN NIL)))
      ((LAMBDA (PR)
         (PROGN
          (PROG (W1)
            (SETQ W1 (CONS (CAR PR) (GETD (CAR PR))))
            (SETQ *RLQEINFCORE-DEFAL (CONS W1 *RLQEINFCORE-DEFAL))
            (RETURN W1))
          (PROG (W1)
            (SETQ W1 (CONS (CDR PR) (GETD (CDR PR))))
            (SETQ *RLQEINFCORE-DEFAL (CONS W1 *RLQEINFCORE-DEFAL))
            (RETURN W1))))
       (CAR PR))
      (SETQ PR (CDR PR))
      (GO LAB))) 
(PUT 'RLQEINFCORE_SWITCH 'NUMBER-OF-ARGS 0) 
(PUT 'RLQEINFCORE_SWITCH 'DEFINED-ON-LINE '117) 
(PUT 'RLQEINFCORE_SWITCH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'RLQEINFCORE_SWITCH 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RLQEINFCORE_SWITCH NIL
    (PROG (S TMP)
      (COND ((NULL *RLQEINFCORE-DEFAL) (RLQEINFCORE_STOREDEFS)))
      (PROG (PR)
        (SETQ PR *RLQEINFCORE-OWAL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ S (COND (*RLQEINFCORE (CAR PR)) (T (CDR PR))))
            (SETQ TMP (ATSOC S *RLQEINFCORE-DEFAL))
            (COND
             ((NULL TMP) (REDERR (LIST "Missing procedure definition " S))))
            (SETQ TMP (CDR TMP))
            (PUTD (CAR PR) (CAR TMP) (CDR TMP))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB)))) 
(FLUID '(*RLQEICSIMPL *ICINITSIMPL)) 
(FLUID '(RLQEICDATA*)) 
(PUT 'INFCOREDATA 'ASSERT_DYNTYPECHK 'VECTORP) 
(FLAG '(INFCOREDATA) 'ASSERT_DYNTYPE) 
(PUT 'IC_INIT 'NUMBER-OF-ARGS 1) 
(DE IC_INIT (F)
    (PROG (ICDATA FLIST K)
      (SETQ ICDATA (MKVECT 14))
      (SETQ FLIST (OFSFIC_FTOL F))
      (SETQ K (DIFFERENCE (LENGTH FLIST) 1))
      (PUTV ICDATA 1
            (LTO_LIST2VECTOR
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I 0)
               (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
               (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
              LOOPLABEL
               (SETQ I (PLUS2 I 1))
               (COND ((MINUSP (DIFFERENCE K I)) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS 0 NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      (PUTV ICDATA 4 0)
      (PUTV ICDATA 5 (LTO_LIST2VECTOR FLIST))
      (PUTV ICDATA 8 (MKVECT K))
      (PUTV ICDATA 12 (LTO_LIST2VECTOR FLIST))
      (RETURN ICDATA))) 
(PUT 'IC_INFCORE 'NUMBER-OF-ARGS 1) 
(DE IC_INFCORE (ICDATA) (GETV ICDATA 0)) 
(PUT 'IC_FALSEVECT 'NUMBER-OF-ARGS 1) 
(DE IC_FALSEVECT (ICDATA) (GETV ICDATA 1)) 
(PUT 'IC_EVTPLIST 'NUMBER-OF-ARGS 1) 
(DE IC_EVTPLIST (ICDATA) (GETV ICDATA 2)) 
(PUT 'IC_MAXFALSEEL 'NUMBER-OF-ARGS 1) 
(DE IC_MAXFALSEEL (ICDATA) (GETV ICDATA 3)) 
(PUT 'IC_COVERAGE 'NUMBER-OF-ARGS 1) 
(DE IC_COVERAGE (ICDATA) (GETV ICDATA 4)) 
(PUT 'IC_CURRENTFVECT 'NUMBER-OF-ARGS 1) 
(DE IC_CURRENTFVECT (ICDATA) (GETV ICDATA 5)) 
(PUT 'IC_GUARDLIST 'NUMBER-OF-ARGS 1) 
(DE IC_GUARDLIST (ICDATA) (GETV ICDATA 6)) 
(PUT 'IC_KNOWL 'NUMBER-OF-ARGS 1) 
(DE IC_KNOWL (ICDATA) (GETV ICDATA 7)) 
(PUT 'IC_ESSENTIALVECT 'NUMBER-OF-ARGS 1) 
(DE IC_ESSENTIALVECT (ICDATA) (GETV ICDATA 8)) 
(PUT 'IC_VARLIST 'NUMBER-OF-ARGS 1) 
(DE IC_VARLIST (ICDATA) (GETV ICDATA 9)) 
(PUT 'IC_CADCELLLIST 'NUMBER-OF-ARGS 1) 
(DE IC_CADCELLLIST (ICDATA) (GETV ICDATA 10)) 
(PUT 'IC_TAGLIST 'NUMBER-OF-ARGS 1) 
(DE IC_TAGLIST (ICDATA) (GETV ICDATA 11)) 
(PUT 'IC_ORIGINALFVECT 'NUMBER-OF-ARGS 1) 
(DE IC_ORIGINALFVECT (ICDATA) (GETV ICDATA 12)) 
(PUT 'IC_OFFSETLIST 'NUMBER-OF-ARGS 1) 
(DE IC_OFFSETLIST (ICDATA) (GETV ICDATA 13)) 
(PUT 'IC_SETINFCORE 'NUMBER-OF-ARGS 2) 
(DE IC_SETINFCORE (ICDATA L) (PUTV ICDATA 0 L)) 
(PUT 'IC_APPENDINFCORE 'NUMBER-OF-ARGS 2) 
(DE IC_APPENDINFCORE (ICDATA K) (PUTV ICDATA 0 (CONS K (GETV ICDATA 0)))) 
(PUT 'IC_APPENDEVTPLIST 'NUMBER-OF-ARGS 2) 
(DE IC_APPENDEVTPLIST (ICDATA VEC) (PUTV ICDATA 2 (CONS VEC (GETV ICDATA 2)))) 
(PUT 'IC_ADDCOVERAGE 'NUMBER-OF-ARGS 2) 
(DE IC_ADDCOVERAGE (ICDATA K) (PUTV ICDATA 4 (PLUS (GETV ICDATA 4) K))) 
(PUT 'IC_SETCURRENTFVECT 'NUMBER-OF-ARGS 2) 
(DE IC_SETCURRENTFVECT (ICDATA V) (PUTV ICDATA 5 V)) 
(PUT 'IC_CLEARGUARDLIST 'NUMBER-OF-ARGS 1) 
(DE IC_CLEARGUARDLIST (ICDATA) (PUTV ICDATA 6 NIL)) 
(PUT 'IC_APPENDGUARDLIST 'NUMBER-OF-ARGS 2) 
(DE IC_APPENDGUARDLIST (ICDATA EL) (PUTV ICDATA 6 (CONS EL (GETV ICDATA 6)))) 
(PUT 'IC_SETKNOWL 'NUMBER-OF-ARGS 2) 
(DE IC_SETKNOWL (ICDATA KNOWL) (PUTV ICDATA 7 KNOWL)) 
(PUT 'IC_SETVARLIST 'NUMBER-OF-ARGS 2) 
(DE IC_SETVARLIST (ICDATA L) (PUTV ICDATA 9 L)) 
(PUT 'IC_INSERTVARLIST 'NUMBER-OF-ARGS 2) 
(DE IC_INSERTVARLIST (ICDATA V)
    (PUTV ICDATA 9 (OFSFIC_INSERTQCAR V (GETV ICDATA 9)))) 
(PUT 'IC_RESETCADCELLLIST 'NUMBER-OF-ARGS 1) 
(DE IC_RESETCADCELLLIST (ICDATA) (PUTV ICDATA 10 NIL)) 
(PUT 'IC_APPENDCADCELLLIST 'NUMBER-OF-ARGS 2) 
(DE IC_APPENDCADCELLLIST (ICDATA CELL)
    (PUTV ICDATA 10 (CONS CELL (GETV ICDATA 10)))) 
(PUT 'IC_SETTAGLIST 'NUMBER-OF-ARGS 2) 
(DE IC_SETTAGLIST (ICDATA L) (PUTV ICDATA 11 L)) 
(PUT 'IC_APPENDTAGLIST 'NUMBER-OF-ARGS 2) 
(DE IC_APPENDTAGLIST (ICDATA P) (PUTV ICDATA 11 (CONS P (GETV ICDATA 11)))) 
(PUT 'IC_SETOFFSETLIST 'NUMBER-OF-ARGS 2) 
(DE IC_SETOFFSETLIST (ICDATA L) (PUTV ICDATA 13 L)) 
(PUT 'IC_ADDTOFALSEVECT 'NUMBER-OF-ARGS 3) 
(DE IC_ADDTOFALSEVECT (ICDATA K J)
    (PUTV (IC_FALSEVECT ICDATA) K (PLUS (GETV (IC_FALSEVECT ICDATA) K) J))) 
(PUT 'IC_TAGLISTREMFALSE 'NUMBER-OF-ARGS 1) 
(DE IC_TAGLISTREMFALSE (ICDATA)
    (PROG (L)
      (SETQ L (GETV ICDATA 11))
      (COND
       ((EQ (CAAR L) 'FALSE)
        (PROGN (PUTV ICDATA 11 (CDR L)) (RETURN (CDAR L)))))
      (RETURN NIL))) 
(PUT 'IC_COMPUTEINFCORE 'NUMBER-OF-ARGS 1) 
(DE IC_COMPUTEINFCORE (ICDATA)
    (PROG (ESSENTIAL L K CHOSEN)
      (SETQ K 0)
      (SETQ CHOSEN 0)
      (SETQ ESSENTIAL (IC_ESSENTIALVECT ICDATA))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV ESSENTIAL) I)) (RETURN NIL)))
        (COND
         ((GETV ESSENTIAL I)
          (PROGN
           (SETQ CHOSEN (GETV (IC_FALSEVECT ICDATA) I))
           (IC_ADDCOVERAGE ICDATA (MINUS CHOSEN))
           (IC_APPENDINFCORE ICDATA I)
           (IC_UPDATEEVTPLIST ICDATA I)
           (IC_UPDATEFALSEVECT ICDATA))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (IC_COVERAGE ICDATA) 0)) (RETURN NIL)))
        (PROGN
         (PROG (G503 G504)
           (SETQ G503 (IC_MAXFALSE ICDATA))
           (SETQ G504 G503)
           (SETQ CHOSEN (CAR G503))
           (SETQ G503 (CDR G503))
           (SETQ K (CAR G503))
           (SETQ G503 (CDR G503))
           (RETURN G504))
         (IC_ADDCOVERAGE ICDATA (MINUS CHOSEN))
         (IC_APPENDINFCORE ICDATA K)
         (IC_UPDATEEVTPLIST ICDATA K)
         (IC_UPDATEFALSEVECT ICDATA))
        (GO WHILELABEL))
      (SETQ L NIL)
      (COND
       ((IC_OFFSETLIST ICDATA)
        (PROGN
         (PROG (E)
           (SETQ E (IC_INFCORE ICDATA))
          LAB
           (COND ((NULL E) (RETURN NIL)))
           ((LAMBDA (E)
              (PROG (N)
                (SETQ N (CADR (ASSOC E (IC_OFFSETLIST ICDATA))))
               LAB
                (COND ((NULL N) (RETURN NIL)))
                ((LAMBDA (N) (SETQ L (LTO_INSERTQ N L))) (CAR N))
                (SETQ N (CDR N))
                (GO LAB)))
            (CAR E))
           (SETQ E (CDR E))
           (GO LAB))
         (IC_SETINFCORE ICDATA L)))))) 
(PUT 'IC_UPDATEEVTPLIST 'NUMBER-OF-ARGS 2) 
(DE IC_UPDATEEVTPLIST (ICDATA K)
    (PROG (EVTPLIST)
      (PROG (TP)
        (SETQ TP (IC_EVTPLIST RLQEICDATA*))
       LAB
        (COND ((NULL TP) (RETURN NIL)))
        ((LAMBDA (TP)
           (COND
            ((NOT (EQ (GETV TP K) 'FALSE))
             (SETQ EVTPLIST (CONS TP EVTPLIST)))))
         (CAR TP))
        (SETQ TP (CDR TP))
        (GO LAB))
      (PUTV ICDATA 2 EVTPLIST))) 
(PUT 'IC_UPDATEFALSEVECT 'NUMBER-OF-ARGS 1) 
(DE IC_UPDATEFALSEVECT (ICDATA)
    (PROG ()
      (PUTV ICDATA 1
            (LTO_LIST2VECTOR
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I 0)
               (COND
                ((MINUSP (DIFFERENCE (UPBV (IC_FALSEVECT ICDATA)) I))
                 (RETURN NIL)))
               (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
              LOOPLABEL
               (SETQ I (PLUS2 I 1))
               (COND
                ((MINUSP (DIFFERENCE (UPBV (IC_FALSEVECT ICDATA)) I))
                 (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS 0 NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      (PROG (TP)
        (SETQ TP (IC_EVTPLIST ICDATA))
       LAB
        (COND ((NULL TP) (RETURN NIL)))
        ((LAMBDA (TP)
           (PROG (I)
             (SETQ I 0)
            LAB
             (COND ((MINUSP (DIFFERENCE (UPBV TP) I)) (RETURN NIL)))
             (COND ((EQ (GETV TP I) 'FALSE) (IC_ADDTOFALSEVECT ICDATA I 1)))
             (SETQ I (PLUS2 I 1))
             (GO LAB)))
         (CAR TP))
        (SETQ TP (CDR TP))
        (GO LAB)))) 
(PUT 'IC_MAXFALSE 'NUMBER-OF-ARGS 1) 
(DE IC_MAXFALSE (ICDATA)
    (PROG (FALSEVECT CHOSEN K CURRENT)
      (SETQ CHOSEN 0)
      (SETQ K 0)
      (SETQ CURRENT 0)
      (SETQ FALSEVECT (IC_FALSEVECT ICDATA))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV FALSEVECT) I)) (RETURN NIL)))
        (PROGN
         (SETQ CURRENT (GETV FALSEVECT I))
         (COND
          ((GREATERP CURRENT CHOSEN)
           (PROGN (SETQ CHOSEN CURRENT) (SETQ K I)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST CHOSEN K)))) 
(DE OFSFIC*CE_MK (VL F V ETERM AN FVECT) (LIST 'CE VL F V ETERM AN FVECT)) 
(PUT 'OFSFIC*CE_MK 'NUMBER-OF-ARGS 6) 
(PUTC 'OFSFIC*CE_MK 'INLINE
      '(LAMBDA (VL F V ETERM AN FVECT) (LIST 'CE VL F V ETERM AN FVECT))) 
(DE CE_FVECT (X) (NTH (CDR X) 6)) 
(PUT 'CE_FVECT 'NUMBER-OF-ARGS 1) 
(PUTC 'CE_FVECT 'INLINE '(LAMBDA (X) (NTH (CDR X) 6))) 
(DE OFSFIC*CO_PUSH2 (CO CE) (CONS CE CO)) 
(PUT 'OFSFIC*CO_PUSH2 'NUMBER-OF-ARGS 2) 
(PUTC 'OFSFIC*CO_PUSH2 'INLINE '(LAMBDA (CO CE) (CONS CE CO))) 
(DE OFSFIC*CL_MK1EQR (F EQL) (LIST (LIST F EQL))) 
(PUT 'OFSFIC*CL_MK1EQR 'NUMBER-OF-ARGS 2) 
(PUTC 'OFSFIC*CL_MK1EQR 'INLINE '(LAMBDA (F EQL) (LIST (LIST F EQL)))) 
(PUT 'OFSFIC*CL_QEA 'NUMBER-OF-ARGS 2) 
(DE OFSFIC*CL_QEA (F THEO)
    (PROG (ER *RLSIPW *RLSIPO *RLQEANS IC *RLQEICFALLBACK)
      (SETQ *RLSIPW (SETQ *RLSIPO (SETQ *RLQEANS T)))
      (SETQ *RLQEGEN NIL)
      (SETQ *RLQESR T)
      (SETQ *RLATALTHEO NIL)
      (SETQ *RLQESTDANS T)
      (SETQ *RLCADTRIMTREE NIL)
      (SETQ *RLQEDYN NIL)
      (SETQ *RLCADANS NIL)
      (SETQ RLQEICDATA* (IC_INIT F))
      (SETQ ER (OFSFIC*CL_QE1 F NIL NIL))
      (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
      (COND
       ((OR (EQUAL ER (LIST NIL)) (EQ (CAADR ER) 'FALSE))
        (PROGN
         (SETQ IC
                 (COND
                  (*RLQEICFALLBACK
                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                     (SETQ I 0)
                     (COND
                      ((MINUSP
                        (DIFFERENCE (DIFFERENCE (LENGTH (OFSFIC_FTOL F)) 1) I))
                       (RETURN NIL)))
                     (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                    LOOPLABEL
                     (SETQ I (PLUS2 I 1))
                     (COND
                      ((MINUSP
                        (DIFFERENCE (DIFFERENCE (LENGTH (OFSFIC_FTOL F)) 1) I))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR (CONS I NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                  (T
                   (PROGN
                    (IC_COMPUTEINFCORE RLQEICDATA*)
                    (IC_INFCORE RLQEICDATA*)))))
         (SETQ SMT_UNSATCORE* IC)
         (SETQ RLQEICDATA* (IC_INIT F))
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_TPRIN2T (LIST "+++ infcore: " IC))
            (IOTO_TPRIN2T (LIST "+++ infcore length: " (LENGTH IC)))
            (IOTO_TPRIN2T
             (LIST "+++ input length: "
                   (PLUS (UPBV (IC_CURRENTFVECT RLQEICDATA*)) 1))))))
         (SETQ ER (CONS NIL '((FALSE))))))
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2 "+++ model: ")
         (PROG (RPR)
           (SETQ RPR (CDAR (CDR ER)))
          LAB
           (COND ((NULL RPR) (RETURN NIL)))
           (PROGN
            (IOTO_PRIN2
             (LIST (CAAR RPR) " = " (IV_TOSTRING (ANU_IV (CDAR RPR)))))
            (COND ((CDR RPR) (IOTO_PRIN2 ", ")) (T (IOTO_PRIN2T "}"))))
           (SETQ RPR (CDR RPR))
           (GO LAB)))))
      (RETURN (CDR ER)))) 
(PUT 'OFSFIC*CL_QE1 'NUMBER-OF-ARGS 3) 
(DE OFSFIC*CL_QE1 (F THEO XBVL)
    (PROG (Q QL VARLL BVL SVF RESULT W WW K RVL JL F2 OFFSET OFFSET2 N)
      (SETQ N 0)
      (COND
       ((OR (NOT *RLQEANS) *RLQEGEN *RLQELOCAL)
        (REDERR (LIST "turn off rlqeinfcore for this"))))
      (COND (*RLQEPNF (SETQ F (RL_PNF F))))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN (CONS THEO (LIST (LIST F NIL))))))
      (SETQ F2 F)
      (SETQ *RLQEICSIMPL T)
      (SETQ *ICINITSIMPL T)
      (SETQ F (LIST F))
      (SETQ F2 (LIST (LIST)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NOT (OR (EQUAL (CAR F2) (CAR F)) (EQ (CAR F) 'FALSE))))
          (RETURN NIL)))
        (PROGN
         (SETQ F2 F)
         (SETQ F (OFSFIC*CL_SIMPL (CAR F2) THEO (MINUS 1)))
         (COND
          (OFFSET
           (PROGN
            (COND
             ((AND (NOT (EQUAL (CAR F2) (CAR F))) (NOT (EQUAL (CAR F) 'FALSE)))
              (PROGN
               (SETQ OFFSET2 (IC_OFFSETLIST RLQEICDATA*))
               (PROG (I)
                 (SETQ I OFFSET2)
                LAB
                 (COND ((NULL I) (RETURN NIL)))
                 ((LAMBDA (I)
                    (PROGN
                     (SETQ W (CADR I))
                     (SETQ WW NIL)
                     (PROG (J)
                       (SETQ J W)
                      LAB
                       (COND ((NULL J) (RETURN NIL)))
                       ((LAMBDA (J)
                          (PROG (K)
                            (SETQ K (CADR (ASSOC J OFFSET)))
                           LAB
                            (COND ((NULL K) (RETURN NIL)))
                            ((LAMBDA (K) (SETQ WW (CONS K WW))) (CAR K))
                            (SETQ K (CDR K))
                            (GO LAB)))
                        (CAR J))
                       (SETQ J (CDR J))
                       (GO LAB))
                     (SETCAR (CDR I) WW)))
                  (CAR I))
                 (SETQ I (CDR I))
                 (GO LAB))
               (SETQ OFFSET OFFSET2))))))
          (T (SETQ OFFSET (IC_OFFSETLIST RLQEICDATA*))))
         (IC_SETOFFSETLIST RLQEICDATA* NIL))
        (GO WHILELABEL))
      (SETQ *RLQEICSIMPL NIL)
      (SETQ *ICINITSIMPL NIL)
      (COND
       ((AND (EQ (CAR F) 'FALSE) (CDR F))
        (PROGN
         (SETQ RLQEICDATA* (IC_INIT (CAR F2)))
         (OFSFIC_FILTERLOCALCORE (LTO_LIST2VECTOR (OFSFIC_FTOL (CAR F2)))
          (CADR F))
         (IC_SETOFFSETLIST RLQEICDATA* OFFSET)
         (SETQ F (CAR F))))
       (T
        (PROGN
         (SETQ RLQEICDATA* (IC_INIT (CAR F)))
         (IC_SETOFFSETLIST RLQEICDATA* OFFSET)
         (SETQ F (CAR F))
         (COND ((EQ F 'FALSE) (PUTV (IC_ESSENTIALVECT RLQEICDATA*) 0 T))))))
      (PROG (G505 G506)
        (SETQ G505 (CL_SPLIT F))
        (SETQ G506 G505)
        (SETQ QL (CAR G505))
        (SETQ G505 (CDR G505))
        (SETQ VARLL (CAR G505))
        (SETQ G505 (CDR G505))
        (SETQ F (CAR G505))
        (SETQ G505 (CDR G505))
        (SETQ BVL (CAR G505))
        (SETQ G505 (CDR G505))
        (RETURN G506))
      (PROG (G507 G508)
        (SETQ G507 (CL_QE1-ITERATE QL VARLL F NIL BVL))
        (SETQ G508 G507)
        (SETQ QL (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ VARLL (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ Q (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ RVL (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ JL (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ THEO (CAR G507))
        (SETQ G507 (CDR G507))
        (SETQ SVF (CAR G507))
        (SETQ G507 (CDR G507))
        (RETURN G508))
      (SETQ JL (CL_QE1-REQUANTIFY QL VARLL Q RVL JL))
      (COND
       ((NULL QL)
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
                   (GO LOOPLABEL)))))
       (T
        (PROGN
         (SETQ F (CAR (CAR JL)))
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2
            (LIST "+++ Final simplification ... " (CL_ATNUM F) " -> "))))
         (SETQ F (RL_SIMPL F NIL (MINUS 1)))
         (COND (*RLVERBOSE (IOTO_PRIN2T (CL_ATNUM F))))
         (COND (RVL (REDERR "unexpected fallback - tell sturm@redlog.eu!")))
         (SETQ RESULT (LIST (LIST F NIL))))))
      (RETURN (CONS THEO RESULT)))) 
(PUT 'OFSFIC*CL_QEBLOCK4 'NUMBER-OF-ARGS 7) 
(DE OFSFIC*CL_QEBLOCK4 (F VARL THEO ANS BVL DPTH VLV)
    (PROG (W CO REMVL NEWJ CVL COE WW TMP C COUNT DELC OLDCOL COMAX COMAXN)
      (SETQ C 0)
      (SETQ COUNT 0)
      (SETQ DELC 0)
      (SETQ OLDCOL 0)
      (SETQ COMAX 0)
      (SETQ COMAXN 0)
      (SETQ CVL VARL)
      (SETQ CO (CO_NEW))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (REDERR "unexpected input disjunction - tell sturm@redlog.eu!")))
      (COND
       ((NOT ANS) (REDERR "unexpected ans = nil - tell sturm@redlog.eu!")))
      (SETQ CO
              (CO_SAVE CO
                       (LIST
                        (LIST 'CE CVL F NIL NIL NIL
                              (IC_CURRENTFVECT RLQEICDATA*)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR CO)) (RETURN NIL)))
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN
            (SETQ WW (CAR (CO_STAT CO)))
            (COND
             ((OR (EQUAL COMAX 0) (LESSP (CAR WW) COMAX)
                  (AND (EQUAL (CAR WW) COMAX) (LESSP (CDR WW) COMAXN)))
              (PROGN
               (SETQ COMAX (CAR WW))
               (SETQ COMAXN (CDR WW))
               (IOTO_PRIN2 (LIST "[" COMAX ":" COMAXN "] "))))))))
         (PROG (G509)
           (SETQ G509 (CO_GET CO))
           (SETQ COE (CAR G509))
           (SETQ CO (CDR G509))
           (RETURN G509))
         (IC_SETCURRENTFVECT RLQEICDATA* (NTH (CDR COE) 6))
         (SETQ CVL (CAR (CDR COE)))
         (SETQ COUNT (PLUS COUNT 1))
         (PROG (G510)
           (SETQ G510
                   (OFSFIC*CL_QEVAR (CADR (CDR COE)) (CAR (CDR COE))
                    (NTH (CDR COE) 5) THEO ANS BVL))
           (SETQ W (CAR G510))
           (SETQ THEO (CDR G510))
           (RETURN G510))
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
            (SETQ TMP (OFSFIC_FALLBACK-ON-COE COE))
            (COND
             (TMP
              (PROGN
               (SETQ CO (CO_NEW))
               (SETQ NEWJ
                       (LIST
                        (CONS 'TRUE (APPEND TMP (NTH (CDR COE) 5))))))))))))
        (GO WHILELABEL))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[DEL:" DELC "/" COUNT "]"))))
      (RETURN (LIST REMVL NEWJ THEO)))) 
(PUT 'OFSFIC_FALLBACK-ON-COE 'NUMBER-OF-ARGS 1) 
(DE OFSFIC_FALLBACK-ON-COE (COE)
    (PROG (FVECT CADINPUT CD CADRES RESAL RFVECT FALSEFOUND CELLEVAL SF VL VLSP
           TMPRES)
      (SETQ *RLQEICFALLBACK T)
      (SETQ FVECT (NTH (CDR COE) 6))
      (SETQ CADINPUT
              (RL_EX
               ((LAMBDA (G512)
                  (COND ((AND G512 (CDR G512)) (CONS 'AND G512))
                        ((NULL G512) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G512))))
                (VECTOR2LIST FVECT))
               NIL))
      (SETQ CD (OFSF_CADPREPARATION CADINPUT (OFSF_CADPORDER CADINPUT) NIL))
      (OFSF_CADPROJECTION CD)
      (OFSF_CADEXTENSION CD)
      (SETQ CADRES (ACELL_GETTV (ATREE_ROOTCELL (CADDATA_DD CD))))
      (COND (NIL NIL))
      (COND
       ((EQ CADRES 'TRUE)
        (PROGN
         (PROG (CELL)
           (SETQ CELL (ATREE_GETLEAVES (CADDATA_DD CD)))
          LAB
           (COND ((NULL CELL) (RETURN NIL)))
           ((LAMBDA (CELL)
              (COND
               ((EQ (ACELL_GETTV CELL) 'TRUE)
                (SETQ RESAL
                        (PROG (ANU FORALL-RESULT FORALL-ENDPTR)
                          (SETQ ANU (ACELL_GETSP CELL))
                          (COND ((NULL ANU) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (ANU)
                                              (CONS (AEX_MVAR (ANU_DP ANU))
                                                    ANU))
                                            (CAR ANU))
                                           NIL)))
                         LOOPLABEL
                          (SETQ ANU (CDR ANU))
                          (COND ((NULL ANU) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (ANU)
                                      (CONS (AEX_MVAR (ANU_DP ANU)) ANU))
                                    (CAR ANU))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))))
            (CAR CELL))
           (SETQ CELL (CDR CELL))
           (GO LAB))
         (OFSF_CADFINISH CD)
         (COND (NIL NIL))
         (RETURN
          (PROG (PR FORALL-RESULT FORALL-ENDPTR)
            (SETQ PR RESAL)
            (COND ((NULL PR) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (PR)
                                (LIST (CAR PR) 'CADANU (ANU_REORDER (CDR PR))
                                      NIL))
                              (CAR PR))
                             NIL)))
           LOOPLABEL
            (SETQ PR (CDR PR))
            (COND ((NULL PR) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (PR)
                        (LIST (CAR PR) 'CADANU (ANU_REORDER (CDR PR)) NIL))
                      (CAR PR))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (OFSF_CADFINISH CD)
      (RETURN NIL)
      (SETQ RFVECT (MKVECT (UPBV FVECT)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (UPBV FVECT) I)) (RETURN NIL)))
        (PUTV RFVECT I (OFSF_REORDER (GETV FVECT I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (CELL)
        (SETQ CELL (ATREE_GETLEAVES (CADDATA_DD CD)))
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (PROGN
            (SETQ FALSEFOUND NIL)
            (SETQ CELLEVAL (MKVECT (UPBV RFVECT)))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND ((MINUSP (DIFFERENCE (UPBV RFVECT) I)) (RETURN NIL)))
              (PROGN
               (SETQ SF (GETV RFVECT I))
               (SETQ VL (CL_FVARL SF))
               (SETQ VLSP
                       (PROG (ANU FORALL-RESULT FORALL-ENDPTR)
                         (SETQ ANU (ACELL_GETSP CELL))
                         (COND ((NULL ANU) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (ANU)
                                             (AEX_MVAR (ANU_DP ANU)))
                                           (CAR ANU))
                                          NIL)))
                        LOOPLABEL
                         (SETQ ANU (CDR ANU))
                         (COND ((NULL ANU) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ANU) (AEX_MVAR (ANU_DP ANU)))
                                   (CAR ANU))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ TMPRES 'TRUE)
               (COND
                ((LTO_SUBSETQ VL VLSP)
                 (PROGN
                  (PROG (V)
                    (SETQ V (LTO_SETMINUS VLSP VL))
                   LAB
                    (COND ((NULL V) (RETURN NIL)))
                    ((LAMBDA (V)
                       (SETQ SF
                               ((LAMBDA (G514)
                                  (COND
                                   ((AND G514 (CDR G514)) (CONS 'AND G514))
                                   ((NULL G514)
                                    (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                   (T (CAR G514))))
                                (LIST SF
                                      (LIST 'GEQ
                                            (ADDF (LIST (CONS (CONS V 1) 1)) 1)
                                            NIL)))))
                     (CAR V))
                    (SETQ V (CDR V))
                    (GO LAB))
                  (SETQ TMPRES (OFSF_TRIALEVAL SF (ACELL_GETSP CELL))))))
               (COND (NIL NIL))
               (PUTV CELLEVAL I TMPRES)
               (COND
                ((EQ TMPRES 'FALSE)
                 (PROGN
                  (SETQ FALSEFOUND T)
                  (IC_ADDTOFALSEVECT RLQEICDATA* I 1)))))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND
             (FALSEFOUND
              (PROGN
               (IC_ADDCOVERAGE RLQEICDATA* 1)
               (IC_APPENDEVTPLIST RLQEICDATA* CELLEVAL)))
             (T
              (PROGN
               (SETQ *RLQEICSIMPL T)
               (OFSFIC_FILTERLOCALCORE RFVECT
                (CADR
                 (RL_SIMPL
                  ((LAMBDA (G516)
                     (COND ((AND G516 (CDR G516)) (CONS 'AND G516))
                           ((NULL G516)
                            (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR G516))))
                   (VECTOR2LIST CELLEVAL))
                  NIL (MINUS 1))))
               (SETQ *RLQEICSIMPL NIL))))))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (OFSF_CADFINISH CD)
      (RETURN NIL))) 
(PUT 'OFSFIC*CL_QEVAR 'NUMBER-OF-ARGS 6) 
(DE OFSFIC*CL_QEVAR (F VL AN THEO ANS BVL)
    (PROG (W CANDVL STATUS LEN)
      (SETQ LEN 0)
      (SETQ CANDVL (CL_VARSEL F VL THEO))
      (PROG (G517)
        (SETQ G517 (OFSFIC*CL_PROCESS-CANDVL F VL AN THEO ANS BVL CANDVL))
        (SETQ STATUS (CAR G517))
        (SETQ W (CDR G517))
        (RETURN G517))
      (COND ((EQ STATUS 'NONOCC) (RETURN (CONS (CONS T W) THEO))))
      (COND ((EQ STATUS 'FAILED) (RETURN (CONS (CONS NIL W) THEO))))
      (COND ((EQ STATUS 'ELIM) (RETURN (CONS (CONS T (CAR W)) (CDR W)))))
      (REDERR (LIST "cl_qevar: bad status" STATUS)))) 
(PUT 'OFSFIC*CL_PROCESS-CANDVL 'NUMBER-OF-ARGS 7) 
(DE OFSFIC*CL_PROCESS-CANDVL (F VL AN THEO ANS BVL CANDVL)
    (PROG (W WW V ALP STATUS NEWFORM WW2 ELIMSET TPL)
      (PROG ()
       WHILELABEL
        (COND ((NOT CANDVL) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR CANDVL) (SETQ CANDVL (CDR CANDVL))))
         (SETQ ALP (CL_QEATAL F V THEO ANS))
         (COND
          ((EQUAL ALP '(NIL))
           (PROGN
            (SETQ W
                    (LIST
                     (LIST 'CE (LTO_DELQ V VL) F NIL NIL
                           (AND ANS (CL_UPDANS V 'ARBITRARY NIL NIL AN ANS))
                           (IC_CURRENTFVECT RLQEICDATA*))))
            (SETQ STATUS 'NONOCC)
            (SETQ CANDVL NIL)))
          ((EQUAL (CAR ALP) 'FAILED)
           (COND ((NULL W) (PROGN (SETQ W (CDR ALP)) (SETQ STATUS 'FAILED)))))
          (T
           (PROGN
            (SETQ ELIMSET (RL_ELIMSET V ALP))
            (PROG ()
             WHILELABEL
              (COND ((NOT ALP) (RETURN NIL)))
              (COND
               ((EQ (CAAR ALP) 'EQUAL1)
                (PROGN
                 (COND
                  ((NOT (MEMBER '(OFSF_QESUBI (PINF)) ELIMSET))
                   (SETQ ELIMSET (CONS '(OFSF_QESUBI (PINF)) ELIMSET))))
                 (COND
                  ((NOT (MEMBER '(OFSF_QESUBI (MINF)) ELIMSET))
                   (SETQ ELIMSET (CONS '(OFSF_QESUBI (MINF)) ELIMSET))))
                 (SETQ ALP NIL)))
               (T (SETQ ALP (CDR ALP))))
              (GO WHILELABEL))
            (SETQ WW
                    (CL_ESETVECTSUBST F (IC_CURRENTFVECT RLQEICDATA*) V ELIMSET
                     (LTO_DELQ V VL) AN ANS BVL))
            (IC_CLEARGUARDLIST RLQEICDATA*)
            (COND
             ((CAR WW)
              (PROGN
               (COND
                ((EQ (CADAAR WW) 'TRUE)
                 (PROGN
                  (SETQ CANDVL NIL)
                  (SETQ WW
                          (LIST
                           (LIST
                            (LIST 'CE 'BREAK 'TRUE NIL NIL (CAR (CDDAAR WW))
                                  NIL))))))
                (T
                 (PROGN
                  (SETQ VL (CADR WW))
                  (SETQ WW (CAR WW))
                  (PROG (TRIPLE)
                    (SETQ TRIPLE WW)
                   LAB
                    (COND ((NULL TRIPLE) (RETURN NIL)))
                    ((LAMBDA (TRIPLE)
                       (PROGN
                        (SETQ TPL (CAR TRIPLE))
                        (SETQ NEWFORM (CADR TRIPLE))
                        (SETQ AN (CADDR TRIPLE))
                        (IC_SETCURRENTFVECT RLQEICDATA* TPL)
                        (COND
                         ((EQ (COND ((ATOM NEWFORM) NEWFORM) (T (CAR NEWFORM)))
                              'OR)
                          (PROG (SUBF)
                            (SETQ SUBF (CDR NEWFORM))
                           LAB
                            (COND ((NULL SUBF) (RETURN NIL)))
                            ((LAMBDA (SUBF)
                               (SETQ WW2
                                       (CONS (LIST 'CE VL SUBF NIL NIL AN TPL)
                                             WW2)))
                             (CAR SUBF))
                            (SETQ SUBF (CDR SUBF))
                            (GO LAB)))
                         ((NEQ NEWFORM 'FALSE)
                          (SETQ WW2
                                  (CONS (LIST 'CE VL NEWFORM NIL NIL AN TPL)
                                        WW2))))))
                     (CAR TRIPLE))
                    (SETQ TRIPLE (CDR TRIPLE))
                    (GO LAB))
                  (SETQ WW (LIST WW2)))))))
             (T (SETQ WW (LIST NIL))))
            (COND ((AND (NULL W) *CLQENEW) (SETQ W '(NIL))))
            (COND
             ((RL_BETTERP WW W) (PROGN (SETQ W WW) (SETQ STATUS 'ELIM))))))))
        (GO WHILELABEL))
      (RETURN (CONS STATUS W)))) 
(PUT 'CL_ESETVECTSUBST 'NUMBER-OF-ARGS 8) 
(DE CL_ESETVECTSUBST (FORM FVECT V ESET VL AN ANS BVL)
    (PROG (A D U ELIMRES JUNCT W F U2 SF RESVECT RES FALSEFOUND)
      (PROG ()
       WHILELABEL
        (COND ((NOT ESET) (RETURN NIL)))
        (PROGN
         (PROG (G518)
           (SETQ G518 (PROG1 (CAR ESET) (SETQ ESET (CDR ESET))))
           (SETQ A (CAR G518))
           (SETQ D (CDR G518))
           (RETURN G518))
         (PROG ()
          WHILELABEL
           (COND ((NOT D) (RETURN NIL)))
           (PROGN
            (SETQ U (PROG1 (CAR D) (SETQ D (CDR D))))
            (COND
             ((CDR U)
              (PROGN
               (SETQ F (ASSOC U (IC_GUARDLIST RLQEICDATA*)))
               (COND (F (SETQ F (CADR F))))
               (SETQ U2 (CONS 'TRUE (CDR U)))))
             (T (SETQ U2 U)))
            (SETQ FALSEFOUND NIL)
            (SETQ RESVECT (MKVECT (UPBV FVECT)))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND ((MINUSP (DIFFERENCE (UPBV FVECT) I)) (RETURN NIL)))
              (PROGN
               (SETQ SF (GETV FVECT I))
               (COND
                ((AND F (OFSFIC_SUBFORMULAP SF F))
                 (PROGN
                  (SETQ W (APPLY A (CONS BVL (CONS NIL (CONS SF (CONS V U))))))
                  (SETQ F NIL)))
                (T
                 (SETQ W
                         (APPLY A
                                (CONS BVL (CONS NIL (CONS SF (CONS V U2))))))))
               (SETQ ELIMRES (RL_SIMPL (CDR W) NIL (MINUS 1)))
               (COND (*RLQEGSD (SETQ ELIMRES (RL_GSD ELIMRES NIL))))
               (COND
                ((EQ ELIMRES 'FALSE)
                 (PROGN
                  (COND
                   ((NOT FALSEFOUND)
                    (PROGN
                     (IC_ADDCOVERAGE RLQEICDATA* 1)
                     (SETQ FALSEFOUND T))))
                  (IC_ADDTOFALSEVECT RLQEICDATA* I 1))))
               (PUTV RESVECT I ELIMRES))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND (FALSEFOUND (IC_APPENDEVTPLIST RLQEICDATA* RESVECT)))
            (SETQ *RLQEICSIMPL T)
            (SETQ RES
                    (OFSFIC*CL_SIMPL
                     ((LAMBDA (G520)
                        (COND ((AND G520 (CDR G520)) (CONS 'AND G520))
                              ((NULL G520)
                               (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                              (T (CAR G520))))
                      (VECTOR2LIST RESVECT))
                     NIL (MINUS 1)))
            (SETQ *RLQEICSIMPL NIL)
            (COND
             ((EQ (CAR RES) 'FALSE)
              (PROGN
               (COND
                ((NOT FALSEFOUND) (OFSFIC_FILTERLOCALCORE RESVECT (CADR RES))))
               NIL))
             (T
              (PROGN
               (SETQ RES (CAR RES))
               (COND
                ((EQ RES 'TRUE)
                 (PROGN
                  (SETQ ESET (SETQ D NIL))
                  (SETQ AN (CL_UPDANS V A U FORM AN ANS))
                  (PROG (VV)
                    (SETQ VV VL)
                   LAB
                    (COND ((NULL VV) (RETURN NIL)))
                    ((LAMBDA (VV)
                       (SETQ AN (CL_UPDANS VV 'ARBITRARY NIL NIL AN ANS)))
                     (CAR VV))
                    (SETQ VV (CDR VV))
                    (GO LAB))
                  (SETQ JUNCT (LIST (LIST RESVECT 'TRUE AN)))
                  NIL))
                (T
                 (SETQ JUNCT
                         (CONS (LIST RESVECT RES (CL_UPDANS V A U FORM AN ANS))
                               JUNCT))))))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN (CONS JUNCT (LIST VL))))) 
(PUT 'OFSFIC*CL_ESETSUBST 'NUMBER-OF-ARGS 8) 
(DE OFSFIC*CL_ESETSUBST (F V ESET VL AN THEO ANS BVL)
    (PROG (A D U ELIMRES JUNCT W)
      (PROG ()
       WHILELABEL
        (COND ((NOT ESET) (RETURN NIL)))
        (PROGN
         (PROG (G521)
           (SETQ G521 (PROG1 (CAR ESET) (SETQ ESET (CDR ESET))))
           (SETQ A (CAR G521))
           (SETQ D (CDR G521))
           (RETURN G521))
         (PROG ()
          WHILELABEL
           (COND ((NOT D) (RETURN NIL)))
           (PROGN
            (SETQ U (PROG1 (CAR D) (SETQ D (CDR D))))
            (SETQ W (APPLY A (CONS BVL (CONS THEO (CONS F (CONS V U))))))
            (SETQ THEO (UNION THEO (CAR W)))
            (SETQ ELIMRES (RL_SIMPL (CDR W) THEO (MINUS 1)))
            (COND (*RLQEGSD (SETQ ELIMRES (RL_GSD ELIMRES THEO))))
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
               (SETQ JUNCT (LIST (LIST 'CE 'BREAK ELIMRES NIL NIL AN NIL)))
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
                                    (CL_UPDANS V A U F AN ANS) NIL)
                              JUNCT)))
                   (CAR SUBF))
                  (SETQ SUBF (CDR SUBF))
                  (GO LAB)))
               (T
                (SETQ JUNCT
                        (CONS
                         (LIST 'CE VL ELIMRES NIL NIL
                               (CL_UPDANS V A U F AN ANS) NIL)
                         JUNCT))))))
            NIL)
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN (CONS JUNCT THEO)))) 
(PUT 'OFSFIC*CL_QEATAL1 'NUMBER-OF-ARGS 5) 
(PUT 'OFSFIC*CL_QEATAL1 'DEFINED-ON-LINE '857) 
(PUT 'OFSFIC*CL_QEATAL1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC*CL_QEATAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSFIC*CL_QEATAL1 (F V THEO FLG ANS)
    (PROG (OP W WW)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (SETQ W
              (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (LIST (CONS NIL NIL)))
                    ((EQ OP 'NOT)
                     (LIST (OFSFIC*CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)))
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
                                            (OFSFIC*CL_QEATAL1 SUBF V THEO FLG
                                             ANS))
                                          (CAR SUBF))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SUBF (CDR SUBF))
                        (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (SUBF)
                                    (OFSFIC*CL_QEATAL1 SUBF V THEO FLG ANS))
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
                                            (OFSFIC*CL_QEATAL1 SUBF V THEO FLG
                                             ANS))
                                          (CAR SUBF))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SUBF (CDR SUBF))
                        (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (SUBF)
                                    (OFSFIC*CL_QEATAL1 SUBF V THEO FLG ANS))
                                  (CAR SUBF))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
                    ((EQ OP 'IMPL)
                     (LIST (OFSFIC*CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)
                           (OFSFIC*CL_QEATAL1 (CADDR F) V THEO FLG ANS)))
                    ((EQ OP 'REPL)
                     (LIST (OFSFIC*CL_QEATAL1 (CADR F) V THEO FLG ANS)
                           (OFSFIC*CL_QEATAL1 (CADDR F) V THEO (NOT FLG) ANS)))
                    ((EQ OP 'EQUIV)
                     (LIST (OFSFIC*CL_QEATAL1 (CADR F) V THEO (NOT FLG) ANS)
                           (OFSFIC*CL_QEATAL1 (CADDR F) V THEO FLG ANS)
                           (OFSFIC*CL_QEATAL1 (CADR F) V THEO FLG ANS)
                           (OFSFIC*CL_QEATAL1 (CADDR F) V THEO (NOT FLG) ANS)))
                    ((OR (EQ OP 'EX) (EQ OP 'ALL))
                     (REDERR "argument formula not prenex"))
                    (T
                     (PROGN
                      (SETQ WW (RL_TRANSLAT F V THEO FLG ANS))
                      (COND
                       ((NEQ (CAR WW) 'FAILED)
                        (PROGN
                         (PROG (A)
                           (SETQ A (CAR WW))
                          LAB
                           (COND ((NULL A) (RETURN NIL)))
                           ((LAMBDA (A)
                              (PROG (G)
                                (SETQ G (CDR A))
                               LAB
                                (COND ((NULL G) (RETURN NIL)))
                                ((LAMBDA (G)
                                   (IC_APPENDGUARDLIST RLQEICDATA*
                                    (CONS G (LIST F))))
                                 (CAR G))
                                (SETQ G (CDR G))
                                (GO LAB)))
                            (CAR A))
                           (SETQ A (CDR A))
                           (GO LAB))
                         NIL)))
                      (LIST WW)))))
      (COND ((SETQ WW (ATSOC 'FAILED W)) (RETURN WW)))
      (RETURN (CL_ALPUNION W)))) 
(PUT 'OFSFIC*CL_SIMPL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSFIC*CL_SIMPL 'DEFINED-ON-LINE '909) 
(PUT 'OFSFIC*CL_SIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC*CL_SIMPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSFIC*CL_SIMPL (F ATL N)
    (PROG (W)
      (SETQ ATL (CL_SIMPLIFYTHEORY ATL))
      (COND ((EQ ATL 'INCTHEO) (RETURN 'INCTHEO)))
      (SETQ W (RL_SMUPDKNOWL 'AND ATL NIL (PLUS N 1)))
      (COND ((EQ W 'FALSE) (RETURN 'INCTHEO)))
      (COND
       (*RLQEICSIMPL
        (PROGN
         (SETQ W (CL_SIMPL1-TAGGED F W N NIL))
         (IC_SETTAGLIST RLQEICDATA* NIL)))
       (T (SETQ W (CL_SIMPL1 F W N NIL))))
      (RETURN W))) 
(PUT 'CL_SIMPL1-TAGGED 'NUMBER-OF-ARGS 4) 
(PUT 'CL_SIMPL1-TAGGED 'DEFINED-ON-LINE '931) 
(PUT 'CL_SIMPL1-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'CL_SIMPL1-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SIMPL1-TAGGED (F KNOWL N SOP)
    (PROG (OP RESULT W NEWKNOWL)
      (COND ((EQN N 0) (RETURN (LIST F NIL))))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'TRUE) (EQ OP 'FALSE))
        (PROGN
         (COND ((EQN N (MINUS 1)) (RETURN (LIST F (CONS 0 NIL)))))
         (RETURN (LIST F NIL)))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (PROGN
         (SETQ W (CL_SMSIMPL-JUNCT-TAGGED OP (CDR F) KNOWL N))
         (RETURN
          (LIST
           (COND ((AND (CAR W) (CDR (CAR W))) (CONS OP (CAR W)))
                 ((NULL (CAR W)) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR (CAR W))))
           (CADR W))))))
      (COND
       ((EQ OP 'NOT)
        (PROGN
         (SETQ RESULT (CL_SIMPL1-TAGGED (CADR F) KNOWL N 'NOT))
         (COND
          ((OR (EQ (CAR RESULT) 'TRUE) (EQ (CAR RESULT) 'FALSE))
           (RETURN (LIST (CL_FLIP (CAR RESULT)) (CADR RESULT)))))
         (COND
          ((CL_ATFP (CAR RESULT))
           (RETURN (LIST (RL_NEGATEAT (CAR RESULT)) (CADR RESULT)))))
         (RETURN (LIST (CL_NEGATE-INVOL (CAR RESULT)) (CADR RESULT))))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (SETQ KNOWL (RL_SMRMKNOWL KNOWL (CADR F)))
         (SETQ RESULT (CL_SIMPL1-TAGGED (CADDR F) KNOWL N OP))
         (COND
          ((OR (EQ (CAR RESULT) 'TRUE) (EQ (CAR RESULT) 'FALSE))
           (RETURN RESULT)))
         (COND ((NOT (MEMQ (CADR F) (CL_FVARL (CAR RESULT)))) (RETURN RESULT)))
         (RETURN (LIST (LIST OP (CADR F) (CAR RESULT)) (CADR RESULT))))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (PROGN
         (SETQ KNOWL (RL_SMRMKNOWL KNOWL (CADR F)))
         (RETURN (CL_SIMPLBQ F KNOWL N)))))
      (SETQ W (CL_SIMPLAT F SOP))
      (SETQ OP (COND ((ATOM W) W) (T (CAR W))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (PROGN
         (SETQ W (CL_SMSIMPL-JUNCT-TAGGED OP (CDR W) KNOWL N))
         (RETURN
          (LIST
           (COND ((AND (CAR W) (CDR (CAR W))) (CONS OP (CAR W)))
                 ((NULL (CAR W)) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR (CAR W))))
           (CADR W))))))
      (COND
       ((OR (EQ OP 'TRUE) (EQ OP 'FALSE))
        (PROGN
         (COND ((EQN N (MINUS 1)) (RETURN (LIST W (LIST 0)))))
         (RETURN (LIST W (LIST (LIST 0) 'ARB N)))
         NIL)))
      (SETQ NEWKNOWL
              (OFSF_SMUPDKNOWL-TAGGED 'AND W (LIST (MINUS 1))
               (RL_SMCPKNOWL KNOWL) N))
      (COND
       ((EQ NEWKNOWL 'FALSE)
        (PROGN
         (COND
          ((EQN N (MINUS 1))
           (RETURN
            (LIST (LIST 'FALSE)
                  (OFSFIC_EXTRACTTAGS
                   (OFSFIC_PRUNETAG (IC_TAGLISTREMFALSE RLQEICDATA*)))))))
         (RETURN (LIST (LIST 'FALSE) (IC_TAGLISTREMFALSE RLQEICDATA*))))))
      (SETQ W (OFSF_SMMKATL-TAGGED 'AND KNOWL NEWKNOWL N))
      (RETURN
       (COND ((EQ (CAR W) 'FALSE) W)
             (T
              (LIST
               (COND ((AND (CAR W) (CDR (CAR W))) (CONS 'AND (CAR W)))
                     ((NULL (CAR W)) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR (CAR W))))
               (CADR W))))))) 
(PUT 'CL_SMSIMPL-JUNCT-TAGGED 'NUMBER-OF-ARGS 4) 
(PUT 'CL_SMSIMPL-JUNCT-TAGGED 'DEFINED-ON-LINE '995) 
(PUT 'CL_SMSIMPL-JUNCT-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'CL_SMSIMPL-JUNCT-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_SMSIMPL-JUNCT-TAGGED (OP JUNCT KNOWL N)
    (PROG (BREAK W ATL COL NEWKNOWL K ATFLIST L L2 L3 REL AT)
      (SETQ NEWKNOWL (RL_SMCPKNOWL KNOWL))
      (SETQ BREAK (CL_CFLIP 'FALSE (EQ OP 'AND)))
      (SETQ K 0)
      (PROG (SUBF)
        (SETQ SUBF JUNCT)
       LAB
        (COND ((NULL SUBF) (RETURN NIL)))
        ((LAMBDA (SUBF)
           (PROGN
            (SETQ W (COND ((CL_ATFP SUBF) (CL_SIMPLAT SUBF OP)) (T SUBF)))
            (COND
             ((CL_ATFP W)
              (PROGN
               (SETQ ATFLIST (CONS W ATFLIST))
               (SETQ NEWKNOWL
                       (OFSF_SMUPDKNOWL-TAGGED OP W (LIST K) NEWKNOWL N))
               NIL))
             (T
              (SETQ COL
                      (CONS (LIST W (LIST (LIST (LIST K) (LIST 'ARB) N)))
                            COL))))
            (SETQ K (PLUS K 1))))
         (CAR SUBF))
        (SETQ SUBF (CDR SUBF))
        (GO LAB))
      (COND
       ((EQ NEWKNOWL 'FALSE)
        (PROGN
         (COND
          ((EQN N (MINUS 1))
           (RETURN
            (LIST (LIST BREAK)
                  (OFSFIC_EXTRACTTAGS
                   (OFSFIC_PRUNETAG (IC_TAGLISTREMFALSE RLQEICDATA*)))))))
         (RETURN (LIST (LIST BREAK) (IC_TAGLISTREMFALSE RLQEICDATA*))))))
      (SETQ W
              (CL_SMSIMPL-JUNCT1-TAGGED OP ATL (REVERSIP COL) KNOWL NEWKNOWL N
               BREAK))
      (COND
       ((AND (NEQ (CAAR W) 'FALSE) *ICINITSIMPL (EQN N (MINUS 1)))
        (PROGN
         (SETQ L2 NIL)
         (SETQ K 0)
         (PROG (SUBF)
           (SETQ SUBF (CAR W))
          LAB
           (COND ((NULL SUBF) (RETURN NIL)))
           ((LAMBDA (SUBF)
              (PROGN
               (SETQ REL NIL)
               (COND
                ((CL_ATFP SUBF)
                 (PROGN
                  (SETQ AT (OFSF_AT2IR SUBF N))
                  (SETQ L
                          (OFSFIC_INITTPS
                           (CDR (ASSOC (CAR AT) (IC_TAGLIST RLQEICDATA*)))
                           (CDADR AT)))
                  (COND
                   ((OR (EQ (CADADR AT) 'LEQ) (EQ (CADADR AT) 'LESSP))
                    (PROGN
                     (COND ((EQ (CAADR (CAR L)) 'LEQ) (SETQ REL (NTH L 4))))
                     (COND
                      (REL
                       (PROGN
                        (SETQ L3 (OFSFIC_TAGNUMBERS (CAAAR REL)))
                        (PROG (I)
                          (SETQ I (OFSFIC_TAGNUMBERS (CAR L)))
                         LAB
                          (COND ((NULL I) (RETURN NIL)))
                          ((LAMBDA (I) (SETQ L3 (CONS I L3))) (CAR I))
                          (SETQ I (CDR I))
                          (GO LAB))
                        (SETQ L L3)))
                      (T (SETQ L (OFSFIC_TAGNUMBERS (CAR L)))))))
                   ((EQ (CADADR AT) 'EQUAL)
                    (PROGN
                     (SETQ REL (CADDR L))
                     (COND
                      ((NULL REL)
                       (PROGN
                        (SETQ L3 (OFSFIC_TAGNUMBERS (CAR REL)))
                        (PROG (I)
                          (SETQ I (OFSFIC_TAGNUMBERS (CADR L)))
                         LAB
                          (COND ((NULL I) (RETURN NIL)))
                          ((LAMBDA (I) (SETQ L3 (CONS I L3))) (CAR I))
                          (SETQ I (CDR I))
                          (GO LAB))
                        (SETQ L L3)))
                      (T (SETQ L (OFSFIC_TAGNUMBERS (CADDR L)))))))
                   ((OR (EQ (CADADR AT) 'GEQ) (EQ (CADADR AT) 'GREATERP))
                    (PROGN
                     (COND ((EQ (CAADR (CADR L)) 'GEQ) (SETQ REL (NTH L 4))))
                     (COND
                      (REL
                       (PROGN
                        (SETQ L3 (OFSFIC_TAGNUMBERS (CAAR REL)))
                        (PROG (I)
                          (SETQ I (OFSFIC_TAGNUMBERS (CADR L)))
                         LAB
                          (COND ((NULL I) (RETURN NIL)))
                          ((LAMBDA (I) (SETQ L3 (CONS I L3))) (CAR I))
                          (SETQ I (CDR I))
                          (GO LAB))
                        (SETQ L L3)))
                      (T (SETQ L (OFSFIC_TAGNUMBERS (CADR L)))))))
                   (T
                    (PROGN
                     (SETQ L
                             (PROG (I FORALL-RESULT FORALL-ENDPTR)
                               (SETQ I (NTH L 4))
                               (COND ((NULL I) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (I) (CAR I)) (CAR I))
                                                NIL)))
                              LOOPLABEL
                               (SETQ I (CDR I))
                               (COND ((NULL I) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS ((LAMBDA (I) (CAR I)) (CAR I))
                                             NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                     (SETQ L
                             (PROG (I FORALL-RESULT FORALL-ENDPTR)
                               (SETQ I L)
                              STARTOVER
                               (COND ((NULL I) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (I) (OFSFIC_TAGNUMBERS L))
                                        (CAR I)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ I (CDR I))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL I) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (I) (OFSFIC_TAGNUMBERS L))
                                        (CAR I)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ I (CDR I))
                               (GO LOOPLABEL))))))))
                (T
                 (SETQ L
                         (OFSFIC_TAGNUMBERS
                          (CADR (ASSOC SUBF (IC_TAGLIST RLQEICDATA*)))))))
               (SETQ L2 (CONS (LIST K L) L2))
               (SETQ K (PLUS K 1))))
            (CAR SUBF))
           (SETQ SUBF (CDR SUBF))
           (GO LAB))
         (IC_SETOFFSETLIST RLQEICDATA* L2))))
      (COND
       ((EQN N (MINUS 1))
        (RETURN
         (LIST (CAR W) (OFSFIC_EXTRACTTAGS (OFSFIC_PRUNETAG (CADR W)))))))
      (RETURN W))) 
(PUT 'CL_SMSIMPL-JUNCT1-TAGGED 'NUMBER-OF-ARGS 7) 
(PUT 'CL_SMSIMPL-JUNCT1-TAGGED 'DEFINED-ON-LINE '1074) 
(PUT 'CL_SMSIMPL-JUNCT1-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'CL_SMSIMPL-JUNCT1-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE CL_SMSIMPL-JUNCT1-TAGGED (OP ATL COL KNOWL NEWKNOWL N BREAK)
    (PROG (A W WOP ARGL SICOL NATL TAG W2 TAGL TTAGL TAGCOPY)
      (PROG ()
       WHILELABEL
        (COND ((NOT COL) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR COL) (SETQ COL (CDR COL))))
         (SETQ TAG (CADR A))
         (SETQ A (CAR A))
         (SETQ TAGCOPY (COPY (IC_TAGLIST RLQEICDATA*)))
         (SETQ W2 (CL_SIMPL1-TAGGED A NEWKNOWL (DIFFERENCE N 1) OP))
         (SETQ W (CAR W2))
         (IC_SETTAGLIST RLQEICDATA* TAGCOPY)
         (SETQ TAGL (OFSFIC_TAGREPL (CADR W2) TAG (DIFFERENCE N 1)))
         (SETQ TAGL (OFSFIC_PRUNETAG TAGL))
         (SETQ WOP (COND ((ATOM W) W) (T (CAR W))))
         (COND ((EQ WOP BREAK) (PROGN (SETQ A BREAK) (SETQ COL NIL)))
               ((EQ WOP OP)
                (PROGN
                 (SETQ ARGL (CDR W))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND ARGL (CL_ATFP (CAR ARGL)))) (RETURN NIL)))
                   (PROGN
                    (SETQ NATL (CONS (LIST (CAR ARGL) TAGL) NATL))
                    (SETQ ARGL (CDR ARGL)))
                   (GO WHILELABEL))
                 (COND
                  (NATL
                   (PROGN
                    (SETQ COL (NCONC (REVERSIP SICOL) COL))
                    (SETQ SICOL NIL))))
                 (COND
                  (ARGL
                   (SETQ SICOL
                           (NCONC SICOL
                                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ X (REVERSE ARGL))
                                    (COND ((NULL X) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X)
                                                        (LIST X TAGL))
                                                      (CAR X))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ X (CDR X))
                                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (X) (LIST X TAGL))
                                              (CAR X))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))))
                 NIL))
               ((OR (OR (EQ WOP 'TRUE) (EQ WOP 'FALSE))
                    (OR (OR (OR (EQ WOP 'OR) (EQ WOP 'AND)) (EQ WOP 'NOT))
                        (OR (EQ WOP 'IMPL) (EQ WOP 'REPL) (EQ WOP 'EQUIV)))
                    (OR (EQ WOP 'EX) (EQ WOP 'ALL))
                    (OR (EQ WOP 'BEX) (EQ WOP 'BALL)))
                (PROGN
                 (COND
                  ((NEQ WOP (CL_FLIP BREAK))
                   (SETQ SICOL (CONS (LIST W TAGL) SICOL)))
                  (T (SETQ TTAGL (APPEND TAGL TTAGL))))))
               (T
                (PROGN
                 (SETQ COL (NCONC (REVERSIP SICOL) COL))
                 (SETQ SICOL NIL)
                 (SETQ NATL (LIST (LIST W TAGL))))))
         (COND
          (NATL
           (PROGN
            (PROG (SF)
              (SETQ SF NATL)
             LAB
              (COND ((NULL SF) (RETURN NIL)))
              ((LAMBDA (SF)
                 (SETQ NEWKNOWL
                         (OFSF_SMUPDKNOWL-TAGGED OP (CAR SF) (CADR SF) NEWKNOWL
                          N)))
               (CAR SF))
              (SETQ SF (CDR SF))
              (GO LAB))
            (COND
             ((EQ NEWKNOWL 'FALSE)
              (PROGN
               (SETQ TAGL (IC_TAGLISTREMFALSE RLQEICDATA*))
               (SETQ A BREAK)
               (SETQ COL NIL))))
            (SETQ NATL NIL)))))
        (GO WHILELABEL))
      (COND ((EQ A BREAK) (RETURN (LIST (LIST BREAK) TAGL))))
      (RETURN
       (CL_SMSIMPL-JUNCT2-TAGGED OP SICOL KNOWL NEWKNOWL N BREAK
        (APPEND TTAGL TAGL))))) 
(PUT 'CL_SMSIMPL-JUNCT2-TAGGED 'NUMBER-OF-ARGS 7) 
(PUT 'CL_SMSIMPL-JUNCT2-TAGGED 'DEFINED-ON-LINE '1134) 
(PUT 'CL_SMSIMPL-JUNCT2-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'CL_SMSIMPL-JUNCT2-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE CL_SMSIMPL-JUNCT2-TAGGED (OP SICOL KNOWL NEWKNOWL N BREAK TAGL)
    (PROG (ATL W)
      (SETQ ATL (OFSF_SMMKATL-TAGGED OP KNOWL NEWKNOWL N))
      (SETQ TAGL (APPEND (CADR ATL) TAGL))
      (SETQ ATL (CAR ATL))
      (COND ((EQ ATL BREAK) (RETURN (LIST (LIST BREAK) TAGL))))
      (SETQ W SICOL)
      (SETQ SICOL NIL)
      (PROG (X)
        (SETQ X W)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ SICOL (OFSFIC_CARINSERT X SICOL))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (*RLSISO
        (PROGN
         (SETQ ATL (SORT ATL 'RL_ORDATP))
         (SETQ SICOL
                 (SORT SICOL
                       (FUNCTION (LAMBDA (X Y) (CL_ORDP (CAR X) (CAR Y)))))))))
      (SETQ W
              (NCONC ATL
                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                       (SETQ I SICOL)
                       (COND ((NULL I) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (I) (CAR I)) (CAR I))
                                             NIL)))
                      LOOPLABEL
                       (SETQ I (CDR I))
                       (COND ((NULL I) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (I) (CAR I)) (CAR I)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
      (PROG (I)
        (SETQ I SICOL)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (COND
             ((AND *ICINITSIMPL (EQN N (MINUS 1)))
              (IC_APPENDTAGLIST RLQEICDATA* (CONS (CAR I) (LIST (CADR I))))))
            (SETQ TAGL (APPEND (CADR I) TAGL))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND (W (RETURN (LIST W TAGL))))
      (RETURN (LIST (LIST (CL_FLIP BREAK)) TAGL)))) 
(PUT 'OFSFIC_FTOL 'NUMBER-OF-ARGS 1) 
(DE OFSFIC_FTOL (F)
    (PROG (FLIST)
      (SETQ FLIST (CADDR (CL_SPLIT F)))
      (COND
       ((EQ (COND ((ATOM FLIST) FLIST) (T (CAR FLIST))) 'AND)
        (RETURN (CDR FLIST))))
      (RETURN (LIST FLIST)))) 
(PUT 'OFSFIC_CARINSERT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSFIC_CARINSERT 'DEFINED-ON-LINE '1172) 
(PUT 'OFSFIC_CARINSERT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_CARINSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSFIC_CARINSERT (X L) (COND ((ASSOC (CAR X) L) L) (T (CONS X L)))) 
(PUT 'OFSFIC_INSERTQCAR 'NUMBER-OF-ARGS 2) 
(PUT 'OFSFIC_INSERTQCAR 'DEFINED-ON-LINE '1175) 
(PUT 'OFSFIC_INSERTQCAR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_INSERTQCAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSFIC_INSERTQCAR (X L)
    (COND ((ASSOC (CAR X) L) L) (T (APPEND L (LIST X))))) 
(PUT 'OFSFIC_TAGFLAT 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_TAGFLAT 'DEFINED-ON-LINE '1178) 
(PUT 'OFSFIC_TAGFLAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_TAGFLAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_TAGFLAT (TAG)
    (COND
     ((LISTP (CAAR TAG))
      (LIST
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J (CAR TAG))
        STARTOVER
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (J)
                    (COND ((LISTP (CAR J)) (COPY (CAR J)))
                          (T (LIST (COPY (CAR J))))))
                  (CAR J)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ J (CDR J))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (J)
                    (COND ((LISTP (CAR J)) (COPY (CAR J)))
                          (T (LIST (COPY (CAR J))))))
                  (CAR J)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ J (CDR J))
         (GO LOOPLABEL))
       (CADR TAG) (CADDR TAG)))
     (T TAG))) 
(PUT 'OFSFIC_THREEPOINTSEARCH 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_THREEPOINTSEARCH 'DEFINED-ON-LINE '1188) 
(PUT 'OFSFIC_THREEPOINTSEARCH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_THREEPOINTSEARCH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_THREEPOINTSEARCH (V)
    (PROG (U L E N LB UB EB EBERROR NB VAL)
      (PROG (I)
        (SETQ I V)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ VAL
                    (COND
                     ((CADADR I)
                      (LIST
                       (COND ((LESSP (CDR (CDADR I)) 0) (CADADR I))
                             (T (MINUS (CADADR I))))
                       (COND
                        ((LESSP (CDR (CDADR I)) 0) (MINUS (CDR (CDADR I))))
                        (T (CDR (CDADR I))))))
                     (T (LIST 0 1))))
            (COND
             ((EQ (CAADR I) 'LEQ)
              (PROGN
               (COND ((NULL UB) (SETQ UB VAL)))
               (COND
                ((OR (NOT U)
                     (GREATERP (TIMES (CADR VAL) (CAR UB))
                               (TIMES (CADR UB) (CAR VAL))))
                 (PROGN (SETQ U I) (SETQ UB VAL)))))))
            (COND
             ((EQ (CAADR I) 'LESSP)
              (PROGN
               (COND ((NULL UB) (SETQ UB VAL)))
               (COND
                ((GEQ (TIMES (CADR VAL) (CAR UB)) (TIMES (CADR UB) (CAR VAL)))
                 (PROGN (SETQ U I) (SETQ UB VAL)))))))
            (COND
             ((EQ (CAADR I) 'GEQ)
              (PROGN
               (COND ((NULL LB) (SETQ LB VAL)))
               (COND
                ((OR (NULL L)
                     (LESSP (TIMES (CADR VAL) (CAR LB))
                            (TIMES (CADR LB) (CAR VAL))))
                 (PROGN (SETQ LB VAL) (SETQ L I)))))))
            (COND
             ((EQ (CAADR I) 'GREATERP)
              (PROGN
               (COND ((NULL LB) (SETQ LB VAL)))
               (COND
                ((LEQ (TIMES (CADR VAL) (CAR LB)) (TIMES (CADR LB) (CAR VAL)))
                 (PROGN (SETQ LB VAL) (SETQ L I)))))))
            (COND ((EQ (CAADR I) 'NEQ) (SETQ N (CONS (LIST I VAL) N))))
            (COND
             ((EQ (CAADR I) 'EQUAL)
              (PROGN
               (COND
                ((AND EB
                      (NOT
                       (EQUAL (TIMES (CADR VAL) (CAR EB))
                              (TIMES (CADR EB) (CAR VAL)))))
                 (SETQ EBERROR (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT I)))))
               (SETQ E I)
               (SETQ EB VAL))))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND (EBERROR (RETURN EBERROR)))
      (COND
       ((AND UB LB)
        (PROGN
         (COND
          ((LESSP (TIMES (CADR LB) (CAR UB)) (TIMES (CADR UB) (CAR LB)))
           (RETURN (LIST (OFSFIC_TAGFLAT L) (OFSFIC_TAGFLAT U)))))
         (COND
          ((AND (EQUAL (TIMES (CADR LB) (CAR UB)) (TIMES (CADR UB) (CAR LB)))
                (OR (EQ (CAADR L) 'GREATERP) (EQ (CAADR U) 'LESSP)))
           (RETURN (LIST (OFSFIC_TAGFLAT L) (OFSFIC_TAGFLAT U))))))))
      (COND
       (EB
        (PROGN
         (COND
          ((AND UB
                (GREATERP (TIMES (CADR UB) (CAR EB))
                          (TIMES (CADR EB) (CAR UB))))
           (RETURN (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT U)))))
         (COND
          ((AND UB
                (EQUAL (TIMES (CADR UB) (CAR EB)) (TIMES (CADR EB) (CAR UB)))
                (EQ (CAADR U) 'LESSP))
           (RETURN (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT U)))))
         (COND
          ((AND LB
                (EQUAL (TIMES (CADR LB) (CAR EB)) (TIMES (CADR EB) (CAR LB)))
                (EQ (CAADR L) 'GREATERP))
           (RETURN (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT L)))))
         (COND
          ((AND LB
                (LESSP (TIMES (CADR LB) (CAR EB)) (TIMES (CADR EB) (CAR LB))))
           (RETURN (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT L))))))))
      (COND
       (N
        (PROGN
         (PROG (P)
           (SETQ P N)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (PROGN
               (SETQ NB (CADR P))
               (COND
                ((AND LB UB
                      (EQUAL (TIMES (CADR LB) (CAR UB))
                             (TIMES (CADR UB) (CAR LB)))
                      (EQUAL (TIMES (CADR UB) (CAR NB))
                             (TIMES (CADR NB) (CAR UB))))
                 (SETQ EBERROR
                         (LIST (OFSFIC_TAGFLAT L) (OFSFIC_TAGFLAT (CAR P))
                               (OFSFIC_TAGFLAT U)))))
               (COND
                ((AND EB
                      (EQUAL (TIMES (CADR EB) (CAR NB))
                             (TIMES (CADR NB) (CAR EB))))
                 (SETQ EBERROR
                         (LIST (OFSFIC_TAGFLAT (CAR P))
                               (OFSFIC_TAGFLAT E)))))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (RETURN EBERROR))))
      (IOTO_TPRIN2T (LIST " "))
      (IOTO_TPRIN2T (LIST "v: " V))
      (IOTO_TPRIN2T (LIST "u: " U))
      (IOTO_TPRIN2T (LIST "ub: " UB))
      (IOTO_TPRIN2T (LIST "l: " L))
      (IOTO_TPRIN2T (LIST "lb: " LB))
      (IOTO_TPRIN2T (LIST "e: " E))
      (IOTO_TPRIN2T (LIST "n: " N))
      (REDERR "Error in ofsfic_threepointsearch"))) 
(PUT 'OFSFIC_INITTPS 'NUMBER-OF-ARGS 2) 
(PUT 'OFSFIC_INITTPS 'DEFINED-ON-LINE '1276) 
(PUT 'OFSFIC_INITTPS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_INITTPS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSFIC_INITTPS (V VALUE)
    (PROG (U L E N LB UB EB EBERROR VAL)
      (PROG (I)
        (SETQ I V)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ VAL
                    (COND
                     ((CADADR I)
                      (LIST
                       (COND ((LESSP (CDR (CDADR I)) 0) (CADADR I))
                             (T (MINUS (CADADR I))))
                       (COND
                        ((LESSP (CDR (CDADR I)) 0) (MINUS (CDR (CDADR I))))
                        (T (CDR (CDADR I))))))
                     (T (LIST 0 1))))
            (COND
             ((EQ (CAADR I) 'LEQ)
              (PROGN
               (COND ((NULL UB) (SETQ UB VAL)))
               (COND
                ((OR (NOT U)
                     (GREATERP (TIMES (CADR VAL) (CAR UB))
                               (TIMES (CADR UB) (CAR VAL))))
                 (PROGN (SETQ U I) (SETQ UB VAL)))))))
            (COND
             ((EQ (CAADR I) 'LESSP)
              (PROGN
               (COND ((NULL UB) (SETQ UB VAL)))
               (COND
                ((GEQ (TIMES (CADR VAL) (CAR UB)) (TIMES (CADR UB) (CAR VAL)))
                 (PROGN (SETQ U I) (SETQ UB VAL)))))))
            (COND
             ((EQ (CAADR I) 'GEQ)
              (PROGN
               (COND ((NULL LB) (SETQ LB VAL)))
               (COND
                ((OR (NULL L)
                     (LESSP (TIMES (CADR VAL) (CAR LB))
                            (TIMES (CADR LB) (CAR VAL))))
                 (PROGN (SETQ LB VAL) (SETQ L I)))))))
            (COND
             ((EQ (CAADR I) 'GREATERP)
              (PROGN
               (COND ((NULL LB) (SETQ LB VAL)))
               (COND
                ((LEQ (TIMES (CADR VAL) (CAR LB)) (TIMES (CADR LB) (CAR VAL)))
                 (PROGN (SETQ LB VAL) (SETQ L I)))))))
            (COND ((EQ (CAADR I) 'NEQ) (SETQ N (CONS (LIST I VAL) N))))
            (COND
             ((EQ (CAADR I) 'EQUAL)
              (PROGN
               (COND
                ((AND EB
                      (NOT
                       (EQUAL (TIMES (CADR VAL) (CAR EB))
                              (TIMES (CADR EB) (CAR VAL)))))
                 (SETQ EBERROR (LIST (OFSFIC_TAGFLAT E) (OFSFIC_TAGFLAT I)))))
               (COND
                ((NOT (AND E (EQUAL (CADR E) VALUE)))
                 (PROGN (SETQ E I) (SETQ EB VAL)))))))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN (LIST U L E N)))) 
(PUT 'OFSFIC_TAGNUMBERS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_TAGNUMBERS 'DEFINED-ON-LINE '1330) 
(PUT 'OFSFIC_TAGNUMBERS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_TAGNUMBERS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_TAGNUMBERS (TAG)
    (PROG ()
      (COND
       ((LISTP TAG)
        (PROGN
         (COND
          ((AND (CDR TAG) (NOT (ATOM (CADR TAG))))
           (PROGN
            (COND
             ((OR (EQ (CAADR TAG) 'ARB) (EQ (CAADR TAG) 'NEQ)
                  (EQ (CAADR TAG) 'LEQ) (EQ (CAADR TAG) 'LESSP)
                  (EQ (CAADR TAG) 'GEQ) (EQ (CAADR TAG) 'GREATERP)
                  (EQ (CAADR TAG) 'EQUAL))
              (RETURN (OFSFIC_TAGNUMBERS (CAR TAG)))))
            (RETURN
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I TAG)
              STARTOVER
               (COND ((NULL I) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (I) (OFSFIC_TAGNUMBERS I)) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ I (CDR I))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL I) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (I) (OFSFIC_TAGNUMBERS I)) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ I (CDR I))
               (GO LOOPLABEL))))))
         (RETURN
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I TAG)
           STARTOVER
            (COND ((NULL I) (RETURN NIL)))
            (SETQ FORALL-RESULT ((LAMBDA (I) (OFSFIC_TAGNUMBERS I)) (CAR I)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
            (SETQ I (CDR I))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND ((NULL I) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR ((LAMBDA (I) (OFSFIC_TAGNUMBERS I)) (CAR I)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ I (CDR I))
            (GO LOOPLABEL))))))
      (RETURN (LIST TAG)))) 
(PUT 'OFSFIC_TAGREPL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSFIC_TAGREPL 'DEFINED-ON-LINE '1345) 
(PUT 'OFSFIC_TAGREPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_TAGREPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSFIC_TAGREPL (TAGL TAGS N)
    (PROG (TAGL2 FOUND)
      (PROG (I)
        (SETQ I TAGL)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (COND
            ((LEQ (CADDR I) N)
             (PROGN
              (COND
               ((NULL FOUND)
                (PROGN
                 (SETQ FOUND T)
                 (PROG (J)
                   (SETQ J TAGS)
                  LAB
                   (COND ((NULL J) (RETURN NIL)))
                   ((LAMBDA (J) (SETQ TAGL2 (CONS J TAGL2))) (CAR J))
                   (SETQ J (CDR J))
                   (GO LAB)))))))
            (T (SETQ TAGL2 (CONS I TAGL2)))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND
       ((NULL FOUND)
        (PROG (J)
          (SETQ J TAGS)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J) (SETQ TAGL2 (CONS J TAGL2))) (CAR J))
          (SETQ J (CDR J))
          (GO LAB))))
      (RETURN TAGL2))) 
(PUT 'OFSFIC_EXTRACTTAGS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_EXTRACTTAGS 'DEFINED-ON-LINE '1364) 
(PUT 'OFSFIC_EXTRACTTAGS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_EXTRACTTAGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_EXTRACTTAGS (TAGL)
    (PROG (L)
      (PROG (I)
        (SETQ I TAGL)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROG (J)
             (SETQ J (CAR I))
            LAB
             (COND ((NULL J) (RETURN NIL)))
             ((LAMBDA (J) (SETQ L (LTO_INSERT J L))) (CAR J))
             (SETQ J (CDR J))
             (GO LAB)))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN L))) 
(PUT 'OFSFIC_PRUNETAG 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_PRUNETAG 'DEFINED-ON-LINE '1373) 
(PUT 'OFSFIC_PRUNETAG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_PRUNETAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_PRUNETAG (TAGL)
    (PROG (FOUND TAGL2)
      (PROG (I)
        (SETQ I TAGL)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ FOUND NIL)
            (PROG (J)
              (SETQ J TAGL2)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J) (COND ((EQUAL J I) (SETQ FOUND T)))) (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (COND ((NOT FOUND) (SETQ TAGL2 (CONS I TAGL2))))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN TAGL2))) 
(PUT 'OFSFIC_SUBFORMULAP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSFIC_SUBFORMULAP 'DEFINED-ON-LINE '1387) 
(PUT 'OFSFIC_SUBFORMULAP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_SUBFORMULAP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSFIC_SUBFORMULAP (SF F)
    (PROG (W)
      (COND
       ((NEQ SF 'TRUE)
        (PROGN
         (COND
          ((OR (EQ (CAR SF) 'OR) (EQ (CAR SF) 'AND))
           (PROGN
            (PROG (SSF)
              (SETQ SSF (CDR SF))
             LAB
              (COND ((NULL SSF) (RETURN NIL)))
              ((LAMBDA (SSF)
                 (COND ((NOT W) (SETQ W (OFSFIC_SUBFORMULAP SSF F)))))
               (CAR SSF))
              (SETQ SSF (CDR SSF))
              (GO LAB))
            (RETURN W))))
         (SETQ W (CAR (OFSF_AT2IR SF (MINUS 1))))
         (RETURN (EQUAL F SF)))))
      (RETURN NIL))) 
(PUT 'OFSFIC_FILTERLOCALCORE 'NUMBER-OF-ARGS 2) 
(DE OFSFIC_FILTERLOCALCORE (FVECT CORE)
    (PROG (W EVECT L)
      (SETQ L 0)
      (SETQ EVECT (IC_ESSENTIALVECT RLQEICDATA*))
      (SETQ W
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE (UPBV EVECT) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (COND ((GETV EVECT I) (GETV FVECT I))
                                       (T 'TRUE))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (UPBV EVECT) I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (COND ((GETV EVECT I) (GETV FVECT I)) (T 'TRUE))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (OFSFIC*CL_SIMPL
               (COND ((AND W (CDR W)) (CONS 'AND W))
                     ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR W)))
               NIL (MINUS 1)))
      (COND ((EQ W 'FALSE) (RETURN NIL)))
      (SETQ L (PLUS (LENGTH CORE) 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP (LENGTH CORE) L)) (RETURN NIL)))
        (PROGN
         (SETQ W
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 0)
                   (COND ((MINUSP (DIFFERENCE (UPBV FVECT) I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (COND ((MEMQ I CORE) (GETV FVECT I))
                                          (T 'TRUE))
                                    NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND
                    ((MINUSP (DIFFERENCE (UPBV FVECT) I))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (COND ((MEMQ I CORE) (GETV FVECT I)) (T 'TRUE))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ *RLQEICSIMPL T)
         (SETQ W
                 (OFSFIC*CL_SIMPL
                  (COND ((AND W (CDR W)) (CONS 'AND W))
                        ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR W)))
                  NIL (MINUS 1)))
         (SETQ *RLQEICSIMPL NIL)
         (SETQ L (LENGTH CORE))
         (SETQ CORE
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I (CADR W))
                  STARTOVER
                   (COND ((NULL I) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (I) (COND ((MEMQ I CORE) (LIST I))))
                            (CAR I)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ I (CDR I))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL I) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (I) (COND ((MEMQ I CORE) (LIST I))))
                            (CAR I)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ I (CDR I))
                   (GO LOOPLABEL))))
        (GO WHILELABEL))
      (PROG (I)
        (SETQ I CORE)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I) (PUTV EVECT I T)) (CAR I))
        (SETQ I (CDR I))
        (GO LAB)))) 
(PUT 'OFSF_SMUPDKNOWL-TAGGED 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_SMUPDKNOWL-TAGGED 'DEFINED-ON-LINE '1435) 
(PUT 'OFSF_SMUPDKNOWL-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SMUPDKNOWL-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMUPDKNOWL-TAGGED (OP A TAG KNOWL N)
    (PROG (W W2 IR TAGLIST)
      (COND ((EQ KNOWL 'FALSE) (RETURN KNOWL)))
      (SETQ A (COND ((EQ OP 'AND) A) (T (OFSF_NEGATEAT A))))
      (SETQ IR (OFSF_AT2IR A N))
      (COND
       ((SETQ W (ASSOC (CAR IR) KNOWL))
        (PROGN
         (SETQ W2
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J W)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) J) (CAR J)) NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) J) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETCDR W (OFSF_SMINSERT (CADR IR) (CDR W)))
         (COND
          ((NOT (AND (EQUAL (CDR W) (CDR W2)) (EQUAL N (CAADR W))))
           (PROGN
            (SETQ TAGLIST (IC_TAGLIST RLQEICDATA*))
            (SETQ W2 (ASSOC (CAR W) TAGLIST))
            (SETCDR W2 (CONS (LIST TAG (CDADR IR) N) (CDR W2))))))
         (COND
          ((EQ (CDR W) 'FALSE)
           (PROGN
            (SETQ KNOWL 'FALSE)
            (IC_APPENDTAGLIST RLQEICDATA*
             (CONS 'FALSE (OFSFIC_THREEPOINTSEARCH (CDR W2)))))))))
       (T
        (PROGN
         (IC_APPENDTAGLIST RLQEICDATA*
          (CONS (CAR IR) (CONS (LIST TAG (CDADR IR) N) NIL)))
         (SETQ KNOWL (CONS IR KNOWL)))))
      (RETURN KNOWL))) 
(PUT 'OFSF_SMMKATL-TAGGED 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_SMMKATL-TAGGED 'DEFINED-ON-LINE '1464) 
(PUT 'OFSF_SMMKATL-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SMMKATL-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMMKATL-TAGGED (OP OLDKNOWL NEWKNOWL N)
    (COND
     (*RLSIPPATL
      (OFSF_SIPPATL-TAGGED OP (OFSF_SMMKATL1-TAGGED OP OLDKNOWL NEWKNOWL N)
       NEWKNOWL N))
     (T (OFSF_SMMKATL1-TAGGED OP OLDKNOWL NEWKNOWL N)))) 
(PUT 'OFSF_SIPPATL-TAGGED 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_SIPPATL-TAGGED 'DEFINED-ON-LINE '1470) 
(PUT 'OFSF_SIPPATL-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SIPPATL-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIPPATL-TAGGED (OP ATL NEWKNOWL N)
    (PROG (GTRUE GFALSE GEQUAL SUBAL ZVL POSVL NEGVL GEQVL LEQVL NEQVL AT NATL
           ATL2 AT2 IR VL M TAGL W LOCTAG ASC NEWT FOUND TMP)
      (SETQ GTRUE (CL_CFLIP 'TRUE (EQ OP 'AND)))
      (SETQ GFALSE (CL_CFLIP 'FALSE (EQ OP 'AND)))
      (SETQ GEQUAL (OFSF_CLNEGREL 'EQUAL (EQ OP 'AND)))
      (PROG (G522 G523)
        (SETQ G522 (OFSFIC*OFSF_EXPLOITKNOWL NEWKNOWL))
        (SETQ G523 G522)
        (SETQ SUBAL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ ZVL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ POSVL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ NEGVL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ GEQVL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ LEQVL (CAR G522))
        (SETQ G522 (CDR G522))
        (SETQ NEQVL (CAR G522))
        (SETQ G522 (CDR G522))
        (RETURN G523))
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ LOCTAG NIL)
         (SETQ ATL2 (PROG1 (CAR ATL) (SETQ ATL (CDR ATL))))
         (COND
          ((EQ (CADDR (CAADR ATL2)) N)
           (PROG (I)
             (SETQ I (CADR ATL2))
            LAB
             (COND ((NULL I) (RETURN NIL)))
             ((LAMBDA (I)
                (COND
                 ((LISTP (CAAR I))
                  (PROG (J)
                    (SETQ J (CAR I))
                   LAB
                    (COND ((NULL J) (RETURN NIL)))
                    ((LAMBDA (J)
                       (PROGN
                        (SETQ LOCTAG (LTO_INSERTQ J LOCTAG))
                        (SETQ TAGL (CONS J TAGL))))
                     (CAR J))
                    (SETQ J (CDR J))
                    (GO LAB)))
                 (T
                  (PROGN
                   (SETQ LOCTAG (LTO_INSERTQ I LOCTAG))
                   (SETQ TAGL (CONS I TAGL))))))
              (CAR I))
             (SETQ I (CDR I))
             (GO LAB))))
         (PROG (AT)
           (SETQ AT (CAR ATL2))
          LAB
           (COND ((NULL AT) (RETURN NIL)))
           ((LAMBDA (AT)
              (PROGN
               (COND
                ((NOT (NULL AT))
                 (PROGN
                  (SETQ AT2 (COPY AT))
                  (COND
                   ((AND *RLSIPPSUBST (NOT (OFSF_VAREQNP GEQUAL AT)))
                    (PROGN
                     (SETQ VL (RL_VARLAT AT))
                     (PROG (V)
                       (SETQ V
                               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ I SUBAL)
                                 (COND ((NULL I) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (I) (CAR I))
                                                   (CAR I))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ I (CDR I))
                                 (COND ((NULL I) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS ((LAMBDA (I) (CAR I)) (CAR I))
                                               NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                      LAB
                       (COND ((NULL V) (RETURN NIL)))
                       ((LAMBDA (V)
                          (PROGN
                           (COND
                            ((MEMBER V VL)
                             (PROGN
                              (SETQ M (ASSOC V (IC_VARLIST RLQEICDATA*)))
                              (SETQ W
                                      (CDR
                                       (ASSOC (CADR M)
                                              (IC_TAGLIST RLQEICDATA*))))
                              (PROG (I)
                                (SETQ I W)
                               LAB
                                (COND ((NULL I) (RETURN NIL)))
                                ((LAMBDA (I)
                                   (COND
                                    ((LISTP (CAAR I))
                                     (PROG (J)
                                       (SETQ J (CAR I))
                                      LAB
                                       (COND ((NULL J) (RETURN NIL)))
                                       ((LAMBDA (J)
                                          (PROGN
                                           (SETQ LOCTAG (LTO_INSERTQ J LOCTAG))
                                           (SETQ TAGL (CONS J TAGL))))
                                        (CAR J))
                                       (SETQ J (CDR J))
                                       (GO LAB)))
                                    (T
                                     (PROGN
                                      (SETQ LOCTAG (LTO_INSERTQ I LOCTAG))
                                      (SETQ TAGL (CONS I TAGL))))))
                                 (CAR I))
                                (SETQ I (CDR I))
                                (GO LAB)))))))
                        (CAR V))
                       (SETQ V (CDR V))
                       (GO LAB))
                     (SETQ AT (OFSF_SIPPSUBST AT SUBAL))
                     ((LAMBDA (*RLSIATADV) (SETQ AT (OFSF_SIMPLAT1 AT OP)))
                      NIL)
                     NIL)))
                  (COND
                   ((NOT (OR (EQ AT 'TRUE) (EQ AT 'FALSE)))
                    (COND
                     ((AND *RLSIPPSIGNCHK (NOT (SFTO_VARISNUMP (CADR AT))))
                      (SETQ AT
                              (OFSF_SIPPSIGNCHK AT ZVL POSVL NEGVL GEQVL LEQVL
                               NEQVL))))))
                  (COND
                   ((NOT (OR (EQ AT GFALSE) (EQUAL AT AT2)))
                    (PROGN
                     (SETQ VL (RL_VARLAT AT2))
                     (PROG (V)
                       (SETQ V VL)
                      LAB
                       (COND ((NULL V) (RETURN NIL)))
                       ((LAMBDA (V)
                          (PROGN
                           (SETQ M (ASSOC V (IC_VARLIST RLQEICDATA*)))
                           (COND
                            (M
                             (PROGN
                              (SETQ W
                                      (CDR
                                       (ASSOC (CADR M)
                                              (IC_TAGLIST RLQEICDATA*))))
                              (PROG (I)
                                (SETQ I W)
                               LAB
                                (COND ((NULL I) (RETURN NIL)))
                                ((LAMBDA (I)
                                   (COND
                                    ((LISTP (CAAR I))
                                     (PROG (J)
                                       (SETQ J (CAR I))
                                      LAB
                                       (COND ((NULL J) (RETURN NIL)))
                                       ((LAMBDA (J)
                                          (PROGN
                                           (SETQ LOCTAG (LTO_INSERTQ J LOCTAG))
                                           (SETQ TAGL (CONS J TAGL))
                                           NIL))
                                        (CAR J))
                                       (SETQ J (CDR J))
                                       (GO LAB)))
                                    (T
                                     (PROGN
                                      (SETQ LOCTAG (LTO_INSERTQ I LOCTAG))
                                      (SETQ TAGL (CONS I TAGL))))))
                                 (CAR I))
                                (SETQ I (CDR I))
                                (GO LAB)))))))
                        (CAR V))
                       (SETQ V (CDR V))
                       (GO LAB)))))
                  (COND
                   ((AND (EQN N (MINUS 1))
                         (NOT (OR (EQ AT GTRUE) (EQ AT GFALSE))))
                    (PROGN
                     (SETQ IR (OFSF_AT2IR AT N))
                     (SETQ ASC (ASSOC (CAR IR) (IC_TAGLIST RLQEICDATA*)))
                     (SETQ NEWT
                             (LIST
                              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                (SETQ I LOCTAG)
                               STARTOVER
                                (COND ((NULL I) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (I) (OFSFIC_TAGNUMBERS I))
                                         (CAR I)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ I (CDR I))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL I) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (I) (OFSFIC_TAGNUMBERS I))
                                         (CAR I)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ I (CDR I))
                                (GO LOOPLABEL))
                              (CDADR IR) (MINUS 1)))
                     (COND
                      (ASC
                       (PROGN
                        (SETQ FOUND NIL)
                        (SETQ TMP ASC)
                        (PROG ()
                         WHILELABEL
                          (COND ((NOT (AND TMP (NOT FOUND))) (RETURN NIL)))
                          (PROGN
                           (COND ((EQUAL (CAR TMP) NEWT) (SETQ FOUND T)))
                           (SETQ TMP (CDR TMP)))
                          (GO WHILELABEL))
                        (COND ((NOT FOUND) (SETCDR ASC (CONS NEWT (CDR ASC)))))
                        NIL))
                      (T
                       (IC_APPENDTAGLIST RLQEICDATA*
                        (CONS (CAR IR)
                              (LIST
                               (LIST
                                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ I LOCTAG)
                                 STARTOVER
                                  (COND ((NULL I) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          ((LAMBDA (I) (OFSFIC_TAGNUMBERS I))
                                           (CAR I)))
                                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                  (SETQ I (CDR I))
                                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                 LOOPLABEL
                                  (COND ((NULL I) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          ((LAMBDA (I) (OFSFIC_TAGNUMBERS I))
                                           (CAR I)))
                                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                  (SETQ I (CDR I))
                                  (GO LOOPLABEL))
                                (CDADR IR) (MINUS 1))))))))))
                  (COND
                   ((EQ AT GFALSE)
                    (PROGN
                     (SETQ TAGL NIL)
                     (COND
                      ((EQ (CADDR (CAADR ATL2)) N)
                       (PROG (I)
                         (SETQ I (CADR ATL2))
                        LAB
                         (COND ((NULL I) (RETURN NIL)))
                         ((LAMBDA (I)
                            (COND
                             ((LISTP (CAAR I))
                              (PROG (J)
                                (SETQ J (CAR I))
                               LAB
                                (COND ((NULL J) (RETURN NIL)))
                                ((LAMBDA (J) (SETQ TAGL (CONS J TAGL)))
                                 (CAR J))
                                (SETQ J (CDR J))
                                (GO LAB)))
                             (T (SETQ TAGL (CONS I TAGL)))))
                          (CAR I))
                         (SETQ I (CDR I))
                         (GO LAB))))
                     (SETQ VL (RL_VARLAT AT2))
                     (PROG (V)
                       (SETQ V VL)
                      LAB
                       (COND ((NULL V) (RETURN NIL)))
                       ((LAMBDA (V)
                          (PROGN
                           (SETQ M (ASSOC V (IC_VARLIST RLQEICDATA*)))
                           (COND
                            (M
                             (PROGN
                              (SETQ W
                                      (CDR
                                       (ASSOC (CADR M)
                                              (IC_TAGLIST RLQEICDATA*))))
                              (PROG (I)
                                (SETQ I W)
                               LAB
                                (COND ((NULL I) (RETURN NIL)))
                                ((LAMBDA (I)
                                   (COND
                                    ((LISTP (CAAR I))
                                     (PROG (J)
                                       (SETQ J (CAR I))
                                      LAB
                                       (COND ((NULL J) (RETURN NIL)))
                                       ((LAMBDA (J) (SETQ TAGL (CONS J TAGL)))
                                        (CAR J))
                                       (SETQ J (CDR J))
                                       (GO LAB)))
                                    (T (SETQ TAGL (CONS I TAGL)))))
                                 (CAR I))
                                (SETQ I (CDR I))
                                (GO LAB)))))))
                        (CAR V))
                       (SETQ V (CDR V))
                       (GO LAB))
                     (SETQ NATL GFALSE)
                     (SETQ ATL NIL)))
                   ((NEQ AT GTRUE) (SETQ NATL (LTO_INSERT AT NATL)))))))))
            (CAR AT))
           (SETQ AT (CDR AT))
           (GO LAB)))
        (GO WHILELABEL))
      (IC_SETVARLIST RLQEICDATA* NIL)
      (RETURN (LIST NATL TAGL)))) 
(PUT 'OFSF_SMMKATL1-TAGGED 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_SMMKATL1-TAGGED 'DEFINED-ON-LINE '1587) 
(PUT 'OFSF_SMMKATL1-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SMMKATL1-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMMKATL1-TAGGED (OP OLDKNOWL NEWKNOWL N)
    (COND ((EQ OP 'AND) (OFSF_SMMKATL-AND-TAGGED OLDKNOWL NEWKNOWL N))
          (T (OFSF_SMMKATL-OR-TAGGED OLDKNOWL NEWKNOWL N)))) 
(PUT 'OFSF_SMMKATL-AND-TAGGED 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SMMKATL-AND-TAGGED 'DEFINED-ON-LINE '1597) 
(PUT 'OFSF_SMMKATL-AND-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SMMKATL-AND-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMMKATL-AND-TAGGED (OLDKNOWL NEWKNOWL N)
    (PROG (W TAG)
      (COND
       ((AND (NOT *RLSIPW) *RLSIPO)
        (RETURN
         (PROG (I FORALL-RESULT FORALL-ENDPTR)
           (SETQ I NEWKNOWL)
           (COND ((NULL I) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (I)
                               (LIST (OFSF_IR2ATL 'AND I N)
                                     (CDR
                                      (ASSOC (CAR I)
                                             (IC_TAGLIST RLQEICDATA*)))))
                             (CAR I))
                            NIL)))
          LOOPLABEL
           (SETQ I (CDR I))
           (COND ((NULL I) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (I)
                       (LIST (OFSF_IR2ATL 'AND I N)
                             (CDR (ASSOC (CAR I) (IC_TAGLIST RLQEICDATA*)))))
                     (CAR I))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (RETURN
       (PROG (IR FORALL-RESULT FORALL-ENDPTR)
         (SETQ IR NEWKNOWL)
         (COND ((NULL IR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (IR)
                             (PROGN
                              (SETQ W (ATSOC (CAR IR) OLDKNOWL))
                              (SETQ TAG
                                      (CDR
                                       (ASSOC (CAR IR)
                                              (IC_TAGLIST RLQEICDATA*))))
                              (COND
                               ((NULL W) (LIST (OFSF_IR2ATL 'AND IR N) TAG))
                               (T (LIST (OFSF_SMMKATL-AND1 W IR N) TAG)))))
                           (CAR IR))
                          NIL)))
        LOOPLABEL
         (SETQ IR (CDR IR))
         (COND ((NULL IR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (IR)
                     (PROGN
                      (SETQ W (ATSOC (CAR IR) OLDKNOWL))
                      (SETQ TAG
                              (CDR (ASSOC (CAR IR) (IC_TAGLIST RLQEICDATA*))))
                      (COND ((NULL W) (LIST (OFSF_IR2ATL 'AND IR N) TAG))
                            (T (LIST (OFSF_SMMKATL-AND1 W IR N) TAG)))))
                   (CAR IR))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_SMMKATL-OR-TAGGED 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SMMKATL-OR-TAGGED 'DEFINED-ON-LINE '1612) 
(PUT 'OFSF_SMMKATL-OR-TAGGED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSF_SMMKATL-OR-TAGGED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SMMKATL-OR-TAGGED (OLDKNOWL NEWKNOWL N)
    (PROG (W TAG)
      (RETURN
       (PROG (IR FORALL-RESULT FORALL-ENDPTR)
         (SETQ IR NEWKNOWL)
         (COND ((NULL IR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (IR)
                             (PROGN
                              (SETQ W (ATSOC (CAR IR) OLDKNOWL))
                              (SETQ TAG
                                      (CDR
                                       (ASSOC (CAR IR)
                                              (IC_TAGLIST RLQEICDATA*))))
                              (COND
                               ((NULL W) (LIST (OFSF_IR2ATL 'OR IR N) TAG))
                               (T (LIST (OFSF_SMMKATL-OR1 W IR N) TAG)))))
                           (CAR IR))
                          NIL)))
        LOOPLABEL
         (SETQ IR (CDR IR))
         (COND ((NULL IR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (IR)
                     (PROGN
                      (SETQ W (ATSOC (CAR IR) OLDKNOWL))
                      (SETQ TAG
                              (CDR (ASSOC (CAR IR) (IC_TAGLIST RLQEICDATA*))))
                      (COND ((NULL W) (LIST (OFSF_IR2ATL 'OR IR N) TAG))
                            (T (LIST (OFSF_SMMKATL-OR1 W IR N) TAG)))))
                   (CAR IR))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'OFSFIC*OFSF_EXPLOITKNOWL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC*OFSF_EXPLOITKNOWL 'DEFINED-ON-LINE '1624) 
(PUT 'OFSFIC*OFSF_EXPLOITKNOWL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC*OFSF_EXPLOITKNOWL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC*OFSF_EXPLOITKNOWL (KNOWL)
    (PROG (SUBAL ZVL POSVL NEGVL GEQVL LEQVL NEQVL V REL A N)
      (SETQ N 0)
      (PROG (IR)
        (SETQ IR KNOWL)
       LAB
        (COND ((NULL IR) (RETURN NIL)))
        ((LAMBDA (IR)
           (COND
            ((SETQ V (SFTO_VARP (CAR IR)))
             (PROG (LE)
               (SETQ LE (CDR IR))
              LAB
               (COND ((NULL LE) (RETURN NIL)))
               ((LAMBDA (LE)
                  (PROGN
                   (PROG (G524)
                     (SETQ G524 (CDR LE))
                     (SETQ REL (CAR G524))
                     (SETQ A (CDR G524))
                     (RETURN G524))
                   (SETQ A (NEGSQ A))
                   (SETQ N (OR (CAR A) 0))
                   (COND
                    ((EQ REL 'EQUAL)
                     (PROGN
                      (COND
                       (*RLSIPPSUBST
                        (PROGN
                         (SETQ SUBAL (CONS (CONS V A) SUBAL))
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         NIL))
                       (T
                        (COND
                         ((GREATERP N 0) (SETQ POSVL (LTO_INSERTQ V POSVL)))
                         ((LESSP N 0) (SETQ NEGVL (LTO_INSERTQ V NEGVL)))
                         ((EQN N 0) (SETQ ZVL (LTO_INSERTQ V ZVL))))))))
                    ((EQ REL 'GREATERP)
                     (PROGN
                      (COND
                       ((GEQ N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ POSVL (LTO_INSERTQ V POSVL)))))))
                    ((EQ REL 'GEQ)
                     (PROGN
                      (COND
                       ((GREATERP N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ POSVL (LTO_INSERTQ V POSVL))))
                       ((EQN N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ GEQVL (LTO_INSERTQ V GEQVL)))))))
                    ((EQ REL 'LESSP)
                     (PROGN
                      (COND
                       ((LEQ N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ NEGVL (LTO_INSERTQ V NEGVL)))))))
                    ((EQ REL 'LEQ)
                     (PROGN
                      (COND
                       ((LESSP N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ NEGVL (LTO_INSERTQ V NEGVL))))
                       ((EQN N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ LEQVL (LTO_INSERTQ V LEQVL)))))))
                    ((EQ REL 'NEQ)
                     (PROGN
                      (COND
                       ((EQN N 0)
                        (PROGN
                         (COND
                          (*RLQEICSIMPL
                           (IC_INSERTVARLIST RLQEICDATA* (CONS V IR))))
                         (SETQ NEQVL (LTO_INSERTQ V NEQVL))))))))))
                (CAR LE))
               (SETQ LE (CDR LE))
               (GO LAB)))))
         (CAR IR))
        (SETQ IR (CDR IR))
        (GO LAB))
      (RETURN (LIST SUBAL ZVL POSVL NEGVL GEQVL LEQVL NEQVL)))) 
(PUT 'OFSFIC*OFSF_QEMKANS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC*OFSF_QEMKANS 'DEFINED-ON-LINE '1689) 
(PUT 'OFSFIC*OFSF_QEMKANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC*OFSF_QEMKANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC*OFSF_QEMKANS (AN)
    (PROG (ANUAL W TIME GCTIME)
      (SETQ TIME 0)
      (SETQ GCTIME 0)
      (COND (*RLVERBOSE (PROGN (SETQ TIME (TIME)) (SETQ GCTIME (GCTIME)))))
      (SETQ ANUAL (OFSFIC_QEMKSTDANS AN))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2
          (LIST "++++ Time for answer processing: " (DIFFERENCE (TIME) TIME)
                " ms"))
         (SETQ GCTIME (DIFFERENCE (GCTIME) GCTIME))
         (COND
          ((GREATERP GCTIME 0)
           (IOTO_PRIN2T (LIST " plus GC time: " GCTIME " ms")))))))
      (RETURN ANUAL))) 
(PUT 'OFSFIC_QEMKSTDANS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSFIC_QEMKSTDANS 'DEFINED-ON-LINE '1718) 
(PUT 'OFSFIC_QEMKSTDANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFIC.RED) 
(PUT 'OFSFIC_QEMKSTDANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSFIC_QEMKSTDANS (AN)
    (PROG (Y V SUB XARGL F CTX RAT ANU)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "++++ Determining standard real numbers for the answers "
               (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                 (SETQ Y AN)
                 (COND ((NULL Y) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL)))
                LOOPLABEL
                 (SETQ Y (CDR Y))
                 (COND ((NULL Y) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               "..."))))
      (PROG ()
       WHILELABEL
        (COND ((NOT AN) (RETURN NIL)))
        (PROGN
         (SETQ Y (PROG1 (CAR AN) (SETQ AN (CDR AN))))
         (PROG (G525 G526)
           (SETQ G525 Y)
           (SETQ G526 G525)
           (SETQ V (CAR G525))
           (SETQ G525 (CDR G525))
           (SETQ SUB (CAR G525))
           (SETQ G525 (CDR G525))
           (SETQ XARGL (CAR G525))
           (SETQ G525 (CDR G525))
           (SETQ F (CAR G525))
           (SETQ G525 (CDR G525))
           (RETURN G526))
         (COND
          ((EQ SUB 'CADANU)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = Anu computed by CAD"))))
            (PROG (W1)
              (SETQ W1 (CONS V XARGL))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))
            NIL))
          ((EQ SUB 'ARBITRARY)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = arbitrary -> 0"))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_ARBITRARY2ANU V)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_SHIFT-INDICATOR)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = shift"))))
            (REDERR "unexpected shift - tell sturm@redlog.eu!")
            NIL))
          ((EQ SUB 'OFSF_QESUBCQ)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = quotient"))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU (CADR XARGL) V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCR1)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = root expression"))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_R2ANU (CADR XARGL) V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBI)
           (PROGN
            (COND (NIL NIL))
            (COND
             ((EQ (CAR XARGL) 'MINF)
              (PROGN
               (COND
                (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = - infinity"))))
               (SETQ RAT (OFSFIC_FIX-MINF F V CTX)))))
            (COND
             ((EQ (CAR XARGL) 'PINF)
              (PROGN
               (COND
                (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = + infinity"))))
               (SETQ RAT (OFSFIC_FIX-PINF F V CTX)))))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (LIST " -> " (IOTO_FORM2STR (PREPSQ RAT))))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU RAT V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCQME)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = quotient - epsilon"))))
            (SETQ ANU (OFSFIC_Q2ANU (CADR XARGL) V CTX))
            (SETQ RAT (OFSFIC_FIX-PME F V ANU CTX))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (LIST " -> " (IOTO_FORM2STR (PREPSQ RAT))))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU RAT V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCQPE)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = quotient + epsilon"))))
            (SETQ ANU (OFSFIC_Q2ANU (CADR XARGL) V CTX))
            (SETQ RAT (OFSFIC_FIX-PPE F V ANU CTX))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (LIST " -> " (IOTO_FORM2STR (PREPSQ RAT))))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU RAT V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCRME1)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = root expression - epsilon"))))
            (SETQ ANU (OFSFIC_R2ANU (CADR XARGL) V CTX))
            (SETQ RAT (OFSFIC_FIX-PME F V ANU CTX))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (LIST " -> " (IOTO_FORM2STR (PREPSQ RAT))))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU RAT V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCRPE1)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = root expression + epsilon"))))
            (SETQ ANU (OFSFIC_R2ANU (CADR XARGL) V CTX))
            (SETQ RAT (OFSFIC_FIX-PPE F V ANU CTX))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2 (LIST " -> " (IOTO_FORM2STR (PREPSQ RAT))))))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSFIC_Q2ANU RAT V CTX)))
              (SETQ CTX (CONS W1 CTX))
              (RETURN W1))))
          (T (REDERR "BUG IN ofsfic_qemkstdans"))))
        (GO WHILELABEL))
      (RETURN (REVERSIP CTX)))) 
(PUT 'OFSFIC_ARBITRARY2ANU 'NUMBER-OF-ARGS 1) 
(DE OFSFIC_ARBITRARY2ANU (V) (ANU_FROMRAT V (RAT_0))) 
(PUT 'OFSFIC_SHIFT2ANU 'NUMBER-OF-ARGS 2) 
(DE OFSFIC_SHIFT2ANU (V SV) (ANU_FROMRAT V (RAT_0))) 
(PUT 'OFSFIC_Q2ANU 'NUMBER-OF-ARGS 3) 
(DE OFSFIC_Q2ANU (Q V CTX)
    (PROG (N D AEX W CB)
      (SETQ N (CAR Q))
      (SETQ D (CDR Q))
      (SETQ AEX
              (AEX_FROMSF
               (ADDF
                ((LAMBDA (G528)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF D G528))
                         (T (POLY-MULTF D G528))))
                 (LIST (CONS (CONS V 1) 1)))
                (NEGF N))))
      (PROG (PR)
        (SETQ PR CTX)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR) (SETQ AEX (AEX_BIND AEX (CAR PR) (CDR PR)))) (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (SETQ CB (AEX_CAUCHYBOUND AEX V))
      (SETQ CB (ADDSQ CB (CONS 1 1)))
      (RETURN (ANU_MK AEX (IV_MK (NEGSQ CB) CB))))) 
(PUT 'OFSFIC_R2ANU 'NUMBER-OF-ARGS 3) 
(DE OFSFIC_R2ANU (R V CTX)
    (PROG (A B C D AEXB AEXC SGNB SGNC P AEX AEXD SGND ROOTS)
      (PROG (G529 G530)
        (SETQ G529 R)
        (SETQ G530 G529)
        (SETQ A (CAR G529))
        (SETQ G529 (CDR G529))
        (SETQ B (CAR G529))
        (SETQ G529 (CDR G529))
        (SETQ C (CAR G529))
        (SETQ G529 (CDR G529))
        (SETQ D (CAR G529))
        (SETQ G529 (CDR G529))
        (RETURN G530))
      (SETQ AEXB (AEX_FROMSF B))
      (SETQ AEXC (AEX_FROMSF C))
      (PROG (PR)
        (SETQ PR CTX)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ AEXB (AEX_BIND AEXB (CAR PR) (CDR PR)))
            (SETQ AEXC (AEX_BIND AEXC (CAR PR) (CDR PR)))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (SETQ SGNB (AEX_SGN AEXB))
      (COND
       ((EQN (AEX_SGN AEXB) 0)
        (RETURN (OFSFIC_Q2ANU (MULTSQ (CONS A 1) (INVSQ (CONS D 1))) V CTX))))
      (SETQ SGNC (AEX_SGN AEXC))
      (COND
       ((EQN SGNC 0)
        (RETURN (OFSFIC_Q2ANU (MULTSQ (CONS A 1) (INVSQ (CONS D 1))) V CTX))))
      (COND (NIL NIL))
      (SETQ P
              (ADDF
               ((LAMBDA (G531 G532)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G531 G532))
                        (T (POLY-MULTF G531 G532))))
                (EXPTF D 2) (EXPTF (LIST (CONS (CONS V 1) 1)) 2))
               (ADDF
                (NEGF
                 ((LAMBDA (G538)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 G538))
                          (T (POLY-MULTF 2 G538))))
                  ((LAMBDA (G536)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G536))
                           (T (POLY-MULTF A G536))))
                   ((LAMBDA (G534)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF D G534))
                            (T (POLY-MULTF D G534))))
                    (LIST (CONS (CONS V 1) 1))))))
                (ADDF (EXPTF A 2)
                      (NEGF
                       ((LAMBDA (G539)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G539 C))
                                (T (POLY-MULTF G539 C))))
                        (EXPTF B 2)))))))
      (SETQ AEX (AEX_FROMSF P))
      (SETQ AEXD (AEX_FROMSF D))
      (PROG (PR)
        (SETQ PR CTX)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ AEX (AEX_BIND AEX (CAR PR) (CDR PR)))
            (SETQ AEXD (AEX_BIND AEXD (CAR PR) (CDR PR)))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (SETQ SGND (AEX_SGN AEXD))
      (SETQ ROOTS (AEX_FINDROOTS AEX V))
      (COND (NIL NIL))
      (COND ((LESSP (TIMES SGNB SGND) 0) (RETURN (CAR ROOTS))))
      (RETURN (CADR ROOTS)))) 
(PUT 'OFSFIC_FIX-MINF 'NUMBER-OF-ARGS 3) 
(DE OFSFIC_FIX-MINF (F V CTX)
    (PROG (TVAL VVAL)
      (SETQ VVAL 0)
      (SETQ VVAL (MINUS 1))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ VVAL (TIMES 2 VVAL))
         (SETQ TVAL
                 (QFF_EVALATP F
                  (CONS (CONS V (ANU_FROMRAT V (CONS VVAL 1))) CTX))))
        (COND ((NOT (EQ TVAL 'TRUE)) (GO REPEATLABEL))))
      (RETURN (CONS VVAL 1)))) 
(PUT 'OFSFIC_FIX-PINF 'NUMBER-OF-ARGS 3) 
(DE OFSFIC_FIX-PINF (F V CTX)
    (PROG (TVAL VVAL)
      (SETQ VVAL 0)
      (SETQ VVAL 1)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ VVAL (TIMES 2 VVAL))
         (SETQ TVAL
                 (QFF_EVALATP F
                  (CONS (CONS V (ANU_FROMRAT V (CONS VVAL 1))) CTX))))
        (COND ((NOT (EQ TVAL 'TRUE)) (GO REPEATLABEL))))
      (RETURN (CONS VVAL 1)))) 
(PUT 'OFSFIC_FIX-PME 'NUMBER-OF-ARGS 4) 
(DE OFSFIC_FIX-PME (F V ROOT CTX)
    (PROG (SC STP LB RB VVAL TVAL)
      (SETQ SC (AEX_STDSTURMCHAIN (ANU_DP ROOT) V))
      (SETQ STP (RAT_1))
      (PROG ()
       REPEATLABEL
        (PROGN
         (ANU_REFINEIP ROOT SC)
         (SETQ LB (IV_LB (ANU_IV ROOT)))
         (SETQ RB (IV_RB (ANU_IV ROOT)))
         (COND
          ((RAT_EQ LB RB)
           (PROGN
            (SETQ VVAL (RAT_MINUS LB STP))
            (SETQ STP (RAT_QUOT STP (RAT_FROMNUM 2)))))
          (T (SETQ VVAL LB)))
         (SETQ TVAL (QFF_EVALATP F (CONS (CONS V (ANU_FROMRAT V VVAL)) CTX))))
        (COND ((NOT (EQ TVAL 'TRUE)) (GO REPEATLABEL))))
      (RETURN VVAL))) 
(PUT 'OFSFIC_FIX-PPE 'NUMBER-OF-ARGS 4) 
(DE OFSFIC_FIX-PPE (F V ROOT CTX)
    (PROG (SC STP LB RB VVAL TVAL)
      (SETQ SC (AEX_STDSTURMCHAIN (ANU_DP ROOT) V))
      (SETQ STP (RAT_1))
      (PROG ()
       REPEATLABEL
        (PROGN
         (ANU_REFINEIP ROOT SC)
         (SETQ LB (IV_LB (ANU_IV ROOT)))
         (SETQ RB (IV_RB (ANU_IV ROOT)))
         (COND
          ((RAT_EQ LB RB)
           (PROGN
            (SETQ VVAL (RAT_ADD LB STP))
            (SETQ STP (RAT_QUOT STP (RAT_FROMNUM 2)))))
          (T (SETQ VVAL RB)))
         (SETQ TVAL (QFF_EVALATP F (CONS (CONS V (ANU_FROMRAT V VVAL)) CTX))))
        (COND ((NOT (EQ TVAL 'TRUE)) (GO REPEATLABEL))))
      (RETURN VVAL))) 
(ENDMODULE) 