(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFTROP)) 
(REVISION 'OFSFTROP
          "$Id: ofsftrop.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFTROP "(c) 2013-2017 T. Sturm") 
(FLUID '(OFSF_LPSOLPREC*)) 
(SETQ OFSF_LPSOLPREC* 8) 
(FLUID '(RLSAT2POLATNUM*)) 
(SWITCH (LIST 'RLGUROBI)) 
(OFF1 'RLGUROBI) 
(SWITCH (LIST 'RLTROPDEL0)) 
(SWITCH (LIST 'RLTROPILP)) 
(ON1 'RLTROPILP) 
(SWITCH (LIST 'RLTROPSOS)) 
(OFF1 'RLTROPSOS) 
(SWITCH (LIST 'RLTROPLCM)) 
(ON1 'RLTROPLCM) 
(PUT 'OFSF_TROPSAT 'NUMBER-OF-ARGS 1) 
(DE OFSF_TROPSAT (F) (OFSF_TROPSAT1 F NIL)) 
(PUT 'OFSF_PTROPSAT 'NUMBER-OF-ARGS 1) 
(DE OFSF_PTROPSAT (F) (OFSF_TROPSAT1 F T)) 
(PUT 'OFSF_SAT2POL 'NUMBER-OF-ARGS 1) 
(DE OFSF_SAT2POL (F) (OFSF_SAT2POL1 F NIL)) 
(PUT 'OFSF_PSAT2POL 'NUMBER-OF-ARGS 1) 
(DE OFSF_PSAT2POL (F) (OFSF_SAT2POL1 F T)) 
(PUT 'OFSF_TROPSAT1 'NUMBER-OF-ARGS 2) 
(DE OFSF_TROPSAT1 (F POSP)
    (PROG (P SOL VL FLAG ONE OTHER)
      (SETQ P (OFSF_SAT2POL1 F POSP))
      (PROG (G429 G430)
        (SETQ G429 (OFSF_ZEROP1 P 'TRUE POSP))
        (SETQ G430 G429)
        (SETQ FLAG (CAR G429))
        (SETQ G429 (CDR G429))
        (SETQ ONE (CAR G429))
        (SETQ G429 (CDR G429))
        (SETQ OTHER (CAR G429))
        (SETQ G429 (CDR G429))
        (RETURN G430))
      (COND
       ((EQN FLAG 1)
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2 "found candidate, solving ... ")))
         (SETQ SOL (CAR (OFSF_ZEROSOLVE P (CAR ONE) (CAR OTHER))))
         (COND (*RLVERBOSE (IOTO_PRIN2T "done")))
         (SETQ VL (CL_FVARL1 F))
         (SETQ SOL
                 (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                   (SETQ PR SOL)
                  STARTOVER
                   (COND ((NULL PR) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (PR) (COND ((MEMQ (CAR PR) VL) (LIST PR))))
                            (CAR PR)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ PR (CDR PR))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL PR) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (PR) (COND ((MEMQ (CAR PR) VL) (LIST PR))))
                            (CAR PR)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ PR (CDR PR))
                   (GO LOOPLABEL)))
         (COND
          (*RLVERBOSE
           (IOTO_PRIN2T "evaluating input formula at candidate solution")))
         (RETURN 'SAT))))
      (COND ((EQN FLAG 0) (RETURN 'UNSAT)))
      (COND ((EQN FLAG (MINUS 1)) (RETURN 'UNKNOWN)))
      (REDERR
       (LIST "something wrong in ofsf_tropsat: ofsf_zerop1 returned " FLAG)))) 
(PUT 'OFSF_SAT2POL1 'NUMBER-OF-ARGS 2) 
(DE OFSF_SAT2POL1 (F POSP)
    (PROG (W RLSAT2POLATNUM*)
      (SETQ RLSAT2POLATNUM* 0)
      (SETQ W (CL_SIMPL (CL_PNF F) NIL (MINUS 1)))
      (SETQ RLSAT2POLATNUM* (CL_ATNUM F))
      (RETURN (CAR (OFSF_FORMULA2POL1 W NIL NIL NIL POSP))))) 
(PUT 'OFSF_FORMULA2POL1 'NUMBER-OF-ARGS 5) 
(DE OFSF_FORMULA2POL1 (F GEAL GRAL NEAL POSP)
    (PROG (OP E EE)
      (COND ((EQ F 'TRUE) (RETURN (LIST 0 GEAL GRAL NEAL))))
      (COND ((EQ F 'FALSE) (RETURN (LIST 1 GEAL GRAL NEAL))))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((EQ OP 'OR)
        (PROGN
         (SETQ E 1)
         (PROG (A)
           (SETQ A (CDR F))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (PROGN
               (PROG (G431 G432)
                 (SETQ G431 (OFSF_FORMULA2POL1 A GEAL GRAL NEAL POSP))
                 (SETQ G432 G431)
                 (SETQ EE (CAR G431))
                 (SETQ G431 (CDR G431))
                 (SETQ GEAL (CAR G431))
                 (SETQ G431 (CDR G431))
                 (SETQ GRAL (CAR G431))
                 (SETQ G431 (CDR G431))
                 (SETQ NEAL (CAR G431))
                 (SETQ G431 (CDR G431))
                 (RETURN G432))
               (SETQ E
                       (SFTO_SQFPARTF
                        (COND (*RLTROPLCM (LCM* EE E))
                              (T
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF EE E))
                                     (T (POLY-MULTF EE E)))))))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (RETURN (LIST E GEAL GRAL NEAL)))))
      (COND
       ((EQ OP 'AND)
        (PROGN
         (SETQ E NIL)
         (PROG (A)
           (SETQ A (CDR F))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (PROGN
               (PROG (G433 G434)
                 (SETQ G433 (OFSF_FORMULA2POL1 A GEAL GRAL NEAL POSP))
                 (SETQ G434 G433)
                 (SETQ EE (CAR G433))
                 (SETQ G433 (CDR G433))
                 (SETQ GEAL (CAR G433))
                 (SETQ G433 (CDR G433))
                 (SETQ GRAL (CAR G433))
                 (SETQ G433 (CDR G433))
                 (SETQ NEAL (CAR G433))
                 (SETQ G433 (CDR G433))
                 (RETURN G434))
               (COND (*RLTROPSOS (SETQ EE (EXPTF EE 2))))
               (SETQ E (ADDF EE E))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ E (SFTO_SQFPARTF E))
         (RETURN (LIST E GEAL GRAL NEAL)))))
      (COND
       (*RLVERBOSE
        (IOTO_PRIN2
         (LIST "[" (SETQ RLSAT2POLATNUM* (DIFFERENCE RLSAT2POLATNUM* 1))))))
      (COND
       ((EQ OP 'EQUAL)
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2 "e] ")))
         (RETURN (LIST (CADR F) GEAL GRAL NEAL)))))
      (COND
       ((EQ OP 'GEQ) (RETURN (OFSF_GEQ2POL (CADR F) GEAL GRAL NEAL POSP))))
      (COND
       ((EQ OP 'LEQ)
        (RETURN (OFSF_GEQ2POL (NEGF (CADR F)) GEAL GRAL NEAL POSP))))
      (COND
       ((EQ OP 'GREATERP)
        (RETURN (OFSF_GREATERP2POL (CADR F) GEAL GRAL NEAL POSP))))
      (COND
       ((EQ OP 'LESSP)
        (RETURN (OFSF_GREATERP2POL (NEGF (CADR F)) GEAL GRAL NEAL POSP))))
      (COND
       ((EQ OP 'NEQ) (RETURN (OFSF_NEQ2POL (CADR F) GEAL GRAL NEAL POSP))))
      (REDERR (LIST "something wrong in ofsf_formula2pol1: op =  " OP)))) 
(PUT 'OFSF_GEQ2POL 'NUMBER-OF-ARGS 5) 
(DE OFSF_GEQ2POL (LHS GEAL GRAL NEAL POSP)
    (PROG (W E)
      (SETQ W (ASSOC LHS GEAL))
      (COND
       (W
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2 "!] ")))
         (RETURN (LIST (CDR W) GEAL GRAL NEAL)))))
      (SETQ E
              (COND
               (POSP
                (SFTO_SQFPARTF
                 (ADDF LHS (NEGF (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1))))))
               (T
                (SFTO_SQFPARTF
                 (ADDF LHS
                       (NEGF
                        (EXPTF (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1))
                               2)))))))
      (COND (*RLVERBOSE (IOTO_PRIN2 "] ")))
      (RETURN (LIST E (CONS (CONS LHS E) GEAL) GRAL NEAL)))) 
(PUT 'OFSF_GREATERP2POL 'NUMBER-OF-ARGS 5) 
(DE OFSF_GREATERP2POL (LHS GEAL GRAL NEAL POSP)
    (PROG (W E)
      (SETQ W (ASSOC LHS GRAL))
      (COND
       (W
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2 "!] ")))
         (RETURN (LIST (CDR W) GEAL GRAL NEAL)))))
      (SETQ E
              (COND
               (POSP
                (SFTO_SQFPARTF
                 (ADDF
                  ((LAMBDA (G435)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G435 LHS))
                           (T (POLY-MULTF G435 LHS))))
                   (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1)))
                  (NEGF 1))))
               (T
                (SFTO_SQFPARTF
                 (ADDF
                  ((LAMBDA (G437)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G437 LHS))
                           (T (POLY-MULTF G437 LHS))))
                   (EXPTF (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1)) 2))
                  (NEGF 1))))))
      (COND (*RLVERBOSE (IOTO_PRIN2 "] ")))
      (RETURN (LIST E GEAL (CONS (CONS LHS E) GRAL) NEAL)))) 
(PUT 'OFSF_NEQ2POL 'NUMBER-OF-ARGS 5) 
(DE OFSF_NEQ2POL (LHS GEAL GRAL NEAL POSP)
    (PROG (W E)
      (SETQ W (ASSOC LHS NEAL))
      (COND
       (W
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2 "!] ")))
         (RETURN (LIST (CDR W) GEAL GRAL NEAL)))))
      (SETQ E
              (COND
               (POSP
                (SFTO_SQFPARTF
                 (ADDF
                  ((LAMBDA (G439)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G439 LHS))
                           (T (POLY-MULTF G439 LHS))))
                   (ADDF (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1))
                         (NEGF (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1)))))
                  (NEGF 1))))
               (T
                (SFTO_SQFPARTF
                 (ADDF
                  ((LAMBDA (G441)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G441 LHS))
                           (T (POLY-MULTF G441 LHS))))
                   (LIST (CONS (CONS (INTERN (GENSYM)) 1) 1)))
                  (NEGF 1))))))
      (COND (*RLVERBOSE (IOTO_PRIN2 "] ")))
      (RETURN (LIST E GEAL GRAL (CONS (CONS LHS E) NEAL))))) 
(PUT 'ZEROP 'PSOPFN 'OFSF_ZEROPEVAL) 
(PUT 'PZEROP 'PSOPFN 'OFSF_PZEROPEVAL) 
(PUT 'OFSF_ZEROPEVAL 'NUMBER-OF-ARGS 1) 
(DE OFSF_ZEROPEVAL (ARGL) (OFSF_ZEROPEVAL1 ARGL NIL)) 
(PUT 'OFSF_PZEROPEVAL 'NUMBER-OF-ARGS 1) 
(DE OFSF_PZEROPEVAL (ARGL) (OFSF_ZEROPEVAL1 ARGL T)) 
(PUT 'OFSF_ZEROPEVAL1 'NUMBER-OF-ARGS 2) 
(DE OFSF_ZEROPEVAL1 (ARGL POSP)
    (PROG (F SCOND FLAG ONE OTHER ZERO)
      (SETQ F (CAR (SIMP (CAR ARGL))))
      (SETQ SCOND (COND ((CDR ARGL) (RL_SIMP (CADR ARGL))) (T 'TRUE)))
      (PROG (G443 G444)
        (SETQ G443 (OFSF_ZEROP1 F SCOND POSP))
        (SETQ G444 G443)
        (SETQ FLAG (CAR G443))
        (SETQ G443 (CDR G443))
        (SETQ ONE (CAR G443))
        (SETQ G443 (CDR G443))
        (SETQ OTHER (CAR G443))
        (SETQ G443 (CDR G443))
        (SETQ ZERO (CAR G443))
        (SETQ G443 (CDR G443))
        (RETURN G444))
      (COND ((NOT (EQN FLAG 1)) (RETURN (LIST 'LIST FLAG))))
      (SETQ ONE (OFSF_S2APOINTPAIR ONE))
      (SETQ OTHER (OFSF_S2APOINTPAIR OTHER))
      (SETQ ZERO (OFSF_S2APOINTPAIR ZERO))
      (RETURN (LIST 'LIST FLAG ONE OTHER ZERO)))) 
(PUT 'OFSF_S2APOINTPAIR 'NUMBER-OF-ARGS 1) 
(DE OFSF_S2APOINTPAIR (PP) (LIST 'LIST (OFSF_AL2EQL (CAR PP)) (CADR PP))) 
(PUT 'OFSF_AL2EQL 'NUMBER-OF-ARGS 1) 
(DE OFSF_AL2EQL (AL)
    (CONS 'LIST
          (PROG (PR FORALL-RESULT FORALL-ENDPTR)
            (SETQ PR AL)
            (COND ((NULL PR) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (PR) (LIST 'EQUAL (CAR PR) (CDR PR)))
                              (CAR PR))
                             NIL)))
           LOOPLABEL
            (SETQ PR (CDR PR))
            (COND ((NULL PR) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (PR) (LIST 'EQUAL (CAR PR) (CDR PR))) (CAR PR))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'OFSF_ZEROP 'NUMBER-OF-ARGS 2) 
(DE OFSF_ZEROP (F SCOND) (OFSF_ZEROP1 F SCOND NIL)) 
(PUT 'OFSF_PZEROP 'NUMBER-OF-ARGS 2) 
(DE OFSF_PZEROP (F SCOND) (OFSF_ZEROP1 F SCOND T)) 
(PUT 'OFSF_ZEROP1 'NUMBER-OF-ARGS 3) 
(DE OFSF_ZEROP1 (F SCOND POSP)
    (PROG (ONE FONE OTHER ZERO VL FLAG)
      (SETQ FLAG 0)
      (COND
       ((OR (ATOM F) (ATOM (CAR F)))
        (RETURN
         (COND ((NULL F) '(1 NIL NIL NIL)) ((MINUSF F) '(0 NIL NIL NIL))
               (T '(3 NIL NIL NIL))))))
      (SETQ VL (SORT (KERNELS F) 'ORDOP))
      (PROG (G445 G446)
        (SETQ G445 (OFSF_ZEROPTRYONE F VL))
        (SETQ G446 G445)
        (SETQ FLAG (CAR G445))
        (SETQ G445 (CDR G445))
        (SETQ ONE (CAR G445))
        (SETQ G445 (CDR G445))
        (SETQ FONE (CAR G445))
        (SETQ G445 (CDR G445))
        (RETURN G446))
      (COND ((EQN FLAG 2) (RETURN '(2 NIL NIL NIL))))
      (COND (NIL NIL))
      (PROG (G447 G448)
        (SETQ G447 (OFSF_ZEROP2 F SCOND ONE FONE POSP))
        (SETQ G448 G447)
        (SETQ FLAG (CAR G447))
        (SETQ G447 (CDR G447))
        (SETQ OTHER (CAR G447))
        (SETQ G447 (CDR G447))
        (SETQ ZERO (CAR G447))
        (SETQ G447 (CDR G447))
        (RETURN G448))
      (COND ((EQN FLAG (MINUS 1)) (RETURN '(-1 NIL NIL NIL))))
      (COND ((EQN FLAG 0) (RETURN '(0 NIL NIL NIL))))
      (COND (NIL NIL))
      (RETURN (LIST 1 (LIST ONE FONE) OTHER ZERO)))) 
(PUT 'OFSF_ZEROPTRYONE 'NUMBER-OF-ARGS 2) 
(DE OFSF_ZEROPTRYONE (F VL)
    (PROG (ONE FONE)
      (SETQ ONE
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VL)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (CONS V 1)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (CONS V 1)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FONE (SFTO_SF2INT (SFTO_FSUB F ONE)))
      (COND
       ((EQN FONE 0)
        (PROGN
         (COND
          (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++ f is zero at (1, ..., 1)"))))
         (RETURN '(2 NIL NIL)))))
      (RETURN (LIST (MINUS 1) ONE FONE)))) 
(PUT 'OFSF_ZEROP2 'NUMBER-OF-ARGS 5) 
(DE OFSF_ZEROP2 (F SCOND ONE FONE POSP)
    (PROG (FF MONL VL W D)
      (SETQ D 0)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (COND
          ((GREATERP FONE 0)
           (LIST
            "+++ f is positive at (1,...,1), looking for a positive point of -f"))
          (T
           (LIST
            "+++ f is negative at (1,...,1) looking for a positive point of f"))))))
      (SETQ FF (COND ((GREATERP FONE 0) (NEGF F)) (T F)))
      (PROG (G449)
        (SETQ G449 (SFTO_SF2MONLIP FF))
        (SETQ VL (CAR G449))
        (SETQ MONL (CDR G449))
        (RETURN G449))
      (SETQ D (LENGTH VL))
      (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++ dimension: " D))))
      (SETQ W (OFSF_POSDIRP FF SCOND ONE FONE D VL MONL POSP))
      (COND
       ((EQCAR W 0)
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T
            (LIST "+++ " (COND ((GREATERP FONE 0) "-") (T ""))
                  "f is negative definite"))))
         (RETURN '(0 NIL NIL)))))
      (COND ((EQCAR W (MINUS 1)) (RETURN '(-1 NIL NIL))))
      (RETURN W))) 
(SWITCH (LIST 'RMONL)) 
(PUT 'OFSF_POSDIRP 'NUMBER-OF-ARGS 8) 
(DE OFSF_POSDIRP (FF SCOND ONE FONE D VL MONL POSP)
    (PROG (POSL SNEGL HNEGL W DELPOSP NP NS NH)
      (SETQ NP 0)
      (SETQ NS 0)
      (SETQ NH 0)
      (COND (*RMONL (SETQ MONL (REVERSIP MONL))))
      (COND (*RLTROPILP (LP_NEWMODEL 1 D)) (T (LP_NEWMODEL (PLUS D 1) 0)))
      (PROG (PT)
        (SETQ PT MONL)
       LAB
        (COND ((NULL PT) (RETURN NIL)))
        ((LAMBDA (PT)
           (COND
            ((AND *RLTROPDEL0 (LTO_0LISTP (CAR PT)))
             (PROGN
              (COND (*RLVERBOSE (IOTO_TPRIN2T "+++ deleting (0,...,0)")))
              (SETQ DELPOSP (GREATERP (CDR PT) 0))))
            ((GREATERP (CDR PT) 0)
             (PROGN (SETQ POSL (CONS (CAR PT) POSL)) (SETQ NP (PLUS NP 1))))
            ((AND (NOT POSP) (OFSF_SOFTNEGP VL (CAR PT)))
             (PROGN (SETQ SNEGL (CONS (CAR PT) SNEGL)) (SETQ NS (PLUS NS 1))))
            (T
             (PROGN
              (SETQ HNEGL (CONS (CAR PT) HNEGL))
              (SETQ NH (PLUS NH 1))))))
         (CAR PT))
        (SETQ PT (CDR PT))
        (GO LAB))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2 (LIST "+++ number of points in the frame "))
         (COND
          (POSP
           (IOTO_PRIN2 (LIST "(+, -, all): " NP ", " NH ", " (PLUS NP NH))))
          (T
           (IOTO_TPRIN2
            (LIST "(+, soft -, hard -, all): " NP ", " NS ", " NH ", "
                  (PLUS NP NS NH)))))
         NIL)))
      (COND
       ((AND (NULL POSL) (OR POSP (NULL SNEGL)) (NOT DELPOSP))
        (RETURN '(0 NIL NIL))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "+++ generating basic " (COND (*RLTROPILP "I") (T ""))
               "LP problem ... "))))
      (PROG (L)
        (SETQ L (LIST POSL SNEGL HNEGL))
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L) (OFSF_ADDCONSTRAINTS L)) (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (LP_UPDATEMODEL)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_PRIN2T "done")
         (IOTO_TPRIN2
          (LIST "+++ " (COND (*RLTROPILP "I") (T "")) "LP solving "
                (LP_OPTACTION) ": ")))))
      (SETQ W (OFSF_POSDIRP1 FF SCOND ONE FONE POSL NP D VL NIL))
      (COND
       ((AND (NOT POSP) (EQCAR W (MINUS 1)))
        (PROGN
         (COND (*RLVERBOSE (IOTO_CTERPRI)))
         (SETQ W (OFSF_POSDIRP1 FF SCOND ONE FONE SNEGL NS D VL T)))))
      (LP_FREEMODEL)
      (RETURN W))) 
(PUT 'OFSF_POSDIRP1 'NUMBER-OF-ARGS 9) 
(DE OFSF_POSDIRP1 (FF SCOND ONE FONE L C D VL SNEGP)
    (PROG (CNT W DIR EV NVAR FLAG OTHER ZERO FZERO WW I SKIPROWS)
      (SETQ I 0)
      (SETQ SKIPROWS 0)
      (COND ((NULL L) (RETURN '(-1 NIL NIL))))
      (SETQ CNT T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CNT L)) (RETURN NIL)))
        (PROGN
         (COND
          (*RLVERBOSE
           (PROGN (IOTO_PRIN2 (LIST "[" C "] ")) (SETQ C (DIFFERENCE C 1)))))
         (SETQ EV (PROG1 (CAR L) (SETQ L (CDR L))))
         (LP_NEGCONSTR SKIPROWS)
         (SETQ W (LP_OPTIMIZE))
         (COND
          ((AND (NOT (MEMQ W '(INFEASIBLE UNBOUNDED))) (OFSF_SMALLP W))
           (PROGN
            (SETQ DIR
                    (CONS (CONS 'C (PROG1 (CAR W) (SETQ W (CDR W))))
                          (PROG (I FORALL-RESULT FORALL-ENDPTR)
                            (SETQ I 1)
                            (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             (CONS (MKID 'N I)
                                                   (PROG1 (CAR W)
                                                     (SETQ W (CDR W))))
                                             NIL)))
                           LOOPLABEL
                            (SETQ I (PLUS2 I 1))
                            (COND
                             ((MINUSP (DIFFERENCE D I))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     (CONS (MKID 'N I)
                                           (PROG1 (CAR W) (SETQ W (CDR W))))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
            (SETQ NVAR (COND (SNEGP (OFSF_SOFTNEGP VL EV))))
            (PROG (G450 G451)
              (SETQ G450 (OFSF_ZEROP3 FF FONE D VL EV DIR NVAR))
              (SETQ G451 G450)
              (SETQ FLAG (CAR G450))
              (SETQ G450 (CDR G450))
              (SETQ OTHER (CAR G450))
              (SETQ G450 (CDR G450))
              (RETURN G451))
            (PROG (G452)
              (SETQ G452
                      (COND (*RLTROPILP (OFSF_ZEROSOLVE FF ONE (CAR OTHER)))
                            (T (OFSF_ZEROAPPROX FF ONE (CAR OTHER)))))
              (SETQ ZERO (CAR G452))
              (SETQ FZERO (CDR G452))
              (RETURN G452))
            (COND
             ((AND *RLVERBOSE (NEQ SCOND 'TRUE))
              (PROGN
               (MATHPRINT
                (OFSF_S2APOINTPAIR
                 (LIST
                  (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                    (SETQ PR ZERO)
                    (COND ((NULL PR) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (PR)
                                        (CONS (CAR PR) (MK*SQ (CDR PR))))
                                      (CAR PR))
                                     NIL)))
                   LOOPLABEL
                    (SETQ PR (CDR PR))
                    (COND ((NULL PR) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (PR) (CONS (CAR PR) (MK*SQ (CDR PR))))
                              (CAR PR))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  (MK*SQ FZERO))))
               (IOTO_TPRIN2 "+++ verifying the side condition ... "))))
            (SETQ WW (OFSF_SCEVAL SCOND ZERO))
            (COND ((AND *RLVERBOSE (NEQ SCOND 'TRUE)) (IOTO_PRIN2T WW)))
            (COND ((EQ WW 'TRUE) (SETQ CNT NIL))
                  (T
                   (PROGN
                    (SETQ W 'SKIP)
                    (COND
                     (L
                      (PROGN
                       (COND
                        (*RLVERBOSE
                         (IOTO_TPRIN2
                          (LIST "+++ resuming " (COND (*RLTROPILP "I") (T ""))
                                "LP solving: "))))
                       (LP_NEGCONSTR SKIPROWS)
                       (LP_UPDATEMODEL)
                       (SETQ SKIPROWS (PLUS SKIPROWS 1))))))))))
          (T (PROGN (LP_DELCONSTR SKIPROWS) (LP_UPDATEMODEL)))))
        (GO WHILELABEL))
      (COND ((MEMQ W '(INFEASIBLE UNBOUNDED SKIP)) (RETURN '(-1 NIL NIL))))
      (SETQ ZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ZERO)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (CAR PR) (MK*SQ (CDR PR))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR) (CONS (CAR PR) (MK*SQ (CDR PR))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST FLAG OTHER (LIST ZERO (MK*SQ FZERO)))))) 
(PUT 'OFSF_SMALLP 'NUMBER-OF-ARGS 1) 
(DE OFSF_SMALLP (W)
    (PROG (L)
      (SETQ L
              (PROG (N FORALL-RESULT FORALL-ENDPTR)
                (SETQ N (CDR W))
                (COND ((NULL N) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (N)
                                    (ABS
                                     (COND ((FLOATP N) (FIX N))
                                           (T
                                            (SFTO_SF2INT
                                             (CAR (SFTO_CEILQ (SIMP N))))))))
                                  (CAR N))
                                 NIL)))
               LOOPLABEL
                (SETQ N (CDR N))
                (COND ((NULL N) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (N)
                            (ABS
                             (COND ((FLOATP N) (FIX N))
                                   (T
                                    (SFTO_SF2INT
                                     (CAR (SFTO_CEILQ (SIMP N))))))))
                          (CAR N))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LESSP (LTO_MAX L) 100)))) 
(PUT 'OFSF_SCEVAL 'NUMBER-OF-ARGS 2) 
(DE OFSF_SCEVAL (F SUBL)
    (PROG (N)
      (COND
       (*RLTROPILP
        (SETQ SUBL
                (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                  (SETQ PR SUBL)
                  (COND ((NULL PR) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (PR)
                                      (PROGN
                                       (SETQ N (CAR (CDR PR)))
                                       (COND
                                        ((AND N (NOT (NUMBERP N)))
                                         (CONS (CAR PR) (RA_L N)))
                                        (T PR))))
                                    (CAR PR))
                                   NIL)))
                 LOOPLABEL
                  (SETQ PR (CDR PR))
                  (COND ((NULL PR) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (PR)
                              (PROGN
                               (SETQ N (CAR (CDR PR)))
                               (COND
                                ((AND N (NOT (NUMBERP N)))
                                 (CONS (CAR PR) (RA_L N)))
                                (T PR))))
                            (CAR PR))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN (CL_EVAL F SUBL (FUNCTION OFSF_SCEVALAT))))) 
(PUT 'OFSF_SCEVALAT 'NUMBER-OF-ARGS 2) 
(DE OFSF_SCEVALAT (AT SUBL)
    (COND
     ((OFSF_EVALATP (COND ((ATOM AT) AT) (T (CAR AT)))
       (CAR (SFTO_QSUB (CADR AT) SUBL)))
      'TRUE)
     (T 'FALSE))) 
(PUT 'OFSF_SOFTNEGP 'NUMBER-OF-ARGS 2) 
(DE OFSF_SOFTNEGP (VL EV)
    (COND
     (EV
      (COND ((NOT (EVENP (CAR EV))) (CAR VL))
            (T (OFSF_SOFTNEGP (CDR VL) (CDR EV))))))) 
(PUT 'OFSF_ADDCONSTRAINTS 'NUMBER-OF-ARGS 1) 
(DE OFSF_ADDCONSTRAINTS (L)
    (PROG (PT)
      (SETQ PT L)
     LAB
      (COND ((NULL PT) (RETURN NIL)))
      ((LAMBDA (PT) (LP_ADDCONSTRAINT 'LEQ (CONS (MINUS 1) PT) (MINUS 1)))
       (CAR PT))
      (SETQ PT (CDR PT))
      (GO LAB))) 
(PUT 'OFSF_ZEROP3 'NUMBER-OF-ARGS 7) 
(DE OFSF_ZEROP3 (FF FONE D VL EV DIR NVAR)
    (PROG (FLAG OTHER)
      (PROG (G453 G454)
        (SETQ G453
                (COND
                 ((AND *RLGUROBI *RLTROPILP)
                  (OFSF_ZEROP3I FF FONE D VL EV DIR NVAR))
                 (*RLGUROBI (OFSF_ZEROP3F FF FONE VL EV DIR NVAR))
                 (T (OFSF_ZEROP3R FF FONE D VL EV DIR NVAR))))
        (SETQ G454 G453)
        (SETQ FLAG (CAR G453))
        (SETQ G453 (CDR G453))
        (SETQ OTHER (CAR G453))
        (SETQ G453 (CDR G453))
        (RETURN G454))
      (RETURN (LIST FLAG OTHER)))) 
(PUT 'OFSF_ZEROP3I 'NUMBER-OF-ARGS 7) 
(DE OFSF_ZEROP3I (FF FONE D VL EV DIRP NVAR)
    (PROG (W G)
      (SETQ W 0)
      (SETQ G 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T
          (LIST "+++ found integer direction towards a positive value"))
         (MATHPRINT (OFSF_AL2EQL DIRP))
         (IOTO_TPRIN2T (LIST "+++ at monomial: "))
         (MATHPRINT
          (CONS 'TIMES
                (PROG (V FORALL-RESULT FORALL-ENDPTR)
                  (SETQ V VL)
                  (COND ((NULL V) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (LIST 'EXPT V
                                            (PROG1 (CAR EV)
                                              (SETQ EV (CDR EV)))))
                                    (CAR V))
                                   NIL)))
                 LOOPLABEL
                  (SETQ V (CDR V))
                  (COND ((NULL V) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (V)
                              (LIST 'EXPT V
                                    (PROG1 (CAR EV) (SETQ EV (CDR EV)))))
                            (CAR V))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
         NIL)))
      (SETQ DIRP
              (CONS (CAR DIRP)
                    (PROG (E FORALL-RESULT FORALL-ENDPTR)
                      (SETQ E (CDR DIRP))
                      (COND ((NULL E) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (E)
                                          (PROGN
                                           (SETQ W (FIX (CDR E)))
                                           (SETQ G (GCDN G W))
                                           (CONS (CAR E) W)))
                                        (CAR E))
                                       NIL)))
                     LOOPLABEL
                      (SETQ E (CDR E))
                      (COND ((NULL E) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (E)
                                  (PROGN
                                   (SETQ W (FIX (CDR E)))
                                   (SETQ G (GCDN G W))
                                   (CONS (CAR E) W)))
                                (CAR E))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (PROG (E)
        (SETQ E (CDR DIRP))
       LAB
        (COND ((NULL E) (RETURN NIL)))
        ((LAMBDA (E) (SETCDR E (QUOTIENT (CDR E) G))) (CAR E))
        (SETQ E (CDR E))
        (GO LAB))
      (RETURN (OFSF_ZEROP4I FF FONE D VL DIRP NVAR)))) 
(PUT 'OFSF_ZEROP4I 'NUMBER-OF-ARGS 6) 
(DE OFSF_ZEROP4I (FF FONE D VL DIRP NVAR)
    (PROG (V ISOL FFINFTY SOL FFVAL INF)
      (PROG1 (CAR DIRP) (SETQ DIRP (CDR DIRP)))
      (SETQ ISOL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
                                  (SETQ INF
                                          (SFTO_MKPOWQ 'INFINITY
                                                       (CDR
                                                        (PROG1 (CAR DIRP)
                                                          (SETQ DIRP
                                                                  (CDR
                                                                   DIRP))))))
                                  (CONS V
                                        (COND ((EQ V NVAR) (NEGSQ INF))
                                              (T INF))))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
                          (SETQ INF
                                  (SFTO_MKPOWQ 'INFINITY
                                               (CDR
                                                (PROG1 (CAR DIRP)
                                                  (SETQ DIRP (CDR DIRP))))))
                          (CONS V (COND ((EQ V NVAR) (NEGSQ INF)) (T INF))))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T (LIST "+++ there is a positive value at: "))
         (MATHPRINT
          (OFSF_AL2EQL
           (PROG (PR FORALL-RESULT FORALL-ENDPTR)
             (SETQ PR ISOL)
             (COND ((NULL PR) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (PR) (CONS (CAR PR) (PREPSQ (CDR PR))))
                               (CAR PR))
                              NIL)))
            LOOPLABEL
             (SETQ PR (CDR PR))
             (COND ((NULL PR) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (PR) (CONS (CAR PR) (PREPSQ (CDR PR))))
                       (CAR PR))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))
         (IOTO_TPRIN2
          (LIST "+++ computing " (COND ((GREATERP FONE 0) "-") (T ""))
                "f at that nonstandard point ... ")))))
      (SETQ FFINFTY (SFTO_QSUB FF ISOL))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_PRIN2T "done:")
         (MATHPRINT (PREPSQ FFINFTY))
         (IOTO_TPRIN2
          (LIST "+++ realizing infinity by increasing powers of 2: ")))))
      (PROG (G455)
        (SETQ G455 (OFSF_REALIZEINFINITY ISOL FFINFTY))
        (SETQ SOL (CAR G455))
        (SETQ FFVAL (CDR G455))
        (RETURN G455))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T (LIST "+++ found a point with positive value:"))
         (MATHPRINT (OFSF_S2APOINTPAIR (LIST SOL (MK*SQ FFVAL)))))))
      (COND ((GREATERP FONE 0) (SETQ FFVAL (NEGSQ FFVAL))))
      (RETURN (LIST 1 (LIST SOL (MK*SQ FFVAL)))))) 
(PUT 'OFSF_REALIZEINFINITY 'NUMBER-OF-ARGS 2) 
(DE OFSF_REALIZEINFINITY (ISOL FFINFTY)
    (PROG (NFFINFTY NVAL VAL ISUBL SOL POW)
      (SETQ POW 0)
      (SETQ NFFINFTY (CAR FFINFTY))
      (COND
       ((MINUSF NFFINFTY) (REDERR "negative lc - probably something wrong")))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ POW (COND ((EQN POW 0) 1) (T (TIMES POW 2))))
         (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" POW "] "))))
         (SETQ ISUBL (LIST (CONS 'INFINITY POW)))
         (SETQ NVAL (SFTO_FSUB NFFINFTY ISUBL)))
        (COND ((NOT (AND NVAL (GREATERP NVAL 0))) (GO REPEATLABEL))))
      (SETQ VAL
              (MULTSQ (CONS NVAL 1)
                      (INVSQ (CONS (SFTO_FSUB (CDR FFINFTY) ISUBL) 1))))
      (SETQ SOL
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E ISOL)
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E)
                                    (CONS (CAR E)
                                          (PREPSQ (SFTO_FSUBQ (CDR E) ISUBL))))
                                  (CAR E))
                                 NIL)))
               LOOPLABEL
                (SETQ E (CDR E))
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E)
                            (CONS (CAR E) (PREPSQ (SFTO_FSUBQ (CDR E) ISUBL))))
                          (CAR E))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS SOL VAL)))) 
(PUT 'OFSF_ZEROP3F 'NUMBER-OF-ARGS 6) 
(DE OFSF_ZEROP3F (FF FONE VL EV DIRP NVAR)
    (PROG (SUBL SCVL VAL POW THIS V ISOL)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2
          (LIST "+++ found approximate direction towards a positive value"))
         (MATHPRINT (OFSF_AL2EQL DIRP))
         (IOTO_TPRIN2T (LIST "+++ at monomial: "))
         (MATHPRINT
          (CONS 'TIMES
                (PROG (V FORALL-RESULT FORALL-ENDPTR)
                  (SETQ V VL)
                  (COND ((NULL V) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (LIST 'EXPT V
                                            (PROG1 (CAR EV)
                                              (SETQ EV (CDR EV)))))
                                    (CAR V))
                                   NIL)))
                 LOOPLABEL
                  (SETQ V (CDR V))
                  (COND ((NULL V) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (V)
                              (LIST 'EXPT V
                                    (PROG1 (CAR EV) (SETQ EV (CDR EV)))))
                            (CAR V))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
         (SETQ ISOL
                 (CONS 'LIST
                       (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                         (SETQ PR (CDR DIRP))
                         (COND ((NULL PR) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (PR)
                                             (LIST 'EXPT 'INFINITY (CDR PR)))
                                           (CAR PR))
                                          NIL)))
                        LOOPLABEL
                         (SETQ PR (CDR PR))
                         (COND ((NULL PR) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (PR)
                                     (LIST 'EXPT 'INFINITY (CDR PR)))
                                   (CAR PR))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (IOTO_TPRIN2T
          (LIST "+++ modulo rounding errors there is a positive value at: "))
         (MATHPRINT ISOL)
         (IOTO_TPRIN2T
          (LIST "+++ realizing infinity by increasing powers of 2 ...")))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ POW (COND ((NULL POW) 1.0) (T (TIMES POW 2))))
         (SETQ SCVL VL)
         (SETQ SUBL
                 (PROG (E FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E (CDR DIRP))
                   (COND ((NULL E) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (E)
                                       (PROGN
                                        (SETQ V
                                                (PROG1 (CAR SCVL)
                                                  (SETQ SCVL (CDR SCVL))))
                                        (SETQ THIS (EXPT POW (CDR E)))
                                        (CONS V
                                              (COND ((EQ V NVAR) (MINUS THIS))
                                                    (T THIS)))))
                                     (CAR E))
                                    NIL)))
                  LOOPLABEL
                   (SETQ E (CDR E))
                   (COND ((NULL E) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (E)
                               (PROGN
                                (SETQ V
                                        (PROG1 (CAR SCVL)
                                          (SETQ SCVL (CDR SCVL))))
                                (SETQ THIS (EXPT POW (CDR E)))
                                (CONS V
                                      (COND ((EQ V NVAR) (MINUS THIS))
                                            (T THIS)))))
                             (CAR E))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ VAL (SFTO_FLOATSUB FF SUBL))
         (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" POW ", " VAL "] "))))
         NIL)
        (COND ((NOT (GREATERP VAL 0)) (GO REPEATLABEL))))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T (LIST "+++ found a point with positive value:"))
         (MATHPRINT (OFSF_S2APOINTPAIR (LIST SUBL VAL))))))
      (COND ((GREATERP FONE 0) (SETQ VAL (MINUS VAL))))
      (RETURN (LIST 1 (LIST SUBL VAL))))) 
(PUT 'OFSF_ZEROP3R 'NUMBER-OF-ARGS 7) 
(DE OFSF_ZEROP3R (FF FONE D VL EV DIRP NVAR)
    (PROG (W L)
      (SETQ W 0)
      (SETQ L 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2
          (LIST "+++ found rational direction towards a positive value"))
         (MATHPRINT (OFSF_AL2EQL DIRP))
         (IOTO_TPRIN2T (LIST "+++ at monomial: "))
         (MATHPRINT
          (CONS 'TIMES
                (PROG (V FORALL-RESULT FORALL-ENDPTR)
                  (SETQ V VL)
                  (COND ((NULL V) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (LIST 'EXPT V
                                            (PROG1 (CAR EV)
                                              (SETQ EV (CDR EV)))))
                                    (CAR V))
                                   NIL)))
                 LOOPLABEL
                  (SETQ V (CDR V))
                  (COND ((NULL V) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (V)
                              (LIST 'EXPT V
                                    (PROG1 (CAR EV) (SETQ EV (CDR EV)))))
                            (CAR V))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
         NIL)))
      (SETQ L 1)
      (SETQ DIRP
              (CONS (CAR DIRP)
                    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                      (SETQ PR (CDR DIRP))
                      (COND ((NULL PR) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (PR)
                                          (PROGN
                                           (SETQ W (SIMP (CDR PR)))
                                           (SETQ L (SFTO_LCMN L (CDR W)))
                                           (CONS (CAR PR) W)))
                                        (CAR PR))
                                       NIL)))
                     LOOPLABEL
                      (SETQ PR (CDR PR))
                      (COND ((NULL PR) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (PR)
                                  (PROGN
                                   (SETQ W (SIMP (CDR PR)))
                                   (SETQ L (SFTO_LCMN L (CDR W)))
                                   (CONS (CAR PR) W)))
                                (CAR PR))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ L (CONS L 1))
      (PROG (PR)
        (SETQ PR (CDR DIRP))
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR) (SETCDR PR (SFTO_SF2INT (CAR (MULTSQ (CDR PR) L)))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (RETURN (OFSF_ZEROP4I FF FONE D VL DIRP NVAR)))) 
(PUT 'ZEROSOLVE 'PSOPFN 'OFSF_ZEROSOLVEEVAL) 
(PUT 'OFSF_ZEROSOLVEEVAL 'NUMBER-OF-ARGS 1) 
(DE OFSF_ZEROSOLVEEVAL (L)
    (PROG (F ZRES ZERO GZERO P Q LEN)
      (SETQ LEN 0)
      (SETQ LEN (LENGTH L))
      (COND
       ((NOT (EQN LEN 2))
        (REDERR "usage: zerosolve(<polynomial>, <[p]zero result>)")))
      (SETQ F (CAR (SIMP (PROG1 (CAR L) (SETQ L (CDR L))))))
      (SETQ ZRES (CDR (REVAL1 (PROG1 (CAR L) (SETQ L (CDR L))) T)))
      (COND
       ((NOT (EQCAR ZRES 1))
        (REDERR (LIST "second argument is not of a solvable type"))))
      (SETQ P
              (PROG (EE FORALL-RESULT FORALL-ENDPTR)
                (SETQ EE (CDR (CADR (CADR ZRES))))
                (COND ((NULL EE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE)))
                                  (CAR EE))
                                 NIL)))
               LOOPLABEL
                (SETQ EE (CDR EE))
                (COND ((NULL EE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE))) (CAR EE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q
              (PROG (EE FORALL-RESULT FORALL-ENDPTR)
                (SETQ EE (CDR (CADR (CADDR ZRES))))
                (COND ((NULL EE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE)))
                                  (CAR EE))
                                 NIL)))
               LOOPLABEL
                (SETQ EE (CDR EE))
                (COND ((NULL EE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE))) (CAR EE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (G456)
        (SETQ G456 (OFSF_ZEROSOLVE F P Q))
        (SETQ ZERO (CAR G456))
        (SETQ GZERO (CDR G456))
        (RETURN G456))
      (SETQ ZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ZERO)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (LIST 'EQUAL (CAR PR) (MK*SQ (CDR PR))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR) (LIST 'EQUAL (CAR PR) (MK*SQ (CDR PR))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST 'LIST (CONS 'LIST ZERO) (MK*SQ GZERO))))) 
(PUT 'OFSF_ZEROSOLVE 'NUMBER-OF-ARGS 3) 
(DE OFSF_ZEROSOLVE (G P Q)
    (PROG (Y YQ SUBAL G0 RL RAL X_I P_I Q_I ZERO W VAL PRECQ GZERO FLOATZERO)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T
          (LIST "+++ computing zero, precision is " (PRECISION 0) " ..."))
         (IOTO_TPRIN2T
          (LIST
           "******************************************************************"))
         (IOTO_TPRIN2T
          (LIST
           "* Intervals of algebraic numbers are refined and printed to the  *"))
         (IOTO_TPRIN2T
          (LIST
           "* current precision of the rounded domain. The returned function *"))
         (IOTO_TPRIN2T
          (LIST
           "* value is the value at the lower bounds of the intervals;       *"))
         (IOTO_TPRIN2T
          (LIST
           "* substituion of the actual algebraic numbers into the original  *"))
         (IOTO_TPRIN2T
          (LIST
           "* polynomial is not feasible at present.                         *"))
         (IOTO_TPRIN2T
          (LIST
           "******************************************************************")))))
      (SETQ Y (RA_X))
      (SETQ YQ (CONS (LIST (CONS (CONS Y 1) 1)) 1))
      (SETQ SUBAL
              (PROG (W FORALL-RESULT FORALL-ENDPTR)
                (SETQ W P)
                (COND ((NULL W) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W)
                                    (PROGN
                                     (SETQ X_I (*A2K (CAR W)))
                                     (SETQ P_I (SIMP (CDR W)))
                                     (SETQ Q_I
                                             (SIMP
                                              (CDR
                                               (PROG1 (CAR Q)
                                                 (SETQ Q (CDR Q))))))
                                     (CONS X_I
                                           (ADDSQ P_I
                                                  (MULTSQ YQ
                                                          (ADDSQ Q_I
                                                                 (NEGSQ
                                                                  P_I)))))))
                                  (CAR W))
                                 NIL)))
               LOOPLABEL
                (SETQ W (CDR W))
                (COND ((NULL W) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (W)
                            (PROGN
                             (SETQ X_I (*A2K (CAR W)))
                             (SETQ P_I (SIMP (CDR W)))
                             (SETQ Q_I
                                     (SIMP
                                      (CDR (PROG1 (CAR Q) (SETQ Q (CDR Q))))))
                             (CONS X_I
                                   (ADDSQ P_I
                                          (MULTSQ YQ
                                                  (ADDSQ Q_I (NEGSQ P_I)))))))
                          (CAR W))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ G0 (SFTO_QSUB G SUBAL))
      (COND (NIL NIL))
      (ON1 'RANUM)
      (SETQ RL (RA_ISOLATE (CAR G0) (CONS NIL 1) (CONS 1 1)))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+++ found " (LENGTH RL) " zero" (COND ((CDR RL) "s") (T ""))
               " on the relevant line segment"))))
      (COND
       ((NULL RL)
        (REDERR
         (LIST "severe error - no zero on the relevant line segement"))))
      (SETQ RAL (LIST (CONS Y (CAR RL))))
      (SETQ PRECQ (CONS 1 (EXPT 10 (PLUS (PRECISION 0) 2))))
      (SETQ ZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR SUBAL)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (PROGN
                                     (SETQ W
                                             (MULTSQ
                                              (CONS
                                               (SFTO_FSUB (CAR (CDR PR)) RAL)
                                               1)
                                              (INVSQ (CONS (CDR (CDR PR)) 1))))
                                     (COND (NIL NIL))
                                     (SETQ VAL (CAR W))
                                     (COND
                                      ((AND VAL (NOT (NUMBERP VAL)))
                                       (PROG ()
                                        WHILELABEL
                                         (COND
                                          ((NOT
                                            (SFTO_GEQQ
                                             (ADDSQ (RA_U VAL)
                                                    (NEGSQ (RA_L VAL)))
                                             PRECQ))
                                           (RETURN NIL)))
                                         (SETQ VAL (RA_REFINE VAL 1))
                                         (GO WHILELABEL))))
                                     (CONS (CAR PR) (CONS VAL 1))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (PROGN
                             (SETQ W
                                     (MULTSQ
                                      (CONS (SFTO_FSUB (CAR (CDR PR)) RAL) 1)
                                      (INVSQ (CONS (CDR (CDR PR)) 1))))
                             (COND (NIL NIL))
                             (SETQ VAL (CAR W))
                             (COND
                              ((AND VAL (NOT (NUMBERP VAL)))
                               (PROG ()
                                WHILELABEL
                                 (COND
                                  ((NOT
                                    (SFTO_GEQQ
                                     (ADDSQ (RA_U VAL) (NEGSQ (RA_L VAL)))
                                     PRECQ))
                                   (RETURN NIL)))
                                 (SETQ VAL (RA_REFINE VAL 1))
                                 (GO WHILELABEL))))
                             (CONS (CAR PR) (CONS VAL 1))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (OFF1 'RANUM)
      (ON1 'ROUNDED)
      (SETQ FLOATZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ZERO)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (CAR PR)
                                          (CAR
                                           (RESIMP
                                            (COND
                                             ((OR (NULL (CAR (CDR PR)))
                                                  (NUMBERP (CAR (CDR PR))))
                                              (CDR PR))
                                             (T (RA_L (CAR (CDR PR)))))))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (CONS (CAR PR)
                                  (CAR
                                   (RESIMP
                                    (COND
                                     ((OR (NULL (CAR (CDR PR)))
                                          (NUMBERP (CAR (CDR PR))))
                                      (CDR PR))
                                     (T (RA_L (CAR (CDR PR)))))))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GZERO (CONS (SFTO_FSUB G FLOATZERO) 1))
      (OFF (LIST 'ROUNDED))
      (RETURN (CONS ZERO GZERO)))) 
(PUT 'ZEROAPPROX 'PSOPFN 'OFSF_ZEROAPPROXEVAL) 
(PUT 'OFSF_ZEROAPPROXEVAL 'NUMBER-OF-ARGS 1) 
(DE OFSF_ZEROAPPROXEVAL (L)
    (PROG (F ZRES ZERO GZERO P Q LEN)
      (SETQ LEN 0)
      (SETQ LEN (LENGTH L))
      (COND
       ((NOT (EQN LEN 2))
        (REDERR "usage: zeroapprox(<polynomial>, <[p]zero result>)")))
      (SETQ F (CAR (SIMP (PROG1 (CAR L) (SETQ L (CDR L))))))
      (SETQ ZRES (CDR (REVAL1 (PROG1 (CAR L) (SETQ L (CDR L))) T)))
      (COND
       ((NOT (EQCAR ZRES 1))
        (REDERR (LIST "second argument is not of a solvable type"))))
      (SETQ P
              (PROG (EE FORALL-RESULT FORALL-ENDPTR)
                (SETQ EE (CDR (CADR (CADR ZRES))))
                (COND ((NULL EE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE)))
                                  (CAR EE))
                                 NIL)))
               LOOPLABEL
                (SETQ EE (CDR EE))
                (COND ((NULL EE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE))) (CAR EE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q
              (PROG (EE FORALL-RESULT FORALL-ENDPTR)
                (SETQ EE (CDR (CADR (CADDR ZRES))))
                (COND ((NULL EE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE)))
                                  (CAR EE))
                                 NIL)))
               LOOPLABEL
                (SETQ EE (CDR EE))
                (COND ((NULL EE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EE) (CONS (CADR EE) (CADDR EE))) (CAR EE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (G457)
        (SETQ G457 (OFSF_ZEROAPPROX F P Q))
        (SETQ ZERO (CAR G457))
        (SETQ GZERO (CDR G457))
        (RETURN G457))
      (SETQ ZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ZERO)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (LIST 'EQUAL (CAR PR) (MK*SQ (CDR PR))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR) (LIST 'EQUAL (CAR PR) (MK*SQ (CDR PR))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST 'LIST (CONS 'LIST ZERO) (MK*SQ GZERO))))) 
(PUT 'OFSF_ZEROAPPROX 'NUMBER-OF-ARGS 3) 
(DE OFSF_ZEROAPPROX (G P Q)
    (PROG (SUBAL X_I P_I Q_I G0 Y YQ W RL RAL ZERO GZERO)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+++ approximating zero, float precision is " (PRECISION 0)
               " ..."))))
      (SETQ Y (INTERN (GENSYM)))
      (SETQ YQ (CONS (LIST (CONS (CONS Y 1) 1)) 1))
      (SETQ SUBAL
              (PROG (W FORALL-RESULT FORALL-ENDPTR)
                (SETQ W P)
                (COND ((NULL W) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W)
                                    (PROGN
                                     (SETQ X_I (*A2K (CAR W)))
                                     (SETQ P_I (SIMP (CDR W)))
                                     (SETQ Q_I
                                             (SIMP
                                              (CDR
                                               (PROG1 (CAR Q)
                                                 (SETQ Q (CDR Q))))))
                                     (CONS X_I
                                           (ADDSQ P_I
                                                  (MULTSQ YQ
                                                          (ADDSQ Q_I
                                                                 (NEGSQ
                                                                  P_I)))))))
                                  (CAR W))
                                 NIL)))
               LOOPLABEL
                (SETQ W (CDR W))
                (COND ((NULL W) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (W)
                            (PROGN
                             (SETQ X_I (*A2K (CAR W)))
                             (SETQ P_I (SIMP (CDR W)))
                             (SETQ Q_I
                                     (SIMP
                                      (CDR (PROG1 (CAR Q) (SETQ Q (CDR Q))))))
                             (CONS X_I
                                   (ADDSQ P_I
                                          (MULTSQ YQ
                                                  (ADDSQ Q_I (NEGSQ P_I)))))))
                          (CAR W))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ G0 (SFTO_QSUB G SUBAL))
      (COND (NIL NIL))
      (SETQ RL (OFSF_REALROOTSWRAP1 (CAR G0)))
      (COND
       ((NULL RL)
        (REDERR
         (LIST "severe error - no zero on the relevant line segement"))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+++ found " (LENGTH RL) " zero" (COND ((CDR RL) "s") (T ""))
               " on the relevant line segment"))))
      (SETQ RAL (LIST (CAR RL)))
      (ON1 'ROUNDED)
      (SETQ ZERO
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR SUBAL)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (CAR PR)
                                          (MULTSQ
                                           (CONS (SFTO_FSUB (CAR (CDR PR)) RAL)
                                                 1)
                                           (INVSQ (CONS (CDR (CDR PR)) 1)))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (CONS (CAR PR)
                                  (MULTSQ
                                   (CONS (SFTO_FSUB (CAR (CDR PR)) RAL) 1)
                                   (INVSQ (CONS (CDR (CDR PR)) 1)))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GZERO (SFTO_QSUB G ZERO))
      (OFF1 'ROUNDED)
      (RETURN (CONS ZERO GZERO)))) 
(PUT 'OFSF_REALROOTSWRAP 'NUMBER-OF-ARGS 1) 
(DE OFSF_REALROOTSWRAP (F)
    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
      (SETQ PR (OFSF_REALROOTSWRAP1 F))
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PR)
                          (COND
                           ((EQCAR (CDR PR) '|:RD:|)
                            (CONS (CAR PR) (PREPF (*RD2RN (CDR PR)))))
                           (T PR)))
                        (CAR PR))
                       NIL)))
     LOOPLABEL
      (SETQ PR (CDR PR))
      (COND ((NULL PR) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (PR)
                  (COND
                   ((EQCAR (CDR PR) '|:RD:|)
                    (CONS (CAR PR) (PREPF (*RD2RN (CDR PR)))))
                   (T PR)))
                (CAR PR))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_REALROOTSWRAP1 'NUMBER-OF-ARGS 1) 
(DE OFSF_REALROOTSWRAP1 (F)
    (PROG (E FORALL-RESULT FORALL-ENDPTR)
      (SETQ E (CDR (REALROOTS (LIST (PREPF F) 0 1))))
      (COND ((NULL E) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (E) (CONS (CADR E) (CAR (SIMP (CADDR E)))))
                        (CAR E))
                       NIL)))
     LOOPLABEL
      (SETQ E (CDR E))
      (COND ((NULL E) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (E) (CONS (CADR E) (CAR (SIMP (CADDR E))))) (CAR E))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'TESTPZEROP 'NUMBER-OF-ARGS 2) 
(FLAG '(TESTPZEROP) 'OPFN) 
(PUT 'TESTPZEROP 'DEFINED-ON-LINE '786) 
(PUT 'TESTPZEROP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFTROP.RED) 
(PUT 'TESTPZEROP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTPZEROP (HU SOL)
    (PROG (W SUBSOL FOUNDSOL OK)
      (SETQ OK (AEVAL 1))
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH SOL)) I))
          (RETURN NIL)))
        (PROGN
         (SETQ W (AEVAL* (LIST 'PART SOL I)))
         (COND
          ((AND (EVALNEQ (AEVAL* W) (AEVAL* 'PD))
                (EVALNEQ (AEVAL* W) (AEVAL* 'ND))
                (EVALNEQ (AEVAL* W) (AEVAL* 'FAILED)))
           (PROGN
            (SETQ SUBSOL (AEVAL* (LIST 'SUB (LIST 'PART W 1) HU)))
            (SETQ SUBSOL (AEVAL* (LIST 'EVALF SUBSOL)))
            (SETQ FOUNDSOL (AEVAL* (LIST 'PART W 2)))
            (SETQ W (AEVAL* (LIST 'EVALF (LIST 'DIFFERENCE FOUNDSOL SUBSOL))))
            (COND
             ((EVALNEQ (AEVAL* W) 0)
              (PROGN
               (AEVAL*
                (LPRIM
                 (LIST "solution" (DIFFERENCE I 1)
                       "is not correct: difference is" W)))
               (SETQ OK (AEVAL* 0)))))))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL OK)))) 
(ENDMODULE) 