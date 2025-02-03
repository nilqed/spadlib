(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'IBALPKAPUR)) 
(REVISION 'IBALPKAPUR
          "$Id: ibalpkapur.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'IBALPKAPUR "(c) 2007-2009 A. Dolzmann, T. Sturm, 2017 T. Sturm") 
(FLUID '(IBALP_KAPUROPTIONS* *IBALP_KAPURGB *RLKAPURCHKTAUT *RLKAPURCHKCONT)) 
(SWITCH (LIST 'IBALP_KAPURGB 'RLKAPURCHKTAUT 'RLKAPURCHKCONT)) 
(FLUID '(*IBALP_KAPURGBDEGD *IBALP_KAPURDISABLEGB)) 
(SWITCH (LIST 'IBALP_KAPURGBDEGD 'IBALP_KAPURDISABLEGB)) 
(FLUID '(*RLVERBOSE *MODULAR VDPSORTMODE*)) 
(PUT 'IBALP_SETKAPUROPTION 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_SETKAPUROPTION 'DEFINED-ON-LINE '43) 
(PUT 'IBALP_SETKAPUROPTION 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_SETKAPUROPTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_SETKAPUROPTION (OPT VAL)
    (PROG (OLDOPT OLDVAL)
      (COND
       ((SETQ OLDOPT (ATSOC OPT IBALP_KAPUROPTIONS*))
        (PROGN (SETQ OLDVAL (CDR OLDOPT)) (SETCDR OLDOPT VAL)))
       (T
        (SETQ IBALP_KAPUROPTIONS* (CONS (CONS OPT VAL) IBALP_KAPUROPTIONS*))))
      (RETURN OLDVAL))) 
(PUT 'IBALP_GETKAPUROPTION 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GETKAPUROPTION 'DEFINED-ON-LINE '56) 
(PUT 'IBALP_GETKAPUROPTION 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GETKAPUROPTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GETKAPUROPTION (OPT) (LTO_CATSOC OPT IBALP_KAPUROPTIONS*)) 
(PUT 'IBALP_INITKAPUROPTIONS 'NUMBER-OF-ARGS 0) 
(PUT 'IBALP_INITKAPUROPTIONS 'DEFINED-ON-LINE '60) 
(PUT 'IBALP_INITKAPUROPTIONS 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_INITKAPUROPTIONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE IBALP_INITKAPUROPTIONS NIL
    (PROGN
     (SETQ IBALP_KAPUROPTIONS*
             (LIST (CONS 'TORDER VDPSORTMODE*) (CONS 'POLYGENMODE 'KAPUR)))
     (COND
      ((AND *RLKAPURCHKTAUT *RLKAPURCHKCONT)
       (IBALP_SETKAPUROPTION 'CHECKMODE 'FULL))
      (*RLKAPURCHKTAUT (IBALP_SETKAPUROPTION 'CHECKMODE 'TAUT))
      (*RLKAPURCHKCONT (IBALP_SETKAPUROPTION 'CHECKMODE 'CONT))
      (T (IBALP_SETKAPUROPTION 'CHECKMODE 'SAT))))) 
(PUT 'IBALP_KAPUR 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_KAPUR 'DEFINED-ON-LINE '76) 
(PUT 'IBALP_KAPUR 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_KAPUR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_KAPUR (F UMODE)
    (PROG (OLDMOD OLDSWITCH NEWF)
      (SETQ OLDMOD (SETMOD 2))
      (SETQ OLDSWITCH *MODULAR)
      (ON1 'MODULAR)
      (IBALP_INITKAPUROPTIONS)
      (IBALP_SETKAPUROPTION 'POLYGENMODE UMODE)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T "++++ Starting ibalp_kapur")
         (IOTO_PRIN2T
          (LIST "Polynomial generation method: "
                (IBALP_GETKAPUROPTION 'POLYGENMODE)))
         (IOTO_TPRIN2T "-------------------------"))))
      (SETQ F (CL_SIMPL F NIL (MINUS 1)))
      (COND
       ((MEMQ (IBALP_GETKAPUROPTION 'CHECKMODE) '(TAUT FULL))
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2T "---- Check for tautology")))
         (SETQ NEWF (IBALP_REGFORMULA (IBALP_KAPUR1 F 0) 0 F)))))
      (COND
       ((AND (MEMQ (IBALP_GETKAPUROPTION 'CHECKMODE) '(CONT FULL SAT))
             (NOT
              ((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
               (COND ((ATOM NEWF) NEWF) (T (CAR NEWF))))))
        (PROGN
         (COND (*RLVERBOSE (IOTO_PRIN2T "---- Check for contradiction")))
         (SETQ NEWF (IBALP_REGFORMULA (IBALP_KAPUR1 F 1) 1 F)))))
      (SETMOD OLDMOD)
      (COND ((NULL OLDSWITCH) (OFF1 'MODULAR)))
      (RETURN NEWF))) 
(PUT 'IBALP_REGFORMULA 'NUMBER-OF-ARGS 3) 
(PUT 'IBALP_REGFORMULA 'DEFINED-ON-LINE '106) 
(PUT 'IBALP_REGFORMULA 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_REGFORMULA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE IBALP_REGFORMULA (PL TRTHVAL ORIGF)
    (COND ((EQN TRTHVAL 0) (COND ((MEMBER 1 PL) 'TRUE) (T ORIGF)))
          ((MEMBER 1 PL) 'FALSE)
          ((EQ (IBALP_GETKAPUROPTION 'CHECKMODE) 'SAT) 'TRUE) (T ORIGF))) 
(PUT 'IBALP_KAPUR1 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_KAPUR1 'DEFINED-ON-LINE '122) 
(PUT 'IBALP_KAPUR1 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_KAPUR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_KAPUR1 (F TRTHVAL)
    (PROG (POLYLIST)
      (COND ((GREATERP (RL_QNUM F) 0) (SETQ F (CL_QE F NIL))))
      (COND (*RLVERBOSE (IOTO_PRIN2T "--- Generate polynomials...")))
      (SETQ POLYLIST (IBALP_POLYSET F TRTHVAL))
      (SETQ POLYLIST (NCONC POLYLIST (IBALP_GENIDEMPPOLYLIST POLYLIST)))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_PRIN2T (LIST "-- Generated " (LENGTH POLYLIST) " polynomials"))
         (IOTO_PRIN2T
          (LIST "--- Compute Groebner Basis (" VDPSORTMODE* ")...")))))
      (SETQ POLYLIST (IBALP_GROEBNEREVAL POLYLIST))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_PRIN2T
          (LIST "-- Generated " (LENGTH POLYLIST) " polynomials")))))
      (RETURN POLYLIST))) 
(PUT 'IBALP_POLYSET 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_POLYSET 'DEFINED-ON-LINE '142) 
(PUT 'IBALP_POLYSET 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_POLYSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_POLYSET (F TRTHVAL)
    (COND
     ((EQ (IBALP_GETKAPUROPTION 'POLYGENMODE) 'KNF) (IBALP_PSET3KNF F TRTHVAL))
     ((EQ (IBALP_GETKAPUROPTION 'POLYGENMODE) 'DIRECT)
      (IBALP_PSETDIREKT F TRTHVAL))
     ((MEMQ (IBALP_GETKAPUROPTION 'POLYGENMODE) '(KAPUR KAPURKNF))
      (IBALP_PSETKAPUR F TRTHVAL))
     (T (IBALP_PSETKAPUR F TRTHVAL)))) 
(PUT 'IBALP_FORMULAFORM 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_FORMULAFORM 'DEFINED-ON-LINE '154) 
(PUT 'IBALP_FORMULAFORM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_FORMULAFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_FORMULAFORM (P)
    (COND ((EQN P 1) 'TRUE) ((EQN P 0) 'FALSE) ((IDP P) (LIST 'EQUAL P 1))
          ((EQCAR P 'TIMES)
           ((LAMBDA (G127)
              (COND ((AND G127 (CDR G127)) (CONS 'AND G127))
                    ((NULL G127) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G127))))
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (CDR P))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X) (IBALP_FORMULAFORM X)) (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (IBALP_FORMULAFORM X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
          ((EQCAR P 'PLUS)
           (LIST 'NOT
                 (LIST 'EQUIV (IBALP_FORMULAFORM (CADR P))
                       (IBALP_FORMULAFORM
                        (KPOLY_NORM (CONS 'PLUS (CDDR P))))))))) 
(PUT 'IBALP_POLYFORM 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_POLYFORM 'DEFINED-ON-LINE '169) 
(PUT 'IBALP_POLYFORM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_POLYFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_POLYFORM (F)
    (PROG (A B)
      (COND
       (((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
         (COND ((ATOM F) F) (T (CAR F))))
        (RETURN (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'TRUE) 1) (T 0)))))
      (COND ((MEMQ (CAR F) '(EQUAL NEQ)) (RETURN (IBALP_POLYFORMATF F))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT)
        (RETURN (KPOLY_PLUS (LIST 1 (IBALP_POLYFORM (CADR F)))))))
      (COND
       (((LAMBDA (X) (OR (EQ X 'OR) (EQ X 'AND)))
         (COND ((ATOM F) F) (T (CAR F))))
        (PROGN
         (COND
          ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
           (RETURN (KPOLY_TIMES (IBALP_POLYFORMLIST (CDR F))))))
         (RETURN
          (KPOLY_PLUS
           (LIST 1
                 (KPOLY_TIMES
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J (IBALP_POLYFORMLIST (CDR F)))
                    (COND ((NULL J) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (J) (KPOLY_PLUS (LIST 1 J)))
                                      (CAR J))
                                     NIL)))
                   LOOPLABEL
                    (SETQ J (CDR J))
                    (COND ((NULL J) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (J) (KPOLY_PLUS (LIST 1 J))) (CAR J))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))))
      (SETQ A (IBALP_POLYFORM (CADR F)))
      (SETQ B (IBALP_POLYFORM (CADDR F)))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'IMPL)
        (RETURN
         (KPOLY_PLUS
          (LIST 1 (KPOLY_TIMES (LIST A B)) (IBALP_CLONESTRUCT A))))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'REPL)
        (RETURN
         (KPOLY_PLUS
          (LIST 1 (KPOLY_TIMES (LIST A B)) (IBALP_CLONESTRUCT B))))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'EQUIV)
        (RETURN (KPOLY_PLUS (LIST 1 A B)))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'XOR)
        (RETURN (KPOLY_PLUS (LIST A B))))))) 
(PUT 'IBALP_POLYFORMATF 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_POLYFORMATF 'DEFINED-ON-LINE '197) 
(PUT 'IBALP_POLYFORMATF 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_POLYFORMATF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_POLYFORMATF (F)
    (COND
     ((EQ (CAR F) 'EQUAL)
      (COND ((EQN (CADDR F) 1) (CADR F)) (T (KPOLY_PLUS (LIST 1 (CADR F))))))
     ((EQN (CADDR F) 0) (CADR F)) (T (KPOLY_PLUS (LIST 1 (CADR F)))))) 
(PUT 'IBALP_REMNESTED 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_REMNESTED 'DEFINED-ON-LINE '211) 
(PUT 'IBALP_REMNESTED 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_REMNESTED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_REMNESTED (PL OP)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J PL)
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J)
                 (COND ((EQCAR J OP) (IBALP_REMNESTED (CDR J) OP))
                       (T (LIST J))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J)
                 (COND ((EQCAR J OP) (IBALP_REMNESTED (CDR J) OP))
                       (T (LIST J))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'IBALP_POLYFORMLIST 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_POLYFORMLIST 'DEFINED-ON-LINE '221) 
(PUT 'IBALP_POLYFORMLIST 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_POLYFORMLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_POLYFORMLIST (L)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (IBALP_POLYFORM X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (X) (IBALP_POLYFORM X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'IBALP_GROEBNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GROEBNEREVAL 'DEFINED-ON-LINE '226) 
(PUT 'IBALP_GROEBNEREVAL 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GROEBNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GROEBNEREVAL (PL)
    (COND ((NULL PL) (LIST 0)) (*IBALP_KAPURDISABLEGB PL)
          (*IBALP_KAPURGBDEGD (IBALP_GBDEGD PL 20))
          (*IBALP_KAPURGB (IBALP_GB PL))
          (T (CDR (GROEBNEREVAL (LIST (CONS 'LIST PL))))))) 
(PUT 'IBALP_TORDERP 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_TORDERP 'DEFINED-ON-LINE '240) 
(PUT 'IBALP_TORDERP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_TORDERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_TORDERP (A B)
    (COND ((EQN B 0) T) ((EQN A 0) NIL) ((EQN B 1) T) ((EQN A 1) NIL)
          ((EQUAL A B) T)
          ((EQ (IBALP_GETKAPUROPTION 'TORDER) 'LEX) (IBALP_TORDERLEXP A B))
          ((EQ (IBALP_GETKAPUROPTION 'TORDER) 'GRADLEX)
           (IBALP_TORDERGRADLEXP A B))
          (T (IBALP_TORDERGRADLEXP A B)))) 
(PUT 'IBALP_TORDERLEXP 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_TORDERLEXP 'DEFINED-ON-LINE '259) 
(PUT 'IBALP_TORDERLEXP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_TORDERLEXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_TORDERLEXP (A B)
    (COND ((AND (ATOM A) (ATOM B)) (ORDOP A B))
          ((AND (ATOM A) (PAIRP B))
           (COND ((EQ A (CADR B)) NIL) (T (ORDOP A (CADR B)))))
          ((AND (PAIRP A) (ATOM B)) (ORDOP (CADR A) B))
          ((AND (PAIRP A) (PAIRP B) (CDR A) (CDR B))
           (COND
            ((EQ (CADR A) (CADR B))
             (COND
              ((AND (CDDR A) (CDDR B))
               (IBALP_TORDERP (CONS 'TIMES (CDDR A)) (CONS 'TIMES (CDDR B))))
              ((CDDR A) T) ((CDDR B) NIL) (T T)))
            (T (ORDOP (CADR A) (CADR B)))))
          ((CDR A) T) (T NIL))) 
(PUT 'IBALP_TORDERGRADLEXP 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_TORDERGRADLEXP 'DEFINED-ON-LINE '283) 
(PUT 'IBALP_TORDERGRADLEXP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_TORDERGRADLEXP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_TORDERGRADLEXP (A B)
    (COND ((AND (ATOM A) (ATOM B)) (ORDOP A B)) ((AND (ATOM A) (PAIRP B)) NIL)
          ((AND (ATOM B) (PAIRP A)) T) ((GREATERP (LENGTH A) (LENGTH B)) T)
          ((LESSP (LENGTH A) (LENGTH B)) NIL)
          ((AND (PAIRP A) (PAIRP B) (CDR A) (CDR B))
           (COND
            ((EQ (CADR A) (CADR B))
             (COND
              ((AND (CDDR A) (CDDR B))
               (IBALP_TORDERGRADLEXP (CONS 'TIMES (CDDR A))
                (CONS 'TIMES (CDDR B))))
              ((CDDR A) T) ((CDDR B) NIL) (T T)))
            (T (ORDOP (CADR A) (CADR B))))))) 
(PUT 'IBALP_GBDEGD 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GBDEGD 'DEFINED-ON-LINE '309) 
(PUT 'IBALP_GBDEGD 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBDEGD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBDEGD (PL MAXDEG)
    (PROG (GLIST GLISTEND SLIST POL NEWRULE SRULE)
      (SETQ GLIST (LIST (KRULE_POLY2RULE (CAR PL))))
      (SETQ GLISTEND GLIST)
      (SETQ SLIST (CDR PL))
      (PROG ()
       WHILELABEL
        (COND ((NOT SLIST) (RETURN NIL)))
        (PROGN
         (SETQ POL (CAR SLIST))
         (SETQ SLIST (CDR SLIST))
         (SETQ POL (IBALP_GBREDUCEPOLY POL GLIST))
         (COND
          ((NOT (EQN POL 0))
           (PROGN
            (SETQ NEWRULE (KRULE_POLY2RULE POL))
            (SETCDR GLISTEND (CONS NEWRULE NIL))
            (SETQ GLISTEND (CDR GLISTEND))
            (PROG (J)
              (SETQ J GLIST)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (SETQ SRULE (IBALP_GBOVERLAPRULES J NEWRULE NIL))
                  (COND
                   ((OR
                     (AND (ATOM (CAR SRULE))
                          (NOT (OR (EQN (CAR SRULE) 0) (EQN (CAR SRULE) 1))))
                     (AND (LISTP (CAR SRULE))
                          (LESSP (LENGTH (CDAR SRULE)) (PLUS MAXDEG 1))))
                    (SETQ SLIST (CONS (KRULE_RULE2POLY SRULE) SLIST))))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))))))
        (GO WHILELABEL))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J GLIST)
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (KRULE_RULE2POLY J)) (CAR J))
                               NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (J) (KRULE_RULE2POLY J)) (CAR J)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'IBALP_GB 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GB 'DEFINED-ON-LINE '338) 
(PUT 'IBALP_GB 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GB (PL)
    (PROG (ALLRULES NEWRULES NEWRULE NEWRULES2)
      (COND ((NULL PL) (RETURN '(0))))
      (COND ((NULL (CDR PL)) (RETURN PL)))
      (SETQ ALLRULES (IBALP_GBINITRULES PL))
      (SETQ NEWRULES (CDR ALLRULES))
      (PROG ()
       WHILELABEL
        (COND ((NOT NEWRULES) (RETURN NIL)))
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T (LIST "- " (LENGTH NEWRULES) " new rules"))))
         (SETQ NEWRULES2 NEWRULES)
         (SETQ NEWRULES NIL)
         (PROG (J)
           (SETQ J NEWRULES2)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (PROG (K)
                (SETQ K ALLRULES)
               LAB
                (COND ((NULL K) (RETURN NIL)))
                ((LAMBDA (K)
                   (PROGN
                    (SETQ NEWRULE
                            (IBALP_GBOVERLAPRULES J K
                             (APPEND ALLRULES NEWRULES)))
                    (COND
                     ((EQUAL NEWRULE '(1 . 0))
                      (PROGN
                       (COND
                        (*RLVERBOSE (IOTO_TPRIN2T "-- 1 in GB generation")))
                       (SETQ ALLRULES '((1 . 0)))
                       (SETQ NEWRULES NIL)))
                     ((AND (EQN (CDR NEWRULE) 1) (EQCAR (CAR NEWRULE) 'TIMES))
                      (SETQ NEWRULES
                              (NCONC
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K (CDAR NEWRULE))
                                 (COND ((NULL K) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (K) (CONS K 1))
                                                   (CAR K))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ K (CDR K))
                                 (COND ((NULL K) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K) (CONS K 1)) (CAR K))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))
                               NEWRULES)))
                     ((NEQ NEWRULE '(0 . 0))
                      (PROGN
                       (COND
                        (*RLVERBOSE
                         (IOTO_TPRIN2T
                          (LIST (CAR NEWRULE) " -> " (CDR NEWRULE)))))
                       (SETQ NEWRULES (CONS NEWRULE NEWRULES)))))))
                 (CAR K))
                (SETQ K (CDR K))
                (GO LAB)))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (COND
          (NEWRULES
           (PROGN
            (SETQ NEWRULES (IBALP_GBSIMPLIFYALL NEWRULES))
            (SETQ ALLRULES
                    (IBALP_GBSIMPLIFYALL (APPEND ALLRULES NEWRULES)))))))
        (GO WHILELABEL))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J ALLRULES)
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (KRULE_RULE2POLY J)) (CAR J))
                               NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (J) (KRULE_RULE2POLY J)) (CAR J)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'IBALP_GBSIMPLIFYALL 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GBSIMPLIFYALL 'DEFINED-ON-LINE '373) 
(PUT 'IBALP_GBSIMPLIFYALL 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBSIMPLIFYALL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GBSIMPLIFYALL (RULES)
    (PROG (CURRULE BEFORR AFTERR NEWP CURLENGTH CNTR)
      (SETQ CURLENGTH 0)
      (SETQ CNTR 0)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T (LIST "-- Simplifing " (LENGTH RULES) " Rules"))))
      (COND ((NULL (CDR RULES)) (RETURN RULES)))
      (SETQ CURLENGTH 1)
      (SETQ CNTR 0)
      (SETQ CURRULE RULES)
      (SETQ BEFORR RULES)
      (SETQ AFTERR (CDR RULES))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR BEFORR)) (RETURN NIL)))
        (PROGN (SETQ CURLENGTH (ADD1 CURLENGTH)) (SETQ BEFORR (CDR BEFORR)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP CNTR CURLENGTH)) (RETURN NIL)))
        (PROGN
         (SETQ NEWP
                 (IBALP_GBREDUCEPOLY (KRULE_RULE2POLY (CAR CURRULE)) AFTERR))
         (COND
          ((EQN NEWP 1)
           (PROGN
            (SETQ CURRULE (LIST (KRULE_POLY2RULE 1)))
            (SETQ CNTR (PLUS CURLENGTH 25))))
          ((EQN NEWP 0)
           (PROGN
            (SETQ CURLENGTH (SUB1 CURLENGTH))
            (SETQ CURRULE AFTERR)
            (SETQ AFTERR (CDR AFTERR))))
          (T
           (PROGN
            (SETCDR BEFORR (CONS (KRULE_POLY2RULE NEWP) NIL))
            (SETQ CNTR
                    (COND ((EQUAL (CADR BEFORR) (CAR CURRULE)) (ADD1 CNTR))
                          (T 0)))
            (SETQ BEFORR (CDR BEFORR))
            (SETQ CURRULE AFTERR)
            (SETQ AFTERR (CDR AFTERR))))))
        (GO WHILELABEL))
      (RETURN CURRULE))) 
(PUT 'IBALP_GBOVERLAPRULES 'NUMBER-OF-ARGS 3) 
(PUT 'IBALP_GBOVERLAPRULES 'DEFINED-ON-LINE '409) 
(PUT 'IBALP_GBOVERLAPRULES 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBOVERLAPRULES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBOVERLAPRULES (R1 R2 RLIST)
    (PROG (SPOLY HEAD1 TAIL1 HEAD2 TAIL2)
      (COND ((IBALP_GBOVERLAPRULESZCRITP R1 R2) (RETURN (KRULE_POLY2RULE 0))))
      (SETQ HEAD1 (KRULE_HEAD R1))
      (SETQ HEAD2 (KRULE_HEAD R2))
      (SETQ TAIL1 (KRULE_TAIL R1))
      (SETQ TAIL2 (KRULE_TAIL R2))
      (SETQ SPOLY
              (COND ((EQUAL HEAD1 HEAD2) (KPOLY_PLUS (LIST TAIL1 TAIL2)))
                    ((AND (ATOM HEAD1) (PAIRP HEAD2) (MEMQ HEAD1 (CDR HEAD2)))
                     (KPOLY_PLUS
                      (LIST TAIL2
                            (KPOLY_TIMES (LIST TAIL1 (DELETE HEAD1 HEAD2))))))
                    ((AND (ATOM HEAD2) (PAIRP HEAD1) (MEMQ HEAD2 (CDR HEAD1)))
                     (KPOLY_PLUS
                      (LIST TAIL1
                            (KPOLY_TIMES (LIST TAIL2 (DELETE HEAD2 HEAD1))))))
                    (T
                     (PROGN
                      (SETQ SPOLY
                              (KPOLY_TIMES (UNION (CDR HEAD1) (CDR HEAD2))))
                      (KPOLY_PLUS
                       (LIST (IBALP_GBAPPLYRULE SPOLY R1)
                             (IBALP_GBAPPLYRULE SPOLY R2)))))))
      (RETURN (KRULE_POLY2RULE (IBALP_GBREDUCEPOLY SPOLY RLIST))))) 
(PUT 'IBALP_GBOVERLAPRULESZCRITP 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GBOVERLAPRULESZCRITP 'DEFINED-ON-LINE '434) 
(PUT 'IBALP_GBOVERLAPRULESZCRITP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBOVERLAPRULESZCRITP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBOVERLAPRULESZCRITP (R1 R2)
    (OR (EQUAL R1 R2)
        (AND (ATOM (CAR R1)) (ATOM (CAR R2)) (NOT (EQCAR R1 (CAR R2))))
        (AND (EQN (CDR R1) 0) (EQN (CDR R2) 0))
        (AND (ATOM (CAR R1)) (PAIRP (CAR R2)) (NOT (MEMQ (CAR R1) (CDAR R2))))
        (AND (ATOM (CAR R2)) (PAIRP (CAR R1)) (NOT (MEMQ (CAR R2) (CDAR R1))))
        (AND (PAIRP (CAR R1)) (PAIRP (CAR R2))
             (NULL (INTERSECTION (CDAR R1) (CDAR R2)))))) 
(PUT 'IBALP_GBREDUCEPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GBREDUCEPOLY 'DEFINED-ON-LINE '445) 
(PUT 'IBALP_GBREDUCEPOLY 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBREDUCEPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBREDUCEPOLY (P RULES)
    (PROG (CHNGE P1 P2)
      (SETQ CHNGE T)
      (SETQ P1 P)
      (SETQ P2 P)
      (PROG ()
       WHILELABEL
        (COND ((NOT CHNGE) (RETURN NIL)))
        (PROGN
         (SETQ CHNGE NIL)
         (PROG (J)
           (SETQ J RULES)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J) (SETQ P1 (IBALP_GBAPPLYRULE P1 J))) (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (COND ((NEQ P1 P2) (PROGN (SETQ CHNGE T) (SETQ P2 P1)))))
        (GO WHILELABEL))
      (RETURN P1))) 
(PUT 'IBALP_GBAPPLYRULE 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GBAPPLYRULE 'DEFINED-ON-LINE '464) 
(PUT 'IBALP_GBAPPLYRULE 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBAPPLYRULE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBAPPLYRULE (P RULE)
    (PROG (W)
      (COND ((EQUAL RULE (KRULE_POLY2RULE 1)) (RETURN 0)))
      (COND ((KPOLY_MONOMIALP P) (RETURN (IBALP_GBAPPLYRULEM P RULE))))
      (SETQ W (CDR P))
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (COND
         ((IBALP_TORDERP (CAR W) (KRULE_HEAD RULE))
          (PROGN
           (SETCAR W (IBALP_GBAPPLYRULEM (CAR W) RULE))
           (SETQ W (CDR W))))
         (T (SETQ W NIL)))
        (GO WHILELABEL))
      (RETURN (KPOLY_PLUS (CDR P))))) 
(PUT 'IBALP_GBAPPLYRULEM 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GBAPPLYRULEM 'DEFINED-ON-LINE '481) 
(PUT 'IBALP_GBAPPLYRULEM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBAPPLYRULEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GBAPPLYRULEM (M RULE)
    (COND ((EQUAL RULE (KRULE_POLY2RULE 1)) 0)
          ((ATOM M) (COND ((EQCAR RULE M) (KRULE_TAIL RULE)) (T M)))
          ((ATOM (KRULE_HEAD RULE))
           (COND
            ((MEMQ (KRULE_HEAD RULE) M)
             (KPOLY_TIMES
              (LIST (LTO_DELQ (KRULE_HEAD RULE) M) (KRULE_TAIL RULE))))
            (T M)))
          ((KPOLY_MONDIVP M (KRULE_HEAD RULE))
           (PROGN
            (PROG (J)
              (SETQ J (CDR (KRULE_HEAD RULE)))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J) (SETQ M (LTO_DELQ J M))) (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (KPOLY_TIMES (LIST M (KRULE_TAIL RULE)))))
          (T M))) 
(PUT 'IBALP_GBINITRULES 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GBINITRULES 'DEFINED-ON-LINE '499) 
(PUT 'IBALP_GBINITRULES 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GBINITRULES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GBINITRULES (PL)
    (PROG (RULES NEWP)
      (SETQ RULES (LIST (KRULE_POLY2RULE (CAR PL))))
      (SETQ PL (CDR PL))
      (PROG ()
       WHILELABEL
        (COND ((NOT PL) (RETURN NIL)))
        (PROGN
         (SETQ NEWP (IBALP_GBREDUCEPOLY (CAR PL) RULES))
         (SETQ PL (CDR PL))
         (COND
          ((EQN NEWP 1)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2T "-- 1 in Ideal Initialisation")))
            (SETQ RULES (LIST (KRULE_POLY2RULE 1)))
            (SETQ PL NIL)))
          ((NOT (EQN NEWP 0))
           (SETQ RULES (CONS (KRULE_POLY2RULE NEWP) RULES)))))
        (GO WHILELABEL))
      (SETQ RULES (IBALP_GBSIMPLIFYALL RULES))
      (RETURN RULES))) 
(PUT 'IBALP_GENPOLYFORM 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_GENPOLYFORM 'DEFINED-ON-LINE '521) 
(PUT 'IBALP_GENPOLYFORM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GENPOLYFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_GENPOLYFORM (F TRTHVAL)
    (COND ((EQN TRTHVAL 1) (KPOLY_PLUS (LIST 1 (IBALP_POLYFORM F))))
          (T (IBALP_POLYFORM F)))) 
(PUT 'IBALP_GENIDEMPPOLYLIST 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_GENIDEMPPOLYLIST 'DEFINED-ON-LINE '529) 
(PUT 'IBALP_GENIDEMPPOLYLIST 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_GENIDEMPPOLYLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_GENIDEMPPOLYLIST (L)
    (PROG (VL)
      (PROG (J)
        (SETQ J L)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND ((IDP J) (SETQ VL (LTO_INSERT J VL)))
                 ((EQCAR J 'TIMES)
                  (PROG (K)
                    (SETQ K (CDR J))
                   LAB
                    (COND ((NULL K) (RETURN NIL)))
                    ((LAMBDA (K) (SETQ VL (LTO_INSERT K VL))) (CAR K))
                    (SETQ K (CDR K))
                    (GO LAB)))
                 ((EQCAR J 'PLUS)
                  (PROG (K)
                    (SETQ K (CDR J))
                   LAB
                    (COND ((NULL K) (RETURN NIL)))
                    ((LAMBDA (K)
                       (COND ((IDP K) (SETQ VL (LTO_INSERT K VL)))
                             ((EQCAR K 'TIMES)
                              (PROG (M)
                                (SETQ M (CDR K))
                               LAB
                                (COND ((NULL M) (RETURN NIL)))
                                ((LAMBDA (M) (SETQ VL (LTO_INSERT M VL)))
                                 (CAR M))
                                (SETQ M (CDR M))
                                (GO LAB)))))
                     (CAR K))
                    (SETQ K (CDR K))
                    (GO LAB)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J VL)
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (KPOLY_IDEMPPOLY J)) (CAR J))
                               NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (J) (KPOLY_IDEMPPOLY J)) (CAR J)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'IBALP_PSET3KNF 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSET3KNF 'DEFINED-ON-LINE '551) 
(PUT 'IBALP_PSET3KNF 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSET3KNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSET3KNF (F TRTHVAL)
    (PROG (NEWF)
      (SETQ NEWF (COND ((EQN TRTHVAL 1) F) (T (LIST 'NOT F))))
      (SETQ NEWF (IBALP_PSET3KNFNF NEWF))
      (SETQ NEWF (IBALP_PSET3KNF2 NEWF NIL))
      (SETQ NEWF
              (COND
               ((EQ (COND ((ATOM NEWF) NEWF) (T (CAR NEWF))) 'AND)
                (CONS 'AND
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J (CDR NEWF))
                       STARTOVER
                        (COND ((NULL J) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (J) (IBALP_PSET3KNF3 J NIL)) (CAR J)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ J (CDR J))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL J) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (J) (IBALP_PSET3KNF3 J NIL)) (CAR J)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ J (CDR J))
                        (GO LOOPLABEL))))
               (T
                ((LAMBDA (G129)
                   (COND ((AND G129 (CDR G129)) (CONS 'AND G129))
                         ((NULL G129) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                         (T (CAR G129))))
                 (IBALP_PSET3KNF3 NEWF NIL)))))
      (RETURN
       (COND
        ((EQ (COND ((ATOM NEWF) NEWF) (T (CAR NEWF))) 'AND)
         (PROG (J FORALL-RESULT FORALL-ENDPTR)
           (SETQ J (CDR NEWF))
           (COND ((NULL J) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (IBALP_GENPOLYFORM J 1)) (CAR J))
                                 NIL)))
          LOOPLABEL
           (SETQ J (CDR J))
           (COND ((NULL J) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (J) (IBALP_GENPOLYFORM J 1)) (CAR J)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
        (T (LIST (IBALP_GENPOLYFORM NEWF 1))))))) 
(PUT 'IBALP_PSET3KNFNF 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_PSET3KNFNF 'DEFINED-ON-LINE '572) 
(PUT 'IBALP_PSET3KNFNF 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSET3KNFNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_PSET3KNFNF (F)
    (COND
     ((OR
       ((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
        (COND ((ATOM F) F) (T (CAR F))))
       (MEMQ (CAR F) '(EQUAL NEQ)))
      F)
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT)
      (COND ((MEMQ (CAR (CADR F)) '(EQUAL NEQ)) F)
            (T (IBALP_PSET3KNFNF1 (CADR F)))))
     (((LAMBDA (X) (OR (EQ X 'OR) (EQ X 'AND)))
       (COND ((ATOM F) F) (T (CAR F))))
      (CONS (COND ((ATOM F) F) (T (CAR F)))
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR F))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (IBALP_PSET3KNFNF J)) (CAR J))
                                    NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (J) (IBALP_PSET3KNFNF J)) (CAR J)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'IMPL)
      (LIST 'OR (IBALP_PSET3KNF1 (LIST 'NOT (CADR F)))
            (IBALP_PSET3KNFNF (CADDR F))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'REPL)
      (LIST 'OR (IBALP_PSET3KNFNF (LIST 'NOT (CADDR F)))
            (IBALP_PSET3KNFNF (CADR F))))
     (T
      (LIST (COND ((ATOM F) F) (T (CAR F))) (IBALP_PSET3KNFNF (CADR F))
            (IBALP_PSET3KNFNF (CADDR F)))))) 
(PUT 'IBALP_PSET3KNFNF1 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_PSET3KNFNF1 'DEFINED-ON-LINE '595) 
(PUT 'IBALP_PSET3KNFNF1 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSET3KNFNF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_PSET3KNFNF1 (F)
    (COND
     (((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
       (COND ((ATOM F) F) (T (CAR F))))
      (CL_FLIP (COND ((ATOM F) F) (T (CAR F)))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT) (IBALP_PSET3KNFNF (CADR F)))
     (((LAMBDA (X) (OR (EQ X 'OR) (EQ X 'AND)))
       (COND ((ATOM F) F) (T (CAR F))))
      (CONS (CL_FLIP (COND ((ATOM F) F) (T (CAR F))))
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR F))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (IBALP_PSET3KNFNF (LIST 'NOT J)))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (IBALP_PSET3KNFNF (LIST 'NOT J))) (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'IMPL)
      (LIST 'AND (IBALP_PSET3KNFNF (CADR F))
            (IBALP_PSET3KNFNF (LIST 'NOT (CADDR F)))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'REPL)
      (LIST 'AND (IBALP_PSET3KNFNF (LIST 'NOT (CADR F)))
            (IBALP_PSET3KNFNF (CADDR F))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'EQUIV)
      (LIST 'EQUIV (IBALP_PSET3KNFNF (LIST 'NOT (CADR F)))
            (IBALP_PSET3KNFNF (CADDR F))))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'XOR)
      (LIST 'EQUIV (IBALP_PSET3KNFNF (CADR F)) (IBALP_PSET3KNFNF (CADDR F)))))) 
(PUT 'IBALP_PSET3KNF2 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSET3KNF2 'DEFINED-ON-LINE '618) 
(PUT 'IBALP_PSET3KNF2 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSET3KNF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSET3KNF2 (F INTREE)
    (PROG (PARTLISTS G)
      (COND
       ((OR
         ((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
          (COND ((ATOM F) F) (T (CAR F))))
         (EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT) (MEMQ (CAR F) '(EQUAL NEQ)))
        (RETURN F)))
      (COND
       ((AND (NULL INTREE) (EQ (COND ((ATOM F) F) (T (CAR F))) 'AND))
        (RETURN
         ((LAMBDA (G131)
            (COND ((AND G131 (CDR G131)) (CONS 'AND G131))
                  ((NULL G131) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G131))))
          (PROG (J FORALL-RESULT FORALL-ENDPTR)
            (SETQ J (CDR F))
           STARTOVER
            (COND ((NULL J) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    ((LAMBDA (J)
                       (PROGN
                        (SETQ G (IBALP_PSET3KNF2 J NIL))
                        (COND
                         ((EQ (COND ((ATOM G) G) (T (CAR G))) 'AND) (CDR G))
                         (T (LIST G)))))
                     (CAR J)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
            (SETQ J (CDR J))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND ((NULL J) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    ((LAMBDA (J)
                       (PROGN
                        (SETQ G (IBALP_PSET3KNF2 J NIL))
                        (COND
                         ((EQ (COND ((ATOM G) G) (T (CAR G))) 'AND) (CDR G))
                         (T (LIST G)))))
                     (CAR J)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ J (CDR J))
            (GO LOOPLABEL))))))
      (COND
       ((AND
         ((LAMBDA (X) (OR (EQ X 'OR) (EQ X 'AND)))
          (COND ((ATOM F) F) (T (CAR F))))
         (LTO_LENGTHP (CDR F) 3 'GEQ))
        (PROGN
         (COND
          ((LTO_LENGTHP (CDR F) 4 'GEQ)
           (PROGN
            (SETQ PARTLISTS (IBALP_SPLITLIST (CDR F)))
            (RETURN
             (LIST (COND ((ATOM F) F) (T (CAR F)))
                   (IBALP_PSET3KNF2
                    (CONS (COND ((ATOM F) F) (T (CAR F))) (CAR PARTLISTS)) T)
                   (IBALP_PSET3KNF2
                    (CONS (COND ((ATOM F) F) (T (CAR F))) (CDR PARTLISTS))
                    T))))))
         (RETURN
          (LIST (COND ((ATOM F) F) (T (CAR F)))
                (IBALP_PSET3KNF2 (CAR (CDR F)) T)
                (LIST (COND ((ATOM F) F) (T (CAR F)))
                      (IBALP_PSET3KNF2 (CADR (CDR F)) T)
                      (IBALP_PSET3KNF2 (CADDR (CDR F)) T)))))))
      (RETURN
       (LIST (COND ((ATOM F) F) (T (CAR F))) (IBALP_PSET3KNF2 (CADR F) T)
             (IBALP_PSET3KNF2 (CADDR F) T))))) 
(PUT 'IBALP_PSET3KNF3 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSET3KNF3 'DEFINED-ON-LINE '645) 
(PUT 'IBALP_PSET3KNF3 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSET3KNF3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSET3KNF3 (F CLAUSEVAR)
    (PROG (NVARL NVARR RETURNLIST)
      (COND
       (((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
         (COND ((ATOM F) F) (T (CAR F))))
        (RETURN (LIST F))))
      (COND
       ((OR (EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT)
            (MEMQ (CAR F) '(EQUAL NEQ)))
        (RETURN (LIST F))))
      (COND
       ((NULL CLAUSEVAR)
        (PROGN
         (SETQ CLAUSEVAR (LIST 'EQUAL (GENSYM) 1))
         (SETQ RETURNLIST (CONS CLAUSEVAR RETURNLIST)))))
      (COND
       ((OR (EQ (COND ((ATOM (CADR F)) (CADR F)) (T (CAR (CADR F)))) 'NOT)
            (MEMQ (CAR (CADR F)) '(EQUAL NEQ)))
        (SETQ NVARL (CADR F)))
       (T
        (PROGN
         (SETQ NVARL (LIST 'EQUAL (GENSYM) 1))
         (SETQ RETURNLIST
                 (NCONC RETURNLIST (IBALP_PSET3KNF3 (CADR F) NVARL))))))
      (COND
       ((OR (EQ ((LAMBDA (F) (COND ((ATOM F) F) (T (CAR F)))) (CADDR F)) 'NOT)
            (MEMQ (CAR (CADDR F)) '(EQUAL NEQ)))
        (SETQ NVARR (CADDR F)))
       (T
        (PROGN
         (SETQ NVARR (LIST 'EQUAL (GENSYM) 1))
         (SETQ RETURNLIST
                 (NCONC RETURNLIST (IBALP_PSET3KNF3 (CADDR F) NVARR))))))
      (RETURN
       (CONS
        (LIST 'EQUIV CLAUSEVAR
              (LIST (COND ((ATOM F) F) (T (CAR F))) NVARL NVARR))
        RETURNLIST)))) 
(PUT 'IBALP_PSETKAPUR 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPUR 'DEFINED-ON-LINE '675) 
(PUT 'IBALP_PSETKAPUR 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPUR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPUR (F TRTHVAL)
    (COND
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT)
      (IBALP_PSETKAPUR (CADR F) (IBALP_FLIP01 TRTHVAL)))
     ((EQN TRTHVAL 1) (IBALP_PSETKAPURCONT F)) (T (IBALP_PSETKAPURTAUT F)))) 
(PUT 'IBALP_PSETKAPURTAUT 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_PSETKAPURTAUT 'DEFINED-ON-LINE '687) 
(PUT 'IBALP_PSETKAPURTAUT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURTAUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_PSETKAPURTAUT (F)
    (COND
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'IMPL)
      (NCONC (IBALP_PSETKAPUR (CADR F) 1) (IBALP_PSETKAPUR (CADDR F) 0)))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'REPL)
      (NCONC (IBALP_PSETKAPUR (CADR F) 0) (IBALP_PSETKAPUR (CADDR F) 1)))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J (CDR F))
       STARTOVER
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT ((LAMBDA (J) (IBALP_PSETKAPUR J 0)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ J (CDR J))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR ((LAMBDA (J) (IBALP_PSETKAPUR J 0)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ J (CDR J))
        (GO LOOPLABEL)))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND) (IBALP_PSETKAPURNARY F 0))
     (T (IBALP_PSETKAPURNOOPT F 0)))) 
(PUT 'IBALP_PSETKAPURCONT 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_PSETKAPURCONT 'DEFINED-ON-LINE '701) 
(PUT 'IBALP_PSETKAPURCONT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURCONT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_PSETKAPURCONT (F)
    (COND
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J (CDR F))
       STARTOVER
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT ((LAMBDA (J) (IBALP_PSETKAPUR J 1)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ J (CDR J))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR ((LAMBDA (J) (IBALP_PSETKAPUR J 1)) (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ J (CDR J))
        (GO LOOPLABEL)))
     ((AND (EQ (COND ((ATOM F) F) (T (CAR F))) 'IMPL)
           (EQ ((LAMBDA (F) (COND ((ATOM F) F) (T (CAR F)))) (CADDR F)) 'AND))
      (IBALP_PSETKAPURDISTLEFT F 1))
     ((AND (EQ (COND ((ATOM F) F) (T (CAR F))) 'REPL)
           (EQ (COND ((ATOM (CADR F)) (CADR F)) (T (CAR (CADR F)))) 'AND))
      (IBALP_PSETKAPURDISTRIGHT F 1))
     ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR) (IBALP_PSETKAPURNARY F 1))
     (T (IBALP_PSETKAPURNOOPT F 1)))) 
(PUT 'IBALP_PSETKAPURNARY 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPURNARY 'DEFINED-ON-LINE '715) 
(PUT 'IBALP_PSETKAPURNARY 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURNARY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPURNARY (F TRTHVAL)
    (PROG (DISTOP)
      (SETQ DISTOP (CL_FLIP (COND ((ATOM F) F) (T (CAR F)))))
      (COND
       ((LTO_LENGTHP (CDR F) 4 'GEQ)
        (RETURN (IBALP_PSETKAPURNARY1 F TRTHVAL))))
      (COND
       ((LTO_LENGTHP (CDR F) 2 'EQN)
        (RETURN
         (COND
          ((EQ ((LAMBDA (F) (COND ((ATOM F) F) (T (CAR F)))) (CADDR F)) DISTOP)
           (IBALP_PSETKAPURDISTLEFT F TRTHVAL))
          ((EQ (COND ((ATOM (CADR F)) (CADR F)) (T (CAR (CADR F)))) DISTOP)
           (IBALP_PSETKAPURDISTRIGHT F TRTHVAL))
          (T (IBALP_PSETKAPURNOOPT F TRTHVAL))))))
      (RETURN (IBALP_PSETKAPURNOOPT F TRTHVAL)))) 
(PUT 'IBALP_PSETKAPURNARY1 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPURNARY1 'DEFINED-ON-LINE '735) 
(PUT 'IBALP_PSETKAPURNARY1 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURNARY1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPURNARY1 (F TRTHVAL)
    (PROG (PARTLISTS NEWVAR L1 L2)
      (SETQ PARTLISTS (IBALP_SPLITLIST (CDR F)))
      (SETQ L1 (CAR PARTLISTS))
      (SETQ L2 (CDR PARTLISTS))
      (SETQ NEWVAR (GENSYM))
      (SETQ L1
              (CONS (COND ((ATOM F) F) (T (CAR F)))
                    (CONS (LIST 'EQUAL NEWVAR 1) L1)))
      (SETQ L2
              (CONS (COND ((ATOM F) F) (T (CAR F)))
                    (CONS (LIST 'NOT (LIST 'EQUAL NEWVAR 1)) L2)))
      (RETURN
       (NCONC (IBALP_PSETKAPUR L1 TRTHVAL) (IBALP_PSETKAPUR L2 TRTHVAL))))) 
(PUT 'IBALP_PSETKAPURNOOPT 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPURNOOPT 'DEFINED-ON-LINE '750) 
(PUT 'IBALP_PSETKAPURNOOPT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURNOOPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPURNOOPT (F TRTHVAL)
    (PROG (P)
      (COND
       ((EQ (IBALP_GETKAPUROPTION 'POLYGENMODE) 'KAPURKNF)
        (RETURN (IBALP_PSET3KNF F TRTHVAL))))
      (SETQ P (IBALP_GENPOLYFORM F TRTHVAL))
      (RETURN (COND ((NOT (EQN P 0)) (LIST P)))))) 
(PUT 'IBALP_PSETKAPURDISTLEFT 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPURDISTLEFT 'DEFINED-ON-LINE '761) 
(PUT 'IBALP_PSETKAPURDISTLEFT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURDISTLEFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPURDISTLEFT (F TRTHVAL)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J (CDR (CADDR F)))
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J)
                 (IBALP_PSETKAPUR
                  (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) J) TRTHVAL))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J)
                 (IBALP_PSETKAPUR
                  (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) J) TRTHVAL))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'IBALP_PSETKAPURDISTRIGHT 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETKAPURDISTRIGHT 'DEFINED-ON-LINE '768) 
(PUT 'IBALP_PSETKAPURDISTRIGHT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETKAPURDISTRIGHT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETKAPURDISTRIGHT (F TRTHVAL)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J (CDR (CADR F)))
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J)
                 (IBALP_PSETKAPUR
                  (LIST (COND ((ATOM F) F) (T (CAR F))) (CADDR F) J) TRTHVAL))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J)
                 (IBALP_PSETKAPUR
                  (LIST (COND ((ATOM F) F) (T (CAR F))) (CADDR F) J) TRTHVAL))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'IBALP_PSETDIREKT 'NUMBER-OF-ARGS 2) 
(PUT 'IBALP_PSETDIREKT 'DEFINED-ON-LINE '775) 
(PUT 'IBALP_PSETDIREKT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_PSETDIREKT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IBALP_PSETDIREKT (F TRTHVAL) (LIST (IBALP_GENPOLYFORM F TRTHVAL))) 
(PUT 'IBALP_SPLITLIST 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_SPLITLIST 'DEFINED-ON-LINE '780) 
(PUT 'IBALP_SPLITLIST 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_SPLITLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_SPLITLIST (L)
    (PROG (ELM L2 LGT CNT)
      (SETQ LGT 0)
      (SETQ CNT 0)
      (COND ((NULL L) (RETURN (CONS NIL NIL))))
      (COND ((NULL (CDR L)) (RETURN (CONS L NIL))))
      (SETQ LGT (LENGTH L))
      (SETQ CNT 1)
      (SETQ ELM L)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP CNT (QUOTIENT LGT 2))) (RETURN NIL)))
        (PROGN (SETQ CNT (ADD1 CNT)) (SETQ ELM (CDR ELM)))
        (GO WHILELABEL))
      (SETQ L2 (CDR ELM))
      (SETCDR ELM NIL)
      (RETURN (CONS L L2)))) 
(PUT 'IBALP_CLONESTRUCT 'NUMBER-OF-ARGS 1) 
(PUT 'IBALP_CLONESTRUCT 'DEFINED-ON-LINE '801) 
(PUT 'IBALP_CLONESTRUCT 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'IBALP_CLONESTRUCT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IBALP_CLONESTRUCT (S)
    (COND ((ATOM S) S)
          (T (CONS (IBALP_CLONESTRUCT (CAR S)) (IBALP_CLONESTRUCT (CDR S)))))) 
(ENDMODULE) 
(MODULE (LIST 'KRULE)) 
(PUT 'KRULE_HEAD 'NUMBER-OF-ARGS 1) 
(PUT 'KRULE_HEAD 'DEFINED-ON-LINE '814) 
(PUT 'KRULE_HEAD 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KRULE_HEAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KRULE_HEAD (R) (CAR R)) 
(PUT 'KRULE_TAIL 'NUMBER-OF-ARGS 1) 
(PUT 'KRULE_TAIL 'DEFINED-ON-LINE '818) 
(PUT 'KRULE_TAIL 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KRULE_TAIL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KRULE_TAIL (R) (CDR R)) 
(PUT 'KRULE_GENRULE 'NUMBER-OF-ARGS 2) 
(PUT 'KRULE_GENRULE 'DEFINED-ON-LINE '822) 
(PUT 'KRULE_GENRULE 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KRULE_GENRULE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KRULE_GENRULE (H TT) (CONS H TT)) 
(PUT 'KRULE_RULE2POLY 'NUMBER-OF-ARGS 1) 
(PUT 'KRULE_RULE2POLY 'DEFINED-ON-LINE '827) 
(PUT 'KRULE_RULE2POLY 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KRULE_RULE2POLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KRULE_RULE2POLY (R) (KPOLY_PLUS (LIST (KRULE_HEAD R) (KRULE_TAIL R)))) 
(PUT 'KRULE_POLY2RULE 'NUMBER-OF-ARGS 1) 
(PUT 'KRULE_POLY2RULE 'DEFINED-ON-LINE '832) 
(PUT 'KRULE_POLY2RULE 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KRULE_POLY2RULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KRULE_POLY2RULE (P)
    (PROG (MONLIST)
      (COND ((KPOLY_MONOMIALP P) (RETURN (CONS P 0))))
      (SETQ MONLIST (SORT (CDR P) 'IBALP_TORDERP))
      (RETURN (KRULE_GENRULE (CAR MONLIST) (KPOLY_PLUS (CDR MONLIST)))))) 
(ENDMODULE) 
(MODULE (LIST 'KPOLY)) 
(PUT 'KPOLY_TIMES 'NUMBER-OF-ARGS 1) 
(PUT 'KPOLY_TIMES 'DEFINED-ON-LINE '851) 
(PUT 'KPOLY_TIMES 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_TIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KPOLY_TIMES (L)
    (PROG (SETLVAR SETLSUM CURPOLY)
      (SETQ L (IBALP_REMNESTED L 'TIMES))
      (COND ((MEMBER 0 L) (RETURN 0)))
      (PROG (J)
        (SETQ J L)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((AND (ATOM J) (NOT (EQN J 1)))
             (SETQ SETLVAR (LTO_INSERT J SETLVAR)))
            ((EQCAR J 'PLUS) (SETQ SETLSUM (LTO_INSERT J SETLSUM)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ SETLVAR (SORT SETLVAR 'ORDOP))
      (COND ((NULL SETLSUM) (RETURN (KPOLY_NORM (CONS 'TIMES SETLVAR)))))
      (COND ((AND (NULL SETLVAR) (NULL (CDR SETLSUM))) (RETURN (CAR SETLSUM))))
      (COND (SETLVAR (SETQ CURPOLY (KPOLY_NORM (CONS 'TIMES SETLVAR))))
            (T
             (PROGN
              (SETQ CURPOLY (CAR SETLSUM))
              (SETQ SETLSUM (CDR SETLSUM)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT SETLSUM) (RETURN NIL)))
        (PROGN
         (SETQ CURPOLY (KPOLY_TIMES2 CURPOLY (CAR SETLSUM)))
         (SETQ SETLSUM (COND ((NOT (EQN CURPOLY 0)) (CDR SETLSUM)))))
        (GO WHILELABEL))
      (RETURN CURPOLY))) 
(PUT 'KPOLY_TIMES2 'NUMBER-OF-ARGS 2) 
(PUT 'KPOLY_TIMES2 'DEFINED-ON-LINE '880) 
(PUT 'KPOLY_TIMES2 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_TIMES2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOLY_TIMES2 (P1 P2)
    (COND
     ((AND (KPOLY_MONOMIALP P1) (KPOLY_MONOMIALP P2))
      (KPOLY_TIMES2MONOMS P1 P2))
     ((KPOLY_MONOMIALP P1) (KPOLY_TIMES2MONOMSUM P1 P2))
     ((KPOLY_MONOMIALP P2) (KPOLY_TIMES2MONOMSUM P2 P1))
     (T (KPOLY_TIMES2SUMS P1 P2)))) 
(PUT 'KPOLY_TIMES2SUMS 'NUMBER-OF-ARGS 2) 
(PUT 'KPOLY_TIMES2SUMS 'DEFINED-ON-LINE '892) 
(PUT 'KPOLY_TIMES2SUMS 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_TIMES2SUMS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOLY_TIMES2SUMS (S1 S2)
    (KPOLY_PLUS
     (PROG (J FORALL-RESULT FORALL-ENDPTR)
       (SETQ J (CDR S1))
       (COND ((NULL J) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (KPOLY_TIMES2MONOMSUM J S2)) (CAR J))
                             NIL)))
      LOOPLABEL
       (SETQ J (CDR J))
       (COND ((NULL J) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (J) (KPOLY_TIMES2MONOMSUM J S2)) (CAR J)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'KPOLY_TIMES2MONOMSUM 'NUMBER-OF-ARGS 2) 
(PUT 'KPOLY_TIMES2MONOMSUM 'DEFINED-ON-LINE '899) 
(PUT 'KPOLY_TIMES2MONOMSUM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_TIMES2MONOMSUM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOLY_TIMES2MONOMSUM (M S)
    (COND ((KPOLY_MONOMIALP S) (KPOLY_TIMES2MONOMS M S))
          (T
           (KPOLY_PLUS
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR S))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (KPOLY_TIMES2MONOMS M J)) (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (J) (KPOLY_TIMES2MONOMS M J)) (CAR J))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT 'KPOLY_TIMES2MONOMS 'NUMBER-OF-ARGS 2) 
(PUT 'KPOLY_TIMES2MONOMS 'DEFINED-ON-LINE '909) 
(PUT 'KPOLY_TIMES2MONOMS 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_TIMES2MONOMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOLY_TIMES2MONOMS (M1 M2)
    (PROG (SETL)
      (COND
       ((ATOM M1)
        (COND ((EQN M1 1) (RETURN M2)) (T (SETQ SETL (LTO_INSERT M1 SETL)))))
       (T
        (PROG (J)
          (SETQ J (CDR M1))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J) (SETQ SETL (LTO_INSERT J SETL))) (CAR J))
          (SETQ J (CDR J))
          (GO LAB))))
      (COND
       ((ATOM M2)
        (COND ((EQN M2 1) (RETURN M1)) (T (SETQ SETL (LTO_INSERT M2 SETL)))))
       (T
        (PROG (J)
          (SETQ J (CDR M2))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J) (SETQ SETL (LTO_INSERT J SETL))) (CAR J))
          (SETQ J (CDR J))
          (GO LAB))))
      (RETURN (KPOLY_NORM (CONS 'TIMES (SORT SETL 'ORDOP)))))) 
(PUT 'KPOLY_PLUS 'NUMBER-OF-ARGS 1) 
(PUT 'KPOLY_PLUS 'DEFINED-ON-LINE '931) 
(PUT 'KPOLY_PLUS 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_PLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KPOLY_PLUS (L)
    (PROG (TMPL W)
      (SETQ TMPL (SORT (IBALP_REMNESTED L 'PLUS) 'IBALP_TORDERP))
      (SETQ W TMPL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND W (CDR W))) (RETURN NIL)))
        (COND ((EQN (CAR W) 0) (PROGN (SETCAR W (CADR W)) (SETCDR W (CDDR W))))
              ((EQUAL (CAR W) (CADR W))
               (COND
                ((CDDR W) (PROGN (SETCAR W (CADDR W)) (SETCDR W (CDDDR W))))
                (T (PROGN (SETCAR W 0) (SETCDR W NIL)))))
              (T (SETQ W (CDR W))))
        (GO WHILELABEL))
      (SETQ TMPL (DELETE 0 TMPL))
      (RETURN (KPOLY_NORM (CONS 'PLUS TMPL))))) 
(PUT 'KPOLY_MONOMIALP 'NUMBER-OF-ARGS 1) 
(PUT 'KPOLY_MONOMIALP 'DEFINED-ON-LINE '957) 
(PUT 'KPOLY_MONOMIALP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_MONOMIALP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KPOLY_MONOMIALP (P) (OR (ATOM P) (EQCAR P 'TIMES))) 
(PUT 'KPOLY_IDEMPPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'KPOLY_IDEMPPOLY 'DEFINED-ON-LINE '962) 
(PUT 'KPOLY_IDEMPPOLY 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_IDEMPPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KPOLY_IDEMPPOLY (VAR) (LIST 'PLUS (LIST 'TIMES VAR VAR) VAR)) 
(PUT 'KPOLY_NORM 'NUMBER-OF-ARGS 1) 
(PUT 'KPOLY_NORM 'DEFINED-ON-LINE '967) 
(PUT 'KPOLY_NORM 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_NORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KPOLY_NORM (P)
    (COND ((ATOM P) P) ((NULL (CDR P)) (COND ((EQCAR P 'TIMES) 1) (T 0)))
          ((NULL (CDDR P)) (CADR P)) (T P))) 
(PUT 'KPOLY_MONDIVP 'NUMBER-OF-ARGS 2) 
(PUT 'KPOLY_MONDIVP 'DEFINED-ON-LINE '979) 
(PUT 'KPOLY_MONDIVP 'DEFINED-IN-FILE 'REDLOG/IBALP/IBALPKAPUR.RED) 
(PUT 'KPOLY_MONDIVP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KPOLY_MONDIVP (M1 M2)
    (PROG (E1 E2 RSL)
      (COND ((OR (EQN M1 0) (EQN M2 1) (EQUAL M1 M2)) (RETURN T)))
      (COND ((EQN M2 0) (RETURN NIL)))
      (COND ((AND (ATOM M1) (ATOM M2)) (RETURN (EQUAL M1 M2))))
      (COND ((ATOM M1) (RETURN NIL)))
      (COND ((ATOM M2) (RETURN (MEMBER M2 M1))))
      (SETQ E1 (CDR M1))
      (SETQ E2 (CDR M2))
      (SETQ RSL T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND E1 E2 RSL)) (RETURN NIL)))
        (COND
         ((EQUAL (CAR E1) (CAR E2))
          (PROGN (SETQ E1 (CDR E1)) (SETQ E2 (CDR E2))))
         ((ORDOP (CAR E1) (CAR E2)) (SETQ E1 (CDR E1))) (T (SETQ RSL NIL)))
        (GO WHILELABEL))
      (RETURN (AND (NULL E2) RSL)))) 
(ENDMODULE) 