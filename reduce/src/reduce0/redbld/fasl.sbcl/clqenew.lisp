(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLQENEW)) 
(REVISION 'CLQENEW "$Id: clqenew.red 6574 2023-08-03 06:49:09Z thomas-sturm $") 
(COPYRIGHT 'CLQENEW "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'CL_QE_NEW 'NUMBER-OF-ARGS 2) 
(DE CL_QE_NEW (F THEORY)
    (PROG (STATE *RLSIPO *RLSIPW)
      (SETQ *RLSIPO T)
      (SETQ *RLSIPW T)
      (SETQ STATE (QESTATE_NEW))
      (QESTATE_SETINPUTFORMULA STATE F)
      (QESTATE_SETINPUTTHEORY STATE THEORY)
      (QESTATE_SETNOASSUMEVARS STATE NIL)
      (QESTATE_SETANSWERMODE STATE NIL)
      (QESTATE_SETASSUMEMODE STATE NIL)
      (SETQ STATE (CL_QE1_NEW STATE))
      (COND ((RL_EXCP STATE) (RETURN STATE)))
      (RETURN (QESTATE_GETFORMULA STATE)))) 
(PUT 'CL_QEA_NEW 'NUMBER-OF-ARGS 2) 
(DE CL_QEA_NEW (F THEORY)
    (PROG (STATE *RLQEANS *RLSIPO *RLSIPW)
      (SETQ *RLQEANS T)
      (SETQ *RLSIPO T)
      (SETQ *RLSIPW T)
      (SETQ STATE (QESTATE_NEW))
      (QESTATE_SETINPUTFORMULA STATE F)
      (QESTATE_SETINPUTTHEORY STATE THEORY)
      (QESTATE_SETNOASSUMEVARS STATE NIL)
      (QESTATE_SETASSUMEMODE STATE NIL)
      (QESTATE_SETANSWERMODE STATE
       (COND ((AND *RLQESTDANS (NULL (RL_FVARL F))) 'STANDARD) (T 'RAW)))
      (SETQ STATE (CL_QE1_NEW STATE))
      (COND ((RL_EXCP STATE) (RETURN STATE)))
      (RETURN (QESTATE_GETANSWER STATE)))) 
(PUT 'CL_GQE_NEW 'NUMBER-OF-ARGS 3) 
(DE CL_GQE_NEW (F THEORY XNOASSUMEVARS)
    (PROG (STATE *RLQEGEN *RLSIPO *RLSIPW)
      (SETQ *RLQEGEN T)
      (SETQ *RLSIPO T)
      (SETQ *RLSIPW T)
      (SETQ STATE (QESTATE_NEW))
      (QESTATE_SETINPUTFORMULA STATE F)
      (QESTATE_SETINPUTTHEORY STATE THEORY)
      (QESTATE_SETNOASSUMEVARS STATE XNOASSUMEVARS)
      (QESTATE_SETASSUMEMODE STATE (COND (*RLQEGENCT 'FULL) (T 'MONOMIAL)))
      (QESTATE_SETANSWERMODE STATE NIL)
      (SETQ STATE (CL_QE1_NEW STATE))
      (COND ((RL_EXCP STATE) (RETURN STATE)))
      (RETURN (CONS (QESTATE_GETTHEORY STATE) (QESTATE_GETFORMULA STATE))))) 
(PUT 'CL_GQEA_NEW 'NUMBER-OF-ARGS 3) 
(DE CL_GQEA_NEW (F THEORY XNOASSUMEVARS)
    (PROG (STATE *RLQEANS *RLQEGEN *RLSIPO *RLSIPW)
      (SETQ *RLQEGEN T)
      (SETQ *RLQEANS T)
      (SETQ *RLSIPO T)
      (SETQ *RLSIPW T)
      (SETQ STATE (QESTATE_NEW))
      (QESTATE_SETINPUTFORMULA STATE F)
      (QESTATE_SETINPUTTHEORY STATE THEORY)
      (QESTATE_SETNOASSUMEVARS STATE XNOASSUMEVARS)
      (QESTATE_SETASSUMEMODE STATE (COND (*RLQEGENCT 'FULL) (T 'MONOMIAL)))
      (QESTATE_SETANSWERMODE STATE
       (COND ((AND *RLQESTDANS (NULL (RL_FVARL F))) 'STANDARD) (T 'RAW)))
      (SETQ STATE (CL_QE1_NEW STATE))
      (COND ((RL_EXCP STATE) (RETURN STATE)))
      (RETURN (CONS (QESTATE_GETTHEORY STATE) (QESTATE_GETANSWER STATE))))) 
(PUT 'CL_QE1_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_QE1_NEW (STATE)
    (PROG (SVRLIDENTIFY INPUTTHEORY INPUTFORMULA BLOCKS CURRENTFORMULA
           ALLQUANTIFIEDVARIABLES CURRENTTHEORY RESULTNODES ANSWER POINT
           COUNTANSWERCASES)
      (SETQ COUNTANSWERCASES 0)
      (SETQ SVRLIDENTIFY *RLIDENTIFY)
      (SETQ INPUTTHEORY (QESTATE_GETINPUTTHEORY STATE))
      (COND
       ((CL_ISINCONSISTENTTHEORY INPUTTHEORY)
        (RETURN (RL_EXC "inconsistent theory"))))
      (SETQ INPUTFORMULA (QESTATE_GETINPUTFORMULA STATE))
      (SETQ CURRENTFORMULA (RL_PNF INPUTFORMULA))
      (PROG (G185 G186)
        (SETQ G185 (CL_SPLIT_NEW CURRENTFORMULA))
        (SETQ G186 G185)
        (SETQ BLOCKS (CAR G185))
        (SETQ G185 (CDR G185))
        (SETQ CURRENTFORMULA (CAR G185))
        (SETQ G185 (CDR G185))
        (SETQ ALLQUANTIFIEDVARIABLES (CAR G185))
        (SETQ G185 (CDR G185))
        (RETURN G186))
      (COND
       ((NULL BLOCKS)
        (PROGN
         (QESTATE_SETTHEORY STATE INPUTTHEORY)
         (QESTATE_SETFORMULA STATE INPUTFORMULA)
         (COND
          ((NOT (NULL (QESTATE_GETANSWERMODE STATE)))
           (QESTATE_SETANSWER STATE (LIST (CONS INPUTFORMULA NIL)))))
         (RETURN STATE))))
      (QESTATE_SETNOASSUMEVARS STATE
       (UNION ALLQUANTIFIEDVARIABLES (QESTATE_GETNOASSUMEVARS STATE)))
      (SETQ CURRENTTHEORY
              (PROG (ATOMICFORMULA FORALL-RESULT FORALL-ENDPTR)
                (SETQ ATOMICFORMULA INPUTTHEORY)
               STARTOVER
                (COND ((NULL ATOMICFORMULA) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (ATOMICFORMULA)
                           (COND
                            ((NULL
                              (INTERSECTION (RL_VARLAT ATOMICFORMULA)
                                            ALLQUANTIFIEDVARIABLES))
                             (LIST ATOMICFORMULA))))
                         (CAR ATOMICFORMULA)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ ATOMICFORMULA (CDR ATOMICFORMULA))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL ATOMICFORMULA) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (ATOMICFORMULA)
                           (COND
                            ((NULL
                              (INTERSECTION (RL_VARLAT ATOMICFORMULA)
                                            ALLQUANTIFIEDVARIABLES))
                             (LIST ATOMICFORMULA))))
                         (CAR ATOMICFORMULA)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ ATOMICFORMULA (CDR ATOMICFORMULA))
                (GO LOOPLABEL)))
      (QESTATE_SETTHEORY STATE CURRENTTHEORY)
      (QESTATE_SETFORMULA STATE CURRENTFORMULA)
      (QESTATE_SETBLOCKS STATE BLOCKS)
      (CL_QE1-ITERATE_NEW STATE)
      (SETQ BLOCKS (QESTATE_GETBLOCKS STATE))
      (SETQ CURRENTFORMULA (QESTATE_GETFORMULA STATE))
      (SETQ CURRENTFORMULA (CL_UNSPLIT_NEW BLOCKS CURRENTFORMULA))
      (COND
       ((QESTATE_GETANSWERMODE STATE)
        (PROGN
         (COND
          ((OR (NULL BLOCKS) (NULL (CDR BLOCKS)))
           (PROGN
            (SETQ RESULTNODES
                    (NCONC (QESTATE_FETCHSUCCESSNODES STATE)
                           (QESTATE_FETCHFAILURENODES STATE)))
            (COND
             ((AND *RLVERBOSE (QEENV_GETVB STATE))
              (PROGN
               (IOTO_TPRIN2 (LIST "+++ Postprocessing answer:"))
               (SETQ COUNTANSWERCASES (LENGTH RESULTNODES)))))
            (SETQ ANSWER
                    (PROG (NODE FORALL-RESULT FORALL-ENDPTR)
                      (SETQ NODE RESULTNODES)
                      (COND ((NULL NODE) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (NODE)
                                          (PROGN
                                           (COND
                                            ((AND *RLVERBOSE
                                                  (QEENV_GETVB STATE))
                                             (PROGN
                                              (IOTO_PRIN2
                                               (LIST " [" COUNTANSWERCASES))
                                              (SETQ COUNTANSWERCASES
                                                      (DIFFERENCE
                                                       COUNTANSWERCASES 1)))))
                                           ((LAMBDA (*RLQESTDANS)
                                              (SETQ POINT
                                                      (RL_QEMKANS
                                                       (QENODE_GETANSWER
                                                        NODE))))
                                            (EQ (QESTATE_GETANSWERMODE STATE)
                                                'STANDARD))
                                           (COND
                                            ((AND *RLVERBOSE
                                                  (QEENV_GETVB STATE))
                                             (IOTO_PRIN2 (LIST "]"))))
                                           (CONS
                                            (CL_UNSPLIT_NEW BLOCKS
                                             (QENODE_GETFORMULA NODE))
                                            POINT)))
                                        (CAR NODE))
                                       NIL)))
                     LOOPLABEL
                      (SETQ NODE (CDR NODE))
                      (COND ((NULL NODE) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (NODE)
                                  (PROGN
                                   (COND
                                    ((AND *RLVERBOSE (QEENV_GETVB STATE))
                                     (PROGN
                                      (IOTO_PRIN2 (LIST " [" COUNTANSWERCASES))
                                      (SETQ COUNTANSWERCASES
                                              (DIFFERENCE COUNTANSWERCASES
                                                          1)))))
                                   ((LAMBDA (*RLQESTDANS)
                                      (SETQ POINT
                                              (RL_QEMKANS
                                               (QENODE_GETANSWER NODE))))
                                    (EQ (QESTATE_GETANSWERMODE STATE)
                                        'STANDARD))
                                   (COND
                                    ((AND *RLVERBOSE (QEENV_GETVB STATE))
                                     (IOTO_PRIN2 (LIST "]"))))
                                   (CONS
                                    (CL_UNSPLIT_NEW BLOCKS
                                     (QENODE_GETFORMULA NODE))
                                    POINT)))
                                (CAR NODE))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
          (T (SETQ ANSWER (LIST (CONS CURRENTFORMULA NIL)))))
         (QESTATE_SETFORMULA STATE CURRENTFORMULA)
         (QESTATE_SETANSWER STATE ANSWER)
         (RETURN STATE))))
      (SETQ CURRENTTHEORY (QESTATE_GETTHEORY STATE))
      (COND
       ((AND *RLQEFB
             ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
              (COND ((ATOM CURRENTFORMULA) CURRENTFORMULA)
                    (T (CAR CURRENTFORMULA)))))
        (PROGN
         (COND
          ((AND *RLVERBOSE (QEENV_GETVB STATE))
           (IOTO_TPRIN2 (LIST "++++ Entering fallback QE: "))))
         (PROG (G187)
           (SETQ G187 (RL_FBQE CURRENTFORMULA CURRENTTHEORY))
           (SETQ CURRENTTHEORY (CAR G187))
           (SETQ CURRENTFORMULA (CDR G187))
           (RETURN G187)))))
      (QESTATE_SETTHEORY STATE CURRENTTHEORY)
      (QESTATE_SETFORMULA STATE CURRENTFORMULA)
      (ONOFF 'RLIDENTIFY SVRLIDENTIFY)
      (RETURN STATE))) 
(PUT 'CL_ISINCONSISTENTTHEORY 'NUMBER-OF-ARGS 1) 
(DE CL_ISINCONSISTENTTHEORY (THEORY)
    (PROG (HASINCONSISTENTATOM SCANTHEORY SIMPLFIEDTHEORY ATOM)
      (SETQ HASINCONSISTENTATOM NIL)
      (SETQ SCANTHEORY THEORY)
      (SETQ SIMPLFIEDTHEORY NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT HASINCONSISTENTATOM) (NOT (NULL SCANTHEORY))))
          (RETURN NIL)))
        (PROGN
         (SETQ ATOM
                 (CL_SIMPLAT
                  (PROG1 (CAR SCANTHEORY) (SETQ SCANTHEORY (CDR SCANTHEORY)))
                  NIL))
         (COND ((EQ ATOM 'FALSE) (SETQ HASINCONSISTENTATOM T))
               ((NEQ ATOM 'TRUE)
                (PROGN
                 (SETQ SIMPLFIEDTHEORY (CONS ATOM SIMPLFIEDTHEORY))
                 ATOM))))
        (GO WHILELABEL))
      (COND (HASINCONSISTENTATOM (RETURN T)))
      (COND
       ((EQ (RL_SMUPDKNOWL 'AND SIMPLFIEDTHEORY NIL 0) 'FALSE) (RETURN T)))
      (RETURN NIL))) 
(PUT 'CL_SPLIT_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_SPLIT_NEW (F)
    (PROG (CURRENTQUANTIFIER CURRENTQUANTIFIEDVARIABLES BLOCKS
           ALLQUANTIFIEDVARIABLES)
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN (LIST NIL F NIL))))
      (SETQ CURRENTQUANTIFIER (COND ((ATOM F) F) (T (CAR F))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
            (COND ((ATOM F) F) (T (CAR F)))))
          (RETURN NIL)))
        (PROGN
         (COND
          ((NEQ (COND ((ATOM F) F) (T (CAR F))) CURRENTQUANTIFIER)
           (PROGN
            (PROG (W1)
              (SETQ W1 (CONS CURRENTQUANTIFIER CURRENTQUANTIFIEDVARIABLES))
              (SETQ BLOCKS (CONS W1 BLOCKS))
              (RETURN W1))
            (SETQ CURRENTQUANTIFIER (COND ((ATOM F) F) (T (CAR F))))
            (SETQ CURRENTQUANTIFIEDVARIABLES NIL))))
         (PROG (W1)
           (SETQ W1 (CADR F))
           (SETQ CURRENTQUANTIFIEDVARIABLES
                   (CONS W1 CURRENTQUANTIFIEDVARIABLES))
           (RETURN W1))
         (PROG (W1)
           (SETQ W1 (CADR F))
           (SETQ ALLQUANTIFIEDVARIABLES (CONS W1 ALLQUANTIFIEDVARIABLES))
           (RETURN W1))
         (SETQ F (CADDR F)))
        (GO WHILELABEL))
      (PROG (W1)
        (SETQ W1 (CONS CURRENTQUANTIFIER CURRENTQUANTIFIEDVARIABLES))
        (SETQ BLOCKS (CONS W1 BLOCKS))
        (RETURN W1))
      (RETURN (LIST BLOCKS F ALLQUANTIFIEDVARIABLES)))) 
(PUT 'CL_UNSPLIT_NEW 'NUMBER-OF-ARGS 2) 
(DE CL_UNSPLIT_NEW (BLOCKS F)
    (PROG (OCCURRINGVARIABLES RESULT)
      (SETQ OCCURRINGVARIABLES (CL_FVARL1 F))
      (SETQ RESULT F)
      (PROG (BLOCK)
        (SETQ BLOCK BLOCKS)
       LAB
        (COND ((NULL BLOCK) (RETURN NIL)))
        ((LAMBDA (BLOCK)
           (PROG (VARIABLE)
             (SETQ VARIABLE (CDR BLOCK))
            LAB
             (COND ((NULL VARIABLE) (RETURN NIL)))
             ((LAMBDA (VARIABLE)
                (COND
                 ((MEMQ VARIABLE OCCURRINGVARIABLES)
                  (SETQ RESULT (LIST (CAR BLOCK) VARIABLE RESULT)))))
              (CAR VARIABLE))
             (SETQ VARIABLE (CDR VARIABLE))
             (GO LAB)))
         (CAR BLOCK))
        (SETQ BLOCK (CDR BLOCK))
        (GO LAB))
      (RETURN RESULT))) 
(PUT 'CL_QE1-ITERATE_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_QE1-ITERATE_NEW (STATE)
    (PROG (SVRLQEPRECISE SVRLQEAPRECISE QUANTIFIER VARIABLES THEORY FORMULA
           PRODUCEANSWER SUCCESSNODES FAILURENODES REMAININGVARIABLES
           FORMULAOPERATOR FORMULAARGUMENTS)
      (SETQ SVRLQEPRECISE *RLQEPRECISE)
      (SETQ SVRLQEAPRECISE *RLQEAPRECISE)
      (QESTATE_SETSUCCESSNODES STATE NIL)
      (QESTATE_SETFAILURENODES STATE NIL)
      (COND (NIL NIL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (QESTATE_ISEMPTYBLOCKS STATE)) (NULL REMAININGVARIABLES)))
          (RETURN NIL)))
        (PROGN
         (PROG (G188)
           (SETQ G188 (QESTATE_FETCHBLOCK STATE))
           (SETQ QUANTIFIER (CAR G188))
           (SETQ VARIABLES (CDR G188))
           (RETURN G188))
         (COND
          ((NOT (QESTATE_ISEMPTYBLOCKS STATE))
           (PROGN (OFF1 'RLQEPRECISE) (OFF1 'RLQEAPRECISE))))
         (SETQ THEORY (QESTATE_GETTHEORY STATE))
         (SETQ FORMULA (QESTATE_GETFORMULA STATE))
         (COND ((EQ QUANTIFIER 'ALL) (SETQ FORMULA (RL_NNFNOT FORMULA))))
         (SETQ FORMULA (RL_SIMPL FORMULA THEORY (MINUS 1)))
         (QESTATE_SETFORMULA STATE FORMULA)
         (QESTATE_SETVARIABLES STATE VARIABLES)
         (SETQ PRODUCEANSWER
                 (AND (QESTATE_GETANSWERMODE STATE)
                      (QESTATE_ISEMPTYBLOCKS STATE)))
         (QESTATE_SETPRODUCEANSWER STATE PRODUCEANSWER)
         (CL_QEBLOCK4_NEW STATE)
         (SETQ SUCCESSNODES (QESTATE_FETCHSUCCESSNODES STATE))
         (SETQ FAILURENODES (QESTATE_FETCHFAILURENODES STATE))
         (COND
          ((EQ QUANTIFIER 'ALL)
           (PROGN
            (SETQ SUCCESSNODES
                    (PROG (NODE FORALL-RESULT FORALL-ENDPTR)
                      (SETQ NODE SUCCESSNODES)
                      (COND ((NULL NODE) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (NODE)
                                          (QENODE_NEW
                                           (QENODE_GETVARIABLES NODE)
                                           (RL_NNFNOT (QENODE_GETFORMULA NODE))
                                           (QENODE_GETANSWER NODE)))
                                        (CAR NODE))
                                       NIL)))
                     LOOPLABEL
                      (SETQ NODE (CDR NODE))
                      (COND ((NULL NODE) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (NODE)
                                  (QENODE_NEW (QENODE_GETVARIABLES NODE)
                                   (RL_NNFNOT (QENODE_GETFORMULA NODE))
                                   (QENODE_GETANSWER NODE)))
                                (CAR NODE))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ FAILURENODES
                    (PROG (NODE FORALL-RESULT FORALL-ENDPTR)
                      (SETQ NODE FAILURENODES)
                      (COND ((NULL NODE) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (NODE)
                                          (QENODE_NEW
                                           (QENODE_GETVARIABLES NODE)
                                           (RL_NNFNOT (QENODE_GETFORMULA NODE))
                                           (QENODE_GETANSWER NODE)))
                                        (CAR NODE))
                                       NIL)))
                     LOOPLABEL
                      (SETQ NODE (CDR NODE))
                      (COND ((NULL NODE) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (NODE)
                                  (QENODE_NEW (QENODE_GETVARIABLES NODE)
                                   (RL_NNFNOT (QENODE_GETFORMULA NODE))
                                   (QENODE_GETANSWER NODE)))
                                (CAR NODE))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ FORMULAOPERATOR 'AND)))
          (T (PROGN (SETQ FORMULAOPERATOR 'OR))))
         (SETQ FORMULAARGUMENTS NIL)
         (PROG (NODE)
           (SETQ NODE SUCCESSNODES)
          LAB
           (COND ((NULL NODE) (RETURN NIL)))
           ((LAMBDA (NODE)
              (PROG (W1)
                (SETQ W1 (QENODE_GETFORMULA NODE))
                (SETQ FORMULAARGUMENTS (CONS W1 FORMULAARGUMENTS))
                (RETURN W1)))
            (CAR NODE))
           (SETQ NODE (CDR NODE))
           (GO LAB))
         (PROG (NODE)
           (SETQ NODE FAILURENODES)
          LAB
           (COND ((NULL NODE) (RETURN NIL)))
           ((LAMBDA (NODE)
              (PROG (W1)
                (SETQ W1 (QENODE_GETFORMULA NODE))
                (SETQ FORMULAARGUMENTS (CONS W1 FORMULAARGUMENTS))
                (RETURN W1)))
            (CAR NODE))
           (SETQ NODE (CDR NODE))
           (GO LAB))
         (SETQ FORMULA
                 ((LAMBDA (G190)
                    (COND ((AND G190 (CDR G190)) (CONS FORMULAOPERATOR G190))
                          ((NULL G190)
                           (COND ((EQ FORMULAOPERATOR 'AND) 'TRUE) (T 'FALSE)))
                          (T (CAR G190))))
                  (REVERSIP FORMULAARGUMENTS)))
         (QESTATE_SETFORMULA STATE FORMULA)
         (COND
          ((NOT (NULL FAILURENODES))
           (PROG (NODE)
             (SETQ NODE FAILURENODES)
            LAB
             (COND ((NULL NODE) (RETURN NIL)))
             ((LAMBDA (NODE)
                (SETQ REMAININGVARIABLES
                        (UNION REMAININGVARIABLES (QENODE_GETVARIABLES NODE))))
              (CAR NODE))
             (SETQ NODE (CDR NODE))
             (GO LAB))))
         (COND
          ((NOT (QESTATE_ISEMPTYBLOCKS STATE))
           (PROGN
            (ONOFF 'RLQEPRECISE SVRLQEPRECISE)
            (ONOFF 'RLQEAPRECISE SVRLQEAPRECISE)))))
        (GO WHILELABEL))
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ FORMULA (QESTATE_GETFORMULA STATE))
      (SETQ FORMULA (RL_SIMPL FORMULA THEORY (MINUS 1)))
      (QESTATE_SETFORMULA STATE FORMULA)
      (COND
       ((NOT (NULL REMAININGVARIABLES))
        (QESTATE_ADDBLOCK STATE (CONS QUANTIFIER REMAININGVARIABLES))))
      (QESTATE_SETSUCCESSNODES STATE SUCCESSNODES)
      (QESTATE_SETFAILURENODES STATE FAILURENODES)
      (RETURN STATE))) 
(PUT 'CL_QEBLOCK4_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_QEBLOCK4_NEW (STATE)
    (PROG (FORMULA VARIABLES THEORY)
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ FORMULA (QESTATE_GETFORMULA STATE))
      (COND (*RLQEGSD (SETQ FORMULA (RL_GSN FORMULA THEORY 'DNF))))
      (SETQ VARIABLES (QESTATE_FETCHVARIABLES STATE))
      (QESTATE_INITIALIZEWORKINGNODES STATE)
      (COND
       ((EQ (COND ((ATOM FORMULA) FORMULA) (T (CAR FORMULA))) 'OR)
        (PROG (SUBFORMULA)
          (SETQ SUBFORMULA (CDR FORMULA))
         LAB
          (COND ((NULL SUBFORMULA) (RETURN NIL)))
          ((LAMBDA (SUBFORMULA)
             (QESTATE_ADDWORKINGNODES STATE
              (LIST (QENODE_NEW VARIABLES SUBFORMULA NIL))))
           (CAR SUBFORMULA))
          (SETQ SUBFORMULA (CDR SUBFORMULA))
          (GO LAB)))
       (T
        (QESTATE_ADDWORKINGNODES STATE
         (LIST (QENODE_NEW VARIABLES FORMULA NIL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (QESTATE_ISEMPTYWORKINGNODES STATE))) (RETURN NIL)))
        (PROGN
         (COND (*RLQEIDENTIFY (ON1 'RLIDENTIFY)))
         (COND ((AND *RLVERBOSE (QEENV_GETVB STATE)) (IOTO_PRIN2 "[")))
         (CL_QEVAR_NEW STATE)
         (COND ((AND *RLVERBOSE (QEENV_GETVB STATE)) (IOTO_PRIN2 "] ")))
         NIL)
        (GO WHILELABEL))
      (QESTATE_FETCHWORKINGNODEDELETIONS STATE)
      (RETURN STATE))) 
(PUT 'CL_QEVAR_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_QEVAR_NEW (STATE)
    (PROG (NODE FAILURENODES)
      (SETQ NODE (QESTATE_FETCHWORKINGNODE STATE))
      (QESTATE_SETCURRENTNODE STATE NODE)
      (CL_TRANSFORM_NEW STATE)
      (COND ((CL_GAUSS_NEW STATE) (RETURN STATE)))
      (COND ((CL_SPECELIMWRAPPER_NEW STATE) (RETURN STATE)))
      (COND ((CL_REGULARELIMINATIONSET STATE) (RETURN STATE)))
      (SETQ NODE (QESTATE_GETCURRENTNODE STATE))
      (QESTATE_SETCURRENTNODE STATE 'FALSE)
      (SETQ FAILURENODES (QESTATE_GETFAILURENODES STATE))
      (SETQ FAILURENODES (LTO_INSERT NODE FAILURENODES))
      (QESTATE_SETFAILURENODES STATE FAILURENODES)
      (RETURN STATE))) 
(PUT 'CL_TRANSFORM_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_TRANSFORM_NEW (STATE)
    (PROG (NODE THEORY NOASSUMEVARS PRODUCEANSWER VARIABLES ANSWER F W
           HASTRANSFORMED)
      (SETQ NODE (QESTATE_GETCURRENTNODE STATE))
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ NOASSUMEVARS (QESTATE_GETNOASSUMEVARS STATE))
      (SETQ PRODUCEANSWER (QESTATE_GETPRODUCEANWER STATE))
      (SETQ F (QENODE_GETFORMULA NODE))
      (SETQ VARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ ANSWER (QENODE_GETANSWER NODE))
      (SETQ HASTRANSFORMED NIL)
      (PROG (VARIABLE)
        (SETQ VARIABLE VARIABLES)
       LAB
        (COND ((NULL VARIABLE) (RETURN NIL)))
        ((LAMBDA (VARIABLE)
           (PROGN
            (SETQ W
                    (RL_TRANSFORM VARIABLE F VARIABLES ANSWER THEORY
                                  PRODUCEANSWER NOASSUMEVARS))
            (COND
             (W
              (PROGN
               (SETQ HASTRANSFORMED T)
               (PROG (G191 G192)
                 (SETQ G191 W)
                 (SETQ G192 G191)
                 (SETQ F (CAR G191))
                 (SETQ G191 (CDR G191))
                 (SETQ VARIABLES (CAR G191))
                 (SETQ G191 (CDR G191))
                 (SETQ ANSWER (CAR G191))
                 (SETQ G191 (CDR G191))
                 (SETQ THEORY (CAR G191))
                 (SETQ G191 (CDR G191))
                 (SETQ PRODUCEANSWER (CAR G191))
                 (SETQ G191 (CDR G191))
                 (SETQ NOASSUMEVARS (CAR G191))
                 (SETQ G191 (CDR G191))
                 (RETURN G192)))))
            NIL))
         (CAR VARIABLE))
        (SETQ VARIABLE (CDR VARIABLE))
        (GO LAB))
      (COND
       (HASTRANSFORMED
        (PROGN
         (SETQ NODE (QENODE_NEW VARIABLES F ANSWER))
         (QESTATE_SETCURRENTNODE STATE NODE)
         (QESTATE_SETTHEORY STATE THEORY)
         (QESTATE_SETNOASSUMEVARS STATE NOASSUMEVARS)
         (QESTATE_SETPRODUCEANSWER STATE PRODUCEANSWER))))
      (RETURN HASTRANSFORMED))) 
(PUT 'CL_GAUSS_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_GAUSS_NEW (STATE)
    (PROG (W NODE THEORY NOASSUMEVARS PRODUCEANSWER F VARIABLES ANSWER VARIABLE
           ELIMINATIONSET NODES)
      (SETQ NODE (QESTATE_GETCURRENTNODE STATE))
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ NOASSUMEVARS (QESTATE_GETNOASSUMEVARS STATE))
      (SETQ PRODUCEANSWER (QESTATE_GETPRODUCEANWER STATE))
      (SETQ F (QENODE_GETFORMULA NODE))
      (SETQ VARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ W (RL_TRYGAUSS F VARIABLES THEORY PRODUCEANSWER NOASSUMEVARS))
      (COND ((EQ W 'FAILED) (RETURN NIL)))
      (COND ((AND *RLVERBOSE (QEENV_GETVB STATE)) (IOTO_PRIN2 "g")))
      (SETQ ANSWER (QENODE_GETANSWER NODE))
      (PROG (G193)
        (SETQ G193 (CAR W))
        (SETQ VARIABLE (CAR G193))
        (SETQ ELIMINATIONSET (CDR G193))
        (RETURN G193))
      (SETQ THEORY (CDR W))
      (SETQ VARIABLES (LTO_DELQ VARIABLE VARIABLES))
      (PROG (G194)
        (SETQ G194
                (CL_ESETSUBST F VARIABLE ELIMINATIONSET VARIABLES ANSWER THEORY
                 PRODUCEANSWER NOASSUMEVARS))
        (SETQ NODES (CAR G194))
        (SETQ THEORY (CDR G194))
        (RETURN G194))
      (QESTATE_SETTHEORY STATE THEORY)
      (QESTATE_SETCURRENTNODE STATE 'FALSE)
      (CL_STOREREGULARNODES NODES (NOT (NULL VARIABLES)) STATE)
      (RETURN T))) 
(PUT 'CL_SPECELIMWRAPPER_NEW 'NUMBER-OF-ARGS 1) 
(DE CL_SPECELIMWRAPPER_NEW (STATE)
    (PROG (W NODE THEORY NOASSUMEVARS PRODUCEANSWER F VARIABLES NODES)
      (SETQ NODE (QESTATE_GETCURRENTNODE STATE))
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ NOASSUMEVARS (QESTATE_GETNOASSUMEVARS STATE))
      (SETQ PRODUCEANSWER (QESTATE_GETPRODUCEANWER STATE))
      (SETQ F (QENODE_GETFORMULA NODE))
      (SETQ VARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ W (RL_SPECELIM F VARIABLES THEORY PRODUCEANSWER NOASSUMEVARS))
      (COND ((EQ W 'FAILED) (RETURN NIL)))
      (PROG (G195)
        (SETQ G195 W)
        (SETQ W (CAR G195))
        (SETQ THEORY (CDR G195))
        (RETURN G195))
      (COND (NIL NIL))
      (SETQ NODES (CDR W))
      (QESTATE_SETTHEORY STATE THEORY)
      (QESTATE_SETCURRENTNODE STATE 'FALSE)
      (CL_STOREREGULARNODES NODES (NOT (NULL (CDR VARIABLES))) STATE)
      (RETURN T))) 
(PUT 'CL_REGULARELIMINATIONSET 'NUMBER-OF-ARGS 1) 
(DE CL_REGULARELIMINATIONSET (STATE)
    (PROG (PRODUCEANSWER NOASSUMEVARS THEORY NODE F NODEVARIABLES NODEANSWER
           FOUND CANDIDATEVARIABLES CANDIDATEVARIABLE ALP SUCCESSORNODES
           NEWTHEORY BESTSUCCESSORNODES BESTTHEORY SUCCESSORNODEVARIABLES
           NUMBEROFCANDIDATES)
      (SETQ NUMBEROFCANDIDATES 0)
      (SETQ PRODUCEANSWER (QESTATE_GETPRODUCEANWER STATE))
      (SETQ NOASSUMEVARS (QESTATE_GETNOASSUMEVARS STATE))
      (SETQ THEORY (QESTATE_GETTHEORY STATE))
      (SETQ NODE (QESTATE_GETCURRENTNODE STATE))
      (SETQ F (QENODE_GETFORMULA NODE))
      (SETQ NODEVARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ NODEANSWER (QENODE_GETANSWER NODE))
      (SETQ CANDIDATEVARIABLES
              (COND ((NULL (CDR NODEVARIABLES)) NODEVARIABLES)
                    ((NOT *RLQEVARSEL) (LIST (CAR NODEVARIABLES)))
                    (T (RL_VARSEL F NODEVARIABLES THEORY))))
      (SETQ FOUND NIL)
      (COND
       ((AND *RLVERBOSE (QEENV_GETVB STATE))
        (PROGN
         (SETQ NUMBEROFCANDIDATES (LENGTH CANDIDATEVARIABLES))
         (COND
          ((GREATERP NUMBEROFCANDIDATES 1)
           (IOTO_PRIN2 (LIST "{" NUMBEROFCANDIDATES ":")))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT CANDIDATEVARIABLES) (RETURN NIL)))
        (PROGN
         (SETQ CANDIDATEVARIABLE
                 (PROG1 (CAR CANDIDATEVARIABLES)
                   (SETQ CANDIDATEVARIABLES (CDR CANDIDATEVARIABLES))))
         (SETQ ALP (CL_QEATAL F CANDIDATEVARIABLE THEORY PRODUCEANSWER))
         (COND
          ((EQUAL ALP '(NIL))
           (PROGN
            (COND ((AND *RLVERBOSE (QEENV_GETVB STATE)) (IOTO_PRIN2 "*")))
            (SETQ BESTTHEORY THEORY)
            (SETQ SUCCESSORNODEVARIABLES
                    (LTO_DELQ CANDIDATEVARIABLE NODEVARIABLES))
            (COND
             (PRODUCEANSWER
              (SETQ NODEANSWER
                      (CL_UPDANS CANDIDATEVARIABLE 'ARBITRARY NIL NIL
                       NODEANSWER PRODUCEANSWER))))
            (SETQ BESTSUCCESSORNODES
                    (LIST (QENODE_NEW SUCCESSORNODEVARIABLES F NODEANSWER)))
            (SETQ FOUND T)
            (GO BRK)))
          ((NEQ (CAR ALP) 'FAILED)
           (PROGN
            (COND ((AND *RLVERBOSE (QEENV_GETVB STATE)) (IOTO_PRIN2 "e")))
            (PROG (G196)
              (SETQ G196
                      (CL_ESETSUBST_NEW NODE CANDIDATEVARIABLE
                       (RL_ELIMSET CANDIDATEVARIABLE ALP) STATE))
              (SETQ SUCCESSORNODES (CAR G196))
              (SETQ NEWTHEORY (CDR G196))
              (RETURN G196))
            (COND
             ((RL_BETTERP (CONS SUCCESSORNODES NEWTHEORY)
                          (CONS BESTSUCCESSORNODES BESTTHEORY))
              (PROGN
               (SETQ BESTTHEORY NEWTHEORY)
               (SETQ BESTSUCCESSORNODES SUCCESSORNODES)
               (SETQ FOUND T))))))))
        (GO WHILELABEL))
     BRK
      (COND
       ((AND *RLVERBOSE (QEENV_GETVB STATE) (GREATERP NUMBEROFCANDIDATES 1))
        (IOTO_PRIN2 (LIST "}"))))
      (COND ((NOT FOUND) (RETURN NIL)))
      (QESTATE_SETTHEORY STATE BESTTHEORY)
      (CL_STOREREGULARNODES BESTSUCCESSORNODES (NOT (NULL (CDR NODEVARIABLES)))
       STATE)
      (QESTATE_SETCURRENTNODE STATE 'FALSE)
      (RETURN T))) 
(PUT 'CL_STOREREGULARNODES 'NUMBER-OF-ARGS 3) 
(DE CL_STOREREGULARNODES (NODES VARIABLESLEFT STATE)
    (PROG (SUCCESSNODES)
      (COND
       ((NOT (NULL NODES))
        (PROGN
         (COND
          ((EQ (QENODE_GETVARIABLES (CAR NODES)) 'BREAK)
           (PROGN
            (QESTATE_INITIALIZEWORKINGNODES STATE)
            (SETQ SUCCESSNODES (QESTATE_GETSUCCESSNODES STATE))
            (SETQ SUCCESSNODES
                    (LIST
                     (QENODE_NEW NIL 'TRUE (QENODE_GETANSWER (CAR NODES)))))
            (QESTATE_SETSUCCESSNODES STATE SUCCESSNODES)
            (QESTATE_SETFAILURENODES STATE NIL)))
          (VARIABLESLEFT (PROGN (QESTATE_ADDWORKINGNODES STATE NODES)))
          (T
           (PROGN
            (SETQ SUCCESSNODES (QESTATE_GETSUCCESSNODES STATE))
            (PROG (NODE)
              (SETQ NODE NODES)
             LAB
              (COND ((NULL NODE) (RETURN NIL)))
              ((LAMBDA (NODE)
                 (SETQ SUCCESSNODES (LTO_INSERT NODE SUCCESSNODES)))
               (CAR NODE))
              (SETQ NODE (CDR NODE))
              (GO LAB))
            (QESTATE_SETSUCCESSNODES STATE SUCCESSNODES)))))))
      (RETURN STATE))) 
(PUT 'CL_ESETSUBST_NEW 'NUMBER-OF-ARGS 4) 
(DE CL_ESETSUBST_NEW (NODE VARIABLE ELIMINATIONSET STATE)
    (PROG (PRODUCEANSWER NOASSUMEVARS CURRENTTHEORY F NODEVARIABLES NODEANSWER
           SUCCESSORNODEVARIABLES SUCCESSORNODES NEWTHEORY)
      (SETQ PRODUCEANSWER (QESTATE_GETPRODUCEANWER STATE))
      (SETQ NOASSUMEVARS (QESTATE_GETNOASSUMEVARS STATE))
      (SETQ CURRENTTHEORY (QESTATE_GETTHEORY STATE))
      (SETQ F (QENODE_GETFORMULA NODE))
      (SETQ NODEVARIABLES (QENODE_GETVARIABLES NODE))
      (SETQ NODEANSWER (QENODE_GETANSWER NODE))
      (SETQ SUCCESSORNODEVARIABLES (LTO_DELQ VARIABLE NODEVARIABLES))
      (PROG (G197)
        (SETQ G197
                (CL_ESETSUBST F VARIABLE ELIMINATIONSET SUCCESSORNODEVARIABLES
                 NODEANSWER CURRENTTHEORY PRODUCEANSWER NOASSUMEVARS))
        (SETQ SUCCESSORNODES (CAR G197))
        (SETQ NEWTHEORY (CDR G197))
        (RETURN G197))
      (RETURN (CONS SUCCESSORNODES NEWTHEORY)))) 
(PUT 'CL_ESETSUBST 'NUMBER-OF-ARGS 8) 
(DE CL_ESETSUBST (F V ESET VL AN THEO ANS BVL)
    (PROG (A D U ELIMRES JUNCT W)
      (PROG ()
       WHILELABEL
        (COND ((NOT ESET) (RETURN NIL)))
        (PROGN
         (PROG (G198)
           (SETQ G198 (PROG1 (CAR ESET) (SETQ ESET (CDR ESET))))
           (SETQ A (CAR G198))
           (SETQ D (CDR G198))
           (RETURN G198))
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
(PUT 'CL_BETTERP_NEW 'NUMBER-OF-ARGS 2) 
(DE CL_BETTERP_NEW (CURRENT BEST)
    (PROG (CURRENTNUMBEROFNODES BESTNUMBEROFNODES)
      (SETQ CURRENTNUMBEROFNODES 0)
      (SETQ BESTNUMBEROFNODES 0)
      (SETQ CURRENTNUMBEROFNODES
              (PROG (NODE FORALL-RESULT)
                (SETQ NODE (CAR CURRENT))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL NODE) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS
                         ((LAMBDA (NODE) (RL_ATNUM (QENODE_GETFORMULA NODE)))
                          (CAR NODE))
                         FORALL-RESULT))
                (SETQ NODE (CDR NODE))
                (GO LAB1)))
      (COND
       ((AND *RLVERBOSE (QEENV_GETVB (QEENV_NEW)))
        (IOTO_PRIN2 (LIST "(" CURRENTNUMBEROFNODES ")"))))
      (COND ((EQUAL BEST '(NIL)) (RETURN T)))
      (SETQ BESTNUMBEROFNODES
              (PROG (NODE FORALL-RESULT)
                (SETQ NODE (CAR BEST))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL NODE) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS
                         ((LAMBDA (NODE) (RL_ATNUM (QENODE_GETFORMULA NODE)))
                          (CAR NODE))
                         FORALL-RESULT))
                (SETQ NODE (CDR NODE))
                (GO LAB1)))
      (RETURN (LESSP CURRENTNUMBEROFNODES BESTNUMBEROFNODES)))) 
(ENDMODULE) 