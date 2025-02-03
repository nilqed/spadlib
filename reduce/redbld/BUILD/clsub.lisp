(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLSUB)) 
(REVISION 'CLSUB "$Id: clsub.red 5972 2021-08-24 18:53:09Z thomas-sturm $") 
(COPYRIGHT 'CLSUB "(c) 2021 A. Dolzmann, T. Sturm") 
(RL_PROVIDESERVICE 'RL_SUBFOF 'CL_SUBFOF
                   '(RL_SUBAT RL_EQNRHSKERNELS RL_SUBALCHK RL_VARSUBSTAT)) 
(PUT 'CL_SUBFOF 'NUMBER-OF-ARGS 2) 
(DE CL_SUBFOF (AL F)
    (PROG (ASGAL W ALLVL)
      (RL_SUBALCHK AL)
      (PROG (X)
        (SETQ X AL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ W (RL_EQNRHSKERNELS X))
            (SETQ ASGAL (LTO_ALUNION (LIST (LIST (CONS (CAR X) W)) ASGAL)))
            (SETQ ALLVL (CONS (CAR X) (APPEND W ALLVL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ W (CL_VARL1 F))
      (SETQ ALLVL (LTO_NCONCN (LIST ALLVL (CAR W) (CDR W))))
      (RETURN (CL_SUBFOF1 AL F ASGAL ALLVL)))) 
(PUT 'CL_SUBFOF1 'NUMBER-OF-ARGS 4) 
(DE CL_SUBFOF1 (AL F ASGAL ALLVL)
    (PROG (OP V NEWV M B)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (SETQ V (CADR F))
         (SETQ M (CADDR F))
         (SETQ AL
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X AL)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (SETQ ASGAL
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X ASGAL)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X) (COND ((NOT (EQCAR X V)) (LIST X))))
                            (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (SETQ NEWV (CL_NEWV V M ASGAL ALLVL))
         (COND
          ((NEQ NEWV V)
           (PROGN
            (SETQ ALLVL (CONS NEWV ALLVL))
            (SETQ M (CL_SUBVARSUBSTAT NEWV V M)))))
         (RETURN (LIST OP NEWV (CL_SUBFOF1 AL M ASGAL ALLVL))))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (RETURN (RL_BQSUBFOF1 AL F ASGAL ALLVL))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CDR F))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X) (CL_SUBFOF1 AL X ASGAL ALLVL))
                                   (CAR X))
                                  NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (CL_SUBFOF1 AL X ASGAL ALLVL)) (CAR X))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND (NIL NIL))
      (RETURN (RL_SUBAT AL F)))) 
(PUT 'CL_NEWV 'NUMBER-OF-ARGS 4) 
(DE CL_NEWV (V M ASGAL ALLVL)
    (PROG (A FVL W NEWV N)
      (SETQ N 0)
      (SETQ NEWV V)
      (SETQ FVL (CL_SUBFVARL M))
      (PROG ()
       WHILELABEL
        (COND ((NOT FVL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR FVL))
         (SETQ FVL (CDR FVL))
         (COND
          ((AND (SETQ W (ATSOC A ASGAL)) (MEMQ V W))
           (PROGN
            (PROG ()
             REPEATLABEL
              (PROGN (SETQ NEWV (MKID V N)) (SETQ N (PLUS N 1)))
              (COND
               ((NOT (NOT (OR (MEMQ NEWV ALLVL) (GET V 'AVALUE))))
                (GO REPEATLABEL))))
            (SETQ FVL NIL)))))
        (GO WHILELABEL))
      (RETURN NEWV))) 
(PUT 'CL_SUBVARSUBSTAT 'NUMBER-OF-ARGS 3) 
(DE CL_SUBVARSUBSTAT (NEWV OLDV F)
    (PROG (OP)
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (COND
         ((EQ (CADR F) OLDV)
          (RETURN (LIST OP NEWV (CL_SUBVARSUBSTAT NEWV OLDV (CADDR F)))))
         (T
          (RETURN
           (LIST OP (CADR F) (CL_SUBVARSUBSTAT NEWV OLDV (CADDR F))))))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CDR F))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X) (CL_SUBVARSUBSTAT NEWV OLDV X))
                                   (CAR X))
                                  NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (CL_SUBVARSUBSTAT NEWV OLDV X)) (CAR X))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (RL_VARSUBSTAT F NEWV OLDV)))) 
(PUT 'CL_SUBFVARL 'NUMBER-OF-ARGS 1) 
(DE CL_SUBFVARL (M) (CL_SUBFVARL1 M NIL)) 
(PUT 'CL_SUBFVARL1 'NUMBER-OF-ARGS 2) 
(DE CL_SUBFVARL1 (F CBVL)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (CL_SUBFVARL1 (CADDR F) (CONS (CADR F) CBVL)))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X (CDR F))
          STARTOVER
           (COND ((NULL X) (RETURN NIL)))
           (SETQ FORALL-RESULT ((LAMBDA (X) (CL_SUBFVARL1 X CBVL)) (CAR X)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ X (CDR X))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL X) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR ((LAMBDA (X) (CL_SUBFVARL1 X CBVL)) (CAR X)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ X (CDR X))
           (GO LOOPLABEL)))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN NIL)))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (RL_VARLAT F))
        STARTOVER
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (X) (COND ((NOT (MEMQ X CBVL)) (LIST X)))) (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ X (CDR X))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (X) (COND ((NOT (MEMQ X CBVL)) (LIST X)))) (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ X (CDR X))
         (GO LOOPLABEL))))) 
(PUT 'CL_REPLACE 'NUMBER-OF-ARGS 2) 
(DE CL_REPLACE (F SAL) (COND (SAL (CL_REPLACE1 F SAL)) (T F))) 
(PUT 'CL_REPLACE1 'NUMBER-OF-ARGS 2) 
(DE CL_REPLACE1 (F SAL)
    (PROG (W OP)
      (COND ((SETQ W (ASSOC F SAL)) (RETURN (CDR W))))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (CL_REPLACE (CADDR F) SAL)))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQREPLACE1 F SAL))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR F))
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SUBF) (CL_REPLACE SUBF SAL))
                                   (CAR SUBF))
                                  NIL)))
                LOOPLABEL
                 (SETQ SUBF (CDR SUBF))
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (SUBF) (CL_REPLACE SUBF SAL)) (CAR SUBF))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND (NIL NIL))
      (RETURN F))) 
(PUT 'VSUB 'PSOPFN 'CL_VSUBEVAL) 
(PUT 'CL_VSUBEVAL 'NUMBER-OF-ARGS 1) 
(DE CL_VSUBEVAL (U)
    (PROG (AL F)
      (COND ((NULL U) (REDERR "no arguments in vsub")))
      (SETQ U (REVERSE U))
      (SETQ F (RL_SIMP (PROG1 (CAR U) (SETQ U (CDR U)))))
      (COND ((NULL U) (RETURN (RL_MK*FOF F))))
      (SETQ U (REVERSIP U))
      (COND
       ((AND (NULL (CDR U)) (EQCAR (CAR U) 'LIST)) (SETQ U (CDR (CAR U)))))
      (SETQ AL
              (PROG (EQN FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQN U)
                (COND ((NULL EQN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQN)
                                    (PROGN
                                     (COND
                                      ((NOT (EQCAR EQN 'EQUAL))
                                       (REDERR
                                        (LIST EQN
                                              "invalid as equation in vsub"))))
                                     (CONS (CADR EQN) (CADDR EQN))))
                                  (CAR EQN))
                                 NIL)))
               LOOPLABEL
                (SETQ EQN (CDR EQN))
                (COND ((NULL EQN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQN)
                            (PROGN
                             (COND
                              ((NOT (EQCAR EQN 'EQUAL))
                               (REDERR
                                (LIST EQN "invalid as equation in vsub"))))
                             (CONS (CADR EQN) (CADDR EQN))))
                          (CAR EQN))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RL_VSUBALCHK AL)
      (RETURN (RL_MK*FOF (CL_VSUBFOF AL F))))) 
(PUT 'CL_VSUBFOF 'NUMBER-OF-ARGS 2) 
(DE CL_VSUBFOF (SUBL F)
    (PROGN
     (PROG (PR)
       (SETQ PR SUBL)
      LAB
       (COND ((NULL PR) (RETURN NIL)))
       ((LAMBDA (PR) (SETQ F (CL_VSUBFOF1 (CAR PR) (CDR PR) F))) (CAR PR))
       (SETQ PR (CDR PR))
       (GO LAB))
     F)) 
(PUT 'CL_VSUBFOF1 'NUMBER-OF-ARGS 3) 
(DE CL_VSUBFOF1 (V U F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN
         (COND ((EQ (CADR F) V) F)
               (T (LIST OP (CADR F) (CL_VSUBFOF1 V U (CADDR F))))))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (REDERR "cl_vsubfof1: bounded quantifiers are not supported")))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CDR F))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X) (CL_VSUBFOF1 V U X)) (CAR X))
                                  NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CL_VSUBFOF1 V U X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (RL_VSUBAT V U F)))) 
(ENDMODULE) 