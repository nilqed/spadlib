(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RED)) 
(SETQ *RED_TOTAL T) 
(SETQ *BCSIMP T) 
(PUT 'RED_BETTER 'NUMBER-OF-ARGS 2) 
(PUT 'RED_BETTER 'DEFINED-ON-LINE '97) 
(PUT 'RED_BETTER 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_BETTER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_BETTER (A B) (LESSP (BAS_DPLEN A) (BAS_DPLEN B))) 
(PUT 'RED_PREPARE 'NUMBER-OF-ARGS 1) 
(PUT 'RED_PREPARE 'DEFINED-ON-LINE '105) 
(PUT 'RED_PREPARE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_PREPARE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_PREPARE (MODEL)
    (BAS_MAKE1 (BAS_NR MODEL) (BAS_DPOLY MODEL)
     (DP_SUM (BAS_REP MODEL) (DP_FROM_EI 0)))) 
(PUT 'RED_EXTRACT 'NUMBER-OF-ARGS 1) 
(PUT 'RED_EXTRACT 'DEFINED-ON-LINE '112) 
(PUT 'RED_EXTRACT 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_EXTRACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_EXTRACT (MODEL)
    ((LAMBDA (Z)
       (CONS
        (BAS_MAKE1 (BAS_NR MODEL) (BAS_DPOLY MODEL)
         (DP_DIFF (BAS_REP MODEL) Z))
        Z))
     (DP_COMP 0 (BAS_REP MODEL)))) 
(PUT 'RED_SUBST 'NUMBER-OF-ARGS 2) 
(PUT 'RED_SUBST 'DEFINED-ON-LINE '122) 
(PUT 'RED_SUBST 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_SUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_SUBST (MODEL BASEL) (RED=SUBST1 MODEL BASEL)) 
(PUT 'RED=SUBST1 'NUMBER-OF-ARGS 2) 
(PUT 'RED=SUBST1 'DEFINED-ON-LINE '132) 
(PUT 'RED=SUBST1 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=SUBST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED=SUBST1 (MODEL BASEL)
    (PROG (POLOLD POLNEW REPOLD REPNEW GCD MO FA Z Z1)
      (SETQ POLOLD (BAS_DPOLY MODEL))
      (SETQ Z1 (DP_LC POLOLD))
      (SETQ REPOLD (BAS_REP MODEL))
      (SETQ FA (BAS_DPOLY BASEL))
      (SETQ Z (DP_LC FA))
      (COND
       (*BCSIMP
        (COND
         ((SETQ GCD (CALI_BC_INV Z))
          (PROGN (SETQ Z1 (CALI_BC_PROD Z1 GCD)) (SETQ Z (CALI_BC_FI 1))))
         (T
          (PROGN
           (SETQ GCD (CALI_BC_GCD Z Z1))
           (SETQ Z (CAR (CALI_BC_DIVMOD Z GCD)))
           (SETQ Z1 (CAR (CALI_BC_DIVMOD Z1 GCD))))))))
      (SETQ MO (MO_DIFF (DP_LMON POLOLD) (DP_LMON FA)))
      (SETQ POLNEW (DP_DIFF (DP_TIMES_BC Z POLOLD) (DP_TIMES_BCMO Z1 MO FA)))
      (SETQ REPNEW
              (DP_DIFF (DP_TIMES_BC Z REPOLD)
               (DP_TIMES_BCMO Z1 MO (BAS_REP BASEL))))
      (COND
       ((GREATERP (CALI_TRACE) 79) (PROGN (PRIN2 "---> ") (DP_PRINT POLNEW)))
       ((GREATERP (CALI_TRACE) 0) (PRIN2 ".")))
      (COND
       ((GREATERP (CALI_TRACE) 89) (PROGN (PRIN2 " uses ") (DP_PRINT FA))))
      (RETURN (BAS_MAKE1 (BAS_NR MODEL) POLNEW REPNEW)))) 
(PUT 'RED=SUBST2 'NUMBER-OF-ARGS 2) 
(PUT 'RED=SUBST2 'DEFINED-ON-LINE '158) 
(PUT 'RED=SUBST2 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=SUBST2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED=SUBST2 (MODEL BASEL)
    (PROG (M B U R)
      (COND ((GREATERP (CALI_TRACE) 0) (PRIN2 ".")))
      (SETQ M (BAS_DPOLY MODEL))
      (SETQ B (BAS_DPOLY BASEL))
      (COND
       ((OR (NEQ (LENGTH B) 2) (BAS_REP MODEL))
        (REDERR "switch off binomial")))
      (SETQ U (MO_QREM (DP_LMON M) (DP_LMON B)))
      (SETQ R
              (LIST
               (DP_TERM (DP_LC M)
                (MO_SUM (MO_POWER (DP_LMON (CDR B)) (CAR U)) (CDR U)))))
      (RETURN (BAS_MAKE (BAS_NR MODEL) (DP_SUM R (CDR M)))))) 
(PUT 'RED_TOPREDBE 'NUMBER-OF-ARGS 2) 
(PUT 'RED_TOPREDBE 'DEFINED-ON-LINE '173) 
(PUT 'RED_TOPREDBE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TOPREDBE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_TOPREDBE (BAS MODEL)
    (COND ((OR (NULL (BAS_DPOLY MODEL)) (NULL BAS)) MODEL)
          (T
           (PROG (V Q)
             (COND
              ((GREATERP (CALI_TRACE) 79)
               (PROGN
                (PROGN (PRIN2 " reduce ") NIL)
                (DP_PRINT (BAS_DPOLY MODEL)))))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (SETQ Q (BAS_DPOLY MODEL))
                       (SETQ V
                               (RED_DIVTESTBE BAS (DP_LMON Q)
                                (BAS_DPECART MODEL)))))
                 (RETURN NIL)))
               (SETQ MODEL (RED_SUBST MODEL V))
               (GO WHILELABEL))
             (RETURN MODEL))))) 
(PUT 'RED_DIVTESTBE 'NUMBER-OF-ARGS 3) 
(PUT 'RED_DIVTESTBE 'DEFINED-ON-LINE '187) 
(PUT 'RED_DIVTESTBE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_DIVTESTBE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RED_DIVTESTBE (A B E)
    (COND ((NULL A) NIL)
          ((AND (LEQ (BAS_DPECART (CAR A)) E)
                (MO_VDIVIDES? (DP_LMON (BAS_DPOLY (CAR A))) B))
           (CAR A))
          (T (RED_DIVTESTBE (CDR A) B E)))) 
(PUT 'RED_DIVTEST 'NUMBER-OF-ARGS 2) 
(PUT 'RED_DIVTEST 'DEFINED-ON-LINE '195) 
(PUT 'RED_DIVTEST 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_DIVTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_DIVTEST (A B)
    (COND ((NULL A) NIL)
          ((MO_VDIVIDES? (DP_LMON (BAS_DPOLY (CAR A))) B) (CAR A))
          (T (RED_DIVTEST (CDR A) B)))) 
(PUT 'RED_TOPRED 'NUMBER-OF-ARGS 2) 
(PUT 'RED_TOPRED 'DEFINED-ON-LINE '202) 
(PUT 'RED_TOPRED 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TOPRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_TOPRED (BAS MODEL)
    (COND ((OR (NULL (BAS_DPOLY MODEL)) (NULL BAS)) MODEL)
          (T
           (PROG (V Q)
             (SETQ MODEL (RED_TOPREDBE BAS MODEL))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (SETQ Q (BAS_DPOLY MODEL))
                       (SETQ V (RED_DIVTEST BAS (DP_LMON Q)))))
                 (RETURN NIL)))
               (PROGN
                (SETQ V (RED_SUBST MODEL V))
                (COND ((NOT *NOETHERIAN) (SETQ BAS (RED_UPDATE BAS MODEL))))
                (SETQ MODEL (RED_TOPREDBE BAS V))
                NIL)
               (GO WHILELABEL))
             (RETURN MODEL))))) 
(PUT 'RED_UPDATE 'NUMBER-OF-ARGS 2) 
(PUT 'RED_UPDATE 'DEFINED-ON-LINE '224) 
(PUT 'RED_UPDATE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_UPDATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_UPDATE (SIMP B)
    (PROG ()
      (COND
       ((GREATERP (CALI_TRACE) 59)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "[ec:") (PRIN2 (BAS_DPECART B)) (PRIN2 "] ->") NIL)
         (DP_PRINT2 (BAS_DPOLY B))))
       ((GREATERP (CALI_TRACE) 0) (PROGN (PRIN2 "*") NIL)))
      (RETURN
       (MERGE (LIST B)
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X SIMP)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND ((RED=CANCELSIMP B X) NIL) (T (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND ((RED=CANCELSIMP B X) NIL) (T (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL))
              (FUNCTION RED_BETTER))))) 
(PUT 'RED=CANCELSIMP 'NUMBER-OF-ARGS 2) 
(PUT 'RED=CANCELSIMP 'DEFINED-ON-LINE '238) 
(PUT 'RED=CANCELSIMP 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=CANCELSIMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED=CANCELSIMP (A B)
    (AND (RED_BETTER A B)
         (MO_VDIVIDES? (DP_LMON (BAS_DPOLY A)) (DP_LMON (BAS_DPOLY B))))) 
(PUT 'RED=HIDE 'NUMBER-OF-ARGS 1) 
(PUT 'RED=HIDE 'DEFINED-ON-LINE '256) 
(PUT 'RED=HIDE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=HIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED=HIDE (P)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X P)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS (MO_TIMES_EI (MINUS 1) (MO_NEG (CAR X)))
                                (CDR X)))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (CONS (MO_TIMES_EI (MINUS 1) (MO_NEG (CAR X))) (CDR X)))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'RED=HIDELT 'NUMBER-OF-ARGS 1) 
(PUT 'RED=HIDELT 'DEFINED-ON-LINE '260) 
(PUT 'RED=HIDELT 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=HIDELT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED=HIDELT (MODEL)
    ((LAMBDA (P)
       (BAS_MAKE1 (BAS_NR MODEL) (CDR P)
        (DP_SUM (BAS_REP MODEL) (RED=HIDE (LIST (CAR P))))))
     (BAS_DPOLY MODEL))) 
(PUT 'RED=RECOVER 'NUMBER-OF-ARGS 1) 
(PUT 'RED=RECOVER 'DEFINED-ON-LINE '265) 
(PUT 'RED=RECOVER 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED=RECOVER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED=RECOVER (MODEL)
    (PROG (U V)
      (PROG (X)
        (SETQ X (BAS_REP MODEL))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((LESSP (COND ((NULL (CAR (CAR X))) 0) (T (CAAR (CAR X)))) 0)
             (SETQ U (CONS X U)))
            (T (SETQ V (CONS X V)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN
       (BAS_MAKE1 (BAS_NR MODEL) (DP_NEWORDER (REVERSIP (RED=HIDE U)))
        (REVERSIP V))))) 
(PUT 'RED_TAILREDDRIVER 'NUMBER-OF-ARGS 3) 
(PUT 'RED_TAILREDDRIVER 'DEFINED-ON-LINE '275) 
(PUT 'RED_TAILREDDRIVER 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TAILREDDRIVER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RED_TAILREDDRIVER (BAS MODEL REDFCTN)
    (COND
     ((OR (NULL (BAS_DPOLY MODEL)) (NULL (CDR (BAS_DPOLY MODEL))) (NULL BAS))
      MODEL)
     (T
      (PROG ()
        (PROG ()
         WHILELABEL
          (COND ((NOT (BAS_DPOLY MODEL)) (RETURN NIL)))
          (SETQ MODEL (APPLY2 REDFCTN BAS (RED=HIDELT MODEL)))
          (GO WHILELABEL))
        (RETURN (RED=RECOVER MODEL)))))) 
(PUT 'RED_TAILRED 'NUMBER-OF-ARGS 2) 
(PUT 'RED_TAILRED 'DEFINED-ON-LINE '286) 
(PUT 'RED_TAILRED 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TAILRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_TAILRED (BAS MODEL)
    (COND (*NOETHERIAN (RED_TAILREDDRIVER BAS MODEL (FUNCTION RED_TOPRED)))
          (T (RED_TAILREDDRIVER BAS MODEL (FUNCTION RED_TOPREDBE))))) 
(PUT 'RED_TOTALRED 'NUMBER-OF-ARGS 2) 
(PUT 'RED_TOTALRED 'DEFINED-ON-LINE '292) 
(PUT 'RED_TOTALRED 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TOTALRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_TOTALRED (BAS MODEL) (RED_TAILRED BAS (RED_TOPRED BAS MODEL))) 
(PUT 'RED_STRAIGHT 'NUMBER-OF-ARGS 1) 
(PUT 'RED_STRAIGHT 'DEFINED-ON-LINE '300) 
(PUT 'RED_STRAIGHT 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_STRAIGHT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_STRAIGHT (BAS)
    (PROG (U)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X BAS)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (RED_TAILRED BAS X)) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (RED_TAILRED BAS X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*BCSIMP (SETQ U (BAS_SIMP U))))
      (RETURN (SORT U (FUNCTION RED_BETTER))))) 
(PUT 'RED_COLLECT 'NUMBER-OF-ARGS 1) 
(PUT 'RED_COLLECT 'DEFINED-ON-LINE '309) 
(PUT 'RED_COLLECT 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_COLLECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_COLLECT (BAS)
    (PROG (BAS1 BAS2)
      (SETQ BAS1
              (LISTMINIMIZE BAS
                            (FUNCTION
                             (LAMBDA (X Y)
                               (MO_VDIVIDES? (DP_LMON (BAS_DPOLY X))
                                (DP_LMON (BAS_DPOLY Y)))))))
      (SETQ BAS2 (SETDIFF BAS BAS1))
      (RETURN (CONS BAS1 BAS2)))) 
(PUT 'RED_TOPINTERREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'RED_TOPINTERREDUCE 'DEFINED-ON-LINE '318) 
(PUT 'RED_TOPINTERREDUCE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_TOPINTERREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_TOPINTERREDUCE (M)
    (PROG (C W BAS1)
      (SETQ M (BAS_SORT (BAS_ZERODELETE M)))
      (COND (*BCSIMP (SETQ M (BAS_SIMP M))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR (SETQ C (RED_COLLECT M)))) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (CALI_TRACE) 69)
           (PROGN (PROGN (PRIN2 " interreduce ") NIL) (TERPRI) (BAS_PRINT M))))
         (SETQ M NIL)
         (SETQ W (CDR C))
         (SETQ BAS1 (CAR C))
         (PROG ()
          WHILELABEL
           (COND ((NOT W) (RETURN NIL)))
           (PROGN
            (SETQ C (RED_TOPRED BAS1 (CAR W)))
            (COND ((BAS_DPOLY C) (SETQ M (CONS C M))))
            (SETQ W (CDR W)))
           (GO WHILELABEL))
         (COND (*BCSIMP (SETQ M (BAS_SIMP M))))
         (SETQ M (MERGE BAS1 (BAS_SORT M) (FUNCTION RED_BETTER)))
         NIL)
        (GO WHILELABEL))
      (RETURN M))) 
(PUT 'RED_REDPOL 'NUMBER-OF-ARGS 2) 
(PUT 'RED_REDPOL 'DEFINED-ON-LINE '343) 
(PUT 'RED_REDPOL 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_REDPOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RED_REDPOL (BAS MODEL)
    (PROG (M)
      (SETQ M (RED_PREPARE MODEL))
      (RETURN
       (RED_EXTRACT
        (COND (*RED_TOTAL (RED_TOTALRED BAS M)) (T (RED_TOPRED BAS M))))))) 
(PUT 'RED_INTERREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'RED_INTERREDUCE 'DEFINED-ON-LINE '351) 
(PUT 'RED_INTERREDUCE 'DEFINED-IN-FILE 'CALI/RED.RED) 
(PUT 'RED_INTERREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RED_INTERREDUCE (M)
    (PROG ()
      (SETQ M (RED_TOPINTERREDUCE M))
      (COND (*RED_TOTAL (SETQ M (RED_STRAIGHT M))))
      (RETURN M))) 
(ENDMODULE) 