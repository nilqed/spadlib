(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'IDENTITIES)) 
(PUT 'DROP_IDTY 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_IDTY 'DEFINED-ON-LINE '33) 
(PUT 'DROP_IDTY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'DROP_IDTY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_IDTY (ID)
    (PROGN
     (SETPROP ID NIL)
     (SETQ RECYCLE_IDS (CONS ID RECYCLE_IDS))
     (SETQ IDNTIES_ (DELETE ID IDNTIES_)))) 
(PUT 'NEW_ID_NAME 'NUMBER-OF-ARGS 0) 
(PUT 'NEW_ID_NAME 'DEFINED-ON-LINE '39) 
(PUT 'NEW_ID_NAME 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'NEW_ID_NAME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEW_ID_NAME NIL
    (PROG (ID)
      (COND
       ((NULL RECYCLE_IDS)
        (PROGN (SETQ ID (MKID IDNAME_ NID_)) (SETQ NID_ (ADD1 NID_)) NIL))
       (T
        (PROGN
         (SETQ ID (CAR RECYCLE_IDS))
         (SETQ RECYCLE_IDS (CDR RECYCLE_IDS)))))
      (SETPROP ID NIL)
      (RETURN ID))) 
(PUT 'REPLACE_IDTY 'NUMBER-OF-ARGS 0) 
(PUT 'REPLACE_IDTY 'DEFINED-ON-LINE '53) 
(PUT 'REPLACE_IDTY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'REPLACE_IDTY 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REPLACE_IDTY NIL
    (PROG (P EX)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN
       (PRIN2 "If you want to replace an identity then ")
       (PRIN2 "type its name, e.g. id_2 <ENTER>.")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "If you want to add an identity then type `new_idty' <ENTER>. ")
       NIL)
      (SETQ P (TERMREAD))
      (COND
       ((OR (EQUAL P 'NEW_IDTY) (MEMBER P IDNTIES_))
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Input of a value for ") NIL)
         (COND ((EQUAL P 'NEW_IDTY) (PROGN (PRIN2 "the new identity.") NIL))
               (T (PROGN (PRIN2 P) (PRIN2 ".") NIL)))
         (TERPRI)
         (PROGN
          (PRIN2
           "You can use names of other identities, e.g. 3*id_2 - df(id_1,x); ")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "Terminate the expression with ; or $ : ") NIL)
         (TERPRI)
         (SETQ EX (TERMXREAD))
         (PROG (A)
           (SETQ A IDNTIES_)
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (SETQ EX (SUBST (GET A 'VAL) A EX))) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (COND ((NEQ P 'NEW_IDTY) (DROP_IDTY P)))
         (NEW_IDTY (REVAL1 EX T) NIL NIL)
         (TERPRI)
         (PROGN (PRIN2 (CAR IDNTIES_)) NIL)
         (COND ((EQUAL P 'NEW_IDTY) (PROGN (PRIN2 " is added") NIL))
               (T (PROGN (PRIN2 " replaces ") (PRIN2 P) NIL)))
         NIL))
       (T
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "An identity ")
          (PRIN2 P)
          (PRIN2 " does not exist! (Back to previous menu)")
          NIL))))
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'TRIVIAL_IDTY 'NUMBER-OF-ARGS 2) 
(PUT 'TRIVIAL_IDTY 'DEFINED-ON-LINE '85) 
(PUT 'TRIVIAL_IDTY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'TRIVIAL_IDTY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIVIAL_IDTY (PDES IDVAL)
    (COND
     ((OR (ZEROP IDVAL)
          (AND PDES (NULL (SMEMBERL PDES (SEARCH_LI IDVAL 'DF)))))
      T)
     (T NIL))) 
(PUT 'NEW_IDTY 'NUMBER-OF-ARGS 3) 
(PUT 'NEW_IDTY 'DEFINED-ON-LINE '91) 
(PUT 'NEW_IDTY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'NEW_IDTY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEW_IDTY (IDTY PDES SIMPLIFY)
    (PROG (ID IDCP)
      (COND
       (SIMPLIFY
        (PROGN
         (SETQ ID CONTRADICTION_)
         (SETQ IDTY (PREPSQ (CAR (SIMPLIFYPDESQ (SIMP IDTY) PDES T NIL NIL))))
         (SETQ CONTRADICTION_ ID))))
      (COND
       ((NOT (TRIVIAL_IDTY PDES IDTY))
        (PROGN
         (SETQ IDCP IDNTIES_)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND IDCP (NEQ (GET (CAR IDCP) 'VAL) IDTY))) (RETURN NIL)))
           (SETQ IDCP (CDR IDCP))
           (GO WHILELABEL))
         (COND
          ((NULL IDCP)
           (PROGN
            (SETQ ID (NEW_ID_NAME))
            (PUT ID 'VAL IDTY)
            (FLAG (LIST ID) 'TO_SUBST)
            (FLAG (LIST ID) 'TO_INT)
            (SETQ IDNTIES_ (CONS ID IDNTIES_)))))))))) 
(PUT 'SHOW_ID 'NUMBER-OF-ARGS 0) 
(PUT 'SHOW_ID 'DEFINED-ON-LINE '113) 
(PUT 'SHOW_ID 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'SHOW_ID 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SHOW_ID NIL
    (PROG (L N)
      (TERPRI)
      (SETQ L (LENGTH IDNTIES_))
      (PROGN
       (PRIN2 (COND ((EQUAL L 0) "No") (T L)))
       (PRIN2 (COND ((EQUAL L 1) " identity.") (T " identities")))
       NIL)
      (COND ((EQUAL L 0) (TERPRI))
            (T
             (PROGN
              (SETQ N 1)
              (PROG (L)
                (SETQ L (REVERSE IDNTIES_))
               LAB
                (COND ((NULL L) (RETURN NIL)))
                ((LAMBDA (L)
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (ASSGNPRI (AEVAL N) NIL 'FIRST)
                     (ASSGNPRI (AEVAL ")  ") NIL NIL)
                     (ASSGNPRI L NIL NIL)
                     (ASSGNPRI (AEVAL " :  0 = ") NIL NIL)
                     (ASSGNPRI (AEVAL (GET L 'VAL)) NIL 'LAST))
                    (SETQ N (ADD1 N))
                    (COND
                     (PRINT_ALL
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "   to_int     : ")
                        (PRIN2 (FLAGP L 'TO_INT))
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2 "   to_subst   : ")
                        (PRIN2 (FLAGP L 'TO_SUBST))
                        NIL)
                       NIL)))))
                 (CAR L))
                (SETQ L (CDR L))
                (GO LAB))))))) 
(PUT 'DEL_RED_ID 'NUMBER-OF-ARGS 1) 
(PUT 'DEL_RED_ID 'DEFINED-ON-LINE '134) 
(PUT 'DEL_RED_ID 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'DEL_RED_ID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEL_RED_ID (PDES)
    (PROG (OLDLI PL S IDTY NEWS SUCC P L)
      (COND
       (IDNTIES_
        (PROGN
         (SETQ OLDLI IDNTIES_)
         (PROG ()
          WHILELABEL
           (COND ((NOT OLDLI) (RETURN NIL)))
           (COND ((NOT (FLAGP (CAR OLDLI) 'TO_SUBST)) (SETQ OLDLI (CDR OLDLI)))
                 (T
                  (PROGN
                   (SETQ IDTY (GET (CAR OLDLI) 'VAL))
                   (SETQ PL (SMEMBERL PDES IDTY))
                   (PROG (P)
                     (SETQ P PL)
                    LAB
                     (COND ((NULL P) (RETURN NIL)))
                     ((LAMBDA (P) (SETQ L (UNION (GET P 'VARS) L))) (CAR P))
                     (SETQ P (CDR P))
                     (GO LAB))
                   (COND (L (SETQ L (LENGTH L))) (T (SETQ L 0)))
                   (SETQ PL (SETDIFF PL (SEARCH_LI IDTY 'DF)))
                   (COND ((NULL PL) (REMFLAG (LIST (CAR OLDLI)) 'TO_SUBST))
                         (T
                          (PROGN
                           (DROP_IDTY (CAR OLDLI))
                           (SETQ S NIL)
                           (PROG ()
                            WHILELABEL
                             (COND ((NOT PL) (RETURN NIL)))
                             (PROGN
                              (COND
                               ((AND (NULL (GET (CAR PL) 'STARDE))
                                     (EQUAL (GET (CAR PL) 'NVARS) L)
                                     (NULL (FLAGP S 'TO_EVAL))
                                     (OR (NULL S)
                                         (GREATERP (GET (CAR PL) 'NVARS)
                                                   (GET S 'NVARS))
                                         (AND
                                          (EQUAL (GET (CAR PL) 'NVARS)
                                                 (GET S 'NVARS))
                                          (GREATERP (GET (CAR PL) 'TERMS)
                                                    (GET S 'TERMS)))))
                                (SETQ S (CAR PL))))
                              (SETQ PL (CDR PL)))
                             (GO WHILELABEL))
                           (COND
                            ((NULL S) (REMFLAG (LIST (CAR OLDLI)) 'TO_SUBST))
                            (T
                             (PROGN
                              (COND
                               (PRINT_
                                (PROGN
                                 (PROGN
                                  (PRIN2 "Equation ")
                                  (PRIN2 S)
                                  (PRIN2
                                   " is dropped as it is a consequence of others: ")
                                  NIL)
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "0 = ") NIL 'FIRST)
                                  (ASSGNPRI (AEVAL* IDTY) NIL 'LAST))
                                 NIL)))
                              (SETQ PL (COEFFN IDTY S 1))
                              (SETQ NEWS
                                      (REVAL1
                                       (LIST 'QUOTIENT
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES PL S) IDTY)
                                             PL)
                                       T))
                              (SETQ SUCC T)
                              (SETQ PDES (DROP_PDE S PDES NEWS))
                              (SETQ OLDLI (CDR OLDLI)))))))))))
           (GO WHILELABEL)))))
      (COND (SUCC (RETURN PDES))))) 
(PUT 'DEL_REDUNDANT_DE 'NUMBER-OF-ARGS 1) 
(PUT 'DEL_REDUNDANT_DE 'DEFINED-ON-LINE '198) 
(PUT 'DEL_REDUNDANT_DE 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'DEL_REDUNDANT_DE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEL_REDUNDANT_DE (ARGLIST)
    (PROG (PDES)
      (COND
       ((SETQ PDES (DEL_RED_ID (CAR ARGLIST)))
        (RETURN (LIST PDES (CADR ARGLIST))))))) 
(PUT 'WRITE_ID_TO_FILE 'NUMBER-OF-ARGS 1) 
(PUT 'WRITE_ID_TO_FILE 'DEFINED-ON-LINE '203) 
(PUT 'WRITE_ID_TO_FILE 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'WRITE_ID_TO_FILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITE_ID_TO_FILE (PDES)
    (PROG (S P H PL A SAVE OFL*BAK)
      (COND
       (IDNTIES_
        (PROGN
         (CHANGE_PROMPT_TO "")
         (PROGN
          (PRIN2 "Please give the name of the file in double quotes")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "without `;' : ") NIL)
         (SETQ S (TERMREAD))
         (SETQ A (OPEN S 'OUTPUT))
         (SETQ OFL*BAK OFL*)
         (SETQ OFL* S)
         (SETQ SAVE (WRS A))
         (OFF (LIST 'NAT))
         (PROGN (PRIN2 "load crack$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "lisp(nequ_:=") (PRIN2 NEQU_) (PRIN2 ")$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "off batch_mode$") NIL)
         (TERPRI)
         (PROGN (PRIN2 "list_of_variables:=") NIL)
         (ASSGNPRI (AEVAL (CONS 'LIST VL_)) NIL 'ONLY)
         (PROGN (PRIN2 "list_of_functions:=") NIL)
         (ASSGNPRI (AEVAL (CONS 'LIST PDES)) NIL 'ONLY)
         (PROG (H)
           (SETQ H PDES)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND
               ((SETQ PL (ASSOC H DEPL*))
                (PROG (P)
                  (SETQ P (CDR PL))
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (PROGN
                      (ASSGNPRI (AEVAL "depend ") NIL 'FIRST)
                      (ASSGNPRI H NIL NIL)
                      (ASSGNPRI (AEVAL ",") NIL NIL)
                      (ASSGNPRI P NIL 'LAST)))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB)))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (PROGN (PRIN2 "list_of_equations:=") NIL)
         (ASSGNPRI
          (AEVAL
           (CONS 'LIST
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H IDNTIES_)
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (H) (GET H 'VAL)) (CAR H))
                                         NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (H) (GET H 'VAL)) (CAR H)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          NIL 'ONLY)
         (TERPRI)
         (PROGN (PRIN2 "solution_:=crack(list_of_equations,{},") NIL)
         (TERPRI)
         (PROGN (PRIN2 "                 list_of_functions,") NIL)
         (TERPRI)
         (PROGN (PRIN2 "                 list_of_variables)$") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "end$") NIL)
         (TERPRI)
         (WRS SAVE)
         (SETQ OFL* OFL*BAK)
         (CLOSE A)
         (ON (LIST 'NAT))
         (RESTORE_INTERACTIVE_PROMPT)
         NIL))))) 
(PUT 'REMOVE_IDL 'NUMBER-OF-ARGS 0) 
(PUT 'REMOVE_IDL 'DEFINED-ON-LINE '251) 
(PUT 'REMOVE_IDL 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'REMOVE_IDL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REMOVE_IDL NIL
    (PROGN
     (PROG (H)
       (SETQ H IDNTIES_)
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H) (SETPROP H NIL)) (CAR H))
       (SETQ H (CDR H))
       (GO LAB))
     (SETQ IDNTIES_ NIL))) 
(PUT 'START_HISTORY 'NUMBER-OF-ARGS 1) 
(PUT 'START_HISTORY 'DEFINED-ON-LINE '255) 
(PUT 'START_HISTORY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'START_HISTORY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE START_HISTORY (PDES)
    (PROG (L)
      (CHANGE_PROMPT_TO "")
      (PROGN
       (PRIN2 "For recording the history of equations all currently")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "recorded histories would be deleted as well as all") NIL)
      (TERPRI)
      (PROGN (PRIN2 "present decoupling information, i.e. `dec_with'") NIL)
      (TERPRI)
      (PROGN (PRIN2 "would be set to nil. Please confirm (y/n). ") NIL)
      (SETQ L (TERMREAD))
      (COND
       ((OR (EQUAL L 'Y) (EQUAL L 'Y))
        (PROGN
         (SETQ RECORD_HIST T)
         (PROG (L)
           (SETQ L PDES)
          LAB
           (COND ((NULL L) (RETURN NIL)))
           ((LAMBDA (L) (PUT L 'HISTRY_ L)) (CAR L))
           (SETQ L (CDR L))
           (GO LAB))
         (PROG (L)
           (SETQ L PDES)
          LAB
           (COND ((NULL L) (RETURN NIL)))
           ((LAMBDA (L) (PUT L 'DEC_WITH NIL)) (CAR L))
           (SETQ L (CDR L))
           (GO LAB))
         NIL)))
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'STOP_HISTORY 'NUMBER-OF-ARGS 1) 
(PUT 'STOP_HISTORY 'DEFINED-ON-LINE '271) 
(PUT 'STOP_HISTORY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'STOP_HISTORY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STOP_HISTORY (PDES)
    (PROGN
     (SETQ RECORD_HIST NIL)
     (PROG (L)
       (SETQ L PDES)
      LAB
       (COND ((NULL L) (RETURN NIL)))
       ((LAMBDA (L) (PUT L 'HISTRY_ L)) (CAR L))
       (SETQ L (CDR L))
       (GO LAB)))) 
(PUT 'IDTY_INTEGRATION 'NUMBER-OF-ARGS 1) 
(PUT 'IDTY_INTEGRATION 'DEFINED-ON-LINE '280) 
(PUT 'IDTY_INTEGRATION 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'IDTY_INTEGRATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDTY_INTEGRATION (ARGLIST)
    (PROG (L PDES IDCP)
      (SETQ PDES (CAR ARGLIST))
      (SETQ IDCP IDNTIES_)
      (PROG ()
       WHILELABEL
        (COND ((NOT IDCP) (RETURN NIL)))
        (COND ((NOT (FLAGP (CAR IDCP) 'TO_INT)) (SETQ IDCP (CDR IDCP)))
              ((SETQ L (INTEGRATE_IDTY (CAR IDCP) PDES FTEM_ VL_))
               (PROGN (SETQ PDES L) (SETQ IDCP NIL)))
              (T
               (PROGN
                (REMFLAG (LIST (CAR IDCP)) 'TO_INT)
                (SETQ IDCP (CDR IDCP))
                NIL)))
        (GO WHILELABEL))
      (COND (L (RETURN (LIST PDES (CADR ARGLIST))))))) 
(PUT 'INTEGRATE_IDTY 'NUMBER-OF-ARGS 4) 
(PUT 'INTEGRATE_IDTY 'DEFINED-ON-LINE '295) 
(PUT 'INTEGRATE_IDTY 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'INTEGRATE_IDTY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATE_IDTY (ORG_IDTY ALLPDES FL VL)
    (COND
     (IDNTIES_
      (PROG (CL NCL VLCP XLIST EQL A F NEWPDES FTEM_BAK NX DL L K IDTY PDES
             EXTRAPDES NEWIDTYLIST)
        (COND
         ((NULL ORG_IDTY)
          (COND ((NULL (CDR IDNTIES_)) (SETQ ORG_IDTY (CAR IDNTIES_)))
                (T
                 (PROGN
                  (SHOW_ID)
                  (CHANGE_PROMPT_TO "")
                  (PROGN
                   (PRIN2 "Which of the identities shall be integrated? (no) ")
                   NIL)
                  (SETQ K (LENGTH IDNTIES_))
                  (PROG ()
                   REPEATLABEL
                    (SETQ L (TERMREAD))
                    (COND
                     ((NOT (AND (FIXP L) (LESSP 0 L) (LEQ L K)))
                      (GO REPEATLABEL))))
                  (SETQ ORG_IDTY (NTH IDNTIES_ (PLUS K (DIFFERENCE 1 L))))
                  (RESTORE_INTERACTIVE_PROMPT))))))
        (SETQ IDTY (REVAL1 (NUM (REVAL1 (GET ORG_IDTY 'VAL) T)) T))
        (COND ((TRIVIAL_IDTY ALLPDES IDTY) (RETURN NIL)))
        (SETQ PDES (SMEMBERL ALLPDES IDTY))
        (SETQ A (ALL_DERIV_SEARCH IDTY PDES))
        (SETQ XLIST (SMEMBERL VL A))
        (SETQ CL (INTCURRENT3 IDTY (CONS 'LIST PDES) (CONS 'LIST XLIST)))
        (COND
         ((AND (NOT (ZEROP (CADDR CL))) INTER_DIVINT)
          (SETQ CL (INTCURRENT2 IDTY (CONS 'LIST PDES) (CONS 'LIST XLIST)))))
        (COND
         ((ZEROP (CADDR CL))
          (PROGN
           (SETQ CL (CDADR CL))
           (SETQ VLCP XLIST)
           (SETQ XLIST NIL)
           (PROG ()
            WHILELABEL
             (COND ((NOT VLCP) (RETURN NIL)))
             (PROGN
              (COND
               ((NOT (ZEROP (CAR CL)))
                (PROGN
                 (SETQ NCL (CONS (CAR CL) NCL))
                 (SETQ XLIST (CONS (CAR VLCP) XLIST)))))
              (SETQ CL (CDR CL))
              (SETQ VLCP (CDR VLCP)))
             (GO WHILELABEL))
           (SETQ CL NCL)
           (SETQ NX (LENGTH XLIST))
           (PROG ()
            WHILELABEL
             (COND ((NOT PDES) (RETURN NIL)))
             (PROGN
              (CP_SQ2P_VAL (CAR PDES))
              (SETQ NCL (SUBST (GET (CAR PDES) 'PVAL) (CAR PDES) NCL))
              (SETQ PDES (CDR PDES)))
             (GO WHILELABEL))
           (SETQ FTEM_BAK FTEM_)
           (SETQ EQL
                   (INT_CURL (REVAL1 (CONS 'LIST NCL) T) (CONS 'LIST FL)
                    (CONS 'LIST XLIST) (CONS 'LIST (VARSLIST NCL FTEM_ VL))))
           (COND
            ((OR (NULL EQL) (NULL (CDADR EQL)) (ZEROP (CADADR EQL)))
             (RETURN NIL)))
           (SETQ EQL (CDR EQL))
           (COND
            (PRINT_
             (PROGN
              (SETQ NCL
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I 1)
                        (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         (LIST 'DF (NTH CL I) (NTH XLIST I))
                                         NIL)))
                       LOOPLABEL
                        (SETQ I (PLUS2 I 1))
                        (COND
                         ((MINUSP (DIFFERENCE NX I)) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS (LIST 'DF (NTH CL I) (NTH XLIST I)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ NCL (COND ((CDR NCL) (CONS 'PLUS NCL)) (T (CAR NCL))))
              (TERPRI)
              (PROGN (PRIN2 "The identity ") NIL)
              (MATHPRINT (REVAL1 NCL T))
              (PROGN (PRIN2 "can be integrated to ") NIL)
              (TERPRI)
              (DEPRINT (CDAR EQL))
              NIL)))
           (COND ((LESSP NX 3) (SETQ A 'Y))
                 ((OR (NULL INTER_DIVINT) *BATCH_MODE)
                  (PROGN
                   (SETQ A 'N)
                   (COND
                    (PRINT_
                     (PROGN
                      (PROGN
                       (PRIN2
                        "The integrated divergence is not used because it ")
                       (PRIN2 "has more than 2 terms and")
                       NIL)
                      (TERPRI)
                      (COND
                       (*BATCH_MODE
                        (PROGN (PRIN2 "`inter_divint' is nil.") NIL))
                       (T (PROGN (PRIN2 "batch_mode is on.") NIL)))
                      NIL)))
                   (TERPRI)))
                 (T
                  (PROGN
                   (CHANGE_PROMPT_TO "")
                   (PROGN (PRIN2 "Shall this integration be used? (y/n) ") NIL)
                   (PROG ()
                    REPEATLABEL
                     (SETQ A (TERMREAD))
                     (COND
                      ((NOT (OR (EQUAL A 'Y) (EQUAL A 'N))) (GO REPEATLABEL))))
                   (RESTORE_INTERACTIVE_PROMPT))))
           (COND
            ((EQUAL A 'N)
             (PROGN
              (SETQ A (SETDIFF_ACCORDING_TO FTEM_ FTEM_BAK FTEM_))
              (PROG (F)
                (SETQ F A)
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F) (DROP_FCT F)) (CAR F))
                (SETQ F (CDR F))
                (GO LAB))
              (SETQ FTEM_ FTEM_BAK)))
            (T
             (PROGN
              (SETQ EXTRAPDES (CDADR EQL))
              (SETQ EQL (CDAR EQL))
              (DROP_IDTY ORG_IDTY)
              (PROG ()
               WHILELABEL
                (COND ((NOT EQL) (RETURN NIL)))
                (PROGN
                 (COND
                  ((NOT (ZEROP (CAR EQL)))
                   (PROGN
                    (SETQ A
                            (MKEQSQ NIL NIL (CAR EQL) FTEM_ VL ALLFLAGS_ NIL
                             (LIST 0) NIL ALLPDES))
                    (SETQ NEWPDES (CONS A NEWPDES))
                    NIL)))
                 (SETQ EQL (CDR EQL))
                 NIL)
                (GO WHILELABEL))
              (SETQ NEWPDES (REVERSE NEWPDES))
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE NX I)) (RETURN NIL)))
                (PROGN
                 (SETQ IDTY (NTH CL I))
                 (COND ((EQUAL NX 1) (SETQ A (CAR NEWPDES)))
                       (T
                        (PROGN
                         (SETQ L (DIFFERENCE I 1))
                         (SETQ DL (DIFFERENCE NX 2))
                         (SETQ A
                                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ J 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (PROGN
                                                     (SETQ K L)
                                                     (SETQ L (PLUS L DL))
                                                     (SETQ DL (SUB1 DL))
                                                     (LIST 'DF (NTH NEWPDES K)
                                                           (NTH XLIST J)))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ J (PLUS2 J 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ K L)
                                             (SETQ L (PLUS L DL))
                                             (SETQ DL (SUB1 DL))
                                             (LIST 'DF (NTH NEWPDES K)
                                                   (NTH XLIST J)))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                         (SETQ A
                                 (COND ((NULL A) 0) ((CDR A) (CONS 'PLUS A))
                                       (T (CAR A))))
                         (SETQ IDTY (LIST 'PLUS IDTY A))
                         (COND ((EQUAL I 1) (SETQ L 1))
                               (T (SETQ L (PLUS K (DIFFERENCE NX I) 1))))
                         (SETQ A
                                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ J (PLUS I 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE NX J)) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (PROGN
                                                     (SETQ K L)
                                                     (SETQ L (PLUS L 1))
                                                     (LIST 'DF (NTH NEWPDES K)
                                                           (NTH XLIST J)))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ J (PLUS2 J 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE NX J))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ K L)
                                             (SETQ L (PLUS L 1))
                                             (LIST 'DF (NTH NEWPDES K)
                                                   (NTH XLIST J)))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                         (SETQ A
                                 (COND ((NULL A) 0) ((CDR A) (CONS 'PLUS A))
                                       (T (CAR A))))
                         NIL)))
                 (SETQ NEWIDTYLIST
                         (CONS (LIST 'DIFFERENCE IDTY A) NEWIDTYLIST))
                 NIL)
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              (SETQ EQL NIL)
              (PROG (A)
                (SETQ A EXTRAPDES)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A)
                   (PROGN
                    (SETQ A
                            (MKEQSQ NIL NIL A FTEM_ VL ALLFLAGS_ T (LIST 0) NIL
                             ALLPDES))
                    (SETQ ALLPDES (EQINSERT A ALLPDES))
                    (SETQ TO_DO_LIST
                            (CONS (LIST 'SUBST_LEVEL_35 (LIST A)) TO_DO_LIST))
                    (SETQ EQL (CONS A EQL))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB))
              (COND
               (PRINT_
                (PROGN
                 (PROGN (PRIN2 "Integration gives: ") NIL)
                 (LISTPRINT NEWPDES)
                 (TERPRI)
                 (COND
                  (EQL
                   (PROGN
                    (PROGN (PRIN2 "with extra conditions: ") NIL)
                    (LISTPRINT EQL))))
                 NIL)))
              (PROG (A)
                (SETQ A NEWPDES)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A) (SETQ ALLPDES (EQINSERT A ALLPDES))) (CAR A))
                (SETQ A (CDR A))
                (GO LAB))
              (PROG (A)
                (SETQ A NEWIDTYLIST)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A) (NEW_IDTY A ALLPDES T)) (CAR A))
                (SETQ A (CDR A))
                (GO LAB))
              (RETURN ALLPDES))))))))))) 
(PUT 'SORTPERMULI 'NUMBER-OF-ARGS 1) 
(PUT 'SORTPERMULI 'DEFINED-ON-LINE '482) 
(PUT 'SORTPERMULI 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'SORTPERMULI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORTPERMULI (A)
    (PROG (FLP CONTI NEWA)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ NEWA NIL)
         (SETQ CONTI NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR A)) (RETURN NIL)))
           (COND
            ((LESSP (CAR A) (CADR A))
             (PROGN (SETQ NEWA (CONS (CAR A) NEWA)) (SETQ A (CDR A))))
            (T
             (PROGN
              (SETQ CONTI T)
              (SETQ FLP (NOT FLP))
              (SETQ NEWA (CONS (CADR A) NEWA))
              (SETQ A (CONS (CAR A) (CDDR A)))
              NIL)))
           (GO WHILELABEL))
         (SETQ NEWA (CONS (CAR A) NEWA))
         (SETQ A (REVERSE NEWA)))
        (COND ((NOT (NULL CONTI)) (GO REPEATLABEL))))
      (RETURN (CONS FLP A)))) 
(PUT 'CURLCONST 'NUMBER-OF-ARGS 2) 
(PUT 'CURLCONST 'DEFINED-ON-LINE '503) 
(PUT 'CURLCONST 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'CURLCONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CURLCONST (XLIST VL)
    (PROG (N QLI I J K QIJ A FLP F RESU QLICP)
      (SETQ N (LENGTH XLIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J (PLUS I 1))
         LAB
          (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
          (PROGN
           (SETQ QIJ NIL)
           (PROG (K)
             (SETQ K 1)
            LAB
             (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
             (COND
              ((AND (NEQ K I) (NEQ K J))
               (PROGN
                (SETQ A (SORTPERMULI (LIST I J K)))
                (SETQ FLP (CAR A))
                (SETQ A (CDR A))
                (SETQ QLICP QLI)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND QLICP (NEQ (CAAR QLICP) A))) (RETURN NIL)))
                  (SETQ QLICP (CDR QLICP))
                  (GO WHILELABEL))
                (COND (QLICP (SETQ F (CDAR QLICP)))
                      (T
                       (PROGN
                        (SETQ F (NEWFCT FNAME_ VL NFCT_))
                        (SETQ NFCT_ (ADD1 NFCT_))
                        (SETQ FTEM_ (FCTINSERT F FTEM_))
                        (SETQ FLIN_ (SORT_ACCORDING_TO (CONS F FLIN_) FTEM_))
                        (SETQ QLI (CONS (CONS A F) QLI)))))
                (SETQ F (LIST 'DF F (NTH XLIST K)))
                (COND (FLP (SETQ F (LIST 'MINUS F))))
                (SETQ QIJ (CONS F QIJ)))))
             (SETQ K (PLUS2 K 1))
             (GO LAB))
           (COND
            ((NULL QIJ)
             (PROGN
              (SETQ QIJ (NEWFCT FNAME_ (SETDIFF VL XLIST) NFCT_))
              (SETQ NFCT_ (ADD1 NFCT_))
              (SETQ FTEM_ (FCTINSERT QIJ FTEM_))
              (SETQ FLIN_ (SORT_ACCORDING_TO (CONS QIJ FLIN_) FTEM_))))
            ((CDR QIJ) (SETQ QIJ (REVAL1 (CONS 'PLUS QIJ) T)))
            (T (SETQ QIJ (CAR QIJ))))
           (SETQ RESU (CONS QIJ RESU)))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN RESU))) 
(PUT 'UPDT_CURL 'NUMBER-OF-ARGS 8) 
(PUT 'UPDT_CURL 'DEFINED-ON-LINE '545) 
(PUT 'UPDT_CURL 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'UPDT_CURL 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE UPDT_CURL (H2 RMDR FL DONE_XLIST X CDRXLIST N K)
    (PROG (I H4 H5 H6 H7 Y PINT SUCC)
      (COND
       ((NOT (ZEROP (REVAL1 (REVAL1 (LIST 'DF RMDR X) T) T)))
        (PROGN
         (COND
          (PRINT_ (PROGN (TERPRI) (PROGN (PRIN2 "No success.") NIL) (TERPRI))))
         (SETQ SUCC NIL)))
       (T
        (PROGN
         (SETQ SUCC T)
         (COND
          (DONE_XLIST
           (PROGN
            (SETQ H7 (INTCURRENT2 RMDR FL (CONS 'LIST DONE_XLIST)))
            (SETQ RMDR (CADDR H7))
            (SETQ H7 (CDADR H7))
            (SETQ H4 NIL)
            (SETQ H5 (MINUS 1))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE K 1) I)) (RETURN NIL)))
              (PROGN
               (SETQ H5 (ADD1 H5))
               (PROG (H6)
                 (SETQ H6 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE N K) H6)) (RETURN NIL)))
                 (PROGN (SETQ H4 (CONS (CAR H2) H4)) (SETQ H2 (CDR H2)))
                 (SETQ H6 (PLUS2 H6 1))
                 (GO LAB))
               (SETQ H4 (CONS (LIST 'DIFFERENCE (CAR H2) (CAR H7)) H4))
               (SETQ H2 (CDR H2))
               (SETQ H7 (CDR H7))
               (PROG (H6)
                 (SETQ H6 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE H5 H6)) (RETURN NIL)))
                 (PROGN (SETQ H4 (CONS (CAR H2) H4)) (SETQ H2 (CDR H2)))
                 (SETQ H6 (PLUS2 H6 1))
                 (GO LAB)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ H2 (REVERSE H4))
            NIL)))
         (COND ((ZEROP RMDR) (SETQ PINT (CONS 0 NIL)))
               (T
                (PROGN
                 (SETQ Y (COND (CDRXLIST (CAR CDRXLIST)) (T (CAR DONE_XLIST))))
                 (SETQ FNEW_ NIL)
                 (SETQ PINT (PARTINT RMDR FL VL_ Y GENINT_))
                 (COND ((NULL PINT) (SETQ SUCC NIL))
                       (T
                        (PROGN
                         (PROG (H4)
                           (SETQ H4 FNEW_)
                          LAB
                           (COND ((NULL H4) (RETURN NIL)))
                           ((LAMBDA (H4)
                              (PROGN
                               (SETQ FTEM_ (FCTINSERT H4 FTEM_))
                               (SETQ FLIN_ (CONS H4 FLIN_))))
                            (CAR H4))
                           (SETQ H4 (CDR H4))
                           (GO LAB))
                         (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_)))))))))))
      (RETURN (COND ((NULL SUCC) NIL) (T (CONS H2 PINT)))))) 
(PUT 'INT_CURL 'NUMBER-OF-ARGS 4) 
(PUT 'INT_CURL 'DEFINED-ON-LINE '595) 
(PUT 'INT_CURL 'DEFINED-IN-FILE 'CRACK/CRIDENT.RED) 
(PUT 'INT_CURL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INT_CURL (PLI FL XLIST VL)
    (PROG (H1 H2 H3 RESU NEWPLI XCP DONE_XLIST N K OK NEWEQ FTEM_BAK)
      (SETQ PLI (CDR PLI))
      (SETQ XLIST (CDR XLIST))
      (SETQ VL (CDR VL))
      (SETQ XCP XLIST)
      (SETQ N (LENGTH XLIST))
      (SETQ K 0)
      (SETQ OK T)
      (SETQ FTEM_BAK FTEM_)
      (COND
       ((EQUAL N 1)
        (RETURN (LIST 'LIST (REVAL1 (CONS 'LIST PLI) T) (LIST 'LIST)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (CDR PLI) OK)) (RETURN NIL)))
        (PROGN
         (SETQ K (ADD1 K))
         (SETQ H3
                 (INTCURRENT2 (REVAL1 (CAR PLI) T) FL
                  (CONS 'LIST (CDR XLIST))))
         (SETQ PLI (CDR PLI))
         (SETQ H1 (CDADR H3))
         (SETQ H3 (REVAL1 (REVAL1 (CADDR H3) T) T))
         (COND
          ((NOT (ZEROP H3))
           (PROGN
            (SETQ H3
                    (UPDT_CURL H2 H3 FL DONE_XLIST (CAR XLIST) (CDR XLIST) N
                     K))
            (COND ((NULL H3) (SETQ OK NIL))
                  (T
                   (PROGN
                    (SETQ NEWEQ (APPEND (CDDR H3) NEWEQ))
                    (SETQ H2 (CAR H3))
                    (SETQ H1 (CONS (LIST 'PLUS (CAR H1) (CADR H3)) (CDR H1)))
                    NIL))))))
         (COND
          (OK
           (PROGN
            (SETQ H2 (APPEND (REVERSE H1) H2))
            (SETQ NEWPLI NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT H1) (RETURN NIL)))
              (PROGN
               (SETQ NEWPLI
                       (CONS
                        (LIST 'PLUS (LIST 'DF (CAR H1) (CAR XLIST)) (CAR PLI))
                        NEWPLI))
               (SETQ H1 (CDR H1))
               (SETQ PLI (CDR PLI)))
              (GO WHILELABEL))
            (SETQ PLI (REVERSE NEWPLI)))))
         (SETQ DONE_XLIST (CONS (CAR XLIST) DONE_XLIST))
         (SETQ XLIST (CDR XLIST)))
        (GO WHILELABEL))
      (COND
       (OK
        (PROGN
         (SETQ PLI (REVAL1 (CAR PLI) T))
         (COND
          ((NEQ PLI 0)
           (PROGN
            (SETQ K (PLUS K 1))
            (SETQ H3 (UPDT_CURL H2 PLI FL DONE_XLIST (CAR XLIST) NIL N K))
            (COND ((NULL H3) (SETQ OK NIL))
                  (T
                   (PROGN
                    (SETQ NEWEQ (APPEND (CDDR H3) NEWEQ))
                    (SETQ H2 (CAR H3))
                    (SETQ H2
                            (CONS (LIST 'DIFFERENCE (CAR H2) (CADR H3))
                                  (CDR H2))))))
            NIL))))))
      (COND
       ((NULL OK)
        (PROGN
         (SETQ H1 (SETDIFF_ACCORDING_TO FTEM_ FTEM_BAK FTEM_))
         (PROG (H2)
           (SETQ H2 H1)
          LAB
           (COND ((NULL H2) (RETURN NIL)))
           ((LAMBDA (H2) (DROP_FCT H2)) (CAR H2))
           (SETQ H2 (CDR H2))
           (GO LAB))
         (SETQ FTEM_ FTEM_BAK)))
       (T
        (PROGN
         (SETQ H1 (CURLCONST XCP VL))
         (PROG ()
          WHILELABEL
           (COND ((NOT H1) (RETURN NIL)))
           (PROGN
            (SETQ RESU (CONS (LIST 'PLUS (CAR H2) (CAR H1)) RESU))
            (SETQ H1 (CDR H1))
            (SETQ H2 (CDR H2))
            NIL)
           (GO WHILELABEL))
         (RETURN
          (LIST 'LIST (REVAL1 (CONS 'LIST RESU) T) (CONS 'LIST NEWEQ)))))))) 
(ENDMODULE) 