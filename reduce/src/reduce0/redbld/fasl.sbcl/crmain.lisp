(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CRACKSTAR)) 
(FLAG '(CRACKSHELL) 'OPFN) 
(PUT 'CRACKSHELL 'NUMBER-OF-ARGS 0) 
(PUT 'CRACKSHELL 'DEFINED-ON-LINE '34) 
(PUT 'CRACKSHELL 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CRACKSHELL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CRACKSHELL NIL
    (PROG (S)
      (TERPRI)
      (COND
       ((NULL OLD_HISTORY)
        (PROGN
         (SETQ STEPCOUNTER_ (MINUS 1))
         (SETQ BATCHCOUNT_ (MINUS 1))
         (CHECK_STOP)
         (COND
          ((EQUAL BATCHCOUNT_ (MINUS 2))
           (RETURN
            (PROGN
             (PROGN
              (PRIN2 "Before running crackshell() you need to delete the file")
              NIL)
             (TERPRI)
             (PROGN (PRIN2 "'stop_now' from the current directory.") NIL)
             (TERPRI)
             NIL))))
         (CHANGE_PROMPT_TO "")
         (PROGN
          (PRIN2 "Please give the name of the file in double quotes")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "(no ;) from which the session is to be restored: ")
          NIL)
         (SETQ S (TERMREAD))
         (SETQ OLD_HISTORY (LIST 'RB S))
         (RESTORE_INTERACTIVE_PROMPT))))
      (SETQ *BATCH_MODE NIL)
      (RETURN
       (AEVAL
        (LIST 'CRACK (LIST 'LIST) (LIST 'LIST) (LIST 'LIST) (LIST 'LIST)))))) 
(PUT 'CRTEST 'NUMBER-OF-ARGS 0) 
(FLAG '(CRTEST) 'OPFN) 
(PUT 'CRTEST 'DEFINED-ON-LINE '59) 
(PUT 'CRTEST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CRTEST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CRTEST NIL
    (PROG (H1 H2 H3)
      (SETQ H1 (AEVAL (INTERN (GENSYM))))
      (SETQ H2 (AEVAL (INTERN (GENSYM))))
      (SETQ H3 (AEVAL (INTERN (GENSYM))))
      (AEVAL (OFF (LIST 'BATCH_MODE)))
      (AEVAL
       (LIST 'CRACK
             (LIST 'LIST (LIST 'PLUS H1 1) (LIST 'PLUS H2 3 (LIST 'EXPT H1 2))
                   (LIST 'PLUS (LIST 'EXPT H3 3) H2 7)
                   (LIST 'PLUS (LIST 'TIMES (LIST 'EXPT H1 2) H2) H3 8))
             (LIST 'LIST H1) (LIST 'LIST H1 H2 H3) (LIST 'LIST))))) 
(FLAG '(NODEPND) 'OPFN) 
(PUT 'NODEPND 'NUMBER-OF-ARGS 1) 
(PUT 'NODEPND 'DEFINED-ON-LINE '71) 
(PUT 'NODEPND 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'NODEPND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NODEPND (FL)
    (PROG (F)
      (SETQ F (CDR FL))
     LAB
      (COND ((NULL F) (RETURN NIL)))
      ((LAMBDA (F) (SETQ DEPL* (DELETE (ASSOC (REVAL1 F T) DEPL*) DEPL*)))
       (CAR F))
      (SETQ F (CDR F))
      (GO LAB))) 
(PUT 'CRACK 'PSOPFN 'SQ*CRACK) 
(PUT 'SQ*CRACK 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*CRACK 'DEFINED-ON-LINE '77) 
(PUT 'SQ*CRACK 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SQ*CRACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*CRACK (INP)
    (PROG (EL IL FL VL L L1 L2 A B N M K P PDES)
      (COND
       ((SETQ L (CHECK_GLOBALS))
        (PROGN
         (PROGN
          (PRIN2 "The global variable ")
          (PRIN2 L)
          (PRIN2 " has an incorrect value, please check!")
          NIL)
         (REDERR " "))))
      (COND
       ((AND PRINT_ LOGOPRINT_)
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          (PRIN2 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "This is CRACK - a solver for overdetermined partial ")
          (PRIN2 "differential equations")
          NIL)
         (TERPRI)
         NIL)))
      (COND
       ((AND (NOT *BATCH_MODE) (NULL OLD_HISTORY))
        (PROGN
         (COND ((NOT PRINT_) (PROGN (TERPRI) (SETQ PRINT_ 8))))
         (PROGN (PRIN2 "Enter `h' for help.") NIL)
         (TERPRI)
         NIL)))
      (BACKUP_REDUCE_FLAGS)
      (SETQ CONTRADICTION_ NIL)
      (SETQ TO_DO_LIST NIL)
      (SETQ FNEW_ NIL)
      (SETQ VL_ NIL)
      (SETQ LEVEL_ NIL)
      (SETQ STEPCOUNTER_ 0)
      (SETQ BATCHCOUNT_ (MINUS 1))
      (SETQ RECYCLE_EQNS (CONS NIL NIL))
      (SETQ RECYCLE_FCTS NIL)
      (SETQ RECYCLE_IDS NIL)
      (SETQ LARGEST_FULLY_SHORTENED NIL)
      (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN NIL)
      (SETQ N (TIME))
      (SETQ M (GCTIME))
      (SETQ EL (REVAL1 (CAR INP) NIL))
      (SETQ IL (REVAL1 (CADR INP) NIL))
      (SETQ FL (REVAL1 (CADDR INP) T))
      (SETQ VL (REVAL1 (CADDDR INP) T))
      (SETQ EL
              (COND
               ((AND (PAIRP EL) (EQUAL (CAR EL) 'LIST))
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P (CDR EL))
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P)
                                      (COND
                                       ((AND (PAIRP P) (EQUAL (CAR P) '*SQ))
                                        (CADR P))
                                       (T (SIMP* P))))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P)
                              (COND
                               ((AND (PAIRP P) (EQUAL (CAR P) '*SQ)) (CADR P))
                               (T (SIMP* P))))
                            (CAR P))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               ((AND (PAIRP EL) (EQUAL (CAR EL) '*SQ)) (LIST (CADR EL)))
               (T (LIST (SIMP* EL)))))
      (SETQ FL
              (COND ((AND (PAIRP FL) (EQUAL (CAR FL) 'LIST)) (CDR FL))
                    (T (LIST FL))))
      (SETQ VL
              (COND ((AND (PAIRP VL) (EQUAL (CAR VL) 'LIST)) (CDR VL))
                    (T (LIST VL))))
      (SETQ IL
              (COND ((AND (PAIRP IL) (EQUAL (CAR IL) 'LIST)) (CDR IL))
                    (T (LIST IL))))
      (SETQ VL_ (UNION (ARGSET FL) VL))
      (SETQ VL NIL)
      (SETQ L FL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L (ATOM (CAR L)))) (RETURN NIL)))
        (SETQ L (CDR L))
        (GO WHILELABEL))
      (COND
       (L
        (PROGN
         (PROGN (PRIN2 "The function ") NIL)
         (TERPRI)
         (PROGN (PRIN2 (CAR L)) NIL)
         (TERPRI)
         (PROGN (PRIN2 "is not a single symbol!") NIL)
         (TERPRI)
         (REDERR " "))))
      (COND (VL_ (SETQ FL (FCTSORT FL))))
      (SETQ FSUB_ NIL)
      (COND ((NULL FLIN_) (SETQ FTEM_ FL))
            (T
             (PROGN
              (SETQ FTEM_ NIL)
              (PROG ()
               WHILELABEL
                (COND ((NOT FL) (RETURN NIL)))
                (PROGN
                 (SETQ M (FCTLENGTH (CAR FL)))
                 (SETQ L NIL)
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT (AND FL (EQUAL M (FCTLENGTH (CAR FL)))))
                     (RETURN NIL)))
                   (PROGN
                    (COND ((FREEOF FLIN_ (CAR FL)) (SETQ L (CONS (CAR FL) L)))
                          (T (SETQ FTEM_ (CONS (CAR FL) FTEM_))))
                    (SETQ FL (CDR FL)))
                   (GO WHILELABEL))
                 (SETQ FTEM_ (APPEND L FTEM_)))
                (GO WHILELABEL))
              (SETQ FTEM_ (REVERSIP FTEM_))
              (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
              NIL)))
      (COND
       ((AND FORM_COMP FORM_PIPE)
        (SYSTEM "./form_start < formin > formout &")))
      (SETQ INEQ_ NIL)
      (SETQ INEQ_OR NIL)
      (SETQ EL
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P EL)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (COND
                                      ((AND (NULL *COMPLEX)
                                            (OR (NOT (FREEOF (CAR P) 'I))
                                                (NOT
                                                 (FREEOF (CAR P) '|:GI:|))))
                                       (AEVAL (ON (LIST 'COMPLEX)))))
                                     (COND
                                      ((NOT (FREEOFLIST (CDR P) FTEM_))
                                       (ADDSQINEQ NIL (CONS (CDR P) 1) T)))
                                     (CONS (CAR P) 1)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (COND
                              ((AND (NULL *COMPLEX)
                                    (OR (NOT (FREEOF (CAR P) 'I))
                                        (NOT (FREEOF (CAR P) '|:GI:|))))
                               (AEVAL (ON (LIST 'COMPLEX)))))
                             (COND
                              ((NOT (FREEOFLIST (CDR P) FTEM_))
                               (ADDSQINEQ NIL (CONS (CDR P) 1) T)))
                             (CONS (CAR P) 1)))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (P)
        (SETQ P IL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((AND (PAIRP P) (EQUAL (CAR P) 'LIST))
             (PROGN
              (SETQ P (CDR P))
              (SETQ L (COND ((NULL P) 1) (T NIL)))
              (PROG ()
               WHILELABEL
                (COND ((NOT P) (RETURN NIL)))
                (COND ((SQZEROP (CAR P)) (SETQ P (CDR P)))
                      ((FREEOFLIST (CAR P) FTEM_)
                       (PROGN (SETQ P NIL) (SETQ L 1)))
                      (T
                       (PROGN
                        (SETQ L (CONS (LIST (CADR (CAR P))) L))
                        (SETQ P (CDR P)))))
                (GO WHILELABEL))
              (COND ((EQUAL L 1) NIL) ((NULL L) (SETQ CONTRADICTION_ T))
                    ((CDR L) (SETQ INEQ_OR (CONS L INEQ_OR)))
                    (T (ADDSQINEQ NIL (CAAR L) T)))))
            (T
             (ADDSQINEQ NIL
              (COND ((AND (PAIRP P) (EQUAL (CAR P) '*SQ)) (CADR P))
                    (T (SIMP P)))
              T))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SIMPSQINEQ_OR_ADHOC NIL)
      (SETQ IL NIL)
      (SETQ HISTORY_ NIL)
      (SETQ SOL_LIST NIL)
      (COND (STRUC_EQN (INI_STRUC)))
      (SETQ PDES (MKEQSQLIST EL NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL))
      (COND (CONTRADICTION_ (RETURN (LIST 'LIST))))
      (COND
       ((EQUAL EQUATIONS_FILE "")
        (PROGN
         (SETQ L PDES)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND L (GET (CAR L) 'LINEAR_))) (RETURN NIL)))
           (SETQ L (CDR L))
           (GO WHILELABEL))
         (COND (L (SETQ LIN_PROBLEM NIL)) (T (SETQ LIN_PROBLEM T)))
         (COND ((AND LIN_PROBLEM (NULL FLIN_)) (SETQ FLIN_ FTEM_)))
         (SETQ L PDES)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND L (NULL CONTRADICTION_))) (RETURN NIL)))
           (PROGN
            (SETQ A (GET (CAR L) 'KERN))
            (SETQ L (CDR L))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND A (NULL CONTRADICTION_))) (RETURN NIL)))
              (PROGN
               (SETQ B (CAR A))
               (SETQ A (CDR A))
               (COND ((AND (PAIRP B) (EQUAL (CAR B) 'DF)) (SETQ B (CADR B))))
               (COND
                ((AND (PAIRP B) (MEMBER (CAR B) FTEM_))
                 (SETQ CONTRADICTION_ T))))
              (GO WHILELABEL))
            (COND
             (CONTRADICTION_
              (PROGN
               (PROGN
                (PRIN2 "##### Error: The input contains ")
                (PRIN2 (CAR B))
                (PRIN2 (CDR B))
                (PRIN2 " instead of ")
                (PRIN2 (CAR B))
                NIL)
               (TERPRI)))))
           (GO WHILELABEL)))))
      (COND (CONTRADICTION_ (RETURN (LIST 'LIST))))
      (SETQ L PDES)
      (SETQ ALG_POLY (COND (VL_ NIL) (T T)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L ALG_POLY)) (RETURN NIL)))
        (PROGN
         (COND ((GET (CAR L) 'NONRATIONAL) (SETQ ALG_POLY NIL))
               (T
                (PROGN
                 (SETQ EL (GET (CAR L) 'DERIVS))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT (AND EL (EQUAL (LENGTH (CAAR EL)) 1))) (RETURN NIL)))
                   (SETQ EL (CDR EL))
                   (GO WHILELABEL))
                 (COND (EL (SETQ ALG_POLY NIL))))))
         (SETQ L (CDR L))
         NIL)
        (GO WHILELABEL))
      (SETQ EL NIL)
      (COND
       (SIZE_WATCH
        (SETQ SIZE_HIST
                (LIST
                 (CONS 'CP
                       (PROG (L FORALL-RESULT FORALL-ENDPTR)
                         (SETQ L PROC_LIST_)
                         (COND ((NULL L) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                          NIL)))
                        LOOPLABEL
                         (SETQ L (CDR L))
                         (COND ((NULL L) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (L) (GET L 'NO)) (CAR L)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (NAME_SESSION)
      (COND
       ((AND (NULL COLLECT_SOL) (NULL PARACRACK_INITIALIZED))
        (START_SOL_LIST_FILE)))
      (COND
       ((AND (FILEP "stop_now") OLD_HISTORY)
        (PROGN
         (PROGN
          (PRIN2
           "######## WARNING: The file 'stop_now' exists already at the start of the ")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "######## computation which will set the variable old_history to nil ! ")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "######## --> delete 'stop_now' with 'bm' if old_history should be used. ")
          NIL)
         (TERPRI))))
      (SETQ L (CRACKMAIN PDES FTEM_))
      (COND ((AND (PAIRP L) (FIXP (CAR L))) (SETQ L NIL)))
      (COND
       ((OR *TIME TIME_)
        (PROGN
         (TERPRI)
         (SETQ A (DIFFERENCE (TIME) N))
         (PROGN (PRIN2 "CRACK needed : ") (PRIN2 A) (PRIN2 " ms ") NIL)
         (PRINTDHMSTIME A)
         (TERPRI)
         (SETQ A (DIFFERENCE (GCTIME) M))
         (PROGN (PRIN2 "GC time : ") (PRIN2 A) (PRIN2 " ms ") NIL)
         (PRINTDHMSTIME A))))
      (SETQ L
              (CONS 'LIST
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A L)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (A)
                                          (PROGN
                                           (SETQ L1 NIL)
                                           (SETQ L2 (CADDR A))
                                           (PROG (B)
                                             (SETQ B (CADR A))
                                            LAB
                                             (COND ((NULL B) (RETURN NIL)))
                                             ((LAMBDA (B)
                                                (COND
                                                 ((AND (PAIRP B)
                                                       (EQUAL (CAR B) 'EQUAL))
                                                  (SETQ L1
                                                          (CONS
                                                           (LIST 'EQUAL
                                                                 (CADR B)
                                                                 (REVAL1
                                                                  (LIST '*SQ
                                                                        (CADDR
                                                                         B)
                                                                        NIL)
                                                                  NIL))
                                                           L1)))
                                                 (T (SETQ L2 (CONS B L2)))))
                                              (CAR B))
                                             (SETQ B (CDR B))
                                             (GO LAB))
                                           (LIST 'LIST
                                                 (CONS 'LIST
                                                       (PROG (M FORALL-RESULT
                                                              FORALL-ENDPTR)
                                                         (SETQ M (CAR A))
                                                         (COND
                                                          ((NULL M)
                                                           (RETURN NIL)))
                                                         (SETQ FORALL-RESULT
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CONS
                                                                          ((LAMBDA
                                                                               (
                                                                                M)
                                                                             (LIST
                                                                              '*SQ
                                                                              M
                                                                              T))
                                                                           (CAR
                                                                            M))
                                                                          NIL)))
                                                        LOOPLABEL
                                                         (SETQ M (CDR M))
                                                         (COND
                                                          ((NULL M)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (RPLACD FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (M)
                                                                     (LIST '*SQ
                                                                           M
                                                                           T))
                                                                   (CAR M))
                                                                  NIL))
                                                         (SETQ FORALL-ENDPTR
                                                                 (CDR
                                                                  FORALL-ENDPTR))
                                                         (GO LOOPLABEL)))
                                                 (CONS 'LIST L1)
                                                 (CONS 'LIST L2)
                                                 (CONS 'LIST
                                                       (APPEND
                                                        (PROG (M FORALL-RESULT
                                                               FORALL-ENDPTR)
                                                          (SETQ M (CADDDR A))
                                                          (COND
                                                           ((NULL M)
                                                            (RETURN NIL)))
                                                          (SETQ FORALL-RESULT
                                                                  (SETQ FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 M)
                                                                              (REVAL1
                                                                               (LIST
                                                                                '*SQ
                                                                                M
                                                                                NIL)
                                                                               NIL))
                                                                            (CAR
                                                                             M))
                                                                           NIL)))
                                                         LOOPLABEL
                                                          (SETQ M (CDR M))
                                                          (COND
                                                           ((NULL M)
                                                            (RETURN
                                                             FORALL-RESULT)))
                                                          (RPLACD FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (M)
                                                                      (REVAL1
                                                                       (LIST
                                                                        '*SQ M
                                                                        NIL)
                                                                       NIL))
                                                                    (CAR M))
                                                                   NIL))
                                                          (SETQ FORALL-ENDPTR
                                                                  (CDR
                                                                   FORALL-ENDPTR))
                                                          (GO LOOPLABEL))
                                                        (PROG (M FORALL-RESULT
                                                               FORALL-ENDPTR)
                                                          (SETQ M
                                                                  (CAR
                                                                   (CDDDDR A)))
                                                          (COND
                                                           ((NULL M)
                                                            (RETURN NIL)))
                                                          (SETQ FORALL-RESULT
                                                                  (SETQ FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 M)
                                                                              (CONS
                                                                               'LIST
                                                                               (PROG (N
                                                                                      FORALL-RESULT
                                                                                      FORALL-ENDPTR)
                                                                                 (SETQ N
                                                                                         M)
                                                                                 (COND
                                                                                  ((NULL
                                                                                    N)
                                                                                   (RETURN
                                                                                    NIL)))
                                                                                 (SETQ FORALL-RESULT
                                                                                         (SETQ FORALL-ENDPTR
                                                                                                 (CONS
                                                                                                  ((LAMBDA
                                                                                                       (
                                                                                                        N)
                                                                                                     (PROGN
                                                                                                      (SETQ P
                                                                                                              (SIMP
                                                                                                               1))
                                                                                                      (PROG (K)
                                                                                                        (SETQ K
                                                                                                                N)
                                                                                                       LAB
                                                                                                        (COND
                                                                                                         ((NULL
                                                                                                           K)
                                                                                                          (RETURN
                                                                                                           NIL)))
                                                                                                        ((LAMBDA
                                                                                                             (
                                                                                                              K)
                                                                                                           (SETQ P
                                                                                                                   (MULTSQ
                                                                                                                    K
                                                                                                                    P)))
                                                                                                         (CAR
                                                                                                          K))
                                                                                                        (SETQ K
                                                                                                                (CDR
                                                                                                                 K))
                                                                                                        (GO
                                                                                                         LAB))
                                                                                                      (REVAL1
                                                                                                       (LIST
                                                                                                        '*SQ
                                                                                                        P
                                                                                                        NIL)
                                                                                                       NIL)))
                                                                                                   (CAR
                                                                                                    N))
                                                                                                  NIL)))
                                                                                LOOPLABEL
                                                                                 (SETQ N
                                                                                         (CDR
                                                                                          N))
                                                                                 (COND
                                                                                  ((NULL
                                                                                    N)
                                                                                   (RETURN
                                                                                    FORALL-RESULT)))
                                                                                 (RPLACD
                                                                                  FORALL-ENDPTR
                                                                                  (CONS
                                                                                   ((LAMBDA
                                                                                        (
                                                                                         N)
                                                                                      (PROGN
                                                                                       (SETQ P
                                                                                               (SIMP
                                                                                                1))
                                                                                       (PROG (K)
                                                                                         (SETQ K
                                                                                                 N)
                                                                                        LAB
                                                                                         (COND
                                                                                          ((NULL
                                                                                            K)
                                                                                           (RETURN
                                                                                            NIL)))
                                                                                         ((LAMBDA
                                                                                              (
                                                                                               K)
                                                                                            (SETQ P
                                                                                                    (MULTSQ
                                                                                                     K
                                                                                                     P)))
                                                                                          (CAR
                                                                                           K))
                                                                                         (SETQ K
                                                                                                 (CDR
                                                                                                  K))
                                                                                         (GO
                                                                                          LAB))
                                                                                       (REVAL1
                                                                                        (LIST
                                                                                         '*SQ
                                                                                         P
                                                                                         NIL)
                                                                                        NIL)))
                                                                                    (CAR
                                                                                     N))
                                                                                   NIL))
                                                                                 (SETQ FORALL-ENDPTR
                                                                                         (CDR
                                                                                          FORALL-ENDPTR))
                                                                                 (GO
                                                                                  LOOPLABEL))))
                                                                            (CAR
                                                                             M))
                                                                           NIL)))
                                                         LOOPLABEL
                                                          (SETQ M (CDR M))
                                                          (COND
                                                           ((NULL M)
                                                            (RETURN
                                                             FORALL-RESULT)))
                                                          (RPLACD FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (M)
                                                                      (CONS
                                                                       'LIST
                                                                       (PROG (N
                                                                              FORALL-RESULT
                                                                              FORALL-ENDPTR)
                                                                         (SETQ N
                                                                                 M)
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            NIL)))
                                                                         (SETQ FORALL-RESULT
                                                                                 (SETQ FORALL-ENDPTR
                                                                                         (CONS
                                                                                          ((LAMBDA
                                                                                               (
                                                                                                N)
                                                                                             (PROGN
                                                                                              (SETQ P
                                                                                                      (SIMP
                                                                                                       1))
                                                                                              (PROG (K)
                                                                                                (SETQ K
                                                                                                        N)
                                                                                               LAB
                                                                                                (COND
                                                                                                 ((NULL
                                                                                                   K)
                                                                                                  (RETURN
                                                                                                   NIL)))
                                                                                                ((LAMBDA
                                                                                                     (
                                                                                                      K)
                                                                                                   (SETQ P
                                                                                                           (MULTSQ
                                                                                                            K
                                                                                                            P)))
                                                                                                 (CAR
                                                                                                  K))
                                                                                                (SETQ K
                                                                                                        (CDR
                                                                                                         K))
                                                                                                (GO
                                                                                                 LAB))
                                                                                              (REVAL1
                                                                                               (LIST
                                                                                                '*SQ
                                                                                                P
                                                                                                NIL)
                                                                                               NIL)))
                                                                                           (CAR
                                                                                            N))
                                                                                          NIL)))
                                                                        LOOPLABEL
                                                                         (SETQ N
                                                                                 (CDR
                                                                                  N))
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            FORALL-RESULT)))
                                                                         (RPLACD
                                                                          FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 N)
                                                                              (PROGN
                                                                               (SETQ P
                                                                                       (SIMP
                                                                                        1))
                                                                               (PROG (K)
                                                                                 (SETQ K
                                                                                         N)
                                                                                LAB
                                                                                 (COND
                                                                                  ((NULL
                                                                                    K)
                                                                                   (RETURN
                                                                                    NIL)))
                                                                                 ((LAMBDA
                                                                                      (
                                                                                       K)
                                                                                    (SETQ P
                                                                                            (MULTSQ
                                                                                             K
                                                                                             P)))
                                                                                  (CAR
                                                                                   K))
                                                                                 (SETQ K
                                                                                         (CDR
                                                                                          K))
                                                                                 (GO
                                                                                  LAB))
                                                                               (REVAL1
                                                                                (LIST
                                                                                 '*SQ
                                                                                 P
                                                                                 NIL)
                                                                                NIL)))
                                                                            (CAR
                                                                             N))
                                                                           NIL))
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CDR
                                                                                  FORALL-ENDPTR))
                                                                         (GO
                                                                          LOOPLABEL))))
                                                                    (CAR M))
                                                                   NIL))
                                                          (SETQ FORALL-ENDPTR
                                                                  (CDR
                                                                   FORALL-ENDPTR))
                                                          (GO LOOPLABEL)))))))
                                        (CAR A))
                                       NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (A)
                                  (PROGN
                                   (SETQ L1 NIL)
                                   (SETQ L2 (CADDR A))
                                   (PROG (B)
                                     (SETQ B (CADR A))
                                    LAB
                                     (COND ((NULL B) (RETURN NIL)))
                                     ((LAMBDA (B)
                                        (COND
                                         ((AND (PAIRP B)
                                               (EQUAL (CAR B) 'EQUAL))
                                          (SETQ L1
                                                  (CONS
                                                   (LIST 'EQUAL (CADR B)
                                                         (REVAL1
                                                          (LIST '*SQ (CADDR B)
                                                                NIL)
                                                          NIL))
                                                   L1)))
                                         (T (SETQ L2 (CONS B L2)))))
                                      (CAR B))
                                     (SETQ B (CDR B))
                                     (GO LAB))
                                   (LIST 'LIST
                                         (CONS 'LIST
                                               (PROG (M FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ M (CAR A))
                                                 (COND ((NULL M) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (M)
                                                                     (LIST '*SQ
                                                                           M
                                                                           T))
                                                                   (CAR M))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ M (CDR M))
                                                 (COND
                                                  ((NULL M)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (M)
                                                             (LIST '*SQ M T))
                                                           (CAR M))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                         (CONS 'LIST L1) (CONS 'LIST L2)
                                         (CONS 'LIST
                                               (APPEND
                                                (PROG (M FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ M (CADDDR A))
                                                  (COND
                                                   ((NULL M) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (M)
                                                                      (REVAL1
                                                                       (LIST
                                                                        '*SQ M
                                                                        NIL)
                                                                       NIL))
                                                                    (CAR M))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ M (CDR M))
                                                  (COND
                                                   ((NULL M)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (M)
                                                              (REVAL1
                                                               (LIST '*SQ M
                                                                     NIL)
                                                               NIL))
                                                            (CAR M))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL))
                                                (PROG (M FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ M (CAR (CDDDDR A)))
                                                  (COND
                                                   ((NULL M) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (M)
                                                                      (CONS
                                                                       'LIST
                                                                       (PROG (N
                                                                              FORALL-RESULT
                                                                              FORALL-ENDPTR)
                                                                         (SETQ N
                                                                                 M)
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            NIL)))
                                                                         (SETQ FORALL-RESULT
                                                                                 (SETQ FORALL-ENDPTR
                                                                                         (CONS
                                                                                          ((LAMBDA
                                                                                               (
                                                                                                N)
                                                                                             (PROGN
                                                                                              (SETQ P
                                                                                                      (SIMP
                                                                                                       1))
                                                                                              (PROG (K)
                                                                                                (SETQ K
                                                                                                        N)
                                                                                               LAB
                                                                                                (COND
                                                                                                 ((NULL
                                                                                                   K)
                                                                                                  (RETURN
                                                                                                   NIL)))
                                                                                                ((LAMBDA
                                                                                                     (
                                                                                                      K)
                                                                                                   (SETQ P
                                                                                                           (MULTSQ
                                                                                                            K
                                                                                                            P)))
                                                                                                 (CAR
                                                                                                  K))
                                                                                                (SETQ K
                                                                                                        (CDR
                                                                                                         K))
                                                                                                (GO
                                                                                                 LAB))
                                                                                              (REVAL1
                                                                                               (LIST
                                                                                                '*SQ
                                                                                                P
                                                                                                NIL)
                                                                                               NIL)))
                                                                                           (CAR
                                                                                            N))
                                                                                          NIL)))
                                                                        LOOPLABEL
                                                                         (SETQ N
                                                                                 (CDR
                                                                                  N))
                                                                         (COND
                                                                          ((NULL
                                                                            N)
                                                                           (RETURN
                                                                            FORALL-RESULT)))
                                                                         (RPLACD
                                                                          FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 N)
                                                                              (PROGN
                                                                               (SETQ P
                                                                                       (SIMP
                                                                                        1))
                                                                               (PROG (K)
                                                                                 (SETQ K
                                                                                         N)
                                                                                LAB
                                                                                 (COND
                                                                                  ((NULL
                                                                                    K)
                                                                                   (RETURN
                                                                                    NIL)))
                                                                                 ((LAMBDA
                                                                                      (
                                                                                       K)
                                                                                    (SETQ P
                                                                                            (MULTSQ
                                                                                             K
                                                                                             P)))
                                                                                  (CAR
                                                                                   K))
                                                                                 (SETQ K
                                                                                         (CDR
                                                                                          K))
                                                                                 (GO
                                                                                  LAB))
                                                                               (REVAL1
                                                                                (LIST
                                                                                 '*SQ
                                                                                 P
                                                                                 NIL)
                                                                                NIL)))
                                                                            (CAR
                                                                             N))
                                                                           NIL))
                                                                         (SETQ FORALL-ENDPTR
                                                                                 (CDR
                                                                                  FORALL-ENDPTR))
                                                                         (GO
                                                                          LOOPLABEL))))
                                                                    (CAR M))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ M (CDR M))
                                                  (COND
                                                   ((NULL M)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (M)
                                                              (CONS 'LIST
                                                                    (PROG (N
                                                                           FORALL-RESULT
                                                                           FORALL-ENDPTR)
                                                                      (SETQ N
                                                                              M)
                                                                      (COND
                                                                       ((NULL
                                                                         N)
                                                                        (RETURN
                                                                         NIL)))
                                                                      (SETQ FORALL-RESULT
                                                                              (SETQ FORALL-ENDPTR
                                                                                      (CONS
                                                                                       ((LAMBDA
                                                                                            (
                                                                                             N)
                                                                                          (PROGN
                                                                                           (SETQ P
                                                                                                   (SIMP
                                                                                                    1))
                                                                                           (PROG (K)
                                                                                             (SETQ K
                                                                                                     N)
                                                                                            LAB
                                                                                             (COND
                                                                                              ((NULL
                                                                                                K)
                                                                                               (RETURN
                                                                                                NIL)))
                                                                                             ((LAMBDA
                                                                                                  (
                                                                                                   K)
                                                                                                (SETQ P
                                                                                                        (MULTSQ
                                                                                                         K
                                                                                                         P)))
                                                                                              (CAR
                                                                                               K))
                                                                                             (SETQ K
                                                                                                     (CDR
                                                                                                      K))
                                                                                             (GO
                                                                                              LAB))
                                                                                           (REVAL1
                                                                                            (LIST
                                                                                             '*SQ
                                                                                             P
                                                                                             NIL)
                                                                                            NIL)))
                                                                                        (CAR
                                                                                         N))
                                                                                       NIL)))
                                                                     LOOPLABEL
                                                                      (SETQ N
                                                                              (CDR
                                                                               N))
                                                                      (COND
                                                                       ((NULL
                                                                         N)
                                                                        (RETURN
                                                                         FORALL-RESULT)))
                                                                      (RPLACD
                                                                       FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              N)
                                                                           (PROGN
                                                                            (SETQ P
                                                                                    (SIMP
                                                                                     1))
                                                                            (PROG (K)
                                                                              (SETQ K
                                                                                      N)
                                                                             LAB
                                                                              (COND
                                                                               ((NULL
                                                                                 K)
                                                                                (RETURN
                                                                                 NIL)))
                                                                              ((LAMBDA
                                                                                   (
                                                                                    K)
                                                                                 (SETQ P
                                                                                         (MULTSQ
                                                                                          K
                                                                                          P)))
                                                                               (CAR
                                                                                K))
                                                                              (SETQ K
                                                                                      (CDR
                                                                                       K))
                                                                              (GO
                                                                               LAB))
                                                                            (REVAL1
                                                                             (LIST
                                                                              '*SQ
                                                                              P
                                                                              NIL)
                                                                             NIL)))
                                                                         (CAR
                                                                          N))
                                                                        NIL))
                                                                      (SETQ FORALL-ENDPTR
                                                                              (CDR
                                                                               FORALL-ENDPTR))
                                                                      (GO
                                                                       LOOPLABEL))))
                                                            (CAR M))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL)))))))
                                (CAR A))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (COND ((NULL COLLECT_SOL) (ADD_TO_SOL_LIST)))
      (COND ((NULL COLLECT_SOL) (DELETE_EMPTY_SOL_LIST_FILE)))
      (SETQ NEQU_ 1)
      (SETQ RECYCLE_EQNS (CONS NIL NIL))
      (SETQ RECYCLE_FCTS NIL)
      (SETQ RECYCLE_IDS NIL)
      (SETQ LIN_PROBLEM NIL)
      (SETQ FLIN_ NIL)
      (SETQ FHOM_ NIL)
      (COND ((EQUAL EQN_INPUT 'DONE) (SETQ EQN_INPUT NIL))
            (EQN_INPUT (PROGN (CLOSE EQN_INPUT) (SETQ EQN_INPUT NIL))))
      (RECOVER_REDUCE_FLAGS)
      (COND
       ((AND PRINT_ (NULL COLLECT_SOL) (NULL PARACRACK_INITIALIZED) SESSION_)
        (DELETE_GENERATED_FILES)))
      (SETQ OLD_HISTORY NIL)
      (COND ((AND FORM_COMP FORM_PIPE) (PROGN NIL)))
      (COND
       ((AND PRINT_ LOGOPRINT_)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "This is the end of the CRACK run") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          (PRIN2 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          NIL)
         (TERPRI)
         NIL)))
      (RETURN L))) 
(PUT 'DELETE_GENERATED_FILES 'NUMBER-OF-ARGS 0) 
(PUT 'DELETE_GENERATED_FILES 'DEFINED-ON-LINE '365) 
(PUT 'DELETE_GENERATED_FILES 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'DELETE_GENERATED_FILES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DELETE_GENERATED_FILES NIL
    (PROG (P)
      (CHANGE_PROMPT_TO "")
      (PROGN
       (PRIN2 "!!!!! Delete generated files including solutions? (Y/N) ")
       NIL)
      (PROG ()
       REPEATLABEL
        (SETQ P (TERMREAD))
        (COND ((NOT (OR (EQUAL P 'Y) (EQUAL P 'N))) (GO REPEATLABEL))))
      (COND
       ((EQUAL P 'Y)
        (PROGN
         (PROGN
          (PRIN2
           "!!!!! Are you sure you want to delete all files generated in this session? (Y/N) ")
          NIL)
         (PROG ()
          REPEATLABEL
           (SETQ P (TERMREAD))
           (COND ((NOT (OR (EQUAL P 'Y) (EQUAL P 'N))) (GO REPEATLABEL))))
         (COND
          ((EQUAL P 'Y)
           (PROGN
            (SETQ SOL_LIST_FILE_CREATED NIL)
            (SETQ P
                    (BLDMSG_INTERNAL "rm ??%w*"
                                     (LIST
                                      (COMPRESS
                                       (CONS '|"|
                                             (CDDDR (EXPLODE SESSION_)))))))
            (SYSTEM P)))))))
      (RESTORE_INTERACTIVE_PROMPT))) 
(FLAG '(SAVE_ALL_SOL) 'OPFN) 
(PUT 'SAVE_ALL_SOL 'NUMBER-OF-ARGS 1) 
(PUT 'SAVE_ALL_SOL 'DEFINED-ON-LINE '387) 
(PUT 'SAVE_ALL_SOL 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SAVE_ALL_SOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SAVE_ALL_SOL (SOL)
    (PROG (S N)
      (COND ((NULL SOL_LIST_FILE_CREATED) (START_SOL_LIST_FILE)))
      (SETQ N 0)
      (PROG (S)
        (SETQ S (CDR SOL))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ N (PLUS N 1))
            (SETQ LEVEL_ (LIST N))
            (SETQ S (CDR S))
            (SAVE_SOLUTION (CDR (REVAL1 (CAR S) T)) (CDR (REVAL1 (CADR S) T))
             (CDR (REVAL1 (CADDR S) T)) (CDR (REVAL1 (CADDDR S) T)) NIL NIL)))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SAVE_SOL_LIST))) 
(PUT 'CRACKMAIN 'NUMBER-OF-ARGS 2) 
(PUT 'CRACKMAIN 'DEFINED-ON-LINE '410) 
(PUT 'CRACKMAIN 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CRACKMAIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CRACKMAIN (PDES FORG)
    (PROG (RESULT L CARPL UNSOLVABLE S H PL PS BATCH_ONE_STEP EXPERT_MODE_COPY
           FNC_TO_ADJUST STOP_ FNC_ADJUSTED LOOPCOUNT LEVEL_LENGTH NEWLI
           FULL_PROC_LIST_LENGTH SI_HI PLCNT NO_OF_CHILDREN PT2INEQ)
      (SETQ NO_OF_CHILDREN 0)
      (SETQ LEVEL_LENGTH (LENGTH LEVEL_))
      (SETQ FULL_PROC_LIST_LENGTH (LENGTH FULL_PROC_LIST_))
      (SETQ CHOOSE_70_65_8_47_ORIGTERMS NIL)
      (COND
       ((AND TR_MAIN PRINT_)
        (PROGN (TERPRI) (PROGN (PRIN2 "start of the main procedure") NIL))))
      (SETQ FNC_TO_ADJUST ADJUST_FNC)
      (COND
       (CONTRADICTION_
        (REDERR "** CONTRADICTION_ AT START OF CRACKMAIN()! **")))
      (SETQ CONTRADICTION_ NIL)
      (F_UPDATE PDES FORG)
      (SETQ PT2INEQ INEQ_)
     AGAIN
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ STOP_ NIL)
         (CHECK_STOP)
         (F_UPDATE PDES FORG)
         (SETQ VL_ (VAR_LIST PDES FORG VL_))
         (COND ((NOT (EQUAL PT2INEQ INEQ_)) (PROGN (SETQ PT2INEQ INEQ_))))
         (SETQ PDES (UPDATE_EQ_SORT_BY_LENGTH PDES))
         (COND
          (TIME_LIMIT
           (PROGN
            (SETQ L (TIME))
            (COND
             ((AND (EQUAL TIME_LIMIT 2) (LESSP LIMIT_TIME L))
              (REDERR "PREMATURE STOP DUE TO TIME CONSTRAINT!"))))))
         (COND
          ((OR *BATCH_MODE TO_DO_LIST BATCH_ONE_STEP
               (AND (GEQ BATCHCOUNT_ STEPCOUNTER_)
                    (OR (EQUAL TIME_LIMIT NIL) (GEQ LIMIT_TIME L))))
           (PROGN
            (COND
             (*BATCH_MODE
              (COND
               (PRINT_MORE
                (PRINT_PDE_FORG_INEQ PDES (CONS INEQ_ INEQ_OR)
                 (APPEND FORG (SETDIFF FTEM_ FORG)) VL_)))))
            (COND
             (SIZE_WATCH
              (SETQ SI_HI
                      (GET_STATISTIC PDES
                       (APPEND FORG (SETDIFF FTEM_ FORG))))))
            (SETQ STEPCOUNTER_ (ADD1 STEPCOUNTER_))
            (CLEAN_PROP_LIST PDES)
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "Step ") (PRIN2 STEPCOUNTER_) (PRIN2 ": ") NIL)
               (COND (PRINT_MORE (TERPRI))))))
            (SETQ BATCH_ONE_STEP NIL)
            (SETQ EXPERT_MODE_COPY EXPERT_MODE)
            (COND
             ((OR (NULL TO_DO_LIST) (NEQ (CAAR TO_DO_LIST) 'SPLIT_INTO_CASES))
              (SETQ EXPERT_MODE NIL)))
            (SETQ PLCNT 1)
            (PROG ()
             WHILELABEL
              (COND ((NOT (LEQ PLCNT (LENGTH PROC_LIST_))) (RETURN NIL)))
              (PROGN
               (SETQ CARPL (NTH PROC_LIST_ PLCNT))
               (COND
                ((AND PRINT_ PRINT_MORE)
                 (COND
                  ((PAIRP (SETQ L (GET CARPL 'DESCRIPTION)))
                   (PROGN
                    (PROG (A)
                      (SETQ A L)
                     LAB
                      (COND ((NULL A) (RETURN NIL)))
                      ((LAMBDA (A) (COND (A (PROGN (PRIN2 A) NIL)))) (CAR A))
                      (SETQ A (CDR A))
                      (GO LAB))
                    (PROGN (PRIN2 " : ") NIL)))
                  (T
                   (PROGN
                    (PRIN2 "trying ")
                    (PRIN2 CARPL)
                    (PRIN2 " : ")
                    NIL)))))
               (SETQ L (APPLY CARPL (LIST (LIST PDES FORG VL_ PDES))))
               (COND
                ((AND SIZE_WATCH
                      (OR CONTRADICTION_ (AND L (GREATERP (LENGTH L) 1))))
                 (SETQ SIZE_HIST
                         (CONS (CONS (GET CARPL 'NO) SI_HI) SIZE_HIST))))
               (COND
                ((AND (FIXP SIZE_WATCH)
                      (EQUAL
                       (DIFFERENCE STEPCOUNTER_
                                   (TIMES SIZE_WATCH
                                          (QUOTIENT STEPCOUNTER_ SIZE_WATCH)))
                       0))
                 (CUT_SIZE_HIST)))
               (COND
                ((AND (NULL CHOOSE_70_65_8_47_ORIGTERMS) SIZE_HIST)
                 (PROGN
                  (SETQ S SIZE_HIST)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND S (NOT (FIXP (CAAR S))))) (RETURN NIL)))
                    (SETQ S (CDR S))
                    (GO WHILELABEL))
                  (SETQ CHOOSE_70_65_8_47_ORIGTERMS
                          (COND (S (CADDR (CDDDAR S))) (T 1000))))))
               (COND
                ((AND (EQUAL (LENGTH L) 1) (NULL (CAR L)))
                 (SETQ CONTRADICTION_ T)))
               (COND
                ((AND L (NOT CONTRADICTION_))
                 (PROGN
                  (COND ((EQUAL (LENGTH L) 1) (SETQ RESULT (CAR L)))
                        (T (PROGN (SETQ PDES (CAR L)) (SETQ FORG (CADR L)))))
                  (SETQ PLCNT 100000)))
                (CONTRADICTION_ (SETQ PLCNT 100000))
                (T
                 (PROGN
                  (SETQ PLCNT (ADD1 PLCNT))
                  (COND
                   ((AND PRINT_ PRINT_MORE)
                    (PROGN (PROGN (PRIN2 " --- ") NIL) (TERPRI))))
                  (COND
                   ((AND (GREATERP PLCNT (LENGTH PROC_LIST_))
                         (NULL EQN_TO_BE_GEN))
                    (SETQ UNSOLVABLE T)))))))
              (GO WHILELABEL))
            (SETQ EXPERT_MODE EXPERT_MODE_COPY)
            NIL))
          (T
           (PROGN
            (COND
             ((AND PRINT_ TIME_LIMIT (LESSP LIMIT_TIME L))
              (PROGN
               (PROGN
                (PRIN2
                 "The time limit for automatic execution has been reached.")
                NIL)
               (TERPRI))))
            (RDS NIL)
            (WRS NIL)
            (CHANGE_PROMPT_TO "next: ")
            (COND ((OR PRINT_ (NULL OLD_HISTORY)) (TERPRI)))
            (SETQ S (TERMREAD))
            (COND
             ((OR (EQUAL S 'H) (EQUAL S 'HELP) (EQUAL S '?) (EQUAL S NIL))
              (PRINTMAINMENU))
             ((EQUAL S 'HD) (PRINT_HD)) ((EQUAL S 'HP) (PRINT_HP))
             ((EQUAL S 'HF) (PRINT_HF)) ((EQUAL S 'HC) (PRINT_HC))
             ((AND (EQUAL S 'HI) (GETD 'SHOW_ID)) (PRINT_HI))
             ((EQUAL S 'HB) (PRINT_HB))
             ((AND (EQUAL S 'HL)
                   (OR (MEMQ 'UNIX LISPSYSTEM*) (MEMQ 'LINUX-GNU LISPSYSTEM*)
                       (MEMQ 'DARWIN13.4.0 LISPSYSTEM*)))
              (PRINT_HL))
             ((EQUAL S 'HE) (PRINT_HE))
             ((EQUAL S 'E)
              (COND (EXPERT_MODE (PRINT_PDES (SELECTPDES PDES 1)))
                    (T (PRINT_PDES PDES))))
             ((EQUAL S 'EO)
              (PROGN
               (SETQ PS PRINT_)
               (SETQ PRINT_ 1)
               (SYMBOL_EXPLANATION)
               (PROG (S)
                 (SETQ S PDES)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PROGN
                     (TERPRI)
                     (PROGN (PRIN2 S) (PRIN2 " : ") NIL)
                     (TYPEEQ S)
                     (PLOT_NON0_SEPARANTS S)
                     (LIST_POSSIBLE_SUBS S)))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               (SETQ PRINT_ PS)))
             ((EQUAL S 'PI) (PRINT_INEQ (CONS INEQ_ INEQ_OR)))
             ((EQUAL S 'F)
              (PROGN
               (PRINT_FORG (APPEND (SETDIFF FORG FTEM_) FTEM_) VL_)
               (TERPRI)))
             ((EQUAL S 'V)
              (PROGN
               (PRINT_FCTS PDES (APPEND FORG (SETDIFF FTEM_ FORG)))
               (TERPRI)))
             ((EQUAL S 'S)
              (PROGN
               (PRINT_LEVEL 1)
               (PRINT_STATISTIC PDES (APPEND FORG (SETDIFF FTEM_ FORG)))))
             ((EQUAL S 'FC)
              (PROGN
               (RECLAIM)
               (TERPRI)
               (PROGN
                (PRIN2 "Used memory: ")
                (PRIN2 *USED-SPACE*)
                (PRIN2 "KB")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "Free memory (without requesting more): ")
                (PRIN2 *AVAIL-SPACE*)
                (PRIN2 "KB")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 (COUNTIDS)) (PRIN2 " identifiers in use") NIL)
               NIL
               (TERPRI)))
             ((EQUAL S 'PE)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (TERPRI)
               (PROGN (PRIN2 "Which expression do you want to print?") NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "You can use names of equations, e.g. coeffn(e_12,df(f,x,y),2); ")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "Terminate the expression with ; ") NIL)
               (TERPRI)
               (SETQ L (TERMXREAD))
               (PROG (S)
                 (SETQ S PDES)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (COND
                     ((NOT (FREEOF L S))
                      (SETQ L (SUBST (LIST '*SQ (GET S 'SQVAL) T) S L)))))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               (SETQ L (REVAL1 L T))
               (PROG (S)
                 (SETQ S FORG)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (COND
                     ((AND (PAIRP S) (EQUAL (CAR S) 'EQUAL))
                      (SETQ L
                              (REVAL1
                               (SUBST (LIST '*SQ (CADDR S) T) (CADR S) L)
                               T)))))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               (TERPRI)
               (MATHPRINT (REVAL1 L T))))
             ((EQUAL S 'PH) (PROGN (TERPRI) (PRI_HIST (CLEAN_HIST))))
             ((EQUAL S 'PV)
              (PROGN
               (PROGN
                (PRIN2
                 "Type in a variable from which you want to know its value: ")
                NIL)
               (CHANGE_PROMPT_TO "")
               (SETQ S (TERMREAD))
               (COND
                ((NOT (ATOM S))
                 (PROGN (PRIN2 "This is not a variable name.") NIL))
                ((NULL (BOUNDP S))
                 (PROGN (PRIN2 S) (PRIN2 " has no value") NIL))
                (T
                 (PROGN
                  (PROGN (PRIN2 S) (PRIN2 " = ") NIL)
                  (PRETTYPRINT (EVAL S)))))))
             ((EQUAL S 'PF) (HOW_OFTEN PDES))
             ((EQUAL S 'PO) (PRINT_FACTORS PDES))
             ((EQUAL S 'PU) (PRINT_COEFFICIENTS PDES))
             ((EQUAL S 'PR)
              (PROGN
               (PROGN (PRIN2 "The currently active substitution rules: ") NIL)
               (TERPRI)
               (ASSGNPRI (AEVAL* USERRULES_) NIL 'ONLY)
               (TERPRI)))
             ((EQUAL S 'PD) (PLOT_DEPENDENCIES PDES))
             ((EQUAL S 'PS) (PLOT_STATISTICS SIZE_HIST))
             ((EQUAL S 'LC) (LIST_CASES SIZE_HIST))
             ((EQUAL S 'CA) (LIST_CURRENT_CASE_ASSUMPTIONS))
             ((EQUAL S 'WS) (WRITE_STAT_IN_FILE))
             ((EQUAL S 'SN)
              (PROGN
               (PROGN
                (PRIN2 "The name of this session is: \"")
                (PRIN2 SESSION_)
                (PRIN2 "\"")
                NIL)
               (TERPRI)))
             ((EQUAL S 'SS) (ERR_CATCH_SUBSYS PDES))
             ((EQUAL S 'W) (WRITE_IN_FILE PDES FORG))
             ((EQUAL S 'A) (SETQ BATCH_ONE_STEP T))
             ((EQUAL S 'G)
              (PROGN
               (CHANGE_PROMPT_TO "number of steps: ")
               (SETQ S (TERMREAD))
               (COND ((FIXP S) (SETQ BATCHCOUNT_ (PLUS (SUB1 STEPCOUNTER_) S)))
                     (T
                      (PROGN (PROGN (PRIN2 "wrong input!!!") NIL) (TERPRI))))))
             ((EQUAL S 'T)
              (PROGN
               (SETQ EXPERT_MODE (NOT EXPERT_MODE))
               (COND
                (EXPERT_MODE
                 (PROGN
                  (PRIN2 "From now on the USER will choose equations.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "From now on the COMPUTER will choose equations.")
                  NIL)))
               (SETQ EXPERT_MODE_COPY EXPERT_MODE)))
             ((EQUAL S 'P1) (PRINTPROCLIST))
             ((EQUAL S 'P2) (PRINTFULLPROCLIST))
             ((EQUAL S '|#|)
              (PROGN
               (PROGN
                (PRIN2 "Type in a number instead of `#' to ")
                (PRIN2 "execute a specific module.")
                NIL)
               (TERPRI)))
             ((OR (EQUAL S 'L) (NUMBERP S))
              (PROGN
               (SETQ LAST_STEPS NIL)
               (COND
                ((EQUAL S 'L)
                 (PROGN
                  (SETQ REPEAT_MODE T)
                  (COND
                   ((OR PRINT_ (NULL OLD_HISTORY))
                    (PROGN
                     (CHANGE_PROMPT_TO "")
                     (PROGN
                      (PRIN2
                       "Select a method by its number that is to be executed ")
                      (PRIN2 "repeatedly:")
                      NIL)
                     (TERPRI)
                     NIL)))
                  (SETQ S (TERMREAD))
                  (COND
                   ((OR PRINT_ (NULL OLD_HISTORY))
                    (PROGN
                     (TERPRI)
                     (PROGN
                      (PRIN2
                       "To repeat this method as often as possible, enter `;' ")
                      NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 "To repeat this method as often as possible, ")
                      (PRIN2 "but at most a number of n times, enter n :")
                      NIL)
                     (TERPRI)
                     NIL)))
                  (SETQ REPEAT_MODE (TERMREAD))
                  (COND ((NOT (NUMBERP REPEAT_MODE)) (SETQ REPEAT_MODE T))))))
               (COND
                ((OR (LEQ S 0) (GREATERP S FULL_PROC_LIST_LENGTH))
                 (COND
                  (PRINT_
                   (PROGN
                    (COND
                     ((AND SIZE_WATCH (NOT (FIXP SIZE_WATCH)))
                      (SETQ HISTORY_
                              (CONS
                               "*** The number is not in the allowed interval."
                               (CONS 'IG HISTORY_)))))
                    (PROGN
                     (PRIN2 "The number must be in 1 .. ")
                     (PRIN2 FULL_PROC_LIST_LENGTH)
                     (PRIN2 " .")
                     NIL)
                    (TERPRI)))
                  (T NIL)))
                (T
                 (PROGN
                  (SETQ LOOPCOUNT 0)
                  (COND
                   (SIZE_WATCH
                    (SETQ SI_HI
                            (GET_STATISTIC PDES
                             (APPEND FORG (SETDIFF FTEM_ FORG))))))
                  (SETQ STEPCOUNTER_ (ADD1 STEPCOUNTER_))
                  (CLEAN_PROP_LIST PDES)
                  (COND
                   (PRINT_
                    (PROGN
                     (TERPRI)
                     (PROGN
                      (PRIN2 "Step ")
                      (PRIN2 STEPCOUNTER_)
                      (PRIN2 ": ")
                      NIL))))
                  (PROG ()
                   REPEATLABEL
                    (PROGN
                     (COND (TO_DO_LIST (SETQ LOOPCOUNT (SUB1 LOOPCOUNT))))
                     (SETQ PS
                             (COND (TO_DO_LIST 'TO_DO)
                                   (T (NTH FULL_PROC_LIST_ S))))
                     (SETQ L (APPLY PS (LIST (LIST PDES FORG VL_ PDES))))
                     (COND
                      ((AND (NEQ REPEAT_MODE T) (NULL L) (NULL CONTRADICTION_)
                            (NULL TO_DO_LIST) SIZE_WATCH
                            (NOT (FIXP SIZE_WATCH)))
                       (SETQ HISTORY_
                               (CONS
                                (BLDMSG_INTERNAL "*** %w un-succ." (LIST PS))
                                (CONS 'IG HISTORY_)))))
                     (COND
                      ((AND SIZE_WATCH (OR L CONTRADICTION_))
                       (SETQ SIZE_HIST (CONS (CONS S SI_HI) SIZE_HIST))))
                     (COND
                      ((AND (FIXP SIZE_WATCH) (NEQ LOOPCOUNT 0)
                            (EQUAL
                             (DIFFERENCE LOOPCOUNT
                                         (TIMES SIZE_WATCH
                                                (QUOTIENT LOOPCOUNT
                                                          SIZE_WATCH)))
                             0))
                       (CUT_SIZE_HIST)))
                     (COND
                      ((AND (NULL CHOOSE_70_65_8_47_ORIGTERMS) SIZE_HIST)
                       (PROGN
                        (SETQ PS SIZE_HIST)
                        (PROG ()
                         WHILELABEL
                          (COND
                           ((NOT (AND PS (NOT (FIXP (CAAR PS)))))
                            (RETURN NIL)))
                          (SETQ PS (CDR PS))
                          (GO WHILELABEL))
                        (SETQ CHOOSE_70_65_8_47_ORIGTERMS
                                (COND (PS (CADDR (CDDDAR PS))) (T 1000))))))
                     (COND
                      ((AND (EQUAL (LENGTH L) 1) (NULL (CAR L)))
                       (SETQ CONTRADICTION_ T)))
                     (COND
                      ((AND L (NOT CONTRADICTION_))
                       (PROGN
                        (SETQ LOOPCOUNT (ADD1 LOOPCOUNT))
                        (COND ((EQUAL (LENGTH L) 1) (SETQ RESULT (CAR L)))
                              (T
                               (PROGN
                                (SETQ PDES (CAR L))
                                (SETQ FORG (CADR L)))))
                        (COND (PRINT_ (TERPRI)))
                        (CHECK_STOP)
                        (COND
                         ((AND TO_DO_LIST (NUMBERP REPEAT_MODE))
                          (SETQ REPEAT_MODE (ADD1 REPEAT_MODE))))
                        (COND ((EQUAL REPEAT_MODE 1) (SETQ REPEAT_MODE NIL))
                              (REPEAT_MODE
                               (PROGN
                                (COND
                                 ((NUMBERP REPEAT_MODE)
                                  (SETQ REPEAT_MODE (SUB1 REPEAT_MODE))))
                                (COND
                                 (SIZE_WATCH
                                  (SETQ SI_HI
                                          (GET_STATISTIC PDES
                                           (APPEND FORG
                                                   (SETDIFF FTEM_ FORG))))))
                                (SETQ STEPCOUNTER_ (ADD1 STEPCOUNTER_))
                                (CLEAN_PROP_LIST PDES)
                                (COND
                                 (PRINT_
                                  (PROGN
                                   (TERPRI)
                                   NIL
                                   (PROGN
                                    (PRIN2 "Step ")
                                    (PRIN2 STEPCOUNTER_)
                                    (PRIN2 ": ")
                                    NIL))))
                                NIL)))))
                      ((AND (NOT CONTRADICTION_) (EQUAL LOOPCOUNT 0)
                            (OR PRINT_ (NULL OLD_HISTORY)))
                       (PROGN (PROGN (PRIN2 "no success") NIL) (TERPRI)))))
                    (COND
                     ((NOT
                       (OR (NOT REPEAT_MODE) RESULT (NOT L) CONTRADICTION_
                           (AND TIME_LIMIT
                                (PROGN
                                 (SETQ PS (TIME))
                                 (LESSP LIMIT_TIME PS)))))
                      (GO REPEATLABEL))))
                  NIL)))
               (SETQ REPEAT_MODE NIL)))
             ((EQUAL S 'SB) (BACKUP_TO_FILE PDES FORG T))
             ((EQUAL S 'RB)
              (PROGN
               (SETQ L (RESTORE_BACKUP_FROM_FILE PDES FORG T))
               (SETQ PDES (CAR L))
               (SETQ FORG (CADR L))
               (SETQ LEVEL_LENGTH (PLUS 1 (LENGTH LEVEL_)))
               (COND
                ((NULL AUTO_PARA_MODE)
                 (SETQ BATCHCOUNT_ (SUB1 STEPCOUNTER_))))))
             ((EQUAL S 'BM) (SYSTEM "rm stop_now"))
             ((EQUAL S 'AN) (SETQ PDES (FLIN_NON_TRIV_COND PDES)))
             ((EQUAL S 'RS) (SETQ PDES (COMP_RESULTANT PDES)))
             ((OR (EQUAL S 'X) (EQUAL S (INT2ID 4))) (SETQ *BATCH_MODE T))
             ((EQUAL S 'Q)
              (PROGN
               (COND
                ((AND EQN_TO_BE_GEN EQN_INPUT (NEQ EQN_INPUT 'DONE))
                 (PROGN (CLOSE EQN_INPUT) (SETQ EQN_INPUT 'DONE))))
               (SETQ STOP_ T)))
             ((EQUAL S 'QH)
              (PROGN
               (COND
                ((AND EQN_TO_BE_GEN EQN_INPUT (NEQ EQN_INPUT 'DONE))
                 (PROGN (CLOSE EQN_INPUT) (SETQ EQN_INPUT 'DONE))))
               (SETQ CONTRADICTION_ T)
               (SETQ STOP_ T)))
             ((EQUAL S 'QQ)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "!!!!! Do you want to stop the computation completely on all levels? (Y/N) ")
                NIL)
               (PROG ()
                REPEATLABEL
                 (SETQ S (TERMREAD))
                 (COND
                  ((NOT (OR (EQUAL S 'Y) (EQUAL S 'N))) (GO REPEATLABEL))))
               (COND
                ((EQUAL S 'Y)
                 (PROGN
                  (PROGN (PRIN2 "!!!!! Are you sure? (Y/N) ") NIL)
                  (PROG ()
                   REPEATLABEL
                    (SETQ S (TERMREAD))
                    (COND
                     ((NOT (OR (EQUAL S 'Y) (EQUAL S 'N))) (GO REPEATLABEL))))
                  (RESTORE_INTERACTIVE_PROMPT)
                  (COND
                   ((EQUAL S 'Y)
                    (PROGN
                     (PROGN (PRIN2 "This was your input (uncleaned): ") NIL)
                     (TERPRI)
                     (PRI_HIST HISTORY_)
                     (TERPRI)
                     (PROGN (PRIN2 "This was your input (cleaned): ") NIL)
                     (TERPRI)
                     (PRI_HIST (CLEAN_HIST))
                     (TERPRI)
                     (COND
                      ((AND EQN_TO_BE_GEN EQN_INPUT (NEQ EQN_INPUT 'DONE))
                       (PROGN (CLOSE EQN_INPUT) (SETQ EQN_INPUT 'DONE))))
                     (SETQ CONTRADICTION_ T)
                     (SETQ STOP_ T)
                     (SETQ LEVEL_ NIL)
                     (SETQ STEPCOUNTER_ 0)
                     (SETQ BATCHCOUNT_ (MINUS 1))
                     (DELETE_GENERATED_FILES)
                     (SETQ SESSION_ NIL)
                     (REDERR "")
                     NIL))))))))
             ((EQUAL S 'PL)
              (PROGN
               (CHANGE_PROMPT_TO "Print length : ")
               (SETQ S (TERMREAD))
               (COND ((OR (NOT S) (FIXP S)) (SETQ PRINT_ S))
                     (T
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "Print length must be NIL or an integer!!!")
                        NIL)
                       (TERPRI))))))
             ((EQUAL S 'PM)
              (PROGN
               (SETQ PRINT_MORE (NOT PRINT_MORE))
               (COND
                (PRINT_MORE
                 (PROGN (PRIN2 "More details will be printed.") NIL))
                (T (PROGN (PRIN2 "Fewer details will be printed.") NIL)))
               (TERPRI)))
             ((EQUAL S 'PA)
              (PROGN
               (SETQ PRINT_ALL (NOT PRINT_ALL))
               (COND
                (PRINT_ALL
                 (PROGN
                  (PRIN2 "All equation properties will be printed.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "No equation properties will be printed.")
                  NIL)))
               (TERPRI)))
             ((EQUAL S 'CP) (CHANGEPROCLIST))
             ((EQUAL S 'OG)
              (PROGN
               (SETQ LEX_FC (NOT LEX_FC))
               (COND
                (LEX_FC
                 (PROGN
                  (PRIN2
                   "Lex. ordering of functions has now highest priority.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2
                   "Lex. ordering of functions is not of highest priority anymore.")
                  NIL)))
               (TERPRI)
               (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
               NIL))
             ((EQUAL S 'OD)
              (PROGN
               (SETQ LEX_DF (NOT LEX_DF))
               (COND
                (LEX_DF
                 (PROGN
                  (PRIN2 "From now on lexicographic ordering of derivatives.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "From now on total-degree ordering of derivatives.")
                  NIL)))
               (TERPRI)
               (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
               NIL))
             ((EQUAL S 'OI)
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "Current variable ordering is : ") NIL)
               (SETQ S VL_)
               (PROG ()
                WHILELABEL
                 (COND ((NOT S) (RETURN NIL)))
                 (PROGN
                  (PROGN (PRIN2 (CAR S)) NIL)
                  (SETQ S (CDR S))
                  (COND (S (PROGN (PRIN2 ",") NIL))))
                 (GO WHILELABEL))
               (PROGN (PRIN2 ";") NIL)
               (TERPRI)
               (CHANGE_PROMPT_TO "New variable ordering : ")
               (SETQ NEWLI (TERMLISTREAD))
               (COND
                (NEWLI
                 (PROGN
                  (COND
                   ((NOT_INCLUDED VL_ NEWLI)
                    (PROGN
                     (PROGN
                      (PRIN2 "Not all variables appear in the new list.")
                      NIL)
                     (TERPRI)))
                   ((NOT_INCLUDED NEWLI VL_)
                    (PROGN
                     (PROGN (PRIN2 "The new list has extra ariables.") NIL)
                     (TERPRI)))
                   (T
                    (PROGN
                     (SETQ VL_ NEWLI)
                     (PROG (S)
                       (SETQ S PDES)
                      LAB
                       (COND ((NULL S) (RETURN NIL)))
                       ((LAMBDA (S)
                          (PUT S 'VARS (SORT_ACCORDING_TO (GET S 'VARS) VL_)))
                        (CAR S))
                       (SETQ S (CDR S))
                       (GO LAB))
                     (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
                     (COND
                      (TR_ORDERINGS
                       (PROGN
                        (TERPRI)
                        (PROGN (PRIN2 "New variable list: ") (PRIN2 VL_) NIL)
                        NIL))))))
                  NIL)))
               NIL))
             ((EQUAL S 'OR)
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2
                 "The current variable ordering is going to be reversed. ")
                NIL)
               (SETQ VL_ (REVERSE VL_))
               (PROG (S)
                 (SETQ S PDES)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PUT S 'VARS (SORT_ACCORDING_TO (GET S 'VARS) VL_)))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
               (COND
                (TR_ORDERINGS
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "New variable list: ") (PRIN2 VL_) NIL)
                  NIL)))))
             ((EQUAL S 'OM)
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "The current variable ordering is going to be mixed. ")
                NIL)
               (SETQ S VL_)
               (SETQ VL_ NIL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT S) (RETURN NIL)))
                 (PROGN
                  (SETQ L (NTH S (ADD1 (RANDOM (LENGTH S)))))
                  (SETQ S (DELETE L S))
                  (SETQ VL_ (CONS L VL_))
                  NIL)
                 (GO WHILELABEL))
               (PROG (S)
                 (SETQ S PDES)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PUT S 'VARS (SORT_ACCORDING_TO (GET S 'VARS) VL_)))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
               (COND
                (TR_ORDERINGS
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "New variable list: ") (PRIN2 VL_) NIL)
                  NIL)))))
             ((EQUAL S 'OF)
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "Current function ordering is : ") NIL)
               (SETQ S FTEM_)
               (PROG ()
                WHILELABEL
                 (COND ((NOT S) (RETURN NIL)))
                 (PROGN
                  (PROGN (PRIN2 (CAR S)) NIL)
                  (SETQ S (CDR S))
                  (COND (S (PROGN (PRIN2 ",") NIL))))
                 (GO WHILELABEL))
               (PROGN (PRIN2 ";") NIL)
               (TERPRI)
               (COND
                ((NULL VL_)
                 (PROGN
                  (PROGN
                   (PRIN2
                    "If you want to sort functions according to frequency, rare")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "functions first, then type:    sort_by_frequency;")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "If you want to sort functions randomly, flin_ first and")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "non-vanishing last, then type:    sort_randomly;")
                   NIL)
                  (TERPRI)
                  (PROGN (PRIN2 "else type the new list.") NIL)
                  (TERPRI))))
               (CHANGE_PROMPT_TO "New function ordering : ")
               (SETQ NEWLI (TERMLISTREAD))
               (COND
                ((AND (NULL VL_) NEWLI (EQUAL (CAR NEWLI) 'SORT_BY_FREQUENCY))
                 (PROGN (HOW_OFTEN PDES) (SETQ NEWLI (FTEM_SORTED_BY_INDEX))))
                ((AND (NULL VL_) NEWLI (EQUAL (CAR NEWLI) 'SORT_RANDOMLY))
                 (PROGN
                  (SETQ S FTEM_)
                  (SETQ BACKUP_ NIL)
                  (SETQ H (LENGTH FTEM_))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (GREATERP H 1)) (RETURN NIL)))
                    (PROGN
                     (SETQ L (NTH S (PLUS 1 (RANDOM H))))
                     (SETQ BACKUP_ (CONS (CONS H L) BACKUP_))
                     (SETQ S (DELETE L S))
                     (SETQ H (DIFFERENCE H 1)))
                    (GO WHILELABEL))
                  (SETQ BACKUP_ (CONS (CONS 1 (CAR S)) BACKUP_))
                  (SETQ NEWLI (FTEM_SORTED_BY_INDEX))))
                ((AND NEWLI
                      (OR (NOT_INCLUDED FTEM_ NEWLI)
                          (NOT_INCLUDED NEWLI FTEM_)))
                 (SETQ NEWLI NIL)))
               (COND (NEWLI (CHANGE_FCTS_ORDERING NEWLI PDES VL_)))))
             ((EQUAL S 'OP)
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "Current orderings are :") NIL)
               (TERPRI)
               (PROGN (PRIN2 "Functions : ") (PRIN2 FTEM_) NIL)
               (TERPRI)
               (PROGN (PRIN2 "Variables : ") (PRIN2 VL_) NIL)
               NIL))
             ((EQUAL S 'NE)
              (PROGN
               (CHANGE_PROMPT_TO "Equation name : ")
               (SETQ S (TERMREAD))
               (COND ((AND S (IDP S)) (SETQ EQNAME_ S))
                     (T
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "Equation name must be an identifier!!")
                        NIL)
                       (TERPRI))))))
             ((EQUAL S 'NF)
              (PROGN
               (CHANGE_PROMPT_TO "Function name : ")
               (SETQ S (TERMREAD))
               (COND ((AND S (IDP S)) (SETQ FNAME_ S))
                     (T
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "Function name must be an identifier!!")
                        NIL)
                       (TERPRI))))))
             ((EQUAL S 'NI)
              (PROGN
               (CHANGE_PROMPT_TO "Identity name : ")
               (SETQ S (TERMREAD))
               (COND ((AND S (IDP S)) (SETQ IDNAME_ S))
                     (T
                      (PROGN
                       (TERPRI)
                       (PROGN
                        (PRIN2 "Identity name must be an identifier!!")
                        NIL)
                       (TERPRI))))))
             ((EQUAL S 'NA)
              (PROGN
               (SETQ *NAT (NOT *NAT))
               (COND (*NAT (PROGN (PRIN2 "NAT is now on.") NIL))
                     (T (PROGN (PRIN2 "NAT is now off.") NIL)))))
             ((EQUAL S 'AS)
              (PROGN
               (CHANGE_PROMPT_TO "The variable name to be assigned: ")
               (SETQ S (TERMREAD))
               (PROGN
                (PRIN2 "What is the value to be assigned to that variable?")
                NIL)
               (TERPRI)
               (CHANGE_PROMPT_TO "Please terminate this input with ';'  : ")
               (SETQ L (TERMXREAD))
               (COND
                ((AND (EQUAL S 'COLLECT_SOL) (EQUAL L NIL) COLLECT_SOL)
                 (COND ((NULL SESSION_) (START_SOL_LIST_FILE))
                       (T (SAVE_SOL_LIST))))
                ((EQUAL S 'SESSION_)
                 (COPY-FILE (BLDMSG_INTERNAL "%wsol_list" (LIST SESSION_))
                  (BLDMSG_INTERNAL "%ssol_list" (LIST L)))))
               (SET S (REVAL1 L T))
               NIL))
             ((EQUAL S 'KE)
              (COND
               (KEEP_PARTI
                (PROGN
                 (SETQ KEEP_PARTI NIL)
                 (PROG (L)
                   (SETQ L PDES)
                  LAB
                   (COND ((NULL L) (RETURN NIL)))
                   ((LAMBDA (L) (PUT L 'PARTITIONED NIL)) (CAR L))
                   (SETQ L (CDR L))
                   (GO LAB))))
               (T (SETQ KEEP_PARTI T))))
             ((EQUAL S 'FI)
              (PROGN
               (SETQ FREEINT_ (NOT FREEINT_))
               (COND
                (FREEINT_
                 (PROGN
                  (PRIN2 "Integration only if result free ")
                  (PRIN2 "of explicit integral from now on.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "Integration result may involve ")
                  (PRIN2 "explicit integral from now on.")
                  NIL)))
               NIL))
             ((EQUAL S 'FA)
              (PROGN
               (SETQ FREEABS_ (NOT FREEABS_))
               (COND
                (FREEABS_
                 (PROGN
                  (PRIN2
                   "Integration only if result free of abs() from now on.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "Integration result may involve abs() from now on.")
                  NIL)))
               NIL))
             ((EQUAL S 'CS)
              (PROGN
               (SETQ CONFIRM_SUBST (NOT CONFIRM_SUBST))
               (COND
                (CONFIRM_SUBST
                 (PROGN
                  (PRIN2 "The user will confirm substitutions from now on.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "No user confirmation of substitutions from now on.")
                  NIL)))
               NIL))
             ((EQUAL S 'FS)
              (PROGN
               (SETQ FORCE_SEP (NOT FORCE_SEP))
               (COND
                (FORCE_SEP
                 (PROGN
                  (PRIN2 "Separation will be inforced from now on.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "Separation will not be inforced from now on.")
                  NIL)))
               NIL))
             ((EQUAL S 'LL)
              (PROGN
               (PROGN (PRIN2 "What is the new line length? ") NIL)
               (CHANGE_PROMPT_TO "What is the new line length? ")
               (PROG ()
                REPEATLABEL
                 (SETQ L (TERMREAD))
                 (COND ((NOT (FIXP L)) (GO REPEATLABEL))))
               (LINELENGTH L)))
             ((EQUAL S 'RE)
              (PROGN
               (SETQ DO_RECYCLE_EQN (NOT DO_RECYCLE_EQN))
               (COND
                (DO_RECYCLE_EQN
                 (PROGN
                  (PRIN2
                   "Equation names will be re-used once the equation is dropped.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2
                   "Equation names will not be re-used once the equation is dropped.")
                  NIL)))))
             ((EQUAL S 'RF)
              (PROGN
               (SETQ DO_RECYCLE_FNC (NOT DO_RECYCLE_FNC))
               (COND
                (DO_RECYCLE_FNC
                 (PROGN
                  (PRIN2 "Function names will be re-used once the function")
                  (PRIN2 " is substituted.")
                  NIL))
                (T
                 (PROGN
                  (PRIN2
                   "Function names will not be re-used once the function")
                  (PRIN2 " is substituted.")
                  NIL)))))
             ((EQUAL S 'ST)
              (PROGN
               (SETQ BATCHCOUNT_ (SUB1 STEPCOUNTER_))
               (COND
                (TIME_LIMIT
                 (PROGN
                  (SETQ L (DIFFERENCE LIMIT_TIME (TIME)))
                  (COND
                   ((LESSP L 0)
                    (PROGN (PRIN2 "The time-limit has expired.") NIL))
                   (T
                    (PROGN
                     (SETQ L (AEVAL* (LIST 'ROUND (LIST 'QUOTIENT L 60000))))
                     (PROGN
                      (PRIN2 "The current CPU time limit for automatic ")
                      (PRIN2 "execution to stop is: ")
                      NIL)
                     (SETQ S (AEVAL* (LIST 'FLOOR (LIST 'QUOTIENT L 60))))
                     (COND
                      ((GREATERP S 0)
                       (PROGN
                        (TERPRI)
                        (PROGN (PRIN2 S) (PRIN2 " hours and ") NIL))))
                     (PROGN
                      (PRIN2 (AEVAL* (LIST 'DIFFERENCE L (LIST 'TIMES 60 S))))
                      (PRIN2 " minutes. ")
                      NIL)
                     NIL)))))
                (T
                 (PROGN (PRIN2 "There is no time-limit set currently.") NIL)))
               (TERPRI)
               (CHANGE_PROMPT_TO "")
               (COND
                ((YESP "Do you want to impose a CPU time-limit? ")
                 (PROGN
                  (PROG ()
                   REPEATLABEL
                    (PROGN
                     (PROGN (PRIN2 "After time has expired,") NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 "   shall CRACK go into interactive mode (1)")
                      NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 "   or shall CRACK terminate with error  (2) ? ")
                      NIL)
                     (TERPRI)
                     (SETQ TIME_LIMIT (TERMREAD)))
                    (COND
                     ((NOT (OR (EQUAL TIME_LIMIT 1) (EQUAL TIME_LIMIT 2)))
                      (GO REPEATLABEL))))
                  (PROGN (PRIN2 "How many hours? ") NIL)
                  (SETQ S (TERMREAD))
                  (PROGN (PRIN2 "How many minutes? ") NIL)
                  (SETQ L (TERMREAD))
                  (COND ((NOT (NUMBERP S)) (SETQ S 0)))
                  (COND ((NOT (NUMBERP L)) (SETQ L 0)))
                  (SETQ LIMIT_TIME
                          (REVAL1
                           (AEVAL*
                            (LIST 'ROUND
                                  (LIST 'PLUS (LIST 'TIMES S 3600000)
                                        (LIST 'TIMES L 60000) (TIME))))
                           T))
                  NIL))
                (T (SETQ TIME_LIMIT NIL)))
               NIL))
             ((EQUAL S 'CM)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "Please type your comment in \" \" for the history_ list: ")
                NIL)
               (TERPRI)
               (SETQ L (TERMREAD))
               (TERPRI)
               NIL))
             ((EQUAL S 'IG) NIL)
             ((EQUAL S 'LR)
              (PROGN
               (SETQ PDES (ADD_A_RULE PDES FORG))
               (SETQ FORG (CADR PDES))
               (SETQ PDES (CAR PDES))))
             ((EQUAL S 'CR) (SETQ PDES (CLEAR_A_RULE PDES)))
             ((EQUAL S 'MO) (START_STOP_MODULO))
             ((EQUAL S 'AP)
              (COND
               (ALG_POLY
                (PROGN
                 (CHANGE_PROMPT_TO "")
                 (COND
                  (FHOM_
                   (PROGN
                    (PROGN
                     (PRIN2
                      "Is the system homogeneous in the following functions/unknowns? (y/n) ")
                     NIL)
                    (LISTPRINT FHOM_)
                    (TERPRI)
                    (SETQ L (TERMREAD))
                    (COND ((EQUAL L 'N) (SETQ FHOM_ NIL))))))
                 (COND
                  ((NULL FHOM_)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "Is the system homogeneous in some functions/unknowns? (y/n) ")
                     NIL)
                    (SETQ L (TERMREAD))
                    (COND
                     ((EQUAL L 'Y)
                      (PROGN
                       (PROGN
                        (PRIN2
                         "Give a list of functions/unknowns (terminated by ; )")
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2
                         "such that all equations in which anyone of these")
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2
                         "functions occurs is homomogeneous wrt. all of them.")
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2
                         "The following is a list of all current unknowns.")
                        NIL)
                       (TERPRI)
                       (LISTPRINT FTEM_)
                       (TERPRI)
                       (PROGN (PRIN2 "To select all, enter only  ;  : ") NIL)
                       (SETQ FHOM_ (TERMLISTREAD))
                       (COND ((NULL FHOM_) (SETQ FHOM_ FTEM_)))
                       (PROG (S)
                         (SETQ S PDES)
                        LAB
                         (COND ((NULL S) (RETURN NIL)))
                         ((LAMBDA (S)
                            (PROGN
                             (PUT S 'FCT_HOM (SMEMBERL (GET S 'FCTS) FHOM_))
                             (PUT S 'HOM_DEG
                                  (FIND_HOM_DEG_SF (CAR (GET S 'SQVAL))))))
                          (CAR S))
                         (SETQ S (CDR S))
                         (GO LAB))))
                     (T (SETQ FHOM_ NIL))))))
                 (PROGN
                  (PRIN2 "Should solutions be stored as files? (y/n) ")
                  NIL)
                 (SETQ L (TERMREAD))
                 (COND
                  ((EQUAL L 'Y)
                   (PROGN
                    (COND (COLLECT_SOL (SAVE_SOL_LIST)))
                    (SETQ COLLECT_SOL NIL)))
                  (T (SETQ COLLECT_SOL T)))
                 (PROGN
                  (PRIN2 "Should FORM be used for long computations? (y/n) ")
                  NIL)
                 (SETQ L (TERMREAD))
                 (COND ((EQUAL L 'Y) (SETQ FORM_COMP T))
                       (T (SETQ FORM_COMP NIL)))
                 (SETQ GROEB_SOLVE
                         (PROGN
                          (PROGN
                           (PRIN2
                            "Should Singular be used for computing Groebner bases? (y/n) ")
                           NIL)
                          (SETQ L (TERMREAD))
                          (COND
                           ((EQUAL L 'Y)
                            (PROGN
                             (PROGN
                              (PRIN2
                               "Use reverse total degree ordering? (y/n) ")
                              NIL)
                             (SETQ L (TERMREAD))
                             (COND ((EQUAL L 'Y) 'SL_REVGRAD)
                                   (T
                                    (PROGN
                                     (PROGN
                                      (PRIN2
                                       "Pure lexicographical ordering is used.")
                                      NIL)
                                     (TERPRI)
                                     'SL_LEX)))))
                           (T
                            (PROGN
                             (PROGN
                              (PRIN2
                               "Should the GB package of J.C.Faugere be used? (y/n) ")
                              NIL)
                             (SETQ L (TERMREAD))
                             (COND
                              ((EQUAL L 'Y)
                               (PROGN
                                (PROGN
                                 (PRIN2
                                  "Use reverse total degree ordering? (y/n) ")
                                 NIL)
                                (SETQ L (TERMREAD))
                                (COND ((EQUAL L 'Y) 'GB_REVGRAD)
                                      (T
                                       (PROGN
                                        (PROGN
                                         (PRIN2
                                          "Pure lexicographical ordering is used.")
                                         NIL)
                                        (TERPRI)
                                        'GB_LEX)))))
                              (T
                               (PROGN
                                (PROGN
                                 (PRIN2 "The REDUCE Groebner package is used.")
                                 NIL)
                                (TERPRI)
                                'REDUCE))))))))
                 (COND
                  ((NULL SIZE_WATCH)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "Will the computation involve 1000's of steps, so that the recording")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "    of the history of the computation should be limited (y/n)? ")
                     NIL)
                    (SETQ L (TERMREAD))
                    (COND ((EQUAL L 'N) (SETQ SIZE_WATCH T))
                          (T
                           (PROGN
                            (PROG ()
                             REPEATLABEL
                              (PROGN
                               (PROGN
                                (PRIN2
                                 "About how many last steps shall statistical data be stored? (at least 50) ")
                                NIL)
                               (SETQ SIZE_WATCH (TERMREAD))
                               NIL)
                              (COND
                               ((NOT (FIXP SIZE_WATCH))
                                (GO REPEATLABEL))))))))))
                 (SETQ PRINT_MORE NIL)
                 (SETQ RECORD_HIST NIL)
                 (SETQ MAX_GC_ELIMIN 5)
                 (SETQ MAX_GC_RED_LEN 1)
                 (COND
                  ((NULL SIZE_WATCH)
                   (SETQ SIZE_HIST
                           (LIST
                            (CONS 'CP
                                  (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ L PROC_LIST_)
                                    (COND ((NULL L) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (L) (GET L 'NO))
                                                      (CAR L))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ L (CDR L))
                                    (COND ((NULL L) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))))))
                 (SETQ MAX_GC_FAC 4)
                 (SETQ CHOOSE_6_20_MAX_FTEM 25)
                 (SETQ CHOOSE_6_20_MAX_TERMS 5000)
                 (SETQ CHOOSE_27_8_16_MAX 15)
                 (SETQ CHOOSE_30_47_21_MAX 10)
                 (SETQ PROC_LIST_
                         '(TO_DO SEPARATION SUBST_LEVEL_0 ALG_LENGTH_REDUCTION
                           CHOOSE_6_20 SUBST_LEVEL_45 CHOOSE_27_8_16
                           DIFF_LENGTH_REDUCTION FACTORIZE_TO_SUBSTITUTE
                           SUBST_LEVEL_3 CHOOSE_30_47_21 DECOUPLING
                           FACTORIZE_ANY SUBST_LEVEL_4 STOP_BATCH))
                 (PROGN (PRIN2 "proc_list_ has been changed, see p1.") NIL)
                 (TERPRI)
                 NIL))
               (T NIL)))
             ((EQUAL S 'HO) (FIND_HOMO_WEIGHTS PDES))
             ((EQUAL S 'R)
              (PROGN
               (SETQ PDES (REPLACEPDE PDES FTEM_ VL_))
               (SETQ FTEM_ (CADR PDES))
               (SETQ PDES (CAR PDES))))
             ((EQUAL S 'RD)
              (PROGN
               (SETQ PDES (REDUCEPDE PDES FTEM_ VL_))
               (SETQ FTEM_ (CADR PDES))
               (SETQ PDES (CAR PDES))))
             ((EQUAL S 'N) (NEWINEQU PDES))
             ((EQUAL S 'DE) (SETQ PDES (DELETEPDE PDES)))
             ((EQUAL S 'DI) (DELETE_INEQ PDES))
             ((EQUAL S 'C) (CHANGE_PDE_FLAG PDES))
             ((EQUAL S 'PT)
              (PROGN
               (SETQ L (GENERAL_TRAFO (LIST PDES FORG)))
               (COND (L (PROGN (SETQ PDES (CAR L)) (SETQ FORG (CADR L)))))))
             ((AND (EQUAL S 'I) (GETD 'SHOW_ID)) (SHOW_ID))
             ((AND (EQUAL S 'ID) (GETD 'SHOW_ID))
              (COND ((SETQ L (DEL_RED_ID PDES)) (SETQ PDES L)) (T NIL)))
             ((AND (EQUAL S 'IW) (GETD 'SHOW_ID)) (WRITE_ID_TO_FILE PDES))
             ((AND (EQUAL S 'IR) (GETD 'SHOW_ID)) (REMOVE_IDL))
             ((AND (EQUAL S 'IA) (GETD 'SHOW_ID)) (REPLACE_IDTY))
             ((AND (EQUAL S 'IH) (GETD 'SHOW_ID)) (START_HISTORY PDES))
             ((AND (EQUAL S 'IS) (GETD 'SHOW_ID)) (STOP_HISTORY PDES))
             ((AND (EQUAL S 'II) (GETD 'SHOW_ID))
              (COND
               ((SETQ L (INTEGRATE_IDTY NIL PDES FTEM_ VL_)) (SETQ PDES L))
               (T (PROGN (PROGN (PRIN2 " no success") NIL) (TERPRI)))))
             ((EQUAL S 'IC) (CHECK_HISTORY PDES))
             ((EQUAL S 'IY)
              (PROG (L)
                (SETQ L PDES)
               LAB
                (COND ((NULL L) (RETURN NIL)))
                ((LAMBDA (L) (MATHPRINT (LIST 'EQUAL L (GET L 'HISTRY_))))
                 (CAR L))
                (SETQ L (CDR L))
                (GO LAB)))
             ((EQUAL S 'TM)
              (PROGN
               (SETQ TR_MAIN (NOT TR_MAIN))
               (COND (TR_MAIN (PROGN (PRIN2 "tr_main is now on.") NIL))
                     (T (PROGN (PRIN2 "tr_main is now off.") NIL)))))
             ((EQUAL S 'TG)
              (PROGN
               (SETQ TR_GENSEP (NOT TR_GENSEP))
               (COND (TR_GENSEP (PROGN (PRIN2 "tr_gensep is now on.") NIL))
                     (T (PROGN (PRIN2 "tr_gensep is now off.") NIL)))))
             ((EQUAL S 'TI)
              (PROGN
               (SETQ TR_GENINT (NOT TR_GENINT))
               (COND (TR_GENINT (PROGN (PRIN2 "tr_genint is now on.") NIL))
                     (T (PROGN (PRIN2 "tr_genint is now off.") NIL)))))
             ((EQUAL S 'TD)
              (PROGN
               (SETQ TR_DECOUPLE (NOT TR_DECOUPLE))
               (COND (TR_DECOUPLE (PROGN (PRIN2 "tr_decouple is now on.") NIL))
                     (T (PROGN (PRIN2 "tr_decouple is now off.") NIL)))))
             ((EQUAL S 'TL)
              (PROGN
               (SETQ TR_REDLENGTH (NOT TR_REDLENGTH))
               (COND
                (TR_REDLENGTH (PROGN (PRIN2 "tr_redlength is now on.") NIL))
                (T (PROGN (PRIN2 "tr_redlength is now off.") NIL)))))
             ((EQUAL S 'TS)
              (PROGN
               (SETQ TR_SHORT (NOT TR_SHORT))
               (COND (TR_SHORT (PROGN (PRIN2 "tr_short is now on.") NIL))
                     (T (PROGN (PRIN2 "tr_short is now off.") NIL)))))
             ((EQUAL S 'TO)
              (PROGN
               (SETQ TR_ORDERINGS (NOT TR_ORDERINGS))
               (COND
                (TR_ORDERINGS (PROGN (PRIN2 "tr_orderings is now on.") NIL))
                (T (PROGN (PRIN2 "tr_orderings is now off.") NIL)))))
             ((EQUAL S 'TR)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2 "Please type the name of the procedure to trace: ")
                NIL)
               (SETQ L (TERMREAD))
               (TERPRI)
               (EVAL (LIST 'TR L))))
             ((EQUAL S 'UT)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2 "Please type the name of the procedure to trace: ")
                NIL)
               (SETQ L (TERMREAD))
               (TERPRI)
               (EVAL (LIST 'UNTR L))))
             ((EQUAL S 'BR)
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "This is Standard Lisp. Return to Reduce by Ctrl D.")
                NIL)
               (TERPRI)
               (BREAK)))
             ((EQUAL S 'PC)
              (PROGN
               (CHANGE_PROMPT_TO "The function name: ")
               (SETQ S (TERMREAD))
               (CHANGE_PROMPT_TO
                "The argument list in the form {arg1,...};  : ")
               (SETQ L (TERMXREAD))
               (COND
                ((AND (PAIRP L) (EQUAL (CAR L) 'LIST) (IDP S))
                 (PRIN2T (LIST "Result: " (APPLY S (CDR L))))))))
             ((EQUAL S 'IN)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2 "Please give the name of the file to be read in")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "double quotes (no ;) : ") NIL)
               (SETQ L (TERMREAD))
               (TERPRI)
               (IN (LIST L))))
             ((EQUAL S 'CU) (INTERNTEST PDES FORG))
             ((EQUAL S 'QT)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (TERPRI)
               (PROGN
                (PRIN2
                 "Please type in a list of procedure names, like:  gcdf, .., reval;")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "which should be profiled: ") NIL)
               (TERPRI)
               (SETQ L (TERMLISTREAD))
               NIL))
             ((EQUAL S 'PQ)
              (COND ((GETD 'PRINT-QUALTIME) (PRINT-QUALTIME))
                    (T
                     (PROGN
                      (PRIN2 "*** Load package qualified-timing first! ***")
                      NIL))))
             ((EQUAL S 'SO)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "Please give the name of the switch to be switched ON: ")
                NIL)
               (SETQ L (TERMREAD))
               (TERPRI)
               (SETQ S
                       (EVAL
                        (INTERN (COMPRESS (APPEND (EXPLODE '*) (EXPLODE L))))))
               (COND
                ((NULL S)
                 (PROGN
                  (SETQ SWITCH_LIST (CONS (LIST LEVEL_ L S) SWITCH_LIST))
                  (ON1 L))))))
             ((EQUAL S 'SF)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "Please give the name of the switch to be switched OFF: ")
                NIL)
               (SETQ L (TERMREAD))
               (TERPRI)
               (SETQ S
                       (EVAL
                        (INTERN (COMPRESS (APPEND (EXPLODE '*) (EXPLODE L))))))
               (COND
                (S
                 (PROGN
                  (SETQ SWITCH_LIST (CONS (LIST LEVEL_ L S) SWITCH_LIST))
                  (OFF1 L))))))
             ((EQUAL S 'LS)
              (PROG (X)
                (SETQ X (OBLIST))
               LAB
                (COND ((NULL X) (RETURN NIL)))
                ((LAMBDA (X) (SWITCHP X)) (CAR X))
                (SETQ X (CDR X))
                (GO LAB)))
             ((EQUAL S 'LG) (LIST_GLOBAL_CRACK_VARIABLES))
             ((EQUAL S 'DC) (DESCRIBE_ID))
             ((OR (EQUAL S 'XP) (EQUAL S 'SP) (EQUAL S 'JP) (EQUAL S 'PP))
              (COND
               ((INI_CHECK_OF_PARALLEL_CRACK)
                (PROGN
                 (SETQ NO_OF_CHILDREN (ADD1 NO_OF_CHILDREN))
                 (SETQ LEVEL_
                         (CONS (BLDMSG_INTERNAL "c%d" (LIST NO_OF_CHILDREN))
                               LEVEL_))
                 (SETQ L *ICONIC)
                 (SETQ *ICONIC NIL)
                 (COND
                  ((EQUAL S 'XP)
                   (PROGN
                    (PROGN (PRIN2 "Duplicating process under new xterm.") NIL)
                    (TERPRI)
                    (ADD_SESSION PDES FORG 1)))
                  ((EQUAL S 'SP)
                   (PROGN
                    (PROGN (PRIN2 "Duplicating process under new screen.") NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "Hint: For this to work, this computation must run on the")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "same computer/node on which this screen has started.")
                     NIL)
                    (TERPRI)
                    (ADD_SESSION PDES FORG 2)))
                  ((EQUAL S 'JP)
                   (PROGN
                    (PROGN (PRIN2 "Duplicating process as batch job.") NIL)
                    (TERPRI)
                    (ADD_SESSION PDES FORG 3)))
                  (T
                   (PROGN
                    (PROGN
                     (PRIN2 "PVM is currently only available in PSL Reduce,")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2 "not in the Lisp system used in this session.")
                     NIL)
                    (TERPRI)
                    NIL)))
                 (SETQ *ICONIC L)
                 (SETQ LEVEL_ (CDR LEVEL_))))
               (T NIL)))
             ((OR (EQUAL S 'WP) (EQUAL S 'YP) (EQUAL S 'ZP) (EQUAL S 'VP))
              (PROGN
               (COND
                (COLLECT_SOL
                 (PROGN
                  (PROGN
                   (PRIN2
                    "### Currently is collect_sol=t. Therefore parallel case")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "### solving is not enabled because solutions would not")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "### be collected. You could set collect_sol to nil using")
                   NIL)
                  (TERPRI)
                  (CHANGE_PROMPT_TO "")
                  (PROGN
                   (PRIN2
                    "Shall solutions from now on be stored exclusively in files (Y/N) ? ")
                   NIL)
                  (PROG ()
                   REPEATLABEL
                    (SETQ L (TERMREAD))
                    (COND
                     ((NOT (OR (EQUAL L 'Y) (EQUAL L 'N))) (GO REPEATLABEL))))
                  (COND ((EQUAL L 'Y) (SETQ COLLECT_SOL NIL))
                        (T (SETQ COLLECT_SOL T))))))
               (COND
                ((AND (NULL COLLECT_SOL) (INI_CHECK_OF_PARALLEL_CRACK))
                 (PROGN
                  (CRACK_LOAD_CMD)
                  (SETQ L (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
                  (COND ((NOT (FILEP L)) (SAVE_SOL_LIST)))
                  (COND
                   ((EQUAL S 'WP)
                    (PROGN
                     (SETQ AUTO_PARA_MODE 1)
                     (PROGN
                      (PRIN2
                       "From now on parallel case solving with extra xterm's.")
                      NIL)
                     (TERPRI)
                     (CHANGE_PROMPT_TO "")
                     (PROGN (PRIN2 "Shall xterms start as icons (Y/N) ? ") NIL)
                     (PROG ()
                      REPEATLABEL
                       (SETQ L (TERMREAD))
                       (COND
                        ((NOT (OR (EQUAL L 'Y) (EQUAL L 'N)))
                         (GO REPEATLABEL))))
                     (COND ((EQUAL L 'Y) (SETQ *ICONIC T))
                           (T (SETQ *ICONIC NIL)))
                     NIL))
                   ((EQUAL S 'YP)
                    (PROGN
                     (SETQ AUTO_PARA_MODE 2)
                     (PROGN
                      (PRIN2
                       "From now on parallel case solving with extra screens.")
                      NIL)
                     (TERPRI)))
                   ((EQUAL S 'ZP)
                    (PROGN
                     (SETQ AUTO_PARA_MODE 3)
                     (PROGN
                      (PRIN2
                       "From now on parallel case solving by submitting jobs.")
                      NIL)
                     (TERPRI)))
                   (T
                    (PROGN
                     (PVM_ACTIVATE)
                     (TERPRI)
                     (COND
                      (PVM_ABLE
                       (PROGN
                        (SETQ AUTO_PARA_MODE 4)
                        (PROGN
                         (PRIN2 "From now on parallel case solving under PVM.")
                         NIL)
                        (TERPRI)))
                      (T
                       (PROGN
                        (PRIN2 "PVM is not active on this computer.")
                        NIL)))))))))))
             ((EQUAL S 'NP)
              (PROGN
               (PROGN
                (PRIN2
                 "The counter of additional parallel REDUCE processes which is stored")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "in the file ")
                (PRIN2 PROCESS_COUNTER)
                (PRIN2 " is set to zero.")
                NIL)
               (TERPRI)
               (SYSTEM (BLDMSG_INTERNAL "touch %w" (LIST PROCESS_COUNTER)))
               (PROCZAEHLER PROCESS_COUNTER 'INIT)
               NIL))
             ((EQUAL S 'MP)
              (PROGN
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "The new maximal number of parallel processes (currently ")
                (PRIN2 MAX_PROC_NO)
                (PRIN2 "): ")
                NIL)
               (SETQ MAX_PROC_NO (TERMREAD))))
             ((EQUAL S 'TP)
              (PROGN
               (CHANGE_PROMPT_TO "The directory name for storing case files: ")
               (SETQ PARA_CASE_DIR (TERMREAD))))
             ((EQUAL S 'DP)
              (PROGN (SETQ PVM_ABLE NIL) (SETQ AUTO_PARA_MODE NIL)))
             ((EQUAL S 'FO)
              (PROGN
               (SETQ FORM_COMP T)
               (CHANGE_PROMPT_TO "")
               (PROGN
                (PRIN2
                 "Do you want to interface with FORM through pipes (Y/N)? ")
                NIL)
               (PROG ()
                REPEATLABEL
                 (SETQ S (TERMREAD))
                 (COND
                  ((NOT (OR (EQUAL S 'Y) (EQUAL S 'N))) (GO REPEATLABEL))))
               (COND ((EQUAL S 'Y) (SETQ FORM_PIPE T))
                     (T (SETQ FORM_PIPE NIL)))
               (PROGN
                (PRIN2 "Shall the temporary FORM directory be \"")
                (PRIN2 FORM_TMP_DIR)
                (PRIN2 "\"  (Y/N)? ")
                NIL)
               (PROG ()
                REPEATLABEL
                 (SETQ S (TERMREAD))
                 (COND
                  ((NOT (OR (EQUAL S 'Y) (EQUAL S 'N))) (GO REPEATLABEL))))
               (COND
                ((EQUAL S 'N)
                 (PROGN
                  (PROGN
                   (PRIN2
                    "Please input the directory for temporary FORM computations in \"...\" : ")
                   NIL)
                  (SETQ FORM_TMP_DIR (TERMREAD))
                  (PROGN
                   (PRIN2 "The temporary FORM directory is now ")
                   (PRIN2 FORM_TMP_DIR)
                   NIL)
                  NIL)))
               (PROGN
                (PRIN2 "What is the maximal number of terms of an equations")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "computed by FORM that shall be read into REDUCE?")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "(Its current value is ")
                (PRIN2 FORM_MAX_READ)
                (PRIN2 ".)  : ")
                NIL)
               (SETQ FORM_MAX_READ (TERMREAD))
               NIL))
             ((EQUAL S 'FF) (SETQ FORM_COMP NIL))
             ((EQUAL S 'GS)
              (SETQ GROEB_SOLVE
                      (PROGN
                       (CHANGE_PROMPT_TO "")
                       (FIND_SINGULAR)
                       (COND
                        ((AND SINGULAR_CALL SINGULAR_LIB)
                         (PROGN
                          (PROG ()
                           REPEATLABEL
                            (PROGN
                             (PROGN
                              (PRIN2
                               "What is the max time allowed in seconds? ")
                              NIL)
                             (SETQ L (TERMREAD))
                             NIL)
                            (COND ((NOT (FIXP L)) (GO REPEATLABEL))))
                          (SETQ SINGULAR_TIME L)
                          (PROGN
                           (PRIN2 "Use reverse total degree ordering? (y/n) ")
                           NIL)
                          (SETQ L (TERMREAD))
                          (COND ((EQUAL L 'Y) 'SL_REVGRAD)
                                (T
                                 (PROGN
                                  (PROGN
                                   (PRIN2
                                    "Pure lexicographical ordering is used.")
                                   NIL)
                                  (TERPRI)
                                  'SL_LEX)))))))))
             ((EQUAL S 'GG)
              (PROGN
               (CHANGE_PROMPT_TO "Use reverse total degree ordering? (y/n) ")
               (SETQ L (TERMREAD))
               (COND ((EQUAL L 'Y) 'GB_REVGRAD)
                     (T
                      (PROGN
                       (PROGN
                        (PRIN2 "Pure lexicographical ordering is used.")
                        NIL)
                       (TERPRI)
                       'GB_LEX)))))
             ((EQUAL S 'GR) (SETQ GROEB_SOLVE 'REDUCE))
             ((EQUAL S 'DF) (SETQ GROEB_SOLVE 'DIFFELIM))
             (T
              (PROGN
               (COND
                ((AND SIZE_WATCH (NOT (FIXP SIZE_WATCH)))
                 (SETQ HISTORY_
                         (CONS
                          (BLDMSG_INTERNAL "*** %w previous input is illegal"
                                           (LIST PS))
                          (CONS 'IG HISTORY_)))))
               (PROGN (PRIN2 "illegal input: '") (PRIN2 S) (PRIN2 "'") NIL)
               (TERPRI))))
            (RESTORE_INTERACTIVE_PROMPT)
            (COND (IFL* (RDS (CADR IFL*))))
            (COND (OFL* (WRS (CDR OFL*))))
            NIL)))
         (COND
          ((AND (NOT PDES) FNC_TO_ADJUST)
           (COND
            (FNC_ADJUSTED (PROGN (SETQ ADJUST_FNC T) (SETQ FNC_TO_ADJUST NIL)))
            ((OR CONTRADICTION_ RESULT) (SETQ FNC_TO_ADJUST NIL))
            (T
             (PROGN
              (SETQ TO_DO_LIST
                      (CONS (LIST 'DEL_REDUNDANT_FC (LIST NIL)) TO_DO_LIST))
              (SETQ ADJUST_FNC NIL)
              (SETQ FNC_ADJUSTED T)))))))
        (COND
         ((NOT
           (OR CONTRADICTION_ RESULT STOP_ UNSOLVABLE
               (AND (NOT PDES) (NOT FNC_TO_ADJUST) (NOT EQN_TO_BE_GEN)
                    (OR (EQUAL EQUATIONS_FILE "") (EQUAL EQN_INPUT 'DONE)))))
          (GO REPEATLABEL))))
      (COND
       ((NOT (OR CONTRADICTION_ RESULT))
        (PROGN
         (COND
          ((AND PRINT_ (NOT STOP_))
           (PROGN
            (TERPRI)
            (TERPRI)
            (PROGN (PRIN2 ">>>>>>>>> Solution") NIL)
            (COND
             (LEVEL_
              (PROGN (PRIN2 " of level ") (PRIN2 (LEVEL_STRING NIL)) NIL)))
            (PROGN (PRIN2 " : ") NIL)
            NIL)))
         (SETQ FORG (SUB_FSUB_IN_ITSELF_AND_IN_FORG FORG))
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR USERRULES_)) (RETURN NIL)))
           (SETQ PDES (MOVERULE2EQN (CADR USERRULES_) PDES))
           (GO WHILELABEL))
         (F_UPDATE PDES FORG)
         (SETQ FORG (FORG_INT FORG FTEM_))
         (COND
          ((AND (NULL COLLECT_SOL)
                (OR STOP_ (NULL (GETD 'CRACK_OUT)) (NULL CALL_CRACK_OUT)))
           (PRINT_PDE_FORG_INEQ PDES (CONS INEQ_ INEQ_OR)
            (APPEND FORG (SETDIFF FTEM_ FORG)) VL_)))
         (COND
          ((NOT STOP_)
           (PROGN
            (SETQ L (COND ((NULL SOL_LIST) 1) (T (ADD1 (LENGTH SOL_LIST)))))
            (COND
             ((AND (NOT (NULL (GETD 'CRACK_OUT))) CALL_CRACK_OUT)
              (SETQ S
                      (AEVAL
                       (LIST 'CRACK_OUT
                             (CONS 'LIST
                                   (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ A PDES)
                                     (COND ((NULL A) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (A)
                                                         (LIST '*SQ
                                                               (GET A 'SQVAL)
                                                               T))
                                                       (CAR A))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ A (CDR A))
                                     (COND ((NULL A) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (A)
                                                 (LIST '*SQ (GET A 'SQVAL) T))
                                               (CAR A))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                             (CONS 'LIST
                                   (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ A (SETDIFF FORG FTEM_))
                                     (COND ((NULL A) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (A)
                                                         (LIST 'EQUAL (CADR A)
                                                               (REVAL1
                                                                (LIST '*SQ
                                                                      (CADDR A)
                                                                      NIL)
                                                                NIL)))
                                                       (CAR A))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ A (CDR A))
                                     (COND ((NULL A) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (A)
                                                 (LIST 'EQUAL (CADR A)
                                                       (REVAL1
                                                        (LIST '*SQ (CADDR A)
                                                              NIL)
                                                        NIL)))
                                               (CAR A))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                             (CONS 'LIST FTEM_)
                             (CONS 'LIST
                                   (APPEND
                                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ A INEQ_)
                                      (COND ((NULL A) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (A)
                                                          (REVAL1
                                                           (LIST '*SQ A NIL)
                                                           NIL))
                                                        (CAR A))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ A (CDR A))
                                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (A)
                                                  (REVAL1 (LIST '*SQ A NIL)
                                                          NIL))
                                                (CAR A))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))
                                    (COND ((NULL INEQ_OR) NIL)
                                          (T
                                           (CONS 'LIST
                                                 (PROG (A FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ A INEQ_OR)
                                                   (COND
                                                    ((NULL A) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (A)
                                                                       (CONS
                                                                        'LIST
                                                                        (PROG (B
                                                                               FORALL-RESULT
                                                                               FORALL-ENDPTR)
                                                                          (SETQ B
                                                                                  A)
                                                                          (COND
                                                                           ((NULL
                                                                             B)
                                                                            (RETURN
                                                                             NIL)))
                                                                          (SETQ FORALL-RESULT
                                                                                  (SETQ FORALL-ENDPTR
                                                                                          (CONS
                                                                                           ((LAMBDA
                                                                                                (
                                                                                                 B)
                                                                                              (CONS
                                                                                               'LIST
                                                                                               (PROG (C
                                                                                                      FORALL-RESULT
                                                                                                      FORALL-ENDPTR)
                                                                                                 (SETQ C
                                                                                                         B)
                                                                                                 (COND
                                                                                                  ((NULL
                                                                                                    C)
                                                                                                   (RETURN
                                                                                                    NIL)))
                                                                                                 (SETQ FORALL-RESULT
                                                                                                         (SETQ FORALL-ENDPTR
                                                                                                                 (CONS
                                                                                                                  ((LAMBDA
                                                                                                                       (
                                                                                                                        C)
                                                                                                                     (REVAL1
                                                                                                                      (LIST
                                                                                                                       '*SQ
                                                                                                                       C
                                                                                                                       NIL)
                                                                                                                      NIL))
                                                                                                                   (CAR
                                                                                                                    C))
                                                                                                                  NIL)))
                                                                                                LOOPLABEL
                                                                                                 (SETQ C
                                                                                                         (CDR
                                                                                                          C))
                                                                                                 (COND
                                                                                                  ((NULL
                                                                                                    C)
                                                                                                   (RETURN
                                                                                                    FORALL-RESULT)))
                                                                                                 (RPLACD
                                                                                                  FORALL-ENDPTR
                                                                                                  (CONS
                                                                                                   ((LAMBDA
                                                                                                        (
                                                                                                         C)
                                                                                                      (REVAL1
                                                                                                       (LIST
                                                                                                        '*SQ
                                                                                                        C
                                                                                                        NIL)
                                                                                                       NIL))
                                                                                                    (CAR
                                                                                                     C))
                                                                                                   NIL))
                                                                                                 (SETQ FORALL-ENDPTR
                                                                                                         (CDR
                                                                                                          FORALL-ENDPTR))
                                                                                                 (GO
                                                                                                  LOOPLABEL))))
                                                                                            (CAR
                                                                                             B))
                                                                                           NIL)))
                                                                         LOOPLABEL
                                                                          (SETQ B
                                                                                  (CDR
                                                                                   B))
                                                                          (COND
                                                                           ((NULL
                                                                             B)
                                                                            (RETURN
                                                                             FORALL-RESULT)))
                                                                          (RPLACD
                                                                           FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  B)
                                                                               (CONS
                                                                                'LIST
                                                                                (PROG (C
                                                                                       FORALL-RESULT
                                                                                       FORALL-ENDPTR)
                                                                                  (SETQ C
                                                                                          B)
                                                                                  (COND
                                                                                   ((NULL
                                                                                     C)
                                                                                    (RETURN
                                                                                     NIL)))
                                                                                  (SETQ FORALL-RESULT
                                                                                          (SETQ FORALL-ENDPTR
                                                                                                  (CONS
                                                                                                   ((LAMBDA
                                                                                                        (
                                                                                                         C)
                                                                                                      (REVAL1
                                                                                                       (LIST
                                                                                                        '*SQ
                                                                                                        C
                                                                                                        NIL)
                                                                                                       NIL))
                                                                                                    (CAR
                                                                                                     C))
                                                                                                   NIL)))
                                                                                 LOOPLABEL
                                                                                  (SETQ C
                                                                                          (CDR
                                                                                           C))
                                                                                  (COND
                                                                                   ((NULL
                                                                                     C)
                                                                                    (RETURN
                                                                                     FORALL-RESULT)))
                                                                                  (RPLACD
                                                                                   FORALL-ENDPTR
                                                                                   (CONS
                                                                                    ((LAMBDA
                                                                                         (
                                                                                          C)
                                                                                       (REVAL1
                                                                                        (LIST
                                                                                         '*SQ
                                                                                         C
                                                                                         NIL)
                                                                                        NIL))
                                                                                     (CAR
                                                                                      C))
                                                                                    NIL))
                                                                                  (SETQ FORALL-ENDPTR
                                                                                          (CDR
                                                                                           FORALL-ENDPTR))
                                                                                  (GO
                                                                                   LOOPLABEL))))
                                                                             (CAR
                                                                              B))
                                                                            NIL))
                                                                          (SETQ FORALL-ENDPTR
                                                                                  (CDR
                                                                                   FORALL-ENDPTR))
                                                                          (GO
                                                                           LOOPLABEL))))
                                                                     (CAR A))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ A (CDR A))
                                                   (COND
                                                    ((NULL A)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (A)
                                                               (CONS 'LIST
                                                                     (PROG (B
                                                                            FORALL-RESULT
                                                                            FORALL-ENDPTR)
                                                                       (SETQ B
                                                                               A)
                                                                       (COND
                                                                        ((NULL
                                                                          B)
                                                                         (RETURN
                                                                          NIL)))
                                                                       (SETQ FORALL-RESULT
                                                                               (SETQ FORALL-ENDPTR
                                                                                       (CONS
                                                                                        ((LAMBDA
                                                                                             (
                                                                                              B)
                                                                                           (CONS
                                                                                            'LIST
                                                                                            (PROG (C
                                                                                                   FORALL-RESULT
                                                                                                   FORALL-ENDPTR)
                                                                                              (SETQ C
                                                                                                      B)
                                                                                              (COND
                                                                                               ((NULL
                                                                                                 C)
                                                                                                (RETURN
                                                                                                 NIL)))
                                                                                              (SETQ FORALL-RESULT
                                                                                                      (SETQ FORALL-ENDPTR
                                                                                                              (CONS
                                                                                                               ((LAMBDA
                                                                                                                    (
                                                                                                                     C)
                                                                                                                  (REVAL1
                                                                                                                   (LIST
                                                                                                                    '*SQ
                                                                                                                    C
                                                                                                                    NIL)
                                                                                                                   NIL))
                                                                                                                (CAR
                                                                                                                 C))
                                                                                                               NIL)))
                                                                                             LOOPLABEL
                                                                                              (SETQ C
                                                                                                      (CDR
                                                                                                       C))
                                                                                              (COND
                                                                                               ((NULL
                                                                                                 C)
                                                                                                (RETURN
                                                                                                 FORALL-RESULT)))
                                                                                              (RPLACD
                                                                                               FORALL-ENDPTR
                                                                                               (CONS
                                                                                                ((LAMBDA
                                                                                                     (
                                                                                                      C)
                                                                                                   (REVAL1
                                                                                                    (LIST
                                                                                                     '*SQ
                                                                                                     C
                                                                                                     NIL)
                                                                                                    NIL))
                                                                                                 (CAR
                                                                                                  C))
                                                                                                NIL))
                                                                                              (SETQ FORALL-ENDPTR
                                                                                                      (CDR
                                                                                                       FORALL-ENDPTR))
                                                                                              (GO
                                                                                               LOOPLABEL))))
                                                                                         (CAR
                                                                                          B))
                                                                                        NIL)))
                                                                      LOOPLABEL
                                                                       (SETQ B
                                                                               (CDR
                                                                                B))
                                                                       (COND
                                                                        ((NULL
                                                                          B)
                                                                         (RETURN
                                                                          FORALL-RESULT)))
                                                                       (RPLACD
                                                                        FORALL-ENDPTR
                                                                        (CONS
                                                                         ((LAMBDA
                                                                              (
                                                                               B)
                                                                            (CONS
                                                                             'LIST
                                                                             (PROG (C
                                                                                    FORALL-RESULT
                                                                                    FORALL-ENDPTR)
                                                                               (SETQ C
                                                                                       B)
                                                                               (COND
                                                                                ((NULL
                                                                                  C)
                                                                                 (RETURN
                                                                                  NIL)))
                                                                               (SETQ FORALL-RESULT
                                                                                       (SETQ FORALL-ENDPTR
                                                                                               (CONS
                                                                                                ((LAMBDA
                                                                                                     (
                                                                                                      C)
                                                                                                   (REVAL1
                                                                                                    (LIST
                                                                                                     '*SQ
                                                                                                     C
                                                                                                     NIL)
                                                                                                    NIL))
                                                                                                 (CAR
                                                                                                  C))
                                                                                                NIL)))
                                                                              LOOPLABEL
                                                                               (SETQ C
                                                                                       (CDR
                                                                                        C))
                                                                               (COND
                                                                                ((NULL
                                                                                  C)
                                                                                 (RETURN
                                                                                  FORALL-RESULT)))
                                                                               (RPLACD
                                                                                FORALL-ENDPTR
                                                                                (CONS
                                                                                 ((LAMBDA
                                                                                      (
                                                                                       C)
                                                                                    (REVAL1
                                                                                     (LIST
                                                                                      '*SQ
                                                                                      C
                                                                                      NIL)
                                                                                     NIL))
                                                                                  (CAR
                                                                                   C))
                                                                                 NIL))
                                                                               (SETQ FORALL-ENDPTR
                                                                                       (CDR
                                                                                        FORALL-ENDPTR))
                                                                               (GO
                                                                                LOOPLABEL))))
                                                                          (CAR
                                                                           B))
                                                                         NIL))
                                                                       (SETQ FORALL-ENDPTR
                                                                               (CDR
                                                                                FORALL-ENDPTR))
                                                                       (GO
                                                                        LOOPLABEL))))
                                                             (CAR A))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))))))
                             L))))
             (T (SETQ S NIL)))
            (COND
             ((AND S (CDR S) (NULL LIN_PROBLEM))
              (PROGN
               (PROG (L)
                 (SETQ L PDES)
                LAB
                 (COND ((NULL L) (RETURN NIL)))
                 ((LAMBDA (L)
                    (PROGN
                     (SETQ H (SIMPLIFYPDESQ (GET L 'SQVAL) FTEM_ T L T))
                     (PUT L 'SQVAL (CAR H))
                     (PUT L 'FAC (CDR H))))
                  (CAR L))
                 (SETQ L (CDR L))
                 (GO LAB))
               (SETQ PL PDES)
               (PROG (L)
                 (SETQ L (CDR S))
                LAB
                 (COND ((NULL L) (RETURN NIL)))
                 ((LAMBDA (L)
                    (SETQ PDES
                            (EQINSERT
                             (MKEQSQ
                              (PROGN
                               (SETQ H (REVAL1 L NIL))
                               (COND
                                ((AND (PAIRP H) (EQUAL (CAR H) '*SQ)) (CADR H))
                                (T (SIMP H))))
                              NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL PDES)
                             PDES)))
                  (CAR L))
                 (SETQ L (CDR L))
                 (GO LAB))
               (COND
                ((SETDIFF PDES PL)
                 (PROGN
                  (COND
                   (PRINT_
                    (PROGN
                     (PROGN (PRIN2 "Not all conditions are solved.") NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 " --> RESTART with extra conditions ")
                      (PRIN2 (SETDIFF PDES PL))
                      NIL)
                     (TERPRI))))
                  (SETQ UNSOLVABLE NIL)
                  (GO AGAIN))))))))))
         (COND
          ((AND SESSION_ (NULL COLLECT_SOL))
           (PROGN
            (SETQ S NIL)
            (PROG (L)
              (SETQ L FORG)
             LAB
              (COND ((NULL L) (RETURN NIL)))
              ((LAMBDA (L)
                 (COND
                  ((OR (NOT (PAIRP L)) (NEQ (CAR L) 'EQUAL))
                   (SETQ S (CONS L S)))))
               (CAR L))
              (SETQ L (CDR L))
              (GO LAB))
            (SAVE_SOLUTION
             (PROG (A FORALL-RESULT FORALL-ENDPTR)
               (SETQ A PDES)
               (COND ((NULL A) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (A) (PREPSQ (GET A 'SQVAL))) (CAR A))
                                NIL)))
              LOOPLABEL
               (SETQ A (CDR A))
               (COND ((NULL A) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (A) (PREPSQ (GET A 'SQVAL))) (CAR A))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (PROG (A FORALL-RESULT FORALL-ENDPTR)
               (SETQ A (SETDIFF FORG S))
               (COND ((NULL A) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (A)
                                   (LIST 'EQUAL (CADR A) (PREPSQ (CADDR A))))
                                 (CAR A))
                                NIL)))
              LOOPLABEL
               (SETQ A (CDR A))
               (COND ((NULL A) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (A) (LIST 'EQUAL (CADR A) (PREPSQ (CADDR A))))
                         (CAR A))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (UNION S FTEM_)
             (PROG (A FORALL-RESULT FORALL-ENDPTR)
               (SETQ A INEQ_)
               (COND ((NULL A) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (A) (PREPSQ A)) (CAR A)) NIL)))
              LOOPLABEL
               (SETQ A (CDR A))
               (COND ((NULL A) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (A) (PREPSQ A)) (CAR A)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (PROG (A FORALL-RESULT FORALL-ENDPTR)
               (SETQ A INEQ_OR)
               (COND ((NULL A) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (A)
                                   (PROG (L FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ L A)
                                     (COND ((NULL L) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (L)
                                                         (PROG (S FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ S L)
                                                           (COND
                                                            ((NULL S)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  S)
                                                                               (PREPSQ
                                                                                S))
                                                                             (CAR
                                                                              S))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ S (CDR S))
                                                           (COND
                                                            ((NULL S)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (S)
                                                                (PREPSQ S))
                                                              (CAR S))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                       (CAR L))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ L (CDR L))
                                     (COND ((NULL L) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L)
                                                 (PROG (S FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ S L)
                                                   (COND
                                                    ((NULL S) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (S)
                                                                       (PREPSQ
                                                                        S))
                                                                     (CAR S))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ S (CDR S))
                                                   (COND
                                                    ((NULL S)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (S)
                                                               (PREPSQ S))
                                                             (CAR S))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                               (CAR L))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR A))
                                NIL)))
              LOOPLABEL
               (SETQ A (CDR A))
               (COND ((NULL A) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (A)
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L A)
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L)
                                                 (PROG (S FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ S L)
                                                   (COND
                                                    ((NULL S) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (S)
                                                                       (PREPSQ
                                                                        S))
                                                                     (CAR S))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ S (CDR S))
                                                   (COND
                                                    ((NULL S)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (S)
                                                               (PREPSQ S))
                                                             (CAR S))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                               (CAR L))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (L)
                                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ S L)
                                           (COND ((NULL S) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (S)
                                                               (PREPSQ S))
                                                             (CAR S))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ S (CDR S))
                                           (COND
                                            ((NULL S) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (S) (PREPSQ S))
                                                     (CAR S))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (CAR L))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR A))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             NIL)
            (COND
             ((AND PDES (GREATERP (LENGTH PDES) 1))
              (PROGN
               (COND
                (LEVEL_
                 (SETQ S
                         (APPEND (REVERSE (CDR (REVERSE (EXPLODE SESSION_))))
                                 (CDR (EXPLODE (LEVEL_STRING NIL))))))
                (T (SETQ S (EXPLODE SESSION_))))
               (SETQ S (COMPRESS (CONS (CAR S) (CONS 'U (CONS 'S (CDDDR S))))))
               (BACKUP_TO_FILE PDES FORG S)))))))
         (SETQ RESULT
                 (COND ((NOT COLLECT_SOL) (LIST 1))
                       (T
                        (LIST
                         (LIST
                          (PROG (A FORALL-RESULT FORALL-ENDPTR)
                            (SETQ A PDES)
                            (COND ((NULL A) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (A) (GET A 'SQVAL))
                                              (CAR A))
                                             NIL)))
                           LOOPLABEL
                            (SETQ A (CDR A))
                            (COND ((NULL A) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (A) (GET A 'SQVAL)) (CAR A))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          FORG (SETDIFF FTEM_ FORG) INEQ_ INEQ_OR))))))))
      (COND
       ((AND TR_MAIN PRINT_)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "end of the main procedure") NIL)
         (TERPRI))))
      (SETQ L (PLUS (LENGTH LEVEL_) (DIFFERENCE 1 LEVEL_LENGTH)))
      (PROG (S)
        (SETQ S 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L S)) (RETURN NIL)))
        (COND
         (LEVEL_
          (FINISH_LEVEL
           (COND ((NULL RESULT) 0)
                 ((OR COLLECT_SOL (NOT (PAIRP RESULT))
                      (NOT (FIXP (CAR RESULT))))
                  (LENGTH RESULT))
                 ((NULL (CAR RESULT)) 0) (T (CAR RESULT))))))
        (SETQ S (PLUS2 S 1))
        (GO LAB))
      (DROP_ALL_PDES PDES)
      (COND
       ((AND (EQUAL LEVEL_LENGTH 0) (NULL PARACRACK_INITIALIZED))
        (DELETE_CASE_TREE)))
      (PROG (L)
        (SETQ L 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (SUB1 NEQU_) L)) (RETURN NIL)))
        (PROGN
         (SETQ S (MKID EQNAME_ L))
         (COND ((PROP S) (PROGN (SETPROP S NIL)))))
        (SETQ L (PLUS2 L 1))
        (GO LAB))
      (PROG (L)
        (SETQ L FORG)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (COND ((PAIRP L) (SETPROP (CADR L) NIL)) (T (SETPROP L NIL))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (RETURN RESULT))) 
(PUT 'SUB_FSUB_IN_ITSELF_AND_IN_FORG 'NUMBER-OF-ARGS 1) 
(PUT 'SUB_FSUB_IN_ITSELF_AND_IN_FORG 'DEFINED-ON-LINE '1721) 
(PUT 'SUB_FSUB_IN_ITSELF_AND_IN_FORG 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SUB_FSUB_IN_ITSELF_AND_IN_FORG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUB_FSUB_IN_ITSELF_AND_IN_FORG (FORG)
    (COND ((NULL FSUB_) FORG)
          (T
           (PROG (L S)
             (SETQ L FSUB_)
             (PROG ()
              WHILELABEL
               (COND ((NOT (CDR L)) (RETURN NIL)))
               (PROGN
                (SETQ S (CDR L))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT S) (RETURN NIL)))
                  (PROGN
                   (RPLACA S
                           (CONS (CAAR S)
                                 (LIST '*SQ
                                       (SUBSQ (CADR (CDAR S)) (LIST (CAR L)))
                                       NIL)))
                   (SETQ S (CDR S)))
                  (GO WHILELABEL))
                (SETQ L (CDR L)))
               (GO WHILELABEL))
             (PROG ()
              WHILELABEL
               (COND ((NOT FSUB_) (RETURN NIL)))
               (PROGN
                (SETQ FORG (CAR (SUB_IN_FORG (CAAR FSUB_) (CDAR FSUB_) FORG)))
                (SETQ FSUB_ (CDR FSUB_)))
               (GO WHILELABEL))
             (RETURN FORG))))) 
(PUT 'PRIPROLI 'NUMBER-OF-ARGS 1) 
(PUT 'PRIPROLI 'DEFINED-ON-LINE '1779) 
(PUT 'PRIPROLI 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRIPROLI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIPROLI (PROCLIST)
    (PROG (I L CPY)
      (SETQ I 0)
      (PROG (A)
        (SETQ A PROCLIST)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ CPY FULL_PROC_LIST_)
            (SETQ I 1)
            (PROG ()
             WHILELABEL
              (COND ((NOT (NEQ A (CAR CPY))) (RETURN NIL)))
              (PROGN (SETQ I (ADD1 I)) (SETQ CPY (CDR CPY)))
              (GO WHILELABEL))
            (COND ((NULL CPY) (SETQ I 0)))
            (TERPRI)
            (COND ((LESSP I 10) (PROGN (PRIN2 " ") NIL)))
            (PROGN (PRIN2 I) NIL)
            (PROGN (PRIN2 " : ") NIL)
            (COND
             ((PAIRP (SETQ L (GET A 'DESCRIPTION)))
              (PROG (S)
                (SETQ S L)
               LAB
                (COND ((NULL S) (RETURN NIL)))
                ((LAMBDA (S) (COND (S (PROGN (PRIN2 S) NIL)))) (CAR S))
                (SETQ S (CDR S))
                (GO LAB)))
             (T (PROGN (PRIN2 A) NIL)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (TERPRI))) 
(PUT 'PRIPROLINR 'NUMBER-OF-ARGS 2) 
(PUT 'PRIPROLINR 'DEFINED-ON-LINE '1797) 
(PUT 'PRIPROLINR 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRIPROLINR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIPROLINR (PROCLIST FULLPROCLIST)
    (PROG (I J CFPL)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ J 0)
      (PROG (A)
        (SETQ A PROCLIST)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ J (PLUS J 1))
            (SETQ I 1)
            (SETQ CFPL FULLPROCLIST)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND CFPL (NEQ A (CAR CFPL)))) (RETURN NIL)))
              (PROGN (SETQ I (ADD1 I)) (SETQ CFPL (CDR CFPL)))
              (GO WHILELABEL))
            (COND
             (CFPL
              (PROGN
               (COND ((GREATERP J 1) (PROGN (PRIN2 ",") NIL)))
               (COND ((GREATERP J 21) (PROGN (SETQ J 1) (TERPRI))))
               (PROGN (PRIN2 I) NIL))))
            NIL))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROGN (PRIN2 ";") NIL)
      (TERPRI))) 
(PUT 'CHANGEPROCLIST 'NUMBER-OF-ARGS 0) 
(PUT 'CHANGEPROCLIST 'DEFINED-ON-LINE '1813) 
(PUT 'CHANGEPROCLIST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CHANGEPROCLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHANGEPROCLIST NIL
    (PROG (L P ERR)
      (TERPRI)
      (PROGN
       (PRIN2 "Please type in a list of the numbers 1 .. ")
       (PRIN2 (LENGTH FULL_PROC_LIST_))
       (PRIN2 ", like 1,2,5,4,..,15; which")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "will be the new priority list of procedures done by CRACK.")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "Numbers stand for the following actions:") NIL)
      (TERPRI)
      (PRIPROLI FULL_PROC_LIST_)
      (TERPRI)
      (PROGN (PRIN2 "The list so far was: ") NIL)
      (PRIPROLINR PROC_LIST_ FULL_PROC_LIST_)
      (CHANGE_PROMPT_TO "The new list: ")
      (SETQ L (TERMLISTREAD))
      (RESTORE_INTERACTIVE_PROMPT)
      (COND ((NULL L) (SETQ ERR T))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT L) (RETURN NIL)))
                (PROGN
                 (COND
                  ((OR (NOT (FIXP (CAR L)))
                       (GREATERP (CAR L) (LENGTH FULL_PROC_LIST_)))
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (PRIN2 "Error: ")
                     (PRIN2 (CAR L))
                     (PRIN2 " is not one of the possible numbers.")
                     NIL)
                    (SETQ L NIL)
                    (SETQ ERR T)))
                  (T
                   (PROGN
                    (SETQ P (UNION (LIST (NTH FULL_PROC_LIST_ (CAR L))) P))
                    (SETQ L (CDR L))))))
                (GO WHILELABEL))
              NIL)))
      (COND
       ((NOT ERR)
        (PROGN
         (SETQ PROC_LIST_ (REVERSE P))
         (COND
          ((OR (NULL PROC_LIST_) (NEQ 'TO_DO (CAR PROC_LIST_)))
           (PROGN
            (SETQ PROC_LIST_ (CONS 'TO_DO PROC_LIST_))
            (PROGN
             (PRIN2
              "The first number must be 1 (to do most urgent steps first).")
             NIL)
            (TERPRI))))
         (COND
          (SIZE_WATCH
           (SETQ SIZE_HIST
                   (CONS
                    (CONS 'CP
                          (PROG (L FORALL-RESULT FORALL-ENDPTR)
                            (SETQ L PROC_LIST_)
                            (COND ((NULL L) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                             NIL)))
                           LOOPLABEL
                            (SETQ L (CDR L))
                            (COND ((NULL L) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                    SIZE_HIST))))
         NIL))
       (T
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "The procedure list is still unchanged.") NIL)
         (TERPRI)))))) 
(PUT 'PRINTPROCLIST 'NUMBER-OF-ARGS 0) 
(PUT 'PRINTPROCLIST 'DEFINED-ON-LINE '1860) 
(PUT 'PRINTPROCLIST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINTPROCLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINTPROCLIST NIL
    (PROG ()
      (TERPRI)
      (PROGN (PRIN2 "Procedures used currently for automatic execution:") NIL)
      (PRIPROLI PROC_LIST_))) 
(PUT 'PRINTFULLPROCLIST 'NUMBER-OF-ARGS 0) 
(PUT 'PRINTFULLPROCLIST 'DEFINED-ON-LINE '1867) 
(PUT 'PRINTFULLPROCLIST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINTFULLPROCLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINTFULLPROCLIST NIL
    (PROG ()
      (TERPRI)
      (PROGN (PRIN2 "The complete list of available procedures:") NIL)
      (PRIPROLI FULL_PROC_LIST_))) 
(PUT 'PRINTMAINMENU 'NUMBER-OF-ARGS 0) 
(PUT 'PRINTMAINMENU 'DEFINED-ON-LINE '1874) 
(PUT 'PRINTMAINMENU 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINTMAINMENU 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINTMAINMENU NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H '(I_HD I_HP I_HF I_HC I_HI I_HB I_HL I_HE))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (PROGN
            (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
            (PRIN2 " : ")
            (PRIN2 (CAR (GET H 'DESCRIPTION)))
            NIL)
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HD 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HD 'DEFINED-ON-LINE '1881) 
(PUT 'PRINT_HD 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HD NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H
               '(I_E I_EO I_PI I_F I_V I_S I_FC I_PE I_PH I_PV I_PF I_PO I_PU
                 I_PR I_PD I_PS I_LC I_CA I_WS I_SN I_SS I_W))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (COND
            ((LESSP (LENGTH (EXPLODE H)) 4)
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 "  : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL))
            (T
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 " : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL)))
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HP 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HP 'DEFINED-ON-LINE '1893) 
(PUT 'PRINT_HP 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HP NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H
               '(I_A I_G I_T I_P1 I_P2 |I_#| I_L I_SB I_RB I_BM I_AN I_RS I_X
                 I_Q I_QH I_QQ))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (COND
            ((OR (LESSP (LENGTH (EXPLODE H)) 4) (EQUAL H '|I_#|))
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 "  : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL))
            (T
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 " : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL)))
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HF 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HF 'DEFINED-ON-LINE '1905) 
(PUT 'PRINT_HF 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HF NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H
               '(I_PL I_PM I_PA I_CP I_OG I_OD I_OI I_OR I_OM I_OF I_OP I_NE
                 I_NF I_NI I_NA I_AS I_KE I_FI I_FA I_CS I_FS I_LL I_RE I_RF
                 I_ST I_CM I_LR I_CR I_MO I_AP I_HO))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (COND
            ((LESSP (LENGTH (EXPLODE H)) 4)
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 "  : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL))
            (T
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 " : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL)))
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HC 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HC 'DEFINED-ON-LINE '1918) 
(PUT 'PRINT_HC 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HC NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H '(I_R I_RD I_N I_DE I_DI I_C I_PT))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (COND
            ((LESSP (LENGTH (EXPLODE H)) 4)
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 "  : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL))
            (T
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 " : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL)))
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HI 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HI 'DEFINED-ON-LINE '1930) 
(PUT 'PRINT_HI 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HI 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HI NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H '(I_I I_ID I_IW I_IR I_IA I_IH I_IS I_II I_IC I_IY))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (COND
            ((LESSP (LENGTH (EXPLODE H)) 4)
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 "  : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL))
            (T
             (PROGN
              (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
              (PRIN2 " : ")
              (PRIN2 (CAR (GET H 'DESCRIPTION)))
              NIL)))
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HB 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HB 'DEFINED-ON-LINE '1941) 
(PUT 'PRINT_HB 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HB NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H NIL)
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (PROGN
            (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
            (PRIN2 " : ")
            (PRIN2 (CAR (GET H 'DESCRIPTION)))
            NIL)
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HL 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HL 'DEFINED-ON-LINE '1958) 
(PUT 'PRINT_HL 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HL NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H '(I_XP I_SP I_JP I_PP I_WP I_YP I_ZP I_VP I_NP I_MP I_TP I_DP))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (PROGN
            (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
            (PRIN2 " : ")
            (PRIN2 (CAR (GET H 'DESCRIPTION)))
            NIL)
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'PRINT_HE 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_HE 'DEFINED-ON-LINE '1966) 
(PUT 'PRINT_HE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'PRINT_HE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_HE NIL
    (PROGN
     (TERPRI)
     (PROG (H)
       (SETQ H '(I_FO I_FF I_GS I_GG I_GR I_DF))
      LAB
       (COND ((NULL H) (RETURN NIL)))
       ((LAMBDA (H)
          (PROGN
           (PROGN
            (PRIN2 (COMPRESS (CDDR (EXPLODE H))))
            (PRIN2 " : ")
            (PRIN2 (CAR (GET H 'DESCRIPTION)))
            NIL)
           (TERPRI)))
        (CAR H))
       (SETQ H (CDR H))
       (GO LAB)))) 
(PUT 'TO_DO 'NUMBER-OF-ARGS 1) 
(PUT 'TO_DO 'DEFINED-ON-LINE '1973) 
(PUT 'TO_DO 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'TO_DO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TO_DO (ARGLIST)
    (COND
     (TO_DO_LIST
      (PROG (P L)
        (SETQ P (CAR TO_DO_LIST))
        (SETQ TO_DO_LIST (CDR TO_DO_LIST))
        (COND
         ((AND TR_MAIN PRINT_ PRINT_MORE)
          (COND
           ((PAIRP (SETQ L (GET (CAR P) 'DESCRIPTION)))
            (PROGN
             (PROG (A)
               (SETQ A L)
              LAB
               (COND ((NULL A) (RETURN NIL)))
               ((LAMBDA (A) (COND (A (PROGN (PRIN2 A) NIL)))) (CAR A))
               (SETQ A (CDR A))
               (GO LAB))
             (PROGN (PRIN2 " : ") NIL)))
           (T (PROGN (PRIN2 "trying ") (PRIN2 (CAR P)) (PRIN2 " : ") NIL)))))
        (SETQ L
                (APPLY (CAR P)
                       (LIST
                        (CONS (CAR ARGLIST)
                              (CONS (CADR ARGLIST)
                                    (CONS (CADDR ARGLIST) (CDR P)))))))
        (COND ((NOT L) (SETQ L ARGLIST)))
        (RETURN L))))) 
(PUT 'SUBST_DERIVATIVE 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_DERIVATIVE 'DEFINED-ON-LINE '1989) 
(PUT 'SUBST_DERIVATIVE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SUBST_DERIVATIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_DERIVATIVE (ARGLIST)
    (PROG (F L Q G H PDES FORG FOUND_SUB)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (SETQ L (CHECK_SUBST_DF PDES FORG))
      (PROG (D)
        (SETQ D L)
       LAB
        (COND ((NULL D) (RETURN NIL)))
        ((LAMBDA (D)
           (COND
            ((NOT (IN_CYCLE (LIST 9 STEPCOUNTER_ D)))
             (PROGN
              (SETQ FOUND_SUB T)
              (SETQ F (NEWFCT FNAME_ (FCTARGS (CADR D)) NFCT_))
              (SETQ NFCT_ (ADD1 NFCT_))
              (SETQ FTEM_ (FCTINSERT F (DELETE (CADR D) FTEM_)))
              (COND
               ((AND FLIN_ (NOT (FREEOF FLIN_ (CADR D))))
                (SETQ FLIN_ (SORT_ACCORDING_TO (CONS F FLIN_) FTEM_))))
              (COND
               (PRINT_
                (PROGN
                 (TERPRI)
                 (PROGN (PRIN2 "replacing ") NIL)
                 (FCTPRINT1 D)
                 (PROGN (PRIN2 " by ") NIL)
                 (FCTPRINT (LIST F))
                 (TERPRI))))
              (PROG (S)
                (SETQ S PDES)
               LAB
                (COND ((NULL S) (RETURN NIL)))
                ((LAMBDA (S) (DFSUBST_UPDATE F D S)) (CAR S))
                (SETQ S (CDR S))
                (GO LAB))
              (SETQ H (CDDR D))
              (SETQ G (SIMP F))
              (PROG ()
               WHILELABEL
                (COND ((NOT H) (RETURN NIL)))
                (PROGN
                 (PROG (R)
                   (SETQ R 1)
                  LAB
                   (COND
                    ((MINUSP
                      (DIFFERENCE
                       (COND
                        ((OR (EQUAL (LENGTH H) 1)
                             (AND (GREATERP (LENGTH H) 1)
                                  (NOT (FIXP (CADR H)))))
                         1)
                        (T (CADR H)))
                       R))
                     (RETURN NIL)))
                   (SETQ G
                           (ADDSQ (SIMP (GENSYM))
                                  (MKSQ (LIST 'INT (PREPSQ G) (CAR H)) 1)))
                   (SETQ R (PLUS2 R 1))
                   (GO LAB))
                 (SETQ H (CDR H))
                 (COND ((AND H (FIXP (CAR H))) (SETQ H (CDR H)))))
                (GO WHILELABEL))
              (SETQ INEQ_
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S INEQ_)
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (S)
                                            (SUBSQ S
                                                   (LIST
                                                    (CONS (CADR D)
                                                          (LIST '*SQ G T)))))
                                          (CAR S))
                                         NIL)))
                       LOOPLABEL
                        (SETQ S (CDR S))
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (SUBSQ S
                                           (LIST
                                            (CONS (CADR D) (LIST '*SQ G T)))))
                                  (CAR S))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ FSUB_
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S FSUB_)
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (S)
                                            (CONS (CAR S)
                                                  (LIST '*SQ
                                                        (SUBSQ (CADDR S)
                                                               (LIST
                                                                (CONS (CADR D)
                                                                      (LIST
                                                                       '*SQ G
                                                                       T))))
                                                        T)))
                                          (CAR S))
                                         NIL)))
                       LOOPLABEL
                        (SETQ S (CDR S))
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS (CAR S)
                                          (LIST '*SQ
                                                (SUBSQ (CADDR S)
                                                       (LIST
                                                        (CONS (CADR D)
                                                              (LIST '*SQ G
                                                                    T))))
                                                T)))
                                  (CAR S))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (COND
               ((MEMBER (CADR D) FORG)
                (PROGN
                 (SETQ FTEM_ (FCTINSERT (CADR D) FTEM_))
                 (SETQ Q
                         (MKEQSQ (ADDSQ (SIMP D) (NEGSQ (SIMP F))) NIL NIL
                          (LIST F (CADR D)) (FCTARGS F) ALLFLAGS_ NIL (LIST 0)
                          NIL PDES))
                 (REMFLAG (LIST Q) 'TO_EVAL)
                 (PUT Q 'NOT_TO_EVAL (CONS F (GET Q 'NOT_TO_EVAL)))
                 (SETQ PDES (EQINSERT Q PDES)))))
              (SETQ FORG (DFSUBST_FORG F G (CADR D) FORG))
              NIL))))
         (CAR D))
        (SETQ D (CDR D))
        (GO LAB))
      (RETURN (COND (FOUND_SUB (LIST PDES FORG)) (T NIL))))) 
(PUT 'UNDO_SUBST_DERIVATIVE 'NUMBER-OF-ARGS 1) 
(PUT 'UNDO_SUBST_DERIVATIVE 'DEFINED-ON-LINE '2044) 
(PUT 'UNDO_SUBST_DERIVATIVE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'UNDO_SUBST_DERIVATIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNDO_SUBST_DERIVATIVE (ARGLIST)
    (PROG (SUCCESS)
      (PROG (P)
        (SETQ P (CAR ARGLIST))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((GET P 'NOT_TO_EVAL)
             (PROGN
              (REMPROP P 'NOT_TO_EVAL)
              (FLAG (LIST P) 'TO_EVAL)
              (SETQ SUCCESS T)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (COND (SUCCESS ARGLIST) (T NIL))))) 
(PUT 'SUBST_POWER 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_POWER 'DEFINED-ON-LINE '2058) 
(PUT 'SUBST_POWER 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SUBST_POWER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_POWER (ARGLIST)
    (PROG (PDES S A AL D F P Q NEWF SB NEW_EQ OLD_EQ OK)
      (SETQ PDES (CAR ARGLIST))
      (PROG (S)
        (SETQ S PDES)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((EQUAL (GET S 'NVARS) 0)
             (PROG (D)
               (SETQ D (GET S 'DERIVS))
              LAB
               (COND ((NULL D) (RETURN NIL)))
               ((LAMBDA (D)
                  (PROGN
                   (SETQ A AL)
                   (SETQ F (CAAR D))
                   (SETQ P (CDR D))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (AND A (NEQ F (CAAR A)))) (RETURN NIL)))
                     (SETQ A (CDR A))
                     (GO WHILELABEL))
                   (COND ((NULL A) (SETQ AL (CONS (LIST F P 1) AL)))
                         ((NEQ (CADAR A) 1)
                          (COND ((EQUAL P 1) (RPLACD (CAR A) (LIST 1 1)))
                                ((EQUAL (CADAR A) P)
                                 (RPLACD (CAR A)
                                         (LIST (CADAR A) (ADD1 (CADDAR A)))))
                                (T
                                 (RPLACD (CAR A)
                                         (LIST (GCDF (CADAR A) P)
                                               (ADD1 (CADDAR A))))))))))
                (CAR D))
               (SETQ D (CDR D))
               (GO LAB)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND AL (OR (EQUAL (CADAR AL) 1) (EQUAL (CADDAR AL) 1))))
          (RETURN NIL)))
        (SETQ AL (CDR AL))
        (GO WHILELABEL))
      (COND
       (AL
        (PROGN
         (SETQ A AL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR A)) (RETURN NIL)))
           (COND
            ((OR (EQUAL (CADAR (CDR A)) 1) (EQUAL (CADDAR (CDR A)) 1))
             (RPLACD A (CDDR A)))
            (T (SETQ A (CDR A))))
           (GO WHILELABEL)))))
      (COND ((NULL AL) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT AL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR AL))
         (SETQ AL (CDR AL))
         (SETQ NEWF (NEWFCT FNAME_ NIL NFCT_))
         (SETQ NFCT_ (ADD1 NFCT_))
         (SETQ FTEM_ (FCTINSERT NEWF FTEM_))
         (SETQ F (CAR A))
         (SETQ SB (LIST 'REPLACEBY (LIST 'EXPT (CAR A) (CADR A)) NEWF))
         (AEVAL* (LET (LIST SB)))
         (SETQ NEW_EQ NIL)
         (SETQ OLD_EQ NIL)
         (SETQ OK T)
         (SETQ P PDES)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND P OK)) (RETURN NIL)))
           (COND ((FREEOF (GET (CAR P) 'FCTS) F) (SETQ P (CDR P)))
                 (T
                  (PROGN
                   (SETQ Q
                           (MKEQSQ (GET (CAR P) 'SQVAL) NIL NIL
                            (CONS NEWF (GET (CAR P) 'FCTS)) (GET (CAR P) 'VARS)
                            ALLFLAGS_ T (LIST 0) NIL PDES))
                   (COND ((NOT (FREEOF (GET Q 'FCTS) F)) (SETQ OK NIL))
                         (T
                          (PROGN
                           (SETQ NEW_EQ (CONS Q NEW_EQ))
                           (SETQ OLD_EQ (CONS (CAR P) OLD_EQ))
                           (SETQ P (CDR P))))))))
           (GO WHILELABEL))
         (COND
          ((NULL OK)
           (PROG (Q)
             (SETQ Q NEW_EQ)
            LAB
             (COND ((NULL Q) (RETURN NIL)))
             ((LAMBDA (Q) (DROP_PDE Q PDES NIL)) (CAR Q))
             (SETQ Q (CDR Q))
             (GO LAB)))
          (T
           (PROGN
            (AEVAL* (CLEAR (LIST SB)))
            (PROG (Q)
              (SETQ Q NEW_EQ)
             LAB
              (COND ((NULL Q) (RETURN NIL)))
              ((LAMBDA (Q) (SETQ PDES (EQINSERT Q PDES))) (CAR Q))
              (SETQ Q (CDR Q))
              (GO LAB))
            (PROG (Q)
              (SETQ Q OLD_EQ)
             LAB
              (COND ((NULL Q) (RETURN NIL)))
              ((LAMBDA (Q) (SETQ PDES (DROP_PDE Q PDES NIL))) (CAR Q))
              (SETQ Q (CDR Q))
              (GO LAB))
            (SETQ Q
                    (MKEQSQ NIL NIL
                     (LIST 'DIFFERENCE NEWF (LIST 'EXPT (CAR A) (CADR A)))
                     (LIST NEWF F) NIL ALLFLAGS_ NIL (LIST 0) NIL PDES))
            (PUT Q 'NOT_TO_EVAL (LIST NEWF))
            (SETQ PDES (EQINSERT Q PDES))))))
        (GO WHILELABEL))
      (RETURN (CONS PDES (CDR ARGLIST))))) 
(PUT 'FACTORIZE_ANY 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE_ANY 'DEFINED-ON-LINE '2145) 
(PUT 'FACTORIZE_ANY 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'FACTORIZE_ANY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE_ANY (ARGLIST)
    (PROG (H L)
      (COND (EXPERT_MODE (SETQ L (SELECTPDES (CAR ARGLIST) 1)))
            (T (SETQ L (CADDDR ARGLIST))))
      (SETQ L (GET_FACT_PDE L NIL))
      (COND (L (SETQ H (GET L 'CASE2SEP))))
      (RETURN
       (COND
        (H
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) H)))
        (L
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST)
                (CAR (GET L 'FAC)))))
        (T NIL))))) 
(PUT 'FACTORIZE_TO_SUBSTITUTE 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE_TO_SUBSTITUTE 'DEFINED-ON-LINE '2160) 
(PUT 'FACTORIZE_TO_SUBSTITUTE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'FACTORIZE_TO_SUBSTITUTE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE_TO_SUBSTITUTE (ARGLIST)
    (PROG (L)
      (COND (EXPERT_MODE (SETQ L (SELECTPDES (CAR ARGLIST) 1)))
            (T (SETQ L (CADDDR ARGLIST))))
      (SETQ L (GET_FACT_PDE L T))
      (RETURN
       (COND
        (L
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST)
                (CAR (GET L 'FAC)))))
        (T NIL))))) 
(PUT 'SEPARATION 'NUMBER-OF-ARGS 1) 
(PUT 'SEPARATION 'DEFINED-ON-LINE '2173) 
(PUT 'SEPARATION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SEPARATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEPARATION (ARGLIST)
    (COND
     (VL_
      (PROG (P L L1 PDES FORG)
        (SETQ PDES (CAR ARGLIST))
        (SETQ FORG (CADR ARGLIST))
        (COND (EXPERT_MODE (SETQ L1 (SELECTPDES PDES 1)))
              (T (SETQ L1 (CADDDR ARGLIST))))
        (COND
         ((SETQ P (GET_SEPAR_PDE L1))
          (PROGN
           (SETQ L (SEPARATE P PDES))
           (COND ((EQUAL L 1) (SETQ L (LIST PDES FORG)))
                 ((AND L
                       (OR (GREATERP (LENGTH L) 1)
                           (AND (EQUAL (LENGTH L) 1) (NEQ (CAR L) P))))
                  (PROGN
                   (SETQ PDES (DROP_PDE P PDES NIL))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT L) (RETURN NIL)))
                     (PROGN
                      (SETQ PDES (EQINSERT (CAR L) PDES))
                      (SETQ L (CDR L)))
                     (GO WHILELABEL))
                   (SETQ L (LIST PDES FORG))))))))
        (RETURN L))))) 
(PUT 'CASE_SEPARATION 'NUMBER-OF-ARGS 1) 
(PUT 'CASE_SEPARATION 'DEFINED-ON-LINE '2198) 
(PUT 'CASE_SEPARATION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CASE_SEPARATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASE_SEPARATION (ARGLIST)
    (COND
     ((NULL LIN_PROBLEM)
      (PROG (FORCE_SEP_BAK H)
        (SETQ FORCE_SEP_BAK FORCE_SEP)
        (SETQ FORCE_SEP T)
        (SETQ H (SEPARATION ARGLIST))
        (SETQ FORCE_SEP FORCE_SEP_BAK)
        (RETURN H))))) 
(PUT 'ALG_SOLVE_SYSTEM 'NUMBER-OF-ARGS 1) 
(PUT 'ALG_SOLVE_SYSTEM 'DEFINED-ON-LINE '2210) 
(PUT 'ALG_SOLVE_SYSTEM 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ALG_SOLVE_SYSTEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG_SOLVE_SYSTEM (ARGLIST)
    (PROG (PDES L1 L2 L3 L4 L5 L6 FL VL ZD PDES2)
      (SETQ PDES (CAR ARGLIST))
      (SETQ L1 (SELECT_FROM_LIST PDES NIL))
      (COND ((NULL L1) (RETURN NIL)))
      (PROG (L2)
        (SETQ L2 L1)
       LAB
        (COND ((NULL L2) (RETURN NIL)))
        ((LAMBDA (L2) (SETQ VL (UNION (GET L2 'VARS) VL))) (CAR L2))
        (SETQ L2 (CDR L2))
        (GO LAB))
      (PROG (L2)
        (SETQ L2 L1)
       LAB
        (COND ((NULL L2) (RETURN NIL)))
        ((LAMBDA (L2) (SETQ FL (UNION (GET L2 'FCTS) FL))) (CAR L2))
        (SETQ L2 (CDR L2))
        (GO LAB))
      (SETQ L1
              (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                (SETQ L2 L1)
                (COND ((NULL L2) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (L2) (GET L2 'SQVAL)) (CAR L2))
                                      NIL)))
               LOOPLABEL
                (SETQ L2 (CDR L2))
                (COND ((NULL L2) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (L2) (GET L2 'SQVAL)) (CAR L2)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROGN
       (PRIN2 "Please give a list of constants, functions or derivatives")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "of functions to be solved algebraically, like f,g,df(g,x,2);")
       NIL)
      (TERPRI)
      (SETQ L2 (TERMLISTREAD))
      (COND
       (L2
        (PROGN
         (SETQ L3 (CDR (SOLVEEVAL (LIST (CONS 'LIST L1) (CONS 'LIST L2)))))
         (COND
          ((NULL L3)
           (PROGN (PROGN (PRIN2 "There is no solution.") NIL) (TERPRI)))
          ((GREATERP (LENGTH L3) 1)
           (PROGN
            (PROGN (PRIN2 "can currently not handle more than 1 solution") NIL)
            (TERPRI)))
          (T
           (PROGN
            (SETQ L3
                    (PROG (L4 FORALL-RESULT FORALL-ENDPTR)
                      (SETQ L4 L3)
                      (COND ((NULL L4) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (L4)
                                          (PROGN
                                           (SETQ L4
                                                   (PROG (L5 FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ L5 (CDR L4))
                                                     (COND
                                                      ((NULL L5) (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (L5)
                                                                         (PROGN
                                                                          (SETQ ZD
                                                                                  (UNION
                                                                                   (ZERO_DEN
                                                                                    (REVAL1
                                                                                     L5
                                                                                     T)
                                                                                    FL)
                                                                                   ZD))
                                                                          (SETQ L6
                                                                                  (REVAL1
                                                                                   (LIST
                                                                                    'PLUS
                                                                                    (CADR
                                                                                     L5)
                                                                                    (LIST
                                                                                     'MINUS
                                                                                     (CADDR
                                                                                      L5)))
                                                                                   T))
                                                                          (COND
                                                                           ((AND
                                                                             (PAIRP
                                                                              L6)
                                                                             (EQUAL
                                                                              (CAR
                                                                               L6)
                                                                              'QUOTIENT))
                                                                            (CADR
                                                                             L6))
                                                                           (T
                                                                            L6))))
                                                                       (CAR
                                                                        L5))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ L5 (CDR L5))
                                                     (COND
                                                      ((NULL L5)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (L5)
                                                                 (PROGN
                                                                  (SETQ ZD
                                                                          (UNION
                                                                           (ZERO_DEN
                                                                            (REVAL1
                                                                             L5
                                                                             T)
                                                                            FL)
                                                                           ZD))
                                                                  (SETQ L6
                                                                          (REVAL1
                                                                           (LIST
                                                                            'PLUS
                                                                            (CADR
                                                                             L5)
                                                                            (LIST
                                                                             'MINUS
                                                                             (CADDR
                                                                              L5)))
                                                                           T))
                                                                  (COND
                                                                   ((AND
                                                                     (PAIRP L6)
                                                                     (EQUAL
                                                                      (CAR L6)
                                                                      'QUOTIENT))
                                                                    (CADR L6))
                                                                   (T L6))))
                                                               (CAR L5))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL)))))
                                        (CAR L4))
                                       NIL)))
                     LOOPLABEL
                      (SETQ L4 (CDR L4))
                      (COND ((NULL L4) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (L4)
                                  (PROGN
                                   (SETQ L4
                                           (PROG (L5 FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ L5 (CDR L4))
                                             (COND ((NULL L5) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (L5)
                                                                 (PROGN
                                                                  (SETQ ZD
                                                                          (UNION
                                                                           (ZERO_DEN
                                                                            (REVAL1
                                                                             L5
                                                                             T)
                                                                            FL)
                                                                           ZD))
                                                                  (SETQ L6
                                                                          (REVAL1
                                                                           (LIST
                                                                            'PLUS
                                                                            (CADR
                                                                             L5)
                                                                            (LIST
                                                                             'MINUS
                                                                             (CADDR
                                                                              L5)))
                                                                           T))
                                                                  (COND
                                                                   ((AND
                                                                     (PAIRP L6)
                                                                     (EQUAL
                                                                      (CAR L6)
                                                                      'QUOTIENT))
                                                                    (CADR L6))
                                                                   (T L6))))
                                                               (CAR L5))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ L5 (CDR L5))
                                             (COND
                                              ((NULL L5)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (L5)
                                                         (PROGN
                                                          (SETQ ZD
                                                                  (UNION
                                                                   (ZERO_DEN
                                                                    (REVAL1 L5
                                                                            T)
                                                                    FL)
                                                                   ZD))
                                                          (SETQ L6
                                                                  (REVAL1
                                                                   (LIST 'PLUS
                                                                         (CADR
                                                                          L5)
                                                                         (LIST
                                                                          'MINUS
                                                                          (CADDR
                                                                           L5)))
                                                                   T))
                                                          (COND
                                                           ((AND (PAIRP L6)
                                                                 (EQUAL
                                                                  (CAR L6)
                                                                  'QUOTIENT))
                                                            (CADR L6))
                                                           (T L6))))
                                                       (CAR L5))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                (CAR L4))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             ((EQUAL (LENGTH L3) 1)
              (PROGN
               (SETQ L4 (CAR L3))
               (SETQ PDES2 PDES)
               (PROG (L5)
                 (SETQ L5 L4)
                LAB
                 (COND ((NULL L5) (RETURN NIL)))
                 ((LAMBDA (L5)
                    (PROGN
                     (SETQ L5
                             (COND
                              (ZD
                               (MKEQSQ NIL (CONS (SIMP L5) ZD) NIL FL VL
                                ALLFLAGS_ NIL (LIST 0) NIL PDES))
                              (T
                               (MKEQSQ NIL NIL L5 FL VL ALLFLAGS_ NIL (LIST 0)
                                NIL PDES))))
                     (SETQ PDES (EQINSERT L5 PDES))
                     NIL))
                  (CAR L5))
                 (SETQ L5 (CDR L5))
                 (GO LAB))
               (COND
                (PRINT_
                 (PROGN
                  (SETQ PDES2 (SETDIFF PDES PDES2))
                  (PROGN (PRIN2 "New equations: ") (PRIN2 PDES2) NIL)
                  (TERPRI)
                  NIL)))
               (RETURN (LIST PDES (CADR ARGLIST)))))))))))))) 
(PUT 'ALG_SOLVE_SINGLE 'NUMBER-OF-ARGS 1) 
(PUT 'ALG_SOLVE_SINGLE 'DEFINED-ON-LINE '2265) 
(PUT 'ALG_SOLVE_SINGLE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ALG_SOLVE_SINGLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG_SOLVE_SINGLE (ARGLIST)
    (PROG (L L1 PDES FORG)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND (EXPERT_MODE (SETQ L1 (SELECTPDES PDES 1)))
            (T (SETQ L1 (CADDDR ARGLIST))))
      (COND
       ((SETQ L (ALGSOLVEDERIV L1 PDES))
        (PROGN
         (SETQ PDES (DROP_PDE (CAR L) PDES NIL))
         (SETQ PDES (EQINSERT (CDR L) PDES))
         (SETQ TO_DO_LIST
                 (CONS (LIST 'FACTORIZE_ANY (LIST (CDR L))) TO_DO_LIST))
         (SETQ L (LIST PDES FORG))
         NIL)))
      (RETURN L))) 
(PUT 'ALG_FOR_DERIV 'NUMBER-OF-ARGS 1) 
(PUT 'ALG_FOR_DERIV 'DEFINED-ON-LINE '2294) 
(PUT 'ALG_FOR_DERIV 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ALG_FOR_DERIV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG_FOR_DERIV (P)
    (PROG (DL D F)
      (SETQ DL (GET P 'DERIVS))
      (COND
       ((OR (NULL DL) (GREATERP (LENGTH (GET P 'ALLVARFCTS)) 1)) (RETURN NIL)))
      (COND
       ((AND FLIN_ (NOT (FREEOFLIST (GET P 'ALLVARFCTS) FLIN_))) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND DL (NULL D))) (RETURN NIL)))
        (PROGN
         (SETQ D (CAR DL))
         (SETQ F (CAAR D))
         (COND
          ((LESSP (FCTLENGTH F) (GET P 'NVARS))
           (PROGN (SETQ D NIL) (SETQ DL NIL)))
          (T
           (PROGN
            (SETQ DL (CDR DL))
            (COND ((EQUAL (CDR D) 1) (SETQ D NIL)))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND DL (EQUAL F (CAAAR DL)))) (RETURN NIL)))
              (PROGN
               (COND ((AND D (NEQ (CAR D) (CAAR DL))) (SETQ D NIL)))
               (SETQ DL (CDR DL)))
              (GO WHILELABEL))))))
        (GO WHILELABEL))
      (RETURN D))) 
(PUT 'ALGSOLVEDERIV 'NUMBER-OF-ARGS 2) 
(PUT 'ALGSOLVEDERIV 'DEFINED-ON-LINE '2341) 
(PUT 'ALGSOLVEDERIV 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ALGSOLVEDERIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGSOLVEDERIV (L PDES)
    (PROG (D P Q DROP FCTRL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND L (NULL (SETQ D (ALG_FOR_DERIV (CAR L)))))) (RETURN NIL)))
        (SETQ L (CDR L))
        (GO WHILELABEL))
      (COND
       (D
        (PROGN
         (SETQ P (CDR D))
         (SETQ D
                 (SOLVEEVAL
                  (LIST (LIST '*SQ (GET (CAR L) 'SQVAL) T)
                        (COND ((EQUAL 1 (LENGTH (CAR D))) (CAAR D))
                              (T (CONS 'DF (CAR D)))))))
         (COND
          ((GREATERP P 2)
           (COND
            ((NOT
              (FREEOFLIST D
               (LIST 'ROOT_OF 'COS 'SIN 'SINH 'COSH 'TAN 'ACOSH 'ASIN 'ATAN
                     'ASINH 'ACOSH)))
             (SETQ D NIL))
            (T
             (PROGN
              (SETQ Q (SEARCH_LI D 'SQRT))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND Q (NULL DROP))) (RETURN NIL)))
                (COND
                 ((PAIRP (CAR Q))
                  (PROGN (SETQ DROP T) (SETQ Q (LIST 'SQRT (CAR Q)))))
                 (T (SETQ Q (CDR Q))))
                (GO WHILELABEL))
              (COND
               ((NULL DROP)
                (PROGN
                 (SETQ Q (SEARCH_LI2 D 'EXPT))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND Q (NULL DROP))) (RETURN NIL)))
                   (COND
                    ((AND (NOT (FIXP (CADDAR Q))) (PAIRP (CADAR Q)))
                     (PROGN (SETQ DROP T) (SETQ Q (CAR Q))))
                    (T (SETQ Q (CDR Q))))
                   (GO WHILELABEL)))))
              (COND
               (DROP
                (PROGN
                 (PROGN (PRIN2 "The solution ") NIL)
                 (MATHPRINT D)
                 (PROGN (PRIN2 "of the equation ") NIL)
                 (MATHPRINT (LIST '*SQ (GET (CAR L) 'SQVAL) T))
                 (PROGN (PRIN2 "was ignored because of the term") NIL)
                 (MATHPRINT Q)
                 (SETQ D NIL))))
              NIL)))))
         (COND
          ((OR (NOT (FREEOF D 'I)) (NOT (FREEOF D '|:GI:|)))
           (AEVAL (ON (LIST 'COMPLEX)))))
         (COND
          ((NOT (FREEOF D 'ABS))
           (PROGN
            (AEVAL (LET '(ABS_)))
            (SETQ D (AEVAL D))
            (AEVAL (CLEARRULES (LIST 'ABS_)))
            NIL)))
         (COND
          ((AND D (EQUAL (CAR D) 'LIST))
           (PROGN
            (SETQ P (SIMP 1))
            (PROG (EL)
              (SETQ EL (CDR D))
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL)
                 (COND
                  ((EQUAL (CAR EL) 'EQUAL)
                   (PROGN
                    (SETQ FCTRL
                            (CONS
                             (ADDSQ (SIMP (CADR EL)) (NEGSQ (SIMP (CADDR EL))))
                             FCTRL))
                    (SETQ P (MULTSQ (CAR FCTRL) P))))
                  (T (SETQ D NIL))))
               (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))))
          (T (SETQ D NIL)))
         (COND
          (D
           (PROGN
            (SETQ D P)
            (SETQ P (CAR L))
            (SETQ D
                    (MKEQSQ D FCTRL NIL (GET P 'FCTS) (GET P 'VARS) ALLFLAGS_
                     NIL (GET P 'ORDERINGS) NIL PDES))
            (COND
             (PRINT_
              (PROGN (PRIN2 P) (PRIN2 " factorized to ") (PRIN2 D) NIL)))))))))
      (RETURN (COND (D (CONS P D)) (T NIL))))) 
(PUT 'QUICK_INTEGRATION 'NUMBER-OF-ARGS 1) 
(PUT 'QUICK_INTEGRATION 'DEFINED-ON-LINE '2416) 
(PUT 'QUICK_INTEGRATION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'QUICK_INTEGRATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUICK_INTEGRATION (ARGLIST)
    (PROG (L L1 PDES FORG)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE
        (PROGN
         (SETQ L1 (SELECTPDES PDES 1))
         (FLAG L1 'TO_INT)
         (FLAG L1 'TO_FULLINT)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (COND
       ((SETQ L (QUICK_INTEGRATE_ONE_PDE L1))
        (PROGN
         (SETQ PDES (DELETE (CAR L) PDES))
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S) (SETQ PDES (EQINSERT S PDES))) (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (SETQ TO_DO_LIST
                      (CONS (LIST 'SUBST_LEVEL_35 (LIST S)) TO_DO_LIST)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ L (LIST PDES FORG)))))
      (COND
       ((AND (NULL L) TO_DO_LIST)
        (PROGN (SETQ L ARGLIST) (FLAG L1 'TO_INT) (FLAG L1 'TO_FULLINT))))
      (RETURN L))) 
(PUT 'FULL_INTEGRATION 'NUMBER-OF-ARGS 1) 
(PUT 'FULL_INTEGRATION 'DEFINED-ON-LINE '2438) 
(PUT 'FULL_INTEGRATION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'FULL_INTEGRATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FULL_INTEGRATION (ARGLIST)
    (PROG (L L1 PDES FORG)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE
        (PROGN
         (SETQ L1 (SELECTPDES PDES 1))
         (FLAG L1 'TO_INT)
         (FLAG L1 'TO_FULLINT)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (COND
       ((SETQ L (INTEGRATE_ONE_PDE L1 GENINT_ T))
        (PROGN
         (SETQ PDES (DELETE (CAR L) PDES))
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S) (SETQ PDES (EQINSERT S PDES))) (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((NOT (FREEOF PDES S))
                (SETQ TO_DO_LIST
                        (CONS (LIST 'SUBST_LEVEL_35 (LIST S)) TO_DO_LIST)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ L (LIST PDES FORG)))))
      (COND
       ((AND (NULL L) TO_DO_LIST)
        (PROGN (SETQ L ARGLIST) (FLAG L1 'TO_INT) (FLAG L1 'TO_FULLINT))))
      (RETURN L))) 
(PUT 'INTEGRATION 'NUMBER-OF-ARGS 1) 
(PUT 'INTEGRATION 'DEFINED-ON-LINE '2463) 
(PUT 'INTEGRATION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'INTEGRATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTEGRATION (ARGLIST)
    (PROG (L L1 PDES FORG)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE
        (PROGN
         (SETQ L1 (SELECTPDES PDES 1))
         (FLAG L1 'TO_INT)
         (FLAG L1 'TO_FULLINT)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (COND
       ((SETQ L (INTEGRATE_ONE_PDE L1 GENINT_ NIL))
        (PROGN
         (SETQ PDES (DELETE (CAR L) PDES))
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S) (SETQ PDES (EQINSERT S PDES))) (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S (CDR L))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((NOT (FREEOF PDES S))
                (SETQ TO_DO_LIST
                        (CONS (LIST 'SUBST_LEVEL_35 (LIST S)) TO_DO_LIST)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ L (LIST PDES FORG)))))
      (COND
       ((AND (NULL L) TO_DO_LIST)
        (PROGN (SETQ L ARGLIST) (FLAG L1 'TO_INT) (FLAG L1 'TO_FULLINT))))
      (RETURN L))) 
(PUT 'MULTINTFAC 'NUMBER-OF-ARGS 1) 
(PUT 'MULTINTFAC 'DEFINED-ON-LINE '2486) 
(PUT 'MULTINTFAC 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'MULTINTFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULTINTFAC (ARGLIST)
    (PROG (PDES FORG L STEM FTEM VL VL1)
      (SETQ PDES (CAR ARGLIST))
      (COND ((OR (NULL PDES) (EQUAL (LENGTH PDES) 1)) (RETURN NIL)))
      (SETQ FORG (CADR ARGLIST))
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((NOT (OR (GET P 'STARDE) (GET P 'NONRATIONAL)))
             (PROGN
              (SETQ STEM (CONS (PREPSQ (GET P 'SQVAL)) STEM))
              (SETQ FTEM (UNION (GET P 'FCTS) FTEM))
              (SETQ VL (UNION (GET P 'VARS) VL))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ VL1 VL)
      (SETQ FNEW_ NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT VL1) (RETURN NIL)))
        (COND
         ((SETQ L (FINDINTFAC STEM FTEM VL (CAR VL1) NIL NIL NIL NIL))
          (PROGN
           (SETQ FTEM (SMEMBERL FTEM (CAR L)))
           (SETQ VL (UNION (SMEMBERL VL (CAR L)) (ARGSET FTEM)))
           (SETQ L (ADDINTCO (CAR L) FTEM NIL VL (CAR VL1)))
           (PROG (F)
             (SETQ F FNEW_)
            LAB
             (COND ((NULL F) (RETURN NIL)))
             ((LAMBDA (F) (SETQ FTEM_ (FCTINSERT F FTEM_))) (CAR F))
             (SETQ F (CDR F))
             (GO LAB))
           (SETQ FTEM (UNION FNEW_ FTEM))
           (SETQ FNEW_ NIL)
           (SETQ PDES
                   (EQINSERT
                    (MKEQSQ NIL NIL L (SMEMBERL FTEM_ FTEM) VL ALLFLAGS_ T
                     (LIST 0) NIL PDES)
                    PDES))
           (SETQ VL1 NIL)
           (SETQ L (LIST PDES FORG))))
         (T (SETQ VL1 (CDR VL1))))
        (GO WHILELABEL))
      (RETURN L))) 
(PUT 'DIFF_LENGTH_REDUCTION 'NUMBER-OF-ARGS 1) 
(PUT 'DIFF_LENGTH_REDUCTION 'DEFINED-ON-LINE '2518) 
(PUT 'DIFF_LENGTH_REDUCTION 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'DIFF_LENGTH_REDUCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFF_LENGTH_REDUCTION (ARGLIST)
    (PROG (L)
      (SETQ L (DEC_AND_RED_LEN_ONE_STEP (CAR ARGLIST) FTEM_ (CADDR ARGLIST) 0))
      (COND (L (SETQ L (LIST L (CADR ARGLIST)))))
      (RETURN L))) 
(PUT 'CLEAN_DEC 'NUMBER-OF-ARGS 3) 
(PUT 'CLEAN_DEC 'DEFINED-ON-LINE '2528) 
(PUT 'CLEAN_DEC 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CLEAN_DEC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CLEAN_DEC (P PDES FLG)
    (PROG (PROPTY EL NL NEWPROPTY)
      (SETQ PROPTY (GET P FLG))
      (PROG (EL)
        (SETQ EL PROPTY)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ NL (INTERSECTION (CDR EL) PDES))
            (COND (NL (SETQ NEWPROPTY (CONS (CONS (CAR EL) NL) NEWPROPTY))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (PUT P FLG (REVERSE NEWPROPTY)))) 
(PUT 'CLEAN_PROP_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAN_PROP_LIST 'DEFINED-ON-LINE '2538) 
(PUT 'CLEAN_PROP_LIST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CLEAN_PROP_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAN_PROP_LIST (PDES)
    (COND
     ((AND (NULL (CAR RECYCLE_EQNS)) (CDR RECYCLE_EQNS)
           (GREATERP (LENGTH (CDR RECYCLE_EQNS)) 50))
      (PROGN
       (PROG (P)
         (SETQ P PDES)
        LAB
         (COND ((NULL P) (RETURN NIL)))
         ((LAMBDA (P)
            (PROGN
             (CLEAN_DEC P PDES 'DEC_WITH)
             (CLEAN_DEC P PDES 'DEC_WITH_RL)
             (CLEAN_DEC P PDES 'RES_WITH)
             (PUT P 'RL_WITH (INTERSECTION PDES (GET P 'RL_WITH)))
             NIL))
          (CAR P))
         (SETQ P (CDR P))
         (GO LAB))
       (SETQ RECYCLE_EQNS
               (CONS (APPEND (CAR RECYCLE_EQNS) (REVERSE (CDR RECYCLE_EQNS)))
                     NIL))
       NIL)))) 
(PUT 'CLEAN_UP 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAN_UP 'DEFINED-ON-LINE '2558) 
(PUT 'CLEAN_UP 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CLEAN_UP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAN_UP (PDES)
    (PROG (NEWPDES)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (COND ((FLAGP (CAR PDES) 'TO_DROP) (DROP_PDE (CAR PDES) NIL NIL))
               (T (SETQ NEWPDES (CONS (CAR PDES) NEWPDES))))
         (SETQ PDES (CDR PDES)))
        (GO WHILELABEL))
      (RETURN (REVERSE NEWPDES)))) 
(PUT 'CUT_SIZE_HIST 'NUMBER-OF-ARGS 0) 
(PUT 'CUT_SIZE_HIST 'DEFINED-ON-LINE '2569) 
(PUT 'CUT_SIZE_HIST 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'CUT_SIZE_HIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CUT_SIZE_HIST NIL
    (COND
     ((FIXP SIZE_WATCH)
      (PROG (N SH L)
        (SETQ N 0)
        (SETQ SH SIZE_HIST)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (LESSP N SIZE_WATCH) SH)) (RETURN NIL)))
          (PROGN (SETQ N (ADD1 N)) (SETQ SH (CDR SH)))
          (GO WHILELABEL))
        (COND
         (SH
          (PROGN
           (RPLACD SH NIL)
           (RPLACA SH
                   (CONS 'CP
                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L PROC_LIST_)
                           (COND ((NULL L) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                            NIL)))
                          LOOPLABEL
                           (SETQ L (CDR L))
                           (COND ((NULL L) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (L) (GET L 'NO)) (CAR L))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))))))) 
(PUT 'ADD_DIFFERENTIATED_PDES 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_DIFFERENTIATED_PDES 'DEFINED-ON-LINE '2581) 
(PUT 'ADD_DIFFERENTIATED_PDES 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ADD_DIFFERENTIATED_PDES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_DIFFERENTIATED_PDES (ARGLIST)
    (PROG (PDES L L1 L2 Q)
      (SETQ PDES (CAR ARGLIST))
      (COND (EXPERT_MODE (SETQ L1 (SELECTPDES PDES 1)))
            (T (SETQ L1 (CADDDR ARGLIST))))
      (PROG (P)
        (SETQ P L1)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((FLAGP P 'TO_DIFF)
             (PROGN
              (PROG (F)
                (SETQ F (GET P 'ALLVARFCTS))
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F)
                   (PROGN
                    (SETQ L2 (LD_DERIV P F))
                    (COND
                     ((AND L2 (GREATERP (CDR L2) 1))
                      (PROGN
                       (COND
                        (PRINT_
                         (PROGN
                          (TERPRI)
                          (PROGN
                           (PRIN2 "differentiating ")
                           (PRIN2 P)
                           (PRIN2 " w.r.t. ")
                           NIL)
                          (LISTPRINT (FCTARGS F))
                          (PROGN (PRIN2 " we get the new equations : ") NIL))))
                       (PROG (V)
                         (SETQ V (FCTARGS F))
                        LAB
                         (COND ((NULL V) (RETURN NIL)))
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ Q
                                     (MKEQSQ (DIFFSQ (GET P 'SQVAL) V) NIL NIL
                                      (GET P 'FCTS) (GET P 'VARS)
                                      (DELETE 'TO_DIFF ALLFLAGS_) T (LIST 0)
                                      NIL PDES))
                             (COND
                              (Q
                               (PROGN
                                (COND
                                 ((GREATERP (GET Q 'TERMS) 1)
                                  (PROGN
                                   (REMFLAG (LIST Q) 'TO_INT)
                                   (REMFLAG (LIST Q) 'TO_FULLINT))))
                                (PREVENT_SIMP V P Q)
                                (COND
                                 (PRINT_ (PROGN (PRIN2 Q) (PRIN2 " ") NIL)))
                                (SETQ PDES (EQINSERT Q PDES)))))))
                          (CAR V))
                         (SETQ V (CDR V))
                         (GO LAB))
                       (REMFLAG (LIST P) 'TO_DIFF)
                       (SETQ L (CONS PDES (CDR ARGLIST))))))))
                 (CAR F))
                (SETQ F (CDR F))
                (GO LAB))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN L))) 
(PUT 'ADD_DIFF_ISE 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_DIFF_ISE 'DEFINED-ON-LINE '2622) 
(PUT 'ADD_DIFF_ISE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'ADD_DIFF_ISE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_DIFF_ISE (ARGLIST)
    (COND
     ((OR EXPERT_MODE (NOT (IN_CYCLE (LIST 32 STEPCOUNTER_))))
      (PROG (PDES L L1 Q VLI)
        (SETQ PDES (CAR ARGLIST))
        (COND (EXPERT_MODE (SETQ L1 (SELECTPDES PDES 1)))
              (T (SETQ L1 (CADDDR ARGLIST))))
        (PROG (P)
          (SETQ P L1)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (COND
              ((AND (FLAGP P 'TO_DIFF) (NULL L) (GET P 'STARDE))
               (PROGN
                (SETQ VLI
                        (COND
                         (EXPERT_MODE (SELECT_FROM_LIST (GET P 'VARS) NIL))
                         (T (GET P 'VARS))))
                (COND
                 (PRINT_
                  (PROGN
                   (TERPRI)
                   (PROGN
                    (PRIN2 "differentiating ")
                    (PRIN2 P)
                    (PRIN2 " w.r.t. ")
                    NIL)
                   (LISTPRINT VLI)
                   (PROGN (PRIN2 " we get the new equations : ") NIL))))
                (PROG (V)
                  (SETQ V VLI)
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V)
                     (PROGN
                      (SETQ Q
                              (MKEQSQ (DIFFSQ (GET P 'SQVAL) V) NIL NIL
                               (GET P 'FCTS) (GET P 'VARS)
                               (DELETE 'TO_FULLINT (DELETE 'TO_INT ALLFLAGS_))
                               T (GET P 'ORDERINGS) NIL PDES))
                      (COND
                       ((NULL (GET Q 'STARDE))
                        (PROGN
                         (FLAG (LIST Q) 'TO_FULLINT)
                         (FLAG (LIST Q) 'TO_INT)
                         NIL)))
                      (PREVENT_SIMP V P Q)
                      (COND (PRINT_ (PROGN (PRIN2 Q) (PRIN2 " ") NIL)))
                      (SETQ PDES (EQINSERT Q PDES))
                      NIL))
                   (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB))
                (REMFLAG (LIST P) 'TO_DIFF)
                (SETQ L (CONS PDES (CDR ARGLIST)))
                NIL))))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (RETURN L))))) 
(PUT 'EXTERNAL_GROEBNER 'NUMBER-OF-ARGS 1) 
(PUT 'EXTERNAL_GROEBNER 'DEFINED-ON-LINE '2661) 
(PUT 'EXTERNAL_GROEBNER 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'EXTERNAL_GROEBNER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTERNAL_GROEBNER (ARGLIST)
    (COND
     ((AND (CAR ARGLIST) (GREATERP (LENGTH (CAR ARGLIST)) 1)
           (NOT
            (IN_CYCLE
             (LIST 59 STEPCOUNTER_ GROEB_SOLVE SINGULAR_TIME
                   (LENGTH (CAR ARGLIST)) (LENGTH (CADR ARGLIST))
                   (PROG (H FORALL-RESULT)
                     (SETQ H (CAR ARGLIST))
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND ((NULL H) (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS ((LAMBDA (H) (GET H 'TERMS)) (CAR H))
                                   FORALL-RESULT))
                     (SETQ H (CDR H))
                     (GO LAB1))
                   (PROG (H FORALL-RESULT)
                     (SETQ H (CAR ARGLIST))
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND ((NULL H) (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS ((LAMBDA (H) (GET H 'LENGTH)) (CAR H))
                                   FORALL-RESULT))
                     (SETQ H (CDR H))
                     (GO LAB1))))))
      (ERR_CATCH_GROEB ARGLIST)))) 
(PUT 'SPLIT_INTO_CASES 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT_INTO_CASES 'DEFINED-ON-LINE '2670) 
(PUT 'SPLIT_INTO_CASES 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SPLIT_INTO_CASES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT_INTO_CASES (ARGLIST)
    (COND
     ((NOT CONTRADICTION_)
      (PROG (H HH S PDES FORG CONTRAD N PF Q SQF L1 L2 RESULT INTACT PRINT_BAK
             ENLARGED_DEPL* SQF_MUST_BE_ZERO)
        (SETQ PDES (CAR ARGLIST))
        (SETQ FORG (CADR ARGLIST))
        (COND ((CDDDR ARGLIST) (SETQ H (CADDDR ARGLIST))))
        (COND
         ((EQUAL H PDES)
          (PROGN
           (SETQ INTACT T)
           (TERPRI)
           (PROGN
            (PRIN2 "Type in the expression for which its vanishing and")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 "non-vanishing should be considered.") NIL)
           (TERPRI)
           (PROGN (PRIN2 "You can use names of pds, e.g.: ") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "coeffn(e_12,df(f,x,2),1);    or   df(e_12,df(f,x,2));")
            NIL)
           (TERPRI)
           (CHANGE_PROMPT_TO "the expression: ")
           (SETQ H (TERMXREAD))
           (RESTORE_INTERACTIVE_PROMPT)
           (PROG (HH)
             (SETQ HH PDES)
            LAB
             (COND ((NULL HH) (RETURN NIL)))
             ((LAMBDA (HH)
                (COND
                 ((NOT (FREEOF H HH))
                  (PROGN
                   (COND
                    ((NULL (GET HH 'VAL))
                     (PUT HH 'VAL (PREPSQ (GET HH 'SQVAL)))))
                   (SETQ H (SUBST (GET HH 'VAL) HH H))
                   NIL))))
              (CAR HH))
             (SETQ HH (CDR HH))
             (GO LAB))
           (SETQ H (SIMP H)))))
        (SETQ PRINT_BAK PRINT_)
        (SETQ PRINT_ NIL)
        (SETQ HH (SIMPLIFYSQ H (SMEMBERL FTEM_ H) T NIL T))
        (COND ((EQUAL HH (LIST (CONS 1 1))) (SETQ CONTRADICTION_ T)))
        (SETQ PRINT_ PRINT_BAK)
        (COND
         (CONTRADICTION_
          (RETURN
           (PROGN
            (SETQ CONTRADICTION_ NIL)
            (COND
             (INTACT
              (PROGN
               (PROGN
                (PRIN2 "According to the known inequalities, ")
                (PRIN2 "this expression can not vanish!")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 " --> Back to main menu.") NIL)
               (TERPRI)
               NIL))
             (T
              (PROGN
               (PROG (L2)
                 (SETQ L2 HH)
                LAB
                 (COND ((NULL L2) (RETURN NIL)))
                 ((LAMBDA (L2)
                    (COND ((NEQ L2 (CONS 1 1)) (ADDSQINEQ PDES L2 NIL))))
                  (CAR L2))
                 (SETQ L2 (CDR L2))
                 (GO LAB))
               (ADDSQINEQ PDES H NIL)
               (LIST PDES FORG))))))))
        (COND
         (INTACT
          (PROGN
           (PROGN
            (PRIN2
             "If you first want to consider this expression to vanish and")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 "afterwards it to be non-zero then input t") NIL)
           (TERPRI)
           (PROGN (PRIN2 "                        otherwise input nil : ") NIL)
           (CHANGE_PROMPT_TO "")
           (SETQ S (TERMREAD))
           (RESTORE_INTERACTIVE_PROMPT)
           NIL))
         (T (SETQ S NIL)))
        (SETQ CONTRAD T)
        (SETQ N 0)
        (BACKUP_TO_FILE PDES FORG NIL)
        (SETQ SQF (CAR HH))
        (PROG (Q)
          (SETQ Q (CDR HH))
         LAB
          (COND ((NULL Q) (RETURN NIL)))
          ((LAMBDA (Q) (SETQ SQF (MULTSQ Q SQF))) (CAR Q))
          (SETQ Q (CDR Q))
          (GO LAB))
        (SETQ PF (PREPSQ SQF))
        (SETQ Q NIL)
       AGAIN
        (SETQ N (ADD1 N))
        (COND
         (S
          (PROGN
           (START_LEVEL N (LIST (LIST 'EQUAL 0 PF)))
           (COND (PRINT_ (TERPRI)))
           (COND
            (SQF_MUST_BE_ZERO
             (PROGN
              (PROG (Q)
                (SETQ Q PDES)
               LAB
                (COND ((NULL Q) (RETURN NIL)))
                ((LAMBDA (Q)
                   (PROGN
                    (SETQ L1 (GET Q 'FAC))
                    (COND ((MEMBER H L1) (SETQ L2 (CONS Q L2))))))
                 (CAR Q))
                (SETQ Q (CDR Q))
                (GO LAB))
              (PROG (Q)
                (SETQ Q L2)
               LAB
                (COND ((NULL Q) (RETURN NIL)))
                ((LAMBDA (Q)
                   (PROGN
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2 "Equation ")
                        (PRIN2 Q)
                        (PRIN2
                         " is deleted because it has a vanishing factor.")
                        NIL)
                       (TERPRI))))
                    (SETQ PDES (DROP_PDE Q PDES NIL))))
                 (CAR Q))
                (SETQ Q (CDR Q))
                (GO LAB))
              (SETQ Q (SETQ L1 (SETQ L2 NIL)))
              NIL)))
           (SETQ Q (MKEQSQ SQF HH PF FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL PDES))
           (COND
            (PRINT_
             (COND
              (CONTRADICTION_
               (PROGN
                (PROGN (PRIN2 "The case") NIL)
                (DEPRINT (LIST PF))
                (PROGN
                 (PRIN2
                  "contradicts inequalities and is not further investigated.")
                 NIL)
                NIL))
              (T
               (PROGN
                (PROGN
                 (PRIN2 "CRACK is now called with the assumption 0 = ")
                 (PRIN2 Q)
                 (PRIN2 " : ")
                 NIL)
                (DEPRINT (LIST PF))
                NIL)))))))
         (T
          (PROGN
           (START_LEVEL N (LIST (LIST 'INEQ 0 PF)))
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 "CRACK is now called with assuming  ") NIL)
              (TERPRI)
              (EQPRINT PF)
              (PROGN (PRIN2 " to be nonzero. ") NIL)
              NIL)))
           (PROG (L2)
             (SETQ L2 HH)
            LAB
             (COND ((NULL L2) (RETURN NIL)))
             ((LAMBDA (L2) (ADDSQINEQ PDES L2 NIL)) (CAR L2))
             (SETQ L2 (CDR L2))
             (GO LAB))
           (COND
            (CONTRADICTION_
             (PROGN
              (COND
               (PRINT_
                (PROGN
                 (PRIN2
                  "According to the system of equations, this expression must be zero!")
                 NIL)))
              (SETQ SQF_MUST_BE_ZERO T)))))))
        (COND
         (CONTRADICTION_
          (PROGN
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (COND ((EQUAL N 1) (PROGN (PRIN2 " --> Next case.") NIL))
                    (T (PROGN (PRIN2 " --> Case splitting completed.") NIL)))
              NIL)))
           (SETQ CONTRADICTION_ NIL)
           (SETQ L1 NIL)
           (FINISH_LEVEL 0)))
         (T
          (PROGN
           (SETQ RECYCLE_FCTS NIL)
           (SETQ L1
                   (CRACKMAIN_IF_POSSIBLE_REMOTE
                    (COND ((NULL S) PDES) (T (EQINSERT Q PDES))) FORG)))))
        (SETQ FORG (RESTORE_AND_MERGE L1 PDES FORG))
        (SETQ PDES (CAR FORG))
        (SETQ FORG (CADR FORG))
        (COND ((NOT CONTRADICTION_) (SETQ CONTRAD NIL)))
        (COND
         ((AND L1 (NOT CONTRADICTION_))
          (SETQ RESULT (MERGE_CRACK_RETURNS L1 RESULT))))
        (SETQ CONTRADICTION_ NIL)
        (COND
         ((EQUAL N 1)
          (PROGN (SETQ ENLARGED_DEPL* DEPL*) (SETQ S (NOT S)) (GO AGAIN)))
         (T (SETQ DEPL* (UNION DEPL* ENLARGED_DEPL*))))
        (DELETE_BACKUP)
        (SETQ CONTRADICTION_ CONTRAD)
        (COND (CONTRADICTION_ (SETQ RESULT NIL)))
        (COND
         (PRINT_
          (PROGN
           (TERPRI)
           (PROGN
            (PRIN2
             "This completes the investigation of all cases of a case-distinction.")
            NIL)
           (TERPRI)
           NIL)))
        (RETURN (LIST RESULT)))))) 
(PUT 'STOP_BATCH 'NUMBER-OF-ARGS 1) 
(PUT 'STOP_BATCH 'DEFINED-ON-LINE '2859) 
(PUT 'STOP_BATCH 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'STOP_BATCH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STOP_BATCH (ARGLIST)
    (PROG (S)
      (COND
       (*BATCH_MODE
        (PROGN
         (PROGN
          (PRIN2
           "Drop this point from the proc_list_ with 'o, 'cp or quit with 'q.")
          NIL)
         (TERPRI)
         (SETQ *BATCH_MODE NIL)
         NIL)))
      (SETQ S (LENGTH (CAR ARGLIST)))
      (COND
       ((OR (EQUAL MAX_EQN_TO_CONTI 0)
            (AND (CAR ARGLIST)
                 (GREATERP (LENGTH (CAR ARGLIST)) MAX_EQN_TO_CONTI)))
        (RETURN
         (PROGN
          (COND ((NULL PRINT_) (SETQ PRINT_ 8)))
          (TERPRI)
          (PROGN
           (PRIN2
            "The program changes now into interactive mode because there ")
           NIL)
          (TERPRI)
          (COND ((EQUAL S 1) (PROGN (PRIN2 "is 1 equation") NIL))
                (T (PROGN (PRIN2 "are ") (PRIN2 S) (PRIN2 " equations") NIL)))
          (PROGN (PRIN2 " left to be solved which could not be solved") NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "with the steps preceeding this step 38. If you want to finish the")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "computation in cases that not more than n equations are unsolved")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "then do 'as max_eqn_to_conti n;' where n is an integer.")
           NIL)
          (TERPRI)
          (SETQ BATCHCOUNT_ (DIFFERENCE STEPCOUNTER_ 2))
          ARGLIST)))))) 
(PUT 'USER_DEFINED 'NUMBER-OF-ARGS 1) 
(PUT 'USER_DEFINED 'DEFINED-ON-LINE '2886) 
(PUT 'USER_DEFINED 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'USER_DEFINED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE USER_DEFINED (ARGLIST) (PROG () (SETQ ARGLIST NIL))) 
(PUT 'BACK_UP 'NUMBER-OF-ARGS 1) 
(PUT 'BACK_UP 'DEFINED-ON-LINE '2891) 
(PUT 'BACK_UP 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'BACK_UP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BACK_UP (ARGLIST) (BACKUP_TO_FILE (CAR ARGLIST) (CADR ARGLIST) NIL)) 
(PUT 'SUB_PROBLEM_INPUT 'NUMBER-OF-ARGS 1) 
(PUT 'SUB_PROBLEM_INPUT 'DEFINED-ON-LINE '2894) 
(PUT 'SUB_PROBLEM_INPUT 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SUB_PROBLEM_INPUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUB_PROBLEM_INPUT (PDES)
    (PROG (S H NEWPDES SUB_AFTERWARDS)
      (TERPRI)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN
       (PRIN2
        "Do you want all linear equations to be solved first    --> Enter 1")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "Do you want to specify a set of equations to be solved --> Enter 2")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "or a set of functions (and then all equations ONLY containing")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "these functions are selected)                          --> Enter 3")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "or a set of functions (and then all equations NOT containing")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "these functions are selected)                          --> Enter 4")
       NIL)
      (PROGN
       (PRIN2 "or a set of variables (and then all equations containing")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "or a set of functions (and then all equations containing")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "these functions linearly)                              --> Enter 5")
       NIL)
      (PROGN
       (PRIN2 "or a set of variables (and then all equations containing")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "only derivatives w.r.t. these variables are selected)  --> Enter 6: ")
       NIL)
      (PROG ()
       REPEATLABEL
        (SETQ H (TERMREAD))
        (COND ((NOT (AND (FIXP H) (LEQ 1 H) (LEQ H 5))) (GO REPEATLABEL))))
      (TERPRI)
      (COND
       ((EQUAL H 1)
        (PROGN
         (PROG (S)
           (SETQ S PDES)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND ((GET S 'LINEAR_) (SETQ NEWPDES (CONS S NEWPDES)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ NEWPDES (REVERSE NEWPDES))
         (COND
          ((NULL NEWPDES)
           (PROGN
            (PROGN (PRIN2 "There are no linear equations in the system.") NIL)
            (TERPRI))))))
       ((EQUAL H 2)
        (PROGN
         (PROGN
          (PRIN2 "Specify a subset of equations to be solved in the form:  ")
          NIL)
         (LISTPRINT PDES)
         (PROGN (PRIN2 ";") NIL)
         (TERPRI)
         (SETQ NEWPDES (TERMLISTREAD))
         (COND
          (NEWPDES
           (PROGN
            (SETQ H (SETDIFF NEWPDES PDES))
            (COND
             (H
              (PROGN
               (PROGN
                (PRIN2 "Equations ")
                (PRIN2 H)
                (PRIN2 " are not valid.")
                NIL)
               (TERPRI)
               (SETQ NEWPDES NIL)))))))))
       ((EQUAL H 3)
        (PROGN
         (PROGN
          (PRIN2 "Specify a subset of functions to be solved in the form:  ")
          NIL)
         (LISTPRINT FTEM_)
         (PROGN (PRIN2 ";") NIL)
         (TERPRI)
         (SETQ S (TERMLISTREAD))
         (COND
          (S
           (PROGN
            (SETQ H (SETDIFF S FTEM_))
            (COND
             (H
              (PROGN
               (PROGN
                (PRIN2 "Functions ")
                (PRIN2 H)
                (PRIN2 " are not valid.")
                NIL)
               (TERPRI)))
             (T
              (PROGN
               (PROG (H)
                 (SETQ H PDES)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (COND
                     ((NULL (SETDIFF (GET H 'FCTS) S))
                      (SETQ NEWPDES (CONS H NEWPDES)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (COND
                ((NULL NEWPDES)
                 (PROGN
                  (PROGN
                   (PRIN2
                    "There is no subset of equations containing only these functions.")
                   NIL)
                  (TERPRI)))
                (T (SETQ NEWPDES (REVERSE NEWPDES))))))))))))
       ((EQUAL H 4)
        (PROGN
         (PROGN
          (PRIN2 "Specify a subset of functions to be avoided in the form:  ")
          NIL)
         (LISTPRINT FTEM_)
         (PROGN (PRIN2 ";") NIL)
         (TERPRI)
         (SETQ S (TERMLISTREAD))
         (COND
          (S
           (PROGN
            (SETQ H (SETDIFF S FTEM_))
            (COND
             (H
              (PROGN
               (PROGN
                (PRIN2 "Functions ")
                (PRIN2 H)
                (PRIN2 " are not valid.")
                NIL)
               (TERPRI)
               NIL))
             (T
              (PROGN
               (PROG (H)
                 (SETQ H PDES)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (COND
                     ((FREEOFLIST (GET H 'FCTS) S)
                      (SETQ NEWPDES (CONS H NEWPDES)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (COND
                ((NULL NEWPDES)
                 (PROGN
                  (PROGN
                   (PRIN2
                    "There is no subset of equations not containing these functions.")
                   NIL)
                  (TERPRI)))
                (T (SETQ NEWPDES (REVERSE NEWPDES))))))))))))
       ((EQUAL H 5)
        (PROGN
         (PROGN (PRIN2 "Specify a subset of functions in the form:  ") NIL)
         (LISTPRINT FTEM_)
         (PROGN (PRIN2 ";") NIL)
         (TERPRI)
         (SETQ S (TERMLISTREAD))
         (COND
          (S
           (PROGN
            (SETQ H (SETDIFF S FTEM_))
            (COND
             (H
              (PROGN
               (PROGN
                (PRIN2 "Functions ")
                (PRIN2 H)
                (PRIN2 " are not valid.")
                NIL)
               (TERPRI)))
             (T
              (PROGN
               (PROG (H)
                 (SETQ H PDES)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (COND
                     ((OR (FREEOFLIST (GET H 'FCTS) S)
                          (LIN_CHECK_SQ (GET H 'SQVAL) S))
                      (SETQ NEWPDES (CONS H NEWPDES)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (COND
                ((NULL NEWPDES)
                 (PROGN
                  (PROGN
                   (PRIN2
                    "There is no subset of equations containing only these functions.")
                   NIL)
                  (TERPRI)))
                (T (SETQ NEWPDES (REVERSE NEWPDES))))))))))))
       (T
        (PROGN
         (PROGN
          (PRIN2
           "Specify a subset of variables that may come up in derivatives in the form:  ")
          NIL)
         (LISTPRINT VL_)
         (PROGN (PRIN2 ";") NIL)
         (TERPRI)
         (SETQ S (TERMLISTREAD))
         (SETQ H (SETDIFF S VL_))
         (COND
          (H
           (PROGN
            (PROGN
             (PRIN2 "Variables ")
             (PRIN2 H)
             (PRIN2 " are not variables.")
             NIL)
            (TERPRI)))
          (T
           (PROGN
            (SETQ H (SETDIFF VL_ S))
            (PROG (S)
              (SETQ S PDES)
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S)
                 (COND
                  ((FREEOFLIST (GET S 'DERIVS) H)
                   (SETQ NEWPDES (CONS S NEWPDES)))))
               (CAR S))
              (SETQ S (CDR S))
              (GO LAB))
            (COND
             ((NULL NEWPDES)
              (PROGN
               (PROGN
                (PRIN2
                 "There is no subset of equations containing only derivatives wrt. these variables.")
                NIL)
               (TERPRI)))
             (T (SETQ NEWPDES (REVERSE NEWPDES))))))))))
      (COND
       ((NULL NEWPDES)
        (PROGN (SETQ SUB_AFTERWARDS NIL) (RESTORE_INTERACTIVE_PROMPT)))
       (T
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Do you want an automatic substitution ") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "of computed functions afterwards           (Y/N)? ")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "If not then the subsystem will be replaced") NIL)
         (TERPRI)
         (PROGN (PRIN2 "by its equivalent solutions.") NIL)
         (TERPRI)
         (PROG ()
          REPEATLABEL
           (SETQ S (TERMREAD))
           (COND ((NOT (OR (EQUAL S 'Y) (EQUAL S 'N))) (GO REPEATLABEL))))
         (COND ((EQUAL S 'Y) (SETQ SUB_AFTERWARDS T))
               (T (SETQ SUB_AFTERWARDS NIL)))
         (RESTORE_INTERACTIVE_PROMPT)
         (PROGN
          (PRIN2 "CRACK is now called with the following subset of equations")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 NEWPDES) NIL)
         (TERPRI)
         (PROGN (PRIN2 "!!!!! HINT: ") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "Because the current system to solve is only a subsystem,")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "--> it may have more solutions --> it is harder to solve")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "--> do not try to solve it completely using decoupling")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "--> use CP to change the priority list and use only fast")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "    and safe steps and use 38 as last step.") NIL)
         (TERPRI)
         (PROGN (PRIN2 "--> Example: cp   1,2,3,4,20,7,24,27,38;") NIL)
         (TERPRI)
         (TERPRI)
         NIL)))
      (RETURN (LIST NEWPDES SUB_AFTERWARDS)))) 
(PUT 'SUB_PROBLEM 'NUMBER-OF-ARGS 1) 
(PUT 'SUB_PROBLEM 'DEFINED-ON-LINE '3064) 
(PUT 'SUB_PROBLEM 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'SUB_PROBLEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUB_PROBLEM (ARGLIST)
    (COND (*BATCH_MODE NIL)
          (T
           (PROG (S H FL NONFL NEWPDES SOL PDES FORG F SUB_AFTERWARDS
                  SESSION_BAK LEVEL_BAK LAZY_EVAL_BAK SO SOLUTIONLIST SINGLESOL
                  KEEP_CASE_TREE_BAK SOLNO)
             (SETQ H (SUB_PROBLEM_INPUT (CAR ARGLIST)))
             (SETQ NEWPDES (CAR H))
             (COND ((NULL NEWPDES) (RETURN NIL)))
             (SETQ SUB_AFTERWARDS (CADR H))
             (BACKUP_TO_FILE (CAR ARGLIST) (CADR ARGLIST) NIL)
             (SETQ COLLECT_SOL T)
             (SETQ SESSION_BAK SESSION_)
             (SETQ SESSION_ NIL)
             (SETQ LEVEL_BAK LEVEL_)
             (SETQ LAZY_EVAL_BAK LAZY_EVAL)
             (SETQ LAZY_EVAL NIL)
             (SETQ KEEP_CASE_TREE_BAK KEEP_CASE_TREE)
             (SETQ KEEP_CASE_TREE NIL)
             (PROG (S)
               (SETQ S NEWPDES)
              LAB
               (COND ((NULL S) (RETURN NIL)))
               ((LAMBDA (S) (SETQ FL (UNION FL (GET S 'FCTS)))) (CAR S))
               (SETQ S (CDR S))
               (GO LAB))
             (SETQ FL (SORT_ACCORDING_TO FL FTEM_))
             (SETQ NONFL (SETDIFF FTEM_ FL))
             (COND
              (NONFL
               (PROGN
                (SETQ H NIL)
                (PROG (S)
                  (SETQ S INEQ_)
                 LAB
                  (COND ((NULL S) (RETURN NIL)))
                  ((LAMBDA (S) (COND ((FREEOF S NONFL) (SETQ H (CONS S H)))))
                   (CAR S))
                  (SETQ S (CDR S))
                  (GO LAB))
                (SETQ INEQ_ H)
                (SETQ H NIL)
                (PROG (S)
                  (SETQ S INEQ_OR)
                 LAB
                  (COND ((NULL S) (RETURN NIL)))
                  ((LAMBDA (S) (COND ((FREEOF S NONFL) (SETQ H (CONS S H)))))
                   (CAR S))
                  (SETQ S (CDR S))
                  (GO LAB))
                (SETQ INEQ_OR H)
                NIL)))
             (SETQ FTEM_ FL)
             (SETQ SOL (CRACKMAIN NEWPDES FL))
             (SETQ H NIL)
             (PROG (SO)
               (SETQ SO SOL)
              LAB
               (COND ((NULL SO) (RETURN NIL)))
               ((LAMBDA (SO) (COND ((PAIRP SO) (SETQ H (CONS SO H)))))
                (CAR SO))
               (SETQ SO (CDR SO))
               (GO LAB))
             (SETQ SOL H)
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN (PRIN2 "----------------------") NIL)
                (TERPRI)
                (PROGN
                 (PRIN2 "The solution of the sub-system is completed.")
                 NIL)
                (TERPRI)
                (PROGN (PRIN2 "The subsystem has ") NIL)
                (COND ((NULL SOL) (PROGN (PRIN2 "no solution.") NIL))
                      ((CDR SOL)
                       (PROGN (PRIN2 (LENGTH SOL)) (PRIN2 " solutions.") NIL))
                      (T (PROGN (PRIN2 "1 solution.") NIL)))
                (TERPRI)
                (PROGN (PRIN2 "----------------------") NIL)
                (TERPRI)
                NIL)))
             (SETQ BATCHCOUNT_ (SUB1 STEPCOUNTER_))
             (SETQ SESSION_ SESSION_BAK)
             (SETQ LEVEL_ LEVEL_BAK)
             (SETQ LAZY_EVAL LAZY_EVAL_BAK)
             (COND
              ((NULL SOL)
               (RETURN
                (PROGN
                 (SETQ KEEP_CASE_TREE KEEP_CASE_TREE_BAK)
                 (SETQ CONTRADICTION_ T)
                 NIL)))
              (T (SETQ SINGLESOL (NULL (CDR SOL)))))
             (SETQ SOLNO 0)
             (PROG (SO)
               (SETQ SO SOL)
              LAB
               (COND ((NULL SO) (RETURN NIL)))
               ((LAMBDA (SO)
                  (PROGN
                   (SETQ H (RESTORE_AND_MERGE (LIST SO) PDES FORG))
                   (SETQ PDES (CAR H))
                   (SETQ FORG (CADR H))
                   (SETQ H NIL)
                   (SETQ KEEP_CASE_TREE NIL)
                   (COND
                    ((NULL SUB_AFTERWARDS)
                     (PROG (H)
                       (SETQ H NEWPDES)
                      LAB
                       (COND ((NULL H) (RETURN NIL)))
                       ((LAMBDA (H) (SETQ PDES (DROP_PDE H PDES NIL))) (CAR H))
                       (SETQ H (CDR H))
                       (GO LAB))))
                   (PROG (F)
                     (SETQ F (CADDR SO))
                    LAB
                     (COND ((NULL F) (RETURN NIL)))
                     ((LAMBDA (F) (PROGN (SETQ FTEM_ (FCTINSERT F FTEM_)) NIL))
                      (CAR F))
                     (SETQ F (CDR F))
                     (GO LAB))
                   (PROG (S)
                     (SETQ S (CAR SO))
                    LAB
                     (COND ((NULL S) (RETURN NIL)))
                     ((LAMBDA (S)
                        (COND
                         ((NULL CONTRADICTION_)
                          (SETQ PDES
                                  (EQINSERT
                                   (MKEQSQ S NIL NIL FTEM_ VL_ ALLFLAGS_ T
                                    (LIST 0) NIL PDES)
                                   PDES)))))
                      (CAR S))
                     (SETQ S (CDR S))
                     (GO LAB))
                   (SETQ INEQ_ (UNION (CADDDR SO) INEQ_))
                   (SETQ INEQ_OR (UNION (CAR (CDDDDR SO)) INEQ_OR))
                   (PROG (S)
                     (SETQ S (CADR SO))
                    LAB
                     (COND ((NULL S) (RETURN NIL)))
                     ((LAMBDA (S)
                        (COND
                         ((NULL CONTRADICTION_)
                          (COND
                           ((AND (PAIRP S) (EQUAL (CAR S) 'EQUAL))
                            (PROGN
                             (SETQ H
                                     (MKEQSQ
                                      (ADDSQ (CADDR S) (NEGSQ (SIMP (CADR S))))
                                      NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0)
                                      NIL PDES))
                             (SETQ PDES (EQINSERT H PDES))
                             (COND
                              (SUB_AFTERWARDS
                               (PROGN
                                (SETQ TO_DO_LIST
                                        (CONS
                                         (LIST 'SUBST_LEVEL_35 (LIST H)
                                               (CADR S))
                                         TO_DO_LIST))
                                (COND
                                 ((NOT (FREEOFLIST (CDR (CADDR S)) FTEM_))
                                  (SETQ INEQ_
                                          (UNION
                                           (LIST (CONS (CDR (CADDR S)) 1))
                                           INEQ_))))
                                NIL)))))))))
                      (CAR S))
                     (SETQ S (CDR S))
                     (GO LAB))
                   (COND
                    ((NULL SINGLESOL)
                     (COND (CONTRADICTION_ (SETQ CONTRADICTION_ NIL))
                           (T
                            (PROGN
                             (SETQ SOLNO (PLUS SOLNO 1))
                             (COND
                              (PRINT_
                               (PROGN
                                (TERPRI)
                                (PROGN (PRIN2 "----------------------") NIL)
                                (TERPRI)
                                (PROGN
                                 (PRIN2 "The ")
                                 (PRIN2 SOLNO)
                                 (PRIN2
                                  ". solution of the sub-system has been substituted into ")
                                 (PRIN2
                                  "the original system and is to be investigated further in the ")
                                 (PRIN2 "following sub-case computation.")
                                 NIL)
                                (TERPRI)
                                (PROGN (PRIN2 "----------------------") NIL)
                                (TERPRI)
                                NIL)))
                             (START_LEVEL SOLNO NIL)
                             (SETQ H
                                     (CRACKMAIN_IF_POSSIBLE_REMOTE PDES
                                      (CADR ARGLIST)))
                             (COND (CONTRADICTION_ (SETQ CONTRADICTION_ NIL))
                                   (T
                                    (SETQ SOLUTIONLIST
                                            (MERGE_CRACK_RETURNS H
                                             SOLUTIONLIST)))))))))))
                (CAR SO))
               (SETQ SO (CDR SO))
               (GO LAB))
             (SETQ KEEP_CASE_TREE KEEP_CASE_TREE_BAK)
             (RETURN
              (COND (SINGLESOL (LIST PDES (CADR ARGLIST)))
                    (T (LIST SOLUTIONLIST)))))))) 
(PUT 'FIRST_INT_FOR_ODE 'NUMBER-OF-ARGS 1) 
(PUT 'FIRST_INT_FOR_ODE 'DEFINED-ON-LINE '3216) 
(PUT 'FIRST_INT_FOR_ODE 'DEFINED-IN-FILE 'CRACK/CRMAIN.RED) 
(PUT 'FIRST_INT_FOR_ODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIRST_INT_FOR_ODE (ARGLIST)
    (PROG ()
      (SETQ ARGLIST (CDR ARGLIST))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2
           "Unfortunately this module is not completely implemented yet.")
          NIL)
         (TERPRI))))
      (RETURN NIL))) 
(ENDMODULE) 