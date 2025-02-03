(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'POLY_GB)) 
(PUT 'FIND_SINGULAR 'NUMBER-OF-ARGS 0) 
(PUT 'FIND_SINGULAR 'DEFINED-ON-LINE '32) 
(PUT 'FIND_SINGULAR 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'FIND_SINGULAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIND_SINGULAR NIL
    (PROG (H1)
      (COND
       ((NULL SINGULAR_CALL)
        (PROGN
         (SETQ SINGULAR_CALL (GETENV "SingularCall"))
         (COND ((NOT (FILEP SINGULAR_CALL)) (SETQ SINGULAR_CALL NIL)))
         NIL)))
      (COND
       ((NULL SINGULAR_CALL)
        (COND
         ((NULL (FILEP (SETQ SINGULAR_CALL "/usr/bin/Singular")))
          (COND
           ((NULL
             (FILEP
              (SETQ SINGULAR_CALL
                      "/home/syam/Singular/3-0-4/x86_64-Linux/Singular")))
            (COND
             ((NULL
               (FILEP
                (SETQ SINGULAR_CALL
                        "/usr/local/share/Singular/3-1-6/x86_64-Linux/Singular")))
              (COND
               ((NULL
                 (FILEP
                  (SETQ SINGULAR_CALL "/usr/local/Singular4/bin/Singular")))
                (COND
                 ((NULL (FILEP (SETQ SINGULAR_CALL "/usr/local/bin/Singular")))
                  (SETQ SINGULAR_CALL NIL)))))))))))))
      (COND
       (SINGULAR_CALL
        (PROGN
         (COND
          ((OR PRINT_ (NULL OLD_HISTORY))
           (PROGN
            (PROGN
             (PRIN2 "Singular will be called by ")
             (PRIN2 SINGULAR_CALL)
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "If this is not the correct call then input the command: ")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "as  singular_call  \"path/Singular\"") NIL)
            (TERPRI)
            (TERPRI))))))
       (T
        (PROGN
         (COND
          ((OR PRINT_ (NULL OLD_HISTORY))
           (PROGN
            (PROGN
             (PRIN2
              "For calling Singular the program needs to know the calling command.")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "The command 'which Singular' gives:") NIL)
            (TERPRI)
            (SYSTEM "which Singular")
            (PROGN
             (PRIN2
              "Please input this call if it is valid or another working call, like \"/usr/bin/Singular\": ")
             NIL)
            (TERPRI)
            NIL)))
         (CHANGE_PROMPT_TO "")
         (SETQ H1 (TERMREAD))
         (RESTORE_INTERACTIVE_PROMPT)
         (SETQ SINGULAR_CALL (BLDMSG_INTERNAL "%w" (LIST H1)))
         (RESTORE_INTERACTIVE_PROMPT)
         NIL)))
      (COND
       ((NULL SINGULAR_LIB)
        (PROGN
         (SETQ SINGULAR_LIB (GETENV "SingularLib"))
         (COND ((NOT (FILEP SINGULAR_LIB)) (SETQ SINGULAR_LIB NIL)))
         NIL)))
      (COND
       ((NULL SINGULAR_LIB)
        (COND
         ((NULL (FILEP (SETQ SINGULAR_LIB "/usr/share/Singular/LIB")))
          (COND
           ((NULL (FILEP (SETQ SINGULAR_LIB "/usr/share/singular/LIB")))
            (COND
             ((NULL
               (FILEP (SETQ SINGULAR_LIB "/home/syam/Singular/3-0-4/LIB")))
              (COND
               ((NULL
                 (FILEP
                  (SETQ SINGULAR_LIB "/usr/local/share/Singular/3-1-6/LIB")))
                (COND
                 ((NULL
                   (FILEP
                    (SETQ SINGULAR_LIB
                            "/usr/local/Singular4/share/singular/LIB")))
                  (COND
                   ((NULL
                     (FILEP
                      (SETQ SINGULAR_LIB "/usr/local/share/singular/LIB")))
                    (SETQ SINGULAR_LIB NIL)))))))))))))))
      (COND
       (SINGULAR_LIB
        (PROGN
         (COND
          ((OR PRINT_ (NULL OLD_HISTORY))
           (PROGN
            (PROGN
             (PRIN2 "The Singular library is assumed to be in ")
             (PRIN2 SINGULAR_LIB)
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "If this is not the correct path then input the command: ")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "as  singular_lib  \"path/LIB\"") NIL)
            (TERPRI)
            (TERPRI))))))
       (T
        (PROGN
         (COND
          ((OR PRINT_ (NULL OLD_HISTORY))
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2
              "For Singular to access its libraries it needs to know the library address.")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "Please input the library directory with its path, e.g.")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "If you do not know it, then input \"\".") NIL)
            (TERPRI)
            NIL)))
         (CHANGE_PROMPT_TO "")
         (SETQ H1 (TERMREAD))
         (RESTORE_INTERACTIVE_PROMPT)
         (SETQ SINGULAR_LIB (BLDMSG_INTERNAL "%w" (LIST H1)))
         NIL))))) 
(PUT 'COMP_GROEBNER_BASIS 'NUMBER-OF-ARGS 1) 
(PUT 'COMP_GROEBNER_BASIS 'DEFINED-ON-LINE '109) 
(PUT 'COMP_GROEBNER_BASIS 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'COMP_GROEBNER_BASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMP_GROEBNER_BASIS (ARGLIST)
    (PROG (PDES FORG SOL N RESULT L1 H1 H2 PSYS KEPT_PDES USE RATIONAL_BAK
           RESIMPLI RAT_INEQ)
      (SETQ PDES (CAR ARGLIST))
      (SETQ H1 (CAR ARGLIST))
      (COND
       ((NEQ (CAR ARGLIST) (CADDDR ARGLIST))
        (PROGN (SETQ KEPT_PDES H1) (SETQ H1 (CADDDR ARGLIST)) NIL)))
      (PROG (P)
        (SETQ P H1)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (COND ((GET P 'NON_RAT_KERN) (SETQ USE NIL))
                  ((OR (NULL VL_) (EQUAL GROEB_SOLVE 'DIFFELIM)) (SETQ USE T))
                  (T
                   (PROGN
                    (SETQ H2 (GET P 'KERN))
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (AND H2 (NOT (PAIRP (CAR H2))))) (RETURN NIL)))
                      (SETQ H2 (CDR H2))
                      (GO WHILELABEL))
                    (COND ((NULL H2) (SETQ USE T)) (T (SETQ USE NIL))))))
            (COND (USE (SETQ PSYS (CONS P PSYS)))
                  (T
                   (PROGN
                    (SETQ KEPT_PDES (UNION (LIST P) KEPT_PDES))
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN (PRIN2 "Equation ") (PRIN2 P) (PRIN2 " is ") NIL)
                       (COND
                        ((GET P 'NON_RAT_KERN)
                         (PROGN (PRIN2 "non-polynomial") NIL))
                        (T (PROGN (PRIN2 "differential") NIL)))
                       (PROGN
                        (PRIN2 " and ignored in calling the external package.")
                        NIL)
                       (TERPRI)))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ SOL
              (COND
               ((EQUAL GROEB_SOLVE 'DIFFELIM)
                (PROGN
                 (SETQ RATIONAL_BAK *RATIONAL)
                 (COND
                  (*RATIONAL
                   (PROGN (SETQ RESIMPLI T) (AEVAL (OFF (LIST 'RATIONAL))))))
                 (SETQ RESIMPLI T)
                 (PROG (H1)
                   (SETQ H1 INEQ_)
                  LAB
                   (COND ((NULL H1) (RETURN NIL)))
                   ((LAMBDA (H1)
                      (PROGN
                       (SETQ H2 (UNION (KERNELS (CDR H1)) (KERNELS (CAR H1))))
                       (PROG ()
                        WHILELABEL
                         (COND
                          ((NOT
                            (AND H2
                                 (OR (ATOM (CAR H2))
                                     (AND (PAIRP (CAR H2))
                                          (EQUAL (CAR (CAR H2)) 'DF)
                                          (ATOM (CADR (CAR H2)))))))
                           (RETURN NIL)))
                         (SETQ H2 (CDR H2))
                         (GO WHILELABEL))
                       (COND ((NULL H2) (SETQ RAT_INEQ (CONS H1 RAT_INEQ))))))
                    (CAR H1))
                   (SETQ H1 (CDR H1))
                   (GO LAB))
                 (SETQ H1
                         (CALL_DIFFELIM
                          (PROG (P FORALL-RESULT FORALL-ENDPTR)
                            (SETQ P PSYS)
                            (COND ((NULL P) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (P)
                                                (COND
                                                 (RESIMPLI
                                                  (CONS
                                                   (CAR
                                                    (RESIMP (GET P 'SQVAL)))
                                                   1))
                                                 (T (GET P 'SQVAL))))
                                              (CAR P))
                                             NIL)))
                           LOOPLABEL
                            (SETQ P (CDR P))
                            (COND ((NULL P) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (P)
                                        (COND
                                         (RESIMPLI
                                          (CONS (CAR (RESIMP (GET P 'SQVAL)))
                                                1))
                                         (T (GET P 'SQVAL))))
                                      (CAR P))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          (PROGN
                           (PROG (P)
                             (SETQ P PSYS)
                            LAB
                             (COND ((NULL P) (RETURN NIL)))
                             ((LAMBDA (P) (SETQ L1 (UNION (GET P 'FCTS) L1)))
                              (CAR P))
                             (SETQ P (CDR P))
                             (GO LAB))
                           (SETQ L1 (SORT_ACCORDING_TO L1 FTEM_))
                           (SETQ N (FCTLENGTH (CAR L1)))
                           (SETQ H1 NIL)
                           (PROG ()
                            WHILELABEL
                             (COND ((NOT L1) (RETURN NIL)))
                             (PROGN
                              (SETQ H2 NIL)
                              (PROG ()
                               WHILELABEL
                                (COND
                                 ((NOT (AND L1 (EQUAL (FCTLENGTH (CAR L1)) N)))
                                  (RETURN NIL)))
                                (PROGN
                                 (SETQ H2 (CONS (CAR L1) H2))
                                 (SETQ L1 (CDR L1)))
                                (GO WHILELABEL))
                              (COND (L1 (SETQ N (FCTLENGTH (CAR L1)))))
                              (SETQ H1 (CONS (REVERSE H2) H1)))
                             (GO WHILELABEL))
                           (REVERSE H1))
                          RAT_INEQ DIFFELIM_STEPS))
                 (COND
                  ((NEQ *RATIONAL RATIONAL_BAK)
                   (COND ((NULL RATIONAL_BAK) (AEVAL (OFF (LIST 'RATIONAL))))
                         (T (AEVAL (ON (LIST 'RATIONAL)))))))
                 (COND
                  ((AND (NULL (NTH H1 4)) (NULL (NTH H1 5)) (ZEROP (NTH H1 6)))
                   (COND
                    (PRINT_
                     (PROGN
                      (PROGN
                       (PRIN2 "The DiffElim computation is complete.")
                       NIL)
                      (TERPRI)))
                    (T NIL)))
                  (T
                   (PROGN
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2 "The DiffElim computation is not complete.")
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2
                         "One could increase diffelim_steps with the interactive")
                        NIL)
                       (TERPRI)
                       (PROGN
                        (PRIN2 "'as' command. Currently diffelim_steps = ")
                        (PRIN2 DIFFELIM_STEPS)
                        NIL)
                       (TERPRI))))
                    (SETQ KEPT_PDES (CAR ARGLIST)))))
                 (PROG (H2)
                   (SETQ H2 (CADDR H1))
                  LAB
                   (COND ((NULL H2) (RETURN NIL)))
                   ((LAMBDA (H2) (ADDSQINEQ PDES H2 T)) (CAR H2))
                   (SETQ H2 (CDR H2))
                   (GO LAB))
                 (SETQ H1
                         (LIST 'LIST
                               (CONS 'LIST
                                     (NCONC (CAR H1)
                                            (NCONC (CADR H1)
                                                   (NCONC (CADDDR H1)
                                                          (CAR
                                                           (CDDDDR H1))))))))
                 H1))
               ((OR (EQUAL GROEB_SOLVE 'SL_LEX) (EQUAL GROEB_SOLVE 'SL_GRAD)
                    (EQUAL GROEB_SOLVE 'SL_REVGRAD))
                (PROGN
                 (FIND_SINGULAR)
                 (COND ((OR (NULL SINGULAR_CALL) (NULL SINGULAR_LIB)) NIL)
                       ((EQUAL GROEB_SOLVE 'SL_LEX)
                        (CALL_SING FTEM_ PSYS 'LEX))
                       ((EQUAL GROEB_SOLVE 'SL_GRAD)
                        (CALL_SING FTEM_ PSYS 'GRADLEX))
                       ((EQUAL GROEB_SOLVE 'SL_REVGRAD)
                        (CALL_SING FTEM_ PSYS 'REVGRADLEX)))))
               ((EQUAL GROEB_SOLVE 'GB_LEX)
                (AEVAL
                 (LIST 'LIST
                       (LIST 'CALL_GB (CONS 'LIST FTEM_)
                             (CONS 'LIST
                                   (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ P PSYS)
                                     (COND ((NULL P) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (P)
                                                         (REVAL1
                                                          (LIST '*SQ
                                                                (GET P 'SQVAL)
                                                                T)
                                                          T))
                                                       (CAR P))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ P (CDR P))
                                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P)
                                                 (REVAL1
                                                  (LIST '*SQ (GET P 'SQVAL) T)
                                                  T))
                                               (CAR P))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                             'LEX))))
               ((EQUAL GROEB_SOLVE 'GB_REVGRAD)
                (AEVAL
                 (LIST 'LIST
                       (LIST 'CALL_GB (CONS 'LIST FTEM_)
                             (CONS 'LIST
                                   (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ P PSYS)
                                     (COND ((NULL P) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (P)
                                                         (REVAL1
                                                          (LIST '*SQ
                                                                (GET P 'SQVAL)
                                                                T)
                                                          T))
                                                       (CAR P))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ P (CDR P))
                                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P)
                                                 (REVAL1
                                                  (LIST '*SQ (GET P 'SQVAL) T)
                                                  T))
                                               (CAR P))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                             'REVGRADLEX))))
               (T
                (PROGN
                 (PROGN (PRIN2 "Situation before call of Groebner:") NIL)
                 (TERPRI)
                 (PRINT_STATISTIC PDES
                  (APPEND (CADR ARGLIST) (SETDIFF FTEM_ (CADR ARGLIST))))
                 (ERR_CATCH_GB PSYS)
                 NIL))))
      (COND
       (PRINT_
        (PROGN
         (TERPRI)
         (COND
          ((EQUAL GROEB_SOLVE 'DIFFELIM)
           (PROGN (PRIN2 "A differential Groebner basis computation ") NIL))
          (T
           (PROGN (PRIN2 "An algebraic Groebner basis computation ") NIL))))))
      (RETURN
       (COND
        ((OR (NULL SOL) (EQUAL SOL (LIST 'LIST NIL)))
         (PROGN
          (COND
           (PRINT_ (PROGN (PROGN (PRIN2 "was not successful.") NIL) (TERPRI))))
          NIL))
        ((OR (EQUAL SOL (LIST 'LIST (LIST 'LIST 1)))
             (EQUAL SOL (LIST 'LIST (LIST 'LIST (CONS 1 1)))))
         (PROGN
          (COND (PRINT_ (PROGN (PRIN2 "yields a contradiction.") NIL)))
          (SETQ CONTRADICTION_ T)
          NIL))
        (T
         (PROGN
          (COND
           ((NULL KEPT_PDES) (PROGN (DROP_ALL_PDES PDES) (SETQ PDES NIL))))
          (SETQ SOL (CDR SOL))
          (COND
           ((NULL (CDR SOL))
            (PROGN
             (SETQ SOL (CDAR SOL))
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN (PRIN2 "yields a single new system of conditions.") NIL)
                (TERPRI)
                (COND
                 ((NULL KEPT_PDES)
                  (PROGN (PRIN2 "All previous equations are dropped.") NIL))
                 (T (PROGN (PRIN2 "New equations are added.") NIL)))
                (TERPRI)
                (PROGN (PRIN2 "The new equations are:") NIL)
                NIL)))
             (COND
              ((EQUAL GROEB_SOLVE 'DIFFELIM)
               (SETQ PDES
                       (MKEQSQLIST SOL NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0)
                        NIL)))
              (T
               (SETQ PDES
                       (MKEQSQLIST NIL NIL SOL FTEM_ VL_ ALLFLAGS_ T (LIST 0)
                        NIL))))
             (COND
              ((AND FLIN_ (NULL LIN_PROBLEM))
               (PROGN
                (PROG (P)
                  (SETQ P PDES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (COND
                      ((NULL (GET P 'LINEAR_))
                       (PROG (H1)
                         (SETQ H1 (GET P 'FCTS))
                        LAB
                         (COND ((NULL H1) (RETURN NIL)))
                         ((LAMBDA (H1)
                            (COND
                             ((AND (MEMBER H1 FLIN_)
                                   (OR
                                    (NULL
                                     (LIN_CHECK_SQ
                                      (CONS
                                       (FIRST_TERM_SF (CAR (GET P 'SQVAL))) 1)
                                      (LIST H1)))
                                    (NULL
                                     (LIN_CHECK_SQ (GET P 'SQVAL) (LIST H1)))))
                              (SETQ FLIN_ (DELETE H1 FLIN_)))))
                          (CAR H1))
                         (SETQ H1 (CDR H1))
                         (GO LAB)))))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB)))))
             (PROG (P)
               (SETQ P KEPT_PDES)
              LAB
               (COND ((NULL P) (RETURN NIL)))
               ((LAMBDA (P) (SETQ PDES (EQINSERT P PDES))) (CAR P))
               (SETQ P (CDR P))
               (GO LAB))
             (LISTPRINT PDES)
             (COND (CONTRADICTION_ NIL) (T (LIST PDES (CADR ARGLIST))))))
           (T
            (PROGN
             (COND
              (PRINT_
               (PROGN
                (TERPRI)
                (PROGN
                 (PRIN2 (LENGTH SOL))
                 (PRIN2 "yields cases. All previous equations are dropped.")
                 NIL)
                NIL)))
             (SETQ N 0)
             (SETQ FORG (CADR ARGLIST))
             (BACKUP_TO_FILE PDES FORG NIL)
             (PROG ()
              WHILELABEL
               (COND ((NOT SOL) (RETURN NIL)))
               (PROGN
                (SETQ N (PLUS N 1))
                (START_LEVEL N
                 (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ L1 (CDAR SOL))
                   (COND ((NULL L1) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (L1)
                                       (LIST 'EQUAL 0 (REVAL1 L1 T)))
                                     (CAR L1))
                                    NIL)))
                  LOOPLABEL
                   (SETQ L1 (CDR L1))
                   (COND ((NULL L1) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (L1) (LIST 'EQUAL 0 (REVAL1 L1 T)))
                             (CAR L1))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
                (COND
                 (PRINT_
                  (PROGN
                   (TERPRI)
                   (PROGN
                    (PRIN2 "CRACK is now called with a case resulting ")
                    NIL)
                   (TERPRI)
                   (PROGN
                    (PRIN2 "from a Groebner Basis computation : ")
                    NIL))))
                (SETQ RECYCLE_FCTS NIL)
                (COND
                 ((EQUAL GROEB_SOLVE 'DIFFELIM)
                  (SETQ PDES
                          (MKEQSQLIST NIL NIL (CDAR SOL) FTEM_ VL_ ALLFLAGS_ T
                           (LIST 0) NIL)))
                 (T
                  (SETQ PDES
                          (MKEQSQLIST NIL NIL (CDAR SOL) FTEM_ VL_ ALLFLAGS_ T
                           (LIST 0) NIL))))
                (COND
                 ((AND FLIN_ (NULL LIN_PROBLEM))
                  (PROGN
                   (PROG (P)
                     (SETQ P PDES)
                    LAB
                     (COND ((NULL P) (RETURN NIL)))
                     ((LAMBDA (P)
                        (COND
                         ((NULL (GET P 'LINEAR_))
                          (PROG (H1)
                            (SETQ H1 (GET P 'FCTS))
                           LAB
                            (COND ((NULL H1) (RETURN NIL)))
                            ((LAMBDA (H1)
                               (COND
                                ((AND (MEMBER H1 FLIN_)
                                      (OR
                                       (NULL
                                        (LIN_CHECK_SQ
                                         (CONS
                                          (FIRST_TERM_SF (CAR (GET P 'SQVAL)))
                                          1)
                                         (LIST H1)))
                                       (NULL
                                        (LIN_CHECK_SQ (GET P 'SQVAL)
                                         (LIST H1)))))
                                 (SETQ FLIN_ (DELETE H1 FLIN_)))))
                             (CAR H1))
                            (SETQ H1 (CDR H1))
                            (GO LAB)))))
                      (CAR P))
                     (SETQ P (CDR P))
                     (GO LAB)))))
                (SETQ SOL (CDR SOL))
                (SETQ L1 (CRACKMAIN_IF_POSSIBLE_REMOTE PDES FORG))
                (COND
                 ((AND L1 (NOT CONTRADICTION_))
                  (SETQ RESULT (MERGE_CRACK_RETURNS L1 RESULT))))
                (SETQ CONTRADICTION_ NIL)
                (COND
                 (SOL
                  (PROGN
                   (SETQ L1 (RESTORE_BACKUP_FROM_FILE PDES FORG NIL))
                   (SETQ PDES (CAR L1))
                   (SETQ FORG (CADR L1))
                   NIL))))
               (GO WHILELABEL))
             (DELETE_BACKUP)
             (LIST RESULT)))))))))) 
(ENDMODULE) 
(MODULE (LIST 'FAUGERE)) 
(FLUID '(*GB_FILEOUT *GB_FILEIN)) 
(PUT 'CALL_GB 'NUMBER-OF-ARGS 3) 
(FLAG '(CALL_GB) 'OPFN) 
(PUT 'CALL_GB 'DEFINED-ON-LINE '372) 
(PUT 'CALL_GB 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'CALL_GB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALL_GB (VARS POLYS ODER)
    (PROG (LL XX CHNOUT FILEIN FILEOUT OFL*BAK)
      (PROGN
       (SETQ FILEIN
               (OR *GB_FILEIN
                   (COND
                    ((FILEP "/tmp")
                     (BLDMSG_INTERNAL "%w%w"
                                      (LIST "/tmp/gb_a1_"
                                            (LEVEL_STRING SESSION_))))
                    (T
                     (BLDMSG_INTERNAL "%w%w"
                                      (LIST "%temp%\\gb_a1_"
                                            (LEVEL_STRING SESSION_)))))))
       (SETQ FILEOUT
               (OR *GB_FILEOUT
                   (COND
                    ((FILEP "/tmp")
                     (BLDMSG_INTERNAL "%w%w"
                                      (LIST "/tmp/gb_a2_"
                                            (LEVEL_STRING SESSION_))))
                    (T
                     (BLDMSG_INTERNAL "%w%w"
                                      (LIST "%temp%\\gb_a2_"
                                            (LEVEL_STRING SESSION_)))))))
       (COND
        ((OR (NOT (LISTP VARS)) (NOT (LISTP POLYS)))
         (RETURN (REDERR "Vars and Polys must be lists"))))
       (SETQ CHNOUT (OPEN FILEIN 'OUTPUT))
       (SETQ OFL*BAK OFL*)
       (SETQ OFL* FILEIN)
       (SETQ CHNOUT (WRS CHNOUT)))
      (SETQ LL (AEVAL (LIST 'LINELENGTH 10000000)))
      (AEVAL (LIST 'GB_VARS VARS))
      (AEVAL (LIST 'GB_POLYS POLYS))
      (PROGN (SETQ OFL* OFL*BAK) (CLOSE (WRS CHNOUT)))
      (AEVAL (LIST 'LINELENGTH LL))
      (COND
       ((EVALEQUAL (AEVAL ODER) (AEVAL 'LEX))
        (SETQ XX
                (SYSTEM
                 (BLDMSG_INTERNAL
                  "/home/neun/gb/serveur__DMP__Lexp__INT -read %w > %w"
                  (LIST FILEIN FILEOUT)))))
       ((EVALEQUAL (AEVAL ODER) (AEVAL 'REVGRADLEX))
        (SETQ XX
                (SYSTEM
                 (BLDMSG_INTERNAL
                  "/home/neun/gb/serveur__DMP__Dexp__INT -read %w > %w"
                  (LIST FILEIN FILEOUT)))))
       (T (AEVAL (REDERR "Unknown Order in call_gb"))))
      (RETURN
       (COND ((EVALEQUAL (AEVAL XX) 0) (AEVAL (LIST 'READ_BG_OUTPUT FILEOUT)))
             (T (AEVAL (REDERR (REVALX XX)))))))) 
(PUT 'READ_BG_OUTPUT 'NUMBER-OF-ARGS 1) 
(FLAG '(READ_BG_OUTPUT) 'OPFN) 
(PUT 'READ_BG_OUTPUT 'DEFINED-ON-LINE '417) 
(PUT 'READ_BG_OUTPUT 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'READ_BG_OUTPUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE READ_BG_OUTPUT (FILE)
    (PROG (XX CHN CHNOUT FILENAM *ECHO OFL*BAK)
      (PROGN
       (SETQ FILENAM
               (COND
                ((FILEP "/tmp")
                 (BLDMSG_INTERNAL "%w%w"
                                  (LIST "/tmp/gb_hihihi_"
                                        (LEVEL_STRING SESSION_))))
                (T
                 (BLDMSG_INTERNAL "%w%w"
                                  (LIST "%tTEMP%\\gb_hihihi_"
                                        (LEVEL_STRING SESSION_))))))
       (OFF (LIST 'ECHO))
       (SETQ CHN (OPEN FILE 'INPUT))
       (SETQ CHN (RDS CHN))
       (PROG ()
        WHILELABEL
         (COND ((NOT (NEQ (SETQ XX (READ)) '|OUTPUT:|)) (RETURN NIL)))
        NIL
         (GO WHILELABEL))
       (READCH)
       (READCH)
       (SETQ CHNOUT (OPEN FILENAM 'OUTPUT))
       (SETQ OFL*BAK OFL*)
       (SETQ OFL* FILENAM)
       (SETQ CHNOUT (WRS CHNOUT))
       (PRIN2 "algebraic << gb_result :={")
       (TERPRI)
       (PROG ()
        WHILELABEL
         (COND ((NOT (NEQ (SETQ XX (READCH)) '])) (RETURN NIL)))
         (PROGN (WRITECHAR XX))
         (GO WHILELABEL))
       (PRIN2 "} >> $ END;")
       (TERPRI)
       (SETQ OFL* OFL*BAK)
       (CLOSE (WRS CHNOUT))
       (CLOSE (RDS CHN))
       NIL)
      (PROGN (SETQ *UNCACHED T) (INFILE FILENAM))
      (RETURN (AEVAL 'GB_RESULT)))) 
(PUT 'GB_VARS 'NUMBER-OF-ARGS 1) 
(FLAG '(GB_VARS) 'OPFN) 
(PUT 'GB_VARS 'DEFINED-ON-LINE '444) 
(PUT 'GB_VARS 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'GB_VARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GB_VARS (LI)
    (PROGN
     (SETQ LI
             (PROG (LL FORALL-RESULT FORALL-ENDPTR)
               (SETQ LL LI)
              STARTOVER
               (COND ((NULL LL) (RETURN NIL)))
               (SETQ FORALL-RESULT ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ LL (CDR LL))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL LL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ LL (CDR LL))
               (GO LOOPLABEL)))
     (MYPRIN2L (CONS "#vars [" (CDDDR LI)) " ")
     (PRIN2 "]")
     (TERPRI))) 
(PUT 'GB_POLYS 'NUMBER-OF-ARGS 1) 
(FLAG '(GB_POLYS) 'OPFN) 
(PUT 'GB_POLYS 'DEFINED-ON-LINE '451) 
(PUT 'GB_POLYS 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'GB_POLYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GB_POLYS (LI)
    (PROG (*NAT)
      (PROGN
       (SETQ LI
               (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ LL LI)
                STARTOVER
                 (COND ((NULL LL) (RETURN NIL)))
                 (SETQ FORALL-RESULT ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ LL (CDR LL))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL LL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ LL (CDR LL))
                 (GO LOOPLABEL)))
       (PRIN2 "#list [")
       (PROG (LL)
         (SETQ LL (CDDDR LI))
        LAB
         (COND ((NULL LL) (RETURN NIL)))
         ((LAMBDA (LL) (PROGN (MAPRIN LL) (TERPRI))) (CAR LL))
         (SETQ LL (CDR LL))
         (GO LAB))
       (PRIN2 "]")
       (TERPRI))
      (RETURN (AEVAL 'NIL)))) 
(ENDMODULE) 
(MODULE (LIST 'SINGULAR)) 
(FLUID '(*SINGULAR_FILEOUT *SINGULAR_FILEIN SUBLIS_LIST)) 
(PUT 'CALL_SINGULAR 'PSOPFN 'CALL_SING) 
(PUT 'CALL_SING 'NUMBER-OF-ARGS 3) 
(PUT 'CALL_SING 'DEFINED-ON-LINE '474) 
(PUT 'CALL_SING 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'CALL_SING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALL_SING (VARS POLYS ODER)
    (PROG (LL XX CHNOUT FILEIN FILEOUT OFL*BAK CPU GC P)
      (SETQ VARS (CONS 'LIST VARS))
      (SETQ POLYS
              (CONS 'LIST
                    (PROG (P FORALL-RESULT FORALL-ENDPTR)
                      (SETQ P POLYS)
                      (COND ((NULL P) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (P)
                                          (REVAL1 (LIST '*SQ (GET P 'SQVAL) T)
                                                  T))
                                        (CAR P))
                                       NIL)))
                     LOOPLABEL
                      (SETQ P (CDR P))
                      (COND ((NULL P) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (P)
                                  (REVAL1 (LIST '*SQ (GET P 'SQVAL) T) T))
                                (CAR P))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ FILEIN
              (OR *SINGULAR_FILEIN
                  (COND
                   ((FILEP "/tmp")
                    (BLDMSG_INTERNAL "%w%w"
                                     (LIST "/tmp/singular_a1_"
                                           (LEVEL_STRING SESSION_))))
                   (T
                    (BLDMSG_INTERNAL "%w%w"
                                     (LIST "%temp%\\singular_a1_"
                                           (LEVEL_STRING SESSION_)))))))
      (SETQ FILEOUT
              (OR *SINGULAR_FILEOUT
                  (COND
                   ((FILEP "/tmp")
                    (BLDMSG_INTERNAL "%w%w"
                                     (LIST "/tmp/singular_a2_"
                                           (LEVEL_STRING SESSION_))))
                   (T
                    (BLDMSG_INTERNAL "%w%w"
                                     (LIST "%temp%\\singular_a2_"
                                           (LEVEL_STRING SESSION_)))))))
      (SETQ LL (LINELENGTH 10000000))
      (SETQ CHNOUT (OPEN FILEIN 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* FILEIN)
      (SETQ CHNOUT (WRS CHNOUT))
      (SINGULAR_VARS VARS ODER POLYS)
      (SINGULAR_POLYS POLYS)
      (LINELENGTH LL)
      (PRIN2 "short=0;")
      (PRIN2 "   option(redSB); ")
      (PRIN2 "   option(prot); ")
      (PRIN2 "   slimgb(idee); ")
      (PRIN2 "   quit; ")
      (SETQ OFL* OFL*BAK)
      (CLOSE (WRS CHNOUT))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2 "Start of the Singular computation, time limit: ")
          (PRIN2 SINGULAR_TIME)
          (PRIN2 " sec.")
          NIL)
         (TERPRI))))
      (SETQ CPU (TIME))
      (SETQ GC (GCTIME))
      (SETQ XX
              (SYSTEM
               (BLDMSG_INTERNAL "cd %w; %w < %w > %w"
                                (LIST SINGULAR_LIB SINGULAR_CALL FILEIN
                                      FILEOUT))))
      (SYSTEM
       (BLDMSG_INTERNAL "mv %w %w_1;  sed 's/_\\[/%%\\[/g' %w_1 >  %w"
                        (LIST FILEOUT FILEOUT FILEOUT FILEOUT)))
      (COND
       (PRINT_
        (PROGN
         (PROGN (PRIN2 "The Singular computation finished.") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "Time: ")
          (PRIN2 (DIFFERENCE (TIME) CPU))
          (PRIN2 " ms,  GC: ")
          (PRIN2 (DIFFERENCE (GCTIME) GC))
          (PRIN2 " ms")
          NIL)
         (TERPRI))))
      (RETURN
       (COND
        ((EQUAL XX 0)
         (LIST 'LIST (AEVAL (LIST 'READ_SINGULAR_OUTPUT FILEOUT))))
        (T NIL))))) 
(FLUID '(SING_MINPOLY_SING)) 
(PUT 'SINGULAR_VARS 'NUMBER-OF-ARGS 3) 
(PUT 'SINGULAR_VARS 'DEFINED-ON-LINE '552) 
(PUT 'SINGULAR_VARS 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'SINGULAR_VARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SINGULAR_VARS (LI ODER POLYS)
    (PROG (MINPOLY *NAT LI2 H1 H2 H3 FLIN_BAK)
      (SETQ SUBLIS_LIST NIL)
      (COND
       ((SMEMBER 'I (CDR POLYS))
        (SETQ POLYS (CONS 'I (SETDIFF (GVARLIS (CDR POLYS)) LI))))
       (T (SETQ POLYS (SETDIFF (GVARLIS (CDR POLYS)) LI))))
      (SETQ LI
              (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                (SETQ LL LI)
               STARTOVER
                (COND ((NULL LL) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ LL (CDR LL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL LL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ LL (CDR LL))
                (GO LOOPLABEL)))
      (SETQ LI2
              (INTERN
               (BLDMSG_INTERNAL "system(\"alarm\",%d);" (LIST SINGULAR_TIME))))
      (PRIN2 LI2)
      (TERPRI)
      (SETQ LI2 NIL)
      (COND
       (POLYS
        (PROGN
         (PRIN2 "ring rng=(0")
         (PROG (PA)
           (SETQ PA POLYS)
          LAB
           (COND ((NULL PA) (RETURN NIL)))
           ((LAMBDA (PA)
              (PROGN
               (COND
                ((EQCAR PA 'SQRT)
                 (PROGN
                  (SETQ LI2
                          (CONS
                           (INTERN (BLDMSG_INTERNAL "wurz%d" (LIST (CADR PA))))
                           LI2))
                  (SETQ SUBLIS_LIST
                          (CONS
                           (CONS PA
                                 (INTERN
                                  (BLDMSG_INTERNAL "wurz%d" (LIST (CADR PA)))))
                           SUBLIS_LIST))
                  (SETQ MINPOLY
                          (CONS
                           (LIST 'DIFFERENCE (CADR PA)
                                 (LIST 'EXPT
                                       (INTERN
                                        (BLDMSG_INTERNAL "wurz%d"
                                                         (LIST (CADR PA))))
                                       2))
                           MINPOLY))
                  NIL))
                ((EQUAL PA 'I)
                 (PROGN
                  (SETQ LI2 (CONS 'COMPLEX_I LI2))
                  (SETQ SUBLIS_LIST (CONS (CONS PA 'COMPLEX_I) SUBLIS_LIST))
                  (SETQ MINPOLY
                          (CONS (LIST 'PLUS 1 '(EXPT COMPLEX_I 2)) MINPOLY))))
                (T (PROGN (PRIN2 " , ") (PRIN2 PA))))))
            (CAR PA))
           (SETQ PA (CDR PA))
           (GO LAB))
         (PRIN2 ") , ( ")
         (PROG (LLL)
           (SETQ LLL LI2)
          LAB
           (COND ((NULL LLL) (RETURN NIL)))
           ((LAMBDA (LLL) (PROGN (PRIN2 LLL) (PRIN2 " , ") NIL)) (CAR LLL))
           (SETQ LLL (CDR LLL))
           (GO LAB))
         (PRIN2 (CDDDR LI))
         (TERPRI)
         (PRIN2 "),")
         NIL))
       (T (PROGN (PRIN2 "ring rng=(0),") (PRIN2 (CDDDR LI)) (PRIN2 ",") NIL)))
      (SETQ H1 (FCTLENGTH (CAR FTEM_)))
      (SETQ H2 (FCTLENGTH (CAR (REVERSE FTEM_))))
      (COND
       ((NEQ H1 H2)
        (PROGN
         (SETQ FLIN_BAK FLIN_)
         (SETQ FLIN_ NIL)
         (SETQ H3 FTEM_)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND H3 (EQUAL (FCTLENGTH (CAR H3)) H1))) (RETURN NIL)))
           (PROGN (SETQ FLIN_ (CONS (CAR H3) FLIN_)) (SETQ H3 (CDR H3)))
           (GO WHILELABEL))
         (SETQ FLIN_ (REVERSE FLIN_)))))
      (COND
       ((OR LI2 (AND FLIN_ (NEQ (LENGTH FLIN_) (LENGTH FTEM_))))
        (PROGN
         (PRIN2 "(")
         (COND (LI2 (PRIN2 (BLDMSG_INTERNAL "dp(%w)," (LIST (LENGTH LI2))))))
         (SETQ ODER
                 (COND
                  ((EQUAL ODER 'LEX)
                   (BLDMSG_INTERNAL "lp(%w)" (LIST (LENGTH FTEM_))))
                  ((EQUAL ODER 'REVGRADLEX)
                   (COND
                    (FLIN_
                     (BLDMSG_INTERNAL "dp(%w),dp(%w)"
                                      (LIST (LENGTH FLIN_)
                                            (DIFFERENCE (LENGTH FTEM_)
                                                        (LENGTH FLIN_)))))
                    (T (BLDMSG_INTERNAL "dp(%w)" (LIST (LENGTH FTEM_))))))
                  ((EQUAL ODER 'GRADLEX)
                   (COND
                    (FLIN_
                     (BLDMSG_INTERNAL "!Dp(%w),!Dp(%w)"
                                      (LIST (LENGTH FLIN_)
                                            (DIFFERENCE (LENGTH FTEM_)
                                                        (LENGTH FLIN_)))))
                    (T '|dP|)))
                  (T (REDERR "Unknown Order in call_singular"))))
         (PRIN2 ODER)
         (PRIN2 ")")
         NIL))
       (T
        (PROGN
         (SETQ ODER
                 (COND ((EQUAL ODER 'LEX) 'LP) ((EQUAL ODER 'REVGRADLEX) 'DP)
                       ((EQUAL ODER 'GRADLEX) '|dP|)
                       (T (REDERR "Unknown Order in call_singular"))))
         (PRIN2 ODER)
         NIL)))
      (COND ((NEQ H1 H2) (SETQ FLIN_ FLIN_BAK)))
      (PRIN2 ";")
      (TERPRI)
      (SETQ SING_MINPOLY_SING MINPOLY))) 
(PUT 'SINGULAR_POLYS 'NUMBER-OF-ARGS 1) 
(PUT 'SINGULAR_POLYS 'DEFINED-ON-LINE '667) 
(PUT 'SINGULAR_POLYS 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'SINGULAR_POLYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SINGULAR_POLYS (LI)
    (PROG (*NAT)
      (SETQ LI
              (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                (SETQ LL LI)
               STARTOVER
                (COND ((NULL LL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (LL) (LIST '|,| (SUBLIS SUBLIS_LIST LL)))
                         (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ LL (CDR LL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL LL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (LL) (LIST '|,| (SUBLIS SUBLIS_LIST LL)))
                         (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ LL (CDR LL))
                (GO LOOPLABEL)))
      (PRIN2 "ideal idee = ")
      (PROG (LL)
        (SETQ LL (CDDDR LI))
       LAB
        (COND ((NULL LL) (RETURN NIL)))
        ((LAMBDA (LL) (PROGN (MAPRIN LL) (TERPRI))) (CAR LL))
        (SETQ LL (CDR LL))
        (GO LAB))
      (SETQ LI
              (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                (SETQ LL SING_MINPOLY_SING)
               STARTOVER
                (COND ((NULL LL) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ LL (CDR LL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL LL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (LL) (LIST '|,| LL)) (CAR LL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ LL (CDR LL))
                (GO LOOPLABEL)))
      (PROG (LL)
        (SETQ LL LI)
       LAB
        (COND ((NULL LL) (RETURN NIL)))
        ((LAMBDA (LL) (PROGN (MAPRIN LL) (TERPRI))) (CAR LL))
        (SETQ LL (CDR LL))
        (GO LAB))
      (PRIN2 ";")
      (TERPRI)
      (RETURN NIL))) 
(PUT 'READ_SINGULAR_OUTPUT 'NUMBER-OF-ARGS 1) 
(FLAG '(READ_SINGULAR_OUTPUT) 'OPFN) 
(PUT 'READ_SINGULAR_OUTPUT 'DEFINED-ON-LINE '680) 
(PUT 'READ_SINGULAR_OUTPUT 'DEFINED-IN-FILE 'CRACK/CRGB.RED) 
(PUT 'READ_SINGULAR_OUTPUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE READ_SINGULAR_OUTPUT (FILE)
    (PROG (XX CHN CHNOUT FILENAM OFL*BAK)
      (PROGN
       (SETQ FILENAM
               (COND
                ((FILEP "/tmp")
                 (BLDMSG_INTERNAL "%w%w"
                                  (LIST "/tmp/singular_helpus_"
                                        (LEVEL_STRING SESSION_))))
                (T
                 (BLDMSG_INTERNAL "%w%w"
                                  (LIST "%tTEMP%\\singular_helpus_"
                                        (LEVEL_STRING SESSION_))))))
       (SETQ CHN (OPEN FILE 'INPUT))
       (SETQ CHN (RDS CHN))
       (OFF (LIST 'ECHO))
       (PROG ()
        WHILELABEL
         (COND ((NOT (NEQ (SETQ XX (READCH)) '%)) (RETURN NIL)))
        NIL
         (GO WHILELABEL))
       (READ)
       (READCH)
       (SETQ CHNOUT (OPEN FILENAM 'OUTPUT))
       (SETQ OFL*BAK OFL*)
       (SETQ OFL* FILENAM)
       (SETQ CHNOUT (WRS CHNOUT))
       (PRIN2 "algebraic << singular_result :={")
       (TERPRI)
       (PROG ()
        WHILELABEL
         (COND
          ((NOT (AND (NEQ (SETQ XX (READCH)) $EOF$) (NEQ XX '>)))
           (RETURN NIL)))
         (PROGN
          (COND ((EQUAL XX '%) (PROGN (READ) (READCH) (SETQ XX '|,|))))
          (TYO (ID2INT XX)))
         (GO WHILELABEL))
       (PRIN2 "} >> $ END;")
       (TERPRI)
       (ON (LIST 'ECHO))
       (SETQ OFL* OFL*BAK)
       (CLOSE (WRS CHNOUT))
       (CLOSE (RDS CHN))
       NIL)
      (AEVAL
       (LIST 'SYSTEM
             (BLDMSG_INTERNAL "%w%w%w%w%w"
                              (LIST "cp /tmp/singular_helpus_"
                                    (LEVEL_STRING SESSION_)
                                    " /tmp/singular_help_"
                                    (LEVEL_STRING SESSION_) ";"))))
      (AEVAL
       (LIST 'SYSTEM
             (BLDMSG_INTERNAL "%w%w%w%w%w%w"
                              (LIST
                               "sed -e 's/auf wiedersehen\\.//g' -e 's/wurz\\([0-9]*\\)/sqrt(\\1)/g' "
                               " -e 's/complex_i/i/g' " "/tmp/singular_help_"
                               (LEVEL_STRING SESSION_)
                               " > /tmp/singular_helpus_"
                               (LEVEL_STRING SESSION_)))))
      (PROGN (SETQ *UNCACHED T) (INFILE FILENAM))
      (RETURN (AEVAL 'SINGULAR_RESULT)))) 
(ENDMODULE) 