(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID
 '(PRINT_ LOGOPRINT_ NFCT_ FNAME_ TIME_ FACINT_ SAFEINT_ FREEINT_ ODESOLVE_)) 
(FLAG '(YESP) 'BOOLEAN) 
(SETQ LOGOPRINT_ T) 
(FLAG '(FREEOFLIST) 'OPFN) 
(FLAG '(TERMXREAD) 'OPFN) 
(PUT 'APPLYSYM 'NUMBER-OF-ARGS 2) 
(FLAG '(APPLYSYM) 'OPFN) 
(PUT 'APPLYSYM 'DEFINED-ON-LINE '43) 
(PUT 'APPLYSYM 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'APPLYSYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLYSYM (PROBLEM SYMTRY)
    (PROG (GENLIST CON E1 E2 H1 H2 H3 H4 H5 H6 H7 MODUS U V EQLIST XLIST YLIST
           N COP1 COP2 SYMANZ OLDSOL OLDMODUS TRAFOPROB ALTLOGO)
      (CHANGE_PROMPT_TO "")
      (AEVAL (LIST 'BACKUP_REDUCE_FLAGS))
      (AEVAL (CLEAR (LIST 'SY_ 'SYM_)))
      (ARRAYFN 'ALGEBRAIC
               (LIST
                (LIST 'SYM_ (IEVAL (LIST 'LENGTH (LIST 'SECOND SYMTRY))))))
      (PUT 'SY_ 'SIMPFN 'SIMPIDEN)
      (PUT 'FF 'SIMPFN 'SIMPIDEN)
      (PUT 'FFI 'SIMPFN 'SIMPIDEN)
      (SETQ EQLIST (AEVAL (LIST 'MAKLIST (LIST 'FIRST PROBLEM))))
      (SETQ YLIST (AEVAL (LIST 'MAKLIST (LIST 'SECOND PROBLEM))))
      (SETQ XLIST (AEVAL (LIST 'MAKLIST (LIST 'THIRD PROBLEM))))
      (SETQ CON (AEVAL (LIST 'SECOND SYMTRY)))
      (SETQ SYMANZ (AEVAL 0))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL CON)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((AND (FREEOF (REVALX EQLIST) (REVALX E1))
                  (BOOLVALUE* (REVALX (LIST 'FREEOFLIST E1 XLIST)))
                  (BOOLVALUE* (REVALX (LIST 'FREEOFLIST E1 YLIST))))
             (PROGN
              (SETQ GENLIST
                      (AEVAL
                       (LIST 'SUB (LIST 'EQUAL E1 1) (LIST 'FIRST SYMTRY))))
              (PROG (EL2)
                (SETQ EL2 (GETRLIST (AEVAL CON)))
               LAB
                (COND ((NULL EL2) (RETURN NIL)))
                ((LAMBDA (EL2)
                   (COND
                    ((EVALNEQ (AEVAL 'EL1) (AEVAL EL2))
                     (SETQ GENLIST
                             (AEVAL
                              (LIST 'SUB (LIST 'EQUAL EL2 0) GENLIST))))))
                 (CAR EL2))
                (SETQ EL2 (CDR EL2))
                (GO LAB))
              (SETQ SYMANZ (AEVAL (LIST 'PLUS SYMANZ 1)))
              (SETK (LIST 'SYM_ SYMANZ) (AEVAL GENLIST))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (REPEAT
       (PROGN
        (SETQ OLDMODUS (AEVAL* MODUS))
        (REPEAT
         (PROGN
          (PROGN
           (TERPRI)
           (PROGN
            (PRIN2 "Do you want to find similarity and symmetry variables ")
            (PRIN2 "(1)")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "or generalize a special solution with new parameters  ")
            (PRIN2 "(2)")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "or exit the program                                   ")
            (PRIN2 "(3) :   ")
            NIL)
           NIL)
          (SETQ MODUS (AEVAL* (TERMREAD))))
         (OR (EVALEQUAL (AEVAL* MODUS) 1) (EVALEQUAL (AEVAL* MODUS) 2)
             (EVALEQUAL (AEVAL* MODUS) 3)))
        (COND
         ((EVALNEQ (AEVAL* MODUS) 3)
          (PROGN
           (COND
            ((EVALEQUAL (AEVAL* SYMANZ) 1)
             (SETQ GENLIST (AEVAL* (LIST 'SYM_ 1))))
            (T
             (PROGN
              (PROG (N)
                (SETQ N 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* SYMANZ) N))
                  (RETURN NIL)))
                (PROGN
                 (PROGN
                  (ASSGNPRI (AEVAL* "----------------------   The ") NIL
                            'FIRST)
                  (ASSGNPRI (AEVAL* N) NIL NIL)
                  (ASSGNPRI (AEVAL* ".  symmetry is:") NIL 'LAST))
                 (PROG (E2)
                   (SETQ E2 (GETRLIST (AEVAL* (LIST 'SYM_ N))))
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (COND
                       ((EVALNEQ (AEVAL* (LIST 'RHS E2)) 0)
                        (ASSGNPRI (AEVAL* E2) NIL 'ONLY))))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB)))
                (SETQ N
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         N))
                (GO LAB))
              (ASSGNPRI (AEVAL* "----------------------") NIL 'ONLY)
              (REPEAT
               (PROGN
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "Which single symmetry or linear combination of symmetries")
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "do you want to apply? ") NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2 "Enter an expression with `sy_(i)' for the i'th ")
                  (PRIN2 "symmetry. Terminate input with `$' or `;'.")
                  NIL)
                 (TERPRI)
                 (TERPRI))
                (SETQ H1 (REVAL1 (TERMXREAD) T))
                (COND
                 ((BOOLVALUE* H1)
                  (PROGN
                   (PROG (H2)
                     (SETQ H2 (GETRLIST (AEVAL* XLIST)))
                    LAB
                     (COND ((NULL H2) (RETURN NIL)))
                     ((LAMBDA (H2)
                        (COND
                         ((BOOLVALUE* H1)
                          (COND
                           ((EVALNEQ (AEVAL* (LIST 'DF H1 H2)) 0)
                            (SETQ H1 (AEVAL* 'NIL)))))))
                      (CAR H2))
                     (SETQ H2 (CDR H2))
                     (GO LAB))
                   (PROG (H2)
                     (SETQ H2 (GETRLIST (AEVAL* YLIST)))
                    LAB
                     (COND ((NULL H2) (RETURN NIL)))
                     ((LAMBDA (H2)
                        (COND
                         ((BOOLVALUE* H1)
                          (COND
                           ((EVALNEQ (AEVAL* (LIST 'DF H1 H2)) 0)
                            (SETQ H1 (AEVAL* 'NIL)))))))
                      (CAR H2))
                     (SETQ H2 (CDR H2))
                     (GO LAB))
                   (COND
                    ((EVALEQUAL (AEVAL* H1) (AEVAL* 'NIL))
                     (PROGN
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        "The coefficients of the sy_(i) must be constant, ")
                       (PRIN2 "i.e. numbers or constants")
                       NIL)
                      (TERPRI))))))))
               (BOOLVALUE* H1))
              (SETQ GENLIST (AEVAL* (LIST 'LIST)))
              (SETQ COP1 (AEVAL* (LIST 'SYM_ 1)))
              (WHILE (EVALNEQ (AEVAL* COP1) (AEVAL* (LIST 'LIST)))
                     (PROGN
                      (SETQ H6 (AEVAL* (LIST 'LHS (LIST 'FIRST COP1))))
                      (SETQ COP1 (AEVAL* (LIST 'REST COP1)))
                      (SETQ GENLIST
                              (AEVAL*
                               (LIST 'CONS (LIST 'EQUAL H6 0) GENLIST)))))
              (SETQ GENLIST (AEVAL* (LIST 'REVERSE GENLIST)))
              (PROG (H2)
                (SETQ H2 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* SYMANZ) H2))
                  (RETURN NIL)))
                (PROGN
                 (SETQ H3 (AEVAL* (LIST 'COEFFN H1 (LIST 'SY_ H2) 1)))
                 (COND
                  ((EVALNEQ (AEVAL* H3) 0)
                   (PROGN
                    (SETQ COP1 (AEVAL* GENLIST))
                    (SETQ COP2 (AEVAL* (LIST 'SYM_ H2)))
                    (SETQ GENLIST (AEVAL* (LIST 'LIST)))
                    (WHILE (EVALNEQ (AEVAL* COP1) (AEVAL* (LIST 'LIST)))
                           (PROGN
                            (SETQ H4 (AEVAL* (LIST 'FIRST COP1)))
                            (SETQ COP1 (AEVAL* (LIST 'REST COP1)))
                            (SETQ H5 (AEVAL* (LIST 'FIRST COP2)))
                            (SETQ COP2 (AEVAL* (LIST 'REST COP2)))
                            (SETQ H6 (AEVAL* (LIST 'LHS H4)))
                            (SETQ GENLIST
                                    (AEVAL*
                                     (LIST 'CONS
                                           (LIST 'EQUAL H6
                                                 (LIST 'PLUS (LIST 'RHS H4)
                                                       (LIST 'TIMES H3
                                                             (LIST 'RHS H5))))
                                           GENLIST)))))
                    (SETQ GENLIST (AEVAL* (LIST 'REVERSE GENLIST)))))))
                (SETQ H2
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         H2))
                (GO LAB)))))
           (ASSGNPRI (AEVAL* "The symmetry to be applied in the following is ")
                     NIL 'ONLY)
           (ASSGNPRI (AEVAL* GENLIST) NIL 'ONLY)
           (ASSGNPRI (AEVAL* "Terminate the following input with $ or ;  .")
                     NIL 'ONLY)
           (COND
            ((EVALEQUAL (AEVAL* MODUS) 1)
             (PROGN
              (PROGN
               (PROGN
                (PRIN2 "Enter the name of the new dependent variable")
                NIL)
               (COND
                ((LESSP 2 (LENGTH (AEVAL* YLIST))) (PROGN (PRIN2 "s") NIL)))
               (TERPRI)
               (PROGN (PRIN2 "(which will get an index attached): ") NIL))
              (SETQ U (AEVAL* (LIST 'TERMXREAD)))
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "Enter the name of the new independent variable")
                NIL)
               (COND
                ((LESSP 2 (LENGTH (AEVAL* XLIST))) (PROGN (PRIN2 "s") NIL)))
               (TERPRI)
               (PROGN (PRIN2 "(which will get an index attached): ") NIL))
              (SETQ V (AEVAL* (LIST 'TERMXREAD)))
              (SETQ ALTLOGO (AEVAL* 'LOGOPRINT_))
              (SETK 'LOGOPRINT_ (AEVAL* 'NIL))
              (SETQ TRAFOPROB
                      (AEVAL*
                       (LIST 'SIMILARITY PROBLEM GENLIST (LIST 'LIST) U V)))
              (SETK 'LOGOPRINT_ (AEVAL* ALTLOGO))))
            (T
             (PROGN
              (COND
               ((EVALLESSP (AEVAL* (LIST 'LENGTH H1)) 2)
                (PROGN
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 "What shall the name of the new constant parameter")
                   (PRIN2 " be? ")
                   NIL)
                  (TERPRI))
                 (SETQ H2 (AEVAL* (LIST 'TERMXREAD))))))
              (REPEAT
               (PROGN
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "Enter the solution to be generalized in form of an ")
                  (PRIN2 "expression, which vanishes")
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "or in form of an equation `... = ...' ") NIL)
                 (COND ((EQUAL OLDSOL NIL) (PROGN (PRIN2 ":") NIL))
                       (T
                        (PROGN
                         (TERPRI)
                         (PROGN
                          (PRIN2
                           "or enter semicolon `;' to work on the solution ")
                          (PRIN2 "specified before:")
                          NIL))))
                 (TERPRI))
                (SETQ H3 (AEVAL* (LIST 'TERMXREAD)))
                (COND
                 ((EVALNEQ (AEVAL* H3) (AEVAL* 'NIL))
                  (SETQ OLDSOL (AEVAL* H3)))))
               (EVALNEQ (AEVAL* OLDSOL) (AEVAL* 'NIL)))
              (SETQ H3 (AEVAL* (LIST 'NEWPARAM OLDSOL GENLIST H2)))
              (COND
               ((EVALNEQ (AEVAL* H3) (AEVAL* 'NIL))
                (SETQ OLDSOL (AEVAL* H3)))))))))))
       (EVALEQUAL (AEVAL* MODUS) 3))
      (AEVAL (CLEAR (LIST 'SYM_)))
      (AEVAL (LIST 'RECOVER_REDUCE_FLAGS))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN
       (COND ((EVALEQUAL (AEVAL OLDMODUS) 1) (AEVAL TRAFOPROB))
             ((EVALEQUAL (AEVAL OLDMODUS) 2) (AEVAL OLDSOL))
             (T (AEVAL 'NIL)))))) 
(PUT 'NEWPARAM 'NUMBER-OF-ARGS 3) 
(FLAG '(NEWPARAM) 'OPFN) 
(PUT 'NEWPARAM 'DEFINED-ON-LINE '202) 
(PUT 'NEWPARAM 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'NEWPARAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWPARAM (OLDSOL GENLIST U_)
    (PROG (H1 H2 H20 H3 H30 H4 VARI PDE E1 PRINTOLD CLIST NEWSOL OLDSOL_EX
           PREV_DEPEND)
      (SETQ VARI (AEVAL (LIST 'MAKEPDE GENLIST U_)))
      (SETQ PDE (AEVAL (LIST 'FIRST VARI)))
      (SETQ VARI (AEVAL (LIST 'REST VARI)))
      (SETQ OLDSOL_EX (AEVAL (LIST 'EQU_TO_EXPR OLDSOL)))
      (SETQ PREV_DEPEND (AEVAL (LIST 'STOREDEPEND VARI)))
      (SETQ H2 (AEVAL (LIST 'SUB (LIST 'EQUAL U_ OLDSOL_EX) PDE)))
      (COND
       ((EVALNEQ (AEVAL H2) 0)
        (PROGN
         (SETQ H1 (AEVAL (LIST 'SOLVE OLDSOL_EX VARI)))
         (COND
          ((EVALNEQ (AEVAL H1) (AEVAL (LIST 'LIST)))
           (PROGN
            (AEVAL (LIST 'EQUAL H1 (LIST 'FIRST H1)))
            (PROG (H3)
              (SETQ H3 (GETRLIST (AEVAL H1)))
             LAB
              (COND ((NULL H3) (RETURN NIL)))
              ((LAMBDA (H3)
                 (COND
                  ((FREEOF (REVALX H3) (REVALX 'ARBCOMPLEX))
                   (SETQ H2 (AEVAL (LIST 'SUB H3 H2))))))
               (CAR H3))
              (SETQ H3 (CDR H3))
              (GO LAB))))))))
      (AEVAL (LIST 'RESTOREDEPEND PREV_DEPEND))
      (COND
       ((EVALEQUAL 0 (AEVAL H2))
        (RETURN
         (PROGN
          (PROGN
           (PRIN2 "The special solution to be generalized is an invariant ")
           (PRIN2 "with respect to")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "this symmetry, therefore no generalization is possible.")
           NIL)
          (TERPRI)
          (PROG (H1)
            (SETQ H1 (FARGS U_))
           LAB
            (COND ((NULL H1) (RETURN NIL)))
            ((LAMBDA (H1) (NODEPEND (LIST U_ H1))) (CAR H1))
            (SETQ H1 (CDR H1))
            (GO LAB))
          (AEVAL OLDSOL)))))
      (SETQ PDE (AEVAL (LIST 'DIFFERENCE PDE 1)))
      (SETQ H1 (AEVAL (LIST 'QUASILINPDE1 PDE U_ VARI)))
      (COND
       ((EVALNEQ (AEVAL H1) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ H1 (AEVAL (LIST 'FIRST H1)))
         (SETQ CLIST (AEVAL (LIST 'LIST)))
         (SETQ H2
                 (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E1 (GETRLIST (AEVAL H1)))
                   (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (E1)
                                       (PROGN
                                        (SETQ H3
                                                (AEVAL
                                                 (NEWFCT FNAME_ NIL NFCT_)))
                                        (SETQ NFCT_ (ADD1 NFCT_))
                                        (SETQ CLIST
                                                (AEVAL (LIST 'CONS H3 CLIST)))
                                        (AEVAL (LIST 'EQUAL H3 E1))))
                                     (CAR E1))
                                    NIL)))
                  LOOPLABEL
                   (SETQ E1 (CDR E1))
                   (COND ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (E1)
                               (PROGN
                                (SETQ H3 (AEVAL (NEWFCT FNAME_ NIL NFCT_)))
                                (SETQ NFCT_ (ADD1 NFCT_))
                                (SETQ CLIST (AEVAL (LIST 'CONS H3 CLIST)))
                                (AEVAL (LIST 'EQUAL H3 E1))))
                             (CAR E1))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ H20 (AEVAL (LIST 'SUB (LIST 'EQUAL U_ 0) H2)))
         (SETQ H3 (AEVAL (LIST 'SOLVE H2 VARI)))
         (COND
          ((EVALNEQ (AEVAL H3) (AEVAL (LIST 'LIST)))
           (PROGN
            (SETQ H3 (AEVAL (LIST 'FIRST H3)))
            (SETQ H30 (AEVAL (LIST 'SUB H20 H3)))
            (ASSGNPRI
             (AEVAL "The substitutions to generalize the solution are: ") NIL
             'ONLY)
            (PROG (H4)
              (SETQ H4 (GETRLIST (AEVAL H30)))
             LAB
              (COND ((NULL H4) (RETURN NIL)))
              ((LAMBDA (H4) (ASSGNPRI (AEVAL H4) NIL 'ONLY)) (CAR H4))
              (SETQ H4 (CDR H4))
              (GO LAB))
            (SETQ NEWSOL (AEVAL (LIST 'SUB H30 OLDSOL_EX)))
            (PROGN (PROGN (PRIN2 "The new solution") NIL) NIL)
            (PROGN
             (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
             (ASSGNPRI (AEVAL NEWSOL) NIL 'LAST))))))))
      (PROG (H1)
        (SETQ H1 (GETRLIST (AEVAL (LIST 'FARGS U_))))
       LAB
        (COND ((NULL H1) (RETURN NIL)))
        ((LAMBDA (H1) (AEVAL (NODEPEND (LIST U_ H1)))) (CAR H1))
        (SETQ H1 (CDR H1))
        (GO LAB))
      (RETURN (AEVAL NEWSOL)))) 
(FLAG '(EINFACHST) 'OPFN) 
(PUT 'EINFACHST 'NUMBER-OF-ARGS 2) 
(PUT 'EINFACHST 'DEFINED-ON-LINE '270) 
(PUT 'EINFACHST 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'EINFACHST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EINFACHST (A X)
    (PROG (EL1 EL2 HP)
      (SETQ HP 10000)
      (SETQ A (CDR A))
      (PROG ()
       WHILELABEL
        (COND ((NOT A) (RETURN NIL)))
        (PROGN
         (SETQ EL2 (CAR A))
         (SETQ A (CDR A))
         (COND
          ((AND (NOT (FREEOF EL2 X))
                (OR (NOT EL1) (EQUAL EL2 X)
                    (PROGN
                     (COND ((NOT (POLYNOP EL2 (CONS X NIL))) NIL)
                           (T
                            (PROGN
                             (COEFF1 EL2 X NIL)
                             (COND
                              ((LESSP HIPOW* HP) (PROGN (SETQ HP HIPOW*) T))
                              (T NIL))))))))
           (SETQ EL1 EL2))))
        (GO WHILELABEL))
      (RETURN EL1))) 
(PUT 'TRANSDF 'NUMBER-OF-ARGS 4) 
(FLAG '(TRANSDF) 'OPFN) 
(PUT 'TRANSDF 'DEFINED-ON-LINE '293) 
(PUT 'TRANSDF 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'TRANSDF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANSDF (Y YSLIST VLIST INDXLIST)
    (PROG (M N E1 DFY)
      (RETURN
       (COND
        ((EVALEQUAL (AEVAL INDXLIST) (AEVAL (LIST 'LIST)))
         (AEVAL (LIST 'SUB YSLIST Y)))
        (T
         (PROGN
          (SETQ M (AEVAL (LIST 'FIRST INDXLIST)))
          (SETQ N (AEVAL 0))
          (SETQ DFY
                  (AEVAL (LIST 'TRANSDF Y YSLIST VLIST (LIST 'REST INDXLIST))))
          (PROG (E1 FORALL-RESULT)
            (SETQ E1 (GETRLIST (AEVAL VLIST)))
            (SETQ FORALL-RESULT 0)
           LAB1
            (COND ((NULL E1) (RETURN FORALL-RESULT)))
            (SETQ FORALL-RESULT
                    (AEVAL*
                     (LIST 'PLUS
                           ((LAMBDA (E1)
                              (PROGN
                               (SETQ N (AEVAL (LIST 'PLUS N 1)))
                               (AEVAL
                                (LIST 'TIMES (LIST 'DF DFY E1)
                                      (LIST 'DV/DX N M)))))
                            (CAR E1))
                           FORALL-RESULT)))
            (SETQ E1 (CDR E1))
            (GO LAB1)))))))) 
(PUT 'TRANSDERIV 'NUMBER-OF-ARGS 3) 
(FLAG '(TRANSDERIV) 'OPFN) 
(PUT 'TRANSDERIV 'DEFINED-ON-LINE '311) 
(PUT 'TRANSDERIV 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'TRANSDERIV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANSDERIV (YIK YSLIST VLIST)
    (PROG (INDXLIST Y L1 L2)
      (SETQ INDXLIST (AEVAL (CONS 'LIST (COMBIDIF YIK))))
      (RETURN
       (AEVAL
        (LIST 'TRANSDF (LIST 'FIRST INDXLIST) YSLIST VLIST
              (LIST 'REST INDXLIST)))))) 
(PUT 'DETRAFO 'NUMBER-OF-ARGS 5) 
(FLAG '(DETRAFO) 'OPFN) 
(PUT 'DETRAFO 'DEFINED-ON-LINE '320) 
(PUT 'DETRAFO 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'DETRAFO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DETRAFO (EQLIST YSLIST XSLIST ULIST VLIST)
    (PROG (AVAR NVAR DETPD N M ORDR E1 E2 E3 SB)
      (SETQ M (AEVAL (LIST 'LENGTH XSLIST)))
      (SETQ N (AEVAL (LIST 'PLUS (LIST 'LENGTH YSLIST) M)))
      (AEVAL (CLEAR (LIST 'DYX/DUV 'DV/DX)))
      (AEVAL (MATRIX (LIST (LIST 'DYX/DUV N N))))
      (AEVAL (MATRIX (LIST (LIST 'DV/DX M M))))
      (SETQ AVAR (AEVAL (LIST 'APPEND YSLIST XSLIST)))
      (SETQ NVAR (AEVAL (LIST 'APPEND ULIST VLIST)))
      (SETQ N (AEVAL 0))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL AVAR)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ N (AEVAL (LIST 'PLUS N 1)))
            (SETQ M (AEVAL 0))
            (PROG (E2)
              (SETQ E2 (GETRLIST (AEVAL NVAR)))
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (PROGN
                  (SETQ M (AEVAL (LIST 'PLUS M 1)))
                  (SETK (LIST 'DYX/DUV M N)
                        (AEVAL (LIST 'DF (LIST 'RHS E1) E2)))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ DETPD (AEVAL (LIST 'DET 'DYX/DUV)))
      (COND
       ((EVALEQUAL (AEVAL DETPD) 0)
        (RETURN
         (PROGN
          (ASSGNPRI (AEVAL "The proposed transformation is not regular!") NIL
                    'ONLY)
          (AEVAL (LIST 'LIST))))))
      (AEVAL (CLEAR (LIST 'DYX/DUV)))
      (SETQ ORDR (AEVAL 0))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL EQLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROG (E2)
             (SETQ E2 (GETRLIST (AEVAL YSLIST)))
            LAB
             (COND ((NULL E2) (RETURN NIL)))
             ((LAMBDA (E2)
                (PROGN
                 (SETQ N (AEVAL (LIST 'TOTDEG E1 (LIST 'LHS E2))))
                 (COND
                  ((EVALGREATERP (AEVAL N) (AEVAL ORDR))
                   (SETQ ORDR (AEVAL N))))))
              (CAR E2))
             (SETQ E2 (CDR E2))
             (GO LAB)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ SB
              (AEVAL
               (LIST 'SUBDIF1
                     (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E1 (GETRLIST (AEVAL XSLIST)))
                       (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (E1) (AEVAL (LIST 'LHS E1)))
                                         (CAR E1))
                                        NIL)))
                      LOOPLABEL
                       (SETQ E1 (CDR E1))
                       (COND ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (E1) (AEVAL (LIST 'LHS E1))) (CAR E1))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E1 (GETRLIST (AEVAL YSLIST)))
                       (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (E1) (AEVAL (LIST 'LHS E1)))
                                         (CAR E1))
                                        NIL)))
                      LOOPLABEL
                       (SETQ E1 (CDR E1))
                       (COND ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (E1) (AEVAL (LIST 'LHS E1))) (CAR E1))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     ORDR)))
      (SETQ N (AEVAL 0))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL XSLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ N (AEVAL (LIST 'PLUS N 1)))
            (SETQ M (AEVAL 0))
            (PROG (E2)
              (SETQ E2 (GETRLIST (AEVAL VLIST)))
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (PROGN
                  (SETQ M (AEVAL (LIST 'PLUS M 1)))
                  (SETK (LIST 'DV/DX N M)
                        (AEVAL
                         (LIST 'TOTAL_ALG_MODE_DERIV (LIST 'RHS E1) E2)))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETK 'DV/DX (AEVAL (LIST 'EXPT 'DV/DX (MINUS 1))))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL SB)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROG (E2)
             (SETQ E2 (GETRLIST (AEVAL E1)))
            LAB
             (COND ((NULL E2) (RETURN NIL)))
             ((LAMBDA (E2)
                (PROGN
                 (COND
                  ((NOT (FREEOF (REVALX EQLIST) (REVALX (LIST 'LHS E2))))
                   (PROGN
                    (SETQ EQLIST
                            (AEVAL
                             (LIST 'SUB
                                   (LIST 'EQUAL (LIST 'LHS E2)
                                         (LIST 'TRANSDERIV (LIST 'RHS E2)
                                               YSLIST VLIST))
                                   EQLIST))))))))
              (CAR E2))
             (SETQ E2 (CDR E2))
             (GO LAB)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'DV/DX)))
      (RETURN (AEVAL (LIST 'SUB XSLIST (LIST 'SUB YSLIST EQLIST)))))) 
(PUT 'GROUPING 'NUMBER-OF-ARGS 6) 
(FLAG '(GROUPING) 'OPFN) 
(PUT 'GROUPING 'DEFINED-ON-LINE '382) 
(PUT 'GROUPING 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'GROUPING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROUPING (EL1 EL2 XLIST YLIST NX NY)
    (PROG (H EL3 XSLIST YSLIST)
      (SETQ H (AEVAL (LIST 'LIST)))
      (SETQ XSLIST (AEVAL (LIST 'LIST)))
      (SETQ YSLIST (AEVAL (LIST 'LIST)))
      (PROG (EL3)
        (SETQ EL3 (GETRLIST (AEVAL EL1)))
       LAB
        (COND ((NULL EL3) (RETURN NIL)))
        ((LAMBDA (EL3)
           (COND
            ((BOOLVALUE* (REVALX (LIST 'FREEOFLIST EL3 YLIST)))
             (SETQ XSLIST (AEVAL (LIST 'CONS EL3 XSLIST))))
            ((BOOLVALUE* (REVALX (LIST 'FREEOFLIST EL3 XLIST)))
             (SETQ YSLIST (AEVAL (LIST 'CONS EL3 YSLIST))))
            (T (SETQ H (AEVAL (LIST 'CONS EL3 H))))))
         (CAR EL3))
        (SETQ EL3 (CDR EL3))
        (GO LAB))
      (COND
       ((OR (BOOLVALUE* (REVALX (LIST 'FREEOFLIST EL2 YLIST)))
            (EVALEQUAL (AEVAL (LIST 'LENGTH YSLIST)) (AEVAL NY)))
        (SETQ XSLIST (AEVAL (LIST 'CONS EL2 XSLIST))))
       ((OR (BOOLVALUE* (REVALX (LIST 'FREEOFLIST EL2 XLIST)))
            (EVALEQUAL (AEVAL (LIST 'LENGTH XSLIST)) (AEVAL NX)))
        (SETQ YSLIST (AEVAL (LIST 'CONS EL2 YSLIST))))
       (T (SETQ XSLIST (AEVAL (LIST 'CONS EL2 XSLIST)))))
      (PROG (EL3)
        (SETQ EL3 (GETRLIST (AEVAL H)))
       LAB
        (COND ((NULL EL3) (RETURN NIL)))
        ((LAMBDA (EL3)
           (COND
            ((EVALLESSP (AEVAL (LIST 'LENGTH YSLIST)) (AEVAL NY))
             (SETQ YSLIST (AEVAL (LIST 'CONS EL3 YSLIST))))
            (T (SETQ XSLIST (AEVAL (LIST 'CONS EL3 XSLIST))))))
         (CAR EL3))
        (SETQ EL3 (CDR EL3))
        (GO LAB))
      (RETURN (AEVAL (LIST 'LIST XSLIST YSLIST))))) 
(PUT 'RENAME_U_ 'NUMBER-OF-ARGS 6) 
(FLAG '(RENAME_U_) 'OPFN) 
(PUT 'RENAME_U_ 'DEFINED-ON-LINE '408) 
(PUT 'RENAME_U_ 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'RENAME_U_ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RENAME_U_ (XSLIST YSLIST EL2 U_ U V)
    (PROG (I VLIST ULIST EL3 H SMV)
      (SETQ I (AEVAL 0))
      (SETQ VLIST (AEVAL (LIST 'LIST)))
      (SETQ XSLIST
              (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL3 (GETRLIST (AEVAL XSLIST)))
                (COND ((NULL EL3) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL3)
                                    (PROGN
                                     (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                     (COND
                                      ((EVALGREATERP
                                        (AEVAL (LIST 'LENGTH XSLIST)) 1)
                                       (SETQ H (AEVAL (LIST 'MKID V I))))
                                      (T (SETQ H (AEVAL V))))
                                     (SETQ VLIST (AEVAL (LIST 'CONS H VLIST)))
                                     (COND
                                      ((EVALEQUAL (AEVAL EL3) (AEVAL EL2))
                                       (SETQ SMV (AEVAL H))))
                                     (AEVAL
                                      (LIST 'SUB (LIST 'EQUAL U_ H) EL3))))
                                  (CAR EL3))
                                 NIL)))
               LOOPLABEL
                (SETQ EL3 (CDR EL3))
                (COND ((NULL EL3) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL3)
                            (PROGN
                             (SETQ I (AEVAL (LIST 'PLUS I 1)))
                             (COND
                              ((EVALGREATERP (AEVAL (LIST 'LENGTH XSLIST)) 1)
                               (SETQ H (AEVAL (LIST 'MKID V I))))
                              (T (SETQ H (AEVAL V))))
                             (SETQ VLIST (AEVAL (LIST 'CONS H VLIST)))
                             (COND
                              ((EVALEQUAL (AEVAL EL3) (AEVAL EL2))
                               (SETQ SMV (AEVAL H))))
                             (AEVAL (LIST 'SUB (LIST 'EQUAL U_ H) EL3))))
                          (CAR EL3))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ I (AEVAL 0))
      (SETQ ULIST (AEVAL (LIST 'LIST)))
      (SETQ YSLIST
              (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL3 (GETRLIST (AEVAL YSLIST)))
                (COND ((NULL EL3) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL3)
                                    (PROGN
                                     (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                     (COND
                                      ((EVALGREATERP
                                        (AEVAL (LIST 'LENGTH YSLIST)) 1)
                                       (SETQ H (AEVAL (LIST 'MKID U I))))
                                      (T (SETQ H (AEVAL U))))
                                     (SETQ ULIST (AEVAL (LIST 'CONS H ULIST)))
                                     (COND
                                      ((EVALEQUAL (AEVAL EL3) (AEVAL EL2))
                                       (SETQ SMV (AEVAL H))))
                                     (AEVAL
                                      (LIST 'SUB (LIST 'EQUAL U_ H) EL3))))
                                  (CAR EL3))
                                 NIL)))
               LOOPLABEL
                (SETQ EL3 (CDR EL3))
                (COND ((NULL EL3) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL3)
                            (PROGN
                             (SETQ I (AEVAL (LIST 'PLUS I 1)))
                             (COND
                              ((EVALGREATERP (AEVAL (LIST 'LENGTH YSLIST)) 1)
                               (SETQ H (AEVAL (LIST 'MKID U I))))
                              (T (SETQ H (AEVAL U))))
                             (SETQ ULIST (AEVAL (LIST 'CONS H ULIST)))
                             (COND
                              ((EVALEQUAL (AEVAL EL3) (AEVAL EL2))
                               (SETQ SMV (AEVAL H))))
                             (AEVAL (LIST 'SUB (LIST 'EQUAL U_ H) EL3))))
                          (CAR EL3))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (AEVAL
        (LIST 'LIST XSLIST YSLIST (LIST 'REVERSE VLIST) (LIST 'REVERSE ULIST)
              SMV))))) 
(PUT 'SOLVE_FOR_OLD_VAR 'NUMBER-OF-ARGS 6) 
(FLAG '(SOLVE_FOR_OLD_VAR) 'OPFN) 
(PUT 'SOLVE_FOR_OLD_VAR 'DEFINED-ON-LINE '436) 
(PUT 'SOLVE_FOR_OLD_VAR 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'SOLVE_FOR_OLD_VAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE_FOR_OLD_VAR (XSLIST YSLIST XLIST YLIST NX NY)
    (PROG (H1 H2)
      (SETQ H1 (AEVAL 'NIL))
      (SETQ H2
              (AEVAL
               (LIST 'SOLVE (LIST 'APPEND YSLIST XSLIST)
                     (LIST 'APPEND XLIST YLIST))))
      (COND ((EVALEQUAL (AEVAL H2) (AEVAL (LIST 'LIST))) (SETQ H1 (AEVAL 'T)))
            (T (SETQ H2 (AEVAL (LIST 'FIRST H2)))))
      (COND
       ((EVALNEQ (AEVAL 'LIST) (AEVAL (CAR (AEVAL H2)))) (SETQ H1 (AEVAL 'T)))
       ((EVALLESSP (AEVAL (LIST 'LENGTH H2)) (AEVAL (LIST 'PLUS NX NY)))
        (SETK 'EL2 (AEVAL 'T))))
      (COND
       ((BOOLVALUE* H1)
        (REPEAT
         (PROGN
          (PROGN
           (PRIN2 "The algebraic system ")
           (PRIN2 (APPEND XSLIST YSLIST))
           (PRIN2 " could not be solved for ")
           (PRIN2 (APPEND XLIST YLIST))
           (PRIN2 ".")
           NIL)
          (PROGN
           (PRIN2 "Please enter the solution in form of a list {")
           (PRIN2 (REVAL1 (AEVAL* (LIST 'FIRST XLIST)) T))
           (PRIN2 "=...,...")
           (PRIN2 (REVAL1 (AEVAL* (LIST 'FIRST YLIST)) T))
           (PRIN2 "=...,...} or enter a ")
           (PRIN2 "semicolon ; to end this investigation:")
           NIL)
          (SETQ H2 (AEVAL* (LIST 'TERMXREAD))))
         (OR (EVALEQUAL (AEVAL* H2) (AEVAL* 'NIL))
             (AND (BOOLVALUE* (REVALX (PAIRP (REVALX H2))))
                  (EVALEQUAL (AEVAL* 'LIST) (AEVAL* (CAR (AEVAL* H2))))
                  (EVALEQUAL (AEVAL* (LIST 'LENGTH H2))
                             (AEVAL* (LIST 'PLUS NX NY)))))))
       (T
        (PROGN
         (PROGN
          (TERPRI)
          (PROGN
           (PRIN2 "The suggested solution of the algebraic system which will")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "do the transformation is: ") NIL)
          (TERPRI))
         (ASSGNPRI (AEVAL H2) NIL 'ONLY)
         (COND
          ((BOOLVALUE* (REVALX (LIST 'YESP "Is the solution ok?")))
           (AEVAL 'NIL))
          (T
           (PROGN
            (PROGN
             (PRIN2 "Please enter the solution in form of a list {")
             (PRIN2 (REVAL1 (AEVAL (LIST 'FIRST XLIST)) T))
             (PRIN2 "=...,...")
             (PRIN2 (REVAL1 (AEVAL (LIST 'FIRST YLIST)) T))
             (PRIN2 "=...,...} or enter a ")
             (PRIN2 "semicolon ; to end this investigation:")
             NIL)
            (SETQ H2 (AEVAL (LIST 'TERMXREAD)))))))))
      (RETURN (AEVAL H2)))) 
(PUT 'SWITCH_R_S 'NUMBER-OF-ARGS 5) 
(FLAG '(SWITCH_R_S) 'OPFN) 
(PUT 'SWITCH_R_S 'DEFINED-ON-LINE '478) 
(PUT 'SWITCH_R_S 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'SWITCH_R_S 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWITCH_R_S (H2 SMV YLIST U V)
    (PROG (XSLIST YSLIST EL3 H)
      (SETQ XSLIST (AEVAL (LIST 'LIST)))
      (SETQ YSLIST (AEVAL (LIST 'LIST)))
      (PROG (EL3)
        (SETQ EL3 (GETRLIST (AEVAL H2)))
       LAB
        (COND ((NULL EL3) (RETURN NIL)))
        ((LAMBDA (EL3)
           (COND
            ((FREEOF (REVALX YLIST) (REVALX (LIST 'LHS EL3)))
             (SETQ XSLIST (AEVAL (LIST 'CONS EL3 XSLIST))))
            (T (SETQ YSLIST (AEVAL (LIST 'CONS EL3 YSLIST))))))
         (CAR EL3))
        (SETQ EL3 (CDR EL3))
        (GO LAB))
      (PROGN
       (TERPRI)
       (PROGN
        (PRIN2 "In the intended transformation shown above")
        (PRIN2 " the dependent ")
        NIL)
       (TERPRI)
       (COND
        ((GREATERP (LENGTH YSLIST) 2)
         (PROGN
          (PRIN2 "variables are the ")
          (PRIN2 (REVAL1 (AEVAL U) T))
          (PRIN2 "i and ")
          NIL))
        (T
         (PROGN
          (PRIN2 "variable is ")
          (PRIN2 (REVAL1 (AEVAL U) T))
          (PRIN2 " and ")
          NIL)))
       (COND
        ((GREATERP (LENGTH XSLIST) 2)
         (PROGN
          (PRIN2 "the independent variables are the ")
          (PRIN2 (REVAL1 (AEVAL V) T))
          (PRIN2 "i.")
          NIL))
        (T
         (PROGN
          (PRIN2 "the independent variable is ")
          (PRIN2 (REVAL1 (AEVAL V) T))
          (PRIN2 ".")
          NIL)))
       (TERPRI)
       (PROGN
        (PRIN2 "The symmetry variable is ")
        (PRIN2 (REVAL1 (AEVAL SMV) T))
        (PRIN2 ", i.e. the ")
        (PRIN2 "transformed expression")
        NIL)
       (TERPRI)
       (PROGN
        (PRIN2 "will be free of ")
        (PRIN2 (REVAL1 (AEVAL SMV) T))
        (PRIN2 ".")
        NIL)
       (TERPRI))
      (SETQ H
              (COND
               ((BOOLVALUE*
                 (REVALX
                  (LIST 'YESP
                        "Is this selection of dependent and independent variables ok?")))
                (AEVAL 'NIL))
               (T
                (PROGN
                 (PROGN
                  (PROGN
                   (PRIN2
                    "Please enter a list of substitutions. For example, to")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "make the variable, which is so far call u1, to an")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "independent variable v2 and the variable, which is ")
                   NIL)
                  (TERPRI)
                  (PROGN
                   (PRIN2 "so far called v2, to an dependent variable u1, ")
                   NIL)
                  (TERPRI)
                  (PROGN (PRIN2 "enter: `{u1=v2, v2=u1};'") NIL))
                 (AEVAL (LIST 'TERMXREAD))))))
      (COND
       ((AND (BOOLVALUE* H) (EVALNEQ (AEVAL H) (AEVAL (LIST 'LIST))))
        (PROGN
         (SETQ XSLIST (AEVAL (LIST 'SUB H XSLIST)))
         (SETQ YSLIST (AEVAL (LIST 'SUB H YSLIST)))
         (SETQ SMV (AEVAL (LIST 'SUB H SMV))))))
      (RETURN (AEVAL (LIST 'LIST XSLIST YSLIST SMV))))) 
(PUT 'MAKEPDE 'NUMBER-OF-ARGS 2) 
(FLAG '(MAKEPDE) 'OPFN) 
(PUT 'MAKEPDE 'DEFINED-ON-LINE '523) 
(PUT 'MAKEPDE 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'MAKEPDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKEPDE (GENLIST U_)
    (PROG (H EL2 EL3 VARI BV)
      (SETQ VARI (AEVAL (LIST 'LIST)))
      (RETURN
       (AEVAL
        (LIST 'CONS
              (LIST 'NUM
                    (PROG (EL2 FORALL-RESULT)
                      (SETQ EL2 (GETRLIST (AEVAL GENLIST)))
                      (SETQ FORALL-RESULT 0)
                     LAB1
                      (COND ((NULL EL2) (RETURN FORALL-RESULT)))
                      (SETQ FORALL-RESULT
                              (AEVAL*
                               (LIST 'PLUS
                                     ((LAMBDA (EL2)
                                        (PROGN
                                         (SETQ H (AEVAL (LIST 'LHS EL2)))
                                         (SETQ H
                                                 (PROGN
                                                  (SETQ EL3
                                                          (EXPLODE
                                                           (REVAL1 (AEVAL H)
                                                                   T)))
                                                  (SETQ BV T)
                                                  (PROG ()
                                                   WHILELABEL
                                                    (COND
                                                     ((NOT BV) (RETURN NIL)))
                                                    (PROGN
                                                     (COND
                                                      ((EQUAL (CAR EL3) '_)
                                                       (SETQ BV NIL)))
                                                     (SETQ EL3 (CDR EL3)))
                                                    (GO WHILELABEL))
                                                  (INTERN (COMPRESS EL3))))
                                         (AEVAL (DEPEND (LIST U_ H)))
                                         (SETQ VARI
                                                 (AEVAL (LIST 'CONS H VARI)))
                                         (AEVAL
                                          (LIST 'TIMES (LIST 'RHS EL2)
                                                (LIST 'DF U_ H)))))
                                      (CAR EL2))
                                     FORALL-RESULT)))
                      (SETQ EL2 (CDR EL2))
                      (GO LAB1)))
              VARI))))) 
(PUT 'TOTDEGLIST 'NUMBER-OF-ARGS 2) 
(FLAG '(TOTDEGLIST) 'OPFN) 
(PUT 'TOTDEGLIST 'DEFINED-ON-LINE '548) 
(PUT 'TOTDEGLIST 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'TOTDEGLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTDEGLIST (EQLIST YLIST)
    (PROG (N ORDR E1 E2)
      (SETQ ORDR (AEVAL 0))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL EQLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROG (E2)
             (SETQ E2 (GETRLIST (AEVAL YLIST)))
            LAB
             (COND ((NULL E2) (RETURN NIL)))
             ((LAMBDA (E2)
                (PROGN
                 (SETQ N (AEVAL (LIST 'TOTDEG E1 E2)))
                 (COND
                  ((EVALGREATERP (AEVAL N) (AEVAL ORDR))
                   (SETQ ORDR (AEVAL N))))))
              (CAR E2))
             (SETQ E2 (CDR E2))
             (GO LAB)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (RETURN (AEVAL ORDR)))) 
(PUT 'SIMILARITY 'NUMBER-OF-ARGS 5) 
(FLAG '(SIMILARITY) 'OPFN) 
(PUT 'SIMILARITY 'DEFINED-ON-LINE '560) 
(PUT 'SIMILARITY 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'SIMILARITY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMILARITY (PROBLEM GENLIST CON U V)
    (PROG (VARI PDE EL1 EL2 EL3 EL4 COPGEN SYMVARFOUND TRANS1 TRANS2 I J H H2 N
           DENEW XLIST YLIST EQLIST ULIST VLIST NX NY XSLIST YSLIST SMV
           TRAFOPROB TR_AS)
      (SETK 'CPU (AEVAL (TIME)))
      (SETK 'GC (AEVAL (GCTIME)))
      (SETQ EQLIST (AEVAL (LIST 'MAKLIST (LIST 'FIRST PROBLEM))))
      (SETQ YLIST (AEVAL (LIST 'MAKLIST (LIST 'SECOND PROBLEM))))
      (SETQ NY (AEVAL (LIST 'LENGTH YLIST)))
      (SETQ XLIST (AEVAL (LIST 'MAKLIST (LIST 'THIRD PROBLEM))))
      (SETQ NX (AEVAL (LIST 'LENGTH XLIST)))
      (SETQ TRAFOPROB (AEVAL (LIST 'LIST PROBLEM 'NIL)))
      (SETQ PROBLEM (AEVAL 'NIL))
      (SETQ EQLIST
              (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL1 (GETRLIST (AEVAL EQLIST)))
                (COND ((NULL EL1) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL1)
                                    (AEVAL (LIST 'EQU_TO_EXPR EL1)))
                                  (CAR EL1))
                                 NIL)))
               LOOPLABEL
                (SETQ EL1 (CDR EL1))
                (COND ((NULL EL1) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL1) (AEVAL (LIST 'EQU_TO_EXPR EL1)))
                          (CAR EL1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (TR_AS (TERPRI)))
      (SETK 'ORDR (AEVAL (LIST 'TOTDEGLIST EQLIST YLIST)))
      (SETQ VARI (AEVAL (LIST 'APPEND YLIST XLIST)))
      (PROG (EL1)
        (SETQ EL1 (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL EL1) (RETURN NIL)))
        ((LAMBDA (EL1)
           (PROG (EL2)
             (SETQ EL2 (GETRLIST (AEVAL YLIST)))
            LAB
             (COND ((NULL EL2) (RETURN NIL)))
             ((LAMBDA (EL2)
                (COND
                 ((NOT (BOOLVALUE* (REVALX (LIST 'MY_FREEOF EL2 EL1))))
                  (AEVAL (NODEPEND (LIST EL2 EL1))))))
              (CAR EL2))
             (SETQ EL2 (CDR EL2))
             (GO LAB)))
         (CAR EL1))
        (SETQ EL1 (CDR EL1))
        (GO LAB))
      (COND
       (TR_AS
        (PROGN
         (PROGN (PRIN2 "The ODE/PDE (-system) under investigation is :") NIL)
         (TERPRI)
         (PROG (EL1)
           (SETQ EL1 (CDR EQLIST))
          LAB
           (COND ((NULL EL1) (RETURN NIL)))
           ((LAMBDA (EL1)
              (PROGN
               (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
               (ASSGNPRI EL1 NIL 'LAST)))
            (CAR EL1))
           (SETQ EL1 (CDR EL1))
           (GO LAB))
         (TERPRI)
         (PROGN (PRIN2 "for the function(s) : ") NIL)
         (FCTPRINT (CDR (REVAL1 (AEVAL YLIST) T)))
         (PROGN (PRIN2 ".") NIL)
         (TERPRI)
         (TERPRI))))
      (COND
       (TR_AS
        (PROGN
         (COND
          ((GREATERP (LENGTH YLIST) 2)
           (PROGN
            (PRIN2 "It will be looked for new dependent variables ")
            (PRIN2 U)
            (PRIN2 "i ")
            NIL))
          (T
           (PROGN
            (PRIN2 "It will be looked for a new dependent variable ")
            (PRIN2 U)
            NIL)))
         (TERPRI)
         (COND
          ((GREATERP (LENGTH XLIST) 2)
           (PROGN
            (PRIN2 "and independent variables ")
            (PRIN2 V)
            (PRIN2 "i")
            NIL))
          (T (PROGN (PRIN2 "and an independent variable ") (PRIN2 V) NIL)))
         (PROGN (PRIN2 " such that the transformed") NIL)
         (TERPRI)
         (PROGN (PRIN2 "de(-system) does not depend on ") (PRIN2 U) NIL)
         (COND ((GREATERP (LENGTH YLIST) 2) (PROGN (PRIN2 "1") NIL)))
         (PROGN (PRIN2 " or ") (PRIN2 V) NIL)
         (COND ((GREATERP (LENGTH XLIST) 2) (PROGN (PRIN2 "1") NIL)))
         (PROGN (PRIN2 ".") NIL)
         (TERPRI))))
      (SETQ COPGEN (AEVAL (LIST 'LIST)))
      (PROG (EL1)
        (SETQ EL1 (GETRLIST (AEVAL GENLIST)))
       LAB
        (COND ((NULL EL1) (RETURN NIL)))
        ((LAMBDA (EL1)
           (COND
            ((FREEOF (REVALX EQLIST) (REVALX (LIST 'LHS EL1)))
             (SETQ COPGEN (AEVAL (LIST 'CONS EL1 COPGEN))))))
         (CAR EL1))
        (SETQ EL1 (CDR EL1))
        (GO LAB))
      (SETQ PDE (AEVAL (LIST 'FIRST (LIST 'MAKEPDE COPGEN 'U_))))
      (SETQ TRANS2 (AEVAL (LIST 'LIST)))
      (PROGN
       (TERPRI)
       (PROGN (PRIN2 "1. Determination of the similarity variable") NIL)
       (COND ((GREATERP (PLUS NX NY) 2) (PROGN (PRIN2 "s") NIL))))
      (SETQ TRANS1 (AEVAL (LIST 'QUASILINPDE1 PDE 'U_ VARI)))
      (COND
       ((EVALNEQ (AEVAL TRANS1) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ I (AEVAL 0))
         (SETQ TRANS1
                 (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL1 (GETRLIST (AEVAL TRANS1)))
                   (COND ((NULL EL1) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL1)
                                       (PROGN
                                        (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                        (SETQ H
                                                (AEVAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'LENGTH GENLIST)
                                                       1)))
                                        (COND
                                         ((EVALEQUAL (AEVAL H) 1)
                                          (PROGN
                                           (SETQ EL2
                                                   (AEVAL
                                                    (LIST 'NUM
                                                          (LIST 'DIFFERENCE
                                                                (LIST 'FIRST
                                                                      EL1)
                                                                (LIST 'SECOND
                                                                      EL1)))))
                                           (COND
                                            ((FREEOF (REVALX EL2) (REVALX 'U_))
                                             (SETQ EL2
                                                     (AEVAL
                                                      (LIST 'NUM
                                                            (LIST 'DIFFERENCE
                                                                  (LIST 'FIRST
                                                                        EL1)
                                                                  (LIST 'TIMES
                                                                        2
                                                                        (LIST
                                                                         'SECOND
                                                                         EL1))))))))
                                           (PROGN
                                            (PROGN
                                             (PRIN2
                                              "A suggestion for this function ff provides:")
                                             NIL)
                                            (TERPRI))
                                           (PROGN
                                            (ASSGNPRI (AEVAL "0 = ") NIL
                                                      'FIRST)
                                            (ASSGNPRI (AEVAL EL2) NIL 'LAST))
                                           (COND
                                            ((BOOLVALUE*
                                              (REVALX
                                               (LIST 'YESP
                                                     "Do you like this choice?")))
                                             (AEVAL (LIST 'LIST EL2)))
                                            (T
                                             (PROGN
                                              (REPEAT
                                               (PROGN
                                                (PROGN
                                                 (PROGN
                                                  (PRIN2
                                                   "Put in an alternative expression which ")
                                                  NIL)
                                                 (TERPRI)
                                                 (PROGN
                                                  (PRIN2
                                                   "- is functionally dependent only on elements of")
                                                  (PRIN2 " ff given above and")
                                                  NIL)
                                                 (TERPRI)
                                                 (PROGN
                                                  (PRIN2
                                                   "- depends on U_ and if set to zero determines U_")
                                                  NIL)
                                                 (TERPRI))
                                                (SETQ H
                                                        (AEVAL*
                                                         (LIST 'TERMXREAD)))
                                                (AEVAL* 'NIL))
                                               (NOT
                                                (FREEOF (REVALX H)
                                                        (REVALX 'U_))))
                                              (AEVAL (LIST 'LIST H)))))))
                                         (T
                                          (PROGN
                                           (PROGN
                                            (SETQ EL2
                                                    (AEVAL
                                                     (LIST 'EINFACHST EL1
                                                           'U_)))
                                            (SETQ H2 (AEVAL (LIST 'LIST)))
                                            (PROG (EL3)
                                              (SETQ EL3 (GETRLIST (AEVAL EL1)))
                                             LAB
                                              (COND ((NULL EL3) (RETURN NIL)))
                                              ((LAMBDA (EL3)
                                                 (COND
                                                  ((EVALNEQ (AEVAL EL3)
                                                            (AEVAL EL2))
                                                   (SETQ H2
                                                           (AEVAL
                                                            (LIST 'CONS
                                                                  (LIST 'NUM
                                                                        (LIST
                                                                         'DIFFERENCE
                                                                         EL2
                                                                         EL3))
                                                                  H2))))))
                                               (CAR EL3))
                                              (SETQ EL3 (CDR EL3))
                                              (GO LAB)))
                                           (PROGN
                                            (PRIN2
                                             "A suggestion for these functions ffi in form of a list ")
                                            (PRIN2 "{ff1,ff2,... } is: ")
                                            NIL)
                                           (TERPRI)
                                           (DEPRINT
                                            (CDR (REVAL1 (AEVAL H2) T)))
                                           (COND
                                            ((YESP "Do you like this choice?")
                                             (AEVAL H2))
                                            (T
                                             (PROGN
                                              (PROGN
                                               (PRIN2
                                                "Put in an alternative list of expression which")
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2
                                                "- are functionally dependent only on the above ")
                                               (PRIN2 "arguments and")
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2
                                                "- which if set to zero determine U_i, i.e.")
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2
                                                "- the functional determinant of these expressions")
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2
                                                "  including U_ from above taken w.r.t. ")
                                               (PRIN2
                                                (CDR
                                                 (REVAL1
                                                  (AEVAL
                                                   (LIST 'APPEND YLIST XLIST))
                                                  T)))
                                               NIL)
                                              (TERPRI)
                                              (PROGN
                                               (PRIN2 "  must not vanish.")
                                               NIL)
                                              (TERPRI)
                                              (SETK H2 (TERMXREAD))))))))))
                                     (CAR EL1))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL1 (CDR EL1))
                   (COND ((NULL EL1) (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL1)
                               (PROGN
                                (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                (SETQ H
                                        (AEVAL
                                         (LIST 'DIFFERENCE
                                               (LIST 'LENGTH GENLIST) 1)))
                                (COND
                                 ((EVALEQUAL (AEVAL H) 1)
                                  (PROGN
                                   (SETQ EL2
                                           (AEVAL
                                            (LIST 'NUM
                                                  (LIST 'DIFFERENCE
                                                        (LIST 'FIRST EL1)
                                                        (LIST 'SECOND EL1)))))
                                   (COND
                                    ((FREEOF (REVALX EL2) (REVALX 'U_))
                                     (SETQ EL2
                                             (AEVAL
                                              (LIST 'NUM
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'FIRST EL1)
                                                          (LIST 'TIMES 2
                                                                (LIST 'SECOND
                                                                      EL1))))))))
                                   (PROGN
                                    (PROGN
                                     (PRIN2
                                      "A suggestion for this function ff provides:")
                                     NIL)
                                    (TERPRI))
                                   (PROGN
                                    (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
                                    (ASSGNPRI (AEVAL EL2) NIL 'LAST))
                                   (COND
                                    ((BOOLVALUE*
                                      (REVALX
                                       (LIST 'YESP
                                             "Do you like this choice?")))
                                     (AEVAL (LIST 'LIST EL2)))
                                    (T
                                     (PROGN
                                      (REPEAT
                                       (PROGN
                                        (PROGN
                                         (PROGN
                                          (PRIN2
                                           "Put in an alternative expression which ")
                                          NIL)
                                         (TERPRI)
                                         (PROGN
                                          (PRIN2
                                           "- is functionally dependent only on elements of")
                                          (PRIN2 " ff given above and")
                                          NIL)
                                         (TERPRI)
                                         (PROGN
                                          (PRIN2
                                           "- depends on U_ and if set to zero determines U_")
                                          NIL)
                                         (TERPRI))
                                        (SETQ H (AEVAL* (LIST 'TERMXREAD)))
                                        (AEVAL* 'NIL))
                                       (NOT (FREEOF (REVALX H) (REVALX 'U_))))
                                      (AEVAL (LIST 'LIST H)))))))
                                 (T
                                  (PROGN
                                   (PROGN
                                    (SETQ EL2
                                            (AEVAL (LIST 'EINFACHST EL1 'U_)))
                                    (SETQ H2 (AEVAL (LIST 'LIST)))
                                    (PROG (EL3)
                                      (SETQ EL3 (GETRLIST (AEVAL EL1)))
                                     LAB
                                      (COND ((NULL EL3) (RETURN NIL)))
                                      ((LAMBDA (EL3)
                                         (COND
                                          ((EVALNEQ (AEVAL EL3) (AEVAL EL2))
                                           (SETQ H2
                                                   (AEVAL
                                                    (LIST 'CONS
                                                          (LIST 'NUM
                                                                (LIST
                                                                 'DIFFERENCE
                                                                 EL2 EL3))
                                                          H2))))))
                                       (CAR EL3))
                                      (SETQ EL3 (CDR EL3))
                                      (GO LAB)))
                                   (PROGN
                                    (PRIN2
                                     "A suggestion for these functions ffi in form of a list ")
                                    (PRIN2 "{ff1,ff2,... } is: ")
                                    NIL)
                                   (TERPRI)
                                   (DEPRINT (CDR (REVAL1 (AEVAL H2) T)))
                                   (COND
                                    ((YESP "Do you like this choice?")
                                     (AEVAL H2))
                                    (T
                                     (PROGN
                                      (PROGN
                                       (PRIN2
                                        "Put in an alternative list of expression which")
                                       NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2
                                        "- are functionally dependent only on the above ")
                                       (PRIN2 "arguments and")
                                       NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2
                                        "- which if set to zero determine U_i, i.e.")
                                       NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2
                                        "- the functional determinant of these expressions")
                                       NIL)
                                      (TERPRI)
                                      (PROGN
                                       (PRIN2
                                        "  including U_ from above taken w.r.t. ")
                                       (PRIN2
                                        (CDR
                                         (REVAL1
                                          (AEVAL (LIST 'APPEND YLIST XLIST))
                                          T)))
                                       NIL)
                                      (TERPRI)
                                      (PROGN (PRIN2 "  must not vanish.") NIL)
                                      (TERPRI)
                                      (SETK H2 (TERMXREAD))))))))))
                             (CAR EL1))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ PDE (AEVAL (LIST 'DIFFERENCE PDE 1)))
         (PROGN
          (TERPRI)
          (PROGN (PRIN2 "2. Determination of the symmetry variable") NIL))
         (SETQ TRANS2 (AEVAL (LIST 'QUASILINPDE1 PDE 'U_ VARI)))
         (COND
          ((AND (EVALEQUAL (AEVAL (LIST 'LENGTH XLIST)) 1)
                (EVALEQUAL (AEVAL TRANS2) (AEVAL (LIST 'LIST))))
           (PROG (E1)
             (SETQ E1 (GETRLIST (AEVAL (LIST 'FARGS 'U_))))
            LAB
             (COND ((NULL E1) (RETURN NIL)))
             ((LAMBDA (E1) (AEVAL (NODEPEND (LIST 'U_ E1)))) (CAR E1))
             (SETQ E1 (CDR E1))
             (GO LAB)))
          (T
           (PROGN
            (COND
             ((EVALEQUAL (AEVAL TRANS2) (AEVAL (LIST 'LIST)))
              (PROGN
               (SETQ H (AEVAL (LIST 'REVERSE VARI)))
               (WHILE
                (EVALEQUAL
                 (AEVAL*
                  (LIST 'PLUS 1
                        (LIST 'SUB (LIST 'EQUAL 'U_ (LIST 'FIRST H)) PDE)))
                 0)
                (SETQ H (AEVAL* (LIST 'REST H))))
               (PROG (E1)
                 (SETQ E1 (GETRLIST (AEVAL (LIST 'FARGS 'U_))))
                LAB
                 (COND ((NULL E1) (RETURN NIL)))
                 ((LAMBDA (E1) (AEVAL (NODEPEND (LIST 'U_ E1)))) (CAR E1))
                 (SETQ E1 (CDR E1))
                 (GO LAB))
               (SETQ H (AEVAL (LIST 'FIRST H)))
               (SETQ TRANS2 (AEVAL (LIST 'LIST (LIST 'DIFFERENCE 'U_ H))))
               (PROGN
                (PROGN
                 (PRIN2 "Because the correct symmetry variable was not ")
                 (PRIN2 "found, the program will")
                 NIL)
                (TERPRI)
                (PROGN
                 (PRIN2 "take ")
                 (PRIN2 (REVAL1 (AEVAL H) T))
                 (PRIN2 " instead with the consequence ")
                 (PRIN2 "that not the whole transformed ")
                 NIL)
                (TERPRI)
                (PROGN
                 (PRIN2 "PDE will be free of ")
                 (PRIN2 (REVAL1 (AEVAL H) T))
                 (PRIN2 " but only those ")
                 (PRIN2 "terms without ")
                 (PRIN2 (REVAL1 (AEVAL H) T))
                 (PRIN2 "-derivative")
                 NIL)
                (TERPRI)
                (PROGN
                 (PRIN2 "which is still of use for finding special ")
                 (PRIN2 (REVAL1 (AEVAL H) T))
                 (PRIN2 "-independent solutions ")
                 NIL)
                (TERPRI)
                (PROGN (PRIN2 "of the PDE.") NIL))))
             (T
              (PROGN
               (PROG (E1)
                 (SETQ E1 (GETRLIST (AEVAL (LIST 'FARGS 'U_))))
                LAB
                 (COND ((NULL E1) (RETURN NIL)))
                 ((LAMBDA (E1) (AEVAL (NODEPEND (LIST 'U_ E1)))) (CAR E1))
                 (SETQ E1 (CDR E1))
                 (GO LAB))
               (SETQ SYMVARFOUND (AEVAL 'T))
               (SETQ I (AEVAL 0))
               (SETQ TRANS2
                       (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                         (SETQ EL1 (GETRLIST (AEVAL TRANS2)))
                         (COND ((NULL EL1) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (EL1)
                                             (PROGN
                                              (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                              (PROGN
                                               (PROGN
                                                (PRIN2
                                                 "A suggestion for this function ff(..) yields:")
                                                NIL)
                                               (TERPRI))
                                              (SETQ H
                                                      (AEVAL
                                                       (LIST 'EINFACHST EL1
                                                             'U_)))
                                              (COND
                                               ((BOOLVALUE*
                                                 (PROGN
                                                  (SETQ H2
                                                          (REVAL1
                                                           (REVALX
                                                            (LIST 'NUM H))
                                                           T))
                                                  (OR (NOT (PAIRP H2))
                                                      (NEQ (CAR H2) 'PLUS))))
                                                (SETQ H
                                                        (AEVAL
                                                         (LIST 'NUM
                                                               (LIST 'PLUS H
                                                                     1))))))
                                              (PROGN
                                               (ASSGNPRI (AEVAL "0 = ") NIL
                                                         'FIRST)
                                               (ASSGNPRI (AEVAL H) NIL 'LAST))
                                              (COND
                                               ((BOOLVALUE*
                                                 (REVALX
                                                  (LIST 'YESP
                                                        "Do you like this choice?")))
                                                (AEVAL H))
                                               (T
                                                (PROGN
                                                 (REPEAT
                                                  (PROGN
                                                   (PROGN
                                                    (PROGN
                                                     (PRIN2
                                                      "Put in an alternative expression which ")
                                                     NIL)
                                                    (TERPRI)
                                                    (PROGN
                                                     (PRIN2
                                                      "- is functionally dependent only on arguments of")
                                                     (PRIN2
                                                      " ff given above and")
                                                     NIL)
                                                    (TERPRI)
                                                    (PROGN
                                                     (PRIN2
                                                      "- depends on u_ and if set to zero determines u_")
                                                     NIL)
                                                    (TERPRI))
                                                   (SETQ H
                                                           (AEVAL*
                                                            (LIST 'TERMXREAD)))
                                                   (AEVAL* 'NIL))
                                                  (NOT
                                                   (FREEOF (REVALX H)
                                                           (REVALX 'U_))))
                                                 (AEVAL H))))))
                                           (CAR EL1))
                                          NIL)))
                        LOOPLABEL
                         (SETQ EL1 (CDR EL1))
                         (COND
                          ((NULL EL1) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL1)
                                     (PROGN
                                      (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                      (PROGN
                                       (PROGN
                                        (PRIN2
                                         "A suggestion for this function ff(..) yields:")
                                        NIL)
                                       (TERPRI))
                                      (SETQ H
                                              (AEVAL
                                               (LIST 'EINFACHST EL1 'U_)))
                                      (COND
                                       ((BOOLVALUE*
                                         (PROGN
                                          (SETQ H2
                                                  (REVAL1
                                                   (REVALX (LIST 'NUM H)) T))
                                          (OR (NOT (PAIRP H2))
                                              (NEQ (CAR H2) 'PLUS))))
                                        (SETQ H
                                                (AEVAL
                                                 (LIST 'NUM
                                                       (LIST 'PLUS H 1))))))
                                      (PROGN
                                       (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
                                       (ASSGNPRI (AEVAL H) NIL 'LAST))
                                      (COND
                                       ((BOOLVALUE*
                                         (REVALX
                                          (LIST 'YESP
                                                "Do you like this choice?")))
                                        (AEVAL H))
                                       (T
                                        (PROGN
                                         (REPEAT
                                          (PROGN
                                           (PROGN
                                            (PROGN
                                             (PRIN2
                                              "Put in an alternative expression which ")
                                             NIL)
                                            (TERPRI)
                                            (PROGN
                                             (PRIN2
                                              "- is functionally dependent only on arguments of")
                                             (PRIN2 " ff given above and")
                                             NIL)
                                            (TERPRI)
                                            (PROGN
                                             (PRIN2
                                              "- depends on u_ and if set to zero determines u_")
                                             NIL)
                                            (TERPRI))
                                           (SETQ H (AEVAL* (LIST 'TERMXREAD)))
                                           (AEVAL* 'NIL))
                                          (NOT
                                           (FREEOF (REVALX H) (REVALX 'U_))))
                                         (AEVAL H))))))
                                   (CAR EL1))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
            (SETQ EL1 (AEVAL (LIST 'FIRST TRANS1)))
            (SETQ EL2 (AEVAL (LIST 'FIRST TRANS2)))
            (SETQ YSLIST (AEVAL (LIST 'GROUPING EL1 EL2 XLIST YLIST NX NY)))
            (SETQ XSLIST (AEVAL (LIST 'FIRST YSLIST)))
            (SETQ YSLIST (AEVAL (LIST 'SECOND YSLIST)))
            (SETQ SMV (AEVAL (LIST 'RENAME_U_ XSLIST YSLIST EL2 'U_ U V)))
            (SETQ XSLIST (AEVAL (LIST 'FIRST SMV)))
            (SETQ YSLIST (AEVAL (LIST 'SECOND SMV)))
            (SETQ VLIST (AEVAL (LIST 'THIRD SMV)))
            (SETQ ULIST (AEVAL (LIST 'PART SMV 4)))
            (SETQ SMV (AEVAL (LIST 'PART SMV 5)))
            (SETQ H2
                    (AEVAL
                     (LIST 'SOLVE_FOR_OLD_VAR XSLIST YSLIST XLIST YLIST NX
                           NY)))
            (COND
             ((EVALNEQ (AEVAL H2) (AEVAL 'NIL))
              (PROGN
               (SETQ SMV (AEVAL (LIST 'SWITCH_R_S H2 SMV YLIST U V)))
               (SETQ XSLIST (AEVAL (LIST 'FIRST SMV)))
               (SETQ YSLIST (AEVAL (LIST 'SECOND SMV)))
               (SETQ SMV (AEVAL (LIST 'THIRD SMV)))
               (PROG (EL3)
                 (SETQ EL3 (GETRLIST (AEVAL ULIST)))
                LAB
                 (COND ((NULL EL3) (RETURN NIL)))
                 ((LAMBDA (EL3)
                    (PROGN
                     (PROG (EL4)
                       (SETQ EL4 (GETRLIST (AEVAL (LIST 'FARGS EL3))))
                      LAB
                       (COND ((NULL EL4) (RETURN NIL)))
                       ((LAMBDA (EL4) (AEVAL (NODEPEND (LIST EL3 EL4))))
                        (CAR EL4))
                       (SETQ EL4 (CDR EL4))
                       (GO LAB))
                     (PROG (EL4)
                       (SETQ EL4 (GETRLIST (AEVAL VLIST)))
                      LAB
                       (COND ((NULL EL4) (RETURN NIL)))
                       ((LAMBDA (EL4) (AEVAL (DEPEND (LIST EL3 EL4))))
                        (CAR EL4))
                       (SETQ EL4 (CDR EL4))
                       (GO LAB))))
                  (CAR EL3))
                 (SETQ EL3 (CDR EL3))
                 (GO LAB))
               (PROG (EL3)
                 (SETQ EL3 (GETRLIST (AEVAL YLIST)))
                LAB
                 (COND ((NULL EL3) (RETURN NIL)))
                 ((LAMBDA (EL3)
                    (PROG (EL4)
                      (SETQ EL4 (GETRLIST (AEVAL XLIST)))
                     LAB
                      (COND ((NULL EL4) (RETURN NIL)))
                      ((LAMBDA (EL4) (AEVAL (DEPEND (LIST EL3 EL4))))
                       (CAR EL4))
                      (SETQ EL4 (CDR EL4))
                      (GO LAB)))
                  (CAR EL3))
                 (SETQ EL3 (CDR EL3))
                 (GO LAB))
               (SETQ EQLIST
                       (AEVAL
                        (LIST 'DETRAFO EQLIST YSLIST XSLIST ULIST VLIST)))
               (COND
                (TR_AS
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "The transformed equation") NIL)
                  (COND
                   ((GREATERP (LENGTH (AEVAL EQLIST)) 2)
                    (PROGN (PRIN2 "s") NIL)))
                  (COND
                   (SYMVARFOUND
                    (PROGN
                     (PRIN2 " which should be free of ")
                     (PRIN2 (REVAL1 (AEVAL SMV) T))
                     (PRIN2 ":")
                     NIL))
                   (T
                    (PROGN
                     (PRIN2 " in which the terms without ")
                     (PRIN2 (REVAL1 (AEVAL SMV) T))
                     (PRIN2 "-derivative are free of ")
                     (PRIN2 (REVAL1 (AEVAL SMV) T))
                     (PRIN2 ":")
                     NIL)))
                  (TERPRI))))
               (SETQ EQLIST
                       (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                         (SETQ EL3 (GETRLIST (AEVAL EQLIST)))
                         (COND ((NULL EL3) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (EL3)
                                             (PROGN
                                              (SETQ EL3
                                                      (AEVAL
                                                       (LIST 'FACTORIZE
                                                             (LIST 'NUM EL3))))
                                              (PROG (EL4 FORALL-RESULT)
                                                (SETQ EL4
                                                        (GETRLIST (AEVAL EL3)))
                                                (SETQ FORALL-RESULT 1)
                                               LAB1
                                                (COND
                                                 ((NULL EL4)
                                                  (RETURN FORALL-RESULT)))
                                                (SETQ FORALL-RESULT
                                                        (AEVAL*
                                                         (LIST 'TIMES
                                                               ((LAMBDA (EL4)
                                                                  (COND
                                                                   ((EVALEQUAL
                                                                     0
                                                                     (AEVAL
                                                                      (LIST
                                                                       'TOTDEGLIST
                                                                       (LIST
                                                                        'LIST
                                                                        EL4)
                                                                       ULIST)))
                                                                    1)
                                                                   (T
                                                                    (AEVAL
                                                                     EL4))))
                                                                (CAR EL4))
                                                               FORALL-RESULT)))
                                                (SETQ EL4 (CDR EL4))
                                                (GO LAB1))))
                                           (CAR EL3))
                                          NIL)))
                        LOOPLABEL
                         (SETQ EL3 (CDR EL3))
                         (COND
                          ((NULL EL3) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL3)
                                     (PROGN
                                      (SETQ EL3
                                              (AEVAL
                                               (LIST 'FACTORIZE
                                                     (LIST 'NUM EL3))))
                                      (PROG (EL4 FORALL-RESULT)
                                        (SETQ EL4 (GETRLIST (AEVAL EL3)))
                                        (SETQ FORALL-RESULT 1)
                                       LAB1
                                        (COND
                                         ((NULL EL4) (RETURN FORALL-RESULT)))
                                        (SETQ FORALL-RESULT
                                                (AEVAL*
                                                 (LIST 'TIMES
                                                       ((LAMBDA (EL4)
                                                          (COND
                                                           ((EVALEQUAL 0
                                                                       (AEVAL
                                                                        (LIST
                                                                         'TOTDEGLIST
                                                                         (LIST
                                                                          'LIST
                                                                          EL4)
                                                                         ULIST)))
                                                            1)
                                                           (T (AEVAL EL4))))
                                                        (CAR EL4))
                                                       FORALL-RESULT)))
                                        (SETQ EL4 (CDR EL4))
                                        (GO LAB1))))
                                   (CAR EL3))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (AEVAL (DEPRINT (CDR (REVAL1 (AEVAL EQLIST) T))))
               (COND
                ((AND (EVALGREATERP (AEVAL (LIST 'LENGTH VLIST)) 1)
                      (NOT (FREEOF (REVALX VLIST) (REVALX SMV))))
                 (PROGN
                  (SETQ VLIST
                          (AEVAL
                           (LIST 'CONS SMV
                                 (DELETE (REVAL1 (AEVAL SMV) T)
                                         (REVAL1 (AEVAL VLIST) T)))))
                  (COND
                   ((BOOLVALUE*
                     (REVALX
                      (LIST 'YESP
                            "Shall the dependence on the symmetry variable be dropped?")))
                    (PROGN
                     (PROG (EL3)
                       (SETQ EL3 (GETRLIST (AEVAL ULIST)))
                      LAB
                       (COND ((NULL EL3) (RETURN NIL)))
                       ((LAMBDA (EL3)
                          (COND
                           ((NOT
                             (BOOLVALUE* (REVALX (LIST 'MY_FREEOF EL3 SMV))))
                            (AEVAL (NODEPEND (LIST EL3 SMV))))))
                        (CAR EL3))
                       (SETQ EL3 (CDR EL3))
                       (GO LAB))
                     (SETQ VLIST (AEVAL (LIST 'REST VLIST))))))
                  (SETQ EQLIST
                          (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EL3 (GETRLIST (AEVAL EQLIST)))
                            (COND ((NULL EL3) (RETURN (MAKELIST NIL))))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL3)
                                                (PROGN
                                                 (SETQ EL3
                                                         (AEVAL
                                                          (LIST 'FACTORIZE
                                                                (LIST 'NUM
                                                                      EL3))))
                                                 (PROG (EL4 FORALL-RESULT)
                                                   (SETQ EL4
                                                           (GETRLIST
                                                            (AEVAL EL3)))
                                                   (SETQ FORALL-RESULT 1)
                                                  LAB1
                                                   (COND
                                                    ((NULL EL4)
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'TIMES
                                                                  ((LAMBDA
                                                                       (EL4)
                                                                     (COND
                                                                      ((EVALEQUAL
                                                                        0
                                                                        (AEVAL
                                                                         (LIST
                                                                          'TOTDEGLIST
                                                                          (LIST
                                                                           'LIST
                                                                           EL4)
                                                                          ULIST)))
                                                                       1)
                                                                      (T
                                                                       (AEVAL
                                                                        EL4))))
                                                                   (CAR EL4))
                                                                  FORALL-RESULT)))
                                                   (SETQ EL4 (CDR EL4))
                                                   (GO LAB1))))
                                              (CAR EL3))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EL3 (CDR EL3))
                            (COND
                             ((NULL EL3) (RETURN (CONS 'LIST FORALL-RESULT))))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL3)
                                        (PROGN
                                         (SETQ EL3
                                                 (AEVAL
                                                  (LIST 'FACTORIZE
                                                        (LIST 'NUM EL3))))
                                         (PROG (EL4 FORALL-RESULT)
                                           (SETQ EL4 (GETRLIST (AEVAL EL3)))
                                           (SETQ FORALL-RESULT 1)
                                          LAB1
                                           (COND
                                            ((NULL EL4)
                                             (RETURN FORALL-RESULT)))
                                           (SETQ FORALL-RESULT
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          ((LAMBDA (EL4)
                                                             (COND
                                                              ((EVALEQUAL 0
                                                                          (AEVAL
                                                                           (LIST
                                                                            'TOTDEGLIST
                                                                            (LIST
                                                                             'LIST
                                                                             EL4)
                                                                            ULIST)))
                                                               1)
                                                              (T (AEVAL EL4))))
                                                           (CAR EL4))
                                                          FORALL-RESULT)))
                                           (SETQ EL4 (CDR EL4))
                                           (GO LAB1))))
                                      (CAR EL3))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))))
               (SETQ TRAFOPROB
                       (AEVAL
                        (LIST 'LIST (LIST 'LIST EQLIST ULIST VLIST)
                              (LIST 'APPEND XSLIST YSLIST)))))))))))))
      (PROG (EL1)
        (SETQ EL1 (GETRLIST (AEVAL XLIST)))
       LAB
        (COND ((NULL EL1) (RETURN NIL)))
        ((LAMBDA (EL1)
           (PROG (EL2)
             (SETQ EL2 (GETRLIST (AEVAL YLIST)))
            LAB
             (COND ((NULL EL2) (RETURN NIL)))
             ((LAMBDA (EL2) (AEVAL (DEPEND (LIST EL2 EL1)))) (CAR EL2))
             (SETQ EL2 (CDR EL2))
             (GO LAB)))
         (CAR EL1))
        (SETQ EL1 (CDR EL1))
        (GO LAB))
      (AEVAL (CLEAR (LIST 'FF 'FFI)))
      (RETURN (AEVAL TRAFOPROB)))) 
(PUT 'QUASILINPDE1 'NUMBER-OF-ARGS 3) 
(FLAG '(QUASILINPDE1) 'OPFN) 
(PUT 'QUASILINPDE1 'DEFINED-ON-LINE '894) 
(PUT 'QUASILINPDE1 'DEFINED-IN-FILE 'CRACK/APPLYSYM.RED) 
(PUT 'QUASILINPDE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUASILINPDE1 (PDE U_ VARI)
    (PROG (TRANS1 E1 E2 Q)
      (SETQ TRANS1 (AEVAL (LIST 'QUASILINPDE PDE U_ VARI)))
      (COND
       ((EVALEQUAL (AEVAL TRANS1) (AEVAL (LIST 'LIST)))
        (PROGN
         (PROGN
          (ASSGNPRI
           (AEVAL "The program was not able to find the general solution ") NIL
           'FIRST)
          (ASSGNPRI (AEVAL "of the PDE: ") NIL NIL)
          (ASSGNPRI (AEVAL PDE) NIL NIL)
          (ASSGNPRI (AEVAL " for the function ") NIL NIL)
          (ASSGNPRI (AEVAL U_) NIL NIL)
          (ASSGNPRI (AEVAL ".") NIL 'LAST))
         (PROGN
          (PROGN
           (PRIN2 "Please enter either only a semicolon if no solution ")
           (PRIN2 "is known or enter ")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "the solution of the PDE in form ")
           (PRIN2 "of a list {A1,A2,...} where ")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "the Ai are algebraic expressions in ")
           (PRIN2 (CDR (REVAL1 (AEVAL (LIST 'CONS U_ VARI)) T)))
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "such that any function ff(A1,A2,...) which is not ")
           (PRIN2 "independent of `")
           (PRIN2 U_)
           NIL)
          (PROGN (PRIN2 "'") NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "determines a solution `")
           (PRIN2 U_)
           (PRIN2 "' of the PDE through 0=ff: ")
           NIL))
         (SETQ TRANS1 (AEVAL (LIST 'LIST (LIST 'TERMXREAD))))
         (COND
          ((EVALEQUAL (AEVAL TRANS1) (AEVAL (LIST 'LIST 'NIL)))
           (SETQ TRANS1 (AEVAL (LIST 'LIST))))))))
      (RETURN (AEVAL TRANS1)))) 