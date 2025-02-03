(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID '(REDUCEFUNCTIONS_ PRINT_)) 
(PUT 'PRINT_DROPPING_NOTICE 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_DROPPING_NOTICE 'DEFINED-ON-LINE '35) 
(PUT 'PRINT_DROPPING_NOTICE 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'PRINT_DROPPING_NOTICE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_DROPPING_NOTICE (H)
    (PROG ()
      (PROGN
       (PRIN2 "For this conservation law the characteristic function")
       NIL)
      (COND ((CDDR H) (PROGN (PROGN (PRIN2 "s:") NIL) (ASSGNPRI H NIL 'ONLY)))
            (T
             (PROGN
              (PROGN (PRIN2 ":") NIL)
              (ASSGNPRI (AEVAL (LIST 'FIRST H)) NIL 'ONLY))))
      (PROGN
       (PRIN2
        "involve derivatives of functions of new variables. Currently, conserved")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "currents can not be computed from such ") NIL)
      (COND ((CDDR H) (PROGN (PRIN2 "characteristic functions.") NIL))
            (T (PROGN (PRIN2 "a characteristic function.") NIL)))
      (TERPRI)
      (PROGN
       (PRIN2 "One might transform the original system to these variables,")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "initialize the characteristic functions with the found")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "expressions and run conlaw3 to find the conserved current.")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "======================================================")
       NIL)
      (TERPRI))) 
(PUT 'PRINT_CLAW 'NUMBER-OF-ARGS 4) 
(FLAG '(PRINT_CLAW) 'OPFN) 
(PUT 'PRINT_CLAW 'DEFINED-ON-LINE '56) 
(PUT 'PRINT_CLAW 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'PRINT_CLAW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINT_CLAW (EQLIST QLIST PLIST XLIST)
    (PROG (N)
      (PROGN
       (ASSGNPRI (AEVAL "  ( ") NIL 'FIRST)
       (ASSGNPRI (AEVAL (LIST 'SQFIRST QLIST)) NIL NIL)
       (ASSGNPRI (AEVAL " ) * ( ") NIL NIL)
       (ASSGNPRI (AEVAL (LIST 'SQFIRST EQLIST)) NIL NIL)
       (ASSGNPRI (AEVAL " )") NIL 'LAST))
      (SETQ QLIST (AEVAL (LIST 'SQREST QLIST)))
      (SETQ EQLIST (AEVAL (LIST 'SQREST EQLIST)))
      (WHILE (EVALNEQ (AEVAL* QLIST) (AEVAL* (LIST 'LIST)))
             (PROGN
              (PROGN
               (ASSGNPRI (AEVAL* "+ ( ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* (LIST 'SQFIRST QLIST)) NIL NIL)
               (ASSGNPRI (AEVAL* " ) * ( ") NIL NIL)
               (ASSGNPRI (AEVAL* (LIST 'SQFIRST EQLIST)) NIL NIL)
               (ASSGNPRI (AEVAL* " )") NIL 'LAST))
              (SETQ QLIST (AEVAL* (LIST 'SQREST QLIST)))
              (SETQ EQLIST (AEVAL* (LIST 'SQREST EQLIST)))))
      (ASSGNPRI (AEVAL " = ") NIL 'ONLY)
      (SETQ N (AEVAL (LIST 'LENGTH XLIST)))
      (WHILE (EVALNEQ (AEVAL* PLIST) (AEVAL* (LIST 'LIST)))
             (PROGN
              (COND
               ((EVALLESSP (AEVAL* (LIST 'LENGTH PLIST)) (AEVAL* N))
                (ASSGNPRI (AEVAL* "+") NIL 'ONLY)))
              (PROGN
               (ASSGNPRI (AEVAL* "df( ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* (LIST 'FIRST PLIST)) NIL NIL)
               (ASSGNPRI (AEVAL* ", ") NIL NIL)
               (ASSGNPRI (AEVAL* (LIST 'FIRST XLIST)) NIL NIL)
               (ASSGNPRI (AEVAL* " )") NIL 'LAST))
              (SETQ PLIST (AEVAL* (LIST 'SQREST PLIST)))
              (SETQ XLIST (AEVAL* (LIST 'SQREST XLIST))))))) 
(FLAG '(LHSLI) 'OPFN) 
(PUT 'LHSLI 'NUMBER-OF-ARGS 1) 
(PUT 'LHSLI 'DEFINED-ON-LINE '75) 
(PUT 'LHSLI 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'LHSLI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LHSLI (EQLIST)
    (PROG (LHSLIST1 LHSLIST2 H1 FLG1 FLG2)
      (PROG (H1)
        (SETQ H1 (CDR EQLIST))
       LAB
        (COND ((NULL H1) (RETURN NIL)))
        ((LAMBDA (H1)
           (PROGN
            (SETQ FLG1 NIL)
            (COND
             ((AND (PAIRP H1) (EQUAL (CAR H1) 'EQUAL))
              (PROGN
               (SETQ H1 (REVAL1 (CADR H1) T))
               (COND
                ((AND (PAIRP H1) (EQUAL (CAR H1) 'EXPT) (NUMBERP (CADDR H1)))
                 (PROGN (SETQ FLG2 NIL) (SETQ H1 (CADR H1))))
                (T (SETQ FLG2 T)))
               (COND
                ((AND (NOT (NUMBERP H1))
                      (OR (ATOM H1)
                          (AND (EQUAL (CAR H1) 'DF) (ATOM (CADR H1)))))
                 (PROGN
                  (SETQ LHSLIST1 (CONS H1 LHSLIST1))
                  (COND
                   (FLG2
                    (PROGN
                     (SETQ LHSLIST2 (CONS H1 LHSLIST2))
                     (SETQ FLG1 T))))))))))
            (COND ((NULL FLG1) (SETQ LHSLIST2 (CONS 0 LHSLIST2))))
            NIL))
         (CAR H1))
        (SETQ H1 (CDR H1))
        (GO LAB))
      (RETURN (LIST 'LIST (CONS 'LIST LHSLIST1) (CONS 'LIST LHSLIST2))))) 
(PUT 'CHKSUB 'PSOPFN 'CHKSUBSTITUTION) 
(PUT 'CHKSUBSTITUTION 'NUMBER-OF-ARGS 1) 
(PUT 'CHKSUBSTITUTION 'DEFINED-ON-LINE '105) 
(PUT 'CHKSUBSTITUTION 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'CHKSUBSTITUTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHKSUBSTITUTION (INP)
    (PROG (H1 H2 DERIL COMPLAINT EQLIST ULIST LS RS)
      (SETQ EQLIST (CDR (REVAL1 (CAR INP) NIL)))
      (SETQ ULIST (CDR (REVAL1 (CADR INP) T)))
      (PROG (E1)
        (SETQ E1 EQLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((OR (NOT (PAIRP E1)) (NEQ (CAR E1) 'EQUAL))
             (PROGN
              (PROGN (PRIN2 "THE INPUT EQUATION ") NIL)
              (DEPRINT (LIST E1))
              (PROGN
               (PRIN2
                "DOES NOT HAVE A FORM WHERE ONE DERIVATIVE IS ELIMINATED.")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "AS A CONSEQUENCE CONSERVATION LAWS MAY BE LOST BY NOT")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "USING THE INPUT EQUATIONS TO ELIMINATE JET VARIABLES.")
               NIL)
              (TERPRI)))
            (T
             (PROGN
              (SETQ LS (SIMP (CADR E1)))
              (SETQ RS (SIMP (CADDR E1)))
              (SETQ DERIL
                      (CONS
                       (CONS
                        (APPEND (ALL_DERIV_SEARCH_SF (CAR LS) ULIST)
                                (ALL_DERIV_SEARCH_SF (CDR LS) ULIST))
                        (APPEND (ALL_DERIV_SEARCH_SF (CAR RS) ULIST)
                                (ALL_DERIV_SEARCH_SF (CDR RS) ULIST)))
                       DERIL))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROG (E1)
        (SETQ E1 DERIL)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((CAR E1)
             (PROGN
              (SETQ H1 (CAAAR E1))
              (PROG (H2)
                (SETQ H2 (CDR E1))
               LAB
                (COND ((NULL H2) (RETURN NIL)))
                ((LAMBDA (H2)
                   (COND
                    ((AND (EQUAL (CAR H1) (CAAR H2))
                          (NULL (WHICH_DERIV (CDAR H2) (CDR H1))))
                     (PROGN
                      (SETQ COMPLAINT T)
                      (PROGN
                       (PRIN2 "The left hand side ")
                       (PRIN2
                        (COND ((EQUAL (LENGTH H1) 1) (CAR H1))
                              (T (CONS 'DF H1))))
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2 " is not a leading derivative in its equation!")
                       NIL)
                      (TERPRI)))))
                 (CAR H2))
                (SETQ H2 (CDR H2))
                (GO LAB))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       (DERIL
        (PROG ()
         WHILELABEL
          (COND ((NOT (CDR DERIL)) (RETURN NIL)))
          (PROGN
           (COND
            ((CAAR DERIL)
             (PROGN
              (SETQ H1 (CAAAAR DERIL))
              (PROG (H2)
                (SETQ H2 (CDR DERIL))
               LAB
                (COND ((NULL H2) (RETURN NIL)))
                ((LAMBDA (H2)
                   (COND
                    ((AND (CAR H2) (EQUAL (CAR H1) (CAAAAR H2))
                          (OR (NULL (WHICH_DERIV (CDR H1) (CDAAAR H2)))
                              (NULL (WHICH_DERIV (CDAAAR H2) (CDR H1)))))
                     (PROGN
                      (SETQ COMPLAINT T)
                      (PROGN
                       (PRIN2
                        "--> One left hand side (lhs) contains a derivative which")
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        "is equal or a derivative of a derivative on another lhs!")
                       NIL)
                      (TERPRI)
                      NIL))))
                 (CAR H2))
                (SETQ H2 (CDR H2))
                (GO LAB))
              NIL)))
           (SETQ DERIL (CDR DERIL)))
          (GO WHILELABEL))))
      (COND (COMPLAINT (TERPRI))))) 
(FLAG '(LISTDIFDIF2) 'OPFN) 
(PUT 'LISTDIFDIF2 'NUMBER-OF-ARGS 2) 
(PUT 'LISTDIFDIF2 'DEFINED-ON-LINE '186) 
(PUT 'LISTDIFDIF2 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'LISTDIFDIF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTDIFDIF2 (LHSLIST DEPLIST)
    (PROG (H)
      (SETQ DEPLIST (CDR (REVAL1 DEPLIST T)))
      (SETQ LHSLIST (CDR (REVAL1 LHSLIST T)))
      (PROG (H)
        (SETQ H LHSLIST)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H) (SETQ DEPLIST (LISTDIFDIF1 H DEPLIST))) (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN (CONS 'LIST DEPLIST)))) 
(PUT 'SIMPPL 'NUMBER-OF-ARGS 4) 
(FLAG '(SIMPPL) 'OPFN) 
(PUT 'SIMPPL 'DEFINED-ON-LINE '213) 
(PUT 'SIMPPL 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'SIMPPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPPL (PLLIST ULIST TT XX)
    (PROG (PL HH TD XD LULIST LTT LXX LTD DV NEWTD E1 DENO OK NEWPLLIST
           CONTRACE)
      (PROGN
       (SETQ LULIST (CDR (REVAL1 (AEVAL ULIST) T)))
       (SETQ LXX (REVAL1 (AEVAL XX) T))
       (SETQ LTT (REVAL1 (AEVAL TT) T))
       NIL)
      (SETQ NEWPLLIST (AEVAL (LIST 'LIST)))
      (PROG (PL)
        (SETQ PL (GETRLIST (AEVAL PLLIST)))
       LAB
        (COND ((NULL PL) (RETURN NIL)))
        ((LAMBDA (PL)
           (PROGN
            (SETQ TD (AEVAL (LIST 'FIRST PL)))
            (SETQ XD (AEVAL (LIST 'SECOND PL)))
            (REPEAT
             (PROGN
              (PROGN
               (SETQ LTD (REVAL1 (AEVAL* TD) T))
               (COND
                (CONTRACE
                 (PROGN (PROGN (PRIN2 "ltd1=") (PRIN2 LTD) NIL) (TERPRI))))
               (SETQ DV NIL)
               (SETQ NEWTD NIL)
               (SETQ DENO NIL)
               (COND
                ((AND (PAIRP LTD) (EQUAL (CAR LTD) 'QUOTIENT)
                      (MY_FREEOF (CADDR LTD) LTT) (MY_FREEOF (CADDR LTD) LXX))
                 (PROGN (SETQ DENO (CADDR LTD)) (SETQ LTD (CADR LTD)))))
               (SETQ OK T)
               (COND
                ((AND (PAIRP LTD) (EQUAL (CAR LTD) 'PLUS))
                 (SETQ LTD (CDR LTD)))
                ((AND (PAIRP LTD) (NEQ (CAR LTD) 'TIMES)) (SETQ OK NIL))
                (T (SETQ LTD (LIST LTD))))
               (COND
                (CONTRACE
                 (PROGN (PROGN (PRIN2 "ltd2=") (PRIN2 LTD) NIL) (TERPRI))))
               (COND
                (OK
                 (PROGN
                  (PROG (E1)
                    (SETQ E1 LTD)
                   LAB
                    (COND ((NULL E1) (RETURN NIL)))
                    ((LAMBDA (E1)
                       (PROGN
                        (SETQ HH (INTPDE E1 LULIST (LIST LXX LTT) LXX T))
                        (COND ((NULL HH) (SETQ HH (LIST NIL E1))))
                        (SETQ DV (CONS (CAR HH) DV))
                        (SETQ NEWTD (CONS (CADR HH) NEWTD))
                        NIL))
                     (CAR E1))
                    (SETQ E1 (CDR E1))
                    (GO LAB))
                  (SETQ DV (REVAL1 (CONS 'PLUS DV) T))
                  (SETQ NEWTD (REVAL1 (CONS 'PLUS NEWTD) T))
                  (COND
                   (DENO
                    (PROGN
                     (SETQ NEWTD (LIST 'QUOTIENT NEWTD DENO))
                     (SETQ DV (LIST 'QUOTIENT DV DENO)))))
                  (COND
                   (CONTRACE
                    (PROGN
                     (PROGN (PRIN2 "newtd=") (PRIN2 NEWTD) NIL)
                     (TERPRI)
                     (PROGN (PRIN2 "dv=") (PRIN2 DV) NIL)
                     (TERPRI))))
                  (SETQ TD NEWTD)
                  (COND
                   (CONTRACE
                    (PROGN (PROGN (PRIN2 "td=") (PRIN2 TD) NIL) (TERPRI))))
                  (COND
                   ((AND (NEQ DV 0) (NEQ DV NIL))
                    (PROGN
                     (SETQ XD (REVAL1 (LIST 'PLUS XD (LIST 'DF DV TT)) T))
                     (COND
                      (CONTRACE
                       (PROGN (PROGN (PRIN2 "xd=") (PRIN2 XD) NIL) (TERPRI))))
                     NIL))))))))
             (EVALEQUAL (AEVAL* DV) 0))
            (SETQ NEWPLLIST (AEVAL (LIST 'CONS (LIST 'LIST TD XD) NEWPLLIST)))
            (AEVAL 'NIL)))
         (CAR PL))
        (SETQ PL (CDR PL))
        (GO LAB))
      (RETURN (AEVAL (LIST 'REVERSE NEWPLLIST))))) 
(FLAG '(FDEPTERMS) 'OPFN) 
(PUT 'FDEPTERMS 'NUMBER-OF-ARGS 2) 
(PUT 'FDEPTERMS 'DEFINED-ON-LINE '282) 
(PUT 'FDEPTERMS 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'FDEPTERMS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FDEPTERMS (TD F)
    (PROG (NU DE E1 SM)
      (SETQ TD (REVAL1 TD T))
      (COND
       ((PAIRP TD)
        (COND
         ((EQUAL (CAR TD) 'QUOTIENT)
          (PROGN (SETQ NU (CADR TD)) (SETQ DE (CADDR TD)))))))
      (COND ((NULL NU) (SETQ NU TD)))
      (COND
       ((NOT (PAIRP NU)) (COND ((FREEOF NU F) (SETQ SM 0)) (T (SETQ SM NU))))
       (T
        (PROGN
         (COND ((EQUAL (CAR NU) 'PLUS) (SETQ NU (CDR NU)))
               (T (SETQ NU (LIST NU))))
         (PROG (E1)
           (SETQ E1 NU)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1) (COND ((NOT (FREEOF E1 F)) (SETQ SM (CONS E1 SM)))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (COND ((NULL SM) (SETQ SM 0))
               ((EQUAL (LENGTH SM) 1) (SETQ SM (CAR SM)))
               (T (SETQ SM (CONS 'PLUS SM)))))))
      (COND (DE (SETQ SM (LIST 'QUOTIENT SM DE))))
      (RETURN SM))) 
(PUT 'SUBTRACT_DIFF 'NUMBER-OF-ARGS 2) 
(PUT 'SUBTRACT_DIFF 'DEFINED-ON-LINE '309) 
(PUT 'SUBTRACT_DIFF 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'SUBTRACT_DIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBTRACT_DIFF (D1 D2)
    (PROG (D)
      (RETURN
       (COND ((GREATERP (CAR D2) (CAR D1)) NIL)
             ((NULL (CDR D1)) (LIST (DIFFERENCE (CAR D1) (CAR D2))))
             ((SETQ D (SUBTRACT_DIFF (CDR D1) (CDR D2)))
              (CONS (DIFFERENCE (CAR D1) (CAR D2)) D))
             (T NIL))))) 
(PUT 'TRANSFER_FCTRS 'NUMBER-OF-ARGS 2) 
(PUT 'TRANSFER_FCTRS 'DEFINED-ON-LINE '325) 
(PUT 'TRANSFER_FCTRS 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'TRANSFER_FCTRS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRANSFER_FCTRS (H FLIST)
    (PROG (FCTR)
      (COND
       ((AND (PAIRP (CDAR H)) (EQUAL (CADAR H) 'MINUS))
        (RPLACA H (CONS (REVAL1 (LIST 'MINUS (CAAR H)) T) (CADR (CDAR H))))))
      (COND
       ((AND (PAIRP (CDAR H)) (EQUAL (CADAR H) 'TIMES))
        (PROG (FC)
          (SETQ FC (CDDAR H))
         LAB
          (COND ((NULL FC) (RETURN NIL)))
          ((LAMBDA (FC)
             (COND ((FREEOFLIST FC FLIST) (SETQ FCTR (CONS FC FCTR)))))
           (CAR FC))
          (SETQ FC (CDR FC))
          (GO LAB))))
      (COND
       (FCTR
        (PROGN
         (COND ((CDR FCTR) (SETQ FCTR (CONS 'TIMES FCTR)))
               (T (SETQ FCTR (CAR FCTR))))
         (RPLACA H
                 (CONS (REVAL1 (LIST 'TIMES (CAAR H) FCTR) T)
                       (REVAL1 (LIST 'QUOTIENT (CDAR H) FCTR) T)))))))) 
(FLAG '(PARTINTDF) 'OPFN) 
(PUT 'PARTINTDF 'NUMBER-OF-ARGS 7) 
(PUT 'PARTINTDF 'DEFINED-ON-LINE '346) 
(PUT 'PARTINTDF 'DEFINED-IN-FILE 'CRACK/CONLAW0.RED) 
(PUT 'PARTINTDF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PARTINTDF (EQLIST QLIST PLIST XLIST FLIST JLIST SB)
    (PROG (F N D DELTALI SUBLI LHS RHS COF X Y CPY NEWPL LOWD SU VLE IDTY
           IDTYSEP SBREV DNO LSB H0 H1 H2 H3 H4 H5 H6 H7 LDH1 LDH2
           REDUCTIONS_TO_DO LD1 LD2 H0_CHANGED TR_PINTD)
      (PROGN
       (SETQ CPY PLIST)
       (PROG (F)
         (SETQ F (GETRLIST FLIST))
        LAB
         (COND ((NULL F) (RETURN NIL)))
         ((LAMBDA (F) (SETQ CPY (AEVAL (LIST 'SUB (LIST 'EQUAL F 0) CPY))))
          (CAR F))
         (SETQ F (CDR F))
         (GO LAB))
       (WHILE
        (AND (EVALNEQ (AEVAL* CPY) (AEVAL* (LIST 'LIST)))
             (EVALEQUAL (AEVAL* (LIST 'FIRST CPY)) 0))
        (SETQ CPY (AEVAL* (LIST 'REST CPY))))
       (AEVAL 'NIL))
      (COND ((NEQ CPY (LIST 'LIST)) (RETURN NIL)))
      (SETQ EQLIST (CDR EQLIST))
      (SETQ QLIST (CDR QLIST))
      (SETQ PLIST (CDR PLIST))
      (SETQ XLIST (CDR XLIST))
      (SETQ FLIST (CDR FLIST))
      (SETQ JLIST (CDR JLIST))
      (SETQ D T)
      (PROG (F)
        (SETQ F FLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (COND ((NOT_INCLUDED (FCTARGS F) XLIST) (SETQ D NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND ((NULL D) (RETURN NIL)))
      (TERPRI)
      (PROGN
       (PRIN2 "An attempt to factor out linear differential operators:")
       NIL)
      (TERPRI)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT EQLIST) (RETURN NIL)))
        (PROGN
         (SETQ N (ADD1 N))
         (SETQ SU PRINT_)
         (SETQ PRINT_ NIL)
         (SETQ D (NEWFCT 'EQ_ XLIST N))
         (SETQ PRINT_ SU)
         (SETQ DELTALI (CONS D DELTALI))
         (PROGN
          (ASSGNPRI (AEVAL* D) NIL 'FIRST)
          (ASSGNPRI (AEVAL* ":=") NIL NIL)
          (ASSGNPRI (AEVAL* (CAR EQLIST)) NIL 'LAST))
         (SETQ SUBLI (CONS (LIST 'EQUAL D (CAR EQLIST)) SUBLI))
         (SETQ LHS (CONS (LIST 'TIMES (CAR QLIST) D) LHS))
         (SETQ EQLIST (CDR EQLIST))
         (SETQ QLIST (CDR QLIST)))
        (GO WHILELABEL))
      (SETQ LHS (REVAL1 (CONS 'PLUS LHS) T))
      (SETQ SUBLI (CONS 'LIST SUBLI))
      (PROG (F)
        (SETQ F FLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ F (REVAL1 F T))
            (PROG ()
             REPEATLABEL
              (PROGN
               (SETQ D (CAR (LDIFFP LHS F)))
               (COND
                (D
                 (PROGN
                  (SETQ CPY D)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT
                       (AND CPY
                            (OR (NUMBERP (CAR CPY)) (FREEOF XLIST (CAR CPY)))))
                      (RETURN NIL)))
                    (SETQ CPY (CDR CPY))
                    (GO WHILELABEL))
                  (COND ((NULL CPY) (SETQ D NIL))
                        (T
                         (PROGN
                          (SETQ COF (COEFFN LHS (CONS 'DF (CONS F D)) 1))
                          (SETQ LHS
                                  (REVAL1
                                   (LIST 'DIFFERENCE LHS
                                         (CONS 'DF
                                               (CONS (LIST 'TIMES COF F) D)))
                                   T))
                          (SETQ X (CAR CPY))
                          (SETQ LOWD (LOWER_DEG D X))
                          (SETQ SU
                                  (COND
                                   (LOWD
                                    (CONS 'DF (CONS (LIST 'TIMES COF F) LOWD)))
                                   (T (LIST 'TIMES COF F))))
                          (SETQ CPY XLIST)
                          (SETQ NEWPL NIL)
                          (PROG ()
                           WHILELABEL
                            (COND
                             ((NOT (AND CPY (NEQ X (CAR CPY)))) (RETURN NIL)))
                            (PROGN
                             (SETQ NEWPL (CONS (CAR PLIST) NEWPL))
                             (SETQ PLIST (CDR PLIST))
                             (SETQ CPY (CDR CPY)))
                            (GO WHILELABEL))
                          (SETQ PLIST
                                  (CONS (LIST 'DIFFERENCE (CAR PLIST) SU)
                                        (CDR PLIST)))
                          (PROG ()
                           WHILELABEL
                            (COND ((NOT NEWPL) (RETURN NIL)))
                            (PROGN
                             (SETQ PLIST (CONS (CAR NEWPL) PLIST))
                             (SETQ NEWPL (CDR NEWPL)))
                            (GO WHILELABEL)))))))))
              (COND ((NOT (NULL D)) (GO REPEATLABEL))))
            (SETQ PLIST (CDR (AEVAL (LIST 'SUB SUBLI (CONS 'LIST PLIST)))))
            (PROG ()
             REPEATLABEL
              (PROGN
               (SETQ NEWPL NIL)
               (SETQ CPY XLIST)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT
                    (AND PLIST
                         (NULL
                          (SETQ D (CAR (LDIFFP (REVAL1 (CAR PLIST) T) F))))))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ NEWPL (CONS (CAR PLIST) NEWPL))
                  (SETQ PLIST (CDR PLIST))
                  (SETQ CPY (CDR CPY)))
                 (GO WHILELABEL))
               (COND
                ((AND D (NEQ (CAR D) (CAR CPY)))
                 (PROGN
                  (SETQ COF (COEFFN (CAR PLIST) (CONS 'DF (CONS F D)) 1))
                  (SETQ X (CAR D))
                  (SETQ LOWD (LOWER_DEG D X))
                  (SETQ SU
                          (COND
                           (LOWD (LIST 'TIMES COF (CONS 'DF (CONS F LOWD))))
                           (T (LIST 'TIMES COF F))))
                  (SETQ PLIST
                          (CONS
                           (REVAL1
                            (LIST 'DIFFERENCE (CAR PLIST) (LIST 'DF SU X)) T)
                           (CDR PLIST)))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT NEWPL) (RETURN NIL)))
                    (PROGN
                     (SETQ PLIST (CONS (CAR NEWPL) PLIST))
                     (SETQ NEWPL (CDR NEWPL)))
                    (GO WHILELABEL))
                  (SETQ Y (CAR CPY))
                  (SETQ CPY XLIST)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (NEQ X (CAR CPY))) (RETURN NIL)))
                    (PROGN
                     (SETQ NEWPL (CONS (CAR PLIST) NEWPL))
                     (SETQ PLIST (CDR PLIST))
                     (SETQ CPY (CDR CPY)))
                    (GO WHILELABEL))
                  (SETQ PLIST
                          (CONS
                           (REVAL1 (LIST 'PLUS (CAR PLIST) (LIST 'DF SU Y)) T)
                           (CDR PLIST)))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT NEWPL) (RETURN NIL)))
                    (PROGN
                     (SETQ PLIST (CONS (CAR NEWPL) PLIST))
                     (SETQ NEWPL (CDR NEWPL)))
                    (GO WHILELABEL))))
                (T
                 (PROGN
                  (SETQ D NIL)
                  (SETQ PLIST (APPEND (REVERSE NEWPL) PLIST))))))
              (COND ((NOT (NULL D)) (GO REPEATLABEL))))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ VLE (LENGTH XLIST))
      (SETQ NEWPL
              (AEVAL
               (LIST 'ABSORBCONST (CONS 'LIST (APPEND QLIST PLIST))
                     (CONS 'LIST FLIST))))
      (COND (NEWPL (SETQ NEWPL (CDADR NEWPL))))
      (PROG (F)
        (SETQ F FLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ DEPL* (DELETE (ASSOC F DEPL*) DEPL*))
            (SETQ DEPL* (CONS (CONS F XLIST) DEPL*))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ IDTY (AEVAL (LIST 'SUB SUBLI LHS)))
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((MINUSP (DIFFERENCE VLE N)) (RETURN NIL)))
        (COND
         ((NOT (ZEROP (NTH PLIST N)))
          (SETQ IDTY
                  (LIST 'DIFFERENCE IDTY
                        (LIST 'DF (NTH PLIST N) (NTH XLIST N))))))
        (SETQ N (PLUS2 N 1))
        (GO LAB))
      (SETQ SBREV
              (CONS 'LIST
                    (PROG (D FORALL-RESULT FORALL-ENDPTR)
                      (SETQ D (CDR SB))
                      (COND ((NULL D) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (D)
                                          (LIST 'EQUAL (CADDR D) (CADR D)))
                                        (CAR D))
                                       NIL)))
                     LOOPLABEL
                      (SETQ D (CDR D))
                      (COND ((NULL D) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (D) (LIST 'EQUAL (CADDR D) (CADR D)))
                                (CAR D))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ IDTY (REVAL1 IDTY T))
      (SETQ DNO (AEVAL (LIST 'DEN IDTY)))
      (COND ((NEQ DNO 1) (SETQ IDTY (AEVAL (LIST 'NUM IDTY)))))
      (SETQ IDTY (AEVAL (LIST 'SUB SBREV IDTY)))
      (SETQ SU PRINT_)
      (SETQ PRINT_ NIL)
      (SETQ IDTYSEP (SEPAR (REVAL1 IDTY T) FLIST JLIST NIL NIL))
      (SETQ PRINT_ SU)
      (SETQ IDTYSEP
              (PROG (D FORALL-RESULT FORALL-ENDPTR)
                (SETQ D IDTYSEP)
                (COND ((NULL D) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (D)
                                    (CONS
                                     (REVAL1 (AEVAL (LIST 'SUB SB (CAR D))) T)
                                     (CDR D)))
                                  (CAR D))
                                 NIL)))
               LOOPLABEL
                (SETQ D (CDR D))
                (COND ((NULL D) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (D)
                            (CONS (REVAL1 (AEVAL (LIST 'SUB SB (CAR D))) T)
                                  (CDR D)))
                          (CAR D))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ REDUCTIONS_TO_DO (DIFFERENCE (LENGTH IDTYSEP) (LENGTH FLIST)))
      (COND
       ((GREATERP REDUCTIONS_TO_DO 0)
        (PROGN
         (SETQ H0 IDTYSEP)
         (PROG ()
          WHILELABEL
           (COND ((NOT H0) (RETURN NIL)))
           (PROGN
            (RPLACA H0 (CONS (REVAL1 (CAAR H0) T) (REVAL1 (CDAR H0) T)))
            (TRANSFER_FCTRS H0 FLIST)
            (SETQ H0 (CDR H0)))
           (GO WHILELABEL))
         (COND
          (TR_PINTD
           (PROGN
            (PROGN (PRIN2 "Separation gives:") NIL)
            (TERPRI)
            (PROG (D)
              (SETQ D IDTYSEP)
             LAB
              (COND ((NULL D) (RETURN NIL)))
              ((LAMBDA (D)
                 (PROGN
                  (ASSGNPRI (AEVAL "0 = (") NIL 'FIRST)
                  (ASSGNPRI (AEVAL (CAR D)) NIL NIL)
                  (ASSGNPRI (AEVAL ") * (") NIL NIL)
                  (ASSGNPRI (AEVAL (CDR D)) NIL NIL)
                  (ASSGNPRI (AEVAL ")") NIL 'LAST)))
               (CAR D))
              (SETQ D (CDR D))
              (GO LAB))
            NIL)))
         (SETQ H0 IDTYSEP)
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ H0_CHANGED NIL)
            (SETQ H1 (CDAR H0))
            (COND
             (TR_PINTD
              (PROGN
               (ASSGNPRI (AEVAL* "caar h0=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* (CAAR H0)) NIL NIL)
               (ASSGNPRI (AEVAL* " cdar h0 =") NIL NIL)
               (ASSGNPRI (AEVAL* (CDAR H0)) NIL 'LAST))))
            (SETQ CPY FLIST)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND CPY (FREEOF H1 (CAR CPY)))) (RETURN NIL)))
              (SETQ CPY (CDR CPY))
              (GO WHILELABEL))
            (SETQ LD1 (CAR (LDIFFP H1 (CAR CPY))))
            (SETQ LDH1 (MAXDERIVS NIL LD1 XLIST))
            (SETQ LD1
                    (COND ((NULL LD1) (CAR CPY))
                          (T (CONS 'DF (CONS (CAR CPY) LD1)))))
            (SETQ H2 IDTYSEP)
            (PROG ()
             WHILELABEL
              (COND ((NOT H2) (RETURN NIL)))
              (COND
               ((OR (EQ H2 H0) (FREEOF (CDAR H2) (CAR CPY)))
                (SETQ H2 (CDR H2)))
               (T
                (PROGN
                 (COND
                  (TR_PINTD
                   (PROGN
                    (ASSGNPRI (AEVAL* "caar h2=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL* (CAAR H2)) NIL NIL)
                    (ASSGNPRI (AEVAL* " cdar h2 =") NIL NIL)
                    (ASSGNPRI (AEVAL* (CDAR H2)) NIL 'LAST))))
                 (SETQ LD2 (CAR (LDIFFP (CDAR H2) (CAR CPY))))
                 (SETQ LDH2 (MAXDERIVS NIL LD2 XLIST))
                 (SETQ LD2
                         (COND ((NULL LD2) (CAR CPY))
                               (T (CONS 'DF (CONS (CAR CPY) LD2)))))
                 (SETQ H3 (SUBTRACT_DIFF LDH1 LDH2))
                 (COND ((NULL H3) (SETQ H2 (CDR H2)))
                       (T
                        (PROGN
                         (SETQ H4 (CDAR H2))
                         (COND
                          ((AND (PAIRP H4) (EQUAL (CAR H4) 'PLUS))
                           (PROGN
                            (PROG (N)
                              (SETQ N 1)
                             LAB
                              (COND ((MINUSP (DIFFERENCE VLE N)) (RETURN NIL)))
                              (COND
                               ((NOT (ZEROP (NTH H3 N)))
                                (SETQ H4
                                        (LIST 'DF H4 (NTH XLIST N)
                                              (NTH H3 N)))))
                              (SETQ N (PLUS2 N 1))
                              (GO LAB))
                            (COND
                             ((NULL
                               (FREEOFLIST
                                (SETQ H5 (AEVAL* (LIST 'QUOTIENT H1 H4)))
                                FLIST))
                              (SETQ H2 (CDR H2)))
                             (T
                              (PROGN
                               (SETQ H6 (LIST 'TIMES (CAAR H0) (REVAL1 H5 T)))
                               (PROG (N)
                                 (SETQ N 1)
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE VLE N)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ H7 (NTH H3 N))
                                  (COND
                                   ((NOT (ZEROP H7))
                                    (SETQ H6
                                            (LIST 'TIMES
                                                  (LIST 'EXPT (MINUS 1) H7)
                                                  (LIST 'DF H6 (NTH XLIST N)
                                                        H7)))))
                                  NIL)
                                 (SETQ N (PLUS2 N 1))
                                 (GO LAB))
                               (RPLACA H2
                                       (CONS
                                        (REVAL1 (LIST 'PLUS (CAAR H2) H6) T)
                                        (CDAR H2)))
                               (RPLACA H0 (CONS 0 0))
                               (COND
                                (TR_PINTD
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "Change(1):") NIL 'ONLY)
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "caar h2=") NIL 'FIRST)
                                   (ASSGNPRI (AEVAL* (CAAR H2)) NIL NIL)
                                   (ASSGNPRI (AEVAL* " cdar h2 =") NIL NIL)
                                   (ASSGNPRI (AEVAL* (CDAR H2)) NIL 'LAST))
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "caar h0=") NIL 'FIRST)
                                   (ASSGNPRI (AEVAL* (CAAR H0)) NIL NIL)
                                   (ASSGNPRI (AEVAL* " cdar h0 =") NIL NIL)
                                   (ASSGNPRI (AEVAL* (CDAR H0)) NIL 'LAST))
                                  NIL)))
                               (SETQ REDUCTIONS_TO_DO (SUB1 REDUCTIONS_TO_DO))
                               (SETQ H2 NIL))))))
                          (T
                           (PROGN
                            (SETQ H6
                                    (AEVAL*
                                     (LIST 'TIMES (CAAR H0)
                                           (LIST 'COEFFN H1 LD1 1))))
                            (PROG (N)
                              (SETQ N 1)
                             LAB
                              (COND ((MINUSP (DIFFERENCE VLE N)) (RETURN NIL)))
                              (PROGN
                               (SETQ H7 (NTH H3 N))
                               (COND
                                ((NOT (ZEROP H7))
                                 (SETQ H6
                                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) H7)
                                               (LIST 'DF H6 (NTH XLIST N)
                                                     H7)))))
                               NIL)
                              (SETQ N (PLUS2 N 1))
                              (GO LAB))
                            (RPLACA H2
                                    (CONS (REVAL1 (LIST 'PLUS (CAAR H2) H6) T)
                                          (CDAR H2)))
                            (COND
                             (TR_PINTD
                              (PROGN
                               (ASSGNPRI (AEVAL* "Change(2):") NIL 'ONLY)
                               (PROGN
                                (ASSGNPRI (AEVAL* "caar h2=") NIL 'FIRST)
                                (ASSGNPRI (AEVAL* (CAAR H2)) NIL NIL)
                                (ASSGNPRI (AEVAL* " cdar h2 =") NIL NIL)
                                (ASSGNPRI (AEVAL* (CDAR H2)) NIL 'LAST))
                               NIL)))
                            (SETQ H1
                                    (REVAL1
                                     (LIST 'DIFFERENCE H1
                                           (LIST 'TIMES (COEFFN H1 LD1 1) LD1))
                                     T))
                            (COND
                             ((ZEROP H1)
                              (PROGN
                               (RPLACA H0 (CONS 0 0))
                               (SETQ H2 NIL)
                               (SETQ REDUCTIONS_TO_DO
                                       (SUB1 REDUCTIONS_TO_DO))))
                             (T
                              (PROGN
                               (RPLACA H0 (CONS (CAAR H0) H1))
                               (TRANSFER_FCTRS H0 FLIST)
                               (SETQ H1 (CDAR H0))
                               (SETQ CPY FLIST)
                               (PROG ()
                                WHILELABEL
                                 (COND
                                  ((NOT (AND CPY (FREEOF H1 (CAR CPY))))
                                   (RETURN NIL)))
                                 (SETQ CPY (CDR CPY))
                                 (GO WHILELABEL))
                               (SETQ LD1 (CAR (LDIFFP H1 (CAR CPY))))
                               (SETQ LDH1 (MAXDERIVS NIL LD1 XLIST))
                               (SETQ LD1
                                       (COND ((NULL LD1) (CAR CPY))
                                             (T
                                              (CONS 'DF
                                                    (CONS (CAR CPY) LD1)))))
                               (SETQ H2 (CDR H2))
                               (SETQ H0_CHANGED T))))
                            (COND
                             (TR_PINTD
                              (PROGN
                               (ASSGNPRI (AEVAL* "caar h0=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* (CAAR H0)) NIL NIL)
                               (ASSGNPRI (AEVAL* " cdar h0 =") NIL NIL)
                               (ASSGNPRI (AEVAL* (CDAR H0)) NIL 'LAST))))
                            NIL)))))))))
              (GO WHILELABEL))
            (COND
             ((OR (NULL H0_CHANGED) (ZEROP (CAAR H0))) (SETQ H0 (CDR H0)))))
           (COND
            ((NOT (OR (EQUAL REDUCTIONS_TO_DO 0) (NULL H0)))
             (GO REPEATLABEL))))
         (COND
          (TR_PINTD
           (PROGN
            (PROGN (PRIN2 "After correction the separation gives:") NIL)
            (TERPRI)
            (PROG (D)
              (SETQ D IDTYSEP)
             LAB
              (COND ((NULL D) (RETURN NIL)))
              ((LAMBDA (D)
                 (COND
                  ((NOT (ZEROP (CAR D)))
                   (PROGN
                    (ASSGNPRI (AEVAL "0 = (") NIL 'FIRST)
                    (ASSGNPRI (AEVAL (CAR D)) NIL NIL)
                    (ASSGNPRI (AEVAL ") * (") NIL NIL)
                    (ASSGNPRI (AEVAL (CDR D)) NIL NIL)
                    (ASSGNPRI (AEVAL ")") NIL 'LAST)))))
               (CAR D))
              (SETQ D (CDR D))
              (GO LAB))
            NIL))))))
      (SETQ N 0)
      (SETQ RHS NIL)
      (PROG (D)
        (SETQ D IDTYSEP)
       LAB
        (COND ((NULL D) (RETURN NIL)))
        ((LAMBDA (D)
           (COND
            ((NOT (ZEROP (CAR D)))
             (PROGN
              (SETQ N (ADD1 N))
              (SETQ SU PRINT_)
              (SETQ PRINT_ NIL)
              (SETQ X (NEWFCT 'L_ XLIST N))
              (SETQ PRINT_ SU)
              (SETQ SU
                      (COND ((EQUAL DNO 1) (CAR D))
                            (T (REVAL1 (LIST 'QUOTIENT (CAR D) DNO) T))))
              (PROGN
               (ASSGNPRI (AEVAL X) NIL 'FIRST)
               (ASSGNPRI (AEVAL ":=") NIL NIL)
               (ASSGNPRI (AEVAL SU) NIL 'LAST))
              (SETQ LSB (CONS (LIST 'EQUAL X SU) LSB))
              (SETQ Y (CDR D))
              (SETQ CPY FLIST)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND Y (NOT (ZEROP Y)))) (RETURN NIL)))
                (PROGN
                 (PROG ()
                  REPEATLABEL
                   (PROGN
                    (SETQ D (LDIFFP Y (CAR CPY)))
                    (COND
                     ((ZEROP (CDR D))
                      (COND
                       ((NULL CPY)
                        (PROGN
                         (PROGN (PRIN2 "The backintegration is faulty.") NIL)
                         (TERPRI)))
                       (T (SETQ CPY (CDR CPY)))))))
                   (COND ((NOT (NOT (ZEROP (CDR D)))) (GO REPEATLABEL))))
                 (COND
                  ((EQUAL (CAR D) NIL)
                   (PROGN
                    (SETQ COF (COEFFN Y (CAR CPY) 1))
                    (SETQ RHS (LIST 'PLUS (LIST 'TIMES X COF (CAR CPY)) RHS))
                    (SETQ Y
                            (REVAL1
                             (LIST 'DIFFERENCE Y (LIST 'TIMES COF (CAR CPY)))
                             T))))
                  (T
                   (PROGN
                    (SETQ COF (COEFFN Y (CONS 'DF (CONS (CAR CPY) (CAR D))) 1))
                    (SETQ RHS
                            (REVAL1
                             (LIST 'PLUS RHS
                                   (LIST 'TIMES
                                         (CONS 'DF
                                               (CONS (LIST 'TIMES X COF)
                                                     (CAR D)))
                                         (CAR CPY)
                                         (LIST 'EXPT (LIST 'MINUS 1)
                                               (ABSODEG (CAR D)))))
                             T))
                    (SETQ Y
                            (REVAL1
                             (LIST 'DIFFERENCE Y
                                   (LIST 'TIMES COF
                                         (CONS 'DF (CONS (CAR CPY) (CAR D)))))
                             T))))))
                (GO WHILELABEL))))))
         (CAR D))
        (SETQ D (CDR D))
        (GO LAB))
      (SETQ LSB (CONS 'LIST LSB))
      (SETQ FLIST (CONS 'LIST FLIST))
      (PROGN
       (SETQ D (AEVAL (LIST 'GCD (LIST 'DEN LHS) (LIST 'DEN RHS))))
       (SETQ LHS (AEVAL (LIST 'TIMES LHS D)))
       (SETQ RHS (AEVAL (LIST 'TIMES RHS D)))
       (SETQ D
               (AEVAL
                (LIST 'DIFFERENCE (LIST 'SUB SUBLI LHS) (LIST 'SUB LSB RHS))))
       (COND
        ((EVALNEQ (AEVAL D) 0)
         (PROGN
          (ASSGNPRI (AEVAL "Not identically zero : ") NIL 'FIRST)
          (ASSGNPRI (AEVAL D) NIL 'LAST))))
       (PROG (F)
         (SETQ F (GETRLIST FLIST))
        LAB
         (COND ((NULL F) (RETURN NIL)))
         ((LAMBDA (F)
            (PROGN
             (SETQ X (AEVAL (LIST 'COEFFN (LIST 'NUM LHS) F 1)))
             (SETQ Y (AEVAL (LIST 'COEFFN (LIST 'NUM RHS) F 1)))
             (SETQ D (AEVAL (LIST 'GCD X Y)))
             (PROGN
              (ASSGNPRI
               (AEVAL (LIST 'QUOTIENT (LIST 'QUOTIENT X D) (LIST 'DEN LHS)))
               NIL 'FIRST)
              (ASSGNPRI (AEVAL " = ") NIL NIL)
              (ASSGNPRI
               (AEVAL (LIST 'QUOTIENT (LIST 'QUOTIENT Y D) (LIST 'DEN RHS)))
               NIL 'LAST))
             (AEVAL 'NIL)))
          (CAR F))
         (SETQ F (CDR F))
         (GO LAB))))) 