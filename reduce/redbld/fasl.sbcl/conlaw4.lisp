(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID
 '(PRINT_ LOGOPRINT_ POTINT_ FACINT_ ADJUST_FNC FLIN_ DONE_TRAFO
   INVERSE_TRAFO_LIST_INCOMPLETE NO_CURRENT)) 
(PUT 'CONLAW4 'NUMBER-OF-ARGS 2) 
(FLAG '(CONLAW4) 'OPFN) 
(PUT 'CONLAW4 'DEFINED-ON-LINE '37) 
(PUT 'CONLAW4 'DEFINED-IN-FILE 'CRACK/CONLAW4.RED) 
(PUT 'CONLAW4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONLAW4 (PROBLEM RUNMODE)
    (PROG (CONTRACE EQLIST ULIST XLIST DEQU CLLIST DIVLIST SB DENSORD FLIST
           EQORD MAXORD DULIST REVDULIST VL EXPL DEPLIST E1 E2 E3 N H1 H2 H3 H4
           H5 H6 H7 H8 H9 H10 H11 CONDI SOLN POTOLD ADJUSTOLD UDENS GENSEPOLD
           NON_INT INEQU0 INEQU LOGOOLD TREQLIST FL FACOLD U NODEP CPU GC
           CPUSTART GCSTART NONTRIV CF0 RTNLIST PARALIST SOLNS FOUND CLCOPY
           EXTRALINE NONDIV NX NDE NONCONSTC MINDENSORD MINDENSORD0 MAXDENSORD
           RULES NEW_VAR_FNC)
      (AEVAL (LIST 'BACKUP_REDUCE_FLAGS))
      (PROGN
       (SETQ ADJUSTOLD ADJUST_FNC)
       (SETQ ADJUST_FNC T)
       (SETQ LOGOOLD LOGOPRINT_)
       (SETQ LOGOPRINT_ T)
       (SETQ POTOLD POTINT_)
       (SETQ POTINT_ T)
       (SETQ FACOLD FACINT_)
       (SETQ FACINT_ 1000))
      (SETQ CPUSTART (AEVAL (TIME)))
      (SETQ GCSTART (AEVAL (GCTIME)))
      (SETQ EQLIST
              (AEVAL
               (LIST 'SQREVERSE (LIST 'MAKLIST (LIST 'SQFIRST PROBLEM)))))
      (SETQ ULIST (AEVAL (LIST 'MAKLIST (LIST 'SQSECOND PROBLEM))))
      (SETQ XLIST (AEVAL (LIST 'MAKLIST (LIST 'SQTHIRD PROBLEM))))
      (SETQ NX (AEVAL (LIST 'LENGTH XLIST)))
      (SETQ NDE (AEVAL (LIST 'LENGTH EQLIST)))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "eqlist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL EQLIST) NIL NIL)
         (ASSGNPRI (AEVAL " ulist=") NIL NIL)
         (ASSGNPRI (AEVAL ULIST) NIL NIL)
         (ASSGNPRI (AEVAL " xlist=") NIL NIL)
         (ASSGNPRI (AEVAL XLIST) NIL 'LAST))))
      (SETQ MINDENSORD (AEVAL (LIST 'SQPART RUNMODE 1)))
      (SETQ MAXDENSORD (AEVAL (LIST 'SQPART RUNMODE 2)))
      (SETQ EXPL (AEVAL (LIST 'SQPART RUNMODE 3)))
      (SETQ FLIST (AEVAL (LIST 'SQPART RUNMODE 4)))
      (SETQ INEQU0 (AEVAL (LIST 'SQPART RUNMODE 5)))
      (SETQ PROBLEM (SETQ RUNMODE (AEVAL 0)))
      (COND
       (LOGOPRINT_
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "--------------------------------------------------")
          (PRIN2 "------------------------")
          NIL)
         (TERPRI)
         (TERPRI)
         (PROGN
          (PRIN2 "This is CONLAW4 - a program for calculating conservation")
          (PRIN2 " laws of DEs")
          NIL)
         (TERPRI)))
       (T (TERPRI)))
      (COND
       ((EVALEQUAL (AEVAL NDE) 1)
        (ASSGNPRI (AEVAL "The DE under investigation is :") NIL 'ONLY))
       (T (ASSGNPRI (AEVAL "The DEs under investigation are :") NIL 'ONLY)))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL (LIST 'SQREVERSE EQLIST))))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (ASSGNPRI (AEVAL E1) NIL 'ONLY)) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROGN
       (TERPRI)
       (PROGN (PRIN2 "for the function(s): ") NIL)
       (FCTPRINT (CDR (REVAL1 ULIST T)))
       (TERPRI))
      (ASSGNPRI
       (AEVAL "======================================================") NIL
       'ONLY)
      (SETQ NODEP (AEVAL (LIST 'SQFIRST (LIST 'LHSLI EQLIST))))
      (AEVAL (LIST 'CHKSUB EQLIST ULIST))
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NDE) N)) (RETURN NIL)))
        (COND
         ((NOT (BOOLVALUE* (REVALX (NULL (GET (MKID 'Q_ N) 'AVALUE)))))
          (SETQ CF0 (AEVAL* 'T))))
        (SETQ N
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 N))
        (GO LAB))
      (SETQ EQLIST
              (AEVAL
               (LIST 'SQREVERSE
                     (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E1 (GETRLIST (AEVAL EQLIST)))
                       (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (E1)
                                           (COND
                                            ((EVALEQUAL
                                              (AEVAL (LIST 'PART E1 0))
                                              (AEVAL 'EQUAL))
                                             (AEVAL
                                              (LIST 'DIFFERENCE (LIST 'LHS E1)
                                                    (LIST 'RHS E1))))
                                            (T (AEVAL E1))))
                                         (CAR E1))
                                        NIL)))
                      LOOPLABEL
                       (SETQ E1 (CDR E1))
                       (COND ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (E1)
                                   (COND
                                    ((EVALEQUAL (AEVAL (LIST 'PART E1 0))
                                                (AEVAL 'EQUAL))
                                     (AEVAL
                                      (LIST 'DIFFERENCE (LIST 'LHS E1)
                                            (LIST 'RHS E1))))
                                    (T (AEVAL E1))))
                                 (CAR E1))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "ulist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL ULIST) NIL NIL)
         (ASSGNPRI (AEVAL "    eqlist=") NIL NIL)
         (ASSGNPRI (AEVAL EQLIST) NIL 'LAST))))
      (SETQ RTNLIST (AEVAL (LIST 'LIST)))
      (SETQ NONDIV (AEVAL (INTERN (GENSYM))))
      (SETQ PARALIST (AEVAL (LIST 'LIST)))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL FLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((NOT (FREEOF (REVALX EQLIST) (REVALX E1)))
             (SETQ PARALIST (AEVAL (LIST 'SQCONS E1 PARALIST))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ EQORD (AEVAL 0))
      (SETQ MINDENSORD0 (AEVAL MINDENSORD))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL EQLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROG (E2)
             (SETQ E2 (GETRLIST (AEVAL ULIST)))
            LAB
             (COND ((NULL E2) (RETURN NIL)))
             ((LAMBDA (E2)
                (PROGN
                 (SETQ H1 (AEVAL (LIST 'TOTDEG E1 E2)))
                 (COND
                  ((EVALGREATERP (AEVAL H1) (AEVAL EQORD))
                   (SETQ EQORD (AEVAL H1))))))
              (CAR E2))
             (SETQ E2 (CDR E2))
             (GO LAB)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NDE) N)) (RETURN NIL)))
        (PROGN
         (SETQ H1 (AEVAL* (LIST 'MKID 'Q_ N)))
         (COND
          ((NOT (BOOLVALUE* (REVALX (NULL (GET (MKID 'Q_ N) 'AVALUE)))))
           (PROGN
            (PROG (E2)
              (SETQ E2 (GETRLIST (AEVAL* ULIST)))
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (PROGN
                  (SETQ H2 (AEVAL* (LIST 'TOTDEG H1 E2)))
                  (COND
                   ((EVALGREATERP (AEVAL* H2) (AEVAL* EQORD))
                    (SETQ EQORD (AEVAL* H2))))
                  (COND
                   ((EVALGREATERP (AEVAL* H2) (AEVAL* MINDENSORD))
                    (SETQ MINDENSORD (AEVAL* H2))))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (SETQ CF0 (AEVAL* 'T))
            (AEVAL* 'NIL)))))
        (SETQ N
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 N))
        (GO LAB))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "eqord=") NIL 'FIRST)
         (ASSGNPRI (AEVAL EQORD) NIL 'LAST))))
      (COND
       ((EVALLESSP (AEVAL MAXDENSORD) (AEVAL MINDENSORD))
        (SETQ MAXDENSORD (AEVAL MINDENSORD))))
      (SETQ SB (AEVAL (LIST 'SUBDIF1 XLIST ULIST EQORD)))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "sb=") NIL 'FIRST)
         (ASSGNPRI (AEVAL SB) NIL 'LAST))))
      (SETQ TREQLIST (AEVAL EQLIST))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL SB)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ TREQLIST (AEVAL (LIST 'SUB E1 TREQLIST)))
            (SETQ NODEP (AEVAL (LIST 'SUB E1 NODEP)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "treqlist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL TREQLIST) NIL NIL)
         (ASSGNPRI (AEVAL "nodep=") NIL NIL)
         (ASSGNPRI (AEVAL NODEP) NIL 'LAST))))
      (AEVAL (LIST 'CHKFLIST FLIST NODEP))
      (COND
       ((BOOLVALUE* CF0)
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NDE) N)) (RETURN NIL)))
          (PROGN
           (SETQ H1 (AEVAL* (LIST 'MKID 'Q_ N)))
           (COND
            ((NOT (BOOLVALUE* (REVALX (NULL (GET (MKID 'Q_ N) 'AVALUE)))))
             (PROGN
              (PROG (E1)
                (SETQ E1 (GETRLIST (AEVAL* SB)))
               LAB
                (COND ((NULL E1) (RETURN NIL)))
                ((LAMBDA (E1) (SETQ H1 (AEVAL* (LIST 'SUB E1 H1)))) (CAR E1))
                (SETQ E1 (CDR E1))
                (GO LAB))
              (SETK (MKID 'Q_ N) (AEVAL* H1))
              (AEVAL* 'NIL)))))
          (SETQ N
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   N))
          (GO LAB))))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL SB)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (SETQ INEQU0 (AEVAL (LIST 'SUB E1 INEQU0)))) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (PROG (DENSORD)
        (SETQ DENSORD (AEVAL* MINDENSORD))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MAXDENSORD) DENSORD))
          (RETURN NIL)))
        (PROGN
         (AEVAL* (LIST 'NODEPENDLIST ULIST))
         (SETQ CPU (AEVAL* (TIME)))
         (SETQ GC (AEVAL* (GCTIME)))
         (COND
          ((BOOLVALUE* CF0)
           (PROGN
            (PROGN
             (PRIN2 "A special ansatz of order ")
             (PRIN2 DENSORD)
             (PRIN2 " for the characteristic")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "function(s) is investigated.") NIL)
            (TERPRI)))
          (T
           (PROGN
            (PROGN
             (PRIN2 "Currently conservation laws with characteristic")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "function(s) of order ")
             (PRIN2 DENSORD)
             (PRIN2 " are determined")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "======================================================")
             NIL)
            NIL)))
         (SETQ MAXORD
                 (AEVAL*
                  (LIST 'PLUS EQORD 1
                        (COND
                         ((EVALGREATERP (AEVAL* EQORD) (AEVAL* DENSORD))
                          (AEVAL* EQORD))
                         (T (AEVAL* DENSORD))))))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "maxord=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* MAXORD) NIL 'LAST))))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'LIST))
                      (AEVAL* (LIST 'FARGS (LIST 'SQFIRST ULIST))))
           (PROG (E1)
             (SETQ E1 (GETRLIST (AEVAL* ULIST)))
            LAB
             (COND ((NULL E1) (RETURN NIL)))
             ((LAMBDA (E1) (AEVAL* (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
              (CAR E1))
             (SETQ E1 (CDR E1))
             (GO LAB))))
         (SETQ SB (AEVAL* (LIST 'SUBDIF1 XLIST ULIST MAXORD)))
         (AEVAL* (LIST 'NODEPENDLIST ULIST))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "sb=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* SB) NIL 'LAST))))
         (SETQ DULIST
                 (AEVAL*
                  (LIST 'CONS ULIST
                        (LIST 'SQREVERSE
                              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ E1 (GETRLIST (AEVAL* SB)))
                                (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (E1)
                                                    (PROG (E2 FORALL-RESULT
                                                           FORALL-ENDPTR)
                                                      (SETQ E2
                                                              (GETRLIST
                                                               (AEVAL* E1)))
                                                      (COND
                                                       ((NULL E2)
                                                        (RETURN
                                                         (MAKELIST NIL))))
                                                      (SETQ FORALL-RESULT
                                                              (SETQ FORALL-ENDPTR
                                                                      (CONS
                                                                       ((LAMBDA
                                                                            (
                                                                             E2)
                                                                          (AEVAL*
                                                                           (LIST
                                                                            'RHS
                                                                            E2)))
                                                                        (CAR
                                                                         E2))
                                                                       NIL)))
                                                     LOOPLABEL
                                                      (SETQ E2 (CDR E2))
                                                      (COND
                                                       ((NULL E2)
                                                        (RETURN
                                                         (CONS 'LIST
                                                               FORALL-RESULT))))
                                                      (RPLACD FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (E2)
                                                                  (AEVAL*
                                                                   (LIST 'RHS
                                                                         E2)))
                                                                (CAR E2))
                                                               NIL))
                                                      (SETQ FORALL-ENDPTR
                                                              (CDR
                                                               FORALL-ENDPTR))
                                                      (GO LOOPLABEL)))
                                                  (CAR E1))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ E1 (CDR E1))
                                (COND
                                 ((NULL E1)
                                  (RETURN (CONS 'LIST FORALL-RESULT))))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (E1)
                                            (PROG (E2 FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ E2 (GETRLIST (AEVAL* E1)))
                                              (COND
                                               ((NULL E2)
                                                (RETURN (MAKELIST NIL))))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (E2)
                                                                  (AEVAL*
                                                                   (LIST 'RHS
                                                                         E2)))
                                                                (CAR E2))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ E2 (CDR E2))
                                              (COND
                                               ((NULL E2)
                                                (RETURN
                                                 (CONS 'LIST FORALL-RESULT))))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E2)
                                                          (AEVAL*
                                                           (LIST 'RHS E2)))
                                                        (CAR E2))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))
                                          (CAR E1))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))))
         (SETQ SB (AEVAL* 0))
         (SETQ REVDULIST (AEVAL* (LIST 'SQREVERSE DULIST)))
         (SETQ UDENS (AEVAL* (LIST 'SQPART DULIST (LIST 'PLUS DENSORD 1))))
         (SETQ VL
                 (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E1 (GETRLIST (AEVAL* DULIST)))
                  STARTOVER
                   (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT ((LAMBDA (E1) (AEVAL* E1)) (CAR E1)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ E1 (CDR E1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL E1) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST ((LAMBDA (E1) (AEVAL* E1)) (CAR E1))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ E1 (CDR E1))
                   (GO LOOPLABEL)))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "vl=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* VL) NIL NIL)
            (ASSGNPRI (AEVAL* "  udens=") NIL NIL)
            (ASSGNPRI (AEVAL* UDENS) NIL 'LAST))))
         (COND ((NOT (BOOLVALUE* FLIST)) (SETQ FL (AEVAL* (LIST 'LIST))))
               (T (SETQ FL (AEVAL* FLIST))))
         (SETQ DEPLIST
                 (AEVAL*
                  (LIST 'CONS (CONS 'LIST (SETDIFF (CDR ULIST) (CDR NODEP)))
                        (PROG (N FORALL-RESULT FORALL-ENDPTR)
                          (SETQ N 1)
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DENSORD) N))
                            (RETURN (MAKELIST NIL))))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           (AEVAL*
                                            (LIST 'LISTDIFDIF2 NODEP
                                                  (LIST 'SQPART DULIST
                                                        (LIST 'PLUS N 1))))
                                           NIL)))
                         LOOPLABEL
                          (SETQ N
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   N))
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DENSORD) N))
                            (RETURN (CONS 'LIST FORALL-RESULT))))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   (AEVAL*
                                    (LIST 'LISTDIFDIF2 NODEP
                                          (LIST 'SQPART DULIST
                                                (LIST 'PLUS N 1))))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))))
         (COND
          ((BOOLVALUE* EXPL)
           (SETQ DEPLIST (AEVAL* (LIST 'CONS XLIST DEPLIST)))))
         (SETQ DEPLIST (AEVAL* (LIST 'SQREVERSE DEPLIST)))
         (SETK 'CF (AEVAL* (LIST 'LIST)))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NDE) N)) (RETURN NIL)))
           (PROGN
            (SETQ H1 (AEVAL* (LIST 'MKID 'Q_ N)))
            (COND
             ((BOOLVALUE* (REVALX (NULL (GET (MKID 'Q_ N) 'AVALUE))))
              (PROGN
               (AEVAL* (LIST 'NODEPENDLIST (LIST 'LIST H1)))
               (AEVAL* (LIST 'DEPENDLIST H1 DEPLIST))
               (SETQ FL (AEVAL* (LIST 'SQCONS H1 FL)))
               (SETQ FLIN_ (CONS (REVAL1 H1 T) FLIN_)))))
            (SETK 'CF (AEVAL* (LIST 'SQCONS H1 'CF)))
            (AEVAL* 'NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))
         (SETK 'CF (AEVAL* (LIST 'SQREVERSE 'CF)))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "fl=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* FL) NIL 'LAST))))
         (COND
          ((BOOLVALUE* CONTRACE) (PROGN (PRIN2 " depl*=") (PRIN2 DEPL*) NIL)))
         (SETQ CONDI (AEVAL* (LIST 'LIST)))
         (PROG (U)
           (SETQ U (GETRLIST (AEVAL* ULIST)))
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U)
              (PROGN
               (COND
                ((BOOLVALUE* CONTRACE)
                 (PROGN
                  (ASSGNPRI (AEVAL* "function=") NIL 'FIRST)
                  (ASSGNPRI (AEVAL* U) NIL 'LAST))))
               (SETQ H1 (AEVAL* TREQLIST))
               (SETQ H2 (AEVAL* 'CF))
               (SETQ H3 (AEVAL* 0))
               (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                      (PROGN
                       (COND
                        ((BOOLVALUE* CONTRACE)
                         (PROGN
                          (ASSGNPRI (AEVAL* "equation :") NIL 'FIRST)
                          (ASSGNPRI (AEVAL* (LIST 'SQFIRST H1)) NIL 'LAST))))
                       (PROG (E1)
                         (SETQ E1 (GETRLIST (AEVAL* VL)))
                        LAB
                         (COND ((NULL E1) (RETURN NIL)))
                         ((LAMBDA (E1)
                            (COND
                             ((BOOLVALUE*
                               (REVALX
                                (EQUAL (REVAL1 (REVALX U) T)
                                       (CAR (COMBIDIF (REVALX E1))))))
                              (PROGN
                               (SETQ E2
                                       (AEVAL*
                                        (LIST 'DF
                                              (LIST 'TIMES (LIST 'SQFIRST H2)
                                                    (LIST 'SQFIRST H1))
                                              E1)))
                               (COND
                                ((EVALNEQ (AEVAL* E2) 0)
                                 (PROGN
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "e1=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* E1) NIL 'LAST))))
                                  (SETQ DEQU (AEVAL* E2))
                                  (SETQ E2 (AEVAL* 1))
                                  (PROG (E3)
                                    (SETQ E3
                                            (GETRLIST
                                             (AEVAL*
                                              (CONS 'LIST
                                                    (CDR
                                                     (COMBIDIF
                                                      (AEVAL* E1)))))))
                                   LAB
                                    (COND ((NULL E3) (RETURN NIL)))
                                    ((LAMBDA (E3)
                                       (PROGN
                                        (SETQ DEQU
                                                (AEVAL*
                                                 (LIST 'TOTDIF DEQU
                                                       (LIST 'SQPART XLIST E3)
                                                       E3 DULIST)))
                                        (SETQ E2 (AEVAL* (LIST 'MINUS E2)))
                                        (COND
                                         ((BOOLVALUE* CONTRACE)
                                          (PROGN
                                           (ASSGNPRI (AEVAL* "dequ=") NIL
                                                     'FIRST)
                                           (ASSGNPRI (AEVAL* DEQU) NIL NIL)
                                           (ASSGNPRI (AEVAL* " e3=") NIL NIL)
                                           (ASSGNPRI (AEVAL* E3) NIL
                                                     'LAST))))))
                                     (CAR E3))
                                    (SETQ E3 (CDR E3))
                                    (GO LAB))
                                  (COND
                                   ((EVALEQUAL (AEVAL* E2) 1)
                                    (SETQ H3 (AEVAL* (LIST 'PLUS H3 DEQU))))
                                   (T
                                    (SETQ H3
                                            (AEVAL*
                                             (LIST 'DIFFERENCE H3 DEQU)))))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "h3=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* H3) NIL 'LAST))))
                                  (AEVAL* 'NIL))))
                               (AEVAL* 'NIL)))))
                          (CAR E1))
                         (SETQ E1 (CDR E1))
                         (GO LAB))
                       (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                       (SETQ H2 (AEVAL* (LIST 'SQREST H2)))))
               (SETQ CONDI (AEVAL* (LIST 'SQCONS H3 CONDI)))))
            (CAR U))
           (SETQ U (CDR U))
           (GO LAB))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "condi=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* CONDI) NIL 'LAST))))
         (SETQ VL (AEVAL* (LIST 'SQREVERSE (LIST 'SQAPPEND XLIST VL))))
         (SETQ INEQU (AEVAL* INEQU0))
         (COND
          ((AND (EVALNEQ (AEVAL* DENSORD) 0)
                (OR (EVALEQUAL (AEVAL* CF0) (AEVAL* 'NIL))
                    (EVALNEQ (AEVAL* MINDENSORD0) 0)))
           (PROGN
            (SETQ DEQU (AEVAL* (LIST 'LIST)))
            (PROG (E1)
              (SETQ E1 (GETRLIST (AEVAL* 'CF)))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROGN
                  (SETQ H1 (AEVAL* UDENS))
                  (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                         (PROGN
                          (SETQ DEQU
                                  (AEVAL*
                                   (LIST 'SQCONS (LIST 'DF E1 (LIST 'FIRST H1))
                                         DEQU)))
                          (SETQ H1 (AEVAL* (LIST 'SQREST H1)))))
                  (AEVAL* 'NIL)))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (SETQ INEQU (AEVAL* (LIST 'SQCONS DEQU INEQU))))))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "inequ=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* INEQU) NIL 'LAST))))
         (COND
          ((AND (NOT (BOOLVALUE* (REVALX (NULL (GET 'CL_CONDI 'AVALUE)))))
                (EVALEQUAL (AEVAL* (LIST 'PART 'CL_CONDI 0)) (AEVAL* 'LIST)))
           (SETQ CONDI (AEVAL* (LIST 'SQAPPEND CONDI 'CL_CONDI)))))
         (SETQ SB
                 (SETQ REVDULIST
                         (SETQ DEPLIST
                                 (SETQ E1
                                         (SETQ E2
                                                 (SETQ E3
                                                         (SETQ N
                                                                 (SETQ H1
                                                                         (SETQ H2
                                                                                 (SETQ H3
                                                                                         (SETQ SOLN
                                                                                                 (SETQ U
                                                                                                         (SETQ DEQU
                                                                                                                 (AEVAL*
                                                                                                                  0))))))))))))))
         (COND
          ((BOOLVALUE* (REVALX *TIME))
           (PROGN
            (ASSGNPRI (AEVAL* "time to formulate condition: ") NIL 'FIRST)
            (ASSGNPRI (AEVAL* (LIST 'DIFFERENCE (TIME) CPU)) NIL NIL)
            (ASSGNPRI (AEVAL* " ms    GC time : ") NIL NIL)
            (ASSGNPRI (AEVAL* (LIST 'DIFFERENCE (GCTIME) GC)) NIL NIL)
            (ASSGNPRI (AEVAL* " ms") NIL 'LAST))))
         (SETK 'INVERSE_TRAFO_LIST_INCOMPLETE (AEVAL* 'NIL))
         (SETQ CONDI (AEVAL* (LIST 'SPLIT_SIMP CONDI INEQU FL VL 'NIL)))
         (SETQ SOLNS (AEVAL* (LIST 'CRACK CONDI INEQU FL VL)))
         (COND
          ((AND DONE_TRAFO (CDR DONE_TRAFO))
           (PROGN
            (TERPRI)
            (COND
             ((CDDR DONE_TRAFO)
              (PROGN
               (PRIN2
                "The following transformations reverse the transformations")
               NIL))
             (T
              (PROGN
               (PRIN2
                "The following transformation reverses the transformation")
               NIL)))
            (TERPRI)
            (PROGN (PRIN2 "performed in the computation:") NIL)
            (ASSGNPRI (AEVAL* DONE_TRAFO) NIL 'ONLY)
            (COND
             (INVERSE_TRAFO_LIST_INCOMPLETE
              (PROGN
               (PROGN
                (PRIN2
                 "***** The list 'done_trafo' of inverse transformations")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "      is not complete as at least one transformation")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "      could not be inverted") NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "======================================================")
                NIL)
               (TERPRI))))
            (SETQ NEW_VAR_FNC (FNC_OF_NEW_VAR))
            NIL)))
         (AEVAL* (TERPRI))
         (SETQ FOUND (AEVAL* 'NIL))
         (WHILE (EVALNEQ (AEVAL* SOLNS) (AEVAL* (LIST 'LIST)))
                (PROGN
                 (SETQ DIVLIST (AEVAL* (LIST 'LIST)))
                 (SETQ CLLIST (AEVAL* (LIST 'LIST)))
                 (SETQ SOLN (AEVAL* (LIST 'SQFIRST SOLNS)))
                 (SETQ SOLNS (AEVAL* (LIST 'SQREST SOLNS)))
                 (SETQ CONDI (AEVAL* (LIST 'SQFIRST SOLN)))
                 (SETK 'CFCOPY (AEVAL* (LIST 'SUB (LIST 'SQSECOND SOLN) 'CF)))
                 (SETQ H1 (AEVAL* (LIST 'SQTHIRD SOLN)))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (PROGN
                    (PROGN
                     (ASSGNPRI (AEVAL* "cfcopy=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* 'CFCOPY) NIL 'LAST))
                    (PROGN
                     (ASSGNPRI (AEVAL* "soln=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* SOLN) NIL 'LAST))
                    (PROGN
                     (ASSGNPRI (AEVAL* "sqthird soln=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* H1) NIL 'LAST))
                    (AEVAL* 'NIL))))
                 (SETQ FL (AEVAL* (LIST 'LIST)))
                 (SETQ H2 (AEVAL* (LIST 'LIST)))
                 (PROG (E1)
                   (SETQ E1 (GETRLIST (AEVAL* H1)))
                  LAB
                   (COND ((NULL E1) (RETURN NIL)))
                   ((LAMBDA (E1)
                      (PROGN
                       (COND
                        ((NOT (FREEOF (REVALX CONDI) (REVALX E1)))
                         (SETQ FL (AEVAL* (LIST 'SQCONS E1 FL)))))
                       (COND
                        ((FREEOF (REVALX PARALIST) (REVALX E1))
                         (SETQ H2 (AEVAL* (LIST 'SQCONS E1 H2)))))))
                    (CAR E1))
                   (SETQ E1 (CDR E1))
                   (GO LAB))
                 (SETQ H1 (AEVAL* (LIST 'PARTI_FN H2 CONDI)))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (PROGN
                    (ASSGNPRI (AEVAL* "h1(partitioned)=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL* H1) NIL 'LAST))))
                 (SETQ EXTRALINE (AEVAL* 'NIL))
                 (SETQ NONCONSTC (AEVAL* (LIST 'LIST)))
                 (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                        (PROGN
                         (SETQ E1 (AEVAL* (LIST 'SQFIRST H1)))
                         (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                         (PROG (H4)
                           (SETQ H4 (GETRLIST (AEVAL* E1)))
                          LAB
                           (COND ((NULL H4) (RETURN NIL)))
                           ((LAMBDA (H4)
                              (COND
                               ((EVALNEQ (AEVAL* (LIST 'FARGS H4))
                                         (AEVAL* (LIST 'LIST)))
                                (PROGN
                                 (SETQ NONCONSTC
                                         (AEVAL* (LIST 'SQCONS H4 NONCONSTC)))
                                 (PROGN
                                  (PROGN
                                   (PRIN2 (REVAL1 H4 T))
                                   (PRIN2 " = ")
                                   NIL)
                                  (FCTPRINT (LIST (REVAL1 H4 T)))
                                  (PROGN (PRIN2 " is not constant!") NIL)
                                  (SETQ EXTRALINE T)
                                  (TERPRI))))))
                            (CAR H4))
                           (SETQ H4 (CDR H4))
                           (GO LAB))
                         (SETQ DEQU (AEVAL* 0))
                         (SETQ H2 (AEVAL* TREQLIST))
                         (COND
                          ((BOOLVALUE* PARALIST)
                           (SETQ H2
                                   (AEVAL*
                                    (LIST 'SUB (LIST 'SQSECOND SOLN) H2)))))
                         (COND
                          ((BOOLVALUE* CONTRACE)
                           (PROGN
                            (ASSGNPRI (AEVAL* "h2=") NIL 'FIRST)
                            (ASSGNPRI (AEVAL* H2) NIL 'LAST))))
                         (SETQ NONTRIV (AEVAL* 'NIL))
                         (SETQ H3
                                 (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ E2 (GETRLIST (AEVAL* 'CFCOPY)))
                                   (COND ((NULL E2) (RETURN (MAKELIST NIL))))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (E2)
                                                       (PROGN
                                                        (SETQ E3
                                                                (PROG (H4
                                                                       FORALL-RESULT)
                                                                  (SETQ H4
                                                                          (GETRLIST
                                                                           (AEVAL*
                                                                            E1)))
                                                                  (SETQ FORALL-RESULT
                                                                          0)
                                                                 LAB1
                                                                  (COND
                                                                   ((NULL H4)
                                                                    (RETURN
                                                                     FORALL-RESULT)))
                                                                  (SETQ FORALL-RESULT
                                                                          (AEVAL*
                                                                           (LIST
                                                                            'PLUS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  H4)
                                                                               (AEVAL*
                                                                                (LIST
                                                                                 'FDEPTERMS
                                                                                 E2
                                                                                 H4)))
                                                                             (CAR
                                                                              H4))
                                                                            FORALL-RESULT)))
                                                                  (SETQ H4
                                                                          (CDR
                                                                           H4))
                                                                  (GO LAB1)))
                                                        (SETQ DEQU
                                                                (AEVAL*
                                                                 (LIST 'PLUS
                                                                       DEQU
                                                                       (LIST
                                                                        'TIMES
                                                                        E3
                                                                        (LIST
                                                                         'SQFIRST
                                                                         H2)))))
                                                        (SETQ H2
                                                                (AEVAL*
                                                                 (LIST 'SQREST
                                                                       H2)))
                                                        (COND
                                                         ((EVALNEQ (AEVAL* E3)
                                                                   0)
                                                          (SETQ NONTRIV
                                                                  (AEVAL*
                                                                   'T))))
                                                        (AEVAL* E3)))
                                                     (CAR E2))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ E2 (CDR E2))
                                   (COND
                                    ((NULL E2)
                                     (RETURN (CONS 'LIST FORALL-RESULT))))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (E2)
                                               (PROGN
                                                (SETQ E3
                                                        (PROG (H4
                                                               FORALL-RESULT)
                                                          (SETQ H4
                                                                  (GETRLIST
                                                                   (AEVAL*
                                                                    E1)))
                                                          (SETQ FORALL-RESULT
                                                                  0)
                                                         LAB1
                                                          (COND
                                                           ((NULL H4)
                                                            (RETURN
                                                             FORALL-RESULT)))
                                                          (SETQ FORALL-RESULT
                                                                  (AEVAL*
                                                                   (LIST 'PLUS
                                                                         ((LAMBDA
                                                                              (
                                                                               H4)
                                                                            (AEVAL*
                                                                             (LIST
                                                                              'FDEPTERMS
                                                                              E2
                                                                              H4)))
                                                                          (CAR
                                                                           H4))
                                                                         FORALL-RESULT)))
                                                          (SETQ H4 (CDR H4))
                                                          (GO LAB1)))
                                                (SETQ DEQU
                                                        (AEVAL*
                                                         (LIST 'PLUS DEQU
                                                               (LIST 'TIMES E3
                                                                     (LIST
                                                                      'SQFIRST
                                                                      H2)))))
                                                (SETQ H2
                                                        (AEVAL*
                                                         (LIST 'SQREST H2)))
                                                (COND
                                                 ((EVALNEQ (AEVAL* E3) 0)
                                                  (SETQ NONTRIV (AEVAL* 'T))))
                                                (AEVAL* E3)))
                                             (CAR E2))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                         (COND
                          ((BOOLVALUE* NONTRIV)
                           (PROGN
                            (SETQ FOUND (AEVAL* 'T))
                            (SETQ CLLIST
                                    (AEVAL*
                                     (LIST 'SQCONS
                                           (PROGN
                                            (COND
                                             ((BOOLVALUE* CONTRACE)
                                              (PROGN
                                               (ASSGNPRI (AEVAL* "h3-1=") NIL
                                                         'FIRST)
                                               (ASSGNPRI (AEVAL* H3) NIL NIL)
                                               (ASSGNPRI (AEVAL* "  dequ=") NIL
                                                         NIL)
                                               (ASSGNPRI (AEVAL* DEQU) NIL
                                                         'LAST))))
                                            (SETQ SB
                                                    (AEVAL*
                                                     (LIST 'ABSORBCONST H3
                                                           E1)))
                                            (COND
                                             ((AND
                                               (EVALNEQ (AEVAL* SB)
                                                        (AEVAL* 'NIL))
                                               (EVALNEQ (AEVAL* SB) 0))
                                              (PROGN
                                               (SETQ H3
                                                       (AEVAL*
                                                        (LIST 'SUB SB H3)))
                                               (SETQ DEQU
                                                       (AEVAL*
                                                        (LIST 'SUB SB
                                                              DEQU))))))
                                            (COND
                                             ((BOOLVALUE* CONTRACE)
                                              (PROGN
                                               (ASSGNPRI (AEVAL* "h3-2=") NIL
                                                         'FIRST)
                                               (ASSGNPRI (AEVAL* H3) NIL NIL)
                                               (ASSGNPRI (AEVAL* "  dequ=") NIL
                                                         NIL)
                                               (ASSGNPRI (AEVAL* DEQU) NIL
                                                         'LAST))))
                                            (COND
                                             ((AND
                                               (EVALEQUAL
                                                (AEVAL* (LIST 'LENGTH E1)) 1)
                                               (EVALEQUAL
                                                (AEVAL*
                                                 (LIST 'FARGS
                                                       (LIST 'SQFIRST E1)))
                                                (AEVAL* (LIST 'LIST))))
                                              (PROGN
                                               (SETQ H4
                                                       (AEVAL*
                                                        (LIST 'SQFIRST E1)))
                                               (SETQ DEQU
                                                       (AEVAL*
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL H4
                                                                    1)
                                                              DEQU)))
                                               (AEVAL*
                                                (LIST 'SUB (LIST 'EQUAL H4 1)
                                                      H3))))
                                             (T (AEVAL* H3))))
                                           CLLIST)))
                            (SETQ DIVLIST
                                    (AEVAL* (LIST 'SQCONS DEQU DIVLIST))))))))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (PROGN
                    (ASSGNPRI (AEVAL* "characteristic functions found so far:")
                              NIL 'ONLY)
                    (ASSGNPRI (AEVAL* CLLIST) NIL 'ONLY)
                    (AEVAL* 'NIL))))
                 (COND
                  ((EVALNEQ (AEVAL* CONDI) (AEVAL* (LIST 'LIST)))
                   (PROGN
                    (COND
                     ((EVALEQUAL (AEVAL* (LIST 'LENGTH CONDI)) 1)
                      (PROGN (PRIN2 "There is a remaining condition: ") NIL))
                     (T
                      (PROGN (PRIN2 "There are remaining conditions: ") NIL)))
                    (PROGN
                     (DEPRINT (CDR (AEVAL* CONDI)))
                     (PROGN (PRIN2 "for the function(s): ") NIL)
                     (FCTPRINT (CDR (REVAL1 (AEVAL* FL) T)))
                     (TERPRI)
                     NIL)
                    (SETQ EXTRALINE (AEVAL* 'T))
                    (AEVAL* 'NIL))))
                 (COND
                  ((BOOLVALUE* EXTRALINE)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "- - - - - - - - - - - - - - - - - - - - - - - - - - - ")
                     NIL)
                    (TERPRI))))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (ASSGNPRI (AEVAL* "Start of dropping CLs of too low order")
                             NIL 'ONLY)))
                 (COND
                  ((AND (EVALGREATERP (AEVAL* DENSORD) 0)
                        (OR (EVALEQUAL (AEVAL* CF0) (AEVAL* 'NIL))
                            (EVALNEQ (AEVAL* MINDENSORD0) 0)))
                   (PROGN
                    (SETQ H1 (AEVAL* (LIST 'LIST)))
                    (SETQ H2 (AEVAL* (LIST 'LIST)))
                    (PROG (E1)
                      (SETQ E1 (GETRLIST (AEVAL* CLLIST)))
                     LAB
                      (COND ((NULL E1) (RETURN NIL)))
                      ((LAMBDA (E1)
                         (PROGN
                          (COND
                           ((BOOLVALUE* CONTRACE)
                            (PROGN
                             (ASSGNPRI (AEVAL* "----- Consideration of: ") NIL
                                       'ONLY)
                             (PROGN
                              (ASSGNPRI (AEVAL* "e1-0=") NIL 'FIRST)
                              (ASSGNPRI (AEVAL* E1) NIL 'LAST)))))
                          (SETQ E2 (AEVAL* (LIST 'LIST)))
                          (PROG (H5)
                            (SETQ H5 (GETRLIST (AEVAL* (LIST 'SQTHIRD SOLN))))
                           LAB
                            (COND ((NULL H5) (RETURN NIL)))
                            ((LAMBDA (H5)
                               (COND
                                ((NOT (FREEOF (REVALX E1) (REVALX H5)))
                                 (SETQ E2
                                         (AEVAL*
                                          (LIST 'SQAPPEND (LIST 'FARGS H5)
                                                E2))))))
                             (CAR H5))
                            (SETQ H5 (CDR H5))
                            (GO LAB))
                          (COND
                           ((BOOLVALUE* CONTRACE)
                            (PROGN
                             (ASSGNPRI (AEVAL* "e2-1=") NIL 'FIRST)
                             (ASSGNPRI (AEVAL* E2) NIL 'LAST))))
                          (COND
                           ((BOOLVALUE* CONTRACE)
                            (PROGN
                             (PROGN
                              (ASSGNPRI (AEVAL* "done_trafo=") NIL 'FIRST)
                              (ASSGNPRI (AEVAL* DONE_TRAFO) NIL 'LAST))
                             (PROGN
                              (ASSGNPRI (AEVAL* "paralist=") NIL 'FIRST)
                              (ASSGNPRI (AEVAL* PARALIST) NIL 'LAST)))))
                          (SETQ E3 (AEVAL* E1))
                          (COND
                           ((EVALNEQ (AEVAL* (LIST 'LIST)) (AEVAL* DONE_TRAFO))
                            (PROGN
                             (PROG (H5)
                               (SETQ H5 (GETRLIST (AEVAL* DONE_TRAFO)))
                              LAB
                               (COND ((NULL H5) (RETURN NIL)))
                               ((LAMBDA (H5)
                                  (PROGN
                                   (SETQ E3 (AEVAL* (LIST 'SUB H5 E3)))
                                   (SETQ E2 (AEVAL* (LIST 'SUB H5 E2)))
                                   (AEVAL* 'NIL)))
                                (CAR H5))
                               (SETQ H5 (CDR H5))
                               (GO LAB))
                             (COND
                              ((EVALNEQ (AEVAL* PARALIST)
                                        (AEVAL* (LIST 'LIST)))
                               (PROGN
                                (SETQ E3
                                        (AEVAL*
                                         (LIST 'SUB (LIST 'SQSECOND SOLN) E3)))
                                (SETQ E2
                                        (AEVAL*
                                         (LIST 'SUB (LIST 'SQSECOND SOLN)
                                               E2)))))))))
                          (COND
                           ((BOOLVALUE* CONTRACE)
                            (PROGN
                             (PROGN
                              (ASSGNPRI (AEVAL* "e1-2=") NIL 'FIRST)
                              (ASSGNPRI (AEVAL* E3) NIL 'LAST))
                             (PROGN
                              (ASSGNPRI (AEVAL* "e2-2=") NIL 'FIRST)
                              (ASSGNPRI (AEVAL* E2) NIL 'LAST)))))
                          (SETQ H5 (AEVAL* UDENS))
                          (WHILE
                           (AND (EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                                (FREEOF (REVALX E3)
                                        (REVAL1 (REVALX (LIST 'SQFIRST H5)) T))
                                (FREEOF (REVALX E2)
                                        (REVAL1 (REVALX (LIST 'SQFIRST H5))
                                                T)))
                           (SETQ H5 (AEVAL* (LIST 'SQREST H5))))
                          (COND
                           ((EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                            (PROGN
                             (SETQ H1 (AEVAL* (LIST 'SQCONS E1 H1)))
                             (SETQ H2
                                     (AEVAL*
                                      (LIST 'SQCONS (LIST 'SQFIRST DIVLIST)
                                            H2)))))
                           ((EVALEQUAL (AEVAL* (LIST 'LENGTH E1)) 1)
                            (PROGN
                             (ASSGNPRI (AEVAL* "Multiplier ") NIL 'FIRST)
                             (ASSGNPRI (AEVAL* (LIST 'FIRST E1)) NIL NIL)
                             (ASSGNPRI
                              (AEVAL*
                               " is dropped because it is of too low order.")
                              NIL 'LAST)))
                           (T
                            (PROGN
                             (ASSGNPRI (AEVAL* "Multipliers ") NIL 'FIRST)
                             (ASSGNPRI (AEVAL* E1) NIL NIL)
                             (ASSGNPRI
                              (AEVAL*
                               " are dropped because they are of too low order.")
                              NIL 'LAST))))
                          (SETQ DIVLIST (AEVAL* (LIST 'SQREST DIVLIST)))
                          (AEVAL* 'NIL)))
                       (CAR E1))
                      (SETQ E1 (CDR E1))
                      (GO LAB))
                    (SETQ CLLIST (AEVAL* H1))
                    (SETQ DIVLIST (AEVAL* H2)))))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (PROGN
                    (ASSGNPRI (AEVAL* "End of dropping CLs of too low order")
                              NIL 'ONLY)
                    (PROGN
                     (ASSGNPRI (AEVAL* "cllist=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* CLLIST) NIL 'LAST))
                    (PROGN
                     (ASSGNPRI (AEVAL* "divlist=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* DIVLIST) NIL 'LAST)))))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (ASSGNPRI (AEVAL* "Start of back-transformations") NIL
                             'ONLY)))
                 (COND
                  ((AND DONE_TRAFO (CDR DONE_TRAFO))
                   (PROGN
                    (SETQ H1
                            (PROG (H3 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ H3 (CDR DONE_TRAFO))
                             STARTOVER
                              (COND ((NULL H3) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (H3)
                                         (PROG (H6 FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ H6 (CDR H3))
                                           (COND ((NULL H6) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (H6)
                                                               (CADR H6))
                                                             (CAR H6))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ H6 (CDR H6))
                                           (COND
                                            ((NULL H6) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (H6) (CADR H6))
                                                     (CAR H6))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (CAR H3)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ H3 (CDR H3))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL H3) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (H3)
                                         (PROG (H6 FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ H6 (CDR H3))
                                           (COND ((NULL H6) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (H6)
                                                               (CADR H6))
                                                             (CAR H6))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ H6 (CDR H6))
                                           (COND
                                            ((NULL H6) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (H6) (CADR H6))
                                                     (CAR H6))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (CAR H3)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ H3 (CDR H3))
                              (GO LOOPLABEL)))
                    (SETQ H2 (SEARCH_LI2 CLLIST 'DF))
                    (SETQ H3 NIL)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT H2) (RETURN NIL)))
                      (PROGN
                       (COND
                        ((NOT (FREEOFLIST (CAR H2) H1))
                         (SETQ H3 (CONS (CAR H2) H3))))
                       (SETQ H2 (CDR H2)))
                      (GO WHILELABEL))
                    (SETQ H8 NIL)
                    (SETQ H9 H3)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT H9) (RETURN NIL)))
                      (PROGN
                       (COND
                        ((FREEOF (CDR H9) (CADAR H9))
                         (PROGN
                          (SETQ H8 (CONS (CAR H9) H8))
                          (SETQ H9 (CDR H9))))
                        (T
                         (PROGN
                          (SETQ H7 NIL)
                          (PROG (H2)
                            (SETQ H2 H9)
                           LAB
                            (COND ((NULL H2) (RETURN NIL)))
                            ((LAMBDA (H2)
                               (COND
                                ((FREEOF H2 (CADAR H9))
                                 (SETQ H7 (CONS H2 H7)))))
                             (CAR H2))
                            (SETQ H2 (CDR H2))
                            (GO LAB))
                          (SETQ H9 H7)))))
                      (GO WHILELABEL))
                    (SETQ H5 NIL)
                    (PROG (H4)
                      (SETQ H4 H3)
                     LAB
                      (COND ((NULL H4) (RETURN NIL)))
                      ((LAMBDA (H4)
                         (PROGN
                          (SETQ H7 (GENSYM))
                          (SETQ H5 (CONS (CONS H4 H7) H5))
                          (SETQ CLLIST (SUBST H7 H4 CLLIST))
                          (SETQ DIVLIST (SUBST H7 H4 DIVLIST))
                          NIL))
                       (CAR H4))
                      (SETQ H4 (CDR H4))
                      (GO LAB))
                    (PROG (H4)
                      (SETQ H4 (CDR DONE_TRAFO))
                     LAB
                      (COND ((NULL H4) (RETURN NIL)))
                      ((LAMBDA (H4)
                         (PROGN
                          (SETQ CLLIST (AEVAL* (LIST 'SUB H4 CLLIST)))
                          (SETQ DIVLIST (AEVAL* (LIST 'SUB H4 DIVLIST)))))
                       (CAR H4))
                      (SETQ H4 (CDR H4))
                      (GO LAB))
                    (PROG (H4)
                      (SETQ H4 H5)
                     LAB
                      (COND ((NULL H4) (RETURN NIL)))
                      ((LAMBDA (H4)
                         (COND
                          ((AND (MEMBER (CAR H4) H8) (FREEOF CONDI (CADAR H4))
                                (FREEOF CLLIST (CADAR H4))
                                (FREEOF DIVLIST (CADAR H4)))
                           (PROGN
                            (PROGN
                             (ASSGNPRI
                              (AEVAL*
                               "To simplify the following output we rename ")
                              NIL 'FIRST)
                             (ASSGNPRI (AEVAL* (CAR H4)) NIL NIL)
                             (ASSGNPRI (AEVAL* " --> ") NIL NIL)
                             (ASSGNPRI (AEVAL* (CADAR H4)) NIL NIL)
                             (ASSGNPRI (AEVAL* " .") NIL 'LAST))
                            (SETQ CLLIST (SUBST (CADAR H4) (CDR H4) CLLIST))
                            (SETQ DIVLIST
                                    (SUBST (CADAR H4) (CDR H4) DIVLIST))))
                          (T
                           (PROGN
                            (SETQ CLLIST (SUBST (CAR H4) (CDR H4) CLLIST))
                            (SETQ DIVLIST (SUBST (CAR H4) (CDR H4) DIVLIST))
                            NIL))))
                       (CAR H4))
                      (SETQ H4 (CDR H4))
                      (GO LAB))
                    NIL)))
                 (COND
                  ((BOOLVALUE* (REVALX 'NIL))
                   (COND
                    ((EVALNEQ (AEVAL* (LIST 'LIST)) (AEVAL* DONE_TRAFO))
                     (PROGN
                      (SETQ H2 (AEVAL* (LIST 'LIST)))
                      (SETQ H3 (AEVAL* (LIST 'LIST)))
                      (PROGN
                       (SETQ H6 CLLIST)
                       (SETQ H7 DIVLIST)
                       (COND
                        (H6
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT (CDR H6)) (RETURN NIL)))
                           (COND
                            ((FREEOFLIST (SEARCH_LI (CDADR H6) 'DF)
                              NEW_VAR_FNC)
                             (PROGN (SETQ H6 (CDR H6)) (SETQ H7 (CDR H7))))
                            (T
                             (PROGN
                              (PRINT_DROPPING_NOTICE (CADR H6))
                              (RPLACD H6 (CDDR H6))
                              (RPLACD H7 (CDDR H7))
                              NIL)))
                           (GO WHILELABEL)))))
                      (AEVAL* 'NIL))))))
                 (COND
                  ((BOOLVALUE* CONTRACE)
                   (ASSGNPRI (AEVAL* "End of back-transformations") NIL
                             'ONLY)))
                 (COND
                  ((EVALNEQ (AEVAL* CLLIST) (AEVAL* (LIST 'LIST)))
                   (PROGN
                    (COND
                     ((EVALNEQ (AEVAL* NO_CURRENT) (AEVAL* 'NIL))
                      (PROGN
                       (SETQ NON_INT
                               (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ E1 1)
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE
                                          (AEVAL* (LIST 'LENGTH CLLIST)) E1))
                                   (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS (AEVAL* E1) NIL)))
                                LOOPLABEL
                                 (SETQ E1
                                         ((LAMBDA (FORALL-RESULT)
                                            (AEVAL*
                                             (LIST 'PLUS FORALL-RESULT 1)))
                                          E1))
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE
                                          (AEVAL* (LIST 'LENGTH CLLIST)) E1))
                                   (RETURN (CONS 'LIST FORALL-RESULT))))
                                 (RPLACD FORALL-ENDPTR (CONS (AEVAL* E1) NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (SETQ H1 (AEVAL* (LIST 'LIST)))))
                     (T
                      (PROGN
                       (SETQ H1
                               (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ E1 1)
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE
                                          (AEVAL* (LIST 'LENGTH CLLIST)) E1))
                                   (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (AEVAL*
                                                   (LIST 'LIST
                                                         (LIST 'INTCURRENT1
                                                               (LIST 'REVAL
                                                                     (LIST
                                                                      'SQPART
                                                                      DIVLIST
                                                                      E1))
                                                               ULIST XLIST
                                                               DULIST NX EQORD
                                                               DENSORD)
                                                         E1))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ E1
                                         ((LAMBDA (FORALL-RESULT)
                                            (AEVAL*
                                             (LIST 'PLUS FORALL-RESULT 1)))
                                          E1))
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE
                                          (AEVAL* (LIST 'LENGTH CLLIST)) E1))
                                   (RETURN (CONS 'LIST FORALL-RESULT))))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (AEVAL*
                                           (LIST 'LIST
                                                 (LIST 'INTCURRENT1
                                                       (LIST 'REVAL
                                                             (LIST 'SQPART
                                                                   DIVLIST E1))
                                                       ULIST XLIST DULIST NX
                                                       EQORD DENSORD)
                                                 E1))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (PROG (E1)
                         (SETQ E1 (GETRLIST (AEVAL* ULIST)))
                        LAB
                         (COND ((NULL E1) (RETURN NIL)))
                         ((LAMBDA (E1)
                            (AEVAL* (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
                          (CAR E1))
                         (SETQ E1 (CDR E1))
                         (GO LAB))
                       (AEVAL* (ON (LIST 'EVALLHSEQP)))
                       (SETQ SB (AEVAL* (LIST 'SUBDIF1 XLIST ULIST MAXORD)))
                       (SETQ SB
                               (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ E1 (GETRLIST (AEVAL* SB)))
                                STARTOVER
                                 (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         ((LAMBDA (E1)
                                            (PROG (E2 FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ E2 (GETRLIST (AEVAL* E1)))
                                              (COND
                                               ((NULL E2)
                                                (RETURN (MAKELIST NIL))))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (E2)
                                                                  (AEVAL*
                                                                   (LIST 'EQUAL
                                                                         (LIST
                                                                          'RHS
                                                                          E2)
                                                                         (LIST
                                                                          'LHS
                                                                          E2))))
                                                                (CAR E2))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ E2 (CDR E2))
                                              (COND
                                               ((NULL E2)
                                                (RETURN
                                                 (CONS 'LIST FORALL-RESULT))))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E2)
                                                          (AEVAL*
                                                           (LIST 'EQUAL
                                                                 (LIST 'RHS E2)
                                                                 (LIST 'LHS
                                                                       E2))))
                                                        (CAR E2))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))
                                          (CAR E1)))
                                 (SETQ FORALL-ENDPTR
                                         (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                                 (SETQ E1 (CDR E1))
                                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                LOOPLABEL
                                 (COND ((NULL E1) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (GETRLIST
                                          ((LAMBDA (E1)
                                             (PROG (E2 FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ E2 (GETRLIST (AEVAL* E1)))
                                               (COND
                                                ((NULL E2)
                                                 (RETURN (MAKELIST NIL))))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (E2)
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'EQUAL
                                                                     (LIST 'RHS
                                                                           E2)
                                                                     (LIST 'LHS
                                                                           E2))))
                                                                 (CAR E2))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ E2 (CDR E2))
                                               (COND
                                                ((NULL E2)
                                                 (RETURN
                                                  (CONS 'LIST FORALL-RESULT))))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (E2)
                                                           (AEVAL*
                                                            (LIST 'EQUAL
                                                                  (LIST 'RHS
                                                                        E2)
                                                                  (LIST 'LHS
                                                                        E2))))
                                                         (CAR E2))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                           (CAR E1))))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                 (SETQ E1 (CDR E1))
                                 (GO LOOPLABEL)))
                       (AEVAL* (OFF (LIST 'EVALLHSEQP)))
                       (SETQ CLLIST (AEVAL* (LIST 'SUB SB CLLIST)))
                       (SETQ H2 (AEVAL* (LIST 'SUB SB H1)))
                       (SETQ H1 (AEVAL* (LIST 'LIST)))
                       (SETQ NON_INT (AEVAL* (LIST 'LIST)))
                       (PROG (E1)
                         (SETQ E1 (GETRLIST (AEVAL* H2)))
                        LAB
                         (COND ((NULL E1) (RETURN NIL)))
                         ((LAMBDA (E1)
                            (COND
                             ((BOOLVALUE* (REVALX (FREEOF E1 'SUB)))
                              (SETQ H1 (AEVAL* (LIST 'SQCONS E1 H1))))
                             (T
                              (SETQ NON_INT
                                      (AEVAL*
                                       (LIST 'SQCONS (LIST 'SQSECOND E1)
                                             NON_INT))))))
                          (CAR E1))
                         (SETQ E1 (CDR E1))
                         (GO LAB))
                       (SETQ H2 (AEVAL* 0))
                       (COND
                        ((EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                         (PROGN
                          (SETQ H2 (AEVAL* (INTERN (GENSYM))))
                          (PROG (E1)
                            (SETQ E1 (GETRLIST (AEVAL* ULIST)))
                           LAB
                            (COND ((NULL E1) (RETURN NIL)))
                            ((LAMBDA (E1)
                               (PROGN
                                (SETQ H10 (AEVAL* (LIST 'LIST)))
                                (PROG (E2)
                                  (SETQ E2 (GETRLIST (AEVAL* H1)))
                                 LAB
                                  (COND ((NULL E2) (RETURN NIL)))
                                  ((LAMBDA (E2)
                                     (PROGN
                                      (SETQ H8
                                              (AEVAL*
                                               (LIST 'SUB
                                                     (LIST 'EQUAL E1
                                                           (LIST 'TIMES H2 E1))
                                                     E2)))
                                      (COND
                                       ((BOOLVALUE* (REVALX (FREEOF H8 'SUB)))
                                        (SETQ H10
                                                (AEVAL*
                                                 (LIST 'SQCONS H8 H10))))
                                       (T
                                        (SETQ NON_INT
                                                (AEVAL*
                                                 (LIST 'SQCONS
                                                       (LIST 'SQSECOND H8)
                                                       NON_INT)))))))
                                   (CAR E2))
                                  (SETQ E2 (CDR E2))
                                  (GO LAB))
                                (SETQ H1 (AEVAL* H10))
                                (AEVAL* 'NIL)))
                             (CAR E1))
                            (SETQ E1 (CDR E1))
                            (GO LAB))
                          (COND
                           ((EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                            (SETQ H1
                                    (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ E1 (GETRLIST (AEVAL* H1)))
                                      (COND
                                       ((NULL E1) (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E1)
                                                          (PROGN
                                                           (SETQ E3
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'SQSECOND
                                                                     E1)))
                                                           (SETQ H10
                                                                   (AEVAL*
                                                                    (LIST 'SUB
                                                                          SB
                                                                          (LIST
                                                                           'SQPART
                                                                           DIVLIST
                                                                           E3))))
                                                           (SETQ E1
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'SQFIRST
                                                                     E1)))
                                                           (SETQ H9
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'INTCURRENT2
                                                                     H10
                                                                     (LIST
                                                                      'SQAPPEND
                                                                      NONCONSTC
                                                                      ULIST)
                                                                     XLIST)))
                                                           (COND
                                                            ((EVALEQUAL
                                                              (AEVAL*
                                                               (LIST 'SQSECOND
                                                                     H9))
                                                              0)
                                                             (SETQ H9
                                                                     (AEVAL*
                                                                      (LIST
                                                                       'SQFIRST
                                                                       H9))))
                                                            (T
                                                             (PROGN
                                                              (SETQ H9
                                                                      (AEVAL*
                                                                       NONDIV))
                                                              (SETQ H8
                                                                      (AEVAL*
                                                                       'T))
                                                              (PROG (E2)
                                                                (SETQ E2
                                                                        (GETRLIST
                                                                         (AEVAL*
                                                                          ULIST)))
                                                               LAB
                                                                (COND
                                                                 ((NULL E2)
                                                                  (RETURN
                                                                   NIL)))
                                                                ((LAMBDA (E2)
                                                                   (PROGN
                                                                    (COND
                                                                     ((BOOLVALUE*
                                                                       H8)
                                                                      (SETQ H10
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'ERR_CATCH_SUB
                                                                                E2
                                                                                0
                                                                                H10)))))
                                                                    (COND
                                                                     ((EQ
                                                                       (REVALX
                                                                        H10)
                                                                       (REVALX
                                                                        'NIL))
                                                                      (SETQ H8
                                                                              (AEVAL*
                                                                               'NIL))))))
                                                                 (CAR E2))
                                                                (SETQ E2
                                                                        (CDR
                                                                         E2))
                                                                (GO LAB))
                                                              (COND
                                                               ((BOOLVALUE*
                                                                 CONTRACE)
                                                                (PROGN
                                                                 (ASSGNPRI
                                                                  (AEVAL*
                                                                   "h10-1=")
                                                                  NIL 'FIRST)
                                                                 (ASSGNPRI
                                                                  (AEVAL* H10)
                                                                  NIL 'LAST))))
                                                              (COND
                                                               ((AND
                                                                 (BOOLVALUE*
                                                                  H8)
                                                                 (EVALNEQ
                                                                  (AEVAL* H10)
                                                                  0))
                                                                (PROGN
                                                                 (PROG (E2)
                                                                   (SETQ E2
                                                                           (GETRLIST
                                                                            (AEVAL*
                                                                             XLIST)))
                                                                  LAB
                                                                   (COND
                                                                    ((NULL E2)
                                                                     (RETURN
                                                                      NIL)))
                                                                   ((LAMBDA
                                                                        (E2)
                                                                      (PROGN
                                                                       (COND
                                                                        ((BOOLVALUE*
                                                                          H8)
                                                                         (SETQ H10
                                                                                 (AEVAL*
                                                                                  (LIST
                                                                                   'ERR_CATCH_SUB
                                                                                   E2
                                                                                   (LIST
                                                                                    'TIMES
                                                                                    H2
                                                                                    E2)
                                                                                   H10)))))
                                                                       (COND
                                                                        ((EQ
                                                                          (REVALX
                                                                           H10)
                                                                          (REVALX
                                                                           'NIL))
                                                                         (SETQ H8
                                                                                 (AEVAL*
                                                                                  'NIL))))))
                                                                    (CAR E2))
                                                                   (SETQ E2
                                                                           (CDR
                                                                            E2))
                                                                   (GO LAB))
                                                                 (COND
                                                                  ((BOOLVALUE*
                                                                    H8)
                                                                   (PROGN
                                                                    (COND
                                                                     ((BOOLVALUE*
                                                                       CONTRACE)
                                                                      (PROGN
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         "h10-2=")
                                                                        NIL
                                                                        'FIRST)
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         H10)
                                                                        NIL
                                                                        'LAST))))
                                                                    (SETQ H10
                                                                            (COND
                                                                             ((NOT
                                                                               (BOOLVALUE*
                                                                                (REVALX
                                                                                 (FREEOF
                                                                                  H10
                                                                                  'SUB))))
                                                                              (AEVAL*
                                                                               'NIL))
                                                                             (T
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'ERR_CATCH_INT
                                                                                (LIST
                                                                                 'TIMES
                                                                                 H10
                                                                                 (LIST
                                                                                  'EXPT
                                                                                  H2
                                                                                  (LIST
                                                                                   'DIFFERENCE
                                                                                   NX
                                                                                   1)))
                                                                                H2)))))
                                                                    (COND
                                                                     ((BOOLVALUE*
                                                                       CONTRACE)
                                                                      (PROGN
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         "h10-3=")
                                                                        NIL
                                                                        'FIRST)
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         H10)
                                                                        NIL
                                                                        'LAST))))
                                                                    (COND
                                                                     ((EQ
                                                                       (REVALX
                                                                        H10)
                                                                       (REVALX
                                                                        'NIL))
                                                                      (SETQ H6
                                                                              (AEVAL*
                                                                               'NIL)))
                                                                     (T
                                                                      (SETQ H6
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'ERR_CATCH_SUB
                                                                                H2
                                                                                1
                                                                                H10)))))
                                                                    (COND
                                                                     ((BOOLVALUE*
                                                                       CONTRACE)
                                                                      (PROGN
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         "h6=")
                                                                        NIL
                                                                        'FIRST)
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         H6)
                                                                        NIL
                                                                        'LAST))))
                                                                    (COND
                                                                     ((EQ
                                                                       (REVALX
                                                                        H6)
                                                                       (REVALX
                                                                        'NIL))
                                                                      (SETQ H7
                                                                              (AEVAL*
                                                                               'NIL)))
                                                                     (T
                                                                      (SETQ H7
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'ERR_CATCH_SUB
                                                                                H2
                                                                                0
                                                                                H10)))))
                                                                    (COND
                                                                     ((BOOLVALUE*
                                                                       CONTRACE)
                                                                      (PROGN
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         "h7=")
                                                                        NIL
                                                                        'FIRST)
                                                                       (ASSGNPRI
                                                                        (AEVAL*
                                                                         H7)
                                                                        NIL
                                                                        'LAST))))
                                                                    (COND
                                                                     ((EQ
                                                                       (REVALX
                                                                        H7)
                                                                       (REVALX
                                                                        'NIL))
                                                                      (SETQ H8
                                                                              (AEVAL*
                                                                               'NIL)))
                                                                     (T
                                                                      (SETQ H10
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'DIFFERENCE
                                                                                H6
                                                                                H7)))))))))))
                                                              (COND
                                                               ((BOOLVALUE*
                                                                 CONTRACE)
                                                                (PROGN
                                                                 (ASSGNPRI
                                                                  (AEVAL*
                                                                   "h10-4=")
                                                                  NIL 'FIRST)
                                                                 (ASSGNPRI
                                                                  (AEVAL* H10)
                                                                  NIL 'LAST))))
                                                              (SETQ H4
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'LIST)))
                                                              (SETQ H11
                                                                      (AEVAL*
                                                                       0))
                                                              (WHILE
                                                               (AND
                                                                (BOOLVALUE* H8)
                                                                (EVALNEQ
                                                                 (AEVAL* E1)
                                                                 (AEVAL*
                                                                  (LIST
                                                                   'LIST))))
                                                               (PROGN
                                                                (SETQ H11
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'PLUS
                                                                          H11
                                                                          1)))
                                                                (SETQ E2
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'SQFIRST
                                                                          E1)))
                                                                (SETQ E1
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'SQREST
                                                                          E1)))
                                                                (COND
                                                                 ((BOOLVALUE*
                                                                   CONTRACE)
                                                                  (PROGN
                                                                   (ASSGNPRI
                                                                    (AEVAL*
                                                                     "e2=")
                                                                    NIL 'FIRST)
                                                                   (ASSGNPRI
                                                                    (AEVAL* E2)
                                                                    NIL
                                                                    'LAST))))
                                                                (SETQ H3
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'ERR_CATCH_INT
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           E2
                                                                           H2)
                                                                          H2)))
                                                                (COND
                                                                 ((BOOLVALUE*
                                                                   CONTRACE)
                                                                  (PROGN
                                                                   (ASSGNPRI
                                                                    (AEVAL*
                                                                     "h3-1=")
                                                                    NIL 'FIRST)
                                                                   (ASSGNPRI
                                                                    (AEVAL* H3)
                                                                    NIL
                                                                    'LAST))))
                                                                (SETQ H6
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'ERR_CATCH_SUB
                                                                          H2 1
                                                                          H3)))
                                                                (COND
                                                                 ((EQ
                                                                   (REVALX H6)
                                                                   (REVALX
                                                                    'NIL))
                                                                  (SETQ H7
                                                                          (AEVAL*
                                                                           'NIL)))
                                                                 (T
                                                                  (SETQ H7
                                                                          (AEVAL*
                                                                           (LIST
                                                                            'ERR_CATCH_SUB
                                                                            H2
                                                                            0
                                                                            H3)))))
                                                                (COND
                                                                 ((EQ
                                                                   (REVALX H7)
                                                                   (REVALX
                                                                    'NIL))
                                                                  (SETQ H8
                                                                          (AEVAL*
                                                                           'NIL)))
                                                                 (T
                                                                  (SETQ H4
                                                                          (AEVAL*
                                                                           (LIST
                                                                            'SQCONS
                                                                            (LIST
                                                                             'PLUS
                                                                             (LIST
                                                                              'DIFFERENCE
                                                                              H6
                                                                              H7)
                                                                             (LIST
                                                                              'TIMES
                                                                              H10
                                                                              (LIST
                                                                               'SQPART
                                                                               XLIST
                                                                               H11)))
                                                                            H4)))))))
                                                              (COND
                                                               ((BOOLVALUE* H8)
                                                                (SETQ H9
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'SQREVERSE
                                                                          H4))))))))
                                                           (AEVAL*
                                                            (LIST 'LIST H9
                                                                  E3))))
                                                        (CAR E1))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ E1 (CDR E1))
                                      (COND
                                       ((NULL E1)
                                        (RETURN (CONS 'LIST FORALL-RESULT))))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (E1)
                                                  (PROGN
                                                   (SETQ E3
                                                           (AEVAL*
                                                            (LIST 'SQSECOND
                                                                  E1)))
                                                   (SETQ H10
                                                           (AEVAL*
                                                            (LIST 'SUB SB
                                                                  (LIST 'SQPART
                                                                        DIVLIST
                                                                        E3))))
                                                   (SETQ E1
                                                           (AEVAL*
                                                            (LIST 'SQFIRST
                                                                  E1)))
                                                   (SETQ H9
                                                           (AEVAL*
                                                            (LIST 'INTCURRENT2
                                                                  H10
                                                                  (LIST
                                                                   'SQAPPEND
                                                                   NONCONSTC
                                                                   ULIST)
                                                                  XLIST)))
                                                   (COND
                                                    ((EVALEQUAL
                                                      (AEVAL*
                                                       (LIST 'SQSECOND H9))
                                                      0)
                                                     (SETQ H9
                                                             (AEVAL*
                                                              (LIST 'SQFIRST
                                                                    H9))))
                                                    (T
                                                     (PROGN
                                                      (SETQ H9 (AEVAL* NONDIV))
                                                      (SETQ H8 (AEVAL* 'T))
                                                      (PROG (E2)
                                                        (SETQ E2
                                                                (GETRLIST
                                                                 (AEVAL*
                                                                  ULIST)))
                                                       LAB
                                                        (COND
                                                         ((NULL E2)
                                                          (RETURN NIL)))
                                                        ((LAMBDA (E2)
                                                           (PROGN
                                                            (COND
                                                             ((BOOLVALUE* H8)
                                                              (SETQ H10
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'ERR_CATCH_SUB
                                                                        E2 0
                                                                        H10)))))
                                                            (COND
                                                             ((EQ (REVALX H10)
                                                                  (REVALX
                                                                   'NIL))
                                                              (SETQ H8
                                                                      (AEVAL*
                                                                       'NIL))))))
                                                         (CAR E2))
                                                        (SETQ E2 (CDR E2))
                                                        (GO LAB))
                                                      (COND
                                                       ((BOOLVALUE* CONTRACE)
                                                        (PROGN
                                                         (ASSGNPRI
                                                          (AEVAL* "h10-1=") NIL
                                                          'FIRST)
                                                         (ASSGNPRI (AEVAL* H10)
                                                                   NIL
                                                                   'LAST))))
                                                      (COND
                                                       ((AND (BOOLVALUE* H8)
                                                             (EVALNEQ
                                                              (AEVAL* H10) 0))
                                                        (PROGN
                                                         (PROG (E2)
                                                           (SETQ E2
                                                                   (GETRLIST
                                                                    (AEVAL*
                                                                     XLIST)))
                                                          LAB
                                                           (COND
                                                            ((NULL E2)
                                                             (RETURN NIL)))
                                                           ((LAMBDA (E2)
                                                              (PROGN
                                                               (COND
                                                                ((BOOLVALUE*
                                                                  H8)
                                                                 (SETQ H10
                                                                         (AEVAL*
                                                                          (LIST
                                                                           'ERR_CATCH_SUB
                                                                           E2
                                                                           (LIST
                                                                            'TIMES
                                                                            H2
                                                                            E2)
                                                                           H10)))))
                                                               (COND
                                                                ((EQ
                                                                  (REVALX H10)
                                                                  (REVALX
                                                                   'NIL))
                                                                 (SETQ H8
                                                                         (AEVAL*
                                                                          'NIL))))))
                                                            (CAR E2))
                                                           (SETQ E2 (CDR E2))
                                                           (GO LAB))
                                                         (COND
                                                          ((BOOLVALUE* H8)
                                                           (PROGN
                                                            (COND
                                                             ((BOOLVALUE*
                                                               CONTRACE)
                                                              (PROGN
                                                               (ASSGNPRI
                                                                (AEVAL*
                                                                 "h10-2=")
                                                                NIL 'FIRST)
                                                               (ASSGNPRI
                                                                (AEVAL* H10)
                                                                NIL 'LAST))))
                                                            (SETQ H10
                                                                    (COND
                                                                     ((NOT
                                                                       (BOOLVALUE*
                                                                        (REVALX
                                                                         (FREEOF
                                                                          H10
                                                                          'SUB))))
                                                                      (AEVAL*
                                                                       'NIL))
                                                                     (T
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'ERR_CATCH_INT
                                                                        (LIST
                                                                         'TIMES
                                                                         H10
                                                                         (LIST
                                                                          'EXPT
                                                                          H2
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           NX
                                                                           1)))
                                                                        H2)))))
                                                            (COND
                                                             ((BOOLVALUE*
                                                               CONTRACE)
                                                              (PROGN
                                                               (ASSGNPRI
                                                                (AEVAL*
                                                                 "h10-3=")
                                                                NIL 'FIRST)
                                                               (ASSGNPRI
                                                                (AEVAL* H10)
                                                                NIL 'LAST))))
                                                            (COND
                                                             ((EQ (REVALX H10)
                                                                  (REVALX
                                                                   'NIL))
                                                              (SETQ H6
                                                                      (AEVAL*
                                                                       'NIL)))
                                                             (T
                                                              (SETQ H6
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'ERR_CATCH_SUB
                                                                        H2 1
                                                                        H10)))))
                                                            (COND
                                                             ((BOOLVALUE*
                                                               CONTRACE)
                                                              (PROGN
                                                               (ASSGNPRI
                                                                (AEVAL* "h6=")
                                                                NIL 'FIRST)
                                                               (ASSGNPRI
                                                                (AEVAL* H6) NIL
                                                                'LAST))))
                                                            (COND
                                                             ((EQ (REVALX H6)
                                                                  (REVALX
                                                                   'NIL))
                                                              (SETQ H7
                                                                      (AEVAL*
                                                                       'NIL)))
                                                             (T
                                                              (SETQ H7
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'ERR_CATCH_SUB
                                                                        H2 0
                                                                        H10)))))
                                                            (COND
                                                             ((BOOLVALUE*
                                                               CONTRACE)
                                                              (PROGN
                                                               (ASSGNPRI
                                                                (AEVAL* "h7=")
                                                                NIL 'FIRST)
                                                               (ASSGNPRI
                                                                (AEVAL* H7) NIL
                                                                'LAST))))
                                                            (COND
                                                             ((EQ (REVALX H7)
                                                                  (REVALX
                                                                   'NIL))
                                                              (SETQ H8
                                                                      (AEVAL*
                                                                       'NIL)))
                                                             (T
                                                              (SETQ H10
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'DIFFERENCE
                                                                        H6
                                                                        H7)))))))))))
                                                      (COND
                                                       ((BOOLVALUE* CONTRACE)
                                                        (PROGN
                                                         (ASSGNPRI
                                                          (AEVAL* "h10-4=") NIL
                                                          'FIRST)
                                                         (ASSGNPRI (AEVAL* H10)
                                                                   NIL
                                                                   'LAST))))
                                                      (SETQ H4
                                                              (AEVAL*
                                                               (LIST 'LIST)))
                                                      (SETQ H11 (AEVAL* 0))
                                                      (WHILE
                                                       (AND (BOOLVALUE* H8)
                                                            (EVALNEQ
                                                             (AEVAL* E1)
                                                             (AEVAL*
                                                              (LIST 'LIST))))
                                                       (PROGN
                                                        (SETQ H11
                                                                (AEVAL*
                                                                 (LIST 'PLUS
                                                                       H11 1)))
                                                        (SETQ E2
                                                                (AEVAL*
                                                                 (LIST 'SQFIRST
                                                                       E1)))
                                                        (SETQ E1
                                                                (AEVAL*
                                                                 (LIST 'SQREST
                                                                       E1)))
                                                        (COND
                                                         ((BOOLVALUE* CONTRACE)
                                                          (PROGN
                                                           (ASSGNPRI
                                                            (AEVAL* "e2=") NIL
                                                            'FIRST)
                                                           (ASSGNPRI
                                                            (AEVAL* E2) NIL
                                                            'LAST))))
                                                        (SETQ H3
                                                                (AEVAL*
                                                                 (LIST
                                                                  'ERR_CATCH_INT
                                                                  (LIST
                                                                   'QUOTIENT E2
                                                                   H2)
                                                                  H2)))
                                                        (COND
                                                         ((BOOLVALUE* CONTRACE)
                                                          (PROGN
                                                           (ASSGNPRI
                                                            (AEVAL* "h3-1=")
                                                            NIL 'FIRST)
                                                           (ASSGNPRI
                                                            (AEVAL* H3) NIL
                                                            'LAST))))
                                                        (SETQ H6
                                                                (AEVAL*
                                                                 (LIST
                                                                  'ERR_CATCH_SUB
                                                                  H2 1 H3)))
                                                        (COND
                                                         ((EQ (REVALX H6)
                                                              (REVALX 'NIL))
                                                          (SETQ H7
                                                                  (AEVAL*
                                                                   'NIL)))
                                                         (T
                                                          (SETQ H7
                                                                  (AEVAL*
                                                                   (LIST
                                                                    'ERR_CATCH_SUB
                                                                    H2 0
                                                                    H3)))))
                                                        (COND
                                                         ((EQ (REVALX H7)
                                                              (REVALX 'NIL))
                                                          (SETQ H8
                                                                  (AEVAL*
                                                                   'NIL)))
                                                         (T
                                                          (SETQ H4
                                                                  (AEVAL*
                                                                   (LIST
                                                                    'SQCONS
                                                                    (LIST 'PLUS
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           H6
                                                                           H7)
                                                                          (LIST
                                                                           'TIMES
                                                                           H10
                                                                           (LIST
                                                                            'SQPART
                                                                            XLIST
                                                                            H11)))
                                                                    H4)))))))
                                                      (COND
                                                       ((BOOLVALUE* H8)
                                                        (SETQ H9
                                                                (AEVAL*
                                                                 (LIST
                                                                  'SQREVERSE
                                                                  H4))))))))
                                                   (AEVAL*
                                                    (LIST 'LIST H9 E3))))
                                                (CAR E1))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))))))))
                       (COND
                        ((BOOLVALUE* CONTRACE)
                         (PROGN
                          (ASSGNPRI (AEVAL* "h1-1=") NIL 'FIRST)
                          (ASSGNPRI (AEVAL* H1) NIL 'LAST))))
                       (AEVAL* 'NIL))))
                    (PROG (E2)
                      (SETQ E2 (GETRLIST (AEVAL* NON_INT)))
                     LAB
                      (COND ((NULL E2) (RETURN NIL)))
                      ((LAMBDA (E2)
                         (PROGN
                          (PROGN
                           (TERPRI)
                           (PROGN (PRIN2 "Conservation law:") NIL)
                           (TERPRI)
                           (PROGN (PRIN2 "-----------------") NIL)
                           (TERPRI))
                          (COND
                           ((EVALNEQ (AEVAL* PARALIST) (AEVAL* (LIST 'LIST)))
                            (PROG (H3)
                              (SETQ H3
                                      (GETRLIST
                                       (AEVAL* (LIST 'SQSECOND SOLN))))
                             LAB
                              (COND ((NULL H3) (RETURN NIL)))
                              ((LAMBDA (H3)
                                 (COND
                                  ((NOT
                                    (FREEOF (REVALX PARALIST)
                                            (REVALX (LIST 'LHS H3))))
                                   (PROGN
                                    (PROGN
                                     (ASSGNPRI (AEVAL* H3) NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* ",") NIL 'LAST))
                                    (AEVAL* (TERPRI))))))
                               (CAR H3))
                              (SETQ H3 (CDR H3))
                              (GO LAB))))
                          (PROG (E1)
                            (SETQ E1 (GETRLIST (AEVAL* ULIST)))
                           LAB
                            (COND ((NULL E1) (RETURN NIL)))
                            ((LAMBDA (E1)
                               (AEVAL*
                                (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
                             (CAR E1))
                            (SETQ E1 (CDR E1))
                            (GO LAB))
                          (AEVAL* (ON (LIST 'EVALLHSEQP)))
                          (SETQ SB (AEVAL* (LIST 'SUBDIF1 XLIST ULIST MAXORD)))
                          (SETQ SB
                                  (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ E1 (GETRLIST (AEVAL* SB)))
                                   STARTOVER
                                    (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (E1)
                                               (PROG (E2 FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ E2
                                                         (GETRLIST
                                                          (AEVAL* E1)))
                                                 (COND
                                                  ((NULL E2)
                                                   (RETURN (MAKELIST NIL))))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (E2)
                                                                     (AEVAL*
                                                                      (LIST
                                                                       'EQUAL
                                                                       (LIST
                                                                        'RHS
                                                                        E2)
                                                                       (LIST
                                                                        'LHS
                                                                        E2))))
                                                                   (CAR E2))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ E2 (CDR E2))
                                                 (COND
                                                  ((NULL E2)
                                                   (RETURN
                                                    (CONS 'LIST
                                                          FORALL-RESULT))))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (E2)
                                                             (AEVAL*
                                                              (LIST 'EQUAL
                                                                    (LIST 'RHS
                                                                          E2)
                                                                    (LIST 'LHS
                                                                          E2))))
                                                           (CAR E2))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                             (CAR E1)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR
                                             (CONS 'LIST FORALL-RESULT)))
                                    (SETQ E1 (CDR E1))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL E1) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (GETRLIST
                                             ((LAMBDA (E1)
                                                (PROG (E2 FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ E2
                                                          (GETRLIST
                                                           (AEVAL* E1)))
                                                  (COND
                                                   ((NULL E2)
                                                    (RETURN (MAKELIST NIL))))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA
                                                                        (E2)
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'RHS
                                                                         E2)
                                                                        (LIST
                                                                         'LHS
                                                                         E2))))
                                                                    (CAR E2))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ E2 (CDR E2))
                                                  (COND
                                                   ((NULL E2)
                                                    (RETURN
                                                     (CONS 'LIST
                                                           FORALL-RESULT))))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (E2)
                                                              (AEVAL*
                                                               (LIST 'EQUAL
                                                                     (LIST 'RHS
                                                                           E2)
                                                                     (LIST 'LHS
                                                                           E2))))
                                                            (CAR E2))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL)))
                                              (CAR E1))))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ E1 (CDR E1))
                                    (GO LOOPLABEL)))
                          (AEVAL* (OFF (LIST 'EVALLHSEQP)))
                          (SETQ CLLIST (AEVAL* (LIST 'SUB SB CLLIST)))
                          (SETQ H3 (AEVAL* (LIST 'SQPART CLLIST E2)))
                          (SETQ H4 (AEVAL* EQLIST))
                          (COND
                           ((EVALNEQ (AEVAL* PARALIST) (AEVAL* (LIST 'LIST)))
                            (PROGN
                             (SETQ H4
                                     (AEVAL*
                                      (LIST 'SUB (LIST 'SQSECOND SOLN) H4)))
                             (SETQ H3
                                     (AEVAL*
                                      (LIST 'SUB (LIST 'SQSECOND SOLN) H3)))
                             (AEVAL* 'NIL))))
                          (PROGN
                           (ASSGNPRI (AEVAL* "  ( ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* (LIST 'SQFIRST H3)) NIL NIL)
                           (ASSGNPRI (AEVAL* " ) * ( ") NIL NIL)
                           (ASSGNPRI (AEVAL* (LIST 'SQFIRST H4)) NIL NIL)
                           (ASSGNPRI (AEVAL* " )") NIL 'LAST))
                          (SETQ H9
                                  (AEVAL*
                                   (LIST 'TIMES (LIST 'SQFIRST H3)
                                         (LIST 'SQFIRST H4))))
                          (SETQ H3 (AEVAL* (LIST 'SQREST H3)))
                          (SETQ H4 (AEVAL* (LIST 'SQREST H4)))
                          (WHILE (EVALNEQ (AEVAL* H3) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (PROGN
                                   (ASSGNPRI (AEVAL* "+ ( ") NIL 'FIRST)
                                   (ASSGNPRI (AEVAL* (LIST 'SQFIRST H3)) NIL
                                             NIL)
                                   (ASSGNPRI (AEVAL* " ) * ( ") NIL NIL)
                                   (ASSGNPRI (AEVAL* (LIST 'SQFIRST H4)) NIL
                                             NIL)
                                   (ASSGNPRI (AEVAL* " )") NIL 'LAST))
                                  (SETQ H9
                                          (AEVAL*
                                           (LIST 'PLUS H9
                                                 (LIST 'TIMES
                                                       (LIST 'SQFIRST H3)
                                                       (LIST 'SQFIRST H4)))))
                                  (SETQ H3 (AEVAL* (LIST 'SQREST H3)))
                                  (SETQ H4 (AEVAL* (LIST 'SQREST H4)))
                                  (AEVAL* 'NIL)))
                          (PROGN
                           (ASSGNPRI (AEVAL* "= ") NIL 'FIRST)
                           (ASSGNPRI (AEVAL* H9) NIL 'LAST))
                          (PROGN
                           (PROGN
                            (PRIN2
                             "should be a divergence but the program was")
                            NIL)
                           (TERPRI)
                           (COND
                            (NO_CURRENT
                             (PROGN
                              (PRIN2
                               "not instructed to try finding the conserved current.")
                              NIL))
                            (T
                             (PROGN
                              (PRIN2 "not able to find the conserved current.")
                              NIL)))
                           (TERPRI)
                           NIL)
                          (COND
                           ((EVALNEQ (AEVAL* CONDI) (AEVAL* (LIST 'LIST)))
                            (PROGN
                             (PROGN
                              (PRIN2
                               "For that the remaining conditions should be applied.")
                              NIL)
                             (TERPRI))))
                          (ASSGNPRI
                           (AEVAL*
                            "======================================================")
                           NIL 'ONLY)
                          (AEVAL* 'NIL)))
                       (CAR E2))
                      (SETQ E2 (CDR E2))
                      (GO LAB))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (PROGN
                        (ASSGNPRI (AEVAL* "h1=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* H1) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "cllist=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* CLLIST) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "eqlist=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* EQLIST) NIL 'LAST)))))
                    (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                           (PROGN
                            (SETQ H9
                                    (AEVAL*
                                     (LIST 'SQSECOND (LIST 'SQFIRST H1))))
                            (SETQ H2
                                    (AEVAL*
                                     (LIST 'SQFIRST (LIST 'SQFIRST H1))))
                            (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                            (SETQ H3 (AEVAL* (LIST 'SQPART CLLIST H9)))
                            (COND
                             ((EVALNEQ (AEVAL* PARALIST) (AEVAL* (LIST 'LIST)))
                              (PROG (E2)
                                (SETQ E2
                                        (GETRLIST
                                         (AEVAL* (LIST 'SQSECOND SOLN))))
                               LAB
                                (COND ((NULL E2) (RETURN NIL)))
                                ((LAMBDA (E2)
                                   (COND
                                    ((NOT
                                      (FREEOF (REVALX PARALIST)
                                              (REVALX (LIST 'LHS E2))))
                                     (PROGN
                                      (ASSGNPRI (AEVAL* E2) NIL 'ONLY)
                                      (AEVAL* (TERPRI))
                                      (SETQ H2 (AEVAL* (LIST 'SUB E2 H2)))
                                      (SETQ H3 (AEVAL* (LIST 'SUB E2 H3)))))))
                                 (CAR E2))
                                (SETQ E2 (CDR E2))
                                (GO LAB))))
                            (SETQ RTNLIST
                                    (AEVAL*
                                     (LIST 'SQCONS (LIST 'LIST H3 H2)
                                           RTNLIST)))
                            (SETQ H4 (AEVAL* EQLIST))
                            (COND
                             ((EVALNEQ (AEVAL* PARALIST) (AEVAL* (LIST 'LIST)))
                              (SETQ H4
                                      (AEVAL*
                                       (LIST 'SUB (LIST 'SQSECOND SOLN) H4)))))
                            (SETQ H8 (AEVAL* 0))
                            (COND
                             ((EVALNEQ (AEVAL* H2) (AEVAL* NONDIV))
                              (PROGN
                               (SETQ H5 (AEVAL* H4))
                               (PROG (E1)
                                 (SETQ E1 (GETRLIST (AEVAL* H3)))
                                LAB
                                 (COND ((NULL E1) (RETURN NIL)))
                                 ((LAMBDA (E1)
                                    (PROGN
                                     (SETQ H8
                                             (AEVAL*
                                              (LIST 'PLUS H8
                                                    (LIST 'TIMES E1
                                                          (LIST 'SQFIRST
                                                                H5)))))
                                     (SETQ H5 (AEVAL* (LIST 'SQREST H5)))))
                                  (CAR E1))
                                 (SETQ E1 (CDR E1))
                                 (GO LAB))
                               (PROG (E1)
                                 (SETQ E1 1)
                                LAB
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE (AEVAL* NX) E1))
                                   (RETURN NIL)))
                                 (PROGN
                                  (SETQ H8
                                          (AEVAL*
                                           (LIST 'DIFFERENCE H8
                                                 (LIST 'DF (LIST 'SQPART H2 E1)
                                                       (LIST 'SQPART XLIST
                                                             E1)))))
                                  (AEVAL* 'NIL))
                                 (SETQ E1
                                         ((LAMBDA (FORALL-RESULT)
                                            (AEVAL*
                                             (LIST 'PLUS FORALL-RESULT 1)))
                                          E1))
                                 (GO LAB))
                               (COND
                                ((EVALNEQ (AEVAL* H8) 0)
                                 (SETQ H2 (AEVAL* NONDIV)))))))
                            (PROGN
                             (TERPRI)
                             (PROGN (PRIN2 "Conservation law:") NIL)
                             (TERPRI)
                             (PROGN (PRIN2 "-----------------") NIL)
                             (TERPRI))
                            (COND
                             ((EVALNEQ (AEVAL* H2) (AEVAL* NONDIV))
                              (PROGN
                               (COND
                                ((EVALEQUAL (AEVAL* NX) 2)
                                 (SETQ H2
                                         (AEVAL*
                                          (LIST 'SQFIRST
                                                (LIST 'SIMPPL (LIST 'LIST H2)
                                                      ULIST
                                                      (LIST 'SQFIRST XLIST)
                                                      (LIST 'SQSECOND
                                                            XLIST)))))))
                               (AEVAL* (LIST 'PRINT_CLAW H4 H3 H2 XLIST))
                               (SETQ H6 (AEVAL* (LIST 'LIST)))
                               (PROG (H5)
                                 (SETQ H5 (GETRLIST (AEVAL* NONCONSTC)))
                                LAB
                                 (COND ((NULL H5) (RETURN NIL)))
                                 ((LAMBDA (H5)
                                    (COND
                                     ((NOT (FREEOF (REVALX H3) (REVALX H5)))
                                      (SETQ H6
                                              (AEVAL* (LIST 'SQCONS H5 H6))))))
                                  (CAR H5))
                                 (SETQ H5 (CDR H5))
                                 (GO LAB))
                               (COND
                                ((EVALNEQ (AEVAL* H6) (AEVAL* (LIST 'LIST)))
                                 (AEVAL*
                                  (LIST 'PARTINTDF H4 H3 H2 XLIST H6 VL
                                        SB))))))
                             (T
                              (PROGN
                               (PROGN
                                (ASSGNPRI (AEVAL* "  ( ") NIL 'FIRST)
                                (ASSGNPRI (AEVAL* (LIST 'SQFIRST H3)) NIL NIL)
                                (ASSGNPRI (AEVAL* " ) * ( ") NIL NIL)
                                (ASSGNPRI (AEVAL* (LIST 'SQFIRST H4)) NIL NIL)
                                (ASSGNPRI (AEVAL* " )") NIL 'LAST))
                               (SETQ H3 (AEVAL* (LIST 'SQREST H3)))
                               (SETQ H4 (AEVAL* (LIST 'SQREST H4)))
                               (WHILE
                                (EVALNEQ (AEVAL* H3) (AEVAL* (LIST 'LIST)))
                                (PROGN
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "+ ( ") NIL 'FIRST)
                                  (ASSGNPRI (AEVAL* (LIST 'SQFIRST H3)) NIL
                                            NIL)
                                  (ASSGNPRI (AEVAL* " ) * ( ") NIL NIL)
                                  (ASSGNPRI (AEVAL* (LIST 'SQFIRST H4)) NIL
                                            NIL)
                                  (ASSGNPRI (AEVAL* " )") NIL 'LAST))
                                 (SETQ H3 (AEVAL* (LIST 'SQREST H3)))
                                 (SETQ H4 (AEVAL* (LIST 'SQREST H4)))))
                               (PROGN
                                (PROGN
                                 (PRIN2
                                  "should be a divergence but the program was")
                                 NIL)
                                (TERPRI)
                                (PROGN
                                 (PRIN2
                                  "not able to find the conserved current.")
                                 NIL)
                                (TERPRI)
                                NIL))))
                            (ASSGNPRI
                             (AEVAL*
                              "======================================================")
                             NIL 'ONLY)
                            (AEVAL* 'NIL)))
                    (AEVAL* 'NIL))))
                 (AEVAL* (LIST 'NODEPENDLIST ULIST))
                 (AEVAL* 'NIL)))
         (COND
          ((EVALEQUAL (AEVAL* FOUND) (AEVAL* 'NIL))
           (PROGN
            (ASSGNPRI (AEVAL* "There is no conservation law of this order.")
                      NIL 'ONLY)
            (ASSGNPRI
             (AEVAL* "======================================================")
             NIL 'ONLY)
            (AEVAL* 'NIL)))))
        (SETQ DENSORD
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 DENSORD))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FARGS (LIST 'SQFIRST ULIST)))
                   (AEVAL (LIST 'LIST)))
        (PROG (E1)
          (SETQ E1 (GETRLIST (AEVAL ULIST)))
         LAB
          (COND ((NULL E1) (RETURN NIL)))
          ((LAMBDA (E1) (AEVAL (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
           (CAR E1))
          (SETQ E1 (CDR E1))
          (GO LAB))))
      (COND
       ((BOOLVALUE* (REVALX *TIME))
        (PROGN
         (ASSGNPRI (AEVAL "time to run conlaw4: ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'DIFFERENCE (TIME) CPUSTART)) NIL NIL)
         (ASSGNPRI (AEVAL " ms    GC time : ") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'DIFFERENCE (GCTIME) GCSTART)) NIL NIL)
         (ASSGNPRI (AEVAL " ms") NIL 'LAST))))
      (PROGN
       (SETQ ADJUST_FNC ADJUSTOLD)
       (SETQ LOGOPRINT_ LOGOOLD)
       (SETQ POTINT_ POTOLD)
       (SETQ FACINT_ FACOLD))
      (AEVAL (LIST 'RECOVER_REDUCE_FLAGS))
      (RETURN (AEVAL RTNLIST)))) 