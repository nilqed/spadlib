(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID
 '(PRINT_ LOGOPRINT_ POTINT_ FACINT_ ADJUST_FNC QUASILIN_RHS FLIN_ DONE_TRAFO
   INVERSE_TRAFO_LIST_INCOMPLETE)) 
(PUT 'CONLAW3 'NUMBER-OF-ARGS 2) 
(FLAG '(CONLAW3) 'OPFN) 
(PUT 'CONLAW3 'DEFINED-ON-LINE '37) 
(PUT 'CONLAW3 'DEFINED-IN-FILE 'CRACK/CONLAW3.RED) 
(PUT 'CONLAW3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONLAW3 (PROBLEM RUNMODE)
    (PROG (CONTRACE EQLIST ULIST XLIST DEQU CLLIST PLLIST SB DENSORD FLIST
           EQORD MAXORD DULIST REVDULIST VL EXPL DEPLIST E1 E2 E3 N H1 H2 H3 H4
           H5 H6 CONDI SOLN ADJUST_OLD POTOLD ADJUSTOLD UDENS GENSEPOLD INEQU0
           INEQU LOGOOLD TREQLIST FL FACOLD U NODEP CPU GC CPUSTART GCSTART
           FOUND CF0 RTNLIST SOLNS NONTRIV EXTRALINE CF CFCOPY NX NDE
           MINDENSORD MINDENSORD0 MAXDENSORD ABSMAXORD NONCONSTC NEW_VAR_FNC)
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
          (PRIN2 "This is CONLAW3 - a program for calculating conservation")
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
      (SETK 'PARALIST (AEVAL (LIST 'LIST)))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL FLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((NOT (FREEOF (REVALX EQLIST) (REVALX E1)))
             (SETK 'PARALIST (AEVAL (LIST 'SQCONS E1 'PARALIST))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ EQORD (AEVAL 0))
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
      (SETQ H3 (AEVAL EQORD))
      (SETQ MINDENSORD0 (AEVAL MINDENSORD))
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
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NX) N)) (RETURN NIL)))
        (PROGN
         (SETQ H4 (REVAL1 (AEVAL* (LIST 'SQPART XLIST N)) T))
         (SETQ H1 (AEVAL* (LIST 'MKID 'P_ H4)))
         (COND
          ((NOT (BOOLVALUE* (REVALX (NULL (GET (MKID 'P_ H4) 'AVALUE)))))
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
                   ((AND (EVALGEQ (AEVAL* H2) (AEVAL* H3))
                         (EVALLEQ (AEVAL* MINDENSORD) (AEVAL* H2)))
                    (SETQ MINDENSORD (AEVAL* (LIST 'PLUS H2 1)))))))
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
       ((EVALLESSP (AEVAL MAXDENSORD) (AEVAL MINDENSORD))
        (SETQ MAXDENSORD (AEVAL MINDENSORD))))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "eqord=") NIL 'FIRST)
         (ASSGNPRI (AEVAL EQORD) NIL NIL)
         (ASSGNPRI (AEVAL " cf0=") NIL NIL)
         (ASSGNPRI (AEVAL CF0) NIL 'LAST))))
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
      (COND
       ((BOOLVALUE* CF0)
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NX) N)) (RETURN NIL)))
          (PROGN
           (SETQ H4 (REVAL1 (AEVAL* (LIST 'SQPART XLIST N)) T))
           (SETQ H1 (AEVAL* (LIST 'MKID 'P_ H4)))
           (COND
            ((NOT (BOOLVALUE* (REVALX (NULL (GET (MKID 'P_ H4) 'AVALUE)))))
             (PROGN
              (PROG (E1)
                (SETQ E1 (GETRLIST (AEVAL* SB)))
               LAB
                (COND ((NULL E1) (RETURN NIL)))
                ((LAMBDA (E1) (SETQ H1 (AEVAL* (LIST 'SUB E1 H1)))) (CAR E1))
                (SETQ E1 (CDR E1))
                (GO LAB))
              (SETK (MKID 'P_ H4) (AEVAL* H1))
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
                 (COND
                  ((EVALGREATERP (AEVAL* EQORD) (AEVAL* DENSORD))
                   (AEVAL* EQORD))
                  (T (AEVAL* DENSORD))))
         (SETQ ABSMAXORD
                 (COND
                  ((BOOLVALUE* (REVALX (NULL QUASILIN_RHS)))
                   (AEVAL* (LIST 'PLUS MAXORD 1)))
                  (T (AEVAL* MAXORD))))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL* "maxord=") NIL 'FIRST)
             (ASSGNPRI (AEVAL* MAXORD) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL* "absmaxord=") NIL 'FIRST)
             (ASSGNPRI (AEVAL* ABSMAXORD) NIL 'LAST))
            (AEVAL* 'NIL))))
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
         (SETQ SB (AEVAL* (LIST 'SUBDIF1 XLIST ULIST ABSMAXORD)))
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
         (SETQ CONDI (AEVAL* 0))
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
         (SETQ CF (AEVAL* (LIST 'LIST)))
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
            (SETQ CF (AEVAL* (LIST 'SQCONS H1 CF)))
            (SETQ CONDI
                    (AEVAL*
                     (LIST 'PLUS CONDI
                           (LIST 'TIMES H1 (LIST 'SQPART TREQLIST N)))))
            (AEVAL* 'NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))
         (SETQ CF (AEVAL* (LIST 'SQREVERSE CF)))
         (SETQ DEPLIST
                 (PROG (H3 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H3 0)
                   (COND
                    ((|AMINUSP:|
                      (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ABSMAXORD 1))
                            H3))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (AEVAL*
                                     (LIST 'SQPART DULIST (LIST 'PLUS H3 1)))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H3
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            H3))
                   (COND
                    ((|AMINUSP:|
                      (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ABSMAXORD 1))
                            H3))
                     (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (AEVAL* (LIST 'SQPART DULIST (LIST 'PLUS H3 1)))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((BOOLVALUE* EXPL)
           (SETQ DEPLIST (AEVAL* (LIST 'CONS XLIST DEPLIST)))))
         (SETQ DEPLIST (AEVAL* (LIST 'SQREVERSE DEPLIST)))
         (SETK 'PL (AEVAL* (LIST 'LIST)))
         (PROG (N)
           (SETQ N 1)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NX) N)) (RETURN NIL)))
           (PROGN
            (SETQ H4 (REVAL1 (AEVAL* (LIST 'SQPART XLIST N)) T))
            (SETQ H1 (AEVAL* (LIST 'MKID 'P_ H4)))
            (COND
             ((BOOLVALUE* (REVALX (NULL (GET (MKID 'P_ H4) 'AVALUE))))
              (PROGN
               (AEVAL* (LIST 'NODEPENDLIST (LIST 'LIST H1)))
               (AEVAL* (LIST 'DEPENDLIST H1 DEPLIST))
               (SETQ FL (AEVAL* (LIST 'CONS H1 FL)))
               (SETQ FLIN_ (CONS (REVAL1 H1 T) FLIN_)))))
            (SETK 'PL (AEVAL* (LIST 'SQCONS H1 'PL)))
            (SETQ CONDI
                    (AEVAL*
                     (LIST 'DIFFERENCE CONDI (LIST 'TOTDIF H1 H4 N DULIST)))))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    N))
           (GO LAB))
         (SETQ SB (AEVAL* 0))
         (COND
          ((BOOLVALUE* CONTRACE)
           (PROGN
            (ASSGNPRI (AEVAL* "fl=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* FL) NIL NIL)
            (ASSGNPRI (AEVAL* " cf=") NIL NIL)
            (ASSGNPRI (AEVAL* CF) NIL NIL)
            (ASSGNPRI (AEVAL* " pl=") NIL NIL)
            (ASSGNPRI (AEVAL* 'PL) NIL 'LAST))))
         (COND
          ((BOOLVALUE* CONTRACE) (PROGN (PRIN2 " depl*=") (PRIN2 DEPL*) NIL)))
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
              (SETQ E1 (GETRLIST (AEVAL* CF)))
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
         (SETQ CONDI (AEVAL* (LIST 'LIST CONDI)))
         (COND
          ((AND (NOT (BOOLVALUE* (REVALX (NULL (GET 'CL_CONDI 'AVALUE)))))
                (EVALEQUAL (AEVAL* (LIST 'PART 'CL_CONDI 0)) (AEVAL* 'LIST)))
           (SETQ CONDI (AEVAL* (LIST 'SQAPPEND CONDI 'CL_CONDI)))))
         (SETQ SB
                 (SETQ DULIST
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
                                                                                                                          0)))))))))))))))
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
         (SETQ PLLIST (AEVAL* (LIST 'LIST)))
         (SETQ CLLIST (AEVAL* (LIST 'LIST)))
         (SETQ FOUND (AEVAL* 'NIL))
         (WHILE (EVALNEQ (AEVAL* SOLNS) (AEVAL* (LIST 'LIST)))
                (PROGN
                 (SETQ SOLN (AEVAL* (LIST 'SQFIRST SOLNS)))
                 (SETQ SOLNS (AEVAL* (LIST 'SQREST SOLNS)))
                 (SETQ CONDI (AEVAL* (LIST 'SQFIRST SOLN)))
                 (SETQ CFCOPY (AEVAL* (LIST 'SUB (LIST 'SQSECOND SOLN) CF)))
                 (SETQ H1 (AEVAL* 0))
                 (PROG (H2)
                   (SETQ H2 (GETRLIST (AEVAL* CFCOPY)))
                  LAB
                   (COND ((NULL H2) (RETURN NIL)))
                   ((LAMBDA (H2)
                      (COND ((EVALNEQ (AEVAL* H2) 0) (SETQ H1 (AEVAL* 1)))))
                    (CAR H2))
                   (SETQ H2 (CDR H2))
                   (GO LAB))
                 (COND
                  ((EVALNEQ (AEVAL* H1) 0)
                   (PROGN
                    (SETK 'PL (AEVAL* (LIST 'SUB (LIST 'SQSECOND SOLN) 'PL)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "cfcopy=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* CFCOPY) NIL NIL)
                       (ASSGNPRI (AEVAL* " pl=") NIL NIL)
                       (ASSGNPRI (AEVAL* 'PL) NIL 'LAST))))
                    (SETQ H1 (AEVAL* (LIST 'SQTHIRD SOLN)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "third soln=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* H1) NIL 'LAST))))
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
                           ((FREEOF (REVALX 'PARALIST) (REVALX E1))
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
                            (SETQ H2 (AEVAL* (LIST 'SQFIRST H1)))
                            (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "h2=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* H2) NIL 'LAST))))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "cfcopy=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* CFCOPY) NIL 'LAST))))
                            (SETQ NONTRIV (AEVAL* 'NIL))
                            (SETQ H3
                                    (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ E2 (GETRLIST (AEVAL* CFCOPY)))
                                      (COND
                                       ((NULL E2) (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (E2)
                                                          (PROGN
                                                           (SETQ E3
                                                                   (PROG (E1
                                                                          FORALL-RESULT)
                                                                     (SETQ E1
                                                                             (GETRLIST
                                                                              (AEVAL*
                                                                               H2)))
                                                                     (SETQ FORALL-RESULT
                                                                             0)
                                                                    LAB1
                                                                     (COND
                                                                      ((NULL
                                                                        E1)
                                                                       (RETURN
                                                                        FORALL-RESULT)))
                                                                     (SETQ FORALL-RESULT
                                                                             (AEVAL*
                                                                              (LIST
                                                                               'PLUS
                                                                               ((LAMBDA
                                                                                    (
                                                                                     E1)
                                                                                  (AEVAL*
                                                                                   (LIST
                                                                                    'FDEPTERMS
                                                                                    E2
                                                                                    E1)))
                                                                                (CAR
                                                                                 E1))
                                                                               FORALL-RESULT)))
                                                                     (SETQ E1
                                                                             (CDR
                                                                              E1))
                                                                     (GO
                                                                      LAB1)))
                                                           (COND
                                                            ((EVALNEQ
                                                              (AEVAL* E3) 0)
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
                                                           (PROG (E1
                                                                  FORALL-RESULT)
                                                             (SETQ E1
                                                                     (GETRLIST
                                                                      (AEVAL*
                                                                       H2)))
                                                             (SETQ FORALL-RESULT
                                                                     0)
                                                            LAB1
                                                             (COND
                                                              ((NULL E1)
                                                               (RETURN
                                                                FORALL-RESULT)))
                                                             (SETQ FORALL-RESULT
                                                                     (AEVAL*
                                                                      (LIST
                                                                       'PLUS
                                                                       ((LAMBDA
                                                                            (
                                                                             E1)
                                                                          (AEVAL*
                                                                           (LIST
                                                                            'FDEPTERMS
                                                                            E2
                                                                            E1)))
                                                                        (CAR
                                                                         E1))
                                                                       FORALL-RESULT)))
                                                             (SETQ E1 (CDR E1))
                                                             (GO LAB1)))
                                                   (COND
                                                    ((EVALNEQ (AEVAL* E3) 0)
                                                     (SETQ NONTRIV
                                                             (AEVAL* 'T))))
                                                   (AEVAL* E3)))
                                                (CAR E2))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (COND
                             ((BOOLVALUE* NONTRIV)
                              (PROGN
                               (PROG (E1)
                                 (SETQ E1 (GETRLIST (AEVAL* H2)))
                                LAB
                                 (COND ((NULL E1) (RETURN NIL)))
                                 ((LAMBDA (E1)
                                    (COND
                                     ((EVALNEQ (AEVAL* (LIST 'FARGS E1))
                                               (AEVAL* (LIST 'LIST)))
                                      (PROGN
                                       (SETQ NONCONSTC
                                               (CONS 'LIST
                                                     (CONS (REVAL1 E1 T)
                                                           (CDR NONCONSTC))))
                                       (PROGN
                                        (PRIN2 (REVAL1 E1 T))
                                        (PRIN2 " = ")
                                        NIL)
                                       (FCTPRINT (LIST (REVAL1 E1 T)))
                                       (PROGN (PRIN2 " is not constant.") NIL)
                                       (SETQ EXTRALINE T)
                                       (TERPRI)))))
                                  (CAR E1))
                                 (SETQ E1 (CDR E1))
                                 (GO LAB))
                               (SETQ H4
                                       (AEVAL*
                                        (LIST 'SQREVERSE
                                              (PROG (E2 FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ E2
                                                        (GETRLIST
                                                         (AEVAL* 'PL)))
                                                (COND
                                                 ((NULL E2)
                                                  (RETURN (MAKELIST NIL))))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (E2)
                                                                    (PROG (E1
                                                                           FORALL-RESULT)
                                                                      (SETQ E1
                                                                              (GETRLIST
                                                                               (AEVAL*
                                                                                H2)))
                                                                      (SETQ FORALL-RESULT
                                                                              0)
                                                                     LAB1
                                                                      (COND
                                                                       ((NULL
                                                                         E1)
                                                                        (RETURN
                                                                         FORALL-RESULT)))
                                                                      (SETQ FORALL-RESULT
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'PLUS
                                                                                ((LAMBDA
                                                                                     (
                                                                                      E1)
                                                                                   (AEVAL*
                                                                                    (LIST
                                                                                     'FDEPTERMS
                                                                                     E2
                                                                                     E1)))
                                                                                 (CAR
                                                                                  E1))
                                                                                FORALL-RESULT)))
                                                                      (SETQ E1
                                                                              (CDR
                                                                               E1))
                                                                      (GO
                                                                       LAB1)))
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
                                                            (PROG (E1
                                                                   FORALL-RESULT)
                                                              (SETQ E1
                                                                      (GETRLIST
                                                                       (AEVAL*
                                                                        H2)))
                                                              (SETQ FORALL-RESULT
                                                                      0)
                                                             LAB1
                                                              (COND
                                                               ((NULL E1)
                                                                (RETURN
                                                                 FORALL-RESULT)))
                                                              (SETQ FORALL-RESULT
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'PLUS
                                                                        ((LAMBDA
                                                                             (
                                                                              E1)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             'FDEPTERMS
                                                                             E2
                                                                             E1)))
                                                                         (CAR
                                                                          E1))
                                                                        FORALL-RESULT)))
                                                              (SETQ E1
                                                                      (CDR E1))
                                                              (GO LAB1)))
                                                          (CAR E2))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL)))))
                               (COND
                                ((BOOLVALUE* CONTRACE)
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "h3-1=") NIL 'FIRST)
                                  (ASSGNPRI (AEVAL* H3) NIL NIL)
                                  (ASSGNPRI (AEVAL* "  h4-1=") NIL NIL)
                                  (ASSGNPRI (AEVAL* H4) NIL 'LAST))))
                               (SETQ SB (AEVAL* (LIST 'ABSORBCONST H3 H2)))
                               (COND
                                ((AND (EVALNEQ (AEVAL* SB) (AEVAL* 'NIL))
                                      (EVALNEQ (AEVAL* SB) 0))
                                 (PROGN
                                  (SETQ H3 (AEVAL* (LIST 'SUB SB H3)))
                                  (SETQ H4 (AEVAL* (LIST 'SUB SB H4))))))
                               (COND
                                ((BOOLVALUE* CONTRACE)
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "h3-2=") NIL 'FIRST)
                                  (ASSGNPRI (AEVAL* H3) NIL NIL)
                                  (ASSGNPRI (AEVAL* "  h4-2=") NIL NIL)
                                  (ASSGNPRI (AEVAL* H4) NIL 'LAST))))
                               (COND
                                ((AND (EVALEQUAL (AEVAL* (LIST 'LENGTH H2)) 1)
                                      (EVALEQUAL
                                       (AEVAL*
                                        (LIST 'FARGS (LIST 'SQFIRST H2)))
                                       (AEVAL* (LIST 'LIST))))
                                 (PROGN
                                  (SETQ E1 (AEVAL* (LIST 'SQFIRST H2)))
                                  (SETQ H4
                                          (AEVAL*
                                           (LIST 'SUB (LIST 'EQUAL E1 1) H4)))
                                  (SETQ H3
                                          (AEVAL*
                                           (LIST 'SUB (LIST 'EQUAL E1 1)
                                                 H3))))))
                               (SETQ H5 (AEVAL* UDENS))
                               (COND
                                ((AND (EVALGREATERP (AEVAL* DENSORD) 0)
                                      (OR
                                       (EVALEQUAL (AEVAL* CF0) (AEVAL* 'NIL))
                                       (EVALNEQ (AEVAL* MINDENSORD0) 0)))
                                 (WHILE
                                  (AND
                                   (EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                                   (FREEOF (REVALX H3)
                                           (REVAL1 (REVALX (LIST 'SQFIRST H5))
                                                   T)))
                                  (SETQ H5 (AEVAL* (LIST 'SQREST H5))))))
                               (COND
                                ((EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                                 (PROGN
                                  (SETQ CLLIST
                                          (AEVAL* (LIST 'SQCONS H3 CLLIST)))
                                  (SETQ PLLIST
                                          (AEVAL*
                                           (LIST 'SQCONS H4 PLLIST)))))))))))
                    (COND
                     ((EVALNEQ (AEVAL* CONDI) (AEVAL* (LIST 'LIST)))
                      (PROGN
                       (PROGN (PRIN2 "There are remaining conditions: ") NIL)
                       (ASSGNPRI (AEVAL* CONDI) NIL 'ONLY)
                       (PROGN
                        (PROGN (PRIN2 "for the functions: ") NIL)
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
                    (PROG (E1)
                      (SETQ E1 (GETRLIST (AEVAL* ULIST)))
                     LAB
                      (COND ((NULL E1) (RETURN NIL)))
                      ((LAMBDA (E1)
                         (AEVAL* (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
                       (CAR E1))
                      (SETQ E1 (CDR E1))
                      (GO LAB))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "cllist2=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* CLLIST) NIL NIL)
                       (ASSGNPRI (AEVAL* "  pllist2=") NIL NIL)
                       (ASSGNPRI (AEVAL* PLLIST) NIL 'LAST))))
                    (AEVAL* (ON (LIST 'EVALLHSEQP)))
                    (SETQ SB (AEVAL* (LIST 'SUBDIF1 XLIST ULIST MAXORD)))
                    (SETQ SB
                            (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ E1 (GETRLIST (AEVAL* SB)))
                             STARTOVER
                              (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (E1)
                                         (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
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
                                                                       'RHS E2)
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
                                                              (LIST 'LHS E2))))
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
                                        (CAR E1))))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ E1 (CDR E1))
                              (GO LOOPLABEL)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "sb=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* SB) NIL 'LAST))))
                    (AEVAL* (OFF (LIST 'EVALLHSEQP)))
                    (SETQ CLLIST (AEVAL* (LIST 'SUB SB CLLIST)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "cllist3=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* CLLIST) NIL 'LAST))))
                    (SETQ PLLIST (AEVAL* (LIST 'SUB SB PLLIST)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (ASSGNPRI (AEVAL* "pllist3=") NIL 'FIRST)
                       (ASSGNPRI (AEVAL* PLLIST) NIL 'LAST))))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (PROGN
                        (ASSGNPRI (AEVAL* "cllist3=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* CLLIST) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "pllist3=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* PLLIST) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "eqlist=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* EQLIST) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "xlist=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* XLIST) NIL 'LAST)))))
                    (WHILE (EVALNEQ (AEVAL* PLLIST) (AEVAL* (LIST 'LIST)))
                           (PROGN
                            (SETQ FOUND (AEVAL* 'T))
                            (ASSGNPRI (AEVAL* "Conservation law:") NIL 'ONLY)
                            (SETQ H2 (AEVAL* (LIST 'SQFIRST PLLIST)))
                            (SETQ H3 (AEVAL* (LIST 'SQFIRST CLLIST)))
                            (COND
                             ((EVALNEQ (AEVAL* 'PARALIST)
                                       (AEVAL* (LIST 'LIST)))
                              (PROG (E2)
                                (SETQ E2
                                        (GETRLIST
                                         (AEVAL* (LIST 'SQSECOND SOLN))))
                               LAB
                                (COND ((NULL E2) (RETURN NIL)))
                                ((LAMBDA (E2)
                                   (COND
                                    ((NOT
                                      (FREEOF (REVALX 'PARALIST)
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
                             ((BOOLVALUE* (REVALX 'PARALIST))
                              (SETQ H4
                                      (AEVAL*
                                       (LIST 'SUB (LIST 'SQSECOND SOLN) H4)))))
                            (AEVAL* (LIST 'PRINT_CLAW H4 H3 H2 XLIST))
                            (SETQ H6 (AEVAL* (LIST 'LIST)))
                            (PROG (H5)
                              (SETQ H5 (GETRLIST (AEVAL* NONCONSTC)))
                             LAB
                              (COND ((NULL H5) (RETURN NIL)))
                              ((LAMBDA (H5)
                                 (COND
                                  ((NOT (FREEOF (REVALX H3) (REVALX H5)))
                                   (SETQ H6 (AEVAL* (LIST 'SQCONS H5 H6))))))
                               (CAR H5))
                              (SETQ H5 (CDR H5))
                              (GO LAB))
                            (COND
                             ((AND (EVALNEQ (AEVAL* H6) (AEVAL* (LIST 'LIST)))
                                   (EVALNEQ (AEVAL* H2) (AEVAL* 'NONDIV)))
                              (AEVAL*
                               (LIST 'PARTINTDF H4 H3 H2 XLIST H6 VL SB))))
                            (ASSGNPRI
                             (AEVAL*
                              "======================================================")
                             NIL 'ONLY)
                            (SETQ PLLIST (AEVAL* (LIST 'SQREST PLLIST)))
                            (SETQ CLLIST (AEVAL* (LIST 'SQREST CLLIST)))
                            (AEVAL* 'NIL)))
                    (AEVAL* 'NIL))))
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
         (ASSGNPRI (AEVAL "time to run conlaw3: ") NIL 'FIRST)
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