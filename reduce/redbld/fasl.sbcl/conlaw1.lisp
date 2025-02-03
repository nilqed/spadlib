(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID
 '(PRINT_ LOGOPRINT_ POTINT_ FACINT_ ADJUST_FNC FLIN_ DONE_TRAFO
   INVERSE_TRAFO_LIST_INCOMPLETE)) 
(PUT 'CONLAW1 'NUMBER-OF-ARGS 2) 
(FLAG '(CONLAW1) 'OPFN) 
(PUT 'CONLAW1 'DEFINED-ON-LINE '37) 
(PUT 'CONLAW1 'DEFINED-IN-FILE 'CRACK/CONLAW1.RED) 
(PUT 'CONLAW1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONLAW1 (PROBLEM RUNMODE)
    (PROG (CONTRACE EQLIST ULIST XLIST DEQU CLLIST PLLIST SB DENSORD FLIST
           EQORD MAXORD DULIST REVDULIST VL EXPL DEPLIST E1 E2 E3 N H1 H2 H3 H4
           H5 H6 H7 H8 H9 H10 CONDI SOLN SOLN2 ADJUST_OLD POTOLD ADJUSTOLD
           UDENS GENSEPOLD INEQU0 INEQU LOGOOLD TREQLIST FL FL2 FACOLD U NODEP
           SUBL CPU GC CPUSTART GCSTART FOUND CF0 RTNLIST DEGLIST MAXDF QLIST
           EXTRAORDER NX NDE HIGHDENSORD PRINT_OLD PARALIST PLCOPY DESYLI
           DDESYLI VL1 LHSORD LOWORDERLIMIT EXTRALINE RULES NONCONSTC
           NEW_VAR_FNC)
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
      (SETQ EQLIST (AEVAL (LIST 'MAKLIST (LIST 'SQFIRST PROBLEM))))
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
      (SETK 'MINDENSORD (AEVAL (LIST 'SQPART RUNMODE 1)))
      (SETK 'MAXDENSORD (AEVAL (LIST 'SQPART RUNMODE 2)))
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
          (PRIN2 "This is CONLAW1 - a program for calculating conservation")
          (PRIN2 " laws of DEs")
          NIL)
         (TERPRI)))
       (T (TERPRI)))
      (COND
       ((EVALEQUAL (AEVAL NDE) 1)
        (ASSGNPRI (AEVAL "The DE under investigation is :") NIL 'ONLY))
       (T (ASSGNPRI (AEVAL "The DEs under investigation are :") NIL 'ONLY)))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL EQLIST)))
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
      (SETQ H1 (AEVAL (LIST 'LHSLI EQLIST)))
      (SETQ NODEP (AEVAL (LIST 'SQFIRST H1)))
      (SETQ SUBL (AEVAL (LIST 'SQSECOND H1)))
      (AEVAL (LIST 'CHKSUB EQLIST ULIST))
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
      (SETQ DEGLIST (AEVAL (LIST 'LIST)))
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
      (SETQ EXTRAORDER (AEVAL 0))
      (SETQ LHSORD (AEVAL 0))
      (PROG (E1)
        (SETQ E1 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NDE) E1)) (RETURN NIL)))
        (PROGN
         (SETQ E3 (AEVAL* (LIST 'SQPART EQLIST E1)))
         (SETQ H2 (AEVAL* 0))
         (SETQ H3
                 (AEVAL*
                  (LIST 'TOTDEG (LIST 'SQPART SUBL E1)
                        (LIST 'SQPART ULIST E1))))
         (COND
          ((EVALGREATERP (AEVAL* H3) (AEVAL* LHSORD))
           (SETQ LHSORD (AEVAL* H3))))
         (PROG (E2)
           (SETQ E2 (GETRLIST (AEVAL* ULIST)))
          LAB
           (COND ((NULL E2) (RETURN NIL)))
           ((LAMBDA (E2)
              (PROGN
               (SETQ H1 (AEVAL* (LIST 'TOTDEG E3 E2)))
               (COND
                ((EVALGREATERP (AEVAL* H1) (AEVAL* H2)) (SETQ H2 (AEVAL* H1))))
               (COND
                ((EVALGREATERP (AEVAL* H1) (AEVAL* EQORD))
                 (SETQ EQORD (AEVAL* H1))))
               (AEVAL* 'NIL)))
            (CAR E2))
           (SETQ E2 (CDR E2))
           (GO LAB))
         (SETQ DEGLIST (AEVAL* (LIST 'SQCONS H2 DEGLIST)))
         (COND
          ((EVALGREATERP (AEVAL* H2) (AEVAL* H3))
           (SETQ EXTRAORDER
                   (AEVAL* (LIST 'PLUS EXTRAORDER (LIST 'DIFFERENCE H2 H3))))))
         (AEVAL* 'NIL))
        (SETQ E1
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 E1))
        (GO LAB))
      (SETQ DEGLIST (AEVAL (LIST 'SQREVERSE DEGLIST)))
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
                   ((EVALGREATERP (AEVAL* H2) (AEVAL* 'MINDENSORD))
                    (SETK 'MINDENSORD (AEVAL* H2))))))
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
         (ASSGNPRI (AEVAL EQORD) NIL NIL)
         (ASSGNPRI (AEVAL "   mindensord=") NIL NIL)
         (ASSGNPRI (AEVAL 'MINDENSORD) NIL NIL)
         (ASSGNPRI (AEVAL "   extraorder=") NIL NIL)
         (ASSGNPRI (AEVAL EXTRAORDER) NIL NIL)
         (ASSGNPRI (AEVAL "   lhsord=") NIL NIL)
         (ASSGNPRI (AEVAL LHSORD) NIL 'LAST))))
      (COND
       ((EVALLESSP (AEVAL 'MAXDENSORD) (AEVAL 'MINDENSORD))
        (SETK 'MAXDENSORD (AEVAL 'MINDENSORD))))
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
            (SETQ NODEP (AEVAL (LIST 'SUB E1 NODEP)))
            (SETQ SUBL (AEVAL (LIST 'SUB E1 SUBL)))
            (AEVAL 'NIL)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       ((BOOLVALUE* CONTRACE)
        (PROGN
         (ASSGNPRI (AEVAL "treqlist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL TREQLIST) NIL NIL)
         (ASSGNPRI (AEVAL " nodep=") NIL NIL)
         (ASSGNPRI (AEVAL NODEP) NIL NIL)
         (ASSGNPRI (AEVAL " subl=") NIL NIL)
         (ASSGNPRI (AEVAL SUBL) NIL 'LAST))))
      (AEVAL (LIST 'CHKFLIST FLIST NODEP))
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
        (SETQ DENSORD (AEVAL* 'MINDENSORD))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'MAXDENSORD) DENSORD))
          (RETURN NIL)))
        (PROGN
         (SETQ CPU (AEVAL* (TIME)))
         (SETQ GC (AEVAL* (GCTIME)))
         (COND
          ((BOOLVALUE* CF0)
           (PROGN
            (PROGN
             (PRIN2 "A special ansatz of order ")
             (PRIN2 DENSORD)
             (PRIN2 " for the")
             (PRIN2 " conserved current is investigated.")
             NIL)
            (TERPRI)))
          (T
           (PROGN
            (PROGN
             (PRIN2 "Currently conservation laws with a conserved")
             (PRIN2 " density")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "of order ")
             (PRIN2 DENSORD)
             (PRIN2 " are determined")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "======================================================")
             NIL)
            NIL)))
         (COND
          ((EVALLESSP (AEVAL* (LIST 'PLUS DENSORD 1)) (AEVAL* LHSORD))
           (PROGN
            (PROGN
             (PRIN2
              "The order of the ansatz is too low for substitutions of equations")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "to be done --> no investigation") NIL)
            (TERPRI)
            (TERPRI)))
          (T
           (PROGN
            (COND
             ((EVALEQUAL (AEVAL* (LIST 'PLUS DENSORD 1)) (AEVAL* LHSORD))
              (SETQ LOWORDERLIMIT (AEVAL* 'NIL)))
             (T (SETQ LOWORDERLIMIT (AEVAL* 'T))))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "loworderlimit=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* LOWORDERLIMIT) NIL 'LAST))))
            (SETQ HIGHDENSORD (AEVAL* (LIST 'PLUS DENSORD EXTRAORDER)))
            (SETQ MAXORD (AEVAL* (LIST 'PLUS HIGHDENSORD 1 EXTRAORDER)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "densord=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* DENSORD) NIL NIL)
               (ASSGNPRI (AEVAL* "  highdensord=") NIL NIL)
               (ASSGNPRI (AEVAL* HIGHDENSORD) NIL NIL)
               (ASSGNPRI (AEVAL* "  maxord=") NIL NIL)
               (ASSGNPRI (AEVAL* MAXORD) NIL 'LAST))))
            (COND
             ((EVALEQUAL (AEVAL* (LIST 'LIST))
                         (AEVAL* (LIST 'FARGS (LIST 'SQFIRST ULIST))))
              (PROG (E1)
                (SETQ E1 (GETRLIST (AEVAL* ULIST)))
               LAB
                (COND ((NULL E1) (RETURN NIL)))
                ((LAMBDA (E1)
                   (AEVAL* (LIST 'DEPENDLIST E1 (LIST 'LIST XLIST))))
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
                                                                      (LIST
                                                                       'RHS
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
                                                                       'RHS
                                                                       E2)))
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
            (SETQ REVDULIST (AEVAL* (LIST 'SQREVERSE DULIST)))
            (SETQ UDENS (AEVAL* (LIST 'SQPART DULIST (LIST 'PLUS DENSORD 1))))
            (SETQ VL
                    (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                      (SETQ E1 (GETRLIST (AEVAL* DULIST)))
                     STARTOVER
                      (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                      (SETQ FORALL-RESULT ((LAMBDA (E1) (AEVAL* E1)) (CAR E1)))
                      (SETQ FORALL-ENDPTR
                              (LASTPAIR (CONS 'LIST FORALL-RESULT)))
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
            (SETQ VL1
                    (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                      (SETQ E1 1)
                     STARTOVER
                      (COND
                       ((|AMINUSP:|
                         (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS HIGHDENSORD 2))
                               E1))
                        (RETURN (MAKELIST NIL))))
                      (SETQ FORALL-RESULT (AEVAL* (LIST 'SQPART DULIST E1)))
                      (SETQ FORALL-ENDPTR
                              (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                      (SETQ E1
                              ((LAMBDA (FORALL-RESULT)
                                 (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                               E1))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND
                       ((|AMINUSP:|
                         (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS HIGHDENSORD 2))
                               E1))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (GETRLIST (AEVAL* (LIST 'SQPART DULIST E1))))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ E1
                              ((LAMBDA (FORALL-RESULT)
                                 (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                               E1))
                      (GO LOOPLABEL)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "vl1=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* VL1) NIL 'LAST))))
            (COND ((NOT (BOOLVALUE* FLIST)) (SETQ FL (AEVAL* (LIST 'LIST))))
                  (T (SETQ FL (AEVAL* FLIST))))
            (SETQ DEPLIST
                    (AEVAL*
                     (LIST 'CONS (CONS 'LIST (SETDIFF (CDR ULIST) (CDR NODEP)))
                           (PROG (N FORALL-RESULT FORALL-ENDPTR)
                             (SETQ N 1)
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* HIGHDENSORD) N))
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
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* HIGHDENSORD) N))
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
            (SETK 'DEPLIST1
                  (PROG (H3 FORALL-RESULT FORALL-ENDPTR)
                    (SETQ H3 1)
                    (COND
                     ((|AMINUSP:|
                       (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS DENSORD 1)) H3))
                      (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS (AEVAL* (LIST 'SQPART DEPLIST H3))
                                          NIL)))
                   LOOPLABEL
                    (SETQ H3
                            ((LAMBDA (FORALL-RESULT)
                               (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                             H3))
                    (COND
                     ((|AMINUSP:|
                       (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS DENSORD 1)) H3))
                      (RETURN (CONS 'LIST FORALL-RESULT))))
                    (RPLACD FORALL-ENDPTR
                            (CONS (AEVAL* (LIST 'SQPART DEPLIST H3)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
            (COND
             ((BOOLVALUE* EXPL)
              (PROGN
               (SETQ DEPLIST (AEVAL* (LIST 'CONS XLIST DEPLIST)))
               (SETK 'DEPLIST1 (AEVAL* (LIST 'CONS XLIST 'DEPLIST1))))))
            (SETQ DEPLIST (AEVAL* (LIST 'SQREVERSE DEPLIST)))
            (SETK 'DEPLIST1 (AEVAL* (LIST 'SQREVERSE 'DEPLIST1)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN (PRIN2 "1. depl*=") (PRIN2 DEPL*) NIL)))
            (SETK 'PL (AEVAL* (LIST 'LIST)))
            (SETQ CONDI (AEVAL* 0))
            (PROG (N)
              (SETQ N 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NX) N)) (RETURN NIL)))
              (PROGN
               (SETQ H4 (REVAL1 (AEVAL* (LIST 'SQPART XLIST N)) T))
               (SETQ H1 (AEVAL* (LIST 'MKID 'P_ H4)))
               (COND
                ((BOOLVALUE* (REVALX (NULL (GET (MKID 'P_ H4) 'AVALUE))))
                 (PROGN
                  (AEVAL* (LIST 'NODEPENDLIST (LIST 'LIST H1)))
                  (COND
                   ((EVALEQUAL (AEVAL* N) 1)
                    (AEVAL* (LIST 'DEPENDLIST H1 'DEPLIST1)))
                   (T (AEVAL* (LIST 'DEPENDLIST H1 DEPLIST))))
                  (SETQ FL (AEVAL* (LIST 'CONS H1 FL)))
                  (SETQ FLIN_ (CONS (REVAL1 H1 T) FLIN_)))))
               (SETK 'PL (AEVAL* (LIST 'SQCONS H1 'PL)))
               (SETQ CONDI
                       (AEVAL*
                        (LIST 'PLUS CONDI (LIST 'TOTDIF H1 H4 N DULIST)))))
              (SETQ N
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       N))
              (GO LAB))
            (SETK 'PL (AEVAL* (LIST 'SQREVERSE 'PL)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "fl=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* FL) NIL NIL)
               (ASSGNPRI (AEVAL* " cf=") NIL NIL)
               (ASSGNPRI (AEVAL* 'CF) NIL NIL)
               (ASSGNPRI (AEVAL* " pl=") NIL NIL)
               (ASSGNPRI (AEVAL* 'PL) NIL 'LAST))))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN (PRIN2 "2. depl*=") (PRIN2 DEPL*) NIL)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "condi=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* CONDI) NIL 'LAST))))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "udens=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* UDENS) NIL 'LAST))))
            (SETK 'SBRESERVE (AEVAL* (LIST 'LIST)))
            (SETQ DESYLI (AEVAL* (LIST 'LIST)))
            (SETQ DDESYLI (AEVAL* (LIST 'LIST)))
            (SETQ H1 (AEVAL* TREQLIST))
            (SETQ H2 (AEVAL* SUBL))
            (SETQ H5 (AEVAL* 0))
            (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                   (PROGN
                    (SETQ H5 (AEVAL* (LIST 'PLUS H5 1)))
                    (SETQ H4 (AEVAL* (INTERN (GENSYM))))
                    (AEVAL* (LIST 'DEPENDLIST H4 (LIST 'LIST XLIST)))
                    (SETQ DESYLI (AEVAL* (LIST 'SQCONS H4 DESYLI)))
                    (SETQ DDESYLI
                            (AEVAL*
                             (LIST 'SQCONS (LIST 'LIST H4 (LIST 'LIST) H5)
                                   DDESYLI)))
                    (SETQ H3 (AEVAL* (LIST 'SQFIRST H2)))
                    (COND
                     ((EVALNEQ (AEVAL* H3) 0)
                      (SETK 'SBRESERVE
                            (AEVAL*
                             (LIST 'SQCONS
                                   (LIST 'EQUAL H3
                                         (LIST 'PLUS
                                               (LIST 'DIFFERENCE H3
                                                     (LIST 'QUOTIENT
                                                           (LIST 'SQFIRST H1)
                                                           (LIST 'COEFFN
                                                                 (LIST 'SQFIRST
                                                                       H1)
                                                                 H3 1)))
                                               H4))
                                   'SBRESERVE)))))
                    (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                    (SETQ H2 (AEVAL* (LIST 'SQREST H2)))
                    (AEVAL* 'NIL)))
            (SETK 'SBRESERVE (AEVAL* (LIST 'SQREVERSE 'SBRESERVE)))
            (SETQ DESYLI (AEVAL* (LIST 'SQREVERSE DESYLI)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "rev desyli=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* DESYLI) NIL 'LAST))))
            (SETQ H1 (AEVAL* 'SBRESERVE))
            (SETQ H2 NIL)
            (PROG (E1)
              (SETQ E1 (GETRLIST (AEVAL* H1)))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROGN
                  (SETQ E3 (COMBIDIF (AEVAL* (LIST 'LHS E1))))
                  (SETQ H2
                          (CONS
                           (LIST (CAR E3) (CDR E3) (AEVAL* (LIST 'RHS E1)))
                           H2))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (PROG (E1)
              (SETQ E1 (GETRLIST (AEVAL* VL1)))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROGN
                  (SETQ H1 H2)
                  (SETQ H5 0)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (NEQ H1 NIL)) (RETURN NIL)))
                    (PROGN
                     (SETQ H5 (PLUS H5 1))
                     (SETQ H3
                             (COMPAREDIF2 (CAAR H1) (CADAR H1)
                              (REVAL1 (AEVAL* E1) T)))
                     (COND
                      ((AND (NEQ H3 NIL) (NEQ H3 0))
                       (PROGN
                        (SETQ H3 (AEVAL* (CONS 'LIST H3)))
                        (SETQ DEQU (AEVAL* (CADDAR H1)))
                        (PROG (N)
                          (SETQ N (GETRLIST (AEVAL* H3)))
                         LAB
                          (COND ((NULL N) (RETURN NIL)))
                          ((LAMBDA (N)
                             (SETQ DEQU
                                     (AEVAL*
                                      (LIST 'TOTDIF DEQU (LIST 'SQPART XLIST N)
                                            N DULIST))))
                           (CAR N))
                          (SETQ N (CDR N))
                          (GO LAB))
                        (SETQ H6 (AEVAL* (LIST 'SQPART DESYLI H5)))
                        (PROG (N)
                          (SETQ N (GETRLIST (AEVAL* H3)))
                         LAB
                          (COND ((NULL N) (RETURN NIL)))
                          ((LAMBDA (N)
                             (SETQ H6
                                     (AEVAL*
                                      (LIST 'DF H6 (LIST 'SQPART XLIST N)))))
                           (CAR N))
                          (SETQ N (CDR N))
                          (GO LAB))
                        (SETQ DDESYLI
                                (AEVAL*
                                 (LIST 'SQCONS (LIST 'LIST H6 H3 H5) DDESYLI)))
                        (SETK 'SBRESERVE
                              (AEVAL*
                               (LIST 'SQCONS (LIST 'EQUAL E1 DEQU)
                                     'SBRESERVE)))
                        (SETQ H1 NIL)))
                      (T (SETQ H1 (CDR H1))))
                     NIL)
                    (GO WHILELABEL))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "sbreserve=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* 'SBRESERVE) NIL 'LAST))))
            (SETQ SB
                    (AEVAL*
                     (LIST 'SUB
                           (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ E1 (GETRLIST (AEVAL* DESYLI)))
                             (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (E1)
                                                 (AEVAL* (LIST 'EQUAL E1 0)))
                                               (CAR E1))
                                              NIL)))
                            LOOPLABEL
                             (SETQ E1 (CDR E1))
                             (COND
                              ((NULL E1) (RETURN (CONS 'LIST FORALL-RESULT))))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (E1)
                                         (AEVAL* (LIST 'EQUAL E1 0)))
                                       (CAR E1))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))
                           'SBRESERVE)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "sb=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* SB) NIL 'LAST))))
            (SETQ RULES (AEVAL* (LIST 'LIST)))
            (PROG (E1)
              (SETQ E1 (GETRLIST (AEVAL* SB)))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROGN
                  (SETQ H5 (AEVAL* (LIST 'LHS E1)))
                  (SETQ H6 (AEVAL* (LIST 'RHS E1)))
                  (SETQ RULES
                          (AEVAL*
                           (LIST 'SQCONS (LIST 'REPLACEBY H5 H6) RULES)))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "rules=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* RULES) NIL 'LAST))))
            (AEVAL* (LET (LIST RULES)))
            (SETQ CONDI (AEVAL* CONDI))
            (AEVAL* (CLEARRULES (LIST RULES)))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "condi=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* CONDI) NIL 'LAST))))
            (SETQ VL (AEVAL* (LIST 'SQREVERSE (LIST 'SQAPPEND XLIST VL))))
            (SETQ INEQU (AEVAL* INEQU0))
            (COND
             ((BOOLVALUE* LOWORDERLIMIT)
              (PROGN
               (SETQ DEQU (AEVAL* (LIST 'LIST)))
               (SETQ E1 (AEVAL* (LIST 'SQFIRST 'PL)))
               (SETQ H1 (AEVAL* UDENS))
               (WHILE (EVALNEQ (AEVAL* H1) (AEVAL* (LIST 'LIST)))
                      (PROGN
                       (SETQ DEQU
                               (AEVAL*
                                (LIST 'SQCONS (LIST 'DF E1 (LIST 'FIRST H1))
                                      DEQU)))
                       (SETQ H1 (AEVAL* (LIST 'SQREST H1)))))
               (SETQ INEQU (AEVAL* (LIST 'SQCONS DEQU INEQU))))))
            (COND
             ((BOOLVALUE* CONTRACE)
              (PROGN
               (ASSGNPRI (AEVAL* "inequ=") NIL 'FIRST)
               (ASSGNPRI (AEVAL* INEQU) NIL 'LAST))))
            (SETQ CONDI (AEVAL* (LIST 'LIST CONDI)))
            (COND
             ((AND (NOT (BOOLVALUE* (REVALX (NULL (GET 'CL_CONDI 'AVALUE)))))
                   (EVALEQUAL (AEVAL* (LIST 'PART 'CL_CONDI 0))
                              (AEVAL* 'LIST)))
              (SETQ CONDI (AEVAL* (LIST 'SQAPPEND CONDI 'CL_CONDI)))))
            (SETQ SB
                    (SETQ REVDULIST
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
                                                                                                             0)))))))))))))
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
            (SETK 'SOLNS (AEVAL* (LIST 'CRACK CONDI INEQU FL VL)))
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
                   (PRIN2
                    "      is not complete as at least one transformation")
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
            (WHILE (EVALNEQ (AEVAL* 'SOLNS) (AEVAL* (LIST 'LIST)))
                   (PROGN
                    (SETQ SOLN (AEVAL* (LIST 'SQFIRST 'SOLNS)))
                    (SETK 'SOLNS (AEVAL* (LIST 'SQREST 'SOLNS)))
                    (SETQ CONDI (AEVAL* (LIST 'SQFIRST SOLN)))
                    (SETQ PLCOPY
                            (AEVAL* (LIST 'SUB (LIST 'SQSECOND SOLN) 'PL)))
                    (SETQ H1 (AEVAL* (LIST 'SQTHIRD SOLN)))
                    (COND
                     ((BOOLVALUE* CONTRACE)
                      (PROGN
                       (PROGN
                        (ASSGNPRI (AEVAL* "plcopy=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* PLCOPY) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "soln=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL* SOLN) NIL 'LAST))
                       (PROGN
                        (ASSGNPRI (AEVAL* "third soln=") NIL 'FIRST)
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
                            (SETQ H2 (AEVAL* (LIST 'SQFIRST H1)))
                            (SETQ H1 (AEVAL* (LIST 'SQREST H1)))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "h2=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* H2) NIL 'LAST))))
                            (SETQ H4
                                    (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ E2 (GETRLIST (AEVAL* PLCOPY)))
                                      (COND
                                       ((NULL E2) (RETURN (MAKELIST NIL))))
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
                                                             ((NULL E1)
                                                              (RETURN
                                                               FORALL-RESULT)))
                                                            (SETQ FORALL-RESULT
                                                                    (AEVAL*
                                                                     (LIST
                                                                      'PLUS
                                                                      ((LAMBDA
                                                                           (E1)
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
                                                  (PROG (E1 FORALL-RESULT)
                                                    (SETQ E1
                                                            (GETRLIST
                                                             (AEVAL* H2)))
                                                    (SETQ FORALL-RESULT 0)
                                                   LAB1
                                                    (COND
                                                     ((NULL E1)
                                                      (RETURN FORALL-RESULT)))
                                                    (SETQ FORALL-RESULT
                                                            (AEVAL*
                                                             (LIST 'PLUS
                                                                   ((LAMBDA
                                                                        (E1)
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'FDEPTERMS
                                                                        E2
                                                                        E1)))
                                                                    (CAR E1))
                                                                   FORALL-RESULT)))
                                                    (SETQ E1 (CDR E1))
                                                    (GO LAB1)))
                                                (CAR E2))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "h4-1=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* H4) NIL 'LAST))))
                            (SETQ SB (AEVAL* (LIST 'ABSORBCONST H4 H2)))
                            (COND
                             ((AND (EVALNEQ (AEVAL* SB) (AEVAL* 'NIL))
                                   (EVALNEQ (AEVAL* SB) 0))
                              (SETQ H4 (AEVAL* (LIST 'SUB SB H4)))))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "h4-2=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* H4) NIL 'LAST))))
                            (COND
                             ((AND (EVALEQUAL (AEVAL* (LIST 'LENGTH H2)) 1)
                                   (EVALEQUAL
                                    (AEVAL* (LIST 'FARGS (LIST 'SQFIRST H2)))
                                    (AEVAL* (LIST 'LIST))))
                              (PROGN
                               (SETQ E1 (AEVAL* (LIST 'SQFIRST H2)))
                               (SETQ H4
                                       (AEVAL*
                                        (LIST 'SUB (LIST 'EQUAL E1 1) H4)))
                               (AEVAL* 'NIL))))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "udens=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* UDENS) NIL 'LAST))))
                            (SETQ H5 (AEVAL* UDENS))
                            (COND
                             ((AND (EVALEQUAL (AEVAL* CF0) (AEVAL* 'NIL))
                                   (BOOLVALUE* LOWORDERLIMIT))
                              (WHILE
                               (AND (EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                                    (BOOLVALUE*
                                     (PROGN
                                      (SETQ H6 (REVALX H4))
                                      (WHILE
                                       (AND
                                        (EVALNEQ (AEVAL* H6)
                                                 (AEVAL* (LIST 'LIST)))
                                        (FREEOF (REVALX (LIST 'SQFIRST H6))
                                                (REVAL1
                                                 (REVALX (LIST 'SQFIRST H5))
                                                 T)))
                                       (SETQ H6 (AEVAL* (LIST 'SQREST H6))))
                                      (COND
                                       ((EVALEQUAL (REVALX H6)
                                                   (REVALX (LIST 'LIST)))
                                        (REVALX 'T))
                                       (T (REVALX 'NIL))))))
                               (SETQ H5 (AEVAL* (LIST 'SQREST H5))))))
                            (COND
                             ((BOOLVALUE* CONTRACE)
                              (PROGN
                               (ASSGNPRI (AEVAL* "h5=") NIL 'FIRST)
                               (ASSGNPRI (AEVAL* H5) NIL 'LAST))))
                            (COND
                             ((EVALNEQ (AEVAL* H5) (AEVAL* (LIST 'LIST)))
                              (PROGN
                               (SETQ H3
                                       (PROG (E1 FORALL-RESULT)
                                         (SETQ E1 1)
                                         (SETQ FORALL-RESULT 0)
                                        LAB1
                                         (COND
                                          ((|AMINUSP:|
                                            (LIST 'DIFFERENCE (AEVAL* NX) E1))
                                           (RETURN FORALL-RESULT)))
                                         (SETQ FORALL-RESULT
                                                 (AEVAL*
                                                  (LIST 'PLUS
                                                        (AEVAL*
                                                         (LIST 'TOTDIF
                                                               (LIST 'SQPART H4
                                                                     E1)
                                                               (LIST 'SQPART
                                                                     XLIST E1)
                                                               E1 DULIST))
                                                        FORALL-RESULT)))
                                         (SETQ E1
                                                 ((LAMBDA (FORALL-RESULT)
                                                    (AEVAL*
                                                     (LIST 'PLUS FORALL-RESULT
                                                           1)))
                                                  E1))
                                         (GO LAB1)))
                               (COND
                                ((BOOLVALUE* CONTRACE)
                                 (PROGN
                                  (ASSGNPRI (AEVAL* "h3-1=") NIL 'FIRST)
                                  (ASSGNPRI (AEVAL* H3) NIL 'LAST))))
                               (COND
                                ((EVALNEQ (AEVAL* H3) 0)
                                 (PROGN
                                  (SETQ H3 (AEVAL* (LIST 'SUB 'SBRESERVE H3)))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "h3-2=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* H3) NIL 'LAST))))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "ddesyli=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* DDESYLI) NIL 'LAST))))
                                  (SETK 'DIVLIST (AEVAL* (LIST 'LIST)))
                                  (PROG (E1)
                                    (SETQ E1 (GETRLIST (AEVAL* DDESYLI)))
                                   LAB
                                    (COND ((NULL E1) (RETURN NIL)))
                                    ((LAMBDA (E1)
                                       (PROGN
                                        (SETQ H6
                                                (AEVAL*
                                                 (LIST 'COEFFN H3
                                                       (LIST 'SQFIRST E1) 1)))
                                        (COND
                                         ((EVALNEQ (AEVAL* H6) 0)
                                          (PROGN
                                           (SETQ H3
                                                   (AEVAL*
                                                    (LIST 'DIFFERENCE H3
                                                          (LIST 'TIMES H6
                                                                (LIST 'SQFIRST
                                                                      E1)))))
                                           (SETK 'DIVLIST
                                                 (AEVAL*
                                                  (LIST 'SQCONS
                                                        (LIST 'LIST H6
                                                              (LIST 'SQSECOND
                                                                    E1)
                                                              (LIST 'SQTHIRD
                                                                    E1))
                                                        'DIVLIST))))))))
                                     (CAR E1))
                                    (SETQ E1 (CDR E1))
                                    (GO LAB))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "h3-3=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* H3) NIL 'LAST))))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "divlist=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* 'DIVLIST) NIL 'LAST))))
                                  (SETQ QLIST
                                          (PROG (E1 FORALL-RESULT
                                                 FORALL-ENDPTR)
                                            (SETQ E1 1)
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE (AEVAL* NDE)
                                                     E1))
                                              (RETURN (MAKELIST NIL))))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS 0 NIL)))
                                           LOOPLABEL
                                            (SETQ E1
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     E1))
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE (AEVAL* NDE)
                                                     E1))
                                              (RETURN
                                               (CONS 'LIST FORALL-RESULT))))
                                            (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))
                                  (PROG (E1)
                                    (SETQ E1 (GETRLIST (AEVAL* 'DIVLIST)))
                                   LAB
                                    (COND ((NULL E1) (RETURN NIL)))
                                    ((LAMBDA (E1)
                                       (PROGN
                                        (SETQ H9 (AEVAL* (LIST 'SQFIRST E1)))
                                        (SETQ E2 (AEVAL* (LIST 'SQSECOND E1)))
                                        (SETQ H10 (AEVAL* (LIST 'SQTHIRD E1)))
                                        (COND
                                         ((EVALNEQ (AEVAL* H9) 0)
                                          (COND
                                           ((EVALEQUAL (AEVAL* E2)
                                                       (AEVAL* (LIST 'LIST)))
                                            (SETQ QLIST
                                                    (AEVAL*
                                                     (LIST 'SETPART* QLIST H10
                                                           (AEVAL*
                                                            (LIST 'PLUS
                                                                  (LIST 'PART
                                                                        QLIST
                                                                        H10)
                                                                  H9))))))
                                           (T
                                            (PROGN
                                             (SETQ H6 (AEVAL* (MINUS 1)))
                                             (COND
                                              ((EVALGREATERP
                                                (AEVAL* (LIST 'LENGTH E2)) 1)
                                               (PROGN
                                                (SETQ H7
                                                        (AEVAL*
                                                         (LIST 'SQPART TREQLIST
                                                               H10)))
                                                (COND
                                                 ((EVALNEQ (AEVAL* PARALIST)
                                                           (AEVAL*
                                                            (LIST 'LIST)))
                                                  (SETQ H7
                                                          (AEVAL*
                                                           (LIST 'SUB
                                                                 (LIST
                                                                  'SQSECOND
                                                                  SOLN)
                                                                 H7)))))
                                                (SETQ H8
                                                        (PROG (E2 FORALL-RESULT
                                                               FORALL-ENDPTR)
                                                          (SETQ E2
                                                                  (GETRLIST
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'SQREST
                                                                     (LIST
                                                                      'SQSECOND
                                                                      E1)))))
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
                                                                              (SETQ H7
                                                                                      (AEVAL*
                                                                                       (LIST
                                                                                        'TOTDIF
                                                                                        H7
                                                                                        (LIST
                                                                                         'SQPART
                                                                                         XLIST
                                                                                         E2)
                                                                                        E2
                                                                                        DULIST))))
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
                                                                   ((LAMBDA
                                                                        (E2)
                                                                      (SETQ H7
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'TOTDIF
                                                                                H7
                                                                                (LIST
                                                                                 'SQPART
                                                                                 XLIST
                                                                                 E2)
                                                                                E2
                                                                                DULIST))))
                                                                    (CAR E2))
                                                                   NIL))
                                                          (SETQ FORALL-ENDPTR
                                                                  (CDR
                                                                   FORALL-ENDPTR))
                                                          (GO LOOPLABEL)))
                                                (AEVAL* 'NIL)))
                                              (T
                                               (SETQ H8
                                                       (AEVAL* (LIST 'LIST)))))
                                             (SETQ H8
                                                     (AEVAL*
                                                      (LIST 'SQAPPEND H8
                                                            (LIST 'LIST
                                                                  (LIST 'SQPART
                                                                        TREQLIST
                                                                        H10)))))
                                             (WHILE
                                              (EVALNEQ (AEVAL* E2)
                                                       (AEVAL* (LIST 'LIST)))
                                              (PROGN
                                               (SETQ E3
                                                       (AEVAL*
                                                        (LIST 'SQFIRST E2)))
                                               (SETQ E2
                                                       (AEVAL*
                                                        (LIST 'SQREST E2)))
                                               (SETQ H4
                                                       (AEVAL*
                                                        (LIST 'SETPART* H4 E3
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     (LIST
                                                                      'PART H4
                                                                      E3)
                                                                     (LIST
                                                                      'TIMES H6
                                                                      H9
                                                                      (LIST
                                                                       'SQFIRST
                                                                       H8)))))))
                                               (SETQ H9
                                                       (AEVAL*
                                                        (LIST 'TOTDIF H9
                                                              (LIST 'SQPART
                                                                    XLIST E3)
                                                              E3 DULIST)))
                                               (COND
                                                ((EVALNEQ (AEVAL* E2)
                                                          (AEVAL*
                                                           (LIST 'LIST)))
                                                 (PROGN
                                                  (SETQ H8
                                                          (AEVAL*
                                                           (LIST 'SQREST H8)))
                                                  (SETQ H6
                                                          (AEVAL*
                                                           (LIST 'MINUS H6)))
                                                  (AEVAL* 'NIL)))
                                                (T
                                                 (SETQ QLIST
                                                         (AEVAL*
                                                          (LIST 'SETPART* QLIST
                                                                H10
                                                                (AEVAL*
                                                                 (LIST 'PLUS
                                                                       (LIST
                                                                        'PART
                                                                        QLIST
                                                                        H10)
                                                                       (LIST
                                                                        'TIMES
                                                                        H6
                                                                        H9))))))))))
                                             (AEVAL* 'NIL))))))
                                        (AEVAL* 'NIL)))
                                     (CAR E1))
                                    (SETQ E1 (CDR E1))
                                    (GO LAB))
                                  (SETQ E2 (AEVAL* 'T))
                                  (PROG (E1)
                                    (SETQ E1 (GETRLIST (AEVAL* QLIST)))
                                   LAB
                                    (COND ((NULL E1) (RETURN NIL)))
                                    ((LAMBDA (E1)
                                       (COND
                                        ((EVALNEQ (AEVAL* E1) 0)
                                         (SETQ E2 (AEVAL* 'NIL)))))
                                     (CAR E1))
                                    (SETQ E1 (CDR E1))
                                    (GO LAB))
                                  (COND
                                   ((BOOLVALUE* E2) (SETQ H4 (AEVAL* 'NIL))))
                                  (COND
                                   ((BOOLVALUE* H4)
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
                                                                 (CDR
                                                                  NONCONSTC))))
                                             (PROGN
                                              (PRIN2 (REVAL1 E1 T))
                                              (PRIN2 " = ")
                                              NIL)
                                             (FCTPRINT (LIST (REVAL1 E1 T)))
                                             (PROGN
                                              (PRIN2 " is not constant.")
                                              NIL)
                                             (SETQ EXTRALINE T)
                                             (TERPRI)))))
                                        (CAR E1))
                                       (SETQ E1 (CDR E1))
                                       (GO LAB))
                                     (SETQ CLLIST
                                             (AEVAL*
                                              (LIST 'SQCONS QLIST CLLIST)))
                                     (SETQ PLLIST
                                             (AEVAL* (LIST 'SQCONS H4 PLLIST)))
                                     (AEVAL* 'NIL))))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "cllist=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* CLLIST) NIL 'LAST))))
                                  (COND
                                   ((BOOLVALUE* CONTRACE)
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "pllist=") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* PLLIST) NIL 'LAST))))
                                  (AEVAL* 'NIL)))))))))
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
                     ((EVALEQUAL (AEVAL* NX) 2)
                      (SETQ PLLIST
                              (AEVAL*
                               (LIST 'SIMPPL PLLIST ULIST (LIST 'SQFIRST XLIST)
                                     (LIST 'SQSECOND XLIST))))))
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
                            (SETQ H2 (AEVAL* (LIST 'SQFIRST PLLIST)))
                            (SETQ H3 (AEVAL* (LIST 'SQFIRST CLLIST)))
                            (SETQ E1 (AEVAL* ULIST))
                            (COND
                             ((AND (EVALEQUAL (AEVAL* CF0) (AEVAL* 'NIL))
                                   (BOOLVALUE* LOWORDERLIMIT))
                              (WHILE
                               (AND (EVALNEQ (AEVAL* E1) (AEVAL* (LIST 'LIST)))
                                    (BOOLVALUE*
                                     (PROGN
                                      (SETQ H4 (REVALX H2))
                                      (WHILE
                                       (AND
                                        (EVALNEQ (AEVAL* H4)
                                                 (AEVAL* (LIST 'LIST)))
                                        (EVALLESSP
                                         (AEVAL*
                                          (LIST 'TOTDEG (LIST 'SQFIRST H4)
                                                (LIST 'SQFIRST E1)))
                                         (AEVAL* DENSORD)))
                                       (SETQ H4 (AEVAL* (LIST 'SQREST H4))))
                                      (COND
                                       ((EVALEQUAL (REVALX H4)
                                                   (REVALX (LIST 'LIST)))
                                        (REVALX 'T))
                                       (T (REVALX 'NIL))))))
                               (SETQ E1 (AEVAL* (LIST 'SQREST E1))))))
                            (COND
                             ((EVALNEQ (AEVAL* E1) (AEVAL* (LIST 'LIST)))
                              (PROGN
                               (SETQ FOUND (AEVAL* 'T))
                               (ASSGNPRI (AEVAL* "Conservation law:") NIL
                                         'ONLY)
                               (COND
                                ((EVALNEQ (AEVAL* PARALIST)
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
                                         (FREEOF (REVALX PARALIST)
                                                 (REVALX (LIST 'LHS E2))))
                                        (PROGN
                                         (ASSGNPRI (AEVAL* E2) NIL 'ONLY)
                                         (AEVAL* (TERPRI))
                                         (SETQ H2 (AEVAL* (LIST 'SUB E2 H2)))
                                         (SETQ H3
                                                 (AEVAL*
                                                  (LIST 'SUB E2 H3)))))))
                                    (CAR E2))
                                   (SETQ E2 (CDR E2))
                                   (GO LAB))))
                               (SETQ RTNLIST
                                       (AEVAL*
                                        (LIST 'SQCONS (LIST 'LIST H3 H2)
                                              RTNLIST)))
                               (COND
                                ((EVALEQUAL 0
                                            (PROG (E1 FORALL-RESULT)
                                              (SETQ E1 (GETRLIST (AEVAL* H3)))
                                              (SETQ FORALL-RESULT 0)
                                             LAB1
                                              (COND
                                               ((NULL E1)
                                                (RETURN FORALL-RESULT)))
                                              (SETQ FORALL-RESULT
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             ((LAMBDA (E1)
                                                                (AEVAL* E1))
                                                              (CAR E1))
                                                             FORALL-RESULT)))
                                              (SETQ E1 (CDR E1))
                                              (GO LAB1)))
                                 (PROGN
                                  (ASSGNPRI (AEVAL* " = 0 ") NIL 'ONLY)
                                  (SETQ H4 (AEVAL* XLIST))
                                  (WHILE
                                   (EVALNEQ (AEVAL* H2) (AEVAL* (LIST 'LIST)))
                                   (PROGN
                                    (COND
                                     ((EVALLESSP (AEVAL* (LIST 'LENGTH H2))
                                                 (AEVAL* NX))
                                      (ASSGNPRI (AEVAL* "+") NIL 'ONLY)))
                                    (PROGN
                                     (ASSGNPRI (AEVAL* "df( ") NIL 'FIRST)
                                     (ASSGNPRI (AEVAL* (LIST 'SQFIRST H2)) NIL
                                               NIL)
                                     (ASSGNPRI (AEVAL* ", ") NIL NIL)
                                     (ASSGNPRI (AEVAL* (LIST 'SQFIRST H4)) NIL
                                               NIL)
                                     (ASSGNPRI (AEVAL* " )") NIL 'LAST))
                                    (SETQ H2 (AEVAL* (LIST 'SQREST H2)))
                                    (SETQ H4 (AEVAL* (LIST 'SQREST H4)))))))
                                (T
                                 (PROGN
                                  (SETQ H4 (AEVAL* EQLIST))
                                  (COND
                                   ((BOOLVALUE* PARALIST)
                                    (SETQ H4
                                            (AEVAL*
                                             (LIST 'SUB (LIST 'SQSECOND SOLN)
                                                   H4)))))
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
                                                 (AEVAL*
                                                  (LIST 'SQCONS H5 H6))))))
                                     (CAR H5))
                                    (SETQ H5 (CDR H5))
                                    (GO LAB))
                                  (COND
                                   ((AND
                                     (EVALNEQ (AEVAL* H6)
                                              (AEVAL* (LIST 'LIST)))
                                     (EVALNEQ (AEVAL* H2) (AEVAL* 'NONDIV)))
                                    (AEVAL*
                                     (LIST 'PARTINTDF H4 H3 H2 XLIST H6 VL
                                           SB))))
                                  (AEVAL* 'NIL))))
                               (ASSGNPRI
                                (AEVAL*
                                 "======================================================")
                                NIL 'ONLY)
                               (AEVAL* 'NIL))))
                            (SETQ PLLIST (AEVAL* (LIST 'SQREST PLLIST)))
                            (SETQ CLLIST (AEVAL* (LIST 'SQREST CLLIST)))
                            (AEVAL* 'NIL)))
                    (COND
                     ((EVALNEQ (AEVAL* 'SOLNS) (AEVAL* (LIST 'LIST)))
                      (AEVAL* (LIST 'NODEPENDLIST ULIST))))
                    (AEVAL* 'NIL)))
            (SETK 'SBRESERVE (AEVAL* 0))
            (AEVAL* (LIST 'NODEPENDLIST DESYLI))
            (COND
             ((EVALEQUAL (AEVAL* FOUND) (AEVAL* 'NIL))
              (PROGN
               (ASSGNPRI (AEVAL* "There is no conservation law of this order.")
                         NIL 'ONLY)
               (ASSGNPRI
                (AEVAL*
                 "======================================================")
                NIL 'ONLY))))
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
         (ASSGNPRI (AEVAL "time to run conlaw1: ") NIL 'FIRST)
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