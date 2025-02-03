(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MULTROOT)) 
(FLUID '(RTERR! MRERR! |ROOTACC##|)) 
(SWITCH (LIST 'FULLPRECISION 'COMPXROOTS)) 
(PUT 'MULTROOT 'PSOPFN 'MULTROOT1) 
(PUT 'MULTROOT1 'NUMBER-OF-ARGS 1) 
(PUT 'MULTROOT1 'DEFINED-ON-LINE '45) 
(PUT 'MULTROOT1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'MULTROOT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULTROOT1 (U)
    (COND
     ((NEQ (LENGTH U) 2)
      (REDERR "2 args required: pr=desired precision, pl=polynomial list"))
     (T (MULTROOT0 (CAR U) (CADR U))))) 
(PUT 'MULTROOT0 'NUMBER-OF-ARGS 2) 
(PUT 'MULTROOT0 'DEFINED-ON-LINE '50) 
(PUT 'MULTROOT0 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'MULTROOT0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTROOT0 (PR PL)
    (PROG (V ANS PR1 C R RTERR! RA)
      (SETQ *PROTFG T)
      (SETQ PR1 (PRECISION 0))
      (SETQ R *ROUNDED)
      (SETQ C *COMPLEX)
      (SETQ RA |ROOTACC##|)
      (SETQ V
              (ERRORSET*
               (LIST 'MULTROOT2 (LIST 'MULTROOT01 (MKQUOTE PR) (MKQUOTE PL)))
               NIL))
      (SETQ *PROTFG NIL)
      (SETQ |ROOTACC##| RA)
      (RETURN
       (COND
        ((ERRORP V)
         (PROGN
          (PRECISION PR1)
          ((LAMBDA (*MSG)
             (PROGN
              (COND (*ROUNDED (COND ((NOT R) (OFF (LIST 'ROUNDED))) (T NIL)))
                    (R (ON (LIST 'ROUNDED))))
              (COND (*COMPLEX (COND ((NOT C) (OFF (LIST 'COMPLEX))) (T NIL)))
                    (C (ON (LIST 'COMPLEX))))))
           NIL)
          ((LAMBDA (*MSG)
             (COND
              (RTERR!
               (LPRIM
                "for some root value(s), a variable depends on an arbitrary variable"))
              (MRERR! (LPRIM MRERR!))))
           T)
          (MK*SQ (MKSQ (LIST 'MULTROOT PR (REVAL1 PL T)) 1))))
        (T
         ((LAMBDA (*MSG)
            (PROGN
             (SETQ V (CAR V))
             (ON (LIST 'ROUNDED 'COMPLEX))
             (SETQ ANS (REVAL1 V NIL))
             (COND (*ROUNDED (COND ((NOT R) (OFF (LIST 'ROUNDED))) (T NIL)))
                   (R (ON (LIST 'ROUNDED))))
             (COND (*COMPLEX (COND ((NOT C) (OFF (LIST 'COMPLEX))) (T NIL)))
                   (C (ON (LIST 'COMPLEX))))
             ANS))
          NIL)))))) 
(SHARE (LIST 'NPOLY* 'PR* 'PL*)) 
(PUT 'MULTROOT01 'NUMBER-OF-ARGS 2) 
(FLAG '(MULTROOT01) 'OPFN) 
(PUT 'MULTROOT01 'DEFINED-ON-LINE '75) 
(PUT 'MULTROOT01 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'MULTROOT01 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTROOT01 (PR PL)
    (PROG (N LINKS PATH PATHS FL FL1 VAR RTS SOLNS MAXV)
      (SETQ MAXV 0)
      (SETQ NPOLY*
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (SETQ N (AEVAL (LIST 'DIFFERENCE (LIST 'LENGTH PL) 1)))))
      (AEVAL
       (CLEAR
        (LIST '|VLL#| '|VLL2#| '|VARBL#| '|POLN#| '|RTLST#| '|DERV#| '|PRA#|
              '|FXER#|)))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST '|VLL#| (IEVAL N)) (LIST '|VLL2#| (IEVAL N))))
      (SETQ PL* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (AEVAL PL)))
      (SETQ PL
              (SETQ PL*
                      (PROGN
                       (SETQ ALGLIST* (CONS NIL NIL))
                       (AEVAL (LIST 'REVAL (LIST 'CLEARDENR PL*))))))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (SETK (LIST '|VLL#| J)
              (AEVAL* (LIST 'GETVARS (LIST 'PART PL (PLUS J 1)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ LINKS (AEVAL (LIST 'FINDLINKS N)))
      (SETQ PATHS (AEVAL (LIST 'LINKS2PATHS LINKS)))
      (SETQ MRERR! NIL)
      (COND
       ((NOT (BOOLVALUE* PATHS))
        (SETQ MRERR!
                "multroot fails because no univariate polynomial was given.")))
      (COND (MRERR! (REDERR "1")))
      (SETQ FL (AEVAL 'NIL))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (PROGN
         (SETQ FL1 (AEVAL* 'NIL))
         (PROG (PATH)
           (SETQ PATH (GETRLIST (AEVAL* PATHS)))
          LAB
           (COND ((NULL PATH) (RETURN NIL)))
           ((LAMBDA (PATH)
              (COND ((MEMBER J (REVALX PATH)) (SETQ FL1 (AEVAL* 'T)))))
            (CAR PATH))
           (SETQ PATH (CDR PATH))
           (GO LAB))
         (COND ((NOT (BOOLVALUE* FL1)) (SETQ FL (AEVAL* 'T)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL FL) (AEVAL 'T))
        (SETQ MRERR!
                "multroot failure: at least one polynomial has no single base.")))
      (COND (MRERR! (REDERR "2")))
      (ARRAYFN 'ALGEBRAIC
               (LIST (LIST '|VARBL#| (IEVAL N)) (LIST '|POLN#| (IEVAL N))
                     (LIST '|RTLST#| (IEVAL N)) (LIST '|DERV#| (IEVAL N))
                     (LIST '|PRA#| (IEVAL N)) (LIST '|FXER#| (IEVAL N))))
      (SETK 'VLIST* (SETK 'RLIST* (SETK 'SOLNS* (AEVAL (LIST 'LIST)))))
      (SETQ PR* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (AEVAL PR)))
      (RETURN (AEVAL PATHS)))) 
(FLAG '(SUBSETP ALGUNION CLEARDENR) 'OPFN) 
(PUT 'CLEARDENR 'NUMBER-OF-ARGS 1) 
(PUT 'CLEARDENR 'DEFINED-ON-LINE '135) 
(PUT 'CLEARDENR 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'CLEARDENR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEARDENR (PL)
    (PROG (PLO)
      (PROG (POL)
        (SETQ POL (CDR PL))
       LAB
        (COND ((NULL POL) (RETURN NIL)))
        ((LAMBDA (POL)
           (SETQ PLO
                   (CONS (COND ((EQCAR POL 'QUOTIENT) (CADR POL)) (T POL))
                         PLO)))
         (CAR POL))
        (SETQ POL (CDR POL))
        (GO LAB))
      (RETURN (CONS 'LIST (REVERSIP PLO))))) 
(PUT 'ALGUNION 'NUMBER-OF-ARGS 2) 
(PUT 'ALGUNION 'DEFINED-ON-LINE '141) 
(PUT 'ALGUNION 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'ALGUNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGUNION (A B) (CONS 'LIST (UNION (CDR A) (CDR B)))) 
(PUT 'FINDLINKS 'NUMBER-OF-ARGS 1) 
(FLAG '(FINDLINKS) 'OPFN) 
(PUT 'FINDLINKS 'DEFINED-ON-LINE '143) 
(PUT 'FINDLINKS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'FINDLINKS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDLINKS (N)
    (PROG (LINKS FL FL1 VAR)
      (SETQ LINKS (AEVAL (LIST 'LIST)))
      (PROG (M)
        (SETQ M 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) M)) (RETURN NIL)))
        (COND
         ((EQUAL M 1)
          (PROG (J)
            (SETQ J 0)
           LAB
            (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
            (SETK (LIST '|VLL2#| J)
                  (COND
                   ((EVALEQUAL
                     (AEVAL*
                      (LIST 'LENGTH (SETQ FL (AEVAL* (LIST '|VLL#| J)))))
                     1)
                    (COND ((NOT (BOOLVALUE* VAR)) (SETQ VAR (AEVAL* FL)))
                          (T
                           (SETK (LIST '|VLL#| J)
                                 (AEVAL* (LIST 'APPEND FL VAR))))))
                   (T (AEVAL* FL))))
            (SETQ J
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     J))
            (GO LAB)))
         (T
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((MINUSP (DIFFERENCE 2 K)) (RETURN NIL)))
            (PROG (J)
              (SETQ J 0)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
              (COND
               ((EVALEQUAL
                 (AEVAL* (LIST 'LENGTH (SETQ FL (AEVAL* (LIST '|VLL#| J))))) M)
                (SETK (LIST '|VLL2#| J)
                      (COND
                       ((EVALLESSP (AEVAL* (LIST 'LENGTH VAR)) M)
                        (COND
                         ((BOOLVALUE* (REVALX (LIST 'SUBSETP VAR FL)))
                          (SETQ VAR (AEVAL* FL)))
                         (T (AEVAL* FL))))
                       ((EVALEQUAL (AEVAL* FL) (AEVAL* VAR)) (AEVAL* FL))
                       (T
                        (SETK (LIST '|VLL#| J)
                              (AEVAL* (LIST 'ALGUNION FL VAR)))))))
               (T (AEVAL* FL)))
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (GO LAB))
            (SETQ K (PLUS2 K 1))
            (GO LAB))))
        (SETQ M
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 M))
        (GO LAB))
      (REPEAT
       (PROGN
        (SETQ FL (AEVAL* 'NIL))
        (PROG (J)
          (SETQ J 0)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
          (PROGN
           (SETQ FL1 (AEVAL* 'NIL))
           (PROG (K)
             (SETQ K 0)
            LAB
             (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
             (COND
              ((AND (NEQ J K)
                    (EVALEQUAL (AEVAL* (LIST 'LENGTH (LIST '|VLL2#| J))) 1)
                    (BOOLVALUE*
                     (REVALX
                      (LIST 'SUBSET1 (LIST '|VLL2#| J) (LIST '|VLL2#| K)))))
               (PROGN
                (SETQ LINKS
                        (AEVAL*
                         (LIST 'APPEND LINKS (LIST 'LIST (LIST 'LIST J K)))))
                (SETQ FL1 (AEVAL* 'T)))))
             (SETQ K
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      K))
             (GO LAB))
           (COND
            ((EVALEQUAL (AEVAL* FL1) (AEVAL* 'T))
             (PROGN
              (SETQ FL (AEVAL* 'T))
              (PROG (K)
                (SETQ K 0)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
                (PROGN
                 (SETQ VAR (AEVAL* (LIST 'FIRST (LIST '|VLL2#| J))))
                 (COND
                  ((NEQ J K)
                   (SETK (LIST '|VLL2#| K)
                         (AEVAL* (LIST 'DELETE VAR (LIST '|VLL2#| K)))))))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB))))))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB)))
       (NOT (BOOLVALUE* FL)))
      (RETURN (AEVAL LINKS)))) 
(PUT 'MULTROOT2 'NUMBER-OF-ARGS 1) 
(FLAG '(MULTROOT2) 'OPFN) 
(PUT 'MULTROOT2 'DEFINED-ON-LINE '177) 
(PUT 'MULTROOT2 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'MULTROOT2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULTROOT2 (PATHS)
    (PROG (PATH LFP FL SOLN1 SOLN2 PR0 NLST)
     LP
      (SETQ PATH (AEVAL (LIST 'FIRST PATHS)))
      (SETQ PATHS (AEVAL (LIST 'REST PATHS)))
      (SETQ LFP (AEVAL (LIST 'LAST PATH)))
      (SETQ FL (AEVAL 'NIL))
      (PROG (PATH2)
        (SETQ PATH2 (GETRLIST (AEVAL PATHS)))
       LAB
        (COND ((NULL PATH2) (RETURN NIL)))
        ((LAMBDA (PATH2)
           (COND
            ((EVALEQUAL (AEVAL (LIST 'LAST PATH2)) (AEVAL LFP))
             (SETQ FL (AEVAL 'T)))))
         (CAR PATH2))
        (SETQ PATH2 (CDR PATH2))
        (GO LAB))
      (COND
       ((BOOLVALUE* FL)
        (PROGN
         (SETQ MRERR! "multroot failure: This error should not occur!")
         (AEVAL (REDERR (REVALX "3"))))))
      (SETQ SOLN1 (AEVAL (LIST 'SOLVEPATH PATH)))
      (COND
       ((AND (BOOLVALUE* (REVALX *COMPXROOTS))
             (EVALNEQ (SETQ NLST (AEVAL (LIST 'SPURIVAL SOLN1)))
                      (AEVAL (LIST 'LIST))))
        (PROGN
         (SETQ PR0 (AEVAL (LIST 'PRECISION 0)))
         (SETQ PR*
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (AEVAL (LIST 'PLUS PR* 5))))
         (SETQ SOLN2 (AEVAL (LIST 'SOLVEPATH PATH)))
         (AEVAL (LIST 'PRECISION PR0))
         (SETQ PR*
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (AEVAL (LIST 'DIFFERENCE PR* 5))))
         (SETQ SOLN1 (AEVAL (LIST 'SPURIFIX NLST SOLN1 SOLN2))))))
      (COND
       ((NOT (MEMBER (REVALX 'V0*) (REVALX 'VLIST*)))
        (PROGN
         (SETK 'VLIST* (AEVAL (LIST 'APPEND 'VLIST* (LIST 'LIST 'V0*))))
         (SETK 'RLIST* (AEVAL (LIST 'APPEND 'RLIST* (LIST 'LIST 'R0*)))))))
      (COND
       ((EVALEQUAL (AEVAL SOLN1) (AEVAL (LIST 'LIST)))
        (PROGN (SETQ FL (AEVAL 'T)) (GO CL))))
      (SETK 'SOLNS* (AEVAL (LIST 'APPEND 'SOLNS* (LIST 'LIST SOLN1))))
      (COND ((EVALNEQ (AEVAL PATHS) (AEVAL (LIST 'LIST))) (GO LP)))
     CL
      (AEVAL
       (CLEAR
        (LIST '|VLL#| '|VLL2#| '|VARBL#| '|POLN#| '|RTLST#| '|DERV#| '|PRA#|
              '|FXER#|)))
      (RETURN
       (COND ((BOOLVALUE* FL) (AEVAL (LIST 'LIST)))
             (T (AEVAL (LIST 'COMBINESOLNS))))))) 
(PUT 'COMBINESOLNS 'NUMBER-OF-ARGS 0) 
(FLAG '(COMBINESOLNS) 'OPFN) 
(PUT 'COMBINESOLNS 'DEFINED-ON-LINE '203) 
(PUT 'COMBINESOLNS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'COMBINESOLNS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COMBINESOLNS NIL
    (PROG (VLIST RLIST PROD SOLNS VAR RTS GRANDPROD)
      (SETQ VLIST (AEVAL 'VLIST*))
      (SETQ RLIST (AEVAL 'RLIST*))
      (SETQ GRANDPROD (AEVAL (LIST 'LIST)))
     LP2
      (SETQ VAR (AEVAL (LIST 'FIRST VLIST)))
      (SETQ VLIST (AEVAL (LIST 'REST VLIST)))
      (SETQ RTS (AEVAL (LIST 'FIRST RLIST)))
      (SETQ RLIST (AEVAL (LIST 'REST RLIST)))
      (SETQ PROD (AEVAL (LIST 'LIST)))
      (SETQ SOLNS (AEVAL 'SOLNS*))
      (COND
       ((AND (EVALNEQ (AEVAL RTS) (AEVAL (LIST 'LIST)))
             (EVALNEQ (AEVAL SOLNS) (AEVAL (LIST 'LIST (LIST 'LIST)))))
        (PROG (SOLN1)
          (SETQ SOLN1 (GETRLIST (AEVAL SOLNS)))
         LAB
          (COND ((NULL SOLN1) (RETURN NIL)))
          ((LAMBDA (SOLN1)
             (COND
              ((BOOLVALUE* (REVALX (LIST 'ISVAR (LIST 'FIRST SOLN1) VAR)))
               (SETQ PROD
                       (COND
                        ((EVALEQUAL (AEVAL PROD) (AEVAL (LIST 'LIST)))
                         (AEVAL SOLN1))
                        (T (AEVAL (LIST 'OUTCOMBINE1 RTS PROD SOLN1))))))))
           (CAR SOLN1))
          (SETQ SOLN1 (CDR SOLN1))
          (GO LAB))))
      (SETQ GRANDPROD
              (COND
               ((EVALEQUAL (AEVAL GRANDPROD) (AEVAL (LIST 'LIST)))
                (AEVAL PROD))
               (T (AEVAL (LIST 'OUTCOMBINE2 GRANDPROD PROD)))))
      (COND ((EVALNEQ (AEVAL VLIST) (AEVAL (LIST 'LIST))) (GO LP2)))
      (SETK 'GRANDSOLN* (AEVAL GRANDPROD))
      (AEVAL (LIST 'SORTVARS))
      (AEVAL (LIST 'SCREENSOLNS1))
      (RETURN (AEVAL (LIST 'SORTVALS))))) 
(FLAG '(SCREENSOLNS1) 'OPFN) 
(PUT 'SCREENSOLNS1 'NUMBER-OF-ARGS 0) 
(PUT 'SCREENSOLNS1 'DEFINED-ON-LINE '234) 
(PUT 'SCREENSOLNS1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SCREENSOLNS1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SCREENSOLNS1 NIL
    (PROG (INLIST OUTLIST TERMOUT VR VR1 VL VL1 FL)
      (SETQ INLIST (REVERSIP (CDR (AEVAL 'VARSORTSOLNS*))))
      (PROG (RTS)
        (SETQ RTS INLIST)
       LAB
        (COND ((NULL RTS) (RETURN NIL)))
        ((LAMBDA (RTS)
           (PROGN
            (SETQ VR (SETQ VR1 (SETQ FL (SETQ TERMOUT NIL))))
            (PROG (TERM)
              (SETQ TERM (CDR RTS))
             LAB
              (COND ((NULL TERM) (RETURN NIL)))
              ((LAMBDA (TERM)
                 (COND
                  ((NOT FL)
                   (PROGN
                    (SETQ VR1 (CADR TERM))
                    (SETQ VL1 (CADDR TERM))
                    (COND
                     ((OR (NOT VR) (NEQ (AEVAL (LIST 'DIFFERENCE VR VR1)) 0))
                      (PROGN
                       (SETQ VR VR1)
                       (SETQ VL VL1)
                       (SETQ TERMOUT (CONS TERM TERMOUT))))
                     ((NEQ (AEVAL (LIST 'DIFFERENCE VL1 VL)) 0)
                      (SETQ FL T)))))))
               (CAR TERM))
              (SETQ TERM (CDR TERM))
              (GO LAB))
            (COND
             ((NOT FL)
              (SETQ OUTLIST (CONS (CONS 'LIST (REVERSIP TERMOUT)) OUTLIST))))))
         (CAR RTS))
        (SETQ RTS (CDR RTS))
        (GO LAB))
      (RETURN (SETK 'VARSORTSOLNS* (CONS 'LIST OUTLIST))))) 
(PUT 'SOLVEPATH 'NUMBER-OF-ARGS 1) 
(FLAG '(SOLVEPATH) 'OPFN) 
(PUT 'SOLVEPATH 'DEFINED-ON-LINE '250) 
(PUT 'SOLVEPATH 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SOLVEPATH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEPATH (PATH)
    (PROG (N VL VLLL M S RTS0 FL FL1 B TST F FF PR1 PRF STRT DFX RTL RT1 *MSG
           ZZ R C)
      (SETQ N (AEVAL (LIST 'LENGTH PATH)))
      (COND
       ((BOOLVALUE* (SETQ R (REVALX *ROUNDED))) (AEVAL (OFF (LIST 'ROUNDED)))))
      (COND
       ((BOOLVALUE* (SETQ C (REVALX *COMPLEX))) (AEVAL (OFF (LIST 'COMPLEX)))))
      (SETQ VL
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (AEVAL*
                                  (LIST 'PART PL*
                                        (LIST 'PLUS (LIST 'PART PATH J) 1)))
                                 NIL)))
               LOOPLABEL
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (AEVAL*
                          (LIST 'PART PL* (LIST 'PLUS (LIST 'PART PATH J) 1)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VLLL
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (AEVAL* (LIST '|VLL#| (LIST 'PART PATH J)))
                                 NIL)))
               LOOPLABEL
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS (AEVAL* (LIST '|VLL#| (LIST 'PART PATH J))) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N (AEVAL (LIST 'DIFFERENCE N 1)))
      (SETK (LIST '|PRA#| 0) (AEVAL (LIST 'PLUS PR* 10)))
      (AEVAL (ON (LIST 'ROUNDED)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (SETK (LIST '|PRA#| J) (AEVAL* PR*))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETK 'V0*
            (SETK (LIST '|VARBL#| 0) (AEVAL (LIST 'FIRST (LIST 'FIRST VLLL)))))
      (SETQ STRT (AEVAL 'T))
     STR
      (AEVAL (LIST 'PRECISION (LIST '|PRA#| 0)))
      (SETQ RTS0
              (COND
               ((AND (BOOLVALUE* STRT) (MEMBER (REVALX 'V0*) (REVALX 'VLIST*)))
                (SETK 'R0*
                      (AEVAL
                       (LIST 'PART 'RLIST* (LIST 'MEMBNO 'V0* 'VLIST*)))))
               ((BOOLVALUE* (REVALX *COMPXROOTS))
                (AEVAL (LIST 'ROOTS (LIST 'FIRST VL))))
               (T (AEVAL (LIST 'REALROOTS (LIST 'FIRST VL))))))
      (SETK 'R0* (AEVAL RTS0))
      (SETK (LIST '|RTLST#| 0)
            (PROG (RT FORALL-RESULT FORALL-ENDPTR)
              (SETQ RT (GETRLIST (AEVAL RTS0)))
              (COND ((NULL RT) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (RT) (AEVAL (LIST 'LIST RT))) (CAR RT))
                               NIL)))
             LOOPLABEL
              (SETQ RT (CDR RT))
              (COND ((NULL RT) (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (RT) (AEVAL (LIST 'LIST RT))) (CAR RT))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETQ M (AEVAL 0))
      (SETQ STRT (AEVAL 'NIL))
     NXT
      (SETQ FL (SETQ FL1 (SETQ B (AEVAL 0))))
      (COND
       ((EVALGREATERP (SETQ M (AEVAL (LIST 'PLUS M 1))) (AEVAL N))
        (PROGN (SETQ M (AEVAL (LIST 'DIFFERENCE M 1))) (GO RET))))
      (SETK (LIST '|POLN#| M) (AEVAL (LIST 'PART VL (LIST 'PLUS M 1))))
      (SETQ RTL (AEVAL (LIST 'LIST)))
      (PROG (RT)
        (SETQ RT (GETRLIST (AEVAL (LIST '|RTLST#| (LIST 'DIFFERENCE M 1)))))
       LAB
        (COND ((NULL RT) (RETURN NIL)))
        ((LAMBDA (RT)
           (PROGN
            (SETQ ZZ (AEVAL (LIST 'SUB RT (LIST '|POLN#| M))))
            (SETQ RT1
                    (COND
                     ((EVALEQUAL (AEVAL (LIST 'MAINVAR ZZ)) 0)
                      (COND ((EVALEQUAL (AEVAL ZZ) 0) (AEVAL (LIST 'LIST RT)))
                            (T (AEVAL (LIST 'LIST)))))
                     (T
                      (AEVAL
                       (LIST 'COMBINERTS RT
                             (PROGN
                              (SETQ RTERR! T)
                              (SETQ ZZ
                                      (COND
                                       ((BOOLVALUE* (REVALX *COMPXROOTS))
                                        (AEVAL (LIST 'ROOTS ZZ)))
                                       (T (AEVAL (LIST 'REALROOTS ZZ)))))
                              (SETQ RTERR! NIL)
                              (AEVAL ZZ)))))))
            (COND
             ((EVALNEQ (AEVAL RT1) (AEVAL (LIST 'LIST)))
              (SETQ RTL (AEVAL (LIST 'APPEND RTL RT1)))))))
         (CAR RT))
        (SETQ RT (CDR RT))
        (GO LAB))
      (SETK (LIST '|RTLST#| M) (AEVAL RTL))
      (SETQ S (AEVAL (LIST 'LENGTH (LIST '|RTLST#| M))))
      (SETK (LIST '|VARBL#| M)
            (AEVAL
             (LIST 'ELIM (LIST 'PART VLLL M)
                   (LIST 'PART VLLL (LIST 'PLUS M 1)))))
      (SETK (LIST '|DERV#| M)
            (AEVAL
             (LIST 'LIST
                   (LIST 'MINUS
                         (LIST 'PLUS
                               (LIST 'DF (LIST '|POLN#| M) (LIST '|VARBL#| 0))
                               (COND ((EVALLESSP (AEVAL M) 2) 0)
                                     (T
                                      (PROG (J FORALL-RESULT)
                                        (SETQ J 1)
                                        (SETQ FORALL-RESULT 0)
                                       LAB1
                                        (COND
                                         ((|AMINUSP:|
                                           (LIST 'DIFFERENCE
                                                 (AEVAL*
                                                  (LIST 'DIFFERENCE M 1))
                                                 J))
                                          (RETURN FORALL-RESULT)))
                                        (SETQ FORALL-RESULT
                                                (AEVAL*
                                                 (LIST 'PLUS
                                                       (AEVAL*
                                                        (LIST 'TIMES
                                                              (LIST 'DF
                                                                    (LIST
                                                                     '|POLN#|
                                                                     M)
                                                                    (LIST
                                                                     '|VARBL#|
                                                                     J))
                                                              (LIST 'QUOTIENT
                                                                    (LIST
                                                                     'FIRST
                                                                     (LIST
                                                                      '|DERV#|
                                                                      J))
                                                                    (LIST
                                                                     'SECOND
                                                                     (LIST
                                                                      '|DERV#|
                                                                      J)))))
                                                       FORALL-RESULT)))
                                        (SETQ J
                                                ((LAMBDA (FORALL-RESULT)
                                                   (AEVAL*
                                                    (LIST 'PLUS FORALL-RESULT
                                                          1)))
                                                 J))
                                        (GO LAB1))))))
                   (LIST 'DF (LIST '|POLN#| M) (LIST '|VARBL#| M)))))
     LP1
      (COND
       ((EVALGREATERP (SETQ B (AEVAL (LIST 'PLUS B 1))) (AEVAL S))
        (PROGN
         (SETQ PRF (AEVAL 'NIL))
         (COND
          ((EVALGREATERP (AEVAL FL) 0)
           (PROGN
            (SETK (LIST '|FXER#| M) (AEVAL (LIST 'PFX PR* FL)))
            (SETQ FL (AEVAL 0))
            (SETQ DFX
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST '|FXER#| M)
                           (LIST '|FXER#| (LIST 'DIFFERENCE M 1)))))
            (PROG (J)
              (SETQ J 0)
             LAB
              (COND
               ((|AMINUSP:|
                 (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE M 1)) J))
                (RETURN NIL)))
              (PROGN
               (SETQ PR1 (AEVAL* (LIST 'PLUS (LIST '|PRA#| J) DFX)))
               (COND
                ((EVALGREATERP (AEVAL* PR1) (AEVAL* (LIST '|PRA#| J)))
                 (PROGN
                  (SETQ PRF (AEVAL* 'T))
                  (SETK (LIST '|PRA#| J) (AEVAL* PR1))))))
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (GO LAB)))))
         (COND ((BOOLVALUE* PRF) (GO STR)) (T (GO NXT)))))
       (T
        (PROGN
         (COND
          ((EVALGREATERP
            (SETQ TST
                    (AEVAL
                     (LIST 'ABS
                           (LIST 'TIMES
                                 (LIST 'EXPT '(|:DN:| 100 . -1)
                                       (LIST 'MINUS (LIST '|PRA#| 0)))
                                 (LIST 'CABS
                                       (LIST 'TESTSUB
                                             (LIST 'PART (LIST '|RTLST#| M) B)
                                             (LIST '|DERV#| M) M))))))
            (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS PR*))))
           (SETQ FL1 (AEVAL TST))))
         (COND ((EVALGREATERP (AEVAL FL1) (AEVAL FL)) (SETQ FL (AEVAL FL1))))
         (GO LP1))))
     RET
      (COND ((BOOLVALUE* R) (AEVAL (ON (LIST 'ROUNDED)))))
      (COND ((BOOLVALUE* C) (AEVAL (ON (LIST 'COMPLEX)))))
      (COND ((NOT (BOOLVALUE* (REVALX *FULLPRECISION))) (GO RT2)))
      (AEVAL (LIST 'PRECISION (LIST '|PRA#| 0)))
      (RETURN
       (COND ((EVALEQUAL (AEVAL M) 0) (AEVAL (LIST '|RTLST#| 0)))
             (T
              (PROG (RTL FORALL-RESULT FORALL-ENDPTR)
                (SETQ RTL (GETRLIST (AEVAL (LIST '|RTLST#| M))))
                (COND ((NULL RTL) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (RTL)
                                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ J 0)
                                      (COND
                                       ((|AMINUSP:|
                                         (LIST 'DIFFERENCE (AEVAL* M) J))
                                        (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       (AEVAL*
                                                        (LIST 'ROUNDROOT
                                                              (LIST 'PART RTL
                                                                    (PLUS J 1))
                                                              (LIST '|PRA#|
                                                                    J)))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ J
                                              ((LAMBDA (FORALL-RESULT)
                                                 (AEVAL*
                                                  (LIST 'PLUS FORALL-RESULT
                                                        1)))
                                               J))
                                      (COND
                                       ((|AMINUSP:|
                                         (LIST 'DIFFERENCE (AEVAL* M) J))
                                        (RETURN (CONS 'LIST FORALL-RESULT))))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               (AEVAL*
                                                (LIST 'ROUNDROOT
                                                      (LIST 'PART RTL
                                                            (PLUS J 1))
                                                      (LIST '|PRA#| J)))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR RTL))
                                 NIL)))
               LOOPLABEL
                (SETQ RTL (CDR RTL))
                (COND ((NULL RTL) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (RTL)
                            (PROG (J FORALL-RESULT FORALL-ENDPTR)
                              (SETQ J 0)
                              (COND
                               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* M) J))
                                (RETURN (MAKELIST NIL))))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               (AEVAL*
                                                (LIST 'ROUNDROOT
                                                      (LIST 'PART RTL
                                                            (PLUS J 1))
                                                      (LIST '|PRA#| J)))
                                               NIL)))
                             LOOPLABEL
                              (SETQ J
                                      ((LAMBDA (FORALL-RESULT)
                                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                       J))
                              (COND
                               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* M) J))
                                (RETURN (CONS 'LIST FORALL-RESULT))))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       (AEVAL*
                                        (LIST 'ROUNDROOT
                                              (LIST 'PART RTL (PLUS J 1))
                                              (LIST '|PRA#| J)))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR RTL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))))
     RT2
      (AEVAL (LIST 'PRECISION PR*))
      (RETURN (AEVAL (LIST '|RTLST#| M))))) 
(PUT 'TESTSUB 'NUMBER-OF-ARGS 3) 
(FLAG '(TESTSUB) 'OPFN) 
(PUT 'TESTSUB 'DEFINED-ON-LINE '330) 
(PUT 'TESTSUB 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'TESTSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TESTSUB (LST QUOTLST M)
    (PROG (NMR DNR DNV)
      (SETQ NMR (AEVAL (LIST 'FIRST QUOTLST)))
      (SETQ DNR (AEVAL (LIST 'SECOND QUOTLST)))
      (SETK 'EX! (AEVAL (LIST 'QUOTIENT NMR DNR)))
      (SETQ NMR (AEVAL (LIST 'NUM 'EX!)))
      (SETQ DNR (AEVAL (LIST 'DEN 'EX!)))
      (WHILE (EVALEQUAL (SETQ DNV (AEVAL* (LIST 'SUB LST DNR))) 0)
             (COND
              ((EVALEQUAL (AEVAL* (LIST 'SUB LST NMR)) 0)
               (PROGN
                (SETQ NMR (AEVAL* (LIST 'DF NMR (LIST '|VARBL#| M))))
                (SETQ DNR (AEVAL* (LIST 'DF DNR (LIST '|VARBL#| M))))
                (COND
                 ((EVALNEQ (AEVAL* DNR) 0)
                  (PROGN
                   (SETK 'EX! (AEVAL* (LIST 'QUOTIENT NMR DNR)))
                   (SETQ NMR (AEVAL* (LIST 'NUM 'EX!)))
                   (SETQ DNR (AEVAL* (LIST 'DEN 'EX!)))))
                 (T
                  (AEVAL*
                   (LIST 'WHEREEXP (LIST 'LIST (LIST 'EQUAL '*MSG 'T))
                         (PROGN
                          (SETQ NMR (AEVAL* 1))
                          (SETQ DNR (AEVAL* 1))
                          (AEVAL* (LIST 'LPRIM "stuffing 1 (dnr prob)")))))))))
              (T
               (AEVAL*
                (LIST 'WHEREEXP (LIST 'LIST (LIST 'EQUAL '*MSG 'T))
                      (PROGN
                       (SETQ NMR (AEVAL* 1))
                       (SETQ DNR (AEVAL* 1))
                       (AEVAL* (LIST 'LPRIM "stuffing 1 (nmr prob)"))))))))
      (RETURN (AEVAL (LIST 'QUOTIENT (LIST 'SUB LST NMR) DNV))))) 
(PUT 'SORTVALS1 'NUMBER-OF-ARGS 2) 
(PUT 'SORTVALS1 'DEFINED-ON-LINE '349) 
(PUT 'SORTVALS1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SORTVALS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORTVALS1 (A B)
    (PROG (C D)
      (SETQ A (CDR A))
      (SETQ B (CDR B))
     LP
      (COND ((OR (NOT A) (NOT B)) (RETURN NIL)))
      (SETQ C (CADDAR A))
      (SETQ D (CADDAR B))
      (SETQ C (AEVAL (LIST 'REPART C)))
      (SETQ D (AEVAL (LIST 'REPART D)))
      (COND ((EVALLESSP (AEVAL C) (AEVAL D)) (RETURN (AEVAL 'T)))
            ((EVALEQUAL (AEVAL C) (AEVAL D)) (GO TST)))
      (RETURN NIL)
     TST
      (COND ((SETQ A (CDR A)) (PROGN (SETQ B (CDR B)) (GO LP)))))) 
(PUT 'GETVARS 'NUMBER-OF-ARGS 1) 
(FLAG '(GETVARS) 'OPFN) 
(PUT 'GETVARS 'DEFINED-ON-LINE '359) 
(PUT 'GETVARS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'GETVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETVARS (P)
    (PROG (VL V V1 LT C *MSG)
      (SETQ C (AEVAL *COMPLEX))
      (COND ((NOT (BOOLVALUE* C)) (AEVAL (ON (LIST 'COMPLEX)))))
      (SETQ VL (AEVAL (LIST 'LIST)))
     LP1
      (COND ((EVALNUMBERP (SETQ V (AEVAL (LIST 'MAINVAR P)))) (GO RET))
            ((NOT (MEMBER (REVALX V) (REVALX VL)))
             (SETQ VL (AEVAL (LIST 'CONS V VL)))))
      (SETQ LT (AEVAL (LIST 'LCOF P V)))
      (SETQ P (AEVAL (LIST 'REDUCT P V)))
     LP2
      (COND ((EVALNUMBERP (SETQ V1 (AEVAL (LIST 'MAINVAR LT)))) (GO LP1))
            ((NOT (MEMBER (REVALX V1) (REVALX VL)))
             (SETQ VL (AEVAL (LIST 'CONS V1 VL)))))
      (SETQ LT (AEVAL (LIST 'LCOF LT V1)))
      (GO LP2)
     RET
      (COND ((NOT (BOOLVALUE* C)) (AEVAL (OFF (LIST 'COMPLEX)))))
      (RETURN (AEVAL (LIST 'REVERSE VL))))) 
(FLAG '(SPURIVAL SPURIFIX) 'OPFN) 
(SHARE (LIST 'VAL$ 'VAL2$ 'EPS$ 'PR*)) 
(PUT 'SPURIVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SPURIVAL 'DEFINED-ON-LINE '375) 
(PUT 'SPURIVAL 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SPURIVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPURIVAL (VALS)
    (PROG (FL R C *MSG M)
      (SETQ M 0)
      (SETQ EPS$
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (EXPT 10.0 (MINUS PR*))))
      (SETQ R *ROUNDED)
      (SETQ C *COMPLEX)
      (ON (LIST 'ROUNDED))
      (OFF (LIST 'COMPLEX))
      (PROG (RLST)
        (SETQ RLST (CDR VALS))
       LAB
        (COND ((NULL RLST) (RETURN NIL)))
        ((LAMBDA (RLST)
           (PROG (VAL)
             (SETQ VAL (CDR RLST))
            LAB
             (COND ((NULL VAL) (RETURN NIL)))
             ((LAMBDA (VAL)
                (PROGN
                 (SETQ M (PLUS M 1))
                 (SETQ VAL$
                         (PROGN
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (PREPSQ (SIMP* (CADDR VAL)))))
                 (COND
                  ((AND (EQCAR VAL$ 'PLUS) (AEVAL (LIST 'TESTVAL1 VAL$)))
                   (SETQ FL (CONS M FL))))))
              (CAR VAL))
             (SETQ VAL (CDR VAL))
             (GO LAB)))
         (CAR RLST))
        (SETQ RLST (CDR RLST))
        (GO LAB))
      (COND ((NOT R) (OFF (LIST 'ROUNDED))))
      (COND (C (ON (LIST 'COMPLEX))))
      (RETURN (CONS 'LIST (REVERSIP FL))))) 
(PUT 'TESTVAL1 'NUMBER-OF-ARGS 1) 
(FLAG '(TESTVAL1) 'OPFN) 
(PUT 'TESTVAL1 'DEFINED-ON-LINE '388) 
(PUT 'TESTVAL1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'TESTVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TESTVAL1 (VAL)
    (PROG (RL IM)
      (SETQ RL (AEVAL (LIST 'ABS (LIST 'REPART VAL))))
      (SETQ IM (AEVAL (LIST 'ABS (LIST 'IMPART VAL))))
      (COND
       ((OR
         (AND (EVALGREATERP (AEVAL RL) 0) (EVALGREATERP (AEVAL IM) 0)
              (EVALLESSP (AEVAL IM) (AEVAL (LIST 'TIMES EPS$ RL))))
         (EVALLESSP (AEVAL RL) (AEVAL (LIST 'TIMES EPS$ IM))))
        (RETURN (AEVAL 'T)))))) 
(PUT 'SPURIFIX 'NUMBER-OF-ARGS 3) 
(PUT 'SPURIFIX 'DEFINED-ON-LINE '393) 
(PUT 'SPURIFIX 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SPURIFIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPURIFIX (NDX SOLN1 SOLN2)
    (COND ((OR (NULL NDX) (NEQ (LENGTH SOLN1) (LENGTH SOLN2))) SOLN1)
          (T
           (PROG (SOL0 SOLN3 LST1 LST2 LST3 VAL1 VAL2 EQ1 EQ2 EQ3 R C *MSG TRI
                  M M0)
             (SETQ M 0)
             (SETQ M0 0)
             (SETQ R *ROUNDED)
             (SETQ C *COMPLEX)
             (ON (LIST 'ROUNDED))
             (OFF (LIST 'COMPLEX))
             (SETQ SOLN1 (CDR (SETQ SOL0 SOLN1)))
             (SETQ SOLN2 (CDR SOLN2))
             (SETQ NDX (CDR NDX))
             (SETQ M0 (CAR NDX))
             (SETQ NDX (CDR NDX))
            LP1
             (COND
              ((NOT SOLN1)
               (PROGN (SETQ SOLN3 (CONS 'LIST (REVERSIP SOLN3))) (GO RET))))
             (SETQ LST1 (CDAR SOLN1))
             (SETQ LST2 (CDAR SOLN2))
             (SETQ SOLN1 (CDR SOLN1))
             (SETQ SOLN2 (CDR SOLN2))
             (COND
              ((NEQ (LENGTH LST1) (LENGTH LST2))
               (PROGN (SETQ SOLN3 SOL0) (GO RET))))
            LP2
             (COND
              ((NOT LST1)
               (PROGN
                (SETQ SOLN3 (CONS (CONS 'LIST (REVERSIP LST3)) SOLN3))
                (SETQ LST3 NIL)
                (GO LP1))))
             (SETQ EQ1 (CAR LST1))
             (SETQ EQ2 (CAR LST2))
             (SETQ M (PLUS M 1))
             (SETQ LST1 (CDR LST1))
             (SETQ LST2 (CDR LST2))
             (COND
              ((OR (NOT M0) (LESSP M M0))
               (PROGN (SETQ LST3 (CONS EQ1 LST3)) (GO LP2))))
             (COND ((NOT NDX) (SETQ M0 NIL))
                   (T (PROGN (SETQ M0 (CAR NDX)) (SETQ NDX (CDR NDX)))))
             (SETQ EQ1 EQ1)
             (SETQ VAL$
                     (PROGN
                      (SETQ ALGLIST* (CONS NIL NIL))
                      (PREPSQ (SIMP* (SETQ VAL1 (CADDR EQ1))))))
             (SETQ VAL2$
                     (PROGN
                      (SETQ ALGLIST* (CONS NIL NIL))
                      (PREPSQ (SIMP* (SETQ VAL2 (CADDR EQ2))))))
             (SETQ TRI (AEVAL (LIST 'TESTVAL2 VAL$ VAL2$)))
             (COND
              ((EQUAL TRI (REVAL1 'FAILED NIL))
               (PROGN
                ((LAMBDA (*MSG)
                   (LPRIM
                    "match failed! root stripping aborted: raw roots returned"))
                 T)
                (SETQ SOLN3 SOL0)
                (GO RET))))
             (SETQ EQ3
                     (COND ((EQUAL TRI 0) EQ1)
                           (T
                            (LIST 'EQUAL (CADR EQ1)
                                  (COND
                                   ((FREEOF (CAR (SETQ VAL1 (CDR VAL1))) 'I)
                                    (COND ((EQUAL TRI 1) (CADR VAL1))
                                          (T (CAR VAL1))))
                                   ((EQUAL TRI 1) (CAR VAL1))
                                   (T (CADR VAL1)))))))
             (SETQ LST3 (CONS EQ3 LST3))
             (GO LP2)
            RET
             (COND ((NOT R) (OFF (LIST 'ROUNDED))))
             (COND (C (ON (LIST 'COMPLEX))))
             (RETURN SOLN3))))) 
(PUT 'TESTVAL2 'NUMBER-OF-ARGS 2) 
(FLAG '(TESTVAL2) 'OPFN) 
(PUT 'TESTVAL2 'DEFINED-ON-LINE '434) 
(PUT 'TESTVAL2 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'TESTVAL2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTVAL2 (A B)
    (PROG (RL1 RL2 IM1 IM2)
      (SETQ RL1 (AEVAL (LIST 'ABS (LIST 'REPART A))))
      (SETQ RL2 (AEVAL (LIST 'ABS (LIST 'REPART B))))
      (SETQ IM1 (AEVAL (LIST 'ABS (LIST 'IMPART A))))
      (SETQ IM2 (AEVAL (LIST 'ABS (LIST 'IMPART B))))
      (RETURN
       (COND
        ((EVALEQUAL (AEVAL RL1) (AEVAL RL2))
         (COND ((EVALEQUAL (AEVAL IM1) (AEVAL IM2)) 0) (T 2)))
        ((EVALEQUAL (AEVAL IM1) (AEVAL IM2)) 1) (T (AEVAL 'FAILED)))))) 
(PUT 'ISVAR 'NUMBER-OF-ARGS 2) 
(PUT 'ISVAR 'DEFINED-ON-LINE '443) 
(PUT 'ISVAR 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'ISVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ISVAR (X VAR) (EQUAL (COND ((EQCAR X 'LIST) (CADADR X)) (T (CADR X))) VAR)) 
(FLAG '(ISVAR) 'OPFN) 
(PUT 'OUTCOMBINE1 'NUMBER-OF-ARGS 3) 
(FLAG '(OUTCOMBINE1) 'OPFN) 
(PUT 'OUTCOMBINE1 'DEFINED-ON-LINE '448) 
(PUT 'OUTCOMBINE1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'OUTCOMBINE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OUTCOMBINE1 (RTS P1 P2)
    (PROG (PROD P1STRT P1END P2STRT P2END)
      (SETQ PROD (AEVAL (LIST 'LIST)))
      (PROG (RT)
        (SETQ RT (GETRLIST (AEVAL RTS)))
       LAB
        (COND ((NULL RT) (RETURN NIL)))
        ((LAMBDA (RT)
           (PROGN
            (SETQ P1END
                    (AEVAL
                     (LIST 'SECOND
                           (SETQ P1STRT (AEVAL (LIST 'FINDVALS P1 RT))))))
            (SETQ P1STRT (AEVAL (LIST 'FIRST P1STRT)))
            (SETQ P2END
                    (AEVAL
                     (LIST 'SECOND
                           (SETQ P2STRT (AEVAL (LIST 'FINDVALS P2 RT))))))
            (SETQ P2STRT (AEVAL (LIST 'FIRST P2STRT)))
            (COND
             ((AND (EVALGREATERP (AEVAL P1STRT) 0)
                   (EVALGREATERP (AEVAL P2STRT) 0))
              (PROG (N1)
                (SETQ N1 (AEVAL* P1STRT))
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* P1END) N1))
                  (RETURN NIL)))
                (PROG (N2)
                  (SETQ N2 (AEVAL* P2STRT))
                 LAB
                  (COND
                   ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* P2END) N2))
                    (RETURN NIL)))
                  (SETQ PROD
                          (AEVAL*
                           (LIST 'APPEND PROD
                                 (LIST 'LIST
                                       (LIST 'APPEND (LIST 'PART P1 N1)
                                             (LIST 'REST
                                                   (LIST 'PART P2 N2)))))))
                  (SETQ N2
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           N2))
                  (GO LAB))
                (SETQ N1
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         N1))
                (GO LAB))))))
         (CAR RT))
        (SETQ RT (CDR RT))
        (GO LAB))
      (RETURN (AEVAL PROD)))) 
(PUT 'FINDVALS 'NUMBER-OF-ARGS 2) 
(PUT 'FINDVALS 'DEFINED-ON-LINE '465) 
(PUT 'FINDVALS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'FINDVALS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDVALS (P RT)
    (PROG (VAL PP PP1 N B E)
      (SETQ N 0)
      (SETQ B 0)
      (SETQ E 0)
      (SETQ VAL (AEVAL (CADDR RT)))
      (SETQ PP (CDR P))
     LP
      (SETQ PP1 (CAR PP))
      (SETQ PP (CDR PP))
      (SETQ N (PLUS N 1))
      (COND
       ((EQUAL (AEVAL (LIST 'DIFFERENCE (CADDR (CADR PP1)) VAL)) 0)
        (PROGN (COND ((EQUAL B 0) (SETQ B N))) (SETQ E N))))
      (COND (PP (GO LP)))
      (RETURN (CONS 'LIST (LIST B E))))) 
(FLAG '(FINDVALS) 'OPFN) 
(PUT 'OUTCOMBINE2 'NUMBER-OF-ARGS 2) 
(FLAG '(OUTCOMBINE2) 'OPFN) 
(PUT 'OUTCOMBINE2 'DEFINED-ON-LINE '477) 
(PUT 'OUTCOMBINE2 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'OUTCOMBINE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OUTCOMBINE2 (P1 P2)
    (PROG (PROD)
      (SETQ PROD (AEVAL (LIST 'LIST)))
      (PROG (R1)
        (SETQ R1 (GETRLIST (AEVAL P1)))
       LAB
        (COND ((NULL R1) (RETURN NIL)))
        ((LAMBDA (R1)
           (PROG (R2)
             (SETQ R2 (GETRLIST (AEVAL P2)))
            LAB
             (COND ((NULL R2) (RETURN NIL)))
             ((LAMBDA (R2)
                (SETQ PROD
                        (AEVAL
                         (LIST 'APPEND PROD
                               (LIST 'LIST (LIST 'APPEND R1 R2))))))
              (CAR R2))
             (SETQ R2 (CDR R2))
             (GO LAB)))
         (CAR R1))
        (SETQ R1 (CDR R1))
        (GO LAB))
      (RETURN (AEVAL PROD)))) 
(PUT 'SORTVARS 'NUMBER-OF-ARGS 0) 
(PUT 'SORTVARS 'DEFINED-ON-LINE '484) 
(PUT 'SORTVARS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SORTVARS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SORTVARS NIL
    (SETK 'VARSORTSOLNS*
          (CONS 'LIST
                (PROG (RTS FORALL-RESULT FORALL-ENDPTR)
                  (SETQ RTS (CDR (AEVAL 'GRANDSOLN*)))
                  (COND ((NULL RTS) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (RTS)
                                      (CONS 'LIST
                                            (SORT (CDR RTS)
                                                  (FUNCTION
                                                   (LAMBDA (A B)
                                                     (ORDOP (CADR A)
                                                            (CADR B)))))))
                                    (CAR RTS))
                                   NIL)))
                 LOOPLABEL
                  (SETQ RTS (CDR RTS))
                  (COND ((NULL RTS) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (RTS)
                              (CONS 'LIST
                                    (SORT (CDR RTS)
                                          (FUNCTION
                                           (LAMBDA (A B)
                                             (ORDOP (CADR A) (CADR B)))))))
                            (CAR RTS))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))) 
(FLAG '(SORTVARS) 'OPFN) 
(PUT 'SORTVALS 'NUMBER-OF-ARGS 0) 
(PUT 'SORTVALS 'DEFINED-ON-LINE '492) 
(PUT 'SORTVALS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SORTVALS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SORTVALS NIL
    (SETK 'SORTVALS*
          (CONS 'LIST
                (SORT (CDR (AEVAL 'VARSORTSOLNS*)) (FUNCTION SORTVALS1))))) 
(FLAG '(SORTVALS) 'OPFN) 
(PUT 'CABS 'NUMBER-OF-ARGS 1) 
(FLAG '(CABS) 'OPFN) 
(PUT 'CABS 'DEFINED-ON-LINE '498) 
(PUT 'CABS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'CABS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CABS (X)
    (COND
     ((BOOLVALUE* (REVALX *COMPXROOTS))
      (AEVAL
       (LIST 'SQRT
             (LIST 'PLUS (LIST 'EXPT (LIST 'REPART X) 2)
                   (LIST 'EXPT (LIST 'IMPART X) 2)))))
     (T (AEVAL X)))) 
(PUT 'MEMBNO 'NUMBER-OF-ARGS 2) 
(FLAG '(MEMBNO) 'OPFN) 
(PUT 'MEMBNO 'DEFINED-ON-LINE '501) 
(PUT 'MEMBNO 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'MEMBNO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MEMBNO (N L)
    (COND ((EVALEQUAL (AEVAL N) (AEVAL (LIST 'FIRST L))) 1)
          ((EVALEQUAL (AEVAL (LIST 'REST L)) 0) 0)
          (T (AEVAL (LIST 'PLUS 1 (LIST 'MEMBNO N (LIST 'REST L))))))) 
(PUT 'GETPATHS 'NUMBER-OF-ARGS 1) 
(FLAG '(GETPATHS) 'OPFN) 
(PUT 'GETPATHS 'DEFINED-ON-LINE '505) 
(PUT 'GETPATHS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'GETPATHS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETPATHS (N)
    (PROG (LINKS)
      (SETQ LINKS (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (PROG (K)
          (SETQ K 0)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
          (COND
           ((AND (NEQ J K)
                 (BOOLVALUE*
                  (REVALX (LIST 'SUBSET1 (LIST '|VLL#| J) (LIST '|VLL#| K)))))
            (SETQ LINKS
                    (AEVAL*
                     (LIST 'APPEND LINKS (LIST 'LIST (LIST 'LIST J K)))))))
          (SETQ K
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   K))
          (GO LAB))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL (LIST 'LINKS2PATHS LINKS))))) 
(PUT 'LINKS2PATHS 'NUMBER-OF-ARGS 1) 
(FLAG '(LINKS2PATHS) 'OPFN) 
(PUT 'LINKS2PATHS 'DEFINED-ON-LINE '513) 
(PUT 'LINKS2PATHS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'LINKS2PATHS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINKS2PATHS (LINKS)
    (PROG (PATHS PATHS2 BASES FL FL2)
      (SETQ PATHS (SETQ PATHS2 (AEVAL (LIST 'LIST))))
      (SETQ BASES (AEVAL (LIST 'ROOT NPOLY*)))
      (COND
       ((EVALEQUAL (AEVAL BASES) (AEVAL (LIST 'LIST))) (RETURN (AEVAL 'NIL))))
      (PROG (BASE)
        (SETQ BASE (GETRLIST (AEVAL BASES)))
       LAB
        (COND ((NULL BASE) (RETURN NIL)))
        ((LAMBDA (BASE)
           (PROGN
            (SETQ FL (AEVAL 'NIL))
            (PROG (LINK)
              (SETQ LINK (GETRLIST (AEVAL LINKS)))
             LAB
              (COND ((NULL LINK) (RETURN NIL)))
              ((LAMBDA (LINK)
                 (COND
                  ((EVALEQUAL (AEVAL BASE) (AEVAL (LIST 'FIRST LINK)))
                   (PROGN
                    (SETQ FL (AEVAL 'T))
                    (SETQ PATHS2
                            (AEVAL
                             (LIST 'APPEND PATHS2 (LIST 'LIST LINK))))))))
               (CAR LINK))
              (SETQ LINK (CDR LINK))
              (GO LAB))
            (COND
             ((NOT (BOOLVALUE* FL))
              (SETQ PATHS
                      (AEVAL
                       (LIST 'APPEND PATHS
                             (LIST 'LIST (LIST 'LIST BASE)))))))))
         (CAR BASE))
        (SETQ BASE (CDR BASE))
        (GO LAB))
     EXT
      (SETQ FL2 (AEVAL 'NIL))
      (PROG (PATH)
        (SETQ PATH (GETRLIST (AEVAL PATHS2)))
       LAB
        (COND ((NULL PATH) (RETURN NIL)))
        ((LAMBDA (PATH)
           (PROGN
            (SETQ FL (AEVAL 'NIL))
            (PROG (LINK)
              (SETQ LINK (GETRLIST (AEVAL LINKS)))
             LAB
              (COND ((NULL LINK) (RETURN NIL)))
              ((LAMBDA (LINK)
                 (COND
                  ((EVALEQUAL (AEVAL (LIST 'FIRST LINK))
                              (AEVAL (LIST 'LAST PATH)))
                   (PROGN
                    (SETQ FL (AEVAL 'T))
                    (SETQ FL2 (AEVAL 'T))
                    (SETQ PATHS2
                            (AEVAL
                             (LIST 'APPEND PATHS2
                                   (LIST 'LIST
                                         (LIST 'APPEND PATH
                                               (LIST 'LIST
                                                     (LIST 'SECOND
                                                           LINK)))))))))))
               (CAR LINK))
              (SETQ LINK (CDR LINK))
              (GO LAB))
            (COND
             ((NOT (BOOLVALUE* FL))
              (SETQ PATHS (AEVAL (LIST 'APPEND PATHS (LIST 'LIST PATH))))))
            (SETQ PATHS2 (AEVAL (LIST 'DELETE PATH PATHS2)))))
         (CAR PATH))
        (SETQ PATH (CDR PATH))
        (GO LAB))
      (COND ((BOOLVALUE* FL2) (GO EXT)))
      (RETURN (AEVAL PATHS)))) 
(FLAG '(DELETE) 'OPFN) 
(PUT 'LAST 'NUMBER-OF-ARGS 1) 
(FLAG '(LAST) 'OPFN) 
(PUT 'LAST 'DEFINED-ON-LINE '539) 
(PUT 'LAST 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'LAST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LAST (X) (LIST 'FIRST (LIST 'REVERSE X))) 
(PUT 'SUBSET1 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSET1 'DEFINED-ON-LINE '541) 
(PUT 'SUBSET1 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'SUBSET1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSET1 (A B)
    (AND (EQUAL (DIFFERENCE (LENGTH B) (LENGTH A)) 1) (SUBSETP A B))) 
(FLAG '(SUBSET1) 'OPFN) 
(PUT 'ROOT 'NUMBER-OF-ARGS 1) 
(FLAG '(ROOT) 'OPFN) 
(PUT 'ROOT 'DEFINED-ON-LINE '546) 
(PUT 'ROOT 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'ROOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOT (N)
    (PROG (TRRT)
      (SETQ TRRT (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) J)) (RETURN NIL)))
        (COND
         ((EVALEQUAL (AEVAL* (LIST 'LENGTH (LIST '|VLL#| J))) 1)
          (SETQ TRRT (AEVAL* (LIST 'APPEND TRRT (LIST 'LIST J))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL TRRT)))) 
(PUT 'PFX 'NUMBER-OF-ARGS 2) 
(FLAG '(PFX) 'OPFN) 
(PUT 'PFX 'DEFINED-ON-LINE '551) 
(PUT 'PFX 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'PFX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PFX (PR FL)
    (PROG (PRF F FF)
      (SETQ PRF (AEVAL (LIST 'TIMES FL (LIST 'EXPT '(|:DN:| 100 . -1) PR))))
      (SETQ FF (AEVAL '(|:DN:| 10 . -1)))
      (SETQ F (AEVAL 0))
      (WHILE
       (EVALGREATERP (AEVAL* (LIST 'TIMES PRF FF)) (AEVAL* '(|:DN:| 10 . -1)))
       (PROGN
        (SETQ FF (AEVAL* (LIST 'QUOTIENT FF 10)))
        (SETQ F (AEVAL* (LIST 'PLUS F 1)))))
      (RETURN (AEVAL F)))) 
(PUT 'ROUNDROOT 'NUMBER-OF-ARGS 2) 
(PUT 'ROUNDROOT 'DEFINED-ON-LINE '557) 
(PUT 'ROUNDROOT 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'ROUNDROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROUNDROOT (A P)
    (PROGN
     (SETQ P
             (LIST 'EQUAL (CADR A)
                   (COND
                    ((EQUAL (CAADDR A) 'MINUS)
                     (LIST 'MINUS (RTRNDA (CADR (CADDR A)) P)))
                    (T (RTRNDA (CADDR A) P)))))
     P)) 
(FLAG '(ROUNDROOT) 'OPFN) 
(PUT 'ELIM 'NUMBER-OF-ARGS 2) 
(FLAG '(ELIM) 'OPFN) 
(PUT 'ELIM 'DEFINED-ON-LINE '565) 
(PUT 'ELIM 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'ELIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIM (A B)
    (PROG (X)
      (PROG (EL)
        (SETQ EL (GETRLIST (AEVAL B)))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND ((NOT (MEMBER (REVALX EL) (REVALX A))) (SETQ X (AEVAL EL)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN (AEVAL X)))) 
(PUT 'COMBINERTS 'NUMBER-OF-ARGS 2) 
(FLAG '(COMBINERTS) 'OPFN) 
(PUT 'COMBINERTS 'DEFINED-ON-LINE '572) 
(PUT 'COMBINERTS 'DEFINED-IN-FILE 'ROOTS/MULTROOT.RED) 
(PUT 'COMBINERTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMBINERTS (R0 R1)
    (PROG (XOUT)
      (SETQ XOUT (AEVAL (LIST 'LIST)))
      (RETURN
       (COND ((EVALEQUAL (AEVAL R1) (AEVAL (LIST 'LIST))) (AEVAL (LIST 'LIST)))
             (T
              (PROGN
               (PROG (RT1)
                 (SETQ RT1 (GETRLIST (AEVAL R1)))
                LAB
                 (COND ((NULL RT1) (RETURN NIL)))
                 ((LAMBDA (RT1)
                    (SETQ XOUT
                            (AEVAL
                             (LIST 'APPEND XOUT
                                   (LIST 'LIST
                                         (LIST 'APPEND R0
                                               (LIST 'LIST RT1)))))))
                  (CAR RT1))
                 (SETQ RT1 (CDR RT1))
                 (GO LAB))
               (AEVAL XOUT))))))) 
(ENDMODULE) 