(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'COATES)) 
(FLUID
 '(*TRA *TRMIN *GALOIS INTVAR MAGICLIST NESTEDSQRTS PREVIOUSBASIS SQRT-INTVAR
   TAYLORASSLIST THISPLACE LISTOFALLSQRTS LISTOFNEWSQRTS BASIC-LISTOFALLSQRTS
   BASIC-LISTOFNEWSQRTS GAUSSIANI *TRFIELD)) 
(GLOBAL '(COATES-FDI)) 
(EXPORTS (LIST 'COATES 'MAKEINITIALBASIS 'CHECKPOLES 'MULTBYALLCOMBINATIONS)) 
(PUT 'COATES 'NUMBER-OF-ARGS 3) 
(PUT 'COATES 'DEFINED-ON-LINE '40) 
(PUT 'COATES 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'COATES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATES (PLACES MULTS X)
    (PROG (U TT)
      (SETQ TT (TIME))
      (SETQ U (COATES-HPFSD PLACES MULTS))
      (COND
       ((OR *TRA *TRMIN)
        ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
         (LIST 'COATES 'TIME (DIFFERENCE (TIME) TT) 'MILLISECONDS))))
      (RETURN U))) 
(PUT 'COATES-REAL 'NUMBER-OF-ARGS 2) 
(PUT 'COATES-REAL 'DEFINED-ON-LINE '51) 
(PUT 'COATES-REAL 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'COATES-REAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COATES-REAL (PLACES MULTS)
    (PROG (THISPLACE U V SAVE)
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRINC "Find function with zeros of order:")
         (PROGN (PRIN2 MULTS) (TERPRI) MULTS)
         (COND (*TRA (PRINC " at ")))
         (TERPRI* T)
         (COND (*TRA (MAPC PLACES (FUNCTION PRINTPLACE)))))))
      (SETQ V MULTS)
      (PROG (UU)
        (SETQ UU PLACES)
       LAB
        (COND ((NULL UU) (RETURN NIL)))
        ((LAMBDA (UU)
           (PROGN
            (COND ((LESSP (CAR V) 0) (SETQ U (CONS (CDAR UU) U))))
            (SETQ V (CDR V))))
         (CAR UU))
        (SETQ UU (CDR UU))
        (GO LAB))
      (SETQ THISPLACE (LIST 'QUOTIENT 1 INTVAR))
      (COND
       ((MEMBER THISPLACE U)
        (PROGN
         (SETQ V (FINITISE PLACES MULTS))
         (SETQ U (COATES-REAL (CAR V) (CADR V)))
         (COND ((ATOM U) (RETURN U)))
         (RETURN
          (MULTSQ U
                  (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) (CADDR V)) 1))
                        1))))))
      (PROG (UU)
        (SETQ UU PLACES)
       LAB
        (COND ((NULL UU) (RETURN NIL)))
        ((LAMBDA (UU)
           (COND
            ((EQUAL (CDAR UU) THISPLACE)
             (SETQ U (APPEND U (MAPOVERCAR (CDR UU)))))))
         (CAR UU))
        (SETQ UU (CDR UU))
        (GO LAB))
      (SETQ COATES-FDI (FRACTIONAL-DEGREE-AT-INFINITY U))
      (COND
       ((IEQUAL COATES-FDI 1) (RETURN (COATESMODULE PLACES MULTS INTVAR))))
      (COND (*TRA (FDI-PRINT)))
      (NEWPLACE
       (LIST (CONS INTVAR THISPLACE) (LIST INTVAR 'EXPT INTVAR COATES-FDI)))
      (SETQ PLACES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J PLACES)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (FDI-UPGRADE J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (FDI-UPGRADE J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SAVE TAYLORASSLIST)
      (SETQ U
              (COATESMODULE PLACES
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U MULTS)
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U) (TIMES U COATES-FDI)) (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (U) (TIMES U COATES-FDI)) (CAR U))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               INTVAR))
      (SETQ TAYLORASSLIST SAVE)
      (RETURN U))) 
(PUT 'COATESMODULE 'NUMBER-OF-ARGS 3) 
(PUT 'COATESMODULE 'DEFINED-ON-LINE '116) 
(PUT 'COATESMODULE 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'COATESMODULE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATESMODULE (PLACES MULTS X)
    (PROG (PZERO MZERO U V BASIS SQRTS MAGICLIST MPOLE PPOLE)
      (SETQ SQRTS (SQRTSINPLACES PLACES))
      (COND (*TRA (PROGN (PRINC "Sqrts on this curve:") (SUPERPRINT SQRTS))))
      (SETQ U PLACES)
      (SETQ V MULTS)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND
          ((LESSP 0 (CAR V))
           (PROGN
            (SETQ MZERO (CONS (CAR V) MZERO))
            (SETQ PZERO (CONS (CAR U) PZERO))))
          (T
           (PROGN
            (SETQ MPOLE (CONS (CAR V) MPOLE))
            (SETQ PPOLE (CONS (CAR U) PPOLE)))))
         (SETQ U (CDR U))
         (SETQ V (CDR V)))
        (GO WHILELABEL))
      (COND (PREVIOUSBASIS (SETQ BASIS PREVIOUSBASIS))
            (T (SETQ BASIS (MKVEC (MAKEINITIALBASIS PPOLE)))))
      (SETQ U (COMPLETEPLACES PPOLE MPOLE))
      (SETQ BASIS (INTEGRALBASIS BASIS (CAR U) (CDR U) X))
      (SETQ BASIS (NORMALBASIS BASIS X 0))
      (SETQ U (COATESSOLVE MZERO PZERO BASIS NIL))
      (COND ((ATOM U) (RETURN U)))
      (SETQ V (CHECKPOLES (LIST U) PLACES MULTS))
      (COND ((NULL V) (RETURN 'FAILED)))
      (COND ((NOT MAGICLIST) (RETURN U)))
      (SETQ U (REMOVECMSQ (SUBSTITUTESQ U V)))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2 "These values give the function")
          (TERPRI)
          "These values give the function")
         (PRINTSQ U))))
      (SETQ MAGICLIST NIL)
      (COND ((CHECKPOLES (LIST U) PLACES MULTS) (RETURN U))
            (T (INTERR "Inconsistent checkpoles"))))) 
(PUT 'MAKEINITIALBASIS 'NUMBER-OF-ARGS 1) 
(PUT 'MAKEINITIALBASIS 'DEFINED-ON-LINE '166) 
(PUT 'MAKEINITIALBASIS 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'MAKEINITIALBASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKEINITIALBASIS (PLACES)
    (PROG (U)
      (SETQ U
              (MULTBYALLCOMBINATIONS (LIST (CONS 1 1))
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U (GETSQRTSFROMPLACES PLACES))
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U)
                                     (CONS
                                      (LIST (CONS (GETPOWER (FKERN U) 1) 1))
                                      1))
                                   (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (U)
                             (CONS (LIST (CONS (GETPOWER (FKERN U) 1) 1)) 1))
                           (CAR U))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Initial basis for the space m(x)")
          (TERPRI)
          "Initial basis for the space m(x)")
         (MAPC U (FUNCTION PRINTSQ)))))
      (RETURN U))) 
(PUT 'MULTBYALLCOMBINATIONS 'NUMBER-OF-ARGS 2) 
(PUT 'MULTBYALLCOMBINATIONS 'DEFINED-ON-LINE '178) 
(PUT 'MULTBYALLCOMBINATIONS 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'MULTBYALLCOMBINATIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTBYALLCOMBINATIONS (U L)
    (COND ((NULL L) U)
          (T (MULTBYALLCOMBINATIONS (NCONC (MULTSQL (CAR L) U) U) (CDR L))))) 
(PUT 'MULTSQL 'NUMBER-OF-ARGS 2) 
(PUT 'MULTSQL 'DEFINED-ON-LINE '185) 
(PUT 'MULTSQL 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'MULTSQL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTSQL (U L)
    (COND ((NULL L) NIL) (T (CONS (*MULTSQ U (CAR L)) (MULTSQL U (CDR L)))))) 
(PUT 'CHECKPOLES 'NUMBER-OF-ARGS 3) 
(PUT 'CHECKPOLES 'DEFINED-ON-LINE '189) 
(PUT 'CHECKPOLES 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'CHECKPOLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECKPOLES (BASIS PLACES MULTS)
    (PROG (U V L)
      (GO OUTER2)
     OUTER
      (SETQ PLACES (CDR PLACES))
      (SETQ MULTS (CDR MULTS))
     OUTER2
      (COND ((NULL PLACES) (RETURN (COND (MAGICLIST (FINDMAGIC L)) (T T)))))
      (COND ((LEQ 0 (CAR MULTS)) (GO OUTER)))
      (SETQ U BASIS)
     INNER
      (COND
       ((NULL U)
        (PROGN
         (COND
          (*TRA
           (PROGN
            (PRINC "The answer from the linear equations did")
            (PROGN
             (PRIN2 " not have the poles at:")
             (TERPRI)
             " not have the poles at:")
            (PRINTPLACE (CAR PLACES)))))
         (RETURN NIL))))
      (SETQ V (TAYLORFORM (XSUBSTITUTESQ (CAR U) (CAR PLACES))))
      (COND
       ((EQUAL (CAADR V) (CAR MULTS))
        (PROGN
         (COND (MAGICLIST (SETQ L (CONS (TAYLOREVALUATE V (CAR MULTS)) L))))
         (GO OUTER))))
      (COND
       ((LESSP (CAADR V) (CAR MULTS)) (INTERR "Extraneous pole introduced")))
      (SETQ U (CDR U))
      (GO INNER))) 
(PUT 'COATES-HPFSD 'NUMBER-OF-ARGS 2) 
(PUT 'COATES-HPFSD 'DEFINED-ON-LINE '229) 
(PUT 'COATES-HPFSD 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'COATES-HPFSD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COATES-HPFSD (OPLACES OMULTS)
    (PROG (MZERO PZERO MPOLE PPOLE FUN SUMMZERO ANSWER PLACES MULTS)
      (SETQ PLACES OPLACES)
      (SETQ MULTS OMULTS)
      (SETQ SUMMZERO 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (COND
          ((LESSP 0 (CAR MULTS))
           (PROGN
            (SETQ SUMMZERO (PLUS SUMMZERO (CAR MULTS)))
            (SETQ MZERO (CONS (CAR MULTS) MZERO))
            (SETQ PZERO (CONS (CAR PLACES) PZERO))))
          (T
           (PROGN
            (SETQ MPOLE (CONS (CAR MULTS) MPOLE))
            (SETQ PPOLE (CONS (CAR PLACES) PPOLE)))))
         (SETQ PLACES (CDR PLACES))
         (SETQ MULTS (CDR MULTS)))
        (GO WHILELABEL))
      (COND
       ((GREATERP SUMMZERO 2)
        (PROG (NPLACES NMULTS F MULTIPLICITY NEWPOLE SQRTS FZ ZFOUND MULT1)
          (SETQ SQRTS (GETSQRTSFROMPLACES PPOLE))
          (COND
           ((OR *TRA *TRMIN)
            (PROGN
             (PRINC "Operate on divisor:")
             ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) (APPEND MZERO MPOLE))
             (PROGN (PRIN2 "at") (TERPRI) "at")
             (MAPC PZERO (FUNCTION PRINTPLACE))
             (MAPC PPOLE (FUNCTION PRINTPLACE)))))
         ITERATE
          (SETQ NPLACES (LIST (CAR PZERO)))
          (SETQ MULTIPLICITY (CAR MZERO))
          (SETQ NMULTS (LIST 1))
          (COND
           ((CDR PPOLE)
            (PROGN
             (SETQ NPLACES (CONS (CAR PPOLE) (CONS (CADR PPOLE) NPLACES)))
             (SETQ MULTIPLICITY
                     (MIN MULTIPLICITY (MINUS (CAR MPOLE))
                          (MINUS (CADR MPOLE))))
             (SETQ NMULTS (CONS (MINUS 1) (CONS (MINUS 1) NMULTS)))))
           (T
            (PROGN
             (SETQ NPLACES (CONS (CAR PPOLE) NPLACES))
             (SETQ MULTIPLICITY
                     (MIN MULTIPLICITY (QUOTIENT (MINUS (CAR MPOLE)) 2)))
             (SETQ NMULTS (CONS (MINUS 2) NMULTS)))))
          (SETQ PREVIOUSBASIS NIL)
          (SETQ F (COATES-REAL NPLACES NMULTS))
          (COND
           ((ATOM F)
            (PROGN
             (COND
              ((OR *TRA *TRMIN)
               (PROGN
                (PRIN2 "Failure: must try whole divisor")
                (TERPRI)
                "Failure: must try whole divisor")))
             (RETURN (COATES-REAL OPLACES OMULTS)))))
          (SETQ FZ (FINDZEROS F SQRTS 2))
          (SETQ ZFOUND (ASSOC (CAR PZERO) FZ))
          (COND
           ((NOT ZFOUND)
            (INTERR
             (LIST "Didn't seem to find the zero" (CAR PZERO)
                   "we looked for"))))
          (COND
           ((GREATERP (CDR ZFOUND) (CAR MZERO))
            (INTERR "We found too many zeros")))
          (SETQ FZ (DELETE ZFOUND FZ))
          (COND
           ((OR *TRA *TRMIN)
            (PROGN
             (PROGN
              (PRIN2 "Replaced by the pole")
              (TERPRI)
              "Replaced by the pole")
             (COND (FZ (PRETTYPRINT FZ))
                   (T
                    (PROGN
                     (TERPRI)
                     (PRIN2T "The zero we were already looking for"))))
             (PRINC MULTIPLICITY)
             (PROGN (PRIN2 " times") (TERPRI) " times"))))
          (SETQ MULT1
                  (DIFFERENCE (CAR MZERO) (TIMES MULTIPLICITY (CDR ZFOUND))))
          (COND
           ((LESSP MULT1 0)
            (PROGN
             (COND
              (*TRA
               (PROGN
                (PRIN2 "*** A zero has turned into a pole")
                (TERPRI)
                "*** A zero has turned into a pole")))
             (SETQ MULTIPLICITY (QUOTIENT (CAR MZERO) (CDR ZFOUND)))
             (SETQ MULT1 (REMAINDER (CAR MZERO) (CDR ZFOUND)))
             NIL)))
          (COND
           ((EQUAL MULT1 0)
            (PROGN (SETQ MZERO (CDR MZERO)) (SETQ PZERO (CDR PZERO))))
           (T (RPLACA MZERO MULT1)))
          (COND
           ((NULL (CDR PPOLE))
            (PROGN
             (COND
              ((EQUAL (PLUS (CAR MPOLE) (TIMES 2 MULTIPLICITY)) 0)
               (PROGN (SETQ PPOLE (CDR PPOLE)) (SETQ MPOLE (CDR MPOLE))))
              (T (RPLACA MPOLE (PLUS (CAR MPOLE) (TIMES 2 MULTIPLICITY)))))))
           (T
            (PROGN
             (COND
              ((EQUAL (PLUS (CADR MPOLE) MULTIPLICITY) 0)
               (PROGN
                (SETQ PPOLE (CONS (CAR PPOLE) (CDDR PPOLE)))
                (SETQ MPOLE (CONS (CAR MPOLE) (CDDR MPOLE)))))
              (T (RPLACA (CDR MPOLE) (PLUS (CADR MPOLE) MULTIPLICITY))))
             (COND
              ((EQUAL (PLUS (CAR MPOLE) MULTIPLICITY) 0)
               (PROGN (SETQ PPOLE (CDR PPOLE)) (SETQ MPOLE (CDR MPOLE))))
              (T (RPLACA MPOLE (PLUS (CAR MPOLE) MULTIPLICITY)))))))
          (PROG ()
           WHILELABEL
            (COND ((NOT FZ) (RETURN NIL)))
            (PROGN
             (SETQ NEWPOLE (CAAR FZ))
             (SETQ MULT1 (TIMES MULTIPLICITY (CDAR FZ)))
             (COND
              ((MEMBER NEWPOLE PZERO)
               (PROG (M P)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (NEQ NEWPOLE (CAR PZERO))) (RETURN NIL)))
                   (PROGN
                    (SETQ M (CONS (CAR MZERO) M))
                    (SETQ MZERO (CDR MZERO))
                    (SETQ P (CONS (CAR PZERO) P))
                    (SETQ PZERO (CDR PZERO)))
                   (GO WHILELABEL))
                 (COND
                  ((LESSP MULT1 (CAR MZERO))
                   (PROGN
                    (SETQ MZERO
                            (CONS (DIFFERENCE (CAR MZERO) MULT1) (CDR MZERO)))
                    (SETQ MZERO (NCONC M MZERO))
                    (SETQ PZERO (NCONC P PZERO))
                    (RETURN NIL))))
                 (COND
                  ((GREATERP MULT1 (CAR MZERO))
                   (PROGN
                    (SETQ PPOLE (CONS NEWPOLE PPOLE))
                    (SETQ MPOLE (CONS (DIFFERENCE (CAR MZERO) MULT1) MPOLE)))))
                 (SETQ MZERO (NCONC M (CDR MZERO)))
                 (SETQ PZERO (NCONC P (CDR PZERO)))))
              ((MEMBER NEWPOLE PPOLE)
               (PROG (M P)
                 (SETQ M MPOLE)
                 (SETQ P PPOLE)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (NEQ NEWPOLE (CAR P))) (RETURN NIL)))
                   (PROGN (SETQ P (CDR P)) (SETQ M (CDR M)))
                   (GO WHILELABEL))
                 (RPLACA M (DIFFERENCE (CAR M) MULT1))))
              (T
               (PROGN
                (SETQ MPOLE (NCONC MPOLE (LIST (MINUS MULT1))))
                (SETQ PPOLE (NCONC PPOLE (LIST NEWPOLE))))))
             (SETQ FZ (CDR FZ)))
            (GO WHILELABEL))
          (SETQ F (MK*SQ F))
          (COND
           ((GREATERP MULTIPLICITY 1)
            (SETQ ANSWER (CONS (LIST 'EXPT F MULTIPLICITY) ANSWER)))
           (T (SETQ ANSWER (CONS F ANSWER))))
          (SETQ SUMMZERO 0)
          (PROG (X)
            (SETQ X MZERO)
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X) (SETQ SUMMZERO (PLUS SUMMZERO X))) (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (COND
           (*TRA
            (PROGN
             (PRINC "Function is now: ")
             ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) (APPEND MZERO MPOLE))
             (PROGN (PRIN2 "at") (TERPRI) "at")
             (MAPC PZERO (FUNCTION PRINTPLACE))
             (MAPC PPOLE (FUNCTION PRINTPLACE)))))
          (COND ((GREATERP SUMMZERO 2) (GO ITERATE))))))
      (COND
       ((OR PZERO PPOLE)
        (PROGN
         (SETQ FUN (COATES-REAL (NCONC PZERO PPOLE) (NCONC MZERO MPOLE)))
         (COND ((NULL ANSWER) (RETURN FUN))
               (T (SETQ ANSWER (CONS (MK*SQ FUN) ANSWER)))))))
      (RETURN (CONS (LIST (CONS (CONS (CONS 'TIMES ANSWER) 1) 1)) 1)))) 
(PUT 'FINDZEROS 'NUMBER-OF-ARGS 3) 
(PUT 'FINDZEROS 'DEFINED-ON-LINE '391) 
(PUT 'FINDZEROS 'DEFINED-IN-FILE 'ALGINT/COATES.RED) 
(PUT 'FINDZEROS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FINDZEROS (SQ SQRTS NZEROS)
    (PROG (U POTENTIALS ANSWER N NOT-ANSWER NZ SERIES)
      (SETQ U (CDR (SQRT2TOP (INVSQ SQ))))
      (SETQ POTENTIALS
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (JFACTOR U INTVAR))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (PROG (W PLACE)
                                      (SETQ W (MAKEMAINVAR (CAR V) INTVAR))
                                      (COND
                                       ((NEQ (CDAAR W) 1)
                                        (INTERR "Can't cope")))
                                      (COND
                                       ((CDR W)
                                        (SETQ PLACE
                                                (LIST INTVAR 'PLUS INTVAR
                                                      (PREPSQ
                                                       (CONS (NEGF (CDR W))
                                                             (CDAR W))))))
                                       (T (SETQ PLACE (CONS INTVAR INTVAR))))
                                      (RETURN PLACE)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V)
                            (PROG (W PLACE)
                              (SETQ W (MAKEMAINVAR (CAR V) INTVAR))
                              (COND ((NEQ (CDAAR W) 1) (INTERR "Can't cope")))
                              (COND
                               ((CDR W)
                                (SETQ PLACE
                                        (LIST INTVAR 'PLUS INTVAR
                                              (PREPSQ
                                               (CONS (NEGF (CDR W))
                                                     (CDAR W))))))
                               (T (SETQ PLACE (CONS INTVAR INTVAR))))
                              (RETURN PLACE)))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ POTENTIALS (CONS (LIST INTVAR 'QUOTIENT 1 INTVAR) POTENTIALS))
      (PROG (PLACE)
        (SETQ PLACE POTENTIALS)
       LAB
        (COND ((NULL PLACE) (RETURN NIL)))
        ((LAMBDA (PLACE)
           (PROG (SLIST NESTEDSQRTS)
             (SETQ PLACE (LIST PLACE))
             (NEWPLACE PLACE)
             (SETQ U (SUBSTITUTESQ SQ PLACE))
             (PROG ()
              WHILELABEL
               (COND ((NOT (INVOLVESQ U SQRT-INTVAR)) (RETURN NIL)))
               (PROG (Z)
                 (SETQ Z (LIST (LIST INTVAR 'EXPT INTVAR 2)))
                 (SETQ PLACE (NCONC PLACE Z))
                 (NEWPLACE PLACE)
                 (SETQ U (SUBSTITUTESQ U Z)))
               (GO WHILELABEL))
             (SETQ SLIST (SQRTSINSQ U INTVAR))
             (PROG (V)
               (SETQ V SQRTS)
              LAB
               (COND ((NULL V) (RETURN NIL)))
               ((LAMBDA (V)
                  (SETQ SLIST
                          (UNION SLIST
                                 (SQRTSINSQ
                                  (XSUBSTITUTESQ
                                   (CONS (LIST (CONS (GETPOWER (FKERN V) 1) 1))
                                         1)
                                   PLACE)
                                  INTVAR))))
                (CAR V))
               (SETQ V (CDR V))
               (GO LAB))
             (SETQ SLIST (SQRTSIGN SLIST INTVAR))
             (PROG (S)
               (SETQ S SLIST)
              LAB
               (COND ((NULL S) (RETURN NIL)))
               ((LAMBDA (S)
                  (COND
                   ((GREATERP
                     (SETQ N
                             (CAADR
                              (SETQ SERIES (TAYLORFORM (SUBSTITUTESQ U S)))))
                     0)
                    (SETQ ANSWER (CONS (CONS (APPEND PLACE S) N) ANSWER)))
                   (T
                    (SETQ NOT-ANSWER
                            (CONS (LIST U PLACE S SERIES) NOT-ANSWER)))))
                (CAR S))
               (SETQ S (CDR S))
               (GO LAB))
             (RETURN ANSWER)))
         (CAR PLACE))
        (SETQ PLACE (CDR PLACE))
        (GO LAB))
      (SETQ NZ
              (PROG (U FORALL-RESULT)
                (SETQ U ANSWER)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (U) (CDR U)) (CAR U)) FORALL-RESULT))
                (SETQ U (CDR U))
                (GO LAB1)))
      (COND ((EQUAL NZ NZEROS) (RETURN ANSWER)))
      (COND ((GREATERP NZ NZEROS) (INTERR "We have too many zeros")))
      (COND
       (*TRA
        (PROGN
         (PRIN2 "Couldn't find enough zeros of the function: try harder")
         (TERPRI)
         "Couldn't find enough zeros of the function: try harder")))
      (PROG (V)
        (SETQ V NOT-ANSWER)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROG (*GALOIS SQRTSAVELIST SUBLIST S)
             (SETQ TAYLORASSLIST NIL)
             (SETQ *GALOIS T)
             (SETQ SQRTSAVELIST (CONS LISTOFALLSQRTS LISTOFNEWSQRTS))
             (SETQ LISTOFNEWSQRTS (LIST (CAAAR GAUSSIANI)))
             (SETQ LISTOFALLSQRTS
                     (LIST (CONS (CADR (CAAAR GAUSSIANI)) GAUSSIANI)))
             (SETQ SERIES (CADDDR V))
             (SETQ S (CDR (ASSOC (CAADR SERIES) (CDDR SERIES))))
             (PROG (U)
               (SETQ U (SORTSQRTS (SQRTSINSQ S NIL) NIL))
              LAB
               (COND ((NULL U) (RETURN NIL)))
               ((LAMBDA (U)
                  (PROG (V UU)
                    (SETQ UU (*Q2F (SIMP (CADR U))))
                    (SETQ V (ACTUALSIMPSQRT UU))
                    (SETQ LISTOFALLSQRTS (CONS (CONS UU V) LISTOFALLSQRTS))
                    (COND
                     ((OR (OR (ATOM V) (ATOM (CAR V))) (NEQ (CAAAR V) U))
                      (PROGN
                       (COND
                        ((OR *TRA *TRFIELD)
                         (PROGN
                          (PROGN (PRIN2 U) (TERPRI) U)
                          (PROGN
                           (PRIN2 "re-expressed as")
                           (TERPRI)
                           "re-expressed as")
                          (PRINTSF V))))
                       (SETQ BASIC-LISTOFNEWSQRTS
                               (UNION (SQRTSINSF V NIL NIL)
                                      BASIC-LISTOFNEWSQRTS))
                       (SETQ BASIC-LISTOFALLSQRTS
                               (CONS (CAR LISTOFALLSQRTS)
                                     BASIC-LISTOFALLSQRTS))
                       (SETQ V (PREPF V))
                       (SETQ SUBLIST (CONS (CONS U V) SUBLIST))
                       (SETQ SQRTSAVELIST
                               (CONS
                                (CONS (CAR LISTOFALLSQRTS)
                                      (DELETE (ASSOC UU (CAR SQRTSAVELIST))
                                              (CAR SQRTSAVELIST)))
                                (DELETE U (CDR SQRTSAVELIST)))))))))
                (CAR U))
               (SETQ U (CDR U))
               (GO LAB))
             (SETQ LISTOFALLSQRTS (CAR SQRTSAVELIST))
             (SETQ LISTOFNEWSQRTS (CDR SQRTSAVELIST))
             (COND
              ((AND SUBLIST (NULL (CAR (SUBSTITUTESQ S SUBLIST))))
               (PROGN
                (COND
                 ((OR *TRA *TRFIELD)
                  (PROGN
                   (PRIN2 "a non-zero term has become zero")
                   (TERPRI)
                   "a non-zero term has become zero")))
                (SETQ *GALOIS NIL)
                (COND
                 ((GREATERP
                   (SETQ N
                           (CAADR
                            (TAYLORFORM (SUBSTITUTESQ (CAR V) (CADDR V)))))
                   0)
                  (PROGN
                   (SETQ ANSWER
                           (CONS (CONS (APPEND (CADR V) (CADDR V)) N) ANSWER))
                   (SETQ NZ (PLUS NZ N))
                   (COND
                    ((OR *TRA *TRFIELD)
                     (PROGN
                      (PRIN2 "that found us a new zero of the function")
                      (TERPRI)
                      "that found us a new zero of the function")))))))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND ((EQUAL NZ NZEROS) (RETURN ANSWER)))
      (INTERR "can't find enough zeros"))) 
(ENDMODULE) 