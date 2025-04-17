(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FINDRES)) 
(FLUID
 '(*GCD *TRA *TRMIN BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS INTVAR
   LISTOFALLSQRTS LISTOFNEWSQRTS NESTEDSQRTS SQRT-INTVAR TAYLORVARIABLE)) 
(EXPORTS (LIST 'FIND-RESIDUE 'FINDPOLES)) 
(IMPORTS (LIST 'SQRT2TOP 'JFACTOR 'PREPSQ 'PRINTPLACE 'SIMPDF 'INVOLVESF 'SIMP)) 
(IMPORTS
 (LIST 'STT 'INTERR 'MKSP 'NEGF 'MULTF 'ADDF 'LET2 'SUBSTITUTESQ 'SUBS2Q
       'QUOTF)) 
(IMPORTS (LIST 'PRINTSQ 'CLEAR 'TAYLORFORM 'TAYLOREVALUATE 'INVOLVESF '*MULTSQ)) 
(IMPORTS (LIST 'SQRTSAVE 'SQRTSINSQ 'SQRTSIGN)) 
(PUT 'FIND-RESIDUE 'NUMBER-OF-ARGS 3) 
(PUT 'FIND-RESIDUE 'DEFINED-ON-LINE '49) 
(PUT 'FIND-RESIDUE 'DEFINED-IN-FILE 'ALGINT/FINDRES.RED) 
(PUT 'FIND-RESIDUE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-RESIDUE (SIMPDL X PLACE)
    (PROG (DERIV NSD POSS SLIST)
      (SETQ LISTOFALLSQRTS BASIC-LISTOFALLSQRTS)
      (SETQ LISTOFNEWSQRTS BASIC-LISTOFNEWSQRTS)
      (SETQ DERIV (SIMPDF (LIST PLACE X)))
      (COND
       ((INVOLVESF (CAR DERIV) INTVAR)
        (RETURN (RESIDUES-AT-NEW-POINT SIMPDL X PLACE))))
      (COND
       ((AND (EQCAR PLACE 'QUOTIENT) (IEQUAL (CADR PLACE) 1))
        (GO PLACE-CORRECT)))
      (SETQ PLACE (SIMP (LIST 'DIFFERENCE INTVAR PLACE)))
      (COND ((INVOLVESQ PLACE INTVAR) (INTERR "Place wrongly formatted")))
      (SETQ PLACE (LIST 'PLUS INTVAR (PREPSQ PLACE)))
     PLACE-CORRECT
      (COND
       ((AND (EQ (CAR PLACE) 'PLUS) (EQUAL (CADDR PLACE) 0))
        (SETQ PLACE (LIST (CONS X X))))
       (T (SETQ PLACE (LIST (CONS X PLACE)))))
      (SETQ NSD (SUBSTITUTESQ SIMPDL PLACE))
      (SETQ DERIV (*MULTSQ NSD DERIV))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Differential after first substitution is ")
          (TERPRI)
          "Differential after first substitution is ")
         (PRINTSQ DERIV))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (INVOLVESQ DERIV SQRT-INTVAR)) (RETURN NIL)))
        (PROGN
         (SQRTSAVE BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS PLACE)
         (SETQ NSD (LIST (LIST X 'EXPT X 2)))
         (SETQ DERIV
                 (*MULTSQ (SUBSTITUTESQ DERIV NSD)
                          (CONS (LIST (CONS (GETPOWER (FKERN X) 1) 1)) 1)))
         (SETQ PLACE (NCONC PLACE NSD)))
        (GO WHILELABEL))
      (SETQ NESTEDSQRTS NIL)
      (SETQ SLIST (SQRTSINSQ DERIV X))
      (COND
       ((AND *TRA NESTEDSQRTS)
        (PROGN
         (PRIN2 "We have nested square roots")
         (TERPRI)
         "We have nested square roots")))
      (SETQ SLIST (SQRTSIGN SLIST INTVAR))
      (SETQ TAYLORVARIABLE X)
      (PROG (BRANCH)
        (SETQ BRANCH SLIST)
       LAB
        (COND ((NULL BRANCH) (RETURN NIL)))
        ((LAMBDA (BRANCH)
           (PROGN
            (SETQ NSD
                    (TAYLOREVALUATE (TAYLORFORM (SUBSTITUTESQ DERIV BRANCH))
                     (MINUS 1)))
            (COND
             ((CAR NSD)
              (SETQ POSS
                      (CONS (CONS (APPEND PLACE BRANCH) (SQRT2TOP NSD))
                            POSS))))))
         (CAR BRANCH))
        (SETQ BRANCH (CDR BRANCH))
        (GO LAB))
      (SETQ POSS (REVERSIP POSS))
      (COND ((NULL POSS) (GO FINISHED)))
      (COND
       (*TRA
        (PROGN
         (PRINC "Residues at ")
         (PRINTPLACE PLACE)
         (PROGN (PRIN2 " are ") (TERPRI) " are ")
         (MAPC POSS
               (FUNCTION
                (LAMBDA (U)
                  (PROGN (PRINTPLACE (CAR U)) (PRINTSQ (CDR U)))))))))
     FINISHED
      (SQRTSAVE BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS PLACE)
      (RETURN POSS))) 
(PUT 'RESIDUES-AT-NEW-POINT 'NUMBER-OF-ARGS 3) 
(PUT 'RESIDUES-AT-NEW-POINT 'DEFINED-ON-LINE '115) 
(PUT 'RESIDUES-AT-NEW-POINT 'DEFINED-IN-FILE 'ALGINT/FINDRES.RED) 
(PUT 'RESIDUES-AT-NEW-POINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESIDUES-AT-NEW-POINT (FUNC X PLACE)
    (PROG (PLACE2 TEMPVAR TOPTERM A B XX)
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Find residues at all roots of")
          (TERPRI)
          "Find residues at all roots of")
         (SUPERPRINT PLACE))))
      (SETQ PLACE2 (CAR (SIMP PLACE)))
      (SETQ TOPTERM (STT PLACE2 X))
      (COND ((EQUAL (CAR TOPTERM) 0) (INTERR "Why are we here?")))
      (SETQ TEMPVAR (GENSYM))
      (SETQ PLACE2
              (ADDF PLACE2
                    ((LAMBDA (G543 G544)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G543 G544))
                             (T (POLY-MULTF G543 G544))))
                     (LIST (CONS (GETPOWER (FKERN X) (CAR TOPTERM)) 1))
                     (NEGF (CDR TOPTERM)))))
      (LET2 (LIST 'EXPT TEMPVAR (CAR TOPTERM))
            (SUBST TEMPVAR X (PREPSQ (CONS PLACE2 (CDR TOPTERM)))) NIL T)
      (SETQ PLACE2 (LIST (LIST X 'PLUS X TEMPVAR)))
      (SETQ *GCD NIL)
      (SETQ FUNC (SUBS2Q (SUBSTITUTESQ FUNC PLACE2)))
      (SETQ *GCD T)
      (SETQ XX (LIST (CONS (CONS X 1) 1)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (SETQ A ((LAMBDA (*EXP) (QUOTF1 (CAR FUNC) XX)) T))
                (SETQ B ((LAMBDA (*EXP) (QUOTF1 (CDR FUNC) XX)) T))))
          (RETURN NIL)))
        (SETQ FUNC (CONS A B))
        (GO WHILELABEL))
      (COND
       (*TRA
        (PROGN
         (PROGN (PRIN2 "which gives rise to ") (TERPRI) "which gives rise to ")
         (PRINTSQ FUNC))))
      (COND ((NULL A) (SETQ B ((LAMBDA (*EXP) (QUOTF1 (CDR FUNC) XX)) T))))
      (COND (B (GO HARD)))
      (COND
       (*TRA
        (PROGN
         (PRIN2 "There were no residues")
         (TERPRI)
         "There were no residues")))
      (CLEAR (LIST TEMPVAR))
      (RETURN NIL)
     HARD
      (SETQ TAYLORVARIABLE X)
      (SETQ FUNC (TAYLOREVALUATE (TAYLORFORM FUNC) (MINUS 1)))
      (COND (*TRA (PRINTSQ FUNC)))
      (INTERR "so far"))) 
(PUT 'FINDPOLES 'NUMBER-OF-ARGS 2) 
(PUT 'FINDPOLES 'DEFINED-ON-LINE '164) 
(PUT 'FINDPOLES 'DEFINED-IN-FILE 'ALGINT/FINDRES.RED) 
(PUT 'FINDPOLES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDPOLES (SIMPDL X)
    (PROG (SIMPDL2 POLES)
      (SETQ SIMPDL2 (SQRT2TOP SIMPDL))
      (SETQ POLES (JFACTOR (CDR SIMPDL2) X))
      (SETQ POLES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J POLES)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ POLES (CONS (LIST 'QUOTIENT 1 X) POLES))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2 "Places at which poles could occur ")
          (TERPRI)
          "Places at which poles could occur ")
         (PROG (U)
           (SETQ U POLES)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U) (PRINTPLACE (LIST (CONS INTVAR U)))) (CAR U))
           (SETQ U (CDR U))
           (GO LAB)))))
      (RETURN POLES))) 
(ENDMODULE) 