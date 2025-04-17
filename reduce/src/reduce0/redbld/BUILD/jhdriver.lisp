(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'JHDRIVER)) 
(FLUID
 '(*ALGINT *BACKTRACE *COATES *NOACN *TRA *TRMIN *STRUCTURE
   BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS GAUSSIANI INTVAR LISTOFALLSQRTS
   LISTOFNEWSQRTS PREVIOUSBASIS SQRT-INTVAR SQRTFLAG SQRTS-IN-INTEGRAND
   SQRTS-MOD-PRIME TAYLORASSLIST VARLIST ZLIST)) 
(GLOBAL '(TRYHARDER)) 
(SWITCH (LIST 'ALGINT 'COATES 'NOACN 'TRA 'TRMIN)) 
(EXPORTS (LIST 'ALGEBRAICCASE 'DOALGGEOM 'COATES-MULTIPLE)) 
(SETQ *ALGINT T) 
(PUT 'OPERATEON 'NUMBER-OF-ARGS 2) 
(PUT 'OPERATEON 'DEFINED-ON-LINE '61) 
(PUT 'OPERATEON 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'OPERATEON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OPERATEON (RESLIST X)
    (PROG (U V ANSWER SAVE SQRTS-MOD-PRIME)
      (SETQ U (ZMODULE RESLIST))
      (SETQ V (SETQ ANSWER (CONS NIL 1)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (NOT (ATOM V)))) (RETURN NIL)))
        (PROGN
         (SETQ V (FINDFUNCTION (CDAR U)))
         (COND
          ((NOT (ATOM V))
           (PROGN
            (COND
             ((OR *TRA *TRMIN)
              (PROGN
               (PROGN
                (PRIN2 "Extension logarithm is ")
                (TERPRI)
                "Extension logarithm is ")
               (PRINTSQ V))))
            (SETQ SAVE TRYHARDER)
            (SETQ TRYHARDER X)
            (SETQ V (COMBINE-LOGS (CAAR U) (SIMPLOGSQ V)))
            (SETQ TRYHARDER SAVE)
            (SETQ ANSWER (*ADDSQ ANSWER V))
            (SETQ U (CDR U))))))
        (GO WHILELABEL))
      (COND ((ATOM V) (RETURN V)) (T (RETURN ANSWER))))) 
(PUT 'FINDFUNCTION 'NUMBER-OF-ARGS 1) 
(PUT 'FINDFUNCTION 'DEFINED-ON-LINE '85) 
(PUT 'FINDFUNCTION 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'FINDFUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDFUNCTION (DIVISOR)
    (PROG (V PLACES MULTS ANS DOF1K PREVIOUSBASIS)
      (SETQ DIVISOR
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U DIVISOR)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CORRECT-MULTS U)) (CAR U))
                                      NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (CORRECT-MULTS U)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*COATES (GO NOHACK)))
      (SETQ V (PRECOATES DIVISOR INTVAR NIL))
      (COND ((NOT (ATOM V)) (RETURN V)))
     NOHACK
      (PROG (U)
        (SETQ U DIVISOR)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (SETQ PLACES (CONS (CAR U) PLACES))
            (SETQ MULTS (CONS (CDR U) MULTS))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (SETQ V (COATES PLACES MULTS INTVAR))
      (COND ((NOT (ATOM V)) (RETURN V)))
      (SETQ DOF1K (DIFFERENTIALS-1 (GETSQRTSFROMPLACES PLACES)))
      (COND
       ((NULL DOF1K)
        (INTERR "Must be able to integrate over curves of genus 0")))
      (COND ((NOT (MAZURP PLACES DOF1K)) (GO GENERAL)))
      (SETQ ANS 'PROVABLY-IMPOSSIBLE)
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE 12 I)) (RETURN NIL)))
        (COND
         ((AND (NEQ I 11)
               (NOT (ATOM (SETQ ANS (COATES-MULTIPLE PLACES MULTS I)))))
          (SETQ I 12)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ANS)
     GENERAL
      (SETQ V (FINDMANINPARM PLACES))
      (COND ((NULL V) (RETURN (ALGEBRAIC-DIVISOR DIVISOR DOF1K))))
      (COND
       ((NOT (INTERR "MANINP called -- not implemented in this version"))
        (RETURN 'PROVABLY-IMPOSSIBLE)))
      (SETQ V 1)
     LOOP
      (SETQ V (IADD1 V))
      (COND
       ((NOT (ATOM (SETQ ANS (COATES-MULTIPLE PLACES MULTS V)))) (RETURN ANS)))
      (GO LOOP))) 
(PUT 'CORRECT-MULTS 'NUMBER-OF-ARGS 1) 
(PUT 'CORRECT-MULTS 'DEFINED-ON-LINE '131) 
(PUT 'CORRECT-MULTS 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'CORRECT-MULTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CORRECT-MULTS (U)
    (PROG (MULTIP)
      (SETQ MULTIP (CDR U))
      (PROG (V)
        (SETQ V (CAR U))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (COND
            ((AND (EQ (CAR V) INTVAR) (EQCAR (CDR V) 'EXPT))
             (SETQ MULTIP (TIMES MULTIP (CADDR (CDR V)))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS (CAR U) MULTIP)))) 
(PUT 'ALGEBRAICCASE 'NUMBER-OF-ARGS 3) 
(PUT 'ALGEBRAICCASE 'DEFINED-ON-LINE '142) 
(PUT 'ALGEBRAICCASE 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'ALGEBRAICCASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ALGEBRAICCASE (EXPRESSION ZLIST VARLIST)
    (PROG (RISCHPART DERIV W FIRSTTERM SQRTFLAG *STRUCTURE)
      (SETQ SQRTFLAG T)
      (SQRTSAVE LISTOFALLSQRTS LISTOFNEWSQRTS (LIST (CONS INTVAR INTVAR)))
      (SETQ RISCHPART
              (ERRORSET* (LIST 'DOALGGEOM (MKQUOTE EXPRESSION)) *BACKTRACE))
      (NEWPLACE (LIST (CONS INTVAR INTVAR)))
      (COND
       ((ATOM RISCHPART)
        (PROGN
         (COND
          (*TRA
           (PROGN
            (PRIN2 "Inner integration failed")
            (TERPRI)
            "Inner integration failed")))
         (SETQ DERIV (CONS NIL 1))
         (SETQ RISCHPART DERIV)))
       ((ATOM (CAR RISCHPART))
        (PROGN
         (COND
          ((OR *TRA *TRMIN)
           (PROGN
            (PRIN2 "The 'logarithmic part' is not elementary")
            (TERPRI)
            "The 'logarithmic part' is not elementary")))
         (RETURN (CONS (CONS NIL 1) EXPRESSION))))
       (T
        (PROGN
         (SETQ RISCHPART (CAR RISCHPART))
         ((LAMBDA (SQRTFLAG) (SETQ DERIV (*DIFFSQ RISCHPART INTVAR))) NIL)
         (COND
          ((OR *TRA *TRMIN)
           (PROGN
            (PROGN
             (PRIN2 "Inner working yields")
             (TERPRI)
             "Inner working yields")
            (PRINTSQ RISCHPART)
            (PROGN (PRIN2 "with derivative") (TERPRI) "with derivative")
            (PRINTSQ DERIV)))))))
      (SETQ DERIV (*ADDSQ EXPRESSION (NEGSQ DERIV)))
      (COND ((NULL (CAR DERIV)) (RETURN (CONS RISCHPART (CONS NIL 1)))))
      (COND
       ((NULL (INVOLVESQ DERIV INTVAR))
        (RETURN
         (CONS
          (*ADDSQ RISCHPART
                  (*MULTSQ DERIV
                           (CONS
                            (CONS (CONS (GETPOWER (FKERN INTVAR) 1) 1) NIL)
                            1)))
          (CONS NIL 1)))))
      (SETQ VARLIST (GETVARIABLES DERIV))
      (SETQ ZLIST (FINDZVARS VARLIST (LIST INTVAR) INTVAR NIL))
      (SETQ VARLIST (SETDIFF VARLIST ZLIST))
      (SETQ FIRSTTERM (SIMP* (CAR ZLIST)))
      (SETQ W (SQRT2TOP (*MULTSQ DERIV (INVSQ (*DIFFSQ FIRSTTERM INTVAR)))))
      (COND
       ((NULL (INVOLVESQ W INTVAR))
        (RETURN (CONS (*ADDSQ RISCHPART (*MULTSQ W FIRSTTERM)) (CONS NIL 1)))))
      (COND (*NOACN (INTERR "Testing only logarithmic code")))
      (SETQ DERIV (TRANSCENDENTALCASE DERIV INTVAR NIL ZLIST VARLIST))
      (RETURN (CONS (*ADDSQ (CAR DERIV) RISCHPART) (CDR DERIV))))) 
(PUT 'DOALGGEOM 'NUMBER-OF-ARGS 1) 
(PUT 'DOALGGEOM 'DEFINED-ON-LINE '194) 
(PUT 'DOALGGEOM 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'DOALGGEOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DOALGGEOM (DIFFERENTIAL)
    (PROG (RESLIST PLACE PLACELIST SAVETAYLORASSLIST SQRTS-IN-INTEGRAND
           TAYLORASSLIST)
      (SETQ PLACELIST (FINDPOLES DIFFERENTIAL INTVAR))
      (SETQ RESLIST NIL)
      (SETQ SQRTS-IN-INTEGRAND (SQRTSINSQ DIFFERENTIAL INTVAR))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACELIST) (RETURN NIL)))
        (PROGN
         (SETQ PLACE (CAR PLACELIST))
         (SETQ PLACELIST (CDR PLACELIST))
         (SETQ SAVETAYLORASSLIST TAYLORASSLIST)
         (SETQ PLACE (FIND-RESIDUE DIFFERENTIAL INTVAR PLACE))
         (COND (PLACE (SETQ RESLIST (APPEND PLACE RESLIST)))
               (T (SETQ TAYLORASSLIST SAVETAYLORASSLIST))))
        (GO WHILELABEL))
      (COND (RESLIST (GO SERIOUS)))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRIN2 "No residues => no logs")
         (TERPRI)
         "No residues => no logs")))
      (RETURN (CONS NIL 1))
     SERIOUS
      (SETQ PLACELIST (OPERATEON RESLIST INTVAR))
      (COND ((EQ PLACELIST 'FAILED) (INTERR "Divisor operations failed")))
      (RETURN PLACELIST))) 
(PUT 'ALGEBRAIC-DIVISOR 'NUMBER-OF-ARGS 2) 
(PUT 'ALGEBRAIC-DIVISOR 'DEFINED-ON-LINE '223) 
(PUT 'ALGEBRAIC-DIVISOR 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'ALGEBRAIC-DIVISOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALGEBRAIC-DIVISOR (DIVISOR DOF1K)
    (COND ((EQUAL (LENGTH DOF1K) 1) (LUTZ-NAGELL DIVISOR))
          (T (BOUND-TORSION DIVISOR DOF1K)))) 
(PUT 'COATES-MULTIPLE 'NUMBER-OF-ARGS 3) 
(PUT 'COATES-MULTIPLE 'DEFINED-ON-LINE '229) 
(PUT 'COATES-MULTIPLE 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'COATES-MULTIPLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATES-MULTIPLE (PLACES MULTS V)
    (PROG (ANS)
      (COND
       ((NOT
         (ATOM
          (SETQ ANS
                  (COATES PLACES
                   (PROG (U FORALL-RESULT FORALL-ENDPTR)
                     (SETQ U MULTS)
                     (COND ((NULL U) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (U) (TIMES V U)) (CAR U))
                                           NIL)))
                    LOOPLABEL
                     (SETQ U (CDR U))
                     (COND ((NULL U) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (U) (TIMES V U)) (CAR U)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))
                   INTVAR))))
        (PROGN
         (COND
          ((OR *TRA *TRMIN)
           (PROGN (PRINC "Divisor has order ") (PROGN (PRIN2 V) (TERPRI) V))))
         (RETURN
          (CONS
           (LIST (CONS (GETPOWER (FKERN (LIST 'NTHROOT (MK*SQ ANS) V)) 1) 1))
           1))))
       (T (RETURN ANS))))) 
(PUT 'MAZURP 'NUMBER-OF-ARGS 2) 
(PUT 'MAZURP 'DEFINED-ON-LINE '244) 
(PUT 'MAZURP 'DEFINED-IN-FILE 'ALGINT/JHDRIVER.RED) 
(PUT 'MAZURP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAZURP (PLACES DOF1K)
    (PROG (ANSWER)
      (COND ((NEQ (LENGTH DOF1K) 1) (RETURN NIL)))
      (SETQ ANSWER T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ANSWER PLACES)) (RETURN NIL)))
        (COND
         ((SQRTSINTREE (BASICPLACE (CAR PLACES)) NIL NIL) (SETQ ANSWER NIL))
         (T (SETQ PLACES (CDR PLACES))))
        (GO WHILELABEL))
      (COND ((NULL ANSWER) (RETURN NIL)))
      (COND
       (*TRA
        (PROGN
         (PRIN2 "*** We can apply Mazur's bound on the torsion of")
         (PRIN2T "elliptic curves over the rationals"))))
      (RETURN T))) 
(ENDMODULE) 