(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PPSOLN)) 
(FLUID '(*COMPLEX *MSG *NUMVAL *PPSOLN)) 
(GLOBAL '(BFONE*)) 
(SETQ *PPSOLN T) 
(PUT 'SOLVE-FRACTIONAL-POWER 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVE-FRACTIONAL-POWER 'DEFINED-ON-LINE '36) 
(PUT 'SOLVE-FRACTIONAL-POWER 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'SOLVE-FRACTIONAL-POWER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-FRACTIONAL-POWER (U X VAR MU)
    (PROG (V W Z)
      (SETQ V (SIMP* (CAR U)))
      (SETQ W (SIMP* (CADR U)))
      (SETQ Z
              (SOLVESQ
               (SUBS2 (ADDSQ (EXPTSQ V (CAR W)) (NEGSQ (EXPTSQ X (CDR W)))))
               VAR MU))
      (SETQ W (ADDSQ (SIMP (CONS 'EXPT U)) (NEGSQ X)))
      (SETQ Z (CHECK-SOLNS Z (CAR W) VAR))
      (RETURN (COND ((EQ Z 'UNSOLVED) (MKROOTSOF W VAR MU)) (T Z))))) 
(PUT 'PRINCIPLE-OF-POWERS-SOLN 'NUMBER-OF-ARGS 4) 
(PUT 'PRINCIPLE-OF-POWERS-SOLN 'DEFINED-ON-LINE '51) 
(PUT 'PRINCIPLE-OF-POWERS-SOLN 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'PRINCIPLE-OF-POWERS-SOLN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINCIPLE-OF-POWERS-SOLN (EX X1 VAR MU)
    (PROG (Z)
     A
      (COND ((NULL X1) (RETURN 'UNSOLVED))
            ((AND (SUITABLE-EXPT (CAR X1))
                  (NOT
                   (EQ (SETQ Z (PR-POW-SOLN1 EX (CAR X1) VAR MU)) 'UNSOLVED)))
             (RETURN Z)))
      (SETQ X1 (CDR X1))
      (GO A))) 
(PUT 'PR-POW-SOLN1 'NUMBER-OF-ARGS 4) 
(PUT 'PR-POW-SOLN1 'DEFINED-ON-LINE '63) 
(PUT 'PR-POW-SOLN1 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'PR-POW-SOLN1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PR-POW-SOLN1 (EX Y VAR MU)
    (PROG (OLDKORD Z)
      (SETQ OLDKORD (UPDKORDER Y))
      (SETQ Z (REORDER EX))
      (SETKORDER OLDKORD)
      (COND ((NEQ (CDAAR Z) 1) (RETURN 'UNSOLVED)))
      (SETQ Z (COEFLIS Z))
      (COND
       ((OR (NEQ (LENGTH Z) 2) (NEQ (CAAR Z) 0))
        (ERRACH (LIST "solve confused" EX Z))))
      (SETQ Z
              (EXPTSQ
               (MULTSQ (NEGSQ (CONS (CDAR Z) 1)) (INVSQ (CONS (CDADR Z) 1)))
               (CADDR (CADDR Y))))
      (SETQ Z (SOLVESQ (SUBS2 (ADDSQ (SIMP* (CADR Y)) (NEGSQ Z))) VAR MU))
      (SETQ Z (CHECK-SOLNS Z EX VAR))
      (RETURN Z))) 
(PUT 'CHECK-SOLNS 'NUMBER-OF-ARGS 3) 
(PUT 'CHECK-SOLNS 'DEFINED-ON-LINE '79) 
(PUT 'CHECK-SOLNS 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'CHECK-SOLNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECK-SOLNS (Z EX VAR)
    (PROG (X Y)
      (COND
       ((NOT
         (ERRORP
          (SETQ X
                  (ERRORSET2
                   (LIST 'CHECK-SOLNS1 (MKQUOTE Z) (MKQUOTE EX)
                         (MKQUOTE VAR))))))
        (RETURN (CAR X)))
       ((OR
         (EQUAL EX (SETQ Y ((LAMBDA (*REDUCED) (CAR (SIMP* (PREPF EX)))) T)))
         (ERRORP
          (SETQ X
                  (ERRORSET2
                   (LIST 'CHECK-SOLNS1 (MKQUOTE Z) (MKQUOTE Y)
                         (MKQUOTE VAR))))))
        (RETURN 'UNSOLVED))
       (T (RETURN (CAR X)))))) 
(PUT 'CHECK-SOLNS1 'NUMBER-OF-ARGS 3) 
(PUT 'CHECK-SOLNS1 'DEFINED-ON-LINE '95) 
(PUT 'CHECK-SOLNS1 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'CHECK-SOLNS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECK-SOLNS1 (Z EX VAR)
    (PROG (X Y FV SX VS)
      (SETQ FV (FREEVARL EX VAR))
      (PROG (Z1)
        (SETQ Z1 Z)
       LAB
        (COND ((NULL Z1) (RETURN NIL)))
        ((LAMBDA (Z1)
           (SETQ FV
                   (UNION FV
                          (UNION (FREEVARL (CAR (CAAR Z1)) VAR)
                                 (FREEVARL (CDR (CAAR Z1)) VAR)))))
         (CAR Z1))
        (SETQ Z1 (CDR Z1))
        (GO LAB))
      (SETQ FV (DELETE 'I FV))
      (COND
       (FV
        (PROG (V)
          (SETQ V FV)
         LAB
          (COND ((NULL V) (RETURN NIL)))
          ((LAMBDA (V)
             (COND
              ((NOT (FLAGP V 'CONSTANT))
               (SETQ VS
                       (CONS
                        (CONS V (LIST 'QUOTIENT (PLUS 1 (RANDOM 999)) 1000))
                        VS)))))
           (CAR V))
          (SETQ V (CDR V))
          (GO LAB))))
      (SETQ SX (COND (VS (CAR (SUBF EX VS))) (T EX)))
      (PROG ()
       WHILELABEL
        (COND ((NOT Z) (RETURN NIL)))
        (COND
         ((OR (NULL (CADAR Z))
              (ERRORP
               (SETQ Y
                       (ERRORSET2
                        (LIST 'CHECK-SOLNS2 (MKQUOTE EX) (MKQUOTE Z))))))
          (PROGN (SETQ Z NIL) (SETQ X 'UNSOLVED)))
         ((OR (NULL (SETQ Y (CAR Y)))
              (AND FV
                   (NULL
                    (SETQ Y
                            (CAR
                             (SUBF SX
                                   (LIST
                                    (CONS (CAADAR Z)
                                          (MK*SQ (SUBSQ (CAAAR Z) VS)))))))))
              (NULL (NUMVALUE Y)))
          (PROGN (SETQ X (CONS (CAR Z) X)) (SETQ Z (CDR Z))))
         (T (SETQ Z (CDR Z))))
        (GO WHILELABEL))
      (RETURN (COND ((NULL X) 'UNSOLVED) (T X))))) 
(PUT 'CHECK-SOLNS2 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK-SOLNS2 'DEFINED-ON-LINE '124) 
(PUT 'CHECK-SOLNS2 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'CHECK-SOLNS2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK-SOLNS2 (EX Z)
    (COND ((SMEMQ 'ROOT_OF Z) (REDERR 'CHECK-SOLNS))
          (T (CAR (SUBF EX (LIST (CONS (CAADAR Z) (MK*SQ (CAAAR Z))))))))) 
(PUT 'SUITABLE-EXPT 'NUMBER-OF-ARGS 1) 
(PUT 'SUITABLE-EXPT 'DEFINED-ON-LINE '131) 
(PUT 'SUITABLE-EXPT 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'SUITABLE-EXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUITABLE-EXPT (U)
    (AND (EQCAR U 'EXPT) (EQCAR (CADDR U) 'QUOTIENT) (EQUAL (CADR (CADDR U)) 1)
         (FIXP (CADDR (CADDR U))))) 
(PUT 'FREEVARL 'NUMBER-OF-ARGS 2) 
(PUT 'FREEVARL 'DEFINED-ON-LINE '135) 
(PUT 'FREEVARL 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'FREEVARL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FREEVARL (EX VAR)
    ((LAMBDA (L)
       (PROGN
        (PROG (K)
          (SETQ K (ALLKERN (LIST (CONS EX 1))))
         LAB
          (COND ((NULL K) (RETURN NIL)))
          ((LAMBDA (K) (SETQ L (UNION L (VARSIFT K VAR)))) (CAR K))
          (SETQ K (CDR K))
          (GO LAB))
        (DELETE VAR L)))
     (COND (VAR (LIST VAR)) (T NIL)))) 
(PUT 'VARSIFT 'NUMBER-OF-ARGS 2) 
(PUT 'VARSIFT 'DEFINED-ON-LINE '140) 
(PUT 'VARSIFT 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'VARSIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VARSIFT (A VAR)
    (COND
     ((ATOM A)
      (COND ((NOT (OR (NULL A) (NUMBERP A) (EQ A VAR))) (LIST A)) (T NIL)))
     ((GET (CAR A) 'DNAME) NIL)
     ((EQ (CAR A) '*SQ) (VARSIFT (PREPSQ (CADR A)) VAR))
     ((OR (MEMQ (CAR A) '(ARBINT ARBCOMPLEX))
          (AND (EQ (GET (CAR A) 'SIMPFN) 'SIMPIDEN) (NOT (SMEMBER VAR A))))
      (LIST A))
     (T
      (PROG (C FORALL-RESULT FORALL-ENDPTR)
        (SETQ C (CDR A))
       STARTOVER
        (COND ((NULL C) (RETURN NIL)))
        (SETQ FORALL-RESULT ((LAMBDA (C) (VARSIFT C VAR)) (CAR C)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ C (CDR C))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL C) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR ((LAMBDA (C) (VARSIFT C VAR)) (CAR C)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ C (CDR C))
        (GO LOOPLABEL))))) 
(PUT 'NUMVALUE 'NUMBER-OF-ARGS 1) 
(PUT 'NUMVALUE 'DEFINED-ON-LINE '151) 
(PUT 'NUMVALUE 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'NUMVALUE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMVALUE (U)
    (PROG (*NUMVAL X C CP P M)
      (SETQ M *MSG)
      (SETQ *MSG NIL)
      (SETQ *NUMVAL T)
      (SETQ C (MEMQ 'I (FREEVARL U NIL)))
      (COND ((SETQ CP *COMPLEX) (OFF (LIST 'COMPLEX))))
      (SETQ X (SETDMODE 'ROUNDED T))
      (SETQ P (PRECISION 10))
      (COND ((EQ X '|:RD:|) (SETQ X 'ROUNDED)))
      (COND (C (ON (LIST 'COMPLEX))))
      (SETQ *MSG M)
      (SETQ U (CAR (SIMP (PREPF U))))
      (SETQ *MSG NIL)
      (COND (C (OFF (LIST 'COMPLEX))))
      (COND (X (SETDMODE X T)) (T (SETDMODE 'ROUNDED NIL)))
      (COND (CP (ON (LIST 'COMPLEX))))
      (PRECISION P)
      (SETQ *MSG M)
      (RETURN
       (COND
        ((OR (AND (EQCAR U '|:RD:|) ((LAMBDA (Z) (NUMVCHK 100 Z)) (ROUND* U)))
             (AND (EQCAR U '|:CR:|)
                  ((LAMBDA (Z) (NUMVCHK 10 Z))
                   (COND ((ATOM (CADR U)) (CADR U))
                         (T (CONS '|:RD:| (CADR U)))))
                  ((LAMBDA (Z) (NUMVCHK 10 Z))
                   (COND ((ATOM (CDDR U)) (CDDR U))
                         (T (CONS '|:RD:| (CDDR U)))))))
         NIL)
        (T U))))) 
(PUT 'NUMVCHK 'NUMBER-OF-ARGS 2) 
(PUT 'NUMVCHK 'DEFINED-ON-LINE '175) 
(PUT 'NUMVCHK 'DEFINED-IN-FILE 'SOLVE/PPSOLN.RED) 
(PUT 'NUMVCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUMVCHK (FACT Z)
    (COND ((ATOM Z) (LESSP (TIMES FACT (ABS Z)) 1))
          (T
           (|LESSP:|
            (NORMBF
             (|ROUND:MT|
              (|TIMES:|
               (COND ((FLOATP FACT) (FL2BF FACT))
                     (T
                      (NORMBF
                       (COND ((NOT (ATOM FACT)) FACT)
                             ((FIXP FACT) (CONS '|:RD:| (CONS FACT 0)))
                             (T (|READ:NUM| FACT))))))
               (|ABS:| Z))
              |:BPREC:|))
            BFONE*)))) 
(ENDMODULE) 