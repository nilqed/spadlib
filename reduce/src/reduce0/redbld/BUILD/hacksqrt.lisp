(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HACKSQRT)) 
(FLUID '(NESTEDSQRTS THISPLACE)) 
(EXPORTS (LIST 'SQRTSINTREE 'SQRTSINSQ 'SQRTSINSQL 'SQRTSINSF 'SQRTSIGN)) 
(EXPORTS (LIST 'DEGREENEST 'SORTSQRTS)) 
(IMPORTS (LIST 'MKVECT 'INTERR 'GETV 'DEPENDSP 'UNION)) 
(PUT 'SQRTSINTREE 'NUMBER-OF-ARGS 3) 
(PUT 'SQRTSINTREE 'DEFINED-ON-LINE '37) 
(PUT 'SQRTSINTREE 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SQRTSINTREE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SQRTSINTREE (U VAR SLIST)
    (COND ((ATOM U) SLIST)
          ((EQ (CAR U) '*SQ) (UNION SLIST (SQRTSINSQ (CADR U) VAR)))
          ((EQ (CAR U) 'SQRT)
           (COND
            ((DEPENDSP (CADR U) VAR)
             (PROGN
              (SETQ SLIST (SQRTSINTREE (CADR U) VAR SLIST))
              (COND ((MEMBER U SLIST) SLIST) (T (CONS U SLIST)))))
            (T SLIST)))
          (T (SQRTSINTREE (CAR U) VAR (SQRTSINTREE (CDR U) VAR SLIST))))) 
(PUT 'SQRTSINSQ 'NUMBER-OF-ARGS 2) 
(PUT 'SQRTSINSQ 'DEFINED-ON-LINE '55) 
(PUT 'SQRTSINSQ 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SQRTSINSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQRTSINSQ (U VAR) (SQRTSINSF (CDR U) (SQRTSINSF (CAR U) NIL VAR) VAR)) 
(PUT 'SQRTSINSQL 'NUMBER-OF-ARGS 2) 
(PUT 'SQRTSINSQL 'DEFINED-ON-LINE '60) 
(PUT 'SQRTSINSQL 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SQRTSINSQL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQRTSINSQL (U VAR)
    (COND ((NULL U) NIL)
          (T
           (SQRTSINSF (CDR (CAR U))
            (SQRTSINSF (CAR (CAR U)) (SQRTSINSQL (CDR U) VAR) VAR) VAR)))) 
(PUT 'SQRTSINSF 'NUMBER-OF-ARGS 3) 
(PUT 'SQRTSINSF 'DEFINED-ON-LINE '68) 
(PUT 'SQRTSINSF 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SQRTSINSF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SQRTSINSF (U SLIST VAR)
    (COND ((OR (OR (ATOM U) (ATOM (CAR U))) (NULL U)) SLIST)
          (T
           (PROGN
            (COND
             ((AND (EQCAR (CAAAR U) 'SQRT) (DEPENDSP (CADR (CAAAR U)) VAR)
                   (NOT (MEMBER (CAAAR U) SLIST)))
              (PROG (SLIST2)
                (SETQ SLIST2 (SQRTSINTREE (CADR (CAAAR U)) VAR NIL))
                (COND
                 (SLIST2
                  (PROGN
                   (SETQ NESTEDSQRTS T)
                   (SETQ SLIST (UNION SLIST2 SLIST)))))
                (SETQ SLIST (CONS (CAAAR U) SLIST)))))
            (SQRTSINSF (CDAR U) (SQRTSINSF (CDR U) SLIST VAR) VAR))))) 
(PUT 'EASYSQRTSIGN 'NUMBER-OF-ARGS 2) 
(PUT 'EASYSQRTSIGN 'DEFINED-ON-LINE '88) 
(PUT 'EASYSQRTSIGN 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'EASYSQRTSIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EASYSQRTSIGN (SLIST THINGS)
    (COND ((NULL SLIST) THINGS)
          (T
           (EASYSQRTSIGN (CDR SLIST)
            (NCONC (MAPCONS THINGS (CONS (CAR SLIST) (CAR SLIST)))
                   (MAPCONS THINGS (LIST (CAR SLIST) 'MINUS (CAR SLIST)))))))) 
(PUT 'HARDSQRTSIGN 'NUMBER-OF-ARGS 2) 
(PUT 'HARDSQRTSIGN 'DEFINED-ON-LINE '98) 
(PUT 'HARDSQRTSIGN 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'HARDSQRTSIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HARDSQRTSIGN (SLIST THINGS)
    (COND ((NULL SLIST) THINGS)
          (T
           (PROG (THISPLACE ANSWERS POS NEG)
             (SETQ THISPLACE (CAR SLIST))
             (SETQ ANSWERS
                     (PROG (U FORALL-RESULT FORALL-ENDPTR)
                       (SETQ U THINGS)
                       (COND ((NULL U) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (U)
                                           (CONS (SUBLIS U THISPLACE) U))
                                         (CAR U))
                                        NIL)))
                      LOOPLABEL
                       (SETQ U (CDR U))
                       (COND ((NULL U) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (U) (CONS (SUBLIS U THISPLACE) U))
                                 (CAR U))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ POS
                     (PROG (U FORALL-RESULT FORALL-ENDPTR)
                       (SETQ U ANSWERS)
                       (COND ((NULL U) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (U)
                                           (CONS (CONS THISPLACE (CAR U))
                                                 (CDR U)))
                                         (CAR U))
                                        NIL)))
                      LOOPLABEL
                       (SETQ U (CDR U))
                       (COND ((NULL U) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (U)
                                   (CONS (CONS THISPLACE (CAR U)) (CDR U)))
                                 (CAR U))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ NEG
                     (PROG (U FORALL-RESULT FORALL-ENDPTR)
                       (SETQ U ANSWERS)
                       (COND ((NULL U) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (U)
                                           (CONS
                                            (LIST THISPLACE 'MINUS (CAR U))
                                            (CDR U)))
                                         (CAR U))
                                        NIL)))
                      LOOPLABEL
                       (SETQ U (CDR U))
                       (COND ((NULL U) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (U)
                                   (CONS (LIST THISPLACE 'MINUS (CAR U))
                                         (CDR U)))
                                 (CAR U))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN (HARDSQRTSIGN (CDR SLIST) (NCONC POS NEG))))))) 
(PUT 'DEGREENEST 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREENEST 'DEFINED-ON-LINE '116) 
(PUT 'DEGREENEST 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'DEGREENEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREENEST (PF VAR)
    (COND ((ATOM PF) 0)
          ((EQ (CAR PF) 'SQRT)
           (COND ((DEPENDSP (CADR PF) VAR) (IADD1 (DEGREENEST (CADR PF) VAR)))
                 (T 0)))
          ((EQ (CAR PF) 'EXPT)
           (COND
            ((DEPENDSP (CADR PF) VAR)
             (COND
              ((EQCAR (CADDR PF) 'QUOTIENT) (IADD1 (DEGREENEST (CADR PF) VAR)))
              (T (DEGREENEST (CADR PF) VAR))))
            (T 0)))
          (T (DEGREENESTL (CDR PF) VAR)))) 
(PUT 'DEGREENESTL 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREENESTL 'DEFINED-ON-LINE '133) 
(PUT 'DEGREENESTL 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'DEGREENESTL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREENESTL (U VAR)
    (COND ((NULL U) 0)
          (T (MAX (DEGREENEST (CAR U) VAR) (DEGREENESTL (CDR U) VAR))))) 
(PUT 'SORTSQRTS 'NUMBER-OF-ARGS 2) 
(PUT 'SORTSQRTS 'DEFINED-ON-LINE '141) 
(PUT 'SORTSQRTS 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SORTSQRTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORTSQRTS (U VAR)
    (PROG (I V)
      (SETQ V (MKVECT 10))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ I (DEGREENEST (CAR U) VAR))
         (COND ((IEQUAL I 0) (INTERR "Non-dependent sqrt found")))
         (COND
          ((GREATERP I 10)
           (INTERR
            "Degree of nesting exceeds 10 (recompile with 10 increased)")))
         (PUTV V I (CONS (CAR U) (GETV V I)))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (SETQ U (GETV V 10))
      (PROG (I)
        (SETQ I 9)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (SETQ U (NCONC (GETV V I) U))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN U))) 
(PUT 'SQRTSIGN 'NUMBER-OF-ARGS 2) 
(PUT 'SQRTSIGN 'DEFINED-ON-LINE '163) 
(PUT 'SQRTSIGN 'DEFINED-IN-FILE 'INT/HACKSQRT.RED) 
(PUT 'SQRTSIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQRTSIGN (SQRTS X)
    (COND (NESTEDSQRTS (HARDSQRTSIGN (SORTSQRTS SQRTS X) (LIST NIL)))
          (T (EASYSQRTSIGN SQRTS (LIST NIL))))) 
(ENDMODULE) 