(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PRE)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'PREPROC 'NUMBER-OF-ARGS 1) 
(PUT 'PREPROC 'DEFINED-ON-LINE '37) 
(PUT 'PREPROC 'DEFINED-IN-FILE 'GENTRAN/PRE.RED) 
(PUT 'PREPROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPROC (EXP)
    (PROG (R)
      (SETQ R (PREPROC1 EXP))
      (COND (R (RETURN (CAR R))) (T (RETURN R))))) 
(FLUID '(*GETDECS)) 
(SETQ *GETDECS NIL) 
(SWITCH (LIST 'GETDECS)) 
(GLOBAL '(DEFTYPE*)) 
(SHARE (LIST 'DEFTYPE*)) 
(SETQ DEFTYPE* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'REAL)) 
(PUT 'PREPROC1 'NUMBER-OF-ARGS 1) 
(PUT 'PREPROC1 'DEFINED-ON-LINE '63) 
(PUT 'PREPROC1 'DEFINED-IN-FILE 'GENTRAN/PRE.RED) 
(PUT 'PREPROC1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPROC1 (EXP)
    (COND ((ATOM EXP) (LIST EXP))
          ((EQUAL (CAR EXP) '|:RD:|)
           (LIST
            (COND
             ((ATOM (CDR EXP))
              (COND ((FLOATP (CDR EXP)) (FL2BF (CDR EXP)))
                    (T
                     (NORMBF
                      (COND ((NOT (ATOM (CDR EXP))) (CDR EXP))
                            ((FIXP (CDR EXP))
                             (CONS '|:RD:| (CONS (CDR EXP) 0)))
                            (T (|READ:NUM| (CDR EXP))))))))
             (T EXP))))
          ((EQUAL (CAR EXP) '|:DN:|)
           (PREPROC1 (DECIMAL2INTERNAL (CADR EXP) (CDDR EXP))))
          ((EQ (CAR EXP) '*SQ) (PREPROC1 (PREPSQ (CADR EXP))))
          ((EQ (CAR EXP) 'PROCEDURE)
           (PROGN
            (SYMTABPUT (CADR EXP) '*PARAMS* (CAR (CDDDDR EXP)))
            (COND
             (*GETDECS
              (COND
               ((MEMQ (CADDR EXP) '(REAL INTEGER))
                (PROGN
                 (SYMTABPUT (CADR EXP) (CADR EXP) (LIST (CADDR EXP)))
                 (PROG (V)
                   (SETQ V (CAR (CDDDDR EXP)))
                  LAB
                   (COND ((NULL V) (RETURN NIL)))
                   ((LAMBDA (V) (SYMTABPUT (CADR EXP) V (LIST (CADDR EXP))))
                    (CAR V))
                   (SETQ V (CDR V))
                   (GO LAB))
                 (LIST
                  (NCONC (LIST 'PROCEDURE (CADR EXP) 'NIL)
                         (PROG (E FORALL-RESULT FORALL-ENDPTR)
                           (SETQ E (CDDDR EXP))
                          STARTOVER
                           (COND ((NULL E) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                           (SETQ E (CDR E))
                           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                          LOOPLABEL
                           (COND ((NULL E) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                           (SETQ E (CDR E))
                           (GO LOOPLABEL))))))
               (T
                (PROGN
                 (PROG (V)
                   (SETQ V (CAR (CDDDDR EXP)))
                  LAB
                   (COND ((NULL V) (RETURN NIL)))
                   ((LAMBDA (V) (SYMTABPUT (CADR EXP) V (LIST DEFTYPE*)))
                    (CAR V))
                   (SETQ V (CDR V))
                   (GO LAB))
                 (LIST
                  (PROG (E FORALL-RESULT FORALL-ENDPTR)
                    (SETQ E EXP)
                   STARTOVER
                    (COND ((NULL E) (RETURN NIL)))
                    (SETQ FORALL-RESULT ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ E (CDR E))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL E) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ E (CDR E))
                    (GO LOOPLABEL)))))))
             (T
              (LIST
               (PROG (E FORALL-RESULT FORALL-ENDPTR)
                 (SETQ E EXP)
                STARTOVER
                 (COND ((NULL E) (RETURN NIL)))
                 (SETQ FORALL-RESULT ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ E (CDR E))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL E) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ E (CDR E))
                 (GO LOOPLABEL)))))))
          ((EQ (CAR EXP) 'DECLARE)
           (PROGN
            (SETQ EXP (CAR (PREPROC1 (CDR EXP))))
            (SETQ EXP (PREPROCDEC EXP))
            (PROG (DEC)
              (SETQ DEC EXP)
             LAB
              (COND ((NULL DEC) (RETURN NIL)))
              ((LAMBDA (DEC)
                 (PROG (VAR)
                   (SETQ VAR (CDR DEC))
                  LAB
                   (COND ((NULL VAR) (RETURN NIL)))
                   ((LAMBDA (VAR)
                      (COND
                       ((MEMQ (CAR DEC) '(SUBROUTINE FUNCTION))
                        (SYMTABPUT VAR '*TYPE* (CAR DEC)))
                       (T
                        (SYMTABPUT NIL (COND ((ATOM VAR) VAR) (T (CAR VAR)))
                         (COND ((ATOM VAR) (LIST (CAR DEC)))
                               (T (CONS (CAR DEC) (CDR VAR))))))))
                    (CAR VAR))
                   (SETQ VAR (CDR VAR))
                   (GO LAB)))
               (CAR DEC))
              (SETQ DEC (CDR DEC))
              (GO LAB))
            NIL))
          ((AND (EQ (CAR EXP) 'SETQ) (PAIRP (CADDR EXP))
                (MEMQ (CAADDR EXP) '(COND PROGN)))
           (MIGRATE-SETQS EXP))
          ((MEMQ (CAR EXP) '(PLUS TIMES DIFFERENCE QUOTIENT MINUS))
           (PROG (SIMP_EXP)
             (RETURN
              (COND
               ((AND (PAIRP (CAR (SETQ SIMP_EXP (SIMP* EXP))))
                     (MEMQ (CAR (CAR SIMP_EXP)) '(|:CR:| |:CRN:| |:GI:|)))
                (COND ((ONEP (CDR SIMP_EXP)) (LIST (CAR SIMP_EXP)))
                      (T
                       (LIST
                        (LIST 'QUOTIENT (CAR SIMP_EXP)
                              (CAR
                               (PREPROC1
                                (PREPSQ (CONS (CDR SIMP_EXP) 1)))))))))
               (T
                (LIST
                 (PROG (E FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E EXP)
                  STARTOVER
                   (COND ((NULL E) (RETURN NIL)))
                   (SETQ FORALL-RESULT ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ E (CDR E))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL E) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ E (CDR E))
                   (GO LOOPLABEL))))))))
          (T
           (PROGN
            (COND
             ((AND *GETDECS (MEMQ (CAR EXP) '(~FOR FOR)))
              (SYMTABPUT NIL (CADR EXP) '(INTEGER))))
            (LIST
             (PROG (E FORALL-RESULT FORALL-ENDPTR)
               (SETQ E EXP)
              STARTOVER
               (COND ((NULL E) (RETURN NIL)))
               (SETQ FORALL-RESULT ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ E (CDR E))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL E) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR ((LAMBDA (E) (PREPROC1 E)) (CAR E)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ E (CDR E))
               (GO LOOPLABEL))))))) 
(PUT 'PREPROCDEC 'NUMBER-OF-ARGS 1) 
(PUT 'PREPROCDEC 'DEFINED-ON-LINE '148) 
(PUT 'PREPROCDEC 'DEFINED-IN-FILE 'GENTRAN/PRE.RED) 
(PUT 'PREPROCDEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPROCDEC (ARG)
    (COND ((ATOM ARG) ARG)
          ((EQ (CAR ARG) 'TIMES)
           (COND
            ((AND (EQUAL (LENGTH ARG) 3) (FIXP (CADDR ARG)))
             (INTERN
              (COMPRESS
               (APPEND (APPEND (EXPLODE (CADR ARG)) (EXPLODE '*))
                       (EXPLODE (CADDR ARG))))))
            (T
             (PROG (RESULT)
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE (LENGTH ARG) I)) (RETURN NIL)))
                 (SETQ RESULT
                         (APPEND RESULT
                                 (COND ((EQUAL (NTH ARG I) 'TIMES) '(*))
                                       (T (EXPLODE (NTH ARG I))))))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (RETURN (INTERN (COMPRESS RESULT)))))))
          ((EQ (CAR ARG) 'IMPLICIT)
           (INTERN
            (COMPRESS
             (APPEND (EXPLODE '|IMPLICIT |)
                     (EXPLODE (PREPROCDEC (CADR ARG)))))))
          ((EQ (CAR ARG) 'DIFFERENCE)
           (INTERN
            (COMPRESS
             (APPEND (APPEND (EXPLODE (CADR ARG)) (EXPLODE '-))
                     (EXPLODE (CADDR ARG))))))
          (T
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A ARG)
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (A) (PREPROCDEC A)) (CAR A)) NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (A) (PREPROCDEC A)) (CAR A)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'MIGRATE-SETQS 'NUMBER-OF-ARGS 1) 
(PUT 'MIGRATE-SETQS 'DEFINED-ON-LINE '183) 
(PUT 'MIGRATE-SETQS 'DEFINED-IN-FILE 'GENTRAN/PRE.RED) 
(PUT 'MIGRATE-SETQS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MIGRATE-SETQS (EXP) (LIST (MIGRATE-SETQS1 (CADR EXP) (CADDR EXP)))) 
(PUT 'MIGRATE-SETQS1 'NUMBER-OF-ARGS 2) 
(PUT 'MIGRATE-SETQS1 'DEFINED-ON-LINE '188) 
(PUT 'MIGRATE-SETQS1 'DEFINED-IN-FILE 'GENTRAN/PRE.RED) 
(PUT 'MIGRATE-SETQS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MIGRATE-SETQS1 (VAR EXP)
    (COND ((ATOM EXP) (PREPROC (LIST 'SETQ VAR EXP)))
          ((EQCAR EXP 'COND)
           (CONS 'COND
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U (CDR EXP))
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U)
                                       (LIST (PREPROC (CAR U))
                                             (MIGRATE-SETQS1 VAR (CADR U))))
                                     (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (U)
                               (LIST (PREPROC (CAR U))
                                     (MIGRATE-SETQS1 VAR (CADR U))))
                             (CAR U))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQCAR EXP 'PROGN)
           (REVERSE
            (RPLACA (SETQ EXP (REVERSE EXP)) (MIGRATE-SETQS1 VAR (CAR EXP)))))
          (T (PREPROC (LIST 'SETQ VAR EXP))))) 
(ENDMODULE) 