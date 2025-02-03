(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TPSCOMP)) 
(FLUID
 '(UNKNOWNS *EXP KNOWNPS |PS:MAX-ORDER| |PS:SPECIALS| |PS:LEVEL| |PS:MAX-LEVEL|
   *RATARG)) 
(SETQ |PS:SPECIALS| (LIST 'PSREV 'PSCOMP 'INT)) 
(SETQ |PS:MAX-LEVEL| 20) 
(PUT '|PS:COMPILE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:COMPILE| 'DEFINED-ON-LINE '47) 
(PUT '|PS:COMPILE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:COMPILE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:COMPILE| (FORM DEPVAR ABOUT)
    (COND ((IDP FORM) (MAKE-PS-ID FORM DEPVAR ABOUT))
          ((OR (NUMBERP FORM)
               (AND (PAIRP FORM) (NEQ (CAR FORM) '|:PS:|)
                    (GET (CAR FORM) 'DNAME)))
           FORM)
          ((AND (PAIRP FORM) (EQUAL (CAR FORM) '|:PS:|))
           (COND
            ((AND (EQUAL (|PS:EXPANSION-POINT| FORM) ABOUT)
                  (EQUAL (|PS:DEPVAR| FORM) DEPVAR))
             FORM)
            (T (|PS:COMPILE| (|PS:VALUE| FORM) DEPVAR ABOUT))))
          ((MEMQ (CAR FORM) |PS:SPECIALS|)
           (APPLY (GET (CAR FORM) '|PS:CRULE|) (LIST FORM DEPVAR ABOUT)))
          (T
           (PROG (SPEC_EXP_FN TMP ISPOLY)
             (SETQ TMP
                     (PREPSQXX
                      (SIMP* (LIST 'DF (|PS:ARG-VALUES| FORM) DEPVAR))))
             (COND
              ((EQUAL TMP 0)
               (PROGN
                (SETQ TMP
                        (CONS (CAR FORM)
                              (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                                (SETQ ARG (CDR FORM))
                                (COND ((NULL ARG) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (ARG)
                                                    (COND
                                                     ((AND (PAIRP ARG)
                                                           (EQUAL (CAR ARG)
                                                                  '|:PS:|))
                                                      (PROGN
                                                       (|PS:FIND-ORDER| ARG)
                                                       (PREPSQ
                                                        (|PS:EVALUATE| ARG
                                                         0))))
                                                     (T
                                                      (SUBST ABOUT DEPVAR
                                                             ARG))))
                                                  (CAR ARG))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ ARG (CDR ARG))
                                (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (ARG)
                                            (COND
                                             ((AND (PAIRP ARG)
                                                   (EQUAL (CAR ARG) '|:PS:|))
                                              (PROGN
                                               (|PS:FIND-ORDER| ARG)
                                               (PREPSQ (|PS:EVALUATE| ARG 0))))
                                             (T (SUBST ABOUT DEPVAR ARG))))
                                          (CAR ARG))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))
                (SETQ TMP (SIMP* TMP))
                (COND ((EQUAL TMP '(NIL . 1)) (RETURN 0))
                      (T (RETURN (MAKE-CONSTANTPS TMP FORM DEPVAR))))
                NIL)))
             (PROG (*RATARG)
               (SETQ *RATARG T)
               (SETQ TMP (REVAL1 FORM T))
               (COND
                ((EQUAL ABOUT '|PS:INF|)
                 (SETQ TMP
                         (REVAL1 (SUBST (LIST 'QUOTIENT 1 DEPVAR) DEPVAR TMP)
                                 T))))
               (SETQ TMP (CDR (COEFFEVAL (LIST TMP DEPVAR))))
               (SETQ ISPOLY T)
               (PROG (C)
                 (SETQ C TMP)
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C)
                    (SETQ ISPOLY (AND ISPOLY (NULL (DEPENDS C DEPVAR)))))
                  (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB))
               (COND
                ((AND ISPOLY (NEQ ABOUT 0) (NEQ ABOUT '|PS:INF|))
                 (PROGN
                  (SETQ TMP
                          (REVAL1 (SUBST (LIST 'PLUS DEPVAR ABOUT) DEPVAR FORM)
                                  T))
                  (SETQ TMP (CDR (COEFFEVAL (LIST TMP DEPVAR))))
                  NIL))))
             (COND (ISPOLY (RETURN (MAKE-POLY-PS FORM DEPVAR ABOUT TMP))))
             (COND
              ((GET (CAR FORM) '|PS:CRULE|)
               (RETURN
                (APPLY (GET (CAR FORM) '|PS:CRULE|)
                       (LIST FORM DEPVAR ABOUT)))))
             (SETQ SPEC_EXP_FN (GET (CAR FORM) '*SPECEXP))
             (COND
              (SPEC_EXP_FN
               (PROGN
                (SETQ TMP (APPLY SPEC_EXP_FN (LIST FORM DEPVAR ABOUT)))
                (COND (TMP (RETURN TMP)))
                NIL)))
             (SETQ TMP (ASSOC FORM KNOWNPS))
             (COND (TMP (RETURN (CONS '|:PS:| (CDR TMP))))
                   (T
                    (RETURN
                     (|PS:UNKNOWN-CRULE|
                      (CONS (CAR FORM)
                            (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                              (SETQ ARG (CDR FORM))
                              (COND ((NULL ARG) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ARG)
                                                  (|PS:COMPILE| ARG DEPVAR
                                                   ABOUT))
                                                (CAR ARG))
                                               NIL)))
                             LOOPLABEL
                              (SETQ ARG (CDR ARG))
                              (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ARG)
                                          (|PS:COMPILE| ARG DEPVAR ABOUT))
                                        (CAR ARG))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                      DEPVAR ABOUT)))))))) 
(PUT 'MAKE-PS-ID 'NUMBER-OF-ARGS 3) 
(PUT 'MAKE-PS-ID 'DEFINED-ON-LINE '114) 
(PUT 'MAKE-PS-ID 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'MAKE-PS-ID 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-PS-ID (ID DEPVAR ABOUT)
    (PROG (PS)
      (SETQ PS (MAKE-PS 'FULL ID DEPVAR ABOUT))
      (COND
       ((EQUAL ID DEPVAR)
        (COND
         ((EQUAL ABOUT '|PS:INF|)
          (PROGN
           (|PS:PUTV| PS 0 (MINUS 1))
           (|PS:PUTV| PS 5 (LIST (CONS 0 (CONS 1 1))))
           (|PS:PUTV| PS 1 (MINUS 1))
           NIL))
         (T
          (PROGN
           (SETQ ABOUT
                   (COND ((IDP ABOUT) (CONS (LIST (CONS (CONS ABOUT 1) 1)) 1))
                         ((OR (NUMBERP ABOUT)
                              (AND (PAIRP ABOUT) (NEQ (CAR ABOUT) '|:PS:|)
                                   (GET (CAR ABOUT) 'DNAME)))
                          (CONS (COND ((ZEROP ABOUT) NIL) (T ABOUT)) 1))
                         (T (SIMP* ABOUT))))
           (COND
            ((CAR ABOUT)
             (PROGN
              (|PS:PUTV| PS 0 0)
              (|PS:PUTV| PS 5 (LIST (CONS 0 ABOUT) (CONS 1 (CONS 1 1))))
              (|PS:PUTV| PS 1 1)
              NIL))
            (T
             (PROGN
              (|PS:PUTV| PS 0 1)
              (|PS:PUTV| PS 5 (LIST (CONS 0 (CONS 1 1))))
              (|PS:PUTV| PS 1 1)
              NIL)))))))
       (T
        (PROGN
         (|PS:PUTV| PS 0 0)
         (|PS:PUTV| PS 5 (LIST (CONS 0 (CONS (LIST (CONS (CONS ID 1) 1)) 1))))
         (|PS:PUTV| PS 1 0)
         NIL)))
      (RETURN PS))) 
(PUT 'MAKE-CONSTANTPS 'NUMBER-OF-ARGS 3) 
(PUT 'MAKE-CONSTANTPS 'DEFINED-ON-LINE '144) 
(PUT 'MAKE-CONSTANTPS 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'MAKE-CONSTANTPS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-CONSTANTPS (U V D)
    (PROG (PS)
      (COND ((EQUAL U '(NIL . 1)) (RETURN U)))
      (SETQ PS (CONS (GET 'TPS 'TAG) (MKVECT 7)))
      (|PS:PUTV| PS 0 0)
      (|PS:PUTV| PS 6 'FULL)
      (|PS:PUTV| PS 4 V)
      (|PS:PUTV| PS 1 0)
      (|PS:PUTV| PS 5 (LIST (CONS 0 U)))
      (|PS:PUTV| PS 2 D)
      (PUTV (CDR PS) 7 *SQVAR*)
      (RETURN PS))) 
(PUT 'MAKE-PS 'NUMBER-OF-ARGS 4) 
(PUT 'MAKE-PS 'DEFINED-ON-LINE '159) 
(PUT 'MAKE-PS 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'MAKE-PS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-PS (FORM EXP DEPVAR ABOUT)
    (PROG (PS)
      (SETQ PS (CONS (GET 'TPS 'TAG) (MKVECT 7)))
      (|PS:PUTV| PS 0 0)
      (|PS:PUTV| PS 6 FORM)
      (|PS:PUTV| PS 4 EXP)
      (|PS:PUTV| PS 2 DEPVAR)
      (|PS:PUTV| PS 3 ABOUT)
      (|PS:PUTV| PS 1 (MINUS 1))
      (PUTV (CDR PS) 7 *SQVAR*)
      (RETURN PS))) 
(PUT 'MAKE-POLY-PS 'NUMBER-OF-ARGS 4) 
(PUT 'MAKE-POLY-PS 'DEFINED-ON-LINE '172) 
(PUT 'MAKE-POLY-PS 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'MAKE-POLY-PS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-POLY-PS (FORM DEPVAR ABOUT COEFFLIST)
    (PROG (PS ORDSETP LEN)
      (SETQ PS (CONS (GET 'TPS 'TAG) (MKVECT 7)))
      (|PS:PUTV| PS 6 'FULL)
      (|PS:PUTV| PS 4 FORM)
      (|PS:PUTV| PS 2 DEPVAR)
      (|PS:PUTV| PS 3 ABOUT)
      (|PS:PUTV| PS 1 (MINUS 1))
      (SETQ LEN (DIFFERENCE (LENGTH COEFFLIST) 1))
      (PROG (N)
        (SETQ N 0)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN N)) (RETURN NIL)))
        (PROGN
         (COND
          ((NEQ (CAR COEFFLIST) 0)
           (PROGN
            (COND
             ((NOT ORDSETP) (PROGN (|PS:PUTV| PS 0 N) (SETQ ORDSETP T) NIL)))
            (|PS:SET-TERM| PS N (SIMP* (CAR COEFFLIST)))
            NIL)))
         (SETQ COEFFLIST (CDR COEFFLIST))
         NIL)
        (SETQ N (PLUS2 N 1))
        (GO LAB))
      (PUTV (CDR PS) 7 *SQVAR*)
      (RETURN PS))) 
(PUT '|PS:PLUS-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:PLUS-CRULE| 'DEFINED-ON-LINE '195) 
(PUT '|PS:PLUS-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:PLUS-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:PLUS-CRULE| (A D N)
    (PROG (PLUSES MINUSES)
      (PROG (TERM)
        (SETQ TERM (CDR A))
       LAB
        (COND ((NULL TERM) (RETURN NIL)))
        ((LAMBDA (TERM)
           (COND
            ((AND (PAIRP TERM) (EQUAL (CAR TERM) 'MINUS))
             (SETQ MINUSES (CONS (CADR TERM) MINUSES)))
            (T (SETQ PLUSES (CONS TERM PLUSES)))))
         (CAR TERM))
        (SETQ TERM (CDR TERM))
        (GO LAB))
      (COND
       ((NOT (NULL PLUSES))
        (PROGN
         (COND
          ((NOT (NULL (CDR PLUSES)))
           (SETQ PLUSES
                   (MAKE-PS
                    (CONS 'PLUS
                          (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                            (SETQ TERM PLUSES)
                            (COND ((NULL TERM) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (TERM)
                                                (|PS:COMPILE| TERM D N))
                                              (CAR TERM))
                                             NIL)))
                           LOOPLABEL
                            (SETQ TERM (CDR TERM))
                            (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (TERM) (|PS:COMPILE| TERM D N))
                                      (CAR TERM))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                    (|PS:ARG-VALUES| (CONS 'PLUS PLUSES)) D N)))
          (T (SETQ PLUSES (|PS:COMPILE| (CAR PLUSES) D N))))
         (|PS:FIND-ORDER| PLUSES))))
      (COND
       ((NOT (NULL MINUSES))
        (PROGN
         (COND
          ((NOT (NULL (CDR MINUSES)))
           (SETQ MINUSES
                   (MAKE-PS
                    (CONS 'PLUS
                          (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                            (SETQ TERM MINUSES)
                            (COND ((NULL TERM) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (TERM)
                                                (|PS:COMPILE| TERM D N))
                                              (CAR TERM))
                                             NIL)))
                           LOOPLABEL
                            (SETQ TERM (CDR TERM))
                            (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (TERM) (|PS:COMPILE| TERM D N))
                                      (CAR TERM))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                    (|PS:ARG-VALUES| (CONS 'PLUS MINUSES)) D N)))
          (T (SETQ MINUSES (|PS:COMPILE| (CAR MINUSES) D N))))
         (|PS:FIND-ORDER| MINUSES))))
      (COND ((NULL MINUSES) (RETURN PLUSES))
            ((NULL PLUSES)
             (SETQ A
                     ((LAMBDA (PS) (MAKE-PS PS (|PS:ARG-VALUES| PS) D N))
                      (CONS 'MINUS (LIST MINUSES)))))
            (T
             (SETQ A
                     ((LAMBDA (PS) (MAKE-PS PS (|PS:ARG-VALUES| PS) D N))
                      (CONS 'DIFFERENCE (LIST PLUSES MINUSES))))))
      (|PS:FIND-ORDER| A)
      (RETURN A))) 
(PUT 'PLUS '|PS:CRULE| '|PS:PLUS-CRULE|) 
(PUT '|PS:UNARY-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:UNARY-CRULE| 'DEFINED-ON-LINE '234) 
(PUT '|PS:UNARY-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:UNARY-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:UNARY-CRULE| (A D N)
    (MAKE-PS (LIST (CAR A) (|PS:COMPILE| (CADR A) D N)) (|PS:ARG-VALUES| A) D
     N)) 
(PUT '|PS:MINUS-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:MINUS-CRULE| 'DEFINED-ON-LINE '238) 
(PUT '|PS:MINUS-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:MINUS-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:MINUS-CRULE| (A D N)
    (COND
     ((OR (NUMBERP (CADR A))
          (AND (PAIRP (CADR A)) (NEQ (CAR (CADR A)) '|:PS:|)
               (GET (CAR (CADR A)) 'DNAME)))
      (|:MINUS| (CADR A)))
     (T (|PS:UNARY-CRULE| A D N)))) 
(PUT 'MINUS '|PS:CRULE| '|PS:MINUS-CRULE|) 
(PUT 'SQRT '|PS:CRULE| '|PS:UNARY-CRULE|) 
(PUT 'CBRT '|PS:CRULE| '|PS:UNARY-CRULE|) 
(PUT '|PS:BINARY-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:BINARY-CRULE| 'DEFINED-ON-LINE '247) 
(PUT '|PS:BINARY-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:BINARY-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:BINARY-CRULE| (A D N)
    (PROGN
     (SETQ A
             (MAKE-PS
              (CONS (CAR A)
                    (LIST (|PS:COMPILE| (CADR A) D N)
                          (|PS:COMPILE| (CADDR A) D N)))
              (|PS:ARG-VALUES| A) D N))
     (|PS:FIND-ORDER| A)
     A)) 
(PUT 'DIFFERENCE '|PS:CRULE| '|PS:BINARY-CRULE|) 
(PUT '|PS:NARY-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:NARY-CRULE| 'DEFINED-ON-LINE '256) 
(PUT '|PS:NARY-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:NARY-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:NARY-CRULE| (A D N)
    (PROGN
     (COND
      ((NULL (CDDDR A))
       (SETQ A
               (MAKE-PS (LIST (CAR A) (CADR A) (CADDR A)) (|PS:ARG-VALUES| A) D
                N)))
      (T
       (SETQ A
               (MAKE-PS
                (LIST (CAR A) (CADR A)
                      (|PS:NARY-CRULE| (CONS (CAR A) (CDDR A)) D N))
                (|PS:ARG-VALUES| A) D N))))
     (|PS:FIND-ORDER| A)
     A)) 
(PUT '|PS:TIMES-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:TIMES-CRULE| 'DEFINED-ON-LINE '268) 
(PUT '|PS:TIMES-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:TIMES-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:TIMES-CRULE| (A D N)
    (PROG (PROD VARIABLES CONSTANTS)
      (SETQ PROD
              (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                (SETQ ARG (CDR A))
                (COND ((NULL ARG) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ARG) (|PS:COMPILE| ARG D N))
                                  (CAR ARG))
                                 NIL)))
               LOOPLABEL
                (SETQ ARG (CDR ARG))
                (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ARG) (|PS:COMPILE| ARG D N)) (CAR ARG))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (ARG)
        (SETQ ARG PROD)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (COND
            ((OR
              (OR (NUMBERP ARG)
                  (AND (PAIRP ARG) (NEQ (CAR ARG) '|:PS:|)
                       (GET (CAR ARG) 'DNAME)))
              (AND (NOT (IDP (CDR ARG)))
                   (EQUAL (|PS:EXPRESSION| ARG) 'PSCONSTANT)))
             (SETQ CONSTANTS (CONS ARG CONSTANTS)))
            (T (SETQ VARIABLES (CONS ARG VARIABLES)))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (COND
       ((NOT (NULL VARIABLES))
        (COND ((NULL (CDR VARIABLES)) (SETQ VARIABLES (CAR VARIABLES)))
              (T
               (SETQ VARIABLES
                       (|PS:NARY-CRULE| (CONS 'TIMES VARIABLES) D N))))))
      (COND ((NULL CONSTANTS) (RETURN VARIABLES))
            (T
             (PROGN
              (SETQ PROD (CONS 1 1))
              (PROG (ARG)
                (SETQ ARG CONSTANTS)
               LAB
                (COND ((NULL ARG) (RETURN NIL)))
                ((LAMBDA (ARG)
                   (SETQ PROD
                           (MULTSQ PROD
                                   (COND
                                    ((OR (NUMBERP ARG)
                                         (AND (PAIRP ARG)
                                              (NEQ (CAR ARG) '|:PS:|)
                                              (GET (CAR ARG) 'DNAME)))
                                     (CONS (COND ((EQUAL ARG 0) NIL) (T ARG))
                                           1))
                                    (T (|PS:GET-TERM| ARG 0))))))
                 (CAR ARG))
                (SETQ ARG (CDR ARG))
                (GO LAB))
              (COND
               (VARIABLES
                (SETQ A
                        (MAKE-PS (LIST 'PSMULT PROD VARIABLES)
                         (|PS:ARG-VALUES| A) D N)))
               (T (RETURN (MAKE-CONSTANTPS PROD (|PS:ARG-VALUES| A) D))))
              (|PS:FIND-ORDER| A)
              (RETURN A)))))) 
(PUT 'TIMES '|PS:CRULE| '|PS:TIMES-CRULE|) 
(PUT 'QUOTIENT '|PS:CRULE| '|PS:QUOTIENT-CRULE|) 
(PUT '|PS:QUOTIENT-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:QUOTIENT-CRULE| 'DEFINED-ON-LINE '300) 
(PUT '|PS:QUOTIENT-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:QUOTIENT-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:QUOTIENT-CRULE| (A D N)
    (PROG (R1 R2)
      (SETQ R1 (CADR A))
      (SETQ R2 (CADDR A))
      (COND
       ((AND (EQCAR R1 'EXPT) (EQCAR R2 'EXPT) (EQUAL (CADR R1) (CADR R2)))
        (RETURN
         (|PS:COMPILE|
          (LIST 'EXPT (CADR R1) (LIST 'DIFFERENCE (CADDR R1) (CADDR R2))) D
          N))))
      (SETQ R1 (|PS:COMPILE| (CADR A) D N))
      (COND
       ((AND
         (OR
          (OR (NUMBERP R1)
              (AND (PAIRP R1) (NEQ (CAR R1) '|:PS:|) (GET (CAR R1) 'DNAME)))
          (AND (NOT (IDP (CDR R1))) (EQUAL (|PS:EXPRESSION| R1) 'PSCONSTANT)))
         (EQCAR R2 'EXPT))
        (PROGN
         (SETQ R2
                 (|PS:COMPILE|
                  (LIST 'EXPT (CADR R2) (PREPSQXX (SIMPMINUS (CDDR R2)))) D N))
         (RETURN
          (COND ((ONEP R1) R2)
                (T
                 (PROGN
                  (SETQ A
                          (MAKE-PS
                           (LIST 'PSMULT
                                 (COND
                                  ((OR (NUMBERP R1)
                                       (AND (PAIRP R1) (NEQ (CAR R1) '|:PS:|)
                                            (GET (CAR R1) 'DNAME)))
                                   (CONS R1 1))
                                  (T (|PS:GET-TERM| R1 0)))
                                 R2)
                           (|PS:ARG-VALUES| A) D N))
                  (|PS:FIND-ORDER| A)
                  A)))))))
      (SETQ R2 (|PS:COMPILE| (CADDR A) D N))
      (COND
       ((OR
         (OR (NUMBERP R2)
             (AND (PAIRP R2) (NEQ (CAR R2) '|:PS:|) (GET (CAR R2) 'DNAME)))
         (AND (NOT (IDP (CDR R2))) (EQUAL (|PS:EXPRESSION| R2) 'PSCONSTANT)))
        (PROGN
         (SETQ R2
                 (COND
                  ((OR (NUMBERP R2)
                       (AND (PAIRP R2) (NEQ (CAR R2) '|:PS:|)
                            (GET (CAR R2) 'DNAME)))
                   (CONS 1 R2))
                  (T (INVSQ (|PS:GET-TERM| R2 0)))))
         (SETQ A (MAKE-PS (LIST 'PSMULT R2 R1) (|PS:ARG-VALUES| A) D N))))
       (T (SETQ A (MAKE-PS (LIST 'QUOTIENT R1 R2) (|PS:ARG-VALUES| A) D N))))
      (|PS:FIND-ORDER| A)
      (RETURN A))) 
(PUT '|PS:INT-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:INT-CRULE| 'DEFINED-ON-LINE '340) 
(PUT '|PS:INT-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:INT-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:INT-CRULE| (A D N)
    (PROG (R ARG1 PSORD INTVAR X)
      (SETQ INTVAR (CADDR A))
      (COND ((NOT (IDP INTVAR)) (TYPERR INTVAR "kernel: ps!:int!-crule")))
      (COND
       ((DEPENDS N INTVAR)
        (PROGN
         (SETQ R (|PS:COMPILE| (CADR A) D N))
         (SETQ X (PREPSQXX (SIMP (LIST 'INT (|PS:VALUE| R) INTVAR))))
         (COND
          ((DEPENDS X 'INT)
           (RERROR 'TPS 11
                   "Can't integrate this series (expansion pt depends on integration variable)"))
          (T (RETURN (|PS:COMPILE| X D N))))
         NIL)))
      (SETQ ARG1 (|PS:COMPILE| (PREPSQXX (SIMP* (CADR A))) D N))
      (SETQ R (MAKE-PS (LIST 'INT ARG1 INTVAR) (|PS:ARG-VALUES| A) D N))
      (SETQ PSORD (|PS:FIND-ORDER| ARG1))
      (COND
       ((EQUAL D INTVAR)
        (COND
         ((NEQ (|PS:EXPANSION-POINT| ARG1) '|PS:INF|)
          (PROGN
           (COND
            ((AND (LESSP PSORD 0)
                  (NEQ (|PS:EVALUATE| ARG1 (MINUS 1)) (CONS NIL 1)))
             (RERROR 'TPS 12 "Logarithmic Singularity")))))
         ((AND (LESSP PSORD 2) (NEQ (|PS:EVALUATE| ARG1 1) (CONS NIL 1)))
          (RERROR 'TPS 13 "Logarithmic Singularity at Infinity")))))
      (|PS:FIND-ORDER| R)
      (RETURN R))) 
(PUT 'INT '|PS:CRULE| '|PS:INT-CRULE|) 
(PUT '|PS:LOG-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:LOG-CRULE| 'DEFINED-ON-LINE '370) 
(PUT '|PS:LOG-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:LOG-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:LOG-CRULE| (A D N)
    (PROG (R DFDX F)
      (SETQ F (|PS:COMPILE| (CADR A) D N))
      (COND
       ((NEQ (|PS:ORDER| F) 0) (RERROR 'TPS 14 "Logarithmic Singularity")))
      (SETQ DFDX (|PS:COMPILE| (PREPSQ (SIMP* (LIST 'DF F D))) D N))
      (SETQ R (|PS:COMPILE| (LIST 'QUOTIENT DFDX F) D N))
      (SETQ R (MAKE-PS (LIST 'INT R D) (|PS:ARG-VALUES| A) D N))
      (|PS:SET-TERM| R 0 (SIMP* (LIST 'LOG (PREPSQ (|PS:GET-TERM| F 0)))))
      (|PS:FIND-ORDER| R)
      (RETURN R))) 
(PUT 'LOG '|PS:CRULE| '|PS:LOG-CRULE|) 
(PUT '|PS:ARG-VALUES| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:ARG-VALUES| 'DEFINED-ON-LINE '385) 
(PUT '|PS:ARG-VALUES| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:ARG-VALUES| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:ARG-VALUES| (FUNCT)
    (CONS (CAR FUNCT)
          (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
            (SETQ ARG (CDR FUNCT))
            (COND ((NULL ARG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ARG)
                                (COND
                                 ((OR (ATOM ARG)
                                      (AND (NEQ (CAR ARG) '|:PS:|)
                                           (GET (CAR ARG) 'DNAME)))
                                  ARG)
                                 ((AND (PAIRP ARG) (EQUAL (CAR ARG) '|:PS:|))
                                  (|PS:VALUE| ARG))
                                 (T (|PS:ARG-VALUES| ARG))))
                              (CAR ARG))
                             NIL)))
           LOOPLABEL
            (SETQ ARG (CDR ARG))
            (COND ((NULL ARG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (ARG)
                        (COND
                         ((OR (ATOM ARG)
                              (AND (NEQ (CAR ARG) '|:PS:|)
                                   (GET (CAR ARG) 'DNAME)))
                          ARG)
                         ((AND (PAIRP ARG) (EQUAL (CAR ARG) '|:PS:|))
                          (|PS:VALUE| ARG))
                         (T (|PS:ARG-VALUES| ARG))))
                      (CAR ARG))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT '|PS:UNKNOWN-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:UNKNOWN-CRULE| 'DEFINED-ON-LINE '391) 
(PUT '|PS:UNKNOWN-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:UNKNOWN-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:UNKNOWN-CRULE| (A D N)
    ((LAMBDA (AVAL TMP)
       (COND ((SETQ TMP (ASSOC AVAL UNKNOWNS)) (CONS '|:PS:| (CDR TMP)))
             ((GREATERP |PS:LEVEL| |PS:MAX-LEVEL|)
              (RERROR 'TPS 15 "Recursion too deep in ps!:unknown!-crule"))
             (T
              ((LAMBDA (DFDX UNKNOWNS)
                 ((LAMBDA (R S)
                    (PROGN
                     (SETQ |PS:LEVEL| (PLUS |PS:LEVEL| 1))
                     (GLOBAL (LIST S))
                     (SET S (CDR R))
                     (|PS:UNKNOWN-TERM1| R A)
                     (SETQ DFDX (|PS:COMPILE| DFDX D N))
                     (COND
                      ((LESSP (|PS:ORDER| DFDX) 0)
                       (RERROR 'TPS 16 "Pole or Logarithmic Singularity")))
                     (|PS:PUTV| R 6 (LIST 'INT DFDX D))
                     (SETQ KNOWNPS (CONS (CONS AVAL S) KNOWNPS))
                     (SETQ |PS:LEVEL| (DIFFERENCE |PS:LEVEL| 1))
                     R))
                  (MAKE-PS NIL AVAL D N) (CDAR UNKNOWNS)))
               (|PS:DIFFERENTIATE| A D)
               (CONS (CONS AVAL (GENSYM)) UNKNOWNS)))))
     (|PS:ARG-VALUES| A) NIL)) 
(PUT '|PS:UNKNOWN-TERM1| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:UNKNOWN-TERM1| 'DEFINED-ON-LINE '431) 
(PUT '|PS:UNKNOWN-TERM1| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:UNKNOWN-TERM1| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:UNKNOWN-TERM1| (PS A)
    (PROG (PSORD TERM ABOUT INFMULT X)
      (SETQ PSORD 0)
      (SETQ ABOUT (|PS:EXPANSION-POINT| PS))
      (SETQ X (|PS:DEPVAR| PS))
     LOOP
      (SETQ TERM (SIMP* (|PS:FIRST-TERM| A)))
      (|PS:SET-TERM| PS PSORD TERM)
      (COND
       ((EQUAL (CAR TERM) NIL)
        (PROGN
         (SETQ PSORD (PLUS PSORD 1))
         (COND
          ((GREATERP PSORD |PS:MAX-ORDER|)
           (RERROR 'TPS 17
                   (LIST (|PS:VALUE| PS) "has zero expansion to order"
                         PSORD))))
         (SETQ A (LIST 'QUOTIENT (LIST 'DF A X) PSORD))
         (COND
          ((EQUAL ABOUT '|PS:INF|)
           (PROGN
            (COND
             ((EQUAL PSORD 1)
              (SETQ INFMULT
                      (|PS:COMPILE| (LIST 'MINUS (LIST 'TIMES X X)) X ABOUT))))
            (SETQ A (LIST 'TIMES INFMULT A)))))
         (SETQ A (PREPSQXX (SIMP* A)))
         (GO LOOP)))))) 
(PUT '|PS:FIRST-TERM| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:FIRST-TERM| 'DEFINED-ON-LINE '456) 
(PUT '|PS:FIRST-TERM| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:FIRST-TERM| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:FIRST-TERM| (L)
    (COND ((ATOM L) L)
          ((AND (PAIRP L) (EQUAL (CAR L) '|:PS:|))
           (COND
            ((LESSP (|PS:FIND-ORDER| L) 0)
             (REDERR "Possible essential singularity"))
            (T (PREPSQXX (|PS:GET-TERM| L 0)))))
          (T
           (CONS (CAR L)
                 (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ARG (CDR L))
                   (COND ((NULL ARG) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ARG) (|PS:FIRST-TERM| ARG))
                                     (CAR ARG))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ARG (CDR ARG))
                   (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ARG) (|PS:FIRST-TERM| ARG)) (CAR ARG))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT '|PS:DIFFERENTIATE| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:DIFFERENTIATE| 'DEFINED-ON-LINE '465) 
(PUT '|PS:DIFFERENTIATE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:DIFFERENTIATE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:DIFFERENTIATE| (A V)
    ((LAMBDA (X)
       (COND
        ((EQCAR X 'DF)
         (RERROR 'TPS 18
                 (LIST "ps:differentiate: no rule to differentiate function"
                       (CAR A) "when it has" (DIFFERENCE (LENGTH A) 1)
                       "arguments")))
        (T X)))
     ((LAMBDA (*EXP) (PREPSQXX (SIMP* (LIST 'DF A V)))) NIL))) 
(PUT '|PS:EXPT-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:EXPT-CRULE| 'DEFINED-ON-LINE '477) 
(PUT '|PS:EXPT-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:EXPT-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:EXPT-CRULE| (A D N)
    (PROG (EFLG EXP1 EXP2 B PSVALUE)
      (SETQ B (CADR A))
      (COND
       ((OR (NOT (AND (PAIRP B) (EQUAL (CAR B) '|:PS:|))) (CONSTANTPSP B))
        (SETQ EFLG (EVALEQUAL B (PREPSQ (SIMP* (REVAL1 'E NIL)))))))
      (SETQ EXP1 (CADDR A))
      (COND
       ((AND (AND (PAIRP EXP1) (EQUAL (CAR EXP1) '|:PS:|)) (CONSTANTPSP EXP1))
        (SETQ EXP1 (|PS:VALUE| EXP1))))
      (PROG (ALGLIST* DMODE*)
        (SETQ ALGLIST* (CONS NIL NIL))
        (SETQ EXP2 (SIMP* EXP1)))
      (SETQ PSVALUE (|PS:ARG-VALUES| A))
      (COND
       ((AND (ATOM (CAR EXP2)) (ATOM (CDR EXP2)))
        (PROGN (SETQ EXP1 (CAR EXP2)) (SETQ EXP2 (CDR EXP2))))
       (T
        (RETURN
         (PROGN
          (SETQ EXP2
                  (|PS:COMPILE|
                   (COND (EFLG EXP1) (T (LIST 'TIMES EXP1 (LIST 'LOG B)))) D
                   N))
          (MAKE-PS (LIST 'EXP EXP2) PSVALUE D N)))))
      (SETQ B (|PS:COMPILE| B D N))
      (COND
       ((EQUAL EXP2 1)
        (COND
         ((EQUAL EXP1 NIL)
          (RETURN
           (COND
            ((|PS:ZEROP:| B) (RERROR 'TPS 19 "0**0 formed: ps:expt-crule"))
            (T 1))))
         ((EQUAL EXP1 1) (RETURN B))
         ((EQUAL EXP1 2) (SETQ A (MAKE-PS (LIST 'TIMES B B) PSVALUE D N)))
         ((EQUAL EXP1 (MINUS 1))
          (SETQ A (MAKE-PS (LIST 'QUOTIENT 1 B) PSVALUE D N)))
         (T (SETQ A (MAKE-PS (LIST 'EXPT B EXP1 1) PSVALUE D N)))))
       (T (SETQ A (MAKE-PS (LIST 'EXPT B EXP1 EXP2) PSVALUE D N))))
      (|PS:FIND-ORDER| A)
      (RETURN A))) 
(PUT 'EXPT '|PS:CRULE| '|PS:EXPT-CRULE|) 
(PROG (FN)
  (SETQ FN '(CSC COT SEC TAN CSCH COTH SECH TANH))
 LAB
  (COND ((NULL FN) (RETURN NIL)))
  ((LAMBDA (FN) (PUT FN '*SPECEXP 'TRIG-HYP-POLEFN)) (CAR FN))
  (SETQ FN (CDR FN))
  (GO LAB)) 
(PUT 'TRIG-HYP-POLEFN 'NUMBER-OF-ARGS 3) 
(PUT 'TRIG-HYP-POLEFN 'DEFINED-ON-LINE '528) 
(PUT 'TRIG-HYP-POLEFN 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'TRIG-HYP-POLEFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIG-HYP-POLEFN (FORM DEPVAR EXPPT)
    (PROG (ARG1 TMP F FP P R)
      (SETQ ARG1 (SUBST EXPPT DEPVAR (CADR FORM)))
      (SETQ F (CAR FORM))
      (COND
       ((OR (EQUAL F 'CSC) (EQUAL F 'COT) (EQUAL F 'CSCH) (EQUAL F 'COTH))
        (COND
         ((OR (EQUAL F 'CSC) (EQUAL F 'COT))
          (PROGN
           (SETQ FP 0)
           (SETQ P 'PI)
           (COND ((EQUAL F 'COT) (SETQ R 'TAN)) (T (SETQ R 'SIN)))
           NIL))
         (T
          (PROGN
           (SETQ FP 0)
           (SETQ P '(TIMES I PI))
           (COND ((EQUAL F 'CSCH) (SETQ R 'SINH)) (T (SETQ R 'TANH)))
           NIL))))
       ((OR (EQUAL F 'SEC) (EQUAL F 'TAN))
        (PROGN
         (SETQ FP '(QUOTIENT PI 2))
         (SETQ P 'PI)
         (COND ((EQUAL F 'SEC) (SETQ R 'COS)) (T (SETQ R 'COT)))
         NIL))
       ((OR (EQUAL F 'SECH) (EQUAL F 'TANH))
        (PROGN
         (SETQ FP '(TIMES I (QUOTIENT PI 2)))
         (SETQ P '(TIMES I PI))
         (COND ((EQUAL F 'SECH) (SETQ R 'COSH)) (T (SETQ R 'COTH)))
         NIL)))
      (SETQ TMP (REVAL1 (LIST 'QUOTIENT (LIST 'DIFFERENCE ARG1 FP) P) T))
      (COND
       ((FIXP TMP)
        (PROGN
         (SETQ TMP
                 (|PS:COMPILE| (LIST 'QUOTIENT 1 (CONS R (CDR FORM))) DEPVAR
                  EXPPT))
         (|PS:PUTV| TMP 4
                    (CONS (CAR FORM)
                          (CDR (|PS:VALUE| (CADR (CDR (|PS:GETV| TMP 6)))))))
         (RETURN TMP)
         NIL))
       (T (RETURN NIL))))) 
(PUT 'JACOBI-EXPANSIONFN 'NUMBER-OF-ARGS 3) 
(PUT 'JACOBI-EXPANSIONFN 'DEFINED-ON-LINE '582) 
(PUT 'JACOBI-EXPANSIONFN 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'JACOBI-EXPANSIONFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JACOBI-EXPANSIONFN (FORM DEPVAR EXPPT)
    (PROG (ARG1 ARG2 TMP F M N R)
      (SETQ ARG1 (SUBST EXPPT DEPVAR (CADR FORM)))
      (SETQ ARG2 (CADDR FORM))
      (SETQ F (CAR FORM))
      (COND
       ((DEPENDS ARG2 DEPVAR)
        (PROGN
         (SETQ ARG2 (REVAL1 (SUBST EXPPT DEPVAR ARG2) T))
         (COND
          ((OR (EQUAL ARG2 0) (EQUAL ARG2 1) (EQUAL ARG2 (MINUS 1)))
           (REDERR
            "Expansion of Jacobi elliptic functions not supported about unit or zero modulus"))
          (T (RETURN NIL)))
         NIL)))
      (PROG (*RATARG)
        (SETQ *RATARG T)
        (SETQ TMP (REVAL1 ARG1 T))
        (SETQ TMP (CDR (COEFFEVAL (LIST TMP (LIST 'ELLIPTICK ARG2))))))
      (COND ((GREATERP (LENGTH TMP) 1) (SETQ M (CADR TMP))) (T (SETQ M 0)))
      (SETQ N
              (REVAL1
               (LIST 'QUOTIENT (LIST 'TIMES 'I (CAR TMP))
                     (LIST '|ELLIPTICK'| ARG2))
               T))
      (COND ((NOT (AND (LEQ (LENGTH TMP) 2) (FIXP M) (FIXP N))) (RETURN NIL)))
      (COND
       ((OR (EQUAL F 'JACOBISN) (EQUAL F 'JACOBICN) (EQUAL F 'JACOBIDN))
        (PROGN (COND ((OR (NOT (EVENP M)) (EVENP N)) (RETURN NIL)))))
       ((OR (EQUAL F 'JACOBIDC) (EQUAL F 'JACOBINC) (EQUAL F 'JACOBISC))
        (PROGN (COND ((OR (EVENP M) (NOT (EVENP N))) (RETURN NIL)))))
       ((OR (EQUAL F 'JACOBICD) (EQUAL F 'JACOBISD) (EQUAL F 'JACOBIND))
        (PROGN (COND ((OR (EVENP M) (EVENP N)) (RETURN NIL)))))
       ((OR (NOT (EVENP M)) (NOT (EVENP N))) (RETURN NIL)))
      (SETQ R (GET F 'RECIPFN))
      (SETQ TMP
              (|PS:COMPILE| (LIST 'QUOTIENT 1 (CONS R (CDR FORM))) DEPVAR
               EXPPT))
      (|PS:PUTV| TMP 4
                 (CONS (CAR FORM)
                       (CDR (|PS:VALUE| (CADR (CDR (|PS:GETV| TMP 6)))))))
      (RETURN TMP))) 
(DEFLIST
 '((JACOBISN JACOBINS) (JACOBICN JACOBINC) (JACOBIDN JACOBIND)
   (JACOBINS JACOBISN) (JACOBINC JACOBICN) (JACOBIND JACOBIDN)
   (JACOBISC JACOBICS) (JACOBISD JACOBIDS) (JACOBICS JACOBISC)
   (JACOBICD JACOBIDC) (JACOBIDS JACOBISD) (JACOBIDC JACOBICD))
 'RECIPFN) 
(PROG (FN)
  (SETQ FN
          '(JACOBISN JACOBICN JACOBIDN JACOBINS JACOBINC JACOBIND JACOBISC
            JACOBISD JACOBICS JACOBICD JACOBIDS JACOBIDC))
 LAB
  (COND ((NULL FN) (RETURN NIL)))
  ((LAMBDA (FN) (PUT FN '*SPECEXP 'JACOBI-EXPANSIONFN)) (CAR FN))
  (SETQ FN (CDR FN))
  (GO LAB)) 
(PUT 'WEIER-SPECEXPFN 'NUMBER-OF-ARGS 3) 
(PUT 'WEIER-SPECEXPFN 'DEFINED-ON-LINE '633) 
(PUT 'WEIER-SPECEXPFN 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'WEIER-SPECEXPFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WEIER-SPECEXPFN (FORM DEPVAR EXPPT)
    (PROG (ARG1 ARG2 ARG3 TMP TMP1 F M N R)
      (SETQ ARG1 (SUBST EXPPT DEPVAR (CADR FORM)))
      (SETQ ARG2 (CADDR FORM))
      (SETQ ARG3 (CADDDR FORM))
      (SETQ F (CAR FORM))
      (COND
       ((OR (DEPENDS ARG2 DEPVAR) (DEPENDS ARG3 DEPVAR))
        (REDERR
         "Expansion not supported if arg 2 or 3 depends on expansion variable")))
      (COND
       ((OR (EQUAL F 'WEIERSTRASS1) (EQUAL F 'WEIERSTRASSZETA1))
        (PROGN
         (SETQ TMP (LIST 'LATTICE_OMEGA1 ARG2 ARG3))
         (SETQ ARG3 (LIST 'LATTICE_OMEGA3 ARG2 ARG3))
         (SETQ ARG2 TMP)
         NIL)))
      (PROG (*RATARG)
        (SETQ *RATARG T)
        (SETQ TMP (REVAL1 ARG1 T))
        (SETQ TMP (CDR (COEFFEVAL (LIST TMP ARG2)))))
      (COND ((GREATERP (LENGTH TMP) 1) (SETQ M (CADR TMP))) (T (SETQ M 0)))
      (SETQ N (REVAL1 (LIST 'QUOTIENT (REVAL1 (CAR TMP) T) ARG3) T))
      (COND
       ((NOT (AND (LEQ (LENGTH TMP) 2) (FIXP M) (FIXP N) (EVENP M) (EVENP N)))
        (RETURN (SIMPPSTAY2 FORM DEPVAR EXPPT NIL))))
      (COND
       ((OR (EQUAL F 'WEIERSTRASS) (EQUAL F 'WEIERSTRASSZETA)) (SETQ R 'RW*))
       (T (SETQ R 'RW1*)))
      (SETQ TMP (SIMPPSTAY2 (CONS R (CDR FORM)) DEPVAR EXPPT NIL))
      (SETQ TMP (|PS:QUOTIENT-CRULE| (LIST 'QUOTIENT 1 TMP) DEPVAR EXPPT))
      (SETQ TMP1 (CDR (|PS:VALUE| (CADR (CDR (|PS:GETV| TMP 6))))))
      (COND
       ((OR (EQUAL F 'WEIERSTRASSZETA) (EQUAL F 'WEIERSTRASSZETA1))
        (PROGN
         (SETQ TMP (|PS:INT-CRULE| (LIST 'INT TMP DEPVAR) DEPVAR EXPPT))
         (SETQ TMP
                 (REVAL1
                  (LIST 'DIFFERENCE
                        (LIST 'PLUS (LIST 'TIMES M (LIST 'ETA1 ARG2 ARG3))
                              (LIST 'TIMES N (LIST 'ETA3 ARG2 ARG3)))
                        TMP)
                  T))
         (SETQ TMP (|PS:COMPILE| TMP DEPVAR EXPPT))
         NIL)))
      (|PS:PUTV| TMP 4 (CONS (CAR FORM) TMP1))
      (RETURN TMP))) 
(PUT 'WEIERSTRASS '*SPECEXP 'WEIER-SPECEXPFN) 
(PUT 'WEIERSTRASS1 '*SPECEXP 'WEIER-SPECEXPFN) 
(PUT 'WEIERSTRASSZETA '*SPECEXP 'WEIER-SPECEXPFN) 
(PUT 'WEIERSTRASSZETA1 '*SPECEXP 'WEIER-SPECEXPFN) 
(PUT 'ELLIPI-MODULUS-EXP 'NUMBER-OF-ARGS 3) 
(PUT 'ELLIPI-MODULUS-EXP 'DEFINED-ON-LINE '700) 
(PUT 'ELLIPI-MODULUS-EXP 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'ELLIPI-MODULUS-EXP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLIPI-MODULUS-EXP (FORM DEPVAR ABOUT)
    (PROG (ARG TMP)
      (COND ((EQUAL (LENGTH FORM) 2) (SETQ TMP (CADR FORM)))
            (T (SETQ TMP (CADDR FORM))))
      (COND ((NOT (DEPENDS TMP DEPVAR)) (RETURN NIL)))
      (SETQ ARG (REVAL1 (SUBST ABOUT DEPVAR TMP) T))
      (COND
       ((AND (NEQ ARG 0) (NEQ ARG 1))
        (COND ((EQUAL (LENGTH FORM) 2) (RETURN NIL))
              (T
               (REDERR
                "Expansion of incomplete elliptic integrals only supported about zero modulus")))))
      (COND
       ((EQUAL ARG 1)
        (REDERR
         "Expansion of elliptic integrals about modulus k=1 not supported")))
      (COND
       ((EQUAL (CAR FORM) 'ELLIPTICE)
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'PLUS 1
                            (LIST 'MINUS
                                  (LIST 'TIMES (LIST 'EXPT (LIST 'SIN '**X) 2)
                                        (LIST 'EXPT TMP 2))))
                      (LIST 'QUOTIENT 1 2))))
       ((OR (EQUAL (CAR FORM) 'ELLIPTICF) (EQUAL (CAR FORM) 'ELLIPTICK))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'PLUS 1
                            (LIST 'MINUS
                                  (LIST 'TIMES (LIST 'EXPT (LIST 'SIN '**X) 2)
                                        (LIST 'EXPT TMP 2))))
                      (LIST 'QUOTIENT (MINUS 1) 2)))))
      (SETQ TMP (|PS:COMPILE| (LIST 'INT TMP '**X) DEPVAR ABOUT))
      (|PS:PUTV| TMP 4 FORM)
      (COND ((EQUAL (LENGTH FORM) 2) (SETQ ARG (LIST 'QUOTIENT 'PI 2)))
            (T (SETQ ARG (CADR FORM))))
      (RETURN (MAKE-PS (LIST 'DEF-INT TMP 0 ARG) FORM DEPVAR ABOUT)))) 
(PUT 'ELLIPTICK '*SPECEXP 'ELLIPI-MODULUS-EXP) 
(PUT 'ELLIPTICF '*SPECEXP 'ELLIPI-MODULUS-EXP) 
(PUT 'ELLIPTICE '*SPECEXP 'ELLIPI-MODULUS-EXP) 
(PROG (FN)
  (SETQ FN
          '(ARCSN ARCCN ARCDN ARCNS ARCNC ARCND ARCSC ARCSD ARCCS ARCCD ARCDS
            ARCDC))
 LAB
  (COND ((NULL FN) (RETURN NIL)))
  ((LAMBDA (FN) (PUT FN '*SPECEXP 'JACOBIINV-MODULUS-EXP)) (CAR FN))
  (SETQ FN (CDR FN))
  (GO LAB)) 
(PUT 'JACOBIINV-MODULUS-EXP 'NUMBER-OF-ARGS 3) 
(PUT 'JACOBIINV-MODULUS-EXP 'DEFINED-ON-LINE '756) 
(PUT 'JACOBIINV-MODULUS-EXP 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'JACOBIINV-MODULUS-EXP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JACOBIINV-MODULUS-EXP (FORM DEPVAR ABOUT)
    (PROG (ARG TMP)
      (COND ((EQUAL (LENGTH FORM) 2) (SETQ TMP (CADR FORM)))
            (T (SETQ TMP (CADDR FORM))))
      (COND ((NOT (DEPENDS TMP DEPVAR)) (RETURN NIL)))
      (SETQ ARG (REVAL1 (SUBST ABOUT DEPVAR TMP) T))
      (COND
       ((NEQ ARG 0)
        (REDERR
         "Expansion of inverse Jacobi functions only supported about modulus k=0")))
      (COND
       ((OR (EQUAL (CAR FORM) 'ARCSN) (EQUAL (CAR FORM) 'ARCNS))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'PLUS 1
                            (LIST 'MINUS
                                  (LIST 'TIMES (LIST 'EXPT (LIST 'SIN '**X) 2)
                                        (LIST 'EXPT TMP 2))))
                      (LIST 'QUOTIENT (MINUS 1) 2))))
       ((OR (EQUAL (CAR FORM) 'ARCCN) (EQUAL (CAR FORM) 'ARCNC))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'PLUS
                            (LIST 'PLUS 1 (LIST 'MINUS (LIST 'EXPT TMP 2))
                                  (LIST 'TIMES (LIST 'EXPT (LIST 'COS '**X) 2)
                                        (LIST 'EXPT TMP 2))))
                      (LIST 'QUOTIENT (MINUS 1) 2))))
       ((OR (EQUAL (CAR FORM) 'ARCDN) (EQUAL (CAR FORM) 'ARCND))
        (REDERR "arcdn and arcnd not defined for zero modulus"))
       ((OR (EQUAL (CAR FORM) 'ARCCD) (EQUAL (CAR FORM) 'ARCDC))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'PLUS 1
                            (LIST 'MINUS
                                  (LIST 'TIMES (LIST 'EXPT (LIST 'COS '**X) 2)
                                        (LIST 'EXPT TMP 2))))
                      (LIST 'QUOTIENT (MINUS 1) 2))))
       ((OR (EQUAL (CAR FORM) 'ARCSC) (EQUAL (CAR FORM) 'ARCCS))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'TIMES (LIST 'PLUS 1 (LIST 'EXPT '**X 2))
                            (LIST 'PLUS (LIST 'PLUS 1 (LIST 'EXPT '**X 2))
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT '**X 2)
                                              (LIST 'EXPT TMP 2)))))
                      (LIST 'QUOTIENT (MINUS 1) 2))))
       ((OR (EQUAL (CAR FORM) 'ARCSD) (EQUAL (CAR FORM) 'ARCDS))
        (SETQ TMP
                (LIST 'EXPT
                      (LIST 'TIMES
                            (LIST 'PLUS 1
                                  (LIST 'TIMES (LIST 'EXPT '**X 2)
                                        (LIST 'EXPT TMP 2)))
                            (LIST 'PLUS
                                  (LIST 'PLUS 1
                                        (LIST 'TIMES (LIST 'EXPT '**X 2)
                                              (LIST 'EXPT TMP 2)))
                                  (LIST 'MINUS (LIST 'EXPT '**X 2))))
                      (LIST 'QUOTIENT (MINUS 1) 2)))))
      (SETQ TMP (|PS:COMPILE| (LIST 'INT TMP '**X) DEPVAR ABOUT))
      (|PS:PUTV| TMP 4 FORM)
      (SETQ ARG
              (COND ((EQUAL (CAR FORM) 'ARCSN) (LIST 'ASIN (CADR FORM)))
                    ((EQUAL (CAR FORM) 'ARCNS)
                     (LIST 'ASIN (LIST 'QUOTIENT 1 (CADR FORM))))
                    ((OR (EQUAL (CAR FORM) 'ARCCN) (EQUAL (CAR FORM) 'ARCCD))
                     (LIST 'ACOS (CADR FORM)))
                    ((OR (EQUAL (CAR FORM) 'ARCNC) (EQUAL (CAR FORM) 'ARCDC))
                     (LIST 'ACOS (LIST 'QUOTIENT 1 (CADR FORM))))
                    ((OR (EQUAL (CAR FORM) 'ARCDS) (EQUAL (CAR FORM) 'ARCCS))
                     (LIST 'QUOTIENT 1 (CADR FORM)))
                    (T (CADR FORM))))
      (RETURN (MAKE-PS (LIST 'DEF-INT TMP 0 ARG) FORM DEPVAR ABOUT)))) 
(PUT 'SIMPPSTAY 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPPSTAY 'DEFINED-ON-LINE '834) 
(PUT 'SIMPPSTAY 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'SIMPPSTAY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPPSTAY (A)
    (COND ((EQUAL (LENGTH A) 3) (APPLY 'SIMPPSTAY1 A))
          (T
           (RERROR 'TPS 5
                   "Args should be <FORM>,<depvar>, and <point>:  simppstay")))) 
(PUT 'PSTAYLOR 'SIMPFN 'SIMPPSTAY) 
(PUT 'SIMPPSTAY1 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPPSTAY1 'DEFINED-ON-LINE '841) 
(PUT 'SIMPPSTAY1 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'SIMPPSTAY1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPPSTAY1 (FORM DEPVAR ABOUT)
    (COND
     ((EQUAL FORM NIL)
      (RERROR 'TPS 6 "Args should be <FORM>,<depvar>, and <point>: simppstay"))
     ((NOT (KERNP (SIMP* DEPVAR))) (TYPERR DEPVAR "kernel:  simppstay"))
     ((SMEMBER DEPVAR (SETQ ABOUT (PREPSQXX (SIMP* ABOUT))))
      (RERROR 'TPS 7
              "Expansion point depends on dependent variable:  simppstay"))
     (T
      (CONS
       (SIMPPSTAY2 (|PS:PRESIMP| FORM) DEPVAR
        (COND ((EQUAL ABOUT 'INFINITY) '|PS:INF|) (T ABOUT)) NIL)
       1)))) 
(PUT 'SIMPPSTAY2 'NUMBER-OF-ARGS 4) 
(PUT 'SIMPPSTAY2 'DEFINED-ON-LINE '852) 
(PUT 'SIMPPSTAY2 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT 'SIMPPSTAY2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPPSTAY2 (FORM DEPVAR ABOUT GAMMAFLG)
    (COND ((IDP FORM) (MAKE-PS-ID FORM DEPVAR ABOUT))
          ((OR (NUMBERP FORM)
               (AND (PAIRP FORM) (NEQ (CAR FORM) '|:PS:|)
                    (GET (CAR FORM) 'DNAME)))
           FORM)
          ((AND (PAIRP FORM) (EQUAL (CAR FORM) '|:PS:|))
           (COND
            ((AND (EQUAL (|PS:EXPANSION-POINT| FORM) ABOUT)
                  (EQUAL (|PS:DEPVAR| FORM) DEPVAR))
             FORM)
            (T (SIMPPSTAY2 (|PS:VALUE| FORM) DEPVAR ABOUT GAMMAFLG))))
          (T
           (PROG (PS DERIV PSORD TERM EPT EVAR)
             (SETQ PSORD 0)
             (COND
              ((EQUAL ABOUT '|PS:INF|)
               (PROGN
                (SETQ EVAR (GENSYM))
                (SETQ EPT 0)
                (COND (GAMMAFLG (RULE-LIST '(PSI_RULES) NIL)))
                (SETQ DERIV
                        (PREPSQ
                         (SIMP (SUBST (LIST 'QUOTIENT 1 EVAR) DEPVAR FORM))))
                (COND (GAMMAFLG (RULE-LIST '(PSI_RULES) T)))))
              (T
               (PROGN (SETQ DERIV FORM) (SETQ EPT ABOUT) (SETQ EVAR DEPVAR))))
             (COND (GAMMAFLG (RULE-LIST '(PSI_RULES) T)))
             (SETQ TERM (SIMP (SUBST EPT EVAR DERIV)))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (AND (EQUAL (CAR TERM) NIL) (NEQ DERIV 0)))
                 (RETURN NIL)))
               (PROGN
                (SETQ PSORD (PLUS PSORD 1))
                (COND (GAMMAFLG (RULE-LIST '(PSI_RULES) NIL)))
                (SETQ DERIV
                        (PREPSQ
                         (SIMP (LIST 'QUOTIENT (LIST 'DF DERIV EVAR) PSORD))))
                (COND (GAMMAFLG (RULE-LIST '(PSI_RULES) T)))
                (SETQ TERM (SIMP (SUBST EPT EVAR DERIV)))
                NIL)
               (GO WHILELABEL))
             (COND ((EQUAL DERIV 0) (RETURN NIL)))
             (SETQ PS
                     (MAKE-PS
                      (LIST (COND (GAMMAFLG '|PS:GAMMA|) (T '|PS:TAYLOR|)) EVAR
                            EPT DERIV)
                      FORM DEPVAR ABOUT))
             (|PS:PUTV| PS 0 PSORD)
             (|PS:SET-TERM| PS PSORD TERM)
             (RETURN PS))))) 
(PUT '|PS:GAMMA-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:GAMMA-CRULE| 'DEFINED-ON-LINE '895) 
(PUT '|PS:GAMMA-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:GAMMA-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:GAMMA-CRULE| (A D N)
    (COND
     ((EQUAL N '|PS:INF|)
      (RERROR 'TPS 106
              "Expansion of gamma functions about infinity not yet supported."))
     (T
      (PROG (OP ARG EXPPT RES FN GAM)
        (SETQ OP (CAR A))
        (SETQ ARG (CADR A))
        (SETQ FN
                (COND
                 ((AND (PAIRP ARG) (EQUAL (CAR ARG) '|:PS:|)) (|PS:VALUE| ARG))
                 ((PAIRP ARG) (|PS:ARG-VALUES| ARG)) (T ARG)))
        (SETQ EXPPT (REVAL1 (SUBST N D FN) T))
        (COND
         ((OR (NOT (FIXP EXPPT)) (GREATERP EXPPT 0))
          (RETURN (SIMPPSTAY2 (LIST OP FN) D N T)))
         (T
          (PROGN
           (SETQ GAM
                   (SIMPPSTAY2
                    (LIST OP (REVAL1 (LIST 'PLUS FN (DIFFERENCE 1 EXPPT)) T)) D
                    N T))
           (SETQ ARG
                   (COND ((AND (PAIRP ARG) (EQUAL (CAR ARG) '|:PS:|)) ARG)
                         (T (|PS:COMPILE| ARG D N))))
           (COND
            ((EQUAL OP 'GAMMA)
             (PROGN
              (SETQ RES (|PS:COMPILE| (LIST 'QUOTIENT GAM ARG) D N))
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE (MINUS EXPPT) I)) (RETURN NIL)))
                (SETQ RES
                        (|PS:COMPILE| (LIST 'QUOTIENT RES (LIST 'PLUS ARG I)) D
                         N))
                (SETQ I (PLUS2 I 1))
                (GO LAB))))
            (T
             (PROGN
              (SETQ RES
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I 1)
                        (COND
                         ((MINUSP (DIFFERENCE (MINUS EXPPT) I)) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         (|PS:COMPILE|
                                          (LIST 'QUOTIENT 1 (LIST 'PLUS ARG I))
                                          D N)
                                         NIL)))
                       LOOPLABEL
                        (SETQ I (PLUS2 I 1))
                        (COND
                         ((MINUSP (DIFFERENCE (MINUS EXPPT) I))
                          (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 (|PS:COMPILE|
                                  (LIST 'QUOTIENT 1 (LIST 'PLUS ARG I)) D N)
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ RES (CONS (|PS:COMPILE| (LIST 'QUOTIENT 1 ARG) D N) RES))
              (SETQ RES
                      (COND ((EQUAL EXPPT 0) (CAR RES)) (T (CONS 'PLUS RES))))
              (SETQ RES (|PS:COMPILE| (LIST 'DIFFERENCE GAM RES) D N))))))))
        (|PS:PUTV| RES 4 (|PS:ARG-VALUES| A))
        (RETURN RES))))) 
(PUT '|PS:POLYGAMMA-CRULE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:POLYGAMMA-CRULE| 'DEFINED-ON-LINE '934) 
(PUT '|PS:POLYGAMMA-CRULE| 'DEFINED-IN-FILE 'TPS/TPSCOMP.RED) 
(PUT '|PS:POLYGAMMA-CRULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:POLYGAMMA-CRULE| (A D N)
    (COND
     ((EQUAL N '|PS:INF|)
      (RERROR 'TPS 107
              "Expansion of polygamma function about infinity not yet supported."))
     (T
      (PROG (EXPPT ORD ARG FN GAM RES FAC)
        (SETQ ORD (REVAL1 (CADR A) T))
        (COND
         ((OR (NOT (FIXP ORD)) (LESSP ORD 0))
          (RERROR 'TPS 108
                  "First argument of polygamma must be a non-negative integer.")))
        (SETQ ARG (CADDR A))
        (SETQ FN
                (COND
                 ((AND (PAIRP ARG) (EQUAL (CAR ARG) '|:PS:|)) (|PS:VALUE| ARG))
                 ((PAIRP ARG) (|PS:ARG-VALUES| ARG)) (T ARG)))
        (SETQ EXPPT (REVAL1 (SUBST N D FN) T))
        (COND
         ((OR (NOT (FIXP EXPPT)) (GREATERP EXPPT 0))
          (RETURN (SIMPPSTAY2 (LIST 'POLYGAMMA ORD FN) D N T)))
         (T
          (PROGN
           (SETQ GAM
                   (SIMPPSTAY2
                    (LIST 'POLYGAMMA ORD
                          (REVAL1 (LIST 'PLUS FN (DIFFERENCE 1 EXPPT)) T))
                    D N T))
           (SETQ ARG (|PS:COMPILE| (LIST 'MINUS ARG) D N))
           (SETQ FAC (FACTORIAL ORD))
           (SETQ ORD (MINUS (PLUS ORD 1)))
           (SETQ RES
                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                     (SETQ I 1)
                     (COND
                      ((MINUSP (DIFFERENCE (MINUS EXPPT) I)) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (|PS:COMPILE|
                                       (LIST 'TIMES FAC
                                             (LIST 'EXPT
                                                   (LIST 'PLUS ARG (MINUS I))
                                                   ORD))
                                       D N)
                                      NIL)))
                    LOOPLABEL
                     (SETQ I (PLUS2 I 1))
                     (COND
                      ((MINUSP (DIFFERENCE (MINUS EXPPT) I))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              (|PS:COMPILE|
                               (LIST 'TIMES FAC
                                     (LIST 'EXPT (LIST 'PLUS ARG (MINUS I))
                                           ORD))
                               D N)
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ RES
                   (CONS
                    (|PS:COMPILE| (LIST 'TIMES FAC (LIST 'EXPT ARG ORD)) D N)
                    RES))
           (SETQ RES (|PS:COMPILE| (CONS 'PLUS (CONS GAM RES)) D N))
           (|PS:PUTV| RES 4 (|PS:ARG-VALUES| A))
           (RETURN RES)))))))) 
(PUT 'GAMMA '|PS:CRULE| '|PS:GAMMA-CRULE|) 
(PUT 'PSI '|PS:CRULE| '|PS:GAMMA-CRULE|) 
(PUT 'POLYGAMMA '|PS:CRULE| '|PS:POLYGAMMA-CRULE|) 
(ENDMODULE) 