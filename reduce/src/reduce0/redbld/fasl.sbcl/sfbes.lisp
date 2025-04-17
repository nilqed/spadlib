(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFBES)) 
(IMPORTS
 (LIST 'COMPLEX*ON*SWITCH 'COMPLEX*OFF*SWITCH 'COMPLEX*RESTORE*SWITCH 'SQ2BF*
       'SF*EVAL)) 
(GLOBAL '(!LOGTEN)) 
(AEVAL (OPERATOR (LIST 'BESSELJ 'BESSELY 'BESSELI 'BESSELK 'HANKEL1 'HANKEL2))) 
(AEVAL (OPERATOR (LIST '|10J|))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (|10J| (QUOTIENT 1 2) (~ X))
      (TIMES (SQRT (QUOTIENT 2 (TIMES PI X))) (SIN X)))
     (REPLACEBY (|10J| (MINUS (QUOTIENT 1 2)) (~ X))
      (TIMES (SQRT (QUOTIENT 2 (TIMES PI X))) (COS X)))
     (REPLACEBY (|10J| (QUOTIENT 3 2) (~ X))
      (TIMES (SQRT (QUOTIENT 2 (TIMES PI X)))
             (DIFFERENCE (QUOTIENT (SIN X) X) (COS X))))
     (REPLACEBY (|10J| (MINUS (QUOTIENT 3 2)) (~ X))
      (TIMES (SQRT (QUOTIENT 2 (TIMES PI X)))
             (DIFFERENCE (MINUS (QUOTIENT (COS X) X)) (SIN X)))))))) 
(AEVAL 'NIL) 
(PUT '|COMPUTE:CVPR130108| 'NUMBER-OF-ARGS 2) 
(FLAG '(|COMPUTE:CVPR130108|) 'OPFN) 
(PUT '|COMPUTE:CVPR130108| 'DEFINED-ON-LINE '75) 
(PUT '|COMPUTE:CVPR130108| 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT '|COMPUTE:CVPR130108| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |COMPUTE:CVPR130108| (NU X)
    (PROG (NUMF N)
      (COND
       ((EVALEQUAL (AEVAL NU) (AEVAL (LIST 'QUOTIENT 1 2)))
        (PROGN (RETURN (AEVAL (LIST '|10J| (LIST 'QUOTIENT 1 2) X))))))
      (COND
       ((EVALEQUAL (AEVAL NU) (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 2))))
        (PROGN
         (RETURN (AEVAL (LIST '|10J| (LIST 'MINUS (LIST 'QUOTIENT 1 2)) X))))))
      (COND
       ((EVALEQUAL (AEVAL NU) (AEVAL (LIST 'QUOTIENT 3 2)))
        (PROGN (RETURN (AEVAL (LIST '|10J| (LIST 'QUOTIENT 3 2) X))))))
      (COND
       ((EVALEQUAL (AEVAL NU) (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 3 2))))
        (PROGN
         (RETURN (AEVAL (LIST '|10J| (LIST 'MINUS (LIST 'QUOTIENT 3 2)) X))))))
      (COND
       ((AND (FIXP (REVALX (LIST 'TIMES NU 2)))
             (EVALGREATERP (AEVAL (LIST 'TIMES 2 NU)) 0))
        (PROGN
         (SETQ NUMF (AEVAL (LIST 'NUM NU)))
         (PROG (N)
           (SETQ N 5)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'TIMES 2 (LIST 'DIFFERENCE (AEVAL* NUMF) N)))
             (RETURN NIL)))
           (PROGN
            (SETQ NU (AEVAL* (LIST 'QUOTIENT N 2)))
            (SETK (LIST '|10J| NU X)
                  (AEVAL*
                   (LIST 'TIMES (LIST 'QUOTIENT 1 X)
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2 (LIST 'DIFFERENCE NU 1)
                                     (LIST '|10J| (LIST 'DIFFERENCE NU 1) X))
                               (LIST 'TIMES X
                                     (LIST '|10J| (LIST 'DIFFERENCE NU 2)
                                           X))))))
            (AEVAL* 'NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 2)))
                    N))
           (GO LAB))
         (RETURN (AEVAL (LIST '|10J| NU X)))
         (AEVAL 'NIL)))
       ((AND (FIXP (REVALX (LIST 'MINUS (LIST 'TIMES 2 NU))))
             (EVALLESSP (AEVAL (LIST 'TIMES 2 NU)) 0))
        (PROGN
         (SETQ NUMF (AEVAL (LIST 'MINUS (LIST 'NUM NU))))
         (PROG (N)
           (SETQ N 5)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'TIMES 2 (LIST 'DIFFERENCE (AEVAL* NUMF) N)))
             (RETURN NIL)))
           (PROGN
            (SETQ NU (AEVAL* (LIST 'MINUS (LIST 'QUOTIENT N 2))))
            (SETK (LIST '|10J| NU X)
                  (AEVAL*
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES 2 (LIST 'PLUS NU 1)
                               (LIST 'QUOTIENT
                                     (LIST '|10J| (LIST 'PLUS NU 1) X) X))
                         (LIST '|10J| (LIST 'PLUS NU 2) X))))
            (AEVAL* 'NIL))
           (SETQ N
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 2)))
                    N))
           (GO LAB))
         (RETURN (AEVAL (LIST '|10J| NU X)))
         (AEVAL 'NIL)))))) 
(FLAG '(BESSELJ BESSELY BESSELI BESSELK HANKEL1 HANKEL2) 'SPECFN) 
(DEFLIST
 '((BESSELJ 2) (BESSELY 2) (BESSELI 2) (BESSELK 2) (HANKEL1 2) (HANKEL2 2))
 'NUMBER-OF-ARGS) 
(AEVAL
 (LET
  '((REPLACEBY (BESSELJ (~ N) (~ X))
     (WHEN (|COMPUTE:CVPR130108| N X)
      (AND (NUMBERP N) (EQUAL (DEN N) 2) (FIXP (NUM N))
           (LESSP (ABS (NUM N)) 6))))))) 
(FLAG '(DO*J DO*Y DO*I) 'OPFN) 
(SETK 'BESSEL*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) 0)
                   (LIST 'WHEN 1 (LIST 'EQUAL 'N 0)))
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) 0)
                   (LIST 'WHEN 0
                         (LIST 'AND (LIST 'NUMBERP 'N) (LIST 'NEQ 'N 0))))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST '~ 'N) 0) 'INFINITY)
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST 'QUOTIENT 1 2) (LIST '~ 'Z))
                   (LIST 'TIMES
                         (LIST 'SQRT (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                         (LIST 'SIN 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'BESSELJ (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST '~ 'Z))
                   (LIST 'TIMES
                         (LIST 'SQRT (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                         (LIST 'COS 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'BESSELY (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST '~ 'Z))
                   (LIST 'TIMES
                         (LIST 'SQRT (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                         (LIST 'SIN 'Z)))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST 'QUOTIENT 1 2) (LIST '~ 'Z))
                   (LIST 'MINUS
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                               (LIST 'COS 'Z))))
             (LIST 'REPLACEBY (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 'Z)))
                               (LIST 'EXPT 'E (LIST 'MINUS 'Z)))
                         (LIST 'OR (LIST 'EQUAL 'N (LIST 'QUOTIENT 1 2))
                               (LIST 'EQUAL 'N
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 2))))))
             (LIST 'REPLACEBY (LIST 'BESSELI (LIST 'QUOTIENT 1 2) (LIST '~ 'Z))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'TIMES 'PI 2 'Z)))
                         (LIST 'DIFFERENCE (LIST 'EXPT 'E 'Z)
                               (LIST 'EXPT 'E (LIST 'MINUS 'Z)))))
             (LIST 'REPLACEBY
                   (LIST 'BESSELI (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST '~ 'Z))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'TIMES 'PI 2 'Z)))
                         (LIST 'PLUS (LIST 'EXPT 'E 'Z)
                               (LIST 'EXPT 'E (LIST 'MINUS 'Z)))))
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'BESSELJ (LIST 'MINUS 'N) 'Z))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'EQUAL 'N (LIST 'FLOOR 'N))
                               (LIST 'LESSP 'N 0))))
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'BESSELJ 'N (LIST 'MINUS 'Z)))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'EQUAL 'N (LIST 'FLOOR 'N))
                               (LIST 'NUMBERP 'Z)
                               (LIST 'LESSP (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'BESSELY (LIST 'MINUS 'N) 'Z))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'EQUAL 'N (LIST 'FLOOR 'N))
                               (LIST 'LESSP 'N 0))))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'BESSELJ 'N 'Z)
                                           (LIST 'COS (LIST 'TIMES 'N 'PI)))
                                     (LIST 'BESSELJ (LIST 'MINUS 'N) 'Z))
                               (LIST 'SIN (LIST 'TIMES 'N 'PI)))
                         (LIST 'AND (LIST 'NOT (LIST 'SYMBOLIC '*ROUNDED))
                               (LIST 'NUMBERP 'N)
                               (LIST 'OR (LIST 'NEQ (LIST 'IMPART 'N) 0)
                                     (LIST 'NOT
                                           (LIST 'EQUAL (LIST 'REPART 'N)
                                                 (LIST 'FLOOR
                                                       (LIST 'REPART 'N))))))))
             (LIST 'REPLACEBY (LIST 'HANKEL1 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                               (LIST 'QUOTIENT (LIST 'EXP (LIST 'TIMES 'I 'Z))
                                     'I))
                         (LIST 'AND (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'EQUAL 'N (LIST 'QUOTIENT 1 2)))))
             (LIST 'REPLACEBY (LIST 'HANKEL2 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z)))
                               (LIST 'QUOTIENT
                                     (LIST 'EXP
                                           (LIST 'MINUS (LIST 'TIMES 'I 'Z)))
                                     (LIST 'MINUS 'I)))
                         (LIST 'AND (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'EQUAL 'N (LIST 'QUOTIENT 1 2)))))
             (LIST 'REPLACEBY (LIST 'HANKEL1 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS (LIST 'BESSELJ 'N 'Z)
                               (LIST 'TIMES 'I (LIST 'BESSELY 'N 'Z)))
                         (LIST 'AND (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'NOT (LIST 'SYMBOLIC '*ROUNDED)))))
             (LIST 'REPLACEBY (LIST 'HANKEL2 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'BESSELJ 'N 'Z)
                               (LIST 'TIMES 'I (LIST 'BESSELY 'N 'Z)))
                         (LIST 'AND (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'NOT (LIST 'SYMBOLIC '*ROUNDED)))))
             (LIST 'REPLACEBY (LIST 'BESSELI (LIST '~ 'N) 0)
                   (LIST 'WHEN
                         (LIST 'COND
                               (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 0) 1)
                               (LIST 'T 0))
                         (LIST 'NUMBERP 'N)))
             (LIST 'REPLACEBY (LIST 'BESSELI (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'BESSELI (LIST 'MINUS 'N) 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'EQUAL 'N (LIST 'FLOOR 'N))
                               (LIST 'LESSP 'N 0))))
             (LIST 'REPLACEBY (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'BESSELK (LIST 'MINUS 'N) 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'EQUAL 'N (LIST 'FLOOR 'N))
                               (LIST 'LESSP 'N 0))))
             (LIST 'REPLACEBY (LIST 'BESSELK (LIST '~ 'N) 0) 'INFINITY)
             (LIST 'REPLACEBY (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'PI 2)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'BESSELI (LIST 'MINUS 'N) 'Z)
                                           (LIST 'BESSELI 'N 'Z))
                                     (LIST 'SIN (LIST 'TIMES 'N 'PI))))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'NOT (LIST 'EQUAL 'N (LIST 'FLOOR 'N))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'DIFFERENCE (LIST 'BESSELJ (LIST 'DIFFERENCE 'N 1) 'Z)
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'Z)
                               (LIST 'BESSELJ 'N 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'DIFFERENCE (LIST 'BESSELY (LIST 'DIFFERENCE 'N 1) 'Z)
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'Z)
                               (LIST 'BESSELY 'N 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS
                               (LIST 'BESSELK (LIST 'DIFFERENCE 'N 1) 'Z))
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'Z)
                               (LIST 'BESSELK 'N 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'HANKEL1 (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'DIFFERENCE (LIST 'HANKEL1 (LIST 'DIFFERENCE 'N 1) 'Z)
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'Z)
                               (LIST 'HANKEL1 'N 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'HANKEL2 (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'DIFFERENCE (LIST 'HANKEL2 (LIST 'DIFFERENCE 'N 1) 'Z)
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'Z)
                               (LIST 'HANKEL2 'N 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'BESSELI (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'BESSELI (LIST 'DIFFERENCE 'N 1) 'Z)
                               (LIST 'BESSELI (LIST 'PLUS 'N 1) 'Z))
                         2))
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'DO*J 'N 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N) (LIST 'NUMBERP 'Z)
                               (LIST 'SYMBOLIC '*ROUNDED))))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'DO*Y 'N 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N) (LIST 'NUMBERP 'Z)
                               (LIST 'SYMBOLIC '*ROUNDED))))
             (LIST 'REPLACEBY (LIST 'BESSELI (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'DO*I 'N 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N) (LIST 'NUMBERP 'Z)
                               (LIST 'SYMBOLIC '*ROUNDED))))))) 
(AEVAL (LET '(BESSEL*RULES))) 
(PUT 'DO*J 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*J) 'OPFN) 
(PUT 'DO*J 'DEFINED-ON-LINE '250) 
(PUT 'DO*J 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'DO*J 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*J (N Z)
    (COND
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART N)) 0)
           (EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
           (EVALGREATERP (AEVAL (LIST 'REPART Z)) 0))
      (AEVAL (SF*EVAL 'J*CALC*S (LIST 'LIST N Z))))
     (T (AEVAL (SF*EVAL 'J*CALC (LIST 'LIST N Z)))))) 
(PUT 'DO*Y 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*Y) 'OPFN) 
(PUT 'DO*Y 'DEFINED-ON-LINE '255) 
(PUT 'DO*Y 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'DO*Y 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*Y (N Z)
    (COND
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART N)) 0)
           (EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
           (EVALEQUAL (AEVAL N) (AEVAL (LIST 'FLOOR N))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART Z)) 0)
        (AEVAL (SF*EVAL 'Y*CALC*SC (LIST 'LIST N Z))))
       (T (AEVAL (SF*EVAL 'Y*CALC*S (LIST 'LIST N Z (LIST 'LIST)))))))
     ((OR (EVALNEQ (AEVAL (LIST 'IMPART N)) 0)
          (EVALNEQ (AEVAL N) (AEVAL (LIST 'FLOOR N))))
      (AEVAL (LIST 'Y*REEXPRESS N Z)))
     (T (AEVAL (SF*EVAL 'Y*CALC (LIST 'LIST N Z)))))) 
(PUT 'DO*I 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*I) 'OPFN) 
(PUT 'DO*I 'DEFINED-ON-LINE '267) 
(PUT 'DO*I 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'DO*I 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*I (N Z)
    (COND
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART N)) 0)
           (EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
           (EVALGREATERP (AEVAL (LIST 'REPART Z)) 0))
      (AEVAL (SF*EVAL 'I*CALC*S (LIST 'LIST N Z))))
     (T (AEVAL (SF*EVAL 'I*CALC (LIST 'LIST N Z)))))) 
(PUT 'J*CALC*S 'NUMBER-OF-ARGS 2) 
(FLAG '(J*CALC*S) 'OPFN) 
(PUT 'J*CALC*S 'DEFINED-ON-LINE '273) 
(PUT 'J*CALC*S 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'J*CALC*S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE J*CALC*S (N Z)
    (PROG (N0 Z0 FKGAMNK RESULT ALGLIST* PREPRE PRECOM)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PRECOM 0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((AND (EVALGREATERP (AEVAL Z) (TIMES 2 PREPRE))
             (EVALGREATERP (AEVAL Z) (AEVAL (LIST 'TIMES 2 N)))
             (EVALNEQ
              (SETQ RESULT (AEVAL (SF*EVAL 'ASYMP*J*CALC (LIST 'LIST N Z))))
              (AEVAL (LIST 'LIST))))
        (PROGN
         (AEVAL (LIST 'PRECISION PREPRE))
         (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
         (RETURN (AEVAL RESULT)))))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL
         (LIST 'PRECISION
               (LIST 'PLUS !NFPD 3
                     (LIST 'FLOOR (LIST 'QUOTIENT (LIST 'ABS N) 10))))))
       (T
        (AEVAL
         (LIST 'PRECISION
               (LIST 'PLUS PREPRE 6
                     (LIST 'FLOOR (LIST 'QUOTIENT (LIST 'ABS N) 10)))))))
      (SETQ N0 (AEVAL N))
      (SETQ Z0 (AEVAL Z))
      (SETQ FKGAMNK (AEVAL (LIST 'GAMMA (LIST 'PLUS N 1))))
      (SETQ RESULT
              (AEVAL
               (SF*EVAL 'J*CALC*S*SUB (LIST 'LIST N0 Z0 FKGAMNK PREPRE))))
      (AEVAL (LIST 'PRECISION PREPRE))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'J*CALC*S*SUB 'NUMBER-OF-ARGS 4) 
(PUT 'J*CALC*S*SUB 'DEFINED-ON-LINE '297) 
(PUT 'J*CALC*S*SUB 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'J*CALC*S*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE J*CALC*S*SUB (N Z FKGAMNK PREPRE)
    (PROG (RESULT ADMISSABLE THIS MODIFY ZFSQ ZFSQP KNK AZFSQ K)
      (SETQ N
              (COND ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* N))))))
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ FKGAMNK
              (COND ((FIXP FKGAMNK) (CONS '|:RD:| (CONS FKGAMNK 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* FKGAMNK))))))
      (SETQ MODIFY
              (|EXP:|
               (NORMBF
                (|ROUND:MT|
                 (|TIMES:|
                  (|LOG:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                          (PLUS |:BPREC:| 2))
                  N)
                 |:BPREC:|))
               |:BPREC:|))
      (SETQ ZFSQ
              (|MINUS:|
               (NORMBF
                (|DIVIDE:| (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|))
                           (CONS '|:RD:| (CONS 4 0)) |:BPREC:|))))
      (SETQ AZFSQ (|ABS:| ZFSQ))
      (SETQ RESULT (NORMBF (|DIVIDE:| BFONE* FKGAMNK |:BPREC:|)))
      (SETQ K BFONE*)
      (SETQ ZFSQP ZFSQ)
      (SETQ FKGAMNK
              (NORMBF
               (|ROUND:MT| (|TIMES:| FKGAMNK (PLUBF N BFONE*)) |:BPREC:|)))
      (COND
       ((|LESSP:| (|ABS:| RESULT) BFONE*)
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:| BFONE*
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:|
                                (|EXP:|
                                 (NORMBF
                                  (|ROUND:MT|
                                   (|TIMES:| (FL2BF !LOGTEN)
                                             (CONS '|:RD:|
                                                   (CONS
                                                    (PLUS PREPRE
                                                          (LENGTH
                                                           (EXPLODE FKGAMNK)))
                                                    0)))
                                   |:BPREC:|))
                                 8)
                                MODIFY)
                               |:BPREC:|))
                             |:BPREC:|)))))
       (T
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:| BFONE*
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:|
                                (|EXP:|
                                 (NORMBF
                                  (|ROUND:MT|
                                   (|TIMES:| (FL2BF !LOGTEN)
                                             (CONS '|:RD:|
                                                   (CONS
                                                    (PLUS PREPRE
                                                          (LENGTH
                                                           (EXPLODE
                                                            (PLUS 1
                                                                  (|CONV:BF2I|
                                                                   (|ABS:|
                                                                    RESULT))))))
                                                    0)))
                                   |:BPREC:|))
                                 8)
                                MODIFY)
                               |:BPREC:|))
                             |:BPREC:|))))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ THIS (NORMBF (|DIVIDE:| ZFSQP FKGAMNK |:BPREC:|)))
         (SETQ RESULT (PLUBF RESULT THIS))
         (SETQ K (PLUBF K BFONE*))
         (SETQ KNK (NORMBF (|ROUND:MT| (|TIMES:| K (PLUBF N K)) |:BPREC:|)))
         (COND
          ((|GREATERP:| AZFSQ KNK)
           (PRECISION
            (PLUS (PRECISION 0)
                  (LENGTH
                   (EXPLODE
                    (PLUS 1
                          (|CONV:BF2I|
                           (NORMBF (|DIVIDE:| AZFSQ KNK |:BPREC:|))))))))))
         (SETQ ZFSQP (NORMBF (|ROUND:MT| (|TIMES:| ZFSQP ZFSQ) |:BPREC:|)))
         (SETQ FKGAMNK (NORMBF (|ROUND:MT| (|TIMES:| FKGAMNK KNK) |:BPREC:|))))
        (GO WHILELABEL))
      (SETQ RESULT (NORMBF (|ROUND:MT| (|TIMES:| RESULT MODIFY) |:BPREC:|)))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(FLAG '(J*CALC*S*SUB) 'OPFN) 
(PUT 'ASYMP*J*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(ASYMP*J*CALC) 'OPFN) 
(PUT 'ASYMP*J*CALC 'DEFINED-ON-LINE '342) 
(PUT 'ASYMP*J*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*J*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMP*J*CALC (N Z)
    (PROG (RESULT ADMISSABLE ALGLIST* MODIFY CHI MU P Q N0 Z0 PREPRE)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 5))))
       (T (AEVAL (LIST 'PRECISION (PLUS PREPRE 8)))))
      (SETQ MODIFY (AEVAL (LIST 'SQRT (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI Z)))))
      (SETQ ADMISSABLE
              (AEVAL (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 5)))))
      (SETQ CHI
              (AEVAL
               (LIST 'DIFFERENCE Z
                     (LIST 'TIMES
                           (LIST 'PLUS (LIST 'QUOTIENT N 2)
                                 (LIST 'QUOTIENT 1 4))
                           'PI))))
      (SETQ MU (AEVAL (LIST 'TIMES 4 (LIST 'EXPT N 2))))
      (SETQ N0 (AEVAL N))
      (SETQ Z0 (AEVAL Z))
      (SETQ P (AEVAL (ASYMP*P N0 Z0 MU ADMISSABLE)))
      (COND
       ((EVALNEQ (AEVAL P) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ Q (AEVAL (ASYMP*Q N0 Z0 MU ADMISSABLE)))
         (COND
          ((EVALNEQ (AEVAL Q) (AEVAL (LIST 'LIST)))
           (SETQ RESULT
                   (AEVAL
                    (LIST 'TIMES MODIFY
                          (LIST 'DIFFERENCE
                                (LIST 'TIMES (LIST 'FIRST P) (LIST 'COS CHI))
                                (LIST 'TIMES (LIST 'FIRST Q)
                                      (LIST 'SIN CHI)))))))
          (T (SETQ RESULT (AEVAL (LIST 'LIST)))))))
       (T (SETQ RESULT (AEVAL (LIST 'LIST)))))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'ASYMP*Y*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(ASYMP*Y*CALC) 'OPFN) 
(PUT 'ASYMP*Y*CALC 'DEFINED-ON-LINE '367) 
(PUT 'ASYMP*Y*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*Y*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMP*Y*CALC (N Z)
    (PROG (RESULT ADMISSABLE ALGLIST* MODIFY CHI MU P Q N0 Z0 PREPRE)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 5))))
       (T (AEVAL (LIST 'PRECISION (PLUS PREPRE 8)))))
      (SETQ MODIFY (AEVAL (LIST 'SQRT (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI Z)))))
      (SETQ ADMISSABLE
              (AEVAL (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 5)))))
      (SETQ CHI
              (AEVAL
               (LIST 'DIFFERENCE Z
                     (LIST 'TIMES
                           (LIST 'PLUS (LIST 'QUOTIENT N 2)
                                 (LIST 'QUOTIENT 1 4))
                           'PI))))
      (SETQ MU (AEVAL (LIST 'TIMES 4 (LIST 'EXPT N 2))))
      (SETQ N0 (AEVAL N))
      (SETQ Z0 (AEVAL Z))
      (SETQ P (AEVAL (ASYMP*P N0 Z0 MU ADMISSABLE)))
      (COND
       ((EVALNEQ (AEVAL P) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ Q (AEVAL (ASYMP*Q N0 Z0 MU ADMISSABLE)))
         (COND
          ((EVALNEQ (AEVAL Q) (AEVAL (LIST 'LIST)))
           (SETQ RESULT
                   (AEVAL
                    (LIST 'TIMES MODIFY
                          (LIST 'PLUS
                                (LIST 'TIMES (LIST 'FIRST P) (LIST 'SIN CHI))
                                (LIST 'TIMES (LIST 'FIRST Q)
                                      (LIST 'COS CHI)))))))
          (T (SETQ RESULT (AEVAL (LIST 'LIST)))))))
       (T (SETQ RESULT (AEVAL (LIST 'LIST)))))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'ASYMP*P 'NUMBER-OF-ARGS 4) 
(PUT 'ASYMP*P 'DEFINED-ON-LINE '392) 
(PUT 'ASYMP*P 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ASYMP*P (N Z MU ADMISSABLE)
    (PROG (RESULT THIS PREV ZSQ ZSQP AJ2T K F)
      (SETQ K 0)
      (SETQ F 0)
      (SETQ N
              (COND ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* N))))))
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ MU
              (COND ((FIXP MU) (CONS '|:RD:| (CONS MU 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* MU))))))
      (SETQ ADMISSABLE
              (COND ((FIXP ADMISSABLE) (CONS '|:RD:| (CONS ADMISSABLE 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* ADMISSABLE))))))
      (SETQ K 2)
      (SETQ F
              (PLUS 1
                    (|CONV:BF2I|
                     (DIFBF (NORMBF (|DIVIDE:| N BFTWO* |:BPREC:|))
                            (NORMBF
                             (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS 4 0))
                                        |:BPREC:|))))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (SETQ RESULT BFONE*)
      (SETQ AJ2T (ASYMP*J*2TERM 2 MU))
      (SETQ ZSQ
              (NORMBF
               (|ROUND:MT|
                (|TIMES:| (CONS '|:RD:| (CONS 4 0))
                          (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|)))
                |:BPREC:|)))
      (SETQ ZSQP ZSQ)
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ PREV (|ABS:| THIS))
         (SETQ THIS
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:|
                    (CONS '|:RD:| (CONS (EXPT (MINUS 1) (QUOTIENT K 2)) 0))
                    (NORMBF (|DIVIDE:| AJ2T ZSQP |:BPREC:|)))
                   |:BPREC:|)))
         (COND
          ((AND (|GREATERP:| (|ABS:| THIS) PREV) (GREATERP K F))
           (SETQ RESULT (SETQ THIS BFZ*)))
          (T
           (PROGN
            (SETQ RESULT (PLUBF RESULT THIS))
            (SETQ ZSQP (NORMBF (|ROUND:MT| (|TIMES:| ZSQP ZSQ) |:BPREC:|)))
            (SETQ K (PLUS K 2))
            (SETQ AJ2T
                    (NORMBF
                     (|ROUND:MT| (|TIMES:| AJ2T (ASYMP*J*2TERM*MODIFIER K MU))
                                 |:BPREC:|)))))))
        (GO WHILELABEL))
      (COND ((EQUAL RESULT BFZ*) (RETURN '(LIST)))
            (T (RETURN (LIST 'LIST (MK*SQ (CONS (MKROUND RESULT) 1)))))))) 
(PUT 'ASYMP*Q 'NUMBER-OF-ARGS 4) 
(PUT 'ASYMP*Q 'DEFINED-ON-LINE '420) 
(PUT 'ASYMP*Q 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*Q 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ASYMP*Q (N Z MU ADMISSABLE)
    (PROG (RESULT THIS PREV ZSQ ZSQP AJ2T K F)
      (SETQ K 0)
      (SETQ F 0)
      (SETQ N
              (COND ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* N))))))
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ MU
              (COND ((FIXP MU) (CONS '|:RD:| (CONS MU 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* MU))))))
      (SETQ ADMISSABLE
              (COND ((FIXP ADMISSABLE) (CONS '|:RD:| (CONS ADMISSABLE 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* ADMISSABLE))))))
      (SETQ K 1)
      (SETQ F
              (PLUS 1
                    (|CONV:BF2I|
                     (DIFBF (NORMBF (|DIVIDE:| N BFTWO* |:BPREC:|))
                            (NORMBF
                             (|DIVIDE:| (CONS '|:RD:| (CONS 3 0))
                                        (CONS '|:RD:| (CONS 4 0))
                                        |:BPREC:|))))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (SETQ RESULT BFZ*)
      (SETQ AJ2T (ASYMP*J*2TERM 1 MU))
      (SETQ ZSQ
              (NORMBF
               (|ROUND:MT|
                (|TIMES:| (CONS '|:RD:| (CONS 4 0))
                          (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|)))
                |:BPREC:|)))
      (SETQ ZSQP (NORMBF (|ROUND:MT| (|TIMES:| BFTWO* Z) |:BPREC:|)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ PREV (|ABS:| THIS))
         (SETQ THIS
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:|
                    (CONS '|:RD:|
                          (CONS (EXPT (MINUS 1) (QUOTIENT (DIFFERENCE K 1) 2))
                                0))
                    (NORMBF (|DIVIDE:| AJ2T ZSQP |:BPREC:|)))
                   |:BPREC:|)))
         (COND
          ((AND (|GREATERP:| (|ABS:| THIS) PREV) (GREATERP K F))
           (SETQ RESULT (SETQ THIS BFZ*)))
          (T
           (PROGN
            (SETQ RESULT (PLUBF RESULT THIS))
            (SETQ ZSQP (NORMBF (|ROUND:MT| (|TIMES:| ZSQP ZSQ) |:BPREC:|)))
            (SETQ K (PLUS K 2))
            (SETQ AJ2T
                    (NORMBF
                     (|ROUND:MT| (|TIMES:| AJ2T (ASYMP*J*2TERM*MODIFIER K MU))
                                 |:BPREC:|)))))))
        (GO WHILELABEL))
      (COND ((EQUAL RESULT BFZ*) (RETURN '(LIST)))
            (T (RETURN (LIST 'LIST (MK*SQ (CONS (MKROUND RESULT) 1)))))))) 
(PUT 'ASYMP*J*2TERM 'NUMBER-OF-ARGS 2) 
(PUT 'ASYMP*J*2TERM 'DEFINED-ON-LINE '448) 
(PUT 'ASYMP*J*2TERM 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*J*2TERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMP*J*2TERM (K MU)
    (PROG (RESULT)
      (SETQ RESULT BFONE*)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((MINUSP (TIMES 2 (DIFFERENCE (DIFFERENCE (TIMES 2 K) 1) J)))
          (RETURN NIL)))
        (SETQ RESULT
                (NORMBF
                 (|ROUND:MT|
                  (|TIMES:| RESULT
                            (DIFBF MU (CONS '|:RD:| (CONS (EXPT J 2) 0))))
                  |:BPREC:|)))
        (SETQ J (PLUS2 J 2))
        (GO LAB))
      (SETQ RESULT
              (NORMBF
               (|DIVIDE:| RESULT
                          (CONS '|:RD:|
                                (CONS
                                 (TIMES (FACTORIAL K) (EXPT 2 (TIMES 2 K))) 0))
                          |:BPREC:|)))
      (RETURN RESULT))) 
(PUT 'ASYMP*J*2TERM*MODIFIER 'NUMBER-OF-ARGS 2) 
(PUT 'ASYMP*J*2TERM*MODIFIER 'DEFINED-ON-LINE '458) 
(PUT 'ASYMP*J*2TERM*MODIFIER 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'ASYMP*J*2TERM*MODIFIER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMP*J*2TERM*MODIFIER (K MU)
    (NORMBF
     (|ROUND:MT|
      (|TIMES:|
       (DIFBF MU (CONS '|:RD:| (CONS (EXPT (DIFFERENCE (TIMES 2 K) 3) 2) 0)))
       (NORMBF
        (|DIVIDE:|
         (DIFBF MU (CONS '|:RD:| (CONS (EXPT (DIFFERENCE (TIMES 2 K) 1) 2) 0)))
         (CONS '|:RD:| (CONS (TIMES (DIFFERENCE K 1) K 16) 0)) |:BPREC:|)))
      |:BPREC:|))) 
(PUT 'Y*CALC*S 'NUMBER-OF-ARGS 3) 
(FLAG '(Y*CALC*S) 'OPFN) 
(PUT 'Y*CALC*S 'DEFINED-ON-LINE '465) 
(PUT 'Y*CALC*S 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*CALC*S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE Y*CALC*S (N Z ST)
    (PROG (N0 Z0 ST0 PS FKGAMNK RESULT ALGLIST* PREPRE PRECOM)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PRECOM 0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((AND (EVALGREATERP (AEVAL Z) (TIMES 2 PREPRE))
             (EVALGREATERP (AEVAL Z) (AEVAL (LIST 'TIMES 2 N)))
             (EVALNEQ (SETQ RESULT (AEVAL (LIST 'ASYMP*Y*CALC N Z)))
                      (AEVAL (LIST 'LIST))))
        (PROGN
         (AEVAL (LIST 'PRECISION PREPRE))
         (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
         (RETURN (AEVAL RESULT)))))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 5))))
       (T (AEVAL (LIST 'PRECISION (PLUS PREPRE 8)))))
      (SETQ N0 (AEVAL N))
      (SETQ Z0 (AEVAL Z))
      (SETQ ST0 (AEVAL ST))
      (SETQ PS (AEVAL (LIST 'PLUS (LIST 'PSI 1) (LIST 'PSI (LIST 'PLUS 1 N)))))
      (SETQ FKGAMNK (AEVAL (LIST 'GAMMA (LIST 'PLUS N 1))))
      (SETQ RESULT (AEVAL (Y*CALC*S*SUB N0 Z0 PS FKGAMNK PREPRE ST0)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'Y*CALC*S*SUB 'NUMBER-OF-ARGS 6) 
(PUT 'Y*CALC*S*SUB 'DEFINED-ON-LINE '499) 
(PUT 'Y*CALC*S*SUB 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*CALC*S*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE Y*CALC*S*SUB (N Z PS FKGAMNK PREPRE ST0)
    (PROG (START RESULT THIS FC MODIFY ZFSQ ZFSQP NPS AZFSQ BJ Z0 N0 TPI
           ADMISSABLE K FK FNK DIFD FCP)
      (SETQ K 0)
      (SETQ FK 0)
      (SETQ FNK 0)
      (SETQ DIFD 0)
      (SETQ FCP 0)
      (SETQ Z0 Z)
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ PS
              (COND ((FIXP PS) (CONS '|:RD:| (CONS PS 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* PS))))))
      (SETQ N
              (COND ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* N))))))
      (SETQ N0 (|CONV:BF2I| N))
      (SETQ TPI (PI*))
      (COND
       ((EQUAL ST0 '(LIST))
        (PROGN
         (SETQ MODIFY
                 (NORMBF
                  (|DIVIDE:|
                   (|EXP:|
                    (NORMBF
                     (|ROUND:MT|
                      (|TIMES:| N
                                (|LOG:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                                        (PLUS |:BPREC:| 2)))
                      |:BPREC:|))
                    |:BPREC:|)
                   TPI |:BPREC:|)))
         (SETQ BJ
                 ((LAMBDA (U) (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                  (CDR
                   (*Q2F
                    (SIMP*
                     (SF*EVAL 'J*CALC*S*SUB
                              (LIST 'LIST N0 Z0 FKGAMNK PREPRE)))))))
         (COND
          ((LESSP N0 1)
           (PROGN
            (SETQ START
                    (NORMBF
                     (|ROUND:MT|
                      (|TIMES:|
                       (NORMBF
                        (|ROUND:MT|
                         (|TIMES:| (NORMBF (|DIVIDE:| BFTWO* TPI |:BPREC:|))
                                   (|LOG:|
                                    (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                                    (PLUS |:BPREC:| 1)))
                         |:BPREC:|))
                       BJ)
                      |:BPREC:|)))
            (SETQ FC (FACTORIAL N0))))
          ((LESSP N0 100)
           (PROGN
            (SETQ START BFZ*)
            (SETQ ZFSQ
                    (NORMBF
                     (|DIVIDE:| (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|))
                                (CONS '|:RD:| (CONS 4 0)) |:BPREC:|)))
            (PROG (K)
              (SETQ K 0)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE N0 1) K)) (RETURN NIL)))
              (SETQ START
                      (PLUBF START
                             (NORMBF
                              (|DIVIDE:|
                               (EXPTBF ZFSQ K
                                       (CONS '|:RD:|
                                             (CONS
                                              (FACTORIAL
                                               (DIFFERENCE (DIFFERENCE N0 K)
                                                           1))
                                              0)))
                               (CONS '|:RD:| (CONS (FACTORIAL K) 0))
                               |:BPREC:|))))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETQ START
                    (|MINUS:|
                     (NORMBF
                      (|ROUND:MT|
                       (|TIMES:| START
                                 (NORMBF
                                  (|DIVIDE:|
                                   (|EXP:|
                                    (NORMBF
                                     (|ROUND:MT|
                                      (|TIMES:| (|MINUS:| N)
                                                (|LOG:|
                                                 (NORMBF
                                                  (|DIVIDE:| Z BFTWO*
                                                             |:BPREC:|))
                                                 (PLUS |:BPREC:| 2)))
                                      |:BPREC:|))
                                    |:BPREC:|)
                                   TPI |:BPREC:|)))
                       |:BPREC:|))))
            (SETQ START
                    (PLUBF START
                           (NORMBF
                            (|ROUND:MT|
                             (|TIMES:|
                              (NORMBF
                               (|ROUND:MT|
                                (|TIMES:|
                                 (NORMBF (|DIVIDE:| BFTWO* TPI |:BPREC:|)) BJ)
                                |:BPREC:|))
                              (|LOG:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                                      (PLUS |:BPREC:| 2)))
                             |:BPREC:|))))
            (SETQ FC (FACTORIAL N0))))
          (T
           (PROGN
            (SETQ ZFSQ
                    (NORMBF
                     (|DIVIDE:| (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|))
                                (CONS '|:RD:| (CONS 4 0)) |:BPREC:|)))
            (SETQ ZFSQP BFONE*)
            (SETQ FK 1)
            (SETQ FNK (FACTORIAL (DIFFERENCE N0 1)))
            (SETQ FC (TIMES FNK N0))
            (SETQ START BFZ*)
            (PROG (K)
              (SETQ K 0)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE N0 2) K)) (RETURN NIL)))
              (PROGN
               (SETQ START
                       (PLUBF START
                              (NORMBF
                               (|ROUND:MT|
                                (|TIMES:| (CONS '|:RD:| (CONS FNK 0))
                                          (NORMBF
                                           (|DIVIDE:| ZFSQP
                                                      (CONS '|:RD:|
                                                            (CONS FK 0))
                                                      |:BPREC:|)))
                                |:BPREC:|))))
               (SETQ FK (TIMES FK (PLUS K 1)))
               (SETQ FNK (QUOTIENT FNK (DIFFERENCE (DIFFERENCE N0 K) 1)))
               (SETQ ZFSQP
                       (NORMBF (|ROUND:MT| (|TIMES:| ZFSQP ZFSQ) |:BPREC:|))))
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETQ START
                    (PLUBF START
                           (NORMBF
                            (|ROUND:MT|
                             (|TIMES:| (CONS '|:RD:| (CONS FNK 0))
                                       (NORMBF
                                        (|DIVIDE:| ZFSQP
                                                   (CONS '|:RD:| (CONS FK 0))
                                                   |:BPREC:|)))
                             |:BPREC:|))))
            (SETQ START
                    (|MINUS:|
                     (PLUBF
                      (NORMBF
                       (|ROUND:MT|
                        (|TIMES:| START
                                  (NORMBF
                                   (|DIVIDE:| BFONE*
                                              (NORMBF
                                               (|ROUND:MT|
                                                (|TIMES:| MODIFY
                                                          (NORMBF
                                                           (|ROUND:MT|
                                                            (|TIMES:| TPI TPI)
                                                            |:BPREC:|)))
                                                |:BPREC:|))
                                              |:BPREC:|)))
                        |:BPREC:|))
                      (NORMBF
                       (|ROUND:MT|
                        (|TIMES:|
                         (NORMBF
                          (|ROUND:MT|
                           (|TIMES:| (NORMBF (|DIVIDE:| BFTWO* TPI |:BPREC:|))
                                     BJ)
                           |:BPREC:|))
                         (|LOG:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                                 (PLUS |:BPREC:| 2)))
                        |:BPREC:|))))))))))
       (T
        (PROGN
         (SETQ START
                 (COND ((FIXP (CADR ST0)) (CONS '|:RD:| (CONS (CADR ST0) 0)))
                       (T
                        ((LAMBDA (Y)
                           (COND
                            ((NEQ (CAR Y) '|:RD:|)
                             ((LAMBDA (U)
                                (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                              (CDR (*RN2RD Y))))
                            (T
                             (COND ((ATOM (CDR Y)) (CDR Y))
                                   (T (CONS '|:RD:| (CDR Y)))))))
                         (*Q2F (SIMP* (CADR ST0)))))))
         (SETQ MODIFY
                 ((LAMBDA (X)
                    (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                          (T
                           ((LAMBDA (Y)
                              (COND
                               ((NEQ (CAR Y) '|:RD:|)
                                ((LAMBDA (U)
                                   (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                                 (CDR (*RN2RD Y))))
                               (T
                                (COND ((ATOM (CDR Y)) (CDR Y))
                                      (T (CONS '|:RD:| (CDR Y)))))))
                            (*Q2F (SIMP* X))))))
                  (CADDR ST0)))
         (SETQ FC (CADDDR ST0)))))
      (SETQ ZFSQ
              (|MINUS:|
               (NORMBF
                (|DIVIDE:| (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|))
                           (CONS '|:RD:| (CONS 4 0)) |:BPREC:|))))
      (SETQ AZFSQ (|ABS:| ZFSQ))
      (SETQ RESULT
              (NORMBF (|DIVIDE:| PS (CONS '|:RD:| (CONS FC 0)) |:BPREC:|)))
      (SETQ K 1)
      (SETQ ZFSQP ZFSQ)
      (SETQ FC (TIMES FC (PLUS N0 1)))
      (SETQ PS
              (PLUBF PS
                     (PLUBF BFONE*
                            (NORMBF
                             (|DIVIDE:| BFONE* (PLUBF N BFONE*) |:BPREC:|)))))
      (COND
       ((|LESSP:| (|ABS:| (PLUBF RESULT START)) BFONE*)
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:|
                   (NORMBF
                    (|DIVIDE:| BFONE*
                               (|EXP:|
                                (NORMBF
                                 (|ROUND:MT|
                                  (|TIMES:| (FL2BF !LOGTEN)
                                            (PLUBF
                                             (CONS '|:RD:|
                                                   (CONS (PLUS PREPRE 2) 0))
                                             (NORMBF
                                              (|DIVIDE:|
                                               (|LOG:|
                                                (NORMBF
                                                 (|DIVIDE:| BFONE*
                                                            (PLUBF
                                                             (|ABS:| RESULT)
                                                             (|ABS:| START))
                                                            |:BPREC:|))
                                                5)
                                               (FL2BF !LOGTEN) |:BPREC:|))))
                                  |:BPREC:|))
                                5)
                               |:BPREC:|))
                   MODIFY |:BPREC:|)))))
       (T
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:|
                   (NORMBF
                    (|DIVIDE:| BFONE*
                               (|EXP:|
                                (NORMBF
                                 (|ROUND:MT|
                                  (|TIMES:| (FL2BF !LOGTEN)
                                            (PLUBF
                                             (CONS '|:RD:|
                                                   (CONS (PLUS PREPRE 2) 0))
                                             (NORMBF
                                              (|DIVIDE:|
                                               (|LOG:|
                                                (PLUBF (|ABS:| RESULT)
                                                       (|ABS:| START))
                                                5)
                                               (FL2BF !LOGTEN) |:BPREC:|))))
                                  |:BPREC:|))
                                5)
                               |:BPREC:|))
                   MODIFY |:BPREC:|))))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ THIS
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:| PS
                             (NORMBF
                              (|DIVIDE:| ZFSQP (CONS '|:RD:| (CONS FC 0))
                                         |:BPREC:|)))
                   |:BPREC:|)))
         (SETQ RESULT (PLUBF RESULT THIS))
         (SETQ K (PLUS K 1))
         (SETQ ZFSQP (NORMBF (|ROUND:MT| (|TIMES:| ZFSQP ZFSQ) |:BPREC:|)))
         (SETQ NPS
                 (PLUBF PS
                        (PLUBF
                         (NORMBF
                          (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS K 0))
                                     |:BPREC:|))
                         (NORMBF
                          (|DIVIDE:| BFONE* (CONS '|:RD:| (CONS (PLUS K N0) 0))
                                     |:BPREC:|)))))
         (SETQ FCP (TIMES K (PLUS N0 K)))
         (COND
          ((|GREATERP:| (NORMBF (|ROUND:MT| (|TIMES:| NPS AZFSQ) |:BPREC:|))
                        (NORMBF
                         (|ROUND:MT| (|TIMES:| PS (CONS '|:RD:| (CONS FCP 0)))
                                     |:BPREC:|)))
           (PROGN
            (SETQ DIFD
                    (PLUS 1
                          (|CONV:BF2I|
                           (NORMBF
                            (|DIVIDE:|
                             (NORMBF
                              (|ROUND:MT| (|TIMES:| NPS AZFSQ) |:BPREC:|))
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:| PS (CONS '|:RD:| (CONS FCP 0)))
                               |:BPREC:|))
                             |:BPREC:|)))))
            (PRECISION (PLUS (PRECISION 0) (LENGTH (EXPLODE DIFD)))))))
         (SETQ FC (TIMES FC FCP))
         (SETQ PS NPS))
        (GO WHILELABEL))
      (SETQ RESULT
              (DIFBF START
                     (NORMBF (|ROUND:MT| (|TIMES:| RESULT MODIFY) |:BPREC:|))))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(PUT 'I*CALC*S 'NUMBER-OF-ARGS 2) 
(FLAG '(I*CALC*S) 'OPFN) 
(PUT 'I*CALC*S 'DEFINED-ON-LINE '599) 
(PUT 'I*CALC*S 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'I*CALC*S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE I*CALC*S (N Z)
    (PROG (N0 Z0 PS FKGAMNK RESULT ALGLIST* PREPRE PRECOM)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PRECOM 0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((EVALLESSP PREPRE (AEVAL !NFPD))
        (AEVAL
         (LIST 'PRECISION
               (LIST 'PLUS !NFPD 3
                     (LIST 'FLOOR (LIST 'QUOTIENT (LIST 'ABS N) 10))))))
       (T
        (AEVAL
         (LIST 'PRECISION
               (LIST 'PLUS PREPRE 8
                     (LIST 'FLOOR (LIST 'QUOTIENT (LIST 'ABS N) 10)))))))
      (SETQ N0 (AEVAL N))
      (SETQ Z0 (AEVAL Z))
      (SETQ FKGAMNK (AEVAL (LIST 'GAMMA (LIST 'PLUS N 1))))
      (SETQ RESULT (AEVAL (I*CALC*S*SUB N0 Z0 FKGAMNK PREPRE)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'I*CALC*S*SUB 'NUMBER-OF-ARGS 4) 
(PUT 'I*CALC*S*SUB 'DEFINED-ON-LINE '617) 
(PUT 'I*CALC*S*SUB 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'I*CALC*S*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE I*CALC*S*SUB (N Z FKGAMNK PREPRE)
    (PROG (RESULT ADMISSABLE THIS MODIFY ZFSQ ZFSQP KNK AZFSQ K)
      (SETQ N
              (COND ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* N))))))
      (SETQ Z
              (COND ((FIXP Z) (CONS '|:RD:| (CONS Z 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* Z))))))
      (SETQ FKGAMNK
              (COND ((FIXP FKGAMNK) (CONS '|:RD:| (CONS FKGAMNK 0)))
                    (T
                     ((LAMBDA (Y)
                        (COND
                         ((NEQ (CAR Y) '|:RD:|)
                          ((LAMBDA (U)
                             (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                           (CDR (*RN2RD Y))))
                         (T
                          (COND ((ATOM (CDR Y)) (CDR Y))
                                (T (CONS '|:RD:| (CDR Y)))))))
                      (*Q2F (SIMP* FKGAMNK))))))
      (SETQ MODIFY
              (|EXP:|
               (NORMBF
                (|ROUND:MT|
                 (|TIMES:|
                  (|LOG:| (NORMBF (|DIVIDE:| Z BFTWO* |:BPREC:|))
                          (PLUS |:BPREC:| 2))
                  N)
                 |:BPREC:|))
               |:BPREC:|))
      (SETQ ZFSQ
              (NORMBF
               (|DIVIDE:| (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|))
                          (CONS '|:RD:| (CONS 4 0)) |:BPREC:|)))
      (SETQ AZFSQ (|ABS:| ZFSQ))
      (SETQ RESULT (NORMBF (|DIVIDE:| BFONE* FKGAMNK |:BPREC:|)))
      (SETQ K BFONE*)
      (SETQ ZFSQP ZFSQ)
      (SETQ FKGAMNK
              (NORMBF
               (|ROUND:MT| (|TIMES:| FKGAMNK (PLUBF N BFONE*)) |:BPREC:|)))
      (COND
       ((|LESSP:| (|ABS:| RESULT) BFONE*)
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:| BFONE*
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:|
                                (|EXP:|
                                 (NORMBF
                                  (|ROUND:MT|
                                   (|TIMES:| (FL2BF !LOGTEN)
                                             (CONS '|:RD:|
                                                   (CONS
                                                    (PLUS PREPRE
                                                          (LENGTH
                                                           (EXPLODE FKGAMNK)))
                                                    0)))
                                   |:BPREC:|))
                                 8)
                                MODIFY)
                               |:BPREC:|))
                             |:BPREC:|)))))
       (T
        (SETQ ADMISSABLE
                (|ABS:|
                 (NORMBF
                  (|DIVIDE:| BFONE*
                             (NORMBF
                              (|ROUND:MT|
                               (|TIMES:|
                                (|EXP:|
                                 (NORMBF
                                  (|ROUND:MT|
                                   (|TIMES:| (FL2BF !LOGTEN)
                                             (CONS '|:RD:|
                                                   (CONS
                                                    (PLUS PREPRE
                                                          (LENGTH
                                                           (EXPLODE
                                                            (PLUS 1
                                                                  (|CONV:BF2I|
                                                                   (|ABS:|
                                                                    RESULT))))))
                                                    0)))
                                   |:BPREC:|))
                                 8)
                                MODIFY)
                               |:BPREC:|))
                             |:BPREC:|))))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ THIS (NORMBF (|DIVIDE:| ZFSQP FKGAMNK |:BPREC:|)))
         (SETQ RESULT (PLUBF RESULT THIS))
         (SETQ K (PLUBF K BFONE*))
         (SETQ KNK (NORMBF (|ROUND:MT| (|TIMES:| K (PLUBF N K)) |:BPREC:|)))
         (COND
          ((|GREATERP:| AZFSQ KNK)
           (PRECISION
            (PLUS (PRECISION 0)
                  (LENGTH
                   (EXPLODE
                    (PLUS 1
                          (|CONV:BF2I|
                           (NORMBF (|DIVIDE:| AZFSQ KNK |:BPREC:|))))))))))
         (SETQ ZFSQP (NORMBF (|ROUND:MT| (|TIMES:| ZFSQP ZFSQ) |:BPREC:|)))
         (SETQ FKGAMNK (NORMBF (|ROUND:MT| (|TIMES:| FKGAMNK KNK) |:BPREC:|))))
        (GO WHILELABEL))
      (SETQ RESULT (NORMBF (|ROUND:MT| (|TIMES:| RESULT MODIFY) |:BPREC:|)))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(PUT 'J*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(J*CALC) 'OPFN) 
(PUT 'J*CALC 'DEFINED-ON-LINE '673) 
(PUT 'J*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'J*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE J*CALC (N Z)
    (PROG (RESULT ADMISSABLE THIS ALGLIST* MODIFY FKGAMNK ZFSQ ZFSQP AZFSQ KNK
           PREPRE K DIFD)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ K 0)
      (SETQ DIFD 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (PLUS PREPRE 4)))
      (SETQ MODIFY (AEVAL (LIST 'EXPT (LIST 'QUOTIENT Z 2) N)))
      (SETQ ZFSQ (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT Z 2) 4))))
      (SETQ AZFSQ (AEVAL (LIST 'ABS ZFSQ)))
      (SETQ FKGAMNK (AEVAL (LIST 'GAMMA (LIST 'PLUS N 1))))
      (SETQ RESULT (AEVAL (LIST 'QUOTIENT 1 FKGAMNK)))
      (SETQ K (AEVAL 1))
      (SETQ ZFSQP (AEVAL ZFSQ))
      (SETQ FKGAMNK (AEVAL (LIST 'TIMES FKGAMNK (LIST 'PLUS N 1))))
      (COND
       ((AND (EVALNUMBERP (AEVAL MODIFY))
             (EVALEQUAL (AEVAL (LIST 'IMPART MODIFY)) 0))
        (COND
         ((EVALLESSP (AEVAL (LIST 'ABS RESULT)) 1)
          (PROGN
           (SETQ DIFD
                   (AEVAL
                    (LIST 'CEILING (LIST 'QUOTIENT 1 (LIST 'ABS RESULT)))))
           (SETQ ADMISSABLE
                   (AEVAL
                    (LIST 'ABS
                          (LIST 'QUOTIENT
                                (LIST 'QUOTIENT 1
                                      (LIST 'EXPT 10
                                            (LIST 'PLUS PREPRE
                                                  (LENGTH (EXPLODE DIFD)))))
                                MODIFY))))))
         (T
          (PROGN
           (SETQ DIFD (AEVAL (LIST 'CEILING (LIST 'ABS RESULT))))
           (SETQ ADMISSABLE
                   (AEVAL
                    (LIST 'ABS
                          (LIST 'QUOTIENT
                                (LIST 'QUOTIENT 1
                                      (LIST 'EXPT 10
                                            (LIST 'DIFFERENCE PREPRE
                                                  (LENGTH (EXPLODE DIFD)))))
                                MODIFY))))))))
       ((EVALLESSP (AEVAL (LIST 'ABS RESULT)) 1)
        (PROGN
         (SETQ DIFD
                 (AEVAL (LIST 'CEILING (LIST 'QUOTIENT 1 (LIST 'ABS RESULT)))))
         (SETQ ADMISSABLE
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'QUOTIENT 1
                              (LIST 'EXPT 10
                                    (LIST 'PLUS PREPRE 10
                                          (LENGTH (EXPLODE DIFD))))))))))
       (T
        (PROGN
         (SETQ DIFD (AEVAL (LIST 'CEILING (LIST 'ABS RESULT))))
         (SETQ ADMISSABLE
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'QUOTIENT 1
                              (LIST 'EXPT 10
                                    (LIST 'PLUS PREPRE
                                          (LIST 'DIFFERENCE 10
                                                (LENGTH
                                                 (EXPLODE DIFD))))))))))))
      (SETQ THIS (AEVAL (LIST 'PLUS ADMISSABLE 1)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS THIS)) (AEVAL* ADMISSABLE))
             (PROGN
              (SETQ THIS (AEVAL* (LIST 'QUOTIENT ZFSQP FKGAMNK)))
              (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT THIS)))
              (SETQ K (AEVAL* (PLUS K 1)))
              (SETQ KNK (AEVAL* (LIST 'TIMES K (LIST 'PLUS N K))))
              (COND
               ((EVALGREATERP (AEVAL* AZFSQ) (AEVAL* KNK))
                (PROGN
                 (SETQ DIFD
                         (AEVAL* (LIST 'CEILING (LIST 'QUOTIENT AZFSQ KNK))))
                 (AEVAL*
                  (LIST 'PRECISION
                        (LIST 'PLUS (LIST 'PRECISION 0)
                              (LENGTH (EXPLODE DIFD))))))))
              (SETQ ZFSQP (AEVAL* (LIST 'TIMES ZFSQP ZFSQ)))
              (SETQ FKGAMNK (AEVAL* (LIST 'TIMES FKGAMNK KNK)))))
      (SETQ RESULT (AEVAL (LIST 'TIMES RESULT MODIFY)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'Y*MODIFIER*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(Y*MODIFIER*CALC) 'OPFN) 
(PUT 'Y*MODIFIER*CALC 'DEFINED-ON-LINE '736) 
(PUT 'Y*MODIFIER*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*MODIFIER*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE Y*MODIFIER*CALC (N Z)
    (PROG (MODIFY START ZFSQ ZFSQP FC FK FNK PREPRE)
      (SETQ FK 0)
      (SETQ FNK 0)
      (SETQ PREPRE 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (SETQ MODIFY
              (AEVAL (LIST 'QUOTIENT (LIST 'EXPT (LIST 'QUOTIENT Z 2) N) 'PI)))
      (COND
       ((EVALLESSP (AEVAL N) 1)
        (PROGN
         (SETQ START
                 (AEVAL
                  (LIST 'TIMES (LIST 'QUOTIENT 2 'PI)
                        (LIST 'LOG (LIST 'QUOTIENT Z 2)) (LIST 'BESSELJ N Z))))
         (SETQ FC (AEVAL (LIST 'FACTORIAL N)))))
       ((EVALLESSP (AEVAL N) 100)
        (PROGN
         (SETQ START
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'MINUS
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT
                                          (LIST 'EXPT (LIST 'QUOTIENT Z 2)
                                                (LIST 'MINUS N))
                                          'PI)
                                    (PROG (K FORALL-RESULT)
                                      (SETQ K 0)
                                      (SETQ FORALL-RESULT 0)
                                     LAB1
                                      (COND
                                       ((|AMINUSP:|
                                         (LIST 'DIFFERENCE
                                               (AEVAL* (LIST 'DIFFERENCE N 1))
                                               K))
                                        (RETURN FORALL-RESULT)))
                                      (SETQ FORALL-RESULT
                                              (AEVAL*
                                               (LIST 'PLUS
                                                     (AEVAL*
                                                      (LIST 'QUOTIENT
                                                            (LIST 'TIMES
                                                                  (LIST
                                                                   'FACTORIAL
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST
                                                                     'DIFFERENCE
                                                                     N K)
                                                                    1))
                                                                  (LIST 'EXPT
                                                                        (LIST
                                                                         'QUOTIENT
                                                                         (LIST
                                                                          'EXPT
                                                                          Z 2)
                                                                         4)
                                                                        K))
                                                            (LIST 'FACTORIAL
                                                                  K)))
                                                     FORALL-RESULT)))
                                      (SETQ K
                                              ((LAMBDA (FORALL-RESULT)
                                                 (AEVAL*
                                                  (LIST 'PLUS FORALL-RESULT
                                                        1)))
                                               K))
                                      (GO LAB1))))
                        (LIST 'TIMES (LIST 'QUOTIENT 2 'PI)
                              (LIST 'LOG (LIST 'QUOTIENT Z 2))
                              (LIST 'BESSELJ N Z)))))
         (SETQ FC (AEVAL (LIST 'FACTORIAL N)))))
       (T
        (PROGN
         (SETQ ZFSQ (AEVAL (LIST 'QUOTIENT (LIST 'EXPT Z 2) 4)))
         (SETQ ZFSQP (AEVAL 1))
         (SETQ FK (AEVAL 1))
         (SETQ FNK (AEVAL (LIST 'FACTORIAL (LIST 'DIFFERENCE N 1))))
         (SETQ FC (AEVAL (LIST 'TIMES FNK N)))
         (SETQ START (AEVAL 0))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 2)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ START
                    (AEVAL*
                     (LIST 'PLUS START
                           (LIST 'TIMES FNK (LIST 'QUOTIENT ZFSQP FK)))))
            (SETQ FK (AEVAL* (TIMES FK (PLUS K 1))))
            (SETQ FNK
                    (AEVAL*
                     (LIST 'FLOOR
                           (LIST 'QUOTIENT FNK
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE N K)
                                       1)))))
            (SETQ ZFSQP (AEVAL* (LIST 'TIMES ZFSQP ZFSQ))))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (SETQ START
                 (AEVAL
                  (LIST 'PLUS START
                        (LIST 'TIMES FNK (LIST 'QUOTIENT ZFSQP FK)))))
         (SETQ START
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'MINUS
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT 1
                                          (LIST 'TIMES MODIFY
                                                (LIST 'EXPT 'PI 2)))
                                    START))
                        (LIST 'TIMES (LIST 'QUOTIENT 2 'PI)
                              (LIST 'LOG (LIST 'QUOTIENT Z 2))
                              (LIST 'BESSELJ N Z))))))))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL (LIST 'LIST START MODIFY FC))))) 
(PUT 'Y*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(Y*CALC) 'OPFN) 
(PUT 'Y*CALC 'DEFINED-ON-LINE '793) 
(PUT 'Y*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE Y*CALC (N Z)
    (PROG (START RESULT THIS PS FC SMF MODIFY ZFSQ ZFSQP ALGLIST* NPS AZFSQ
           PREPRE K FK FNK DIFD FCP)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ K 0)
      (SETQ FK 0)
      (SETQ FNK 0)
      (SETQ DIFD 0)
      (SETQ FCP 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (PLUS PREPRE 8)))
      (SETQ SMF (AEVAL (LIST 'Y*MODIFIER*CALC N Z)))
      (SETQ START (AEVAL (LIST 'FIRST SMF)))
      (SETQ MODIFY (AEVAL (LIST 'SECOND SMF)))
      (SETQ FC (AEVAL (LIST 'THIRD SMF)))
      (SETQ PS (AEVAL (LIST 'PLUS (LIST 'PSI 1) (LIST 'PSI (LIST 'PLUS 1 N)))))
      (SETQ ZFSQ (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT Z 2) 4))))
      (SETQ AZFSQ (AEVAL (LIST 'ABS ZFSQ)))
      (SETQ RESULT (AEVAL (LIST 'QUOTIENT PS FC)))
      (SETQ K (AEVAL 1))
      (SETQ ZFSQP (AEVAL ZFSQ))
      (SETQ FC (AEVAL (LIST 'TIMES FC (LIST 'PLUS N 1))))
      (SETQ PS (AEVAL (LIST 'PLUS PS 1 (LIST 'QUOTIENT 1 (LIST 'PLUS N 1)))))
      (COND
       ((EVALNUMBERP (AEVAL START))
        (COND
         ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'PLUS RESULT START))) 1)
          (SETK 'ADMISSABLE
                (AEVAL
                 (LIST 'ABS
                       (LIST 'QUOTIENT
                             (LIST 'QUOTIENT 1
                                   (LIST 'EXPT 10
                                         (LIST 'PLUS PREPRE 2
                                               (LIST 'LOG10
                                                     (LIST 'QUOTIENT 1
                                                           (LIST 'PLUS
                                                                 (LIST 'ABS
                                                                       RESULT)
                                                                 (LIST 'ABS
                                                                       START)))))))
                             MODIFY)))))
         (T
          (SETK 'ADMISSABLE
                (AEVAL
                 (LIST 'ABS
                       (LIST 'TIMES
                             (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 2)))
                             (LIST 'QUOTIENT
                                   (LIST 'LOG10
                                         (LIST 'PLUS (LIST 'ABS RESULT)
                                               (LIST 'ABS START)))
                                   MODIFY))))))))
       (T
        (SETK 'ADMISSABLE
              (AEVAL
               (LIST 'ABS
                     (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 10))))))))
      (SETQ THIS (AEVAL (LIST 'PLUS 'ADMISSABLE 1)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS THIS)) (AEVAL* 'ADMISSABLE))
             (PROGN
              (SETQ THIS (AEVAL* (LIST 'TIMES PS (LIST 'QUOTIENT ZFSQP FC))))
              (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT THIS)))
              (SETQ K (AEVAL* (PLUS K 1)))
              (SETQ ZFSQP (AEVAL* (LIST 'TIMES ZFSQP ZFSQ)))
              (SETQ NPS
                      (AEVAL*
                       (LIST 'PLUS PS (LIST 'QUOTIENT 1 K)
                             (LIST 'QUOTIENT 1 (LIST 'PLUS K N)))))
              (SETQ FCP (AEVAL* (LIST 'TIMES K (LIST 'PLUS N K))))
              (COND
               ((EVALGREATERP (AEVAL* (LIST 'TIMES NPS AZFSQ))
                              (AEVAL* (LIST 'TIMES PS FCP)))
                (PROGN
                 (SETQ DIFD
                         (AEVAL*
                          (LIST 'CEILING
                                (LIST 'QUOTIENT (LIST 'TIMES NPS AZFSQ)
                                      (LIST 'TIMES PS FCP)))))
                 (AEVAL*
                  (LIST 'PRECISION
                        (LIST 'PLUS (LIST 'PRECISION 0)
                              (LENGTH (EXPLODE DIFD))))))))
              (SETQ FC (AEVAL* (LIST 'TIMES FC FCP)))
              (SETQ PS (AEVAL* NPS))))
      (SETQ RESULT
              (AEVAL (LIST 'DIFFERENCE START (LIST 'TIMES RESULT MODIFY))))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'I*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(I*CALC) 'OPFN) 
(PUT 'I*CALC 'DEFINED-ON-LINE '862) 
(PUT 'I*CALC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'I*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE I*CALC (N Z)
    (PROG (RESULT ADMISSABLE THIS PREV NPREV ALGLIST* MODIFY FKGAMNK ZFSQ ZFSQP
           KNK PREPRE K DIFD)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ K 0)
      (SETQ DIFD 0)
      (SETQ MODIFY (AEVAL (LIST 'EXPT (LIST 'QUOTIENT Z 2) N)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (PLUS PREPRE 4)))
      (SETQ ZFSQ (AEVAL (LIST 'QUOTIENT (LIST 'EXPT Z 2) 4)))
      (SETK 'AZFSQ (AEVAL (LIST 'ABS ZFSQ)))
      (SETQ FKGAMNK (AEVAL (LIST 'GAMMA (LIST 'PLUS N 1))))
      (SETQ RESULT (AEVAL (LIST 'QUOTIENT 1 FKGAMNK)))
      (SETQ K (AEVAL 1))
      (SETQ ZFSQP (AEVAL ZFSQ))
      (SETQ FKGAMNK (AEVAL (LIST 'TIMES FKGAMNK (LIST 'PLUS N 1))))
      (COND
       ((EVALNUMBERP (AEVAL MODIFY))
        (COND
         ((EVALLESSP (AEVAL (LIST 'ABS RESULT)) 1)
          (PROGN
           (SETQ DIFD
                   (AEVAL
                    (LIST 'CEILING (LIST 'QUOTIENT 1 (LIST 'ABS RESULT)))))
           (SETQ ADMISSABLE
                   (AEVAL
                    (LIST 'ABS
                          (LIST 'QUOTIENT
                                (LIST 'QUOTIENT 1
                                      (LIST 'EXPT 10
                                            (LIST 'PLUS PREPRE
                                                  (LENGTH (EXPLODE DIFD)))))
                                MODIFY))))))
         (T
          (PROGN
           (SETQ DIFD (AEVAL (LIST 'CEILING (LIST 'ABS RESULT))))
           (SETQ ADMISSABLE
                   (AEVAL
                    (LIST 'ABS
                          (LIST 'QUOTIENT
                                (LIST 'QUOTIENT 1
                                      (LIST 'EXPT 10
                                            (LIST 'DIFFERENCE PREPRE
                                                  (LENGTH (EXPLODE DIFD)))))
                                MODIFY))))))))
       ((EVALLESSP (AEVAL (LIST 'ABS RESULT)) 1)
        (PROGN
         (SETQ DIFD
                 (AEVAL (LIST 'CEILING (LIST 'QUOTIENT 1 (LIST 'ABS RESULT)))))
         (SETQ ADMISSABLE
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'QUOTIENT 1
                              (LIST 'EXPT 10
                                    (LIST 'PLUS PREPRE 10
                                          (LENGTH (EXPLODE DIFD))))))))))
       (T
        (PROGN
         (SETQ DIFD (AEVAL (LIST 'CEILING (LIST 'ABS RESULT))))
         (SETQ ADMISSABLE
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'QUOTIENT 1
                              (LIST 'EXPT 10
                                    (LIST 'PLUS PREPRE
                                          (LIST 'DIFFERENCE 10
                                                (LENGTH
                                                 (EXPLODE DIFD))))))))))))
      (SETQ THIS (AEVAL (LIST 'PLUS ADMISSABLE 1)))
      (SETQ NPREV (AEVAL (LIST 'ABS THIS)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS THIS)) (AEVAL* ADMISSABLE))
             (PROGN
              (SETQ THIS (AEVAL* (LIST 'QUOTIENT ZFSQP FKGAMNK)))
              (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT THIS)))
              (SETQ K (AEVAL* (PLUS K 1)))
              (SETQ KNK (AEVAL* (LIST 'TIMES K (LIST 'PLUS N K))))
              (COND
               ((EVALGREATERP (AEVAL* 'AZFSQ) (AEVAL* KNK))
                (PROGN
                 (SETQ DIFD
                         (AEVAL* (LIST 'CEILING (LIST 'QUOTIENT 'AZFSQ KNK))))
                 (AEVAL*
                  (LIST 'PRECISION
                        (LIST 'PLUS (LIST 'PRECISION 0)
                              (LENGTH (EXPLODE DIFD))))))))
              (SETQ ZFSQP (AEVAL* (LIST 'TIMES ZFSQP ZFSQ)))
              (SETQ FKGAMNK (AEVAL* (LIST 'TIMES FKGAMNK KNK)))))
      (SETQ RESULT (AEVAL (LIST 'TIMES RESULT MODIFY)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'K*CALC*2 'NUMBER-OF-ARGS 2) 
(FLAG '(K*CALC*2) 'OPFN) 
(PUT 'K*CALC*2 'DEFINED-ON-LINE '910) 
(PUT 'K*CALC*2 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'K*CALC*2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE K*CALC*2 (N Z)
    (PROG (RESULT PRECOM PREPRE)
      (SETQ PREPRE 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (PLUS PREPRE 8)))
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*ON*SWITCH)))
      (SETQ RESULT
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 'PI 2) 'I
                     (LIST 'EXP (LIST 'TIMES (LIST 'QUOTIENT 'PI 2) N 'I))
                     (LIST 'HANKEL1 N
                           (LIST 'TIMES Z
                                 (LIST 'EXP
                                       (LIST 'TIMES (LIST 'QUOTIENT 'PI 2)
                                             'I)))))))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (AEVAL (LIST 'PRECISION PREPRE))
      (RETURN (AEVAL RESULT)))) 
(PUT 'Y*REEXPRESS 'NUMBER-OF-ARGS 2) 
(FLAG '(Y*REEXPRESS) 'OPFN) 
(PUT 'Y*REEXPRESS 'DEFINED-ON-LINE '932) 
(PUT 'Y*REEXPRESS 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*REEXPRESS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE Y*REEXPRESS (N Z)
    (PROG (RESULT PREMSG)
      (SETQ PREMSG (AEVAL *MSG))
      (AEVAL (OFF (LIST 'MSG)))
      (AEVAL (OFF (LIST 'ROUNDED)))
      (SETQ RESULT
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES (LIST 'BESSELJ N Z)
                                 (LIST 'COS (LIST 'TIMES N 'PI)))
                           (LIST 'BESSELJ (LIST 'MINUS N) Z))
                     (LIST 'SIN (LIST 'TIMES N 'PI)))))
      (AEVAL (ON (LIST 'ROUNDED)))
      (COND ((BOOLVALUE* PREMSG) (AEVAL (ON (LIST 'MSG)))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'Y*CALC*SC 'NUMBER-OF-ARGS 2) 
(FLAG '(Y*CALC*SC) 'OPFN) 
(PUT 'Y*CALC*SC 'DEFINED-ON-LINE '959) 
(PUT 'Y*CALC*SC 'DEFINED-IN-FILE 'SPECFN/SFBES.RED) 
(PUT 'Y*CALC*SC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE Y*CALC*SC (N Z)
    (PROG (ST IC RC MD FC RESULT PRECOM PREPRE)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (SETQ Z (AEVAL (LIST 'MINUS Z)))
      (COND
       ((EVALLESSP (AEVAL PREPRE) (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 2))))
       (T (AEVAL (LIST 'PRECISION (LIST 'PLUS PREPRE 4)))))
      (SETQ ST (AEVAL (LIST 'Y*MODIFIER*CALC N Z)))
      (SETQ RC (AEVAL (LIST 'MINUS (LIST 'FIRST ST))))
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*ON*SWITCH)))
      (SETQ IC
              (AEVAL
               (LIST 'IMPART
                     (LIST 'LOG (LIST 'MINUS (LIST 'QUOTIENT 'PI 2))))))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (SETQ IC
              (AEVAL
               (LIST 'TIMES IC (LIST 'QUOTIENT 2 'PI)
                     (LIST 'BESSELJ N (LIST 'MINUS Z)))))
      (SETQ MD (AEVAL (LIST 'MINUS (LIST 'SECOND ST))))
      (SETQ FC (AEVAL (LIST 'THIRD ST)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ RESULT (AEVAL (LIST 'Y*CALC*S N Z (LIST 'LIST RC MD FC))))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (COND
       ((BOOLVALUE* (REVALX *COMPLEX))
        (SETQ RESULT (AEVAL (LIST 'PLUS RESULT (LIST 'TIMES 'I IC)))))
       (T
        (SETQ RESULT
                (AEVAL
                 (LIST 'PLUS
                       (LIST 'TIMES
                             (COND ((EVALLESSP (AEVAL IC) 0) 1) (T (MINUS 1)))
                             (LIST 'SQRT (LIST 'MINUS (LIST 'EXPT IC 2))))
                       RESULT)))))
      (RETURN (AEVAL RESULT)))) 
(ENDMODULE) 