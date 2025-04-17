(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFGAMM)) 
(IMPORTS (LIST 'COMPLEX*OFF*SWITCH 'COMPLEX*RESTORE*SWITCH 'SF*EVAL)) 
(EXPORTS (LIST 'DO*GAMMA 'DO*POCHHAMMER 'DO*POCH*CONJ*CALC)) 
(FLUID '(COMPUTE-BERNOULLI)) 
(PUT 'DO*GAMMA 'NUMBER-OF-ARGS 1) 
(FLAG '(DO*GAMMA) 'OPFN) 
(PUT 'DO*GAMMA 'DEFINED-ON-LINE '271) 
(PUT 'DO*GAMMA 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'DO*GAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DO*GAMMA (Z)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
      (AEVAL (SF*EVAL 'GAMMA*CALC*S (LIST 'LIST Z))))
     (T (AEVAL (SF*EVAL 'GAMMA*CALC (LIST 'LIST Z)))))) 
(PUT 'GAMMA*CALC*S 'NUMBER-OF-ARGS 1) 
(FLAG '(GAMMA*CALC*S) 'OPFN) 
(PUT 'GAMMA*CALC*S 'DEFINED-ON-LINE '278) 
(PUT 'GAMMA*CALC*S 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'GAMMA*CALC*S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GAMMA*CALC*S (Z)
    (PROG (SCALE RESULT ALGLIST* P PRECOM)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ P 0)
      (SETQ PRECOM 0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ P (AEVAL (LIST 'PRECISION 0)))
      (SETK 'OP (AEVAL |:BPREC:|))
      (COND
       ((EVALLESSP P (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 1)))))
      (COND ((GREATERP P 49) (SETQ SCALE (AEVAL (PLUS 500 P))))
            (T (SETQ SCALE (AEVAL (TIMES 10 (PLUS P 1))))))
      (COND ((EVALGREATERP (AEVAL Z) (AEVAL SCALE)) (SETQ SCALE (AEVAL 2))))
      (SETQ RESULT (AEVAL (LIST 'GAMMA*CALC*S*SUB Z SCALE 'OP)))
      (AEVAL (LIST 'PRECISION P))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'GAMMA*CALC*S*SUB 'NUMBER-OF-ARGS 3) 
(FLAG '(GAMMA*CALC*S*SUB) 'OPFN) 
(PUT 'GAMMA*CALC*S*SUB 'DEFINED-ON-LINE '298) 
(PUT 'GAMMA*CALC*S*SUB 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'GAMMA*CALC*S*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GAMMA*CALC*S*SUB (Z SCALE OP)
    (PROG (ZA Z1 RESULT Z0)
      (SETQ Z0 0)
      (SETQ ZA (AEVAL Z))
      (SETQ Z0 (AEVAL (LIST 'FLOOR (LIST 'PLUS Z 1))))
      (SETQ Z1 (AEVAL (LIST 'PLUS Z SCALE)))
      (SETQ RESULT (AEVAL (LOG*GAMMA Z1 Z0)))
      (SETQ RESULT
              (AEVAL
               (LIST 'QUOTIENT (LIST 'EXP RESULT) (LIST 'POCHHAMMER Z SCALE))))
      (RETURN (AEVAL RESULT)))) 
(PUT 'LOG*GAMMA 'NUMBER-OF-ARGS 2) 
(PUT 'LOG*GAMMA 'DEFINED-ON-LINE '308) 
(PUT 'LOG*GAMMA 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'LOG*GAMMA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOG*GAMMA (Z ZINT)
    (PROG (RESULT THIS ZPWR ZSQ ADMISSABLE ABK K RP ORDA MAGN)
      (SETQ K 0)
      (SETQ RP 0)
      (SETQ ORDA 0)
      (SETQ MAGN 0)
      (SETQ MAGN (EXPT 2 |:BPREC:|))
      (COND
       ((LESSP ZINT 1000)
        (SETQ ADMISSABLE
                (NORMBF
                 (|ROUND:MT|
                  (|TIMES:|
                   (CONS '|:RD:|
                         (CONS (|MSD:| (PLUS 1 (QUOTIENT (FACTORIAL ZINT) 3)))
                               0))
                   (CONS '|:RD:| (CONS 1 (MINUS |:BPREC:|))))
                  |:BPREC:|))))
       (T
        (SETQ ADMISSABLE
                (NORMBF
                 (|DIVIDE:|
                  (DIFBF
                   (|LOG:|
                    (NORMBF
                     (|ROUND:MT|
                      (|TIMES:| (PLUBF BFTWO* BFHALF*)
                                (|SQRT:|
                                 (EXPTBF (CONS '|:RD:| (CONS ZINT 0))
                                         (PLUS (TIMES 2 ZINT) 1) BFONE*)
                                 8))
                      |:BPREC:|))
                    8)
                   (CONS '|:RD:| (CONS ZINT 0)))
                  (CONS '|:RD:| (CONS 1 |:BPREC:|)) |:BPREC:|)))))
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
      (SETQ RESULT
              (NORMBF
               (|ROUND:MT| (|TIMES:| (LOG* Z) (|DIFFERENCE:| Z BFHALF*))
                           |:BPREC:|)))
      (SETQ RESULT
              (PLUBF (|DIFFERENCE:| RESULT Z)
                     (NORMBF
                      (|ROUND:MT|
                       (|TIMES:| BFHALF*
                                 (LOG*
                                  (NORMBF
                                   (|ROUND:MT| (|TIMES:| (PI*) BFTWO*)
                                               |:BPREC:|))))
                       |:BPREC:|))))
      (SETQ THIS (PLUBF ADMISSABLE BFONE*))
      (SETQ RP |:BPREC:|)
      (SETQ ORDA (DIFFERENCE (|ORDER:| ADMISSABLE) 5))
      (SETQ K 2)
      (SETQ ZPWR Z)
      (SETQ ZSQ (NORMBF (|ROUND:MT| (|TIMES:| Z Z) |:BPREC:|)))
      (COND
       ((NULL COMPUTE-BERNOULLI)
        (PROGN (ERRORSET* '(LOAD_PACKAGE '(SPECFAUX)) NIL) NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| (|ABS:| THIS) ADMISSABLE)) (RETURN NIL)))
        (PROGN
         (SETQ ABK
                 ((LAMBDA (U) (COND ((ATOM U) U) (T (CONS '|:RD:| U))))
                  (CDR (*Q2F (SIMP* (RETRIEVE*BERN K))))))
         (SETQ THIS
                 (|DIVIDE:| ABK
                            (NORMBF
                             (|ROUND:MT|
                              (|TIMES:|
                               (CONS '|:RD:|
                                     (CONS (TIMES K (DIFFERENCE K 1)) 0))
                               ZPWR)
                              |:BPREC:|))
                            RP))
         (SETQ RP (DIFFERENCE (|ORDER:| THIS) ORDA))
         (SETQ RESULT (PLUBF RESULT THIS))
         (SETQ ZPWR (NORMBF (|ROUND:MT| (|TIMES:| ZPWR ZSQ) |:BPREC:|)))
         (SETQ K (PLUS K 2))
         NIL)
        (GO WHILELABEL))
      (RETURN (MK*SQ (CONS (MKROUND RESULT) 1))))) 
(PUT 'LOGGAMMA*CALC*SUB 'NUMBER-OF-ARGS 3) 
(FLAG '(LOGGAMMA*CALC*SUB) 'OPFN) 
(PUT 'LOGGAMMA*CALC*SUB 'DEFINED-ON-LINE '369) 
(PUT 'LOGGAMMA*CALC*SUB 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'LOGGAMMA*CALC*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOGGAMMA*CALC*SUB (Z PREMIER DERNIER)
    (PROG (RESULT FT SOFAR DIV)
      (COND
       ((EVALEQUAL (AEVAL PREMIER) 2)
        (SETQ RESULT
                (AEVAL
                 (LIST 'PLUS
                       (LIST 'DIFFERENCE
                             (LIST 'TIMES
                                   (LIST 'DIFFERENCE Z (LIST 'QUOTIENT 1 2))
                                   (LIST 'LOG Z))
                             Z)
                       (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                             (LIST 'LOG (LIST 'TIMES 2 'PI)))))))
       (T (SETQ RESULT (AEVAL 0))))
      (SETQ SOFAR (AEVAL (LIST 'EXPT Z (LIST 'DIFFERENCE DERNIER 1))))
      (SETQ DIV (AEVAL (LIST 'EXPT Z 2)))
      (SETQ RESULT
              (AEVAL
               (LIST 'PLUS RESULT
                     (SETQ FT
                             (AEVAL
                              (LIST 'QUOTIENT (LIST 'BERNOULLI*CALC DERNIER)
                                    (LIST 'TIMES (LIST 'DIFFERENCE DERNIER 1)
                                          DERNIER SOFAR)))))))
      (PROG (N)
        (SETQ N (AEVAL* (LIST 'DIFFERENCE DERNIER 2)))
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'TIMES (MINUS 2) (LIST 'DIFFERENCE (AEVAL* PREMIER) N)))
          (RETURN NIL)))
        (PROGN
         (SETQ SOFAR (AEVAL* (LIST 'QUOTIENT SOFAR DIV)))
         (SETQ RESULT
                 (AEVAL*
                  (LIST 'PLUS RESULT
                        (LIST 'QUOTIENT (LIST 'BERNOULLI*CALC N)
                              (LIST 'TIMES N (LIST 'DIFFERENCE N 1) SOFAR))))))
        (SETQ N
                ((LAMBDA (FORALL-RESULT)
                   (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 2))))
                 N))
        (GO LAB))
      (RETURN (AEVAL (LIST 'LIST RESULT FT))))) 
(PUT 'GAMMA*CALC*SUB 'NUMBER-OF-ARGS 2) 
(FLAG '(GAMMA*CALC*SUB) 'OPFN) 
(PUT 'GAMMA*CALC*SUB 'DEFINED-ON-LINE '397) 
(PUT 'GAMMA*CALC*SUB 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'GAMMA*CALC*SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GAMMA*CALC*SUB (Z SCALE)
    (PROG (RESULT EXPRESULT FT ERR NEWERR RESCALE ADMISSABLE ALGLIST* PREPRE
           PREMIER DERNIER)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ PREPRE 0)
      (SETQ PREMIER 0)
      (SETQ DERNIER 0)
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (SETQ RESCALE
              (PROG (K FORALL-RESULT)
                (SETQ K 1)
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* SCALE) K))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'TIMES (AEVAL* (LIST 'PLUS Z (DIFFERENCE K 1)))
                               FORALL-RESULT)))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB1)))
      (SETQ ADMISSABLE
              (AEVAL (LIST 'QUOTIENT 1 (LIST 'EXPT 10 (PLUS PREPRE 4)))))
      (SETQ ERR (AEVAL (LIST 'PLUS ADMISSABLE 1)))
      (SETQ PREMIER (AEVAL 2))
      (SETQ DERNIER (AEVAL 50))
      (SETQ RESULT (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* ERR) (AEVAL* ADMISSABLE))
             (PROGN
              (SETQ FT
                      (AEVAL*
                       (LIST 'LOGGAMMA*CALC*SUB (LIST 'PLUS Z SCALE) PREMIER
                             DERNIER)))
              (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT (LIST 'FIRST FT))))
              (SETQ FT (AEVAL* (LIST 'SECOND FT)))
              (SETQ EXPRESULT (AEVAL* (LIST 'EXP RESULT)))
              (SETQ NEWERR
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'ABS
                                   (LIST 'DIFFERENCE
                                         (LIST 'QUOTIENT EXPRESULT
                                               (LIST 'EXP FT))
                                         EXPRESULT))
                             RESCALE)))
              (COND
               ((OR (EVALGREATERP (AEVAL* NEWERR) (AEVAL* ERR))
                    (AND (GREATERP DERNIER 180)
                         (EVALGREATERP (AEVAL* NEWERR)
                                       (AEVAL*
                                        (LIST 'TIMES ADMISSABLE 1000)))))
                (PROGN
                 (SETQ SCALE (AEVAL* (LIST 'TIMES SCALE 3)))
                 (SETQ RESCALE
                         (PROG (M FORALL-RESULT)
                           (SETQ M 1)
                           (SETQ FORALL-RESULT 1)
                          LAB1
                           (COND
                            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* SCALE) M))
                             (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (AEVAL*
                                    (LIST 'TIMES
                                          (AEVAL*
                                           (LIST 'PLUS Z (DIFFERENCE M 1)))
                                          FORALL-RESULT)))
                           (SETQ M
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    M))
                           (GO LAB1)))
                 (PROGN
                  (ASSGNPRI (AEVAL* "Scaling up to scale=") NIL 'FIRST)
                  (ASSGNPRI (AEVAL* SCALE) NIL NIL)
                  (ASSGNPRI (AEVAL* " (from ") NIL NIL)
                  (ASSGNPRI (AEVAL* (LIST 'QUOTIENT SCALE 3)) NIL NIL)
                  (ASSGNPRI (AEVAL* ")") NIL 'LAST))
                 (SETQ RESULT (AEVAL* 0))
                 (SETQ PREMIER (AEVAL* 2))
                 (SETQ DERNIER (AEVAL* 100))
                 (SETQ ERR (AEVAL* (LIST 'PLUS ADMISSABLE 1)))))
               (T
                (PROGN
                 (SETQ ERR (AEVAL* NEWERR))
                 (SETQ PREMIER (AEVAL* (PLUS DERNIER 2)))
                 (SETQ DERNIER (AEVAL* (PLUS DERNIER 30))))))
              (AEVAL* 'NIL)))
      (SETQ RESULT (AEVAL (LIST 'QUOTIENT EXPRESULT RESCALE)))
      (RETURN (AEVAL (LIST 'LIST RESULT ERR))))) 
(PUT 'GAMMA*CALC 'NUMBER-OF-ARGS 1) 
(FLAG '(GAMMA*CALC) 'OPFN) 
(PUT 'GAMMA*CALC 'DEFINED-ON-LINE '449) 
(PUT 'GAMMA*CALC 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'GAMMA*CALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GAMMA*CALC (Z)
    (COND
     ((EVALGREATERP (AEVAL (LIST 'PRECISION 0)) 49)
      (AEVAL
       (LIST 'FIRST
             (LIST 'GAMMA*CALC*SUB Z
                   (LIST 'PLUS 500 (LIST 'TIMES 4 (LIST 'PRECISION 0)))))))
     (T
      (AEVAL
       (LIST 'FIRST
             (LIST 'GAMMA*CALC*SUB Z
                   (LIST 'CEILING
                         (LIST 'TIMES
                               (LIST 'EXP
                                     (LIST 'QUOTIENT (LIST 'PRECISION 0) 10))
                               2)))))))) 
(PUT 'DO*POCHHAMMER 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*POCHHAMMER) 'OPFN) 
(PUT 'DO*POCHHAMMER 'DEFINED-ON-LINE '463) 
(PUT 'DO*POCHHAMMER 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'DO*POCHHAMMER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*POCHHAMMER (A K) (SF*EVAL 'POCHHAMMER*CALC (LIST 'LIST A K))) 
(PUT 'DO*POCH*CONJ*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(DO*POCH*CONJ*CALC) 'OPFN) 
(PUT 'DO*POCH*CONJ*CALC 'DEFINED-ON-LINE '466) 
(PUT 'DO*POCH*CONJ*CALC 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'DO*POCH*CONJ*CALC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DO*POCH*CONJ*CALC (Z N) (SF*EVAL 'POCH*CONJ*CALC (LIST 'LIST Z N))) 
(PUT 'POCHHAMMER*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(POCHHAMMER*CALC) 'OPFN) 
(PUT 'POCHHAMMER*CALC 'DEFINED-ON-LINE '470) 
(PUT 'POCHHAMMER*CALC 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCHHAMMER*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POCHHAMMER*CALC (A K)
    (COND
     ((AND (FIXP (REVALX A)) (NOT (BOOLVALUE* (REVALX *ROUNDED))))
      (AEVAL (FAC-PART A (PLUS A (DIFFERENCE K 1)))))
     (T (AEVAL (LIST 'POCHHAMMER*CALC*SUB A K))))) 
(PUT 'POCHHAMMER*CALC*SUB 'NUMBER-OF-ARGS 2) 
(FLAG '(POCHHAMMER*CALC*SUB) 'OPFN) 
(PUT 'POCHHAMMER*CALC*SUB 'DEFINED-ON-LINE '476) 
(PUT 'POCHHAMMER*CALC*SUB 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCHHAMMER*CALC*SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POCHHAMMER*CALC*SUB (A K)
    (PROG (RESULT PREPRE PRECOM A0)
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (SETQ PREPRE (AEVAL (LIST 'PRECISION 0)))
      (COND
       ((EVALLESSP (AEVAL PREPRE) (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS 1 !NFPD)))))
      (SETQ A0 (AEVAL A))
      (SETQ RESULT (AEVAL (POCHHAMMER*CALC*SUB*SUB*NEWBF A K)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'POCHHAMMER*CALC*SUB*SUB*NEWBF 'NUMBER-OF-ARGS 2) 
(PUT 'POCHHAMMER*CALC*SUB*SUB*NEWBF 'DEFINED-ON-LINE '489) 
(PUT 'POCHHAMMER*CALC*SUB*SUB*NEWBF 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCHHAMMER*CALC*SUB*SUB*NEWBF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POCHHAMMER*CALC*SUB*SUB*NEWBF (A K)
    (PROG (RESULT)
      (COND
       ((FIXP A)
        (SETQ RESULT
                (POCH*SUB*2 0 (DIFFERENCE K 1) (CONS '|:RD:| (CONS A 0)))))
       (T
        (PROGN
         (SETQ A
                 (COND ((FIXP A) (CONS '|:RD:| (CONS A 0)))
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
                         (*Q2F (SIMP* A))))))
         (COND
          ((LESSP (|ORDER:| A) (MINUS |:BPREC:|))
           (SETQ RESULT (POCH*SUB*2 0 (DIFFERENCE K 1) BFONE*)))
          ((AND (LESSP (|MSD:| (ABS (CADR A))) (QUOTIENT |:BPREC:| 2))
                (GREATERP (|ORDER:| A) (MINUS 2)))
           (SETQ RESULT (POCH*SUB*2 0 (DIFFERENCE K 1) A)))
          (T (SETQ RESULT (POCH*SUB*1 A (DIFFERENCE K 1) BFONE*)))))))
      (RETURN (MK*SQ (CONS RESULT 1))))) 
(PUT 'POCH*SUB*1 'NUMBER-OF-ARGS 3) 
(PUT 'POCH*SUB*1 'DEFINED-ON-LINE '503) 
(PUT 'POCH*SUB*1 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCH*SUB*1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POCH*SUB*1 (A K TOT)
    (COND ((EQUAL K 0) (NORMBF (|ROUND:MT| (|TIMES:| TOT A) |:BPREC:|)))
          (T
           (POCH*SUB*1 (|PLUS:| A BFONE*) (DIFFERENCE K 1)
            (NORMBF (|ROUND:MT| (|TIMES:| TOT A) |:BPREC:|)))))) 
(PUT 'POCH*SUB*2 'NUMBER-OF-ARGS 3) 
(PUT 'POCH*SUB*2 'DEFINED-ON-LINE '508) 
(PUT 'POCH*SUB*2 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCH*SUB*2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POCH*SUB*2 (M N A)
    (COND ((EQUAL M N) (|PLUS:| A (CONS '|:RD:| (CONS M 0))))
          ((EQUAL M (DIFFERENCE N 1))
           (NORMBF
            (|ROUND:MT|
             (|TIMES:| (|PLUS:| A (CONS '|:RD:| (CONS M 0)))
                       (|PLUS:| A (CONS '|:RD:| (CONS N 0))))
             |:BPREC:|)))
          (T
           ((LAMBDA (P)
              (NORMBF
               (|ROUND:MT|
                (|TIMES:| (POCH*SUB*2 M P A) (POCH*SUB*2 (PLUS P 1) N A))
                |:BPREC:|)))
            (QUOTIENT (PLUS M N) 2))))) 
(PUT 'POCH*CONJ*CALC 'NUMBER-OF-ARGS 2) 
(FLAG '(POCH*CONJ*CALC) 'OPFN) 
(PUT 'POCH*CONJ*CALC 'DEFINED-ON-LINE '516) 
(PUT 'POCH*CONJ*CALC 'DEFINED-IN-FILE 'SPECFN/SFGAMM.RED) 
(PUT 'POCH*CONJ*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POCH*CONJ*CALC (Z N)
    (PROG (I FORALL-RESULT)
      (SETQ I 1)
      (SETQ FORALL-RESULT 1)
     LAB1
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (AEVAL*
               (LIST 'TIMES
                     (AEVAL*
                      (LIST 'PLUS
                            (LIST 'EXPT
                                  (LIST 'PLUS (LIST 'REPART Z)
                                        (DIFFERENCE I 1))
                                  2)
                            (LIST 'EXPT (LIST 'IMPART Z) 2)))
                     FORALL-RESULT)))
      (SETQ I
              ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
               I))
      (GO LAB1))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (PROD (~ N) (~ N) (~ ANF) (~ ENDE))
      (WHEN (QUOTIENT (GAMMA (PLUS ENDE 1)) (GAMMA ANF))
       (NOT (AND (FIXP ANF) (LESSP ANF 0)))))
     (REPLACEBY (PROD (~ N) (~ N) (~ ANF))
      (WHEN (QUOTIENT (GAMMA (PLUS N 1)) (GAMMA ANF))
       (NOT (AND (FIXP ANF) (LESSP ANF 0)))))
     (REPLACEBY (PROD (PLUS (~ K) (~ N)) K (~ NANF) (~ NEND))
      (WHEN (QUOTIENT (GAMMA (PLUS NEND 1 N)) (GAMMA (PLUS NANF N)))
       (AND (NUMBERP NANF) (NUMBERP N) (GREATERP (PLUS NANF N) 0))))
     (REPLACEBY (PROD (PLUS (~ K) (~ N)) K (~ NANF) (~ NEND))
      (WHEN 0 (AND (NUMBERP NANF) (NUMBERP N) (EQUAL NANF (MINUS N)))))
     (REPLACEBY (PROD (PLUS (TIMES (~ (~ A)) (~ K)) (~ N)) K (~ NANF) (~ NEND))
      (WHEN
       (TIMES (PROD A K NANF NEND)
              (QUOTIENT (GAMMA (PLUS NEND 1 (QUOTIENT N A)))
                        (GAMMA (PLUS NANF (QUOTIENT N A)))))
       (AND (FREEOF A K) (FREEOF N K))))
     (REPLACEBY
      (SLASH (TIMES (~ (~ U)) (GAMMA (PLUS (~ X) (~ (~ N0)))))
       (TIMES (~ (~ V)) (GAMMA (PLUS X (~ (~ N1))))))
      (WHEN
       (QUOTIENT (TIMES U (GAMMA (PLUS (~ X) N0)))
                 (TIMES V (PLUS X (DIFFERENCE N1 1))
                        (GAMMA (PLUS X (DIFFERENCE N1 1)))))
       (AND (NOT (AND (NUMBERP X) (EQ X 0))) (FIXP N0) (FIXP N1) (LESSP N0 N1)
            (LESSP (DIFFERENCE N1 N0) 6))))
     (REPLACEBY
      (SLASH (TIMES (~ (~ U)) (GAMMA (PLUS (~ X) (~ (~ N0)))))
       (TIMES (~ (~ V)) (GAMMA (PLUS X (~ (~ N1))))))
      (WHEN
       (QUOTIENT
        (TIMES U (GAMMA (PLUS (~ X) (DIFFERENCE N0 1)))
               (PLUS X (DIFFERENCE N0 1)))
        (TIMES V (GAMMA (PLUS X N1))))
       (AND (NOT (AND (NUMBERP X) (EQ X 0))) (FIXP N0) (FIXP N1)
            (GREATERP N0 N1) (LESSP (DIFFERENCE N0 N1) 6)))))))) 
(ENDMODULE) 