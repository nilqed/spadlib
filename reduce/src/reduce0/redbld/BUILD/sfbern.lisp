(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFBERN)) 
(FLUID '(COMPUTE-BERNOULLI)) 
(IMPORTS (LIST 'COMPLEX*ON*SWITCH 'COMPLEX*OFF*SWITCH 'COMPLEX*RESTORE*SWITCH)) 
(EXPORTS
 (LIST 'NEAREST-INT-TO-BF 'BERNOULLI*CALC 'MULTI*BERN 'SINGLE*BERN
       'RETRIEVE*BERN)) 
(FLAG '(EULER BERNOULLI) 'SPECFN) 
(DEFLIST '((EULER 1) (BERNOULLI 1)) 'NUMBER-OF-ARGS) 
(PUT 'BERNOULLI*CALC 'NUMBER-OF-ARGS 1) 
(FLAG '(BERNOULLI*CALC) 'OPFN) 
(PUT 'BERNOULLI*CALC 'DEFINED-ON-LINE '69) 
(PUT 'BERNOULLI*CALC 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'BERNOULLI*CALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BERNOULLI*CALC (N)
    (PROG (PRECOM RESULT PREPRE)
      (COND
       ((BOOLVALUE* (REVALX (NULL COMPUTE-BERNOULLI)))
        (PROGN (ERRORSET* '(LOAD_PACKAGE '(SPECFAUX)) NIL) NIL)))
      (SETQ PRECOM (AEVAL (LIST 'COMPLEX*OFF*SWITCH)))
      (COND
       ((EVALLESSP (SETQ PREPRE (AEVAL (LIST 'PRECISION 0))) (AEVAL !NFPD))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS !NFPD 1)))))
      (SETQ RESULT (AEVAL (RETRIEVE*BERN N)))
      (AEVAL (LIST 'PRECISION PREPRE))
      (AEVAL (LIST 'COMPLEX*RESTORE*SWITCH PRECOM))
      (RETURN (AEVAL RESULT)))) 
(PUT 'RETRIEVE*BERN 'NUMBER-OF-ARGS 1) 
(PUT 'RETRIEVE*BERN 'DEFINED-ON-LINE '93) 
(PUT 'RETRIEVE*BERN 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'RETRIEVE*BERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RETRIEVE*BERN (N)
    (PROG (INFO RESULT HELDPRE)
      (SETQ HELDPRE 0)
      (SETQ INFO (ASSOC N BERNOULLI-ALIST))
      (COND ((NOT INFO) (SETQ RESULT (BERN*CALC N '(NIL NIL NIL))))
            (T
             (PROGN
              (SETQ INFO (CDR INFO))
              (COND
               (*ROUNDED
                (COND
                 ((AND (SETQ HELDPRE (CADR INFO)) (GEQ HELDPRE |:BPREC:|))
                  (SETQ RESULT (MK*SQ (CONS (|RD:PREP| (CADDR INFO)) 1))))
                 ((SETQ RESULT (CAR INFO))
                  (SETQ RESULT
                          (MK*SQ
                           (CONS
                            (MKROUND
                             (NORMBF
                              (|DIVIDE:| (CONS '|:RD:| (CONS (CAADR RESULT) 0))
                                         (CONS '|:RD:| (CONS (CDADR RESULT) 0))
                                         |:BPREC:|)))
                            1))))
                 (T (SETQ RESULT (BERN*CALC N INFO)))))
               ((NOT (SETQ RESULT (CAR INFO)))
                (SETQ RESULT (BERN*CALC N INFO)))))))
      (RETURN RESULT))) 
(PUT 'BERN*CALC 'NUMBER-OF-ARGS 2) 
(PUT 'BERN*CALC 'DEFINED-ON-LINE '114) 
(PUT 'BERN*CALC 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'BERN*CALC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BERN*CALC (N INFO)
    (PROG (RESULT)
      (SETQ RESULT (SINGLE*BERN (QUOTIENT N 2)))
      (COND (*ROUNDED (SETQ INFO (LIST (CAR INFO) |:BPREC:| RESULT)))
            (T (SETQ INFO (LIST RESULT (CADR INFO) (CADDR INFO)))))
      (SETQ BERNOULLI-ALIST (CONS (CONS N INFO) BERNOULLI-ALIST))
      (RETURN RESULT))) 
(PUT 'NEAREST-INT-TO-BF 'NUMBER-OF-ARGS 1) 
(PUT 'NEAREST-INT-TO-BF 'DEFINED-ON-LINE '139) 
(PUT 'NEAREST-INT-TO-BF 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'NEAREST-INT-TO-BF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEAREST-INT-TO-BF (X)
    ((LAMBDA (RB) (|CONV:BF2I| RB))
     (COND ((|LESSP:| X BFZ*) (|DIFFERENCE:| X BFHALF*))
           (T (|PLUS:| X BFHALF*))))) 
(PUT 'MULTI*BERN 'NUMBER-OF-ARGS 1) 
(PUT 'MULTI*BERN 'DEFINED-ON-LINE '161) 
(PUT 'MULTI*BERN 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'MULTI*BERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MULTI*BERN (N)
    (PROG (RESULTS PRIMES TPRIMES R0 RK RKM1 B2K TPI PIE TK N2K THISP GN PREPRE
           PRERND P2K K2 PLIM D2K)
      (SETQ THISP 0)
      (SETQ GN 0)
      (SETQ PREPRE 0)
      (SETQ PRERND 0)
      (SETQ P2K 0)
      (SETQ K2 0)
      (SETQ PLIM 0)
      (SETQ D2K 0)
      (SETQ RESULTS NIL)
      (SETQ PRERND *ROUNDED)
      (COND ((NOT PRERND) (ON (LIST 'ROUNDED))))
      (SETQ PREPRE (PRECISION 0))
      (SETQ GN (TIMES 2 N (|MSD:| N)))
      (COND ((LESSP GN !NBFPD) (PRECISION (PLUS !NFPD 2)))
            ((OR (GREATERP PREPRE (QUOTIENT GN 3)) (NOT PRERND))
             (PRECISION (PLUS (QUOTIENT GN 3) 1)))
            (T (PRECISION (PLUS PREPRE 2))))
      (SETQ TPI (PI*))
      (SETQ PIE
              (NORMBF
               (|DIVIDE:| BFONE*
                          (NORMBF (|ROUND:MT| (|TIMES:| TPI (E*)) |:BPREC:|))
                          |:BPREC:|)))
      (COND ((LESSP N 1786) (SETQ PRIMES *PRIMELIST*))
            (T
             (PROGN
              (SETQ PRIMES NIL)
              (PROG (THISP)
                (SETQ THISP 3573)
               LAB
                (COND
                 ((MINUSP (TIMES 2 (DIFFERENCE (PLUS (TIMES 2 N) 1) THISP)))
                  (RETURN NIL)))
                (COND ((PRIMEP THISP) (SETQ PRIMES (CONS THISP PRIMES))))
                (SETQ THISP (PLUS2 THISP 2))
                (GO LAB))
              (SETQ PRIMES (APPEND *PRIMELIST* (REVERSE PRIMES))))))
      (SETQ R0
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
               (AEVAL (LIST 'EXPT (LIST 'TIMES 2 'PI) (MINUS 2)))))
      (SETQ RKM1
              (NORMBF
               (|ROUND:MT| (|TIMES:| (CONS '|:RD:| (CONS 4 0)) R0) |:BPREC:|)))
      (PROG (K)
        (SETQ K 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
        (PROGN
         (SETQ K2 (TIMES 2 K))
         (SETQ RK
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:|
                    (CONS '|:RD:| (CONS (TIMES K2 (DIFFERENCE K2 1)) 0))
                    (NORMBF (|ROUND:MT| (|TIMES:| R0 RKM1) |:BPREC:|)))
                   |:BPREC:|)))
         (SETQ RKM1 RK)
         (SETQ TK BFONE*)
         (SETQ D2K 1)
         (SETQ PLIM
                 (PLUS 1
                       (|CONV:BF2I|
                        (NORMBF
                         (|ROUND:MT| (|TIMES:| (CONS '|:RD:| (CONS K2 0)) PIE)
                                     |:BPREC:|)))))
         (SETQ TPRIMES (CDR PRIMES))
         (SETQ THISP (CAR PRIMES))
         (PROG ()
          WHILELABEL
           (COND ((NOT (LEQ THISP PLIM)) (RETURN NIL)))
           (PROGN
            (SETQ P2K (EXPT THISP K2))
            (SETQ TK
                    (NORMBF
                     (|ROUND:MT|
                      (|TIMES:| TK
                                (NORMBF
                                 (|DIVIDE:| (CONS '|:RD:| (CONS P2K 0))
                                            (CONS '|:RD:|
                                                  (CONS (DIFFERENCE P2K 1) 0))
                                            |:BPREC:|)))
                      |:BPREC:|)))
            (SETQ THISP (CAR TPRIMES))
            (SETQ TPRIMES (CDR TPRIMES)))
           (GO WHILELABEL))
         (SETQ TPRIMES (CDR PRIMES))
         (SETQ THISP (CAR PRIMES))
         (PROG ()
          WHILELABEL
           (COND ((NOT (LEQ THISP (PLUS K 1))) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (CDR (DIVIDE K2 (DIFFERENCE THISP 1))) 0)
              (SETQ D2K (TIMES D2K THISP))))
            (SETQ THISP (CAR TPRIMES))
            (SETQ TPRIMES (CDR TPRIMES)))
           (GO WHILELABEL))
         (COND ((PRIMEP (PLUS K2 1)) (SETQ D2K (TIMES D2K (PLUS K2 1)))))
         (SETQ N2K
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:| (NORMBF (|ROUND:MT| (|TIMES:| RK TK) |:BPREC:|))
                             (CONS '|:RD:| (CONS D2K 0)))
                   |:BPREC:|)))
         (COND
          (PRERND
           (SETQ B2K
                   (MK*SQ
                    (CONS
                     (MKROUND
                      (NORMBF
                       (|DIVIDE:|
                        (CONS '|:RD:|
                              (CONS
                               (TIMES (EXPT (MINUS 1) (PLUS K 1))
                                      (NEAREST-INT-TO-BF N2K))
                               0))
                        (CONS '|:RD:| (CONS D2K 0)) |:BPREC:|)))
                     1))))
          (T
           (SETQ B2K
                   (LIST '*SQ
                         (CONS
                          (TIMES (EXPT (MINUS 1) (PLUS K 1))
                                 (NEAREST-INT-TO-BF N2K))
                          D2K)
                         T))))
         (SETQ RESULTS (CONS B2K RESULTS)))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PRECISION PREPRE)
      (COND ((NOT PRERND) (OFF (LIST 'ROUNDED))))
      (RETURN RESULTS))) 
(PUT 'SINGLE*BERN 'NUMBER-OF-ARGS 1) 
(PUT 'SINGLE*BERN 'DEFINED-ON-LINE '236) 
(PUT 'SINGLE*BERN 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT 'SINGLE*BERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SINGLE*BERN (N)
    (PROG (RESULT PRIMES TPRIMES RN TN N2N PIE D2N THISP GN PREPRE PRERND P2N
           N2 PLIM)
      (SETQ D2N 0)
      (SETQ THISP 0)
      (SETQ GN 0)
      (SETQ PREPRE 0)
      (SETQ PRERND 0)
      (SETQ P2N 0)
      (SETQ N2 0)
      (SETQ PLIM 0)
      (SETQ PRERND *ROUNDED)
      (COND ((NOT PRERND) (ON (LIST 'ROUNDED))))
      (SETQ PREPRE (PRECISION 0))
      (SETQ GN (TIMES 2 N (|MSD:| N)))
      (COND ((LESSP GN !NBFPD) (PRECISION (PLUS !NFPD 2)))
            ((OR (GREATERP PREPRE (QUOTIENT GN 3)) (NOT PRERND))
             (PRECISION (PLUS (QUOTIENT GN 3) 1)))
            (T (PRECISION (PLUS PREPRE 2))))
      (SETQ PIE
              (NORMBF
               (|DIVIDE:| BFONE*
                          (NORMBF (|ROUND:MT| (|TIMES:| (PI*) (E*)) |:BPREC:|))
                          |:BPREC:|)))
      (COND ((LESSP N 1786) (SETQ PRIMES *PRIMELIST*))
            (T
             (PROGN
              (SETQ PRIMES NIL)
              (PROG (THISP)
                (SETQ THISP 3573)
               LAB
                (COND
                 ((MINUSP (TIMES 2 (DIFFERENCE (PLUS (TIMES 2 N) 1) THISP)))
                  (RETURN NIL)))
                (COND ((PRIMEP THISP) (SETQ PRIMES (CONS THISP PRIMES))))
                (SETQ THISP (PLUS2 THISP 2))
                (GO LAB))
              (SETQ PRIMES (APPEND *PRIMELIST* (REVERSE PRIMES))))))
      (SETQ N2 (TIMES 2 N))
      (SETQ RN
              (NORMBF
               (|DIVIDE:| (CONS '|:RD:| (CONS (TIMES 2 (FACTORIAL N2)) 0))
                          ((LAMBDA (X)
                             (COND ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                   (T
                                    ((LAMBDA (Y)
                                       (COND
                                        ((NEQ (CAR Y) '|:RD:|)
                                         ((LAMBDA (U)
                                            (COND ((ATOM U) U)
                                                  (T (CONS '|:RD:| U))))
                                          (CDR (*RN2RD Y))))
                                        (T
                                         (COND ((ATOM (CDR Y)) (CDR Y))
                                               (T (CONS '|:RD:| (CDR Y)))))))
                                     (*Q2F (SIMP* X))))))
                           (AEVAL (LIST 'EXPT (LIST 'TIMES 2 'PI) N2)))
                          |:BPREC:|)))
      (SETQ TN BFONE*)
      (SETQ D2N 1)
      (SETQ PLIM
              (PLUS 1
                    (|CONV:BF2I|
                     (NORMBF
                      (|ROUND:MT| (|TIMES:| (CONS '|:RD:| (CONS N2 0)) PIE)
                                  |:BPREC:|)))))
      (SETQ TPRIMES (CDR PRIMES))
      (SETQ THISP (CAR PRIMES))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ THISP PLIM)) (RETURN NIL)))
        (PROGN
         (SETQ P2N (EXPT THISP N2))
         (SETQ TN
                 (NORMBF
                  (|ROUND:MT|
                   (|TIMES:| TN
                             (NORMBF
                              (|DIVIDE:| (CONS '|:RD:| (CONS P2N 0))
                                         (CONS '|:RD:|
                                               (CONS (DIFFERENCE P2N 1) 0))
                                         |:BPREC:|)))
                   |:BPREC:|)))
         (SETQ THISP (CAR TPRIMES))
         (SETQ TPRIMES (CDR TPRIMES)))
        (GO WHILELABEL))
      (SETQ TPRIMES (CDR PRIMES))
      (SETQ THISP (CAR PRIMES))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ THISP (PLUS N 1))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (CDR (DIVIDE N2 (DIFFERENCE THISP 1))) 0)
           (SETQ D2N (TIMES D2N THISP))))
         (SETQ THISP (CAR TPRIMES))
         (SETQ TPRIMES (CDR TPRIMES)))
        (GO WHILELABEL))
      (COND ((PRIMEP (PLUS N2 1)) (SETQ D2N (TIMES D2N (PLUS N2 1)))))
      (SETQ N2N
              (NORMBF
               (|ROUND:MT|
                (|TIMES:| (NORMBF (|ROUND:MT| (|TIMES:| RN TN) |:BPREC:|))
                          (CONS '|:RD:| (CONS D2N 0)))
                |:BPREC:|)))
      (PRECISION PREPRE)
      (COND
       (PRERND
        (SETQ RESULT
                (MKROUND
                 (NORMBF
                  (|DIVIDE:|
                   (CONS '|:RD:|
                         (CONS
                          (TIMES (EXPT (MINUS 1) (PLUS N 1))
                                 (NEAREST-INT-TO-BF N2N))
                          0))
                   (CONS '|:RD:| (CONS D2N 0)) |:BPREC:|)))))
       (T
        (PROGN
         (OFF (LIST 'ROUNDED))
         (SETQ RESULT
                 (LIST '*SQ
                       (CONS
                        (TIMES (EXPT (MINUS 1) (PLUS N 1))
                               (NEAREST-INT-TO-BF N2N))
                        D2N)
                       T)))))
      (RETURN RESULT))) 
(PUT '|EULER:AUX| 'NUMBER-OF-ARGS 1) 
(PUT '|EULER:AUX| 'DEFINED-ON-LINE '308) 
(PUT '|EULER:AUX| 'DEFINED-IN-FILE 'SPECFN/SFBERN.RED) 
(PUT '|EULER:AUX| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |EULER:AUX| (N)
    (COND ((NOT (EVENP N)) 0)
          (T
           (PROG (NN LIST_OF_BINCOEFF NEWLIST OLD CURR EULERS SUM)
             (SETQ LIST_OF_BINCOEFF (LIST 1))
             (SETQ EULERS (LIST (MINUS 1) 1))
             (SETQ NN (MINUS 2))
             (PROG ()
              WHILELABEL
               (COND ((NOT (GREATERP N 0)) (RETURN NIL)))
               (PROGN
                (SETQ NN (PLUS NN 1))
                (SETQ OLD 0)
                (SETQ NEWLIST (LIST))
                (PROG ()
                 WHILELABEL
                  (COND
                   ((NOT (NOT (EQUAL LIST_OF_BINCOEFF (LIST)))) (RETURN NIL)))
                  (PROGN
                   (SETQ CURR (FIRST LIST_OF_BINCOEFF))
                   (SETQ NEWLIST (CONS (PLUS OLD CURR) NEWLIST))
                   (SETQ OLD CURR)
                   (SETQ LIST_OF_BINCOEFF (REST LIST_OF_BINCOEFF))
                   NIL)
                  (GO WHILELABEL))
                (SETQ LIST_OF_BINCOEFF (CONS 1 NEWLIST))
                (SETQ N (DIFFERENCE N 1))
                (COND
                 ((AND (GREATERP NN 0) (EVENP NN))
                  (PROGN
                   (SETQ CURR LIST_OF_BINCOEFF)
                   (SETQ OLD EULERS)
                   (SETQ SUM 0)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT OLD) (RETURN NIL)))
                     (PROGN
                      (SETQ CURR (CDDR CURR))
                      (SETQ SUM
                              (DIFFERENCE SUM
                                          (TIMES (FIRST OLD) (FIRST CURR))))
                      (SETQ OLD (CDR OLD))
                      NIL)
                     (GO WHILELABEL))
                   (SETQ EULERS (CONS SUM EULERS))
                   NIL)))
                NIL)
               (GO WHILELABEL))
             (RETURN (FIRST EULERS)))))) 
(ENDMODULE) 