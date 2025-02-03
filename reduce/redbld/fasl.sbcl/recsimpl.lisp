(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RECSIMPL)) 
(FLUID '(SPEC_NNNNN)) 
(FLAG '(SPEC_CHECK_N) 'BOOLEAN) 
(PUT 'TRIM 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIM) 'OPFN) 
(PUT 'TRIM 'DEFINED-ON-LINE '37) 
(PUT 'TRIM 'DEFINED-IN-FILE 'SPECFN/RECSIMPL.RED) 
(PUT 'TRIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIM (U)
    (COND ((EVALEQUAL (AEVAL U) (AEVAL (LIST 'LIST))) (AEVAL (LIST 'LIST)))
          ((MEMBER (REVALX (LIST 'FIRST U)) (REVALX (LIST 'REST U)))
           (AEVAL (LIST 'TRIM (LIST 'REST U))))
          (T (AEVAL (LIST 'CONS (LIST 'FIRST U) (LIST 'TRIM (LIST 'REST U))))))) 
(PUT 'ADELETE 'NUMBER-OF-ARGS 2) 
(FLAG '(ADELETE) 'OPFN) 
(PUT 'ADELETE 'DEFINED-ON-LINE '43) 
(PUT 'ADELETE 'DEFINED-IN-FILE 'SPECFN/RECSIMPL.RED) 
(PUT 'ADELETE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADELETE (V U)
    (COND ((EVALEQUAL (AEVAL U) (AEVAL (LIST 'LIST))) (AEVAL (LIST 'LIST)))
          ((EVALEQUAL (AEVAL V) (AEVAL (LIST 'FIRST U)))
           (AEVAL (LIST 'ADELETE V (LIST 'REST U))))
          (T
           (AEVAL
            (LIST 'CONS (LIST 'FIRST U) (LIST 'ADELETE V (LIST 'REST U))))))) 
(PUT 'RECURSIONSIMPLIFY 'NUMBER-OF-ARGS 1) 
(FLAG '(RECURSIONSIMPLIFY) 'OPFN) 
(PUT 'RECURSIONSIMPLIFY 'DEFINED-ON-LINE '49) 
(PUT 'RECURSIONSIMPLIFY 'DEFINED-IN-FILE 'SPECFN/RECSIMPL.RED) 
(PUT 'RECURSIONSIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RECURSIONSIMPLIFY (EX)
    (PROG (EQQ L1 L2 L3 L4 L5 F NARGS N A X KERN)
      (SETQ EQQ (AEVAL EX))
      (SETQ KERN
              (UNION (KERNELS (*Q2F (CONS (CAR (SIMP EQQ)) 1)))
                     (KERNELS (*Q2F (CONS (CDR (SIMP EQQ)) 1)))))
      (SETQ L1
              (AEVAL
               (CONS 'LIST
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K KERN)
                      STARTOVER
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (K) (COND ((ATOM K) NIL) (T (LIST K))))
                                (CAR K)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ K (CDR K))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (K) (COND ((ATOM K) NIL) (T (LIST K))))
                                (CAR K)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ K (CDR K))
                       (GO LOOPLABEL)))))
      (SETQ L2
              (AEVAL
               (CONS 'LIST
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K KERN)
                      STARTOVER
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (K)
                                  (COND ((ATOM K) NIL)
                                        (T
                                         (LIST (CAR K)
                                               (PLUS (MINUS 1) (LENGTH K))))))
                                (CAR K)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ K (CDR K))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (K)
                                  (COND ((ATOM K) NIL)
                                        (T
                                         (LIST (CAR K)
                                               (PLUS (MINUS 1) (LENGTH K))))))
                                (CAR K)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ K (CDR K))
                       (GO LOOPLABEL)))))
      (WHILE (NOT (EVALEQUAL (AEVAL* L2) (AEVAL* (LIST 'LIST))))
             (PROGN
              (SETQ F (AEVAL* (LIST 'FIRST L2)))
              (SETQ L2 (AEVAL* (LIST 'REST L2)))
              (SETQ NARGS (AEVAL* (LIST 'FIRST L2)))
              (SETQ L2 (AEVAL* (LIST 'REST L2)))
              (SETQ L3
                      (PROG (KK FORALL-RESULT FORALL-ENDPTR)
                        (SETQ KK (GETRLIST (AEVAL* L1)))
                       STARTOVER
                        (COND ((NULL KK) (RETURN (MAKELIST NIL))))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (KK)
                                   (COND
                                    ((AND
                                      (EVALEQUAL (AEVAL* (LIST 'PART KK 0))
                                                 (AEVAL* F))
                                      (BOOLVALUE*
                                       (REVALX
                                        (EQUAL
                                         (PLUS (MINUS 1)
                                               (LENGTH (PREPSQ (CADR KK))))
                                         NARGS))))
                                     (AEVAL* (LIST 'LIST KK)))
                                    (T (AEVAL* (LIST 'LIST)))))
                                 (CAR KK)))
                        (SETQ FORALL-ENDPTR
                                (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                        (SETQ KK (CDR KK))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL KK) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (GETRLIST
                                 ((LAMBDA (KK)
                                    (COND
                                     ((AND
                                       (EVALEQUAL (AEVAL* (LIST 'PART KK 0))
                                                  (AEVAL* F))
                                       (BOOLVALUE*
                                        (REVALX
                                         (EQUAL
                                          (PLUS (MINUS 1)
                                                (LENGTH (PREPSQ (CADR KK))))
                                          NARGS))))
                                      (AEVAL* (LIST 'LIST KK)))
                                     (T (AEVAL* (LIST 'LIST)))))
                                  (CAR KK))))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ KK (CDR KK))
                        (GO LOOPLABEL)))
              (SETQ L4
                      (PROG (KK FORALL-RESULT FORALL-ENDPTR)
                        (SETQ KK (GETRLIST (AEVAL* L3)))
                        (COND ((NULL KK) (RETURN (MAKELIST NIL))))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (KK)
                                            (AEVAL*
                                             (CONS 'LIST
                                                   (CDDR (PREPSQ (CADR KK))))))
                                          (CAR KK))
                                         NIL)))
                       LOOPLABEL
                        (SETQ KK (CDR KK))
                        (COND ((NULL KK) (RETURN (CONS 'LIST FORALL-RESULT))))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (KK)
                                    (AEVAL*
                                     (CONS 'LIST (CDDR (PREPSQ (CADR KK))))))
                                  (CAR KK))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ L4 (AEVAL* (LIST 'TRIM L4)))
              (PROG (KKK)
                (SETQ KKK (GETRLIST (AEVAL* L4)))
               LAB
                (COND ((NULL KKK) (RETURN NIL)))
                ((LAMBDA (KKK)
                   (PROGN
                    (SETQ L5
                            (PROG (KKKK FORALL-RESULT FORALL-ENDPTR)
                              (SETQ KKKK (GETRLIST (AEVAL* L3)))
                             STARTOVER
                              (COND ((NULL KKKK) (RETURN (MAKELIST NIL))))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (KKKK)
                                         (COND
                                          ((EVALEQUAL (AEVAL* KKK)
                                                      (AEVAL*
                                                       (CONS 'LIST
                                                             (CDDR
                                                              (PREPSQ
                                                               (CADR KKKK))))))
                                           (AEVAL*
                                            (LIST 'LIST
                                                  (CADR
                                                   (PREPSQ (CADR KKKK))))))
                                          (T (AEVAL* (LIST 'LIST)))))
                                       (CAR KKKK)))
                              (SETQ FORALL-ENDPTR
                                      (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                              (SETQ KKKK (CDR KKKK))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL KKKK) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (GETRLIST
                                       ((LAMBDA (KKKK)
                                          (COND
                                           ((EVALEQUAL (AEVAL* KKK)
                                                       (AEVAL*
                                                        (CONS 'LIST
                                                              (CDDR
                                                               (PREPSQ
                                                                (CADR
                                                                 KKKK))))))
                                            (AEVAL*
                                             (LIST 'LIST
                                                   (CADR
                                                    (PREPSQ (CADR KKKK))))))
                                           (T (AEVAL* (LIST 'LIST)))))
                                        (CAR KKKK))))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ KKKK (CDR KKKK))
                              (GO LOOPLABEL)))
                    (WHILE (EVALGREATERP (AEVAL* (LIST 'LENGTH L5)) 2)
                           (PROGN
                            (SETQ N (AEVAL* (LIST 'MAX L5)))
                            (COND
                             ((AND
                               (MEMBER (REVALX (LIST 'DIFFERENCE N 1))
                                       (REVALX L5))
                               (MEMBER (REVALX (LIST 'DIFFERENCE N 2))
                                       (REVALX L5)))
                              (PROGN
                               (SETK 'SPEC_NNNNN (AEVAL* N))
                               (SETQ EQQ
                                       (AEVAL*
                                        (LIST 'WHEREEXP
                                              (LIST 'LIST 'SPEC_RECRULES)
                                              EQQ)))
                               (SETK 'SPEC_NNNNN (AEVAL* 'NIL)))))
                            (SETQ L5 (AEVAL* (LIST 'ADELETE N L5)))
                            (AEVAL* 'NIL)))
                    (AEVAL* 'NIL)))
                 (CAR KKK))
                (SETQ KKK (CDR KKK))
                (GO LAB))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL EQQ)))) 
(PUT 'SPEC_CHECK_N 'NUMBER-OF-ARGS 1) 
(FLAG '(SPEC_CHECK_N) 'OPFN) 
(PUT 'SPEC_CHECK_N 'DEFINED-ON-LINE '90) 
(PUT 'SPEC_CHECK_N 'DEFINED-IN-FILE 'SPECFN/RECSIMPL.RED) 
(PUT 'SPEC_CHECK_N 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPEC_CHECK_N (N)
    (COND ((EVALEQUAL (AEVAL N) (AEVAL 'SPEC_NNNNN)) (AEVAL 'T))
          (T (AEVAL 'NIL)))) 
(SETK 'SPEC_RECRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'BESSELJ (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'BESSELJ (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'BESSELY (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'BESSELY (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'BESSELI (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'BESSELI (LIST 'DIFFERENCE 'N 2) 'Z)
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'BESSELI (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS (LIST 'BESSELK (LIST 'DIFFERENCE 'N 2) 'Z)
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'BESSELK (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'HANKEL1 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'HANKEL1 (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'HANKEL1 (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'HANKEL2 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'HANKEL2 (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES
                                     (LIST 'TIMES 2
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))
                                     (LIST 'HANKEL2 (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERM (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE 'A 1))
                               (LIST 'PLUS
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'DIFFERENCE 'B 'A)
                                                 1)
                                           (LIST 'KUMMERM
                                                 (LIST 'DIFFERENCE 'A 2) 'B
                                                 'Z))
                                     (LIST 'TIMES
                                           (LIST 'PLUS
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES 2 'A)
                                                             2)
                                                       'B)
                                                 'Z)
                                           (LIST 'KUMMERM
                                                 (LIST 'DIFFERENCE 'A 1) 'B
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'A)))
             (LIST 'REPLACEBY
                   (LIST 'KUMMERU (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'A 1)
                                                 (LIST 'DIFFERENCE 'A 'B)))
                                     (LIST 'PLUS
                                           (LIST 'KUMMERU
                                                 (LIST 'DIFFERENCE 'A 2) 'B 'Z)
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE 'B
                                                             (LIST 'TIMES 2
                                                                   'A))
                                                       (LIST 'DIFFERENCE 2 'Z))
                                                 (LIST 'KUMMERU
                                                       (LIST 'DIFFERENCE 'A 1)
                                                       'B 'Z)))))
                         (LIST 'SPEC_CHECK_N 'A)))
             (LIST 'REPLACEBY
                   (LIST 'WHITTAKERM (LIST '~ 'N) (LIST '~ 'M) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'PLUS (LIST 'TIMES 2 'M)
                                           (LIST 'DIFFERENCE (LIST 'TIMES 2 'N)
                                                 1)))
                               (LIST 'PLUS
                                     (LIST 'TIMES
                                           (LIST 'PLUS 3
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 'M)
                                                       (LIST 'TIMES 2 'N)))
                                           (LIST 'WHITTAKERM
                                                 (LIST 'DIFFERENCE 'N 2) 'M
                                                 'Z))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 4 'N) 4)
                                                 (LIST 'TIMES 2 'Z))
                                           (LIST 'WHITTAKERM
                                                 (LIST 'DIFFERENCE 'N 1) 'M
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY
                   (LIST 'WHITTAKERW (LIST '~ 'N) (LIST '~ 'M) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'PLUS (MINUS 9)
                                                 (LIST 'TIMES 4
                                                       (LIST 'EXPT 'M 2))
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 12 'N)
                                                       (LIST 'TIMES 4
                                                             (LIST 'EXPT 'N
                                                                   2))))
                                           (LIST 'WHITTAKERW
                                                 (LIST 'DIFFERENCE 'N 2) 'M
                                                 'Z))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 8 'N) 8)
                                                 (LIST 'TIMES 4 'Z))
                                           (LIST 'WHITTAKERW
                                                 (LIST 'DIFFERENCE 'N 1) 'M
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY
                   (LIST 'LEGENDREP (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE 'A 1)
                                                       'B)
                                                 (LIST 'LEGENDREP
                                                       (LIST 'DIFFERENCE 'A 2)
                                                       'B 'Z)))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE (LIST 'TIMES 2 'A)
                                                 1)
                                           'Z
                                           (LIST 'LEGENDREP
                                                 (LIST 'DIFFERENCE 'A 1) 'B
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'A)))
             (LIST 'REPLACEBY
                   (LIST 'LEGENDREQ (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE 'A 1)
                                                       'B)
                                                 (LIST 'LEGENDREQ
                                                       (LIST 'DIFFERENCE 'A 2)
                                                       'B 'Z)))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE (LIST 'TIMES 2 'A)
                                                 1)
                                           'Z
                                           (LIST 'LEGENDREQ
                                                 (LIST 'DIFFERENCE 'A 1) 'B
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'A)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIP (LIST '~ 'N) (LIST '~ 'A) (LIST '~ 'B)
                         (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES 2 'N (LIST 'PLUS 'A 'B 'N)
                                           (LIST 'PLUS (MINUS 2) 'A 'B
                                                 (LIST 'TIMES 2 'N))))
                               (LIST 'PLUS
                                     (LIST 'TIMES 2
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE 1 'A) 'N)
                                           (LIST 'PLUS (MINUS 1) 'B 'N)
                                           (LIST 'PLUS 'A 'B
                                                 (LIST 'TIMES 2 'N))
                                           (LIST 'JACOBIP
                                                 (LIST 'DIFFERENCE 'N 2) 'A 'B
                                                 'Z))
                                     (LIST 'TIMES
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'A 2)
                                                             (LIST 'EXPT 'B 2))
                                                       (LIST 'PLUS (MINUS 1) 'A
                                                             'B
                                                             (LIST 'TIMES 2
                                                                   'N)))
                                                 (LIST 'TIMES
                                                       (LIST 'PLUS (MINUS 2) 'A
                                                             'B
                                                             (LIST 'TIMES 2
                                                                   'N))
                                                       (LIST 'PLUS (MINUS 1) 'A
                                                             'B
                                                             (LIST 'TIMES 2
                                                                   'N))
                                                       (LIST 'PLUS 'A 'B
                                                             (LIST 'TIMES 2
                                                                   'N))
                                                       'Z))
                                           (LIST 'JACOBIP
                                                 (LIST 'DIFFERENCE 'N 1) 'A 'B
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY
                   (LIST 'GEGENBAUERP (LIST '~ 'N) (LIST '~ 'A) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'PLUS 'N
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES 2 'A)
                                                             2))
                                                 (LIST 'GEGENBAUERP
                                                       (LIST 'DIFFERENCE 'N 2)
                                                       'A 'Z)))
                                     (LIST 'TIMES 2
                                           (LIST 'PLUS (LIST 'DIFFERENCE 'N 1)
                                                 'A)
                                           'Z
                                           (LIST 'GEGENBAUERP
                                                 (LIST 'DIFFERENCE 'N 1) 'A
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'CHEBYSHEVT (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'CHEBYSHEVT (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES 2 'Z
                                     (LIST 'CHEBYSHEVT (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'CHEBYSHEVU (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'CHEBYSHEVU (LIST 'DIFFERENCE 'N 2)
                                           'Z))
                               (LIST 'TIMES 2 'Z
                                     (LIST 'CHEBYSHEVU (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'LEGENDREP
                                                       (LIST 'DIFFERENCE 'N 2)
                                                       'Z)))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE (LIST 'TIMES 2 'N)
                                                 1)
                                           'Z
                                           (LIST 'LEGENDREP
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY
                   (LIST 'LAGUERREP (LIST '~ 'N) (LIST '~ 'A) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       'A)
                                                 (LIST 'LAGUERREP
                                                       (LIST 'DIFFERENCE 'N 2)
                                                       'A 'Z)))
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE 'A 1)
                                                       'Z))
                                           (LIST 'LAGUERREP
                                                 (LIST 'DIFFERENCE 'N 1) 'A
                                                 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'LAGUERREP (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'LAGUERREP
                                                       (LIST 'DIFFERENCE 'N 2)
                                                       'Z)))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 'N) 1)
                                                 'Z)
                                           (LIST 'LAGUERREP
                                                 (LIST 'DIFFERENCE 'N 1) 'Z))))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'HERMITEP (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'TIMES 2 (LIST 'DIFFERENCE 'N 1)
                                           (LIST 'HERMITEP
                                                 (LIST 'DIFFERENCE 'N 2) 'Z)))
                               (LIST 'TIMES 2 'Z
                                     (LIST 'HERMITEP (LIST 'DIFFERENCE 'N 1)
                                           'Z)))
                         (LIST 'SPEC_CHECK_N 'N)))
             (LIST 'REPLACEBY (LIST 'STRUVEH (LIST '~ 'NNNNN) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST 'EXPT 'X 2)
                                           (LIST 'STRUVEH
                                                 (LIST 'PLUS (MINUS 3) 'NNNNN)
                                                 'X))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 5 'X
                                                 (LIST 'STRUVEH
                                                       (LIST 'PLUS (MINUS 2)
                                                             'NNNNN)
                                                       'X))
                                           (LIST 'TIMES 4 'NNNNN 'X
                                                 (LIST 'STRUVEH
                                                       (LIST 'PLUS (MINUS 2)
                                                             'NNNNN)
                                                       'X)))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2
                                                 (LIST 'STRUVEH
                                                       (LIST 'PLUS (MINUS 1)
                                                             'NNNNN)
                                                       'X))
                                           (LIST 'TIMES 6 'NNNNN
                                                 (LIST 'STRUVEH
                                                       (LIST 'PLUS (MINUS 1)
                                                             'NNNNN)
                                                       'X)))
                                     (LIST 'TIMES 4 (LIST 'EXPT 'NNNNN 2)
                                           (LIST 'STRUVEH
                                                 (LIST 'PLUS (MINUS 1) 'NNNNN)
                                                 'X))
                                     (LIST 'TIMES (LIST 'EXPT 'X 2)
                                           (LIST 'STRUVEH
                                                 (LIST 'PLUS (MINUS 1) 'NNNNN)
                                                 'X)))
                               (LIST 'PLUS (LIST 'MINUS 'X)
                                     (LIST 'TIMES 2 'NNNNN 'X)))
                         (LIST 'SPEC_CHECK_N 'NNNNN)))
             (LIST 'REPLACEBY (LIST 'STRUVEL (LIST '~ 'NNNNN) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'X
                                                 (LIST 'STRUVEL
                                                       (LIST 'PLUS (MINUS 3)
                                                             'NNNNN)
                                                       'X)))
                                     (LIST 'TIMES
                                           (LIST 'PLUS (MINUS 1)
                                                 (LIST 'TIMES 4
                                                       (LIST 'PLUS (MINUS 1)
                                                             'NNNNN)))
                                           (LIST 'STRUVEL
                                                 (LIST 'PLUS (MINUS 2) 'NNNNN)
                                                 'X))
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'MINUS
                                                                   (LIST 'TIMES
                                                                         2
                                                                         (LIST
                                                                          'PLUS
                                                                          (MINUS
                                                                           1)
                                                                          'NNNNN)))
                                                             (LIST 'TIMES 4
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (MINUS
                                                                           1)
                                                                          'NNNNN)
                                                                         2)))
                                                       (LIST 'EXPT 'X 2))
                                                 (LIST 'STRUVEL
                                                       (LIST 'PLUS (MINUS 1)
                                                             'NNNNN)
                                                       'X))
                                           'X))
                               (LIST 'PLUS 1
                                     (LIST 'TIMES 2
                                           (LIST 'PLUS (MINUS 1) 'NNNNN))))
                         (LIST 'SPEC_CHECK_N 'NNNNN)))))) 
(ENDMODULE) 