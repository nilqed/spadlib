(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFNUMERIC)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'AGM_BASIC 'NUMBER-OF-ARGS 2) 
(FLAG '(AGM_BASIC) 'OPFN) 
(PUT 'AGM_BASIC 'DEFINED-ON-LINE '37) 
(PUT 'AGM_BASIC 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'AGM_BASIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AGM_BASIC (A0 B0)
    (PROG (AN BN CN TOL)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (REPEAT
       (PROGN
        (SETQ AN (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS A0 B0) 2)))
        (SETQ BN (AEVAL* (LIST 'SQRT (LIST 'TIMES A0 B0))))
        (SETQ CN (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE A0 B0) 2)))
        (SETQ A0 (AEVAL* AN))
        (SETQ B0 (AEVAL* BN)))
       (EVALLESSP (AEVAL* CN) (AEVAL* TOL)))
      (RETURN (AEVAL AN)))) 
(PUT 'AGM_FUNCTION 'NUMBER-OF-ARGS 3) 
(FLAG '(AGM_FUNCTION) 'OPFN) 
(PUT 'AGM_FUNCTION 'DEFINED-ON-LINE '53) 
(PUT 'AGM_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'AGM_FUNCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE AGM_FUNCTION (A0 B0 C0)
    (PROG (N AN BN CN ALIST CLIST TOL)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ CN (AEVAL 20))
      (SETQ ALIST (AEVAL (LIST 'LIST A0)))
      (SETQ CLIST (AEVAL (LIST 'LIST C0)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS CN)) (AEVAL* TOL))
             (PROGN
              (SETQ AN (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS A0 B0) 2)))
              (SETQ BN (AEVAL* (LIST 'SQRT (LIST 'TIMES A0 B0))))
              (SETQ CN (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE A0 B0) 2)))
              (SETQ ALIST (AEVAL* (LIST 'CONS AN ALIST)))
              (SETQ CLIST (AEVAL* (LIST 'CONS CN CLIST)))
              (SETQ A0 (AEVAL* AN))
              (SETQ B0 (AEVAL* BN))))
      (SETQ N (AEVAL (LIST 'DIFFERENCE (LIST 'LENGTH ALIST) 1)))
      (RETURN (AEVAL (LIST 'LIST N ALIST CLIST))))) 
(PUT 'PHI_FUNCTION 'NUMBER-OF-ARGS 4) 
(FLAG '(PHI_FUNCTION) 'OPFN) 
(PUT 'PHI_FUNCTION 'DEFINED-ON-LINE '91) 
(PUT 'PHI_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'PHI_FUNCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PHI_FUNCTION (A0 B0 C0 U)
    (PROG (AGM ALIST CLIST N AN CN PHI_N PHI_LIST)
      (SETQ AGM (AEVAL (LIST 'AGM_FUNCTION A0 B0 C0)))
      (SETQ ALIST (AEVAL (LIST 'SECOND AGM)))
      (SETQ CLIST (AEVAL (LIST 'THIRD AGM)))
      (SETQ N (AEVAL (LIST 'FIRST AGM)))
      (SETQ AN (AEVAL (LIST 'FIRST ALIST)))
      (SETQ PHI_N (AEVAL (LIST 'TIMES (LIST 'EXPT 2 N) AN U)))
      (SETQ PHI_LIST (AEVAL (LIST 'LIST PHI_N)))
      (WHILE (EVALNEQ (AEVAL* (LIST 'REST ALIST)) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ AN (AEVAL* (LIST 'FIRST ALIST)))
              (SETQ CN (AEVAL* (LIST 'FIRST CLIST)))
              (SETQ PHI_N
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS
                                   (LIST 'ASIN
                                         (LIST 'TIMES (LIST 'QUOTIENT CN AN)
                                               (LIST 'SIN PHI_N)))
                                   PHI_N)
                             2)))
              (SETQ PHI_LIST (AEVAL* (LIST 'CONS PHI_N PHI_LIST)))
              (SETQ ALIST (AEVAL* (LIST 'REST ALIST)))
              (SETQ CLIST (AEVAL* (LIST 'REST CLIST)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL PHI_LIST)))) 
(PUT 'F_FUNCTION 'NUMBER-OF-ARGS 2) 
(FLAG '(F_FUNCTION) 'OPFN) 
(PUT 'F_FUNCTION 'DEFINED-ON-LINE '121) 
(PUT 'F_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'F_FUNCTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE F_FUNCTION (PHI M)
    (COND
     ((EVALEQUAL (AEVAL PHI) (AEVAL (LIST 'QUOTIENT 'PI 2)))
      (AEVAL (LIST 'NUM_ELLK M)))
     (T
      (PROG (BOTHLISTS ALIST PLIST PHI_N)
        (SETQ BOTHLISTS (AEVAL (LIST 'LANDENTRANS PHI (LIST 'ASIN M))))
        (SETQ ALIST (AEVAL (LIST 'REST (LIST 'SECOND BOTHLISTS))))
        (SETQ PLIST (AEVAL (LIST 'FIRST BOTHLISTS)))
        (SETQ PHI_N (AEVAL (LIST 'FIRST (LIST 'REVERSE PLIST))))
        (RETURN
         (AEVAL
          (LIST 'TIMES PHI_N
                (PROG (Y FORALL-RESULT)
                  (SETQ Y (GETRLIST (AEVAL ALIST)))
                  (SETQ FORALL-RESULT 1)
                 LAB1
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (AEVAL*
                           (LIST 'TIMES
                                 ((LAMBDA (Y)
                                    (AEVAL
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS 1 (LIST 'SIN Y)) 2)))
                                  (CAR Y))
                                 FORALL-RESULT)))
                  (SETQ Y (CDR Y))
                  (GO LAB1))))))))) 
(PUT 'K_FUNCTION 'NUMBER-OF-ARGS 1) 
(FLAG '(K_FUNCTION) 'OPFN) 
(PUT 'K_FUNCTION 'DEFINED-ON-LINE '133) 
(PUT 'K_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'K_FUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE K_FUNCTION (M)
    (PROG (AGM AN)
      (SETQ AGM
              (AEVAL
               (LIST 'AGM_FUNCTION 1
                     (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT M 2))) M)))
      (SETQ AN (AEVAL (LIST 'FIRST (LIST 'SECOND AGM))))
      (RETURN (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 AN)))))) 
(PUT 'E_FUNCTION 'NUMBER-OF-ARGS 2) 
(FLAG '(E_FUNCTION) 'OPFN) 
(PUT 'E_FUNCTION 'DEFINED-ON-LINE '144) 
(PUT 'E_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'E_FUNCTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE E_FUNCTION (PHI M)
    (PROG (F N BOTHLISTS ALIST PLIST S SINALIST SINPLIST B BLIST ALLZ Z ALLX X)
      (SETQ F (AEVAL (LIST 'F_FUNCTION PHI M)))
      (SETQ BOTHLISTS (AEVAL (LIST 'LANDENTRANS PHI (LIST 'ASIN M))))
      (SETQ ALIST (AEVAL (LIST 'SECOND BOTHLISTS)))
      (SETQ PLIST (AEVAL (LIST 'FIRST BOTHLISTS)))
      (SETQ N (AEVAL (LIST 'DIFFERENCE (LIST 'LENGTH ALIST) 1)))
      (SETQ SINALIST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (GETRLIST (AEVAL (LIST 'REST ALIST))))
                (COND ((NULL A) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A) (AEVAL (LIST 'SIN A))) (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (AEVAL (LIST 'SIN A))) (CAR A))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SINPLIST
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (GETRLIST (AEVAL (LIST 'REST PLIST))))
                (COND ((NULL P) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (AEVAL (LIST 'SIN P))) (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (AEVAL (LIST 'SIN P))) (CAR P))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ B (AEVAL (LIST 'FIRST SINALIST)))
      (SETQ BLIST
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C (GETRLIST (AEVAL (LIST 'REST SINALIST))))
                (COND ((NULL C) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C)
                                    (SETQ B (AEVAL (LIST 'TIMES B C))))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (C) (SETQ B (AEVAL (LIST 'TIMES B C))))
                          (CAR C))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BLIST (AEVAL (LIST 'CONS (LIST 'FIRST SINALIST) BLIST)))
      (SETQ ALLZ (AEVAL 0))
      (SETQ ALLX (AEVAL 0))
      (PROG (W)
        (SETQ W 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) W)) (RETURN NIL)))
        (PROGN
         (SETQ Z
                 (AEVAL*
                  (LIST 'QUOTIENT (LIST 'FIRST BLIST) (LIST 'EXPT 2 W))))
         (SETQ X
                 (AEVAL*
                  (LIST 'TIMES (LIST 'SQRT (LIST 'FIRST BLIST))
                        (LIST 'QUOTIENT (LIST 'FIRST SINPLIST)
                              (LIST 'EXPT 2 W)))))
         (SETQ ALLZ (AEVAL* (LIST 'PLUS ALLZ Z)))
         (SETQ ALLX (AEVAL* (LIST 'PLUS ALLX X)))
         (SETQ BLIST (AEVAL* (LIST 'REST BLIST)))
         (SETQ SINPLIST (AEVAL* (LIST 'REST SINPLIST)))
         (AEVAL* 'NIL))
        (SETQ W
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 W))
        (GO LAB))
      (SETQ S (AEVAL (LIST 'SIN (LIST 'FIRST ALIST))))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'TIMES F
                    (LIST 'DIFFERENCE 1
                          (LIST 'TIMES (LIST 'EXPT S 2)
                                (LIST 'QUOTIENT (LIST 'PLUS 1 ALLZ) 2))))
              (LIST 'TIMES S ALLX)))))) 
(PUT 'JE_FUNCTION 'NUMBER-OF-ARGS 2) 
(FLAG '(JE_FUNCTION) 'OPFN) 
(PUT 'JE_FUNCTION 'DEFINED-ON-LINE '179) 
(PUT 'JE_FUNCTION 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'JE_FUNCTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JE_FUNCTION (PHI M) (LIST 'E_FUNCTION (LIST 'NUM_JACOBIAM PHI M) M)) 
(PUT 'N_ELLIPTIC 'NUMBER-OF-ARGS 1) 
(PUT 'N_ELLIPTIC 'DEFINED-ON-LINE '188) 
(PUT 'N_ELLIPTIC 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'N_ELLIPTIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE N_ELLIPTIC (U)
    (COND ((LESSP (LENGTH U) 2) (REDERR "illegal call to n_elliptic"))
          (T
           (PROG (OLDPREC RES)
             (SETQ OLDPREC (PRECISION 0))
             (PRECISION (MAX (PLUS OLDPREC 4) 16))
             (SETQ RES (REVAL1 U NIL))
             (PRECISION OLDPREC)
             (RETURN RES))))) 
(PUT 'NUM_ELLIPTIC 'PSOPFN 'N_ELLIPTIC) 
(PUT 'LANDENTRANS 'NUMBER-OF-ARGS 2) 
(FLAG '(LANDENTRANS) 'OPFN) 
(PUT 'LANDENTRANS 'DEFINED-ON-LINE '207) 
(PUT 'LANDENTRANS 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'LANDENTRANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LANDENTRANS (PHI ALPHA)
    (PROG (ALPHA1 PHI1 ALIST PLIST TOL)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ ALIST (AEVAL (LIST 'LIST ALPHA)))
      (SETQ PLIST (AEVAL (LIST 'LIST PHI)))
      (WHILE (EVALGREATERP (AEVAL* ALPHA) (AEVAL* TOL))
             (PROGN
              (SETQ ALPHA1
                      (AEVAL*
                       (LIST 'ASIN
                             (LIST 'DIFFERENCE
                                   (LIST 'QUOTIENT 2
                                         (LIST 'PLUS 1 (LIST 'COS ALPHA)))
                                   1))))
              (SETQ PHI1
                      (AEVAL*
                       (LIST 'PLUS PHI
                             (LIST 'ATAN
                                   (LIST 'TIMES (LIST 'COS ALPHA)
                                         (LIST 'TAN PHI)))
                             (LIST 'TIMES
                                   (LIST 'FLOOR
                                         (LIST 'QUOTIENT
                                               (LIST 'PLUS
                                                     (LIST 'FLOOR
                                                           (LIST 'QUOTIENT PHI
                                                                 (LIST
                                                                  'QUOTIENT 'PI
                                                                  2)))
                                                     1)
                                               2))
                                   'PI))))
              (SETQ ALIST (AEVAL* (LIST 'CONS ALPHA1 ALIST)))
              (SETQ PLIST (AEVAL* (LIST 'CONS PHI1 PLIST)))
              (SETQ ALPHA (AEVAL* ALPHA1))
              (SETQ PHI (AEVAL* PHI1))
              (AEVAL* 'NIL)))
      (RETURN
       (AEVAL (LIST 'LIST (LIST 'REVERSE PLIST) (LIST 'REVERSE ALIST)))))) 
(PUT 'RC 'NUMBER-OF-ARGS 2) 
(FLAG '(RC) 'OPFN) 
(PUT 'RC 'DEFINED-ON-LINE '280) 
(PUT 'RC 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RC (X Y)
    (PROG (TMP MU SN W OLDPREC TOL)
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0) (EVALLESSP (AEVAL X) 0))
        (AEVAL (REDERR (REVALX "RC: first parameter must be non-negative")))))
      (COND
       ((EVALEQUAL (AEVAL Y) 0)
        (AEVAL (REDERR (REVALX "RC: second parameter must be non-zero")))))
      (SETQ W (AEVAL 1))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0) (EVALLESSP (AEVAL Y) 0))
        (PROGN
         (SETQ TMP (AEVAL (LIST 'DIFFERENCE X Y)))
         (SETQ Y (AEVAL (LIST 'MINUS Y)))
         (SETQ W (AEVAL (LIST 'QUOTIENT (LIST 'SQRT X) (LIST 'SQRT TMP))))
         (SETQ X (AEVAL TMP))
         (AEVAL 'NIL))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ TMP (AEVAL (LIST 'CEILING (LIST 'QUOTIENT OLDPREC 3))))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS TMP))))
      (SETQ TMP (AEVAL 'T))
      (WHILE (BOOLVALUE* TMP)
             (PROGN
              (SETQ MU
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'PLUS X (LIST 'TIMES 2 Y)) 3)))
              (SETQ SN
                      (AEVAL*
                       (LIST 'DIFFERENCE (LIST 'QUOTIENT (LIST 'PLUS Y MU) MU)
                             2)))
              (COND
               ((EVALLESSP (AEVAL* (LIST 'ABS SN)) (AEVAL* TOL))
                (SETQ TMP (AEVAL* 'NIL)))
               (T
                (PROGN
                 (SETK 'LAMDA
                       (AEVAL*
                        (LIST 'PLUS
                              (LIST 'TIMES 2 (LIST 'SQRT X) (LIST 'SQRT Y))
                              Y)))
                 (SETQ X (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS X 'LAMDA) 4)))
                 (SETQ Y (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Y 'LAMDA) 4)))
                 (AEVAL* 'NIL))))
              (AEVAL* 'NIL)))
      (SETQ TMP
              (AEVAL
               (LIST 'TIMES SN SN
                     (LIST 'PLUS (LIST 'QUOTIENT 3 10)
                           (LIST 'TIMES SN
                                 (LIST 'PLUS (LIST 'QUOTIENT 1 7)
                                       (LIST 'TIMES SN
                                             (LIST 'PLUS (LIST 'QUOTIENT 3 8)
                                                   (LIST 'TIMES 9
                                                         (LIST 'QUOTIENT SN
                                                               22))))))))))
      (SETQ TMP
              (AEVAL
               (LIST 'TIMES W
                     (LIST 'QUOTIENT (LIST 'PLUS 1 TMP) (LIST 'SQRT MU)))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'RF 'NUMBER-OF-ARGS 3) 
(FLAG '(RF) 'OPFN) 
(PUT 'RF 'DEFINED-ON-LINE '317) 
(PUT 'RF 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RF (X Y Z)
    (PROG (DX DY DZ XR YR ZR MU LAMDA E2 E3 TOL TMP OLDPREC N)
      (SETQ N (AEVAL 0))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN X)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative first argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Y)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative second argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Z)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative third argument")))))
         (COND ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALGREATERP (AEVAL N) 1)
        (AEVAL
         (REDERR
          (REVALX "divergent integral RF: more than one argument is zero")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ N (AEVAL (LIST 'CEILING (LIST 'QUOTIENT OLDPREC 3))))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS N))))
      (SETQ TMP (AEVAL 'T))
      (WHILE (BOOLVALUE* TMP)
             (PROGN
              (SETQ MU (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS X Y Z) 3)))
              (SETQ DX
                      (AEVAL*
                       (LIST 'DIFFERENCE 2
                             (LIST 'QUOTIENT (LIST 'PLUS MU X) MU))))
              (SETQ DY
                      (AEVAL*
                       (LIST 'DIFFERENCE 2
                             (LIST 'QUOTIENT (LIST 'PLUS MU Y) MU))))
              (SETQ DZ
                      (AEVAL*
                       (LIST 'DIFFERENCE 2
                             (LIST 'QUOTIENT (LIST 'PLUS MU Z) MU))))
              (COND
               ((EVALLESSP
                 (AEVAL*
                  (LIST 'MAX (LIST 'ABS DX) (LIST 'ABS DY) (LIST 'ABS DZ)))
                 (AEVAL* TOL))
                (SETQ TMP (AEVAL* 'NIL)))
               (T
                (PROGN
                 (SETQ XR (AEVAL* (LIST 'SQRT X)))
                 (SETQ YR (AEVAL* (LIST 'SQRT Y)))
                 (SETQ ZR (AEVAL* (LIST 'SQRT Z)))
                 (SETQ LAMDA
                         (AEVAL*
                          (LIST 'PLUS (LIST 'TIMES XR (LIST 'PLUS YR ZR))
                                (LIST 'TIMES YR ZR))))
                 (SETQ X (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS X LAMDA) 4)))
                 (SETQ Y (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Y LAMDA) 4)))
                 (SETQ Z (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Z LAMDA) 4)))
                 (AEVAL* 'NIL))))
              (AEVAL* 'NIL)))
      (SETQ E2
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES DX DY) (LIST 'TIMES DZ DZ))))
      (SETQ E3 (AEVAL (LIST 'TIMES DX DY DZ)))
      (SETQ TMP
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'PLUS 1
                           (LIST 'TIMES E2
                                 (LIST 'DIFFERENCE
                                       (LIST 'DIFFERENCE (LIST 'QUOTIENT E2 24)
                                             (LIST 'TIMES 3
                                                   (LIST 'QUOTIENT E3 44)))
                                       (LIST 'QUOTIENT 1 10)))
                           (LIST 'QUOTIENT E3 14))
                     (LIST 'SQRT MU))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'RD 'NUMBER-OF-ARGS 3) 
(FLAG '(RD) 'OPFN) 
(PUT 'RD 'DEFINED-ON-LINE '366) 
(PUT 'RD 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RD (X Y Z)
    (PROG (DX DY DZ XR YR ZR MU LAMDA SIGMA E1 E2 E3 E4 TOL TMP OLDPREC POW)
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0) (EVALLESSP (AEVAL X) 0))
        (AEVAL
         (REDERR
          (REVALX "divergent integral RD: first argument is negative"))))
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0) (EVALLESSP (AEVAL Y) 0))
        (AEVAL
         (REDERR
          (REVALX "divergent integral RD: second argument is negative"))))
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'PLUS X Y))) 0)
             (EVALLEQ (AEVAL (LIST 'PLUS X Y)) 0))
        (AEVAL
         (REDERR
          (REVALX "divergent integral RD: sum of first two aruments <= 0"))))
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0) (EVALLEQ (AEVAL Z) 0))
        (AEVAL
         (REDERR (REVALX "divergent integral RD: third argument <= 0")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ TMP (AEVAL (LIST 'CEILING (LIST 'QUOTIENT OLDPREC 3))))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS TMP))))
      (SETQ TMP (AEVAL 'T))
      (SETQ SIGMA (AEVAL 0))
      (SETQ POW (AEVAL 1))
      (WHILE (BOOLVALUE* TMP)
             (PROGN
              (SETQ MU
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'PLUS X Y (LIST 'TIMES 3 Z)) 5)))
              (SETQ DX (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU X) MU)))
              (SETQ DY (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU Y) MU)))
              (SETQ DZ (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU Z) MU)))
              (COND
               ((EVALLESSP
                 (AEVAL*
                  (LIST 'MAX (LIST 'ABS DX) (LIST 'ABS DY) (LIST 'ABS DZ)))
                 (AEVAL* TOL))
                (SETQ TMP (AEVAL* 'NIL)))
               (T
                (PROGN
                 (SETQ XR (AEVAL* (LIST 'SQRT X)))
                 (SETQ YR (AEVAL* (LIST 'SQRT Y)))
                 (SETQ ZR (AEVAL* (LIST 'SQRT Z)))
                 (SETQ LAMDA
                         (AEVAL*
                          (LIST 'PLUS (LIST 'TIMES XR (LIST 'PLUS YR ZR))
                                (LIST 'TIMES YR ZR))))
                 (SETQ SIGMA
                         (AEVAL*
                          (LIST 'PLUS SIGMA
                                (LIST 'QUOTIENT POW
                                      (LIST 'TIMES ZR (LIST 'PLUS Z LAMDA))))))
                 (SETQ X (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS X LAMDA) 4)))
                 (SETQ Y (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Y LAMDA) 4)))
                 (SETQ Z (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Z LAMDA) 4)))
                 (SETQ POW (AEVAL* (LIST 'QUOTIENT POW 4)))
                 (AEVAL* 'NIL))))
              (AEVAL* 'NIL)))
      (SETQ E1 (AEVAL (LIST 'TIMES DX DY)))
      (SETQ TMP (AEVAL (LIST 'TIMES DZ DZ)))
      (SETQ E2 (AEVAL (LIST 'DIFFERENCE E1 TMP)))
      (SETQ E3 (AEVAL (LIST 'DIFFERENCE E1 (LIST 'TIMES 6 TMP))))
      (SETQ E4 (AEVAL (LIST 'PLUS E3 (LIST 'TIMES 2 E2))))
      (SETQ TMP
              (AEVAL
               (LIST 'TIMES E3
                     (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 3 14))
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES (LIST 'QUOTIENT 9 88) E3)
                                 (LIST 'TIMES (LIST 'QUOTIENT 9 52) DZ E4))))))
      (SETQ E1
              (AEVAL
               (LIST 'TIMES DZ
                     (LIST 'PLUS (LIST 'TIMES (LIST 'QUOTIENT 1 6) E4)
                           (LIST 'TIMES DZ
                                 (LIST 'PLUS
                                       (LIST 'MINUS
                                             (LIST 'TIMES (LIST 'QUOTIENT 9 22)
                                                   E2))
                                       (LIST 'TIMES (LIST 'QUOTIENT 3 26) DZ
                                             E1)))))))
      (SETQ TMP
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES 3 SIGMA)
                     (LIST 'TIMES POW
                           (LIST 'QUOTIENT (LIST 'PLUS 1 TMP E1)
                                 (LIST 'TIMES MU (LIST 'SQRT MU)))))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'RJ 'NUMBER-OF-ARGS 4) 
(FLAG '(RJ) 'OPFN) 
(PUT 'RJ 'DEFINED-ON-LINE '410) 
(PUT 'RJ 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RJ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RJ (X Y Z P)
    (PROG (OLDPREC TOL DX DY DZ DP XR YR ZR POW TMP RCX OLDP A B TMP2 TMP3
           SIGMA ALFA BETA MU LAMDA N EA EB EC E1 E2)
      (SETQ N (AEVAL 0))
      (COND
       ((EVALEQUAL (AEVAL P) 0)
        (AEVAL
         (REDERR (REVALX "divergent integral RJ: fourth argument is zero")))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN X)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RJ: negative first argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Y)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RJ: negative second argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Z)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RJ: negative third argument")))))
         (COND ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALGREATERP (AEVAL N) 1)
        (AEVAL
         (REDERR
          (REVALX
           "divergent integral RJ: more than 1 of first 3 args are zero")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ N (AEVAL (LIST 'CEILING (LIST 'QUOTIENT OLDPREC 3))))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS N))))
      (SETQ OLDP (AEVAL P))
      (COND
       ((EVALLESSP (AEVAL P) 0)
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL Y) 0)
           (PROGN
            (SETQ TMP (AEVAL X))
            (SETQ X (AEVAL Y))
            (SETQ Y (AEVAL TMP))
            (AEVAL 'NIL))))
         (SETQ A (AEVAL (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE Y P))))
         (SETQ B
                 (AEVAL
                  (LIST 'TIMES A (LIST 'DIFFERENCE Z Y)
                        (LIST 'DIFFERENCE Y X))))
         (SETQ TMP (AEVAL (LIST 'PLUS Y B)))
         (SETK 'TMP1 (AEVAL (LIST 'TIMES X (LIST 'QUOTIENT Z Y))))
         (SETQ TMP2 (AEVAL (LIST 'TIMES P (LIST 'QUOTIENT TMP Y))))
         (SETQ P (AEVAL TMP))
         (AEVAL (LIST 'PRECISION OLDPREC))
         (SETQ RCX (AEVAL (LIST 'RC 'TMP1 TMP2)))
         (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
         (AEVAL 'NIL))))
      (SETQ TMP (AEVAL 'T))
      (SETQ POW (AEVAL 1))
      (SETQ SIGMA (AEVAL 0))
      (WHILE (BOOLVALUE* TMP)
             (PROGN
              (SETQ MU
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'PLUS X Y Z (LIST 'TIMES 2 P))
                             5)))
              (SETQ DX (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU X) MU)))
              (SETQ DY (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU Y) MU)))
              (SETQ DZ (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU Z) MU)))
              (SETQ DP (AEVAL* (LIST 'QUOTIENT (LIST 'DIFFERENCE MU P) MU)))
              (COND
               ((EVALLESSP
                 (AEVAL*
                  (LIST 'MAX (LIST 'ABS DX) (LIST 'ABS DY) (LIST 'ABS DZ)
                        (LIST 'ABS DP)))
                 (AEVAL* TOL))
                (SETQ TMP (AEVAL* 'NIL)))
               (T
                (PROGN
                 (SETQ XR (AEVAL* (LIST 'SQRT X)))
                 (SETQ YR (AEVAL* (LIST 'SQRT Y)))
                 (SETQ ZR (AEVAL* (LIST 'SQRT Z)))
                 (SETQ LAMDA
                         (AEVAL*
                          (LIST 'PLUS (LIST 'TIMES XR (LIST 'PLUS YR ZR))
                                (LIST 'TIMES YR ZR))))
                 (SETQ ALFA
                         (AEVAL*
                          (LIST 'PLUS (LIST 'TIMES P (LIST 'PLUS XR YR ZR))
                                (LIST 'TIMES XR YR ZR))))
                 (SETQ ALFA (AEVAL* (LIST 'TIMES ALFA ALFA)))
                 (SETQ BETA
                         (AEVAL*
                          (LIST 'TIMES P (LIST 'EXPT (LIST 'PLUS P LAMDA) 2))))
                 (AEVAL* (LIST 'PRECISION OLDPREC))
                 (SETQ TMP2 (AEVAL* (LIST 'RC ALFA BETA)))
                 (AEVAL*
                  (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
                 (SETQ SIGMA
                         (AEVAL* (LIST 'PLUS SIGMA (LIST 'TIMES POW TMP2))))
                 (SETQ POW (AEVAL* (LIST 'QUOTIENT POW 4)))
                 (SETQ X (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS X LAMDA) 4)))
                 (SETQ Y (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Y LAMDA) 4)))
                 (SETQ Z (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS Z LAMDA) 4)))
                 (SETQ P (AEVAL* (LIST 'QUOTIENT (LIST 'PLUS P LAMDA) 4)))
                 (AEVAL* 'NIL))))
              (AEVAL* 'NIL)))
      (SETQ EA
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES DX (LIST 'PLUS DY DZ))
                     (LIST 'TIMES DY DZ))))
      (SETQ EB (AEVAL (LIST 'TIMES DX DY DZ)))
      (SETQ EC (AEVAL (LIST 'TIMES DP DP)))
      (SETQ E2 (AEVAL (LIST 'DIFFERENCE EA (LIST 'TIMES 3 EC))))
      (SETK 'E3
            (AEVAL
             (LIST 'PLUS EB (LIST 'TIMES 2 DP (LIST 'DIFFERENCE EA EC)))))
      (SETQ TMP
              (AEVAL
               (LIST 'PLUS 1
                     (LIST 'TIMES E2
                           (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT 3 14))
                                 (LIST 'DIFFERENCE
                                       (LIST 'TIMES (LIST 'QUOTIENT 9 88) E2)
                                       (LIST 'TIMES (LIST 'QUOTIENT 9 52)
                                             'E3)))))))
      (SETQ TMP2
              (AEVAL
               (LIST 'TIMES EB
                     (LIST 'PLUS (LIST 'QUOTIENT 1 6)
                           (LIST 'TIMES 3 DP
                                 (LIST 'PLUS
                                       (LIST 'MINUS (LIST 'QUOTIENT 1 11))
                                       (LIST 'QUOTIENT DP 26)))))))
      (SETQ TMP3
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES DP EA
                           (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 3)
                                 (LIST 'TIMES 3 (LIST 'QUOTIENT DP 22))))
                     (LIST 'TIMES (LIST 'QUOTIENT 1 3) DP EC))))
      (SETQ TMP
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES 3 SIGMA)
                     (LIST 'TIMES POW
                           (LIST 'QUOTIENT (LIST 'PLUS TMP TMP2 TMP3)
                                 (LIST 'TIMES MU (LIST 'SQRT MU)))))))
      (COND
       ((EVALLESSP (AEVAL OLDP) 0)
        (PROGN
         (AEVAL (LIST 'PRECISION OLDPREC))
         (SETQ TMP2 (AEVAL (LIST 'RF X Y Z)))
         (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
         (SETQ TMP
                 (AEVAL
                  (LIST 'TIMES A
                        (LIST 'PLUS (LIST 'TIMES B TMP)
                              (LIST 'TIMES 3 (LIST 'DIFFERENCE RCX TMP2))))))
         (AEVAL 'NIL))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'CARLSON_RCR 'NUMBER-OF-ARGS 2) 
(FLAG '(CARLSON_RCR) 'OPFN) 
(PUT 'CARLSON_RCR 'DEFINED-ON-LINE '514) 
(PUT 'CARLSON_RCR 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'CARLSON_RCR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CARLSON_RCR (X Y)
    (LIST 'WHEREEXP
          (LIST 'LIST
                (LIST 'REPLACEBY 'Z
                      (LIST 'SQRT (LIST 'ABS (LIST 'DIFFERENCE X Y)))))
          (COND
           ((OR (EVALLESSP (AEVAL X) 0) (EVALEQUAL (AEVAL Y) 0))
            (AEVAL
             (REDERR
              (REVALX
               "1st parameter of RC must be non-negative and the 2nd non-zero"))))
           ((EVALLESSP (AEVAL Y) 0)
            (AEVAL
             (LIST 'QUOTIENT (LIST 'ATANH (LIST 'QUOTIENT (LIST 'SQRT X) 'Z))
                   'Z)))
           ((EVALLESSP (AEVAL Y) (AEVAL X))
            (AEVAL
             (LIST 'QUOTIENT (LIST 'ATANH (LIST 'QUOTIENT 'Z (LIST 'SQRT X)))
                   'Z)))
           ((EVALEQUAL (AEVAL X) 0)
            (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 'Z))))
           ((EVALLESSP (AEVAL X) (AEVAL Y))
            (AEVAL
             (LIST 'QUOTIENT (LIST 'ATAN (LIST 'QUOTIENT 'Z (LIST 'SQRT X)))
                   'Z)))
           (T (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT X))))))) 
(PUT 'CARLSON_RC 'NUMBER-OF-ARGS 2) 
(FLAG '(CARLSON_RC) 'OPFN) 
(PUT 'CARLSON_RC 'DEFINED-ON-LINE '524) 
(PUT 'CARLSON_RC 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'CARLSON_RC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CARLSON_RC (X Y)
    (COND
     ((OR (EVALEQUAL (AEVAL Y) 0)
          (AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0) (EVALLESSP (AEVAL X) 0)))
      (AEVAL
       (REDERR
        (REVALX "1st parameter of RC must be non-negative and 2nd non-zero"))))
     ((EVALEQUAL (AEVAL X) (AEVAL Y))
      (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT X))))
     ((EVALEQUAL (AEVAL X) 0)
      (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 (LIST 'SQRT Y)))))
     (T
      (AEVAL
       (LIST 'WHEREEXP
             (LIST 'LIST
                   (LIST 'REPLACEBY 'Z (LIST 'SQRT (LIST 'DIFFERENCE X Y))))
             (COND
              ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0)
                    (EVALLESSP (AEVAL Y) 0))
               (AEVAL
                (LIST 'QUOTIENT
                      (LIST 'ATANH (LIST 'QUOTIENT (LIST 'SQRT X) 'Z)) 'Z)))
              (T
               (AEVAL
                (LIST 'QUOTIENT
                      (LIST 'ATANH (LIST 'QUOTIENT 'Z (LIST 'SQRT X)))
                      'Z))))))))) 
(PUT 'SYM_INT_RFR 'NUMBER-OF-ARGS 3) 
(FLAG '(SYM_INT_RFR) 'OPFN) 
(PUT 'SYM_INT_RFR 'DEFINED-ON-LINE '534) 
(PUT 'SYM_INT_RFR 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'SYM_INT_RFR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SYM_INT_RFR (X Y Z)
    (PROG (T0 TN A0 AN C0 CN TOL THETA TMP OLDPREC N)
      (SETQ N (AEVAL 0))
      (COND
       ((EVALLEQ (AEVAL X) 0)
        (COND ((EVALEQUAL (AEVAL X) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))
              (T
               (AEVAL
                (REDERR
                 (REVALX
                  "divergent integral RF: negative first argument")))))))
      (COND
       ((EVALLEQ (AEVAL Y) 0)
        (COND ((EVALEQUAL (AEVAL Y) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))
              (T
               (AEVAL
                (REDERR
                 (REVALX
                  "divergent integral RF: negative second argument")))))))
      (COND
       ((EVALLEQ (AEVAL Z) 0)
        (COND ((EVALEQUAL (AEVAL Z) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))
              (T
               (AEVAL
                (REDERR
                 (REVALX
                  "divergent integral RF: negative third argument")))))))
      (COND
       ((EVALGREATERP (AEVAL N) 1)
        (AEVAL
         (REDERR
          (REVALX "divergent integral RF: more than one zero argument")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (COND
       ((EVALGREATERP (AEVAL X) (AEVAL Y))
        (PROGN (SETQ TMP (AEVAL Y)) (SETQ Y (AEVAL X)) (SETQ X (AEVAL TMP)))))
      (COND
       ((EVALGREATERP (AEVAL Y) (AEVAL Z))
        (PROGN (SETQ TMP (AEVAL Z)) (SETQ Z (AEVAL Y)) (SETQ Y (AEVAL TMP)))))
      (COND
       ((EVALGREATERP (AEVAL X) (AEVAL Y))
        (PROGN (SETQ TMP (AEVAL Y)) (SETQ Y (AEVAL X)) (SETQ X (AEVAL TMP)))))
      (COND
       ((EVALGEQ (AEVAL (LIST 'TIMES 2 Y)) (AEVAL (LIST 'PLUS X Z)))
        (PROGN
         (SETQ TMP (AEVAL X))
         (SETQ X (AEVAL Z))
         (SETQ Z (AEVAL TMP))
         (SETQ THETA (AEVAL (MINUS 1)))
         (AEVAL 'NIL)))
       (T (SETQ THETA (AEVAL 1))))
      (SETQ CN (AEVAL 1))
      (SETQ T0 (AEVAL (LIST 'SQRT X)))
      (SETQ A0 (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'DIFFERENCE X Z)))))
      (SETQ C0 (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'DIFFERENCE X Y)))))
      (SETQ N (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS CN)) (AEVAL* TOL))
             (PROGN
              (SETQ TN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS T0
                                   (LIST 'SQRT
                                         (LIST 'PLUS (LIST 'EXPT T0 2)
                                               (LIST 'TIMES THETA
                                                     (LIST 'EXPT C0 2)))))
                             2)))
              (SETQ AN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS A0
                                   (LIST 'SQRT
                                         (LIST 'DIFFERENCE (LIST 'EXPT A0 2)
                                               (LIST 'EXPT C0 2))))
                             2)))
              (SETQ CN
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'EXPT C0 2) (LIST 'TIMES 4 AN))))
              (SETQ C0 (AEVAL* CN))
              (SETQ A0 (AEVAL* AN))
              (SETQ T0 (AEVAL* TN))
              (SETQ N (AEVAL* (LIST 'PLUS N 1)))
              (AEVAL* 'NIL)))
      (SETQ Y (AEVAL (LIST 'EXPT TN 2)))
      (SETQ X (AEVAL (LIST 'PLUS Y THETA (LIST 'EXPT AN 2))))
      (SETQ Z (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'DIFFERENCE X Y)))))
      (COND
       ((EVALLESSP (AEVAL Y) (AEVAL X))
        (SETQ TMP
                (AEVAL
                 (LIST 'QUOTIENT
                       (LIST 'ATANH (LIST 'QUOTIENT Z (LIST 'SQRT X))) Z))))
       ((EVALEQUAL (AEVAL X) 0)
        (SETQ TMP (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 Z)))))
       ((EVALLESSP (AEVAL X) (AEVAL Y))
        (SETQ TMP
                (AEVAL
                 (LIST 'QUOTIENT (LIST 'ATAN (LIST 'QUOTIENT Z (LIST 'SQRT X)))
                       Z))))
       (T (SETQ TMP (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT X))))))
      (SETQ TMP
              (AEVAL
               (LIST 'CARLSON_RCR
                     (LIST 'PLUS (LIST 'EXPT TN 2)
                           (LIST 'TIMES THETA (LIST 'EXPT AN 2)))
                     (LIST 'EXPT TN 2))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'SYM_INT_RF 'NUMBER-OF-ARGS 3) 
(FLAG '(SYM_INT_RF) 'OPFN) 
(PUT 'SYM_INT_RF 'DEFINED-ON-LINE '591) 
(PUT 'SYM_INT_RF 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'SYM_INT_RF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SYM_INT_RF (X Y Z)
    (PROG (T0 TN A0 AN C0 CN TOL TMP OLDPREC N)
      (SETQ N (AEVAL 0))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN X)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative first argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Y)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative second argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Z)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative third argument")))))
         (COND ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALGREATERP (AEVAL N) 1)
        (AEVAL
         (REDERR
          (REVALX "divergent integral RF: more than one argument is zero")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TMP (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Y X))))
      (SETK 'TMP2 (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Z X))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Z X))) (AEVAL TMP))
        (PROGN (SETQ TMP (AEVAL Y)) (SETQ Y (AEVAL Z)) (SETQ Z (AEVAL TMP)))))
      (SETQ CN (AEVAL 1))
      (SETQ T0 (AEVAL (LIST 'SQRT X)))
      (SETQ A0 (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE Z X))))
      (SETQ C0 (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE Y X))))
      (SETQ N (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS CN)) (AEVAL* TOL))
             (PROGN
              (SETQ TN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS T0
                                   (LIST 'SQRT
                                         (LIST 'PLUS (LIST 'EXPT T0 2)
                                               (LIST 'EXPT C0 2))))
                             2)))
              (SETQ AN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS A0
                                   (LIST 'SQRT
                                         (LIST 'DIFFERENCE (LIST 'EXPT A0 2)
                                               (LIST 'EXPT C0 2))))
                             2)))
              (SETQ CN
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'EXPT C0 2) (LIST 'TIMES 4 AN))))
              (SETQ C0 (AEVAL* CN))
              (SETQ A0 (AEVAL* AN))
              (SETQ T0 (AEVAL* TN))
              (SETQ N (AEVAL* (LIST 'PLUS N 1)))
              (AEVAL* 'NIL)))
      (SETQ Y (AEVAL (LIST 'EXPT TN 2)))
      (SETQ X (AEVAL (LIST 'PLUS Y (LIST 'EXPT AN 2))))
      (SETQ Z (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE X Y))))
      (COND
       ((EVALEQUAL (AEVAL X) (AEVAL Y))
        (SETQ TMP (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT X)))))
       ((EVALEQUAL (AEVAL X) 0)
        (SETQ TMP (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 (LIST 'SQRT Y))))))
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0) (EVALLESSP (AEVAL Y) 0))
        (SETQ TMP
                (AEVAL
                 (LIST 'QUOTIENT
                       (LIST 'ATANH (LIST 'QUOTIENT (LIST 'SQRT X) Z)) Z))))
       (T
        (SETQ TMP
                (AEVAL
                 (LIST 'QUOTIENT
                       (LIST 'ATANH (LIST 'QUOTIENT Z (LIST 'SQRT X))) Z)))))
      (SETQ TMP
              (AEVAL
               (LIST 'CARLSON_RC
                     (LIST 'PLUS (LIST 'EXPT TN 2) (LIST 'EXPT AN 2))
                     (LIST 'EXPT TN 2))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'RC1 'NUMBER-OF-ARGS 2) 
(FLAG '(RC1) 'OPFN) 
(PUT 'RC1 'DEFINED-ON-LINE '652) 
(PUT 'RC1 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RC1 (X Y)
    (PROG (OLDPREC RES Z)
      (COND
       ((OR (EVALEQUAL (AEVAL Y) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
                 (EVALLESSP (AEVAL X) 0)))
        (AEVAL
         (REDERR
          (REVALX
           "1st parameter of RC must be non-negative and 2nd non-zero")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (COND
       ((EVALEQUAL (AEVAL X) (AEVAL Y))
        (SETQ RES (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT X)))))
       ((EVALEQUAL (AEVAL X) 0)
        (SETQ RES (AEVAL (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 (LIST 'SQRT Y))))))
       (T
        (PROGN
         (SETQ Z (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE X Y))))
         (COND
          ((AND (EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0) (EVALLESSP (AEVAL Y) 0))
           (SETQ RES
                   (AEVAL
                    (LIST 'QUOTIENT
                          (LIST 'ATANH (LIST 'QUOTIENT (LIST 'SQRT X) Z)) Z))))
          (T
           (SETQ RES
                   (AEVAL
                    (LIST 'QUOTIENT
                          (LIST 'ATANH (LIST 'QUOTIENT Z (LIST 'SQRT X)))
                          Z)))))
         (AEVAL 'NIL))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL RES)))) 
(PUT 'RF1 'NUMBER-OF-ARGS 3) 
(FLAG '(RF1) 'OPFN) 
(PUT 'RF1 'DEFINED-ON-LINE '671) 
(PUT 'RF1 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'RF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RF1 (X Y Z)
    (PROG (T0 TN A0 AN C0 CN TOL TMP OLDPREC N)
      (SETQ N (AEVAL 0))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN X)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative first argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Y)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Y)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative second argument")))))
         (COND
          ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1))))))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART Z)) 0)
        (PROGN
         (SETQ TMP (AEVAL (LIST 'SIGN Z)))
         (COND
          ((EVALEQUAL (AEVAL TMP) (MINUS 1))
           (AEVAL
            (REDERR
             (REVALX "divergent integral RF: negative third argument")))))
         (COND ((EVALEQUAL (AEVAL TMP) 0) (SETQ N (AEVAL (LIST 'PLUS N 1)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALGREATERP (AEVAL N) 1)
        (AEVAL
         (REDERR
          (REVALX "divergent integral RF: more than one argument is zero")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 4) 16)))
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TMP (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Y X))))
      (SETK 'TMP2 (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Z X))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'DIFFERENCE Z X))) (AEVAL TMP))
        (PROGN (SETQ TMP (AEVAL Y)) (SETQ Y (AEVAL Z)) (SETQ Z (AEVAL TMP)))))
      (SETQ CN (AEVAL 1))
      (SETQ T0 (AEVAL (LIST 'SQRT X)))
      (SETQ A0 (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE Z X))))
      (SETQ C0 (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE Y X))))
      (SETQ N (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS CN)) (AEVAL* TOL))
             (PROGN
              (SETQ TN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS T0
                                   (LIST 'SQRT
                                         (LIST 'PLUS (LIST 'EXPT T0 2)
                                               (LIST 'EXPT C0 2))))
                             2)))
              (SETQ AN
                      (AEVAL*
                       (LIST 'QUOTIENT
                             (LIST 'PLUS A0
                                   (LIST 'SQRT
                                         (LIST 'DIFFERENCE (LIST 'EXPT A0 2)
                                               (LIST 'EXPT C0 2))))
                             2)))
              (SETQ CN
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'EXPT C0 2) (LIST 'TIMES 4 AN))))
              (SETQ C0 (AEVAL* CN))
              (SETQ A0 (AEVAL* AN))
              (SETQ T0 (AEVAL* TN))
              (SETQ N (AEVAL* (LIST 'PLUS N 1)))
              (AEVAL* 'NIL)))
      (SETQ TMP
              (AEVAL
               (LIST 'RC1 (LIST 'PLUS (LIST 'EXPT TN 2) (LIST 'EXPT AN 2))
                     (LIST 'EXPT TN 2))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL TMP)))) 
(PUT 'CHECK_DEPENDENCE 'NUMBER-OF-ARGS 2) 
(FLAG '(CHECK_DEPENDENCE) 'OPFN) 
(PUT 'CHECK_DEPENDENCE 'DEFINED-ON-LINE '728) 
(PUT 'CHECK_DEPENDENCE 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'CHECK_DEPENDENCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_DEPENDENCE (F1 F2)
    (COND
     ((EVALEQUAL
       (AEVAL
        (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'FIRST F1) (LIST 'SECOND F2))
              (LIST 'TIMES (LIST 'FIRST F2) (LIST 'SECOND F1))))
       0)
      (AEVAL
       (REDERR (REVALX "Factors in elliptic integral not independent")))))) 
(PUT 'CHECK_FACTORS 'NUMBER-OF-ARGS 5) 
(FLAG '(CHECK_FACTORS) 'OPFN) 
(PUT 'CHECK_FACTORS 'DEFINED-ON-LINE '732) 
(PUT 'CHECK_FACTORS 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'CHECK_FACTORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECK_FACTORS (FAC1 FAC2 FAC3 FAC4 FAC5)
    (PROGN
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC1 FAC2))
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC1 FAC3))
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC1 FAC4))
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC2 FAC3))
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC2 FAC4))
     (AEVAL (LIST 'CHECK_DEPENDENCE FAC3 FAC4))
     (COND
      ((EVALNEQ (AEVAL FAC5) 0)
       (PROGN
        (AEVAL (LIST 'CHECK_DEPENDENCE FAC1 FAC5))
        (AEVAL (LIST 'CHECK_DEPENDENCE FAC2 FAC5))
        (AEVAL (LIST 'CHECK_DEPENDENCE FAC3 FAC5))
        (AEVAL (LIST 'CHECK_DEPENDENCE FAC4 FAC5))
        (AEVAL 'NIL))))
     (AEVAL 'NIL))) 
(PUT 'ELLINT_1ST 'NUMBER-OF-ARGS 6) 
(FLAG '(ELLINT_1ST) 'OPFN) 
(PUT 'ELLINT_1ST 'DEFINED-ON-LINE '748) 
(PUT 'ELLINT_1ST 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'ELLINT_1ST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLINT_1ST (LOWLIM UPLIM FAC1 FAC2 FAC3 FAC4)
    (PROG (X1 X2 X3 X4 Y1 Y2 Y3 Y4 U12 U13 U23)
      (AEVAL (LIST 'CHECK_FACTORS FAC1 FAC2 FAC3 FAC4 0))
      (COND
       ((AND (EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (AEVAL
         (LIST 'PLUS
               (LIST 'ELLINT_1ST (LIST 'MINUS 'INFINITY) 0 FAC1 FAC2 FAC3 FAC4)
               (LIST 'ELLINT_1ST 0 'INFINITY FAC1 FAC2 FAC3 FAC4)))))
      (COND
       ((EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
        (PROGN
         (SETQ X1 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC1))))
         (SETQ X2 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC2))))
         (SETQ X3 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC3))))
         (SETQ X4 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC4))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ X1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) UPLIM)))))
         (SETQ X2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) UPLIM)))))
         (SETQ X3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) UPLIM)))))
         (SETQ X4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) UPLIM)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY)))
        (PROGN
         (SETQ Y1 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC1)))))
         (SETQ Y2 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC2)))))
         (SETQ Y3 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC3)))))
         (SETQ Y4 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC4)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ Y1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) LOWLIM)))))
         (SETQ Y2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) LOWLIM)))))
         (SETQ Y3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) LOWLIM)))))
         (SETQ Y4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) LOWLIM)))))
         (AEVAL 'NIL))))
      (SETQ U12
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X2 Y3 Y4)
                     (LIST 'TIMES Y1 Y2 X3 X4))))
      (SETQ U13
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X3 Y2 Y4)
                     (LIST 'TIMES Y1 Y3 X2 X4))))
      (SETQ U23
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X2 X3 Y1 Y4)
                     (LIST 'TIMES Y2 Y3 X1 X4))))
      (COND
       ((AND (EVALNEQ (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALNEQ (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (PROGN
         (SETQ U12
                 (AEVAL (LIST 'QUOTIENT U12 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U13
                 (AEVAL (LIST 'QUOTIENT U13 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U23
                 (AEVAL (LIST 'QUOTIENT U23 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'TIMES 2
              (LIST 'RF (LIST 'EXPT U12 2) (LIST 'EXPT U13 2)
                    (LIST 'EXPT U23 2))))))) 
(PUT 'ELLINT_2ND 'NUMBER-OF-ARGS 6) 
(FLAG '(ELLINT_2ND) 'OPFN) 
(PUT 'ELLINT_2ND 'DEFINED-ON-LINE '793) 
(PUT 'ELLINT_2ND 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'ELLINT_2ND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLINT_2ND (LOWLIM UPLIM FAC1 FAC2 FAC3 FAC4)
    (PROG (X1 X2 X3 X4 Y1 Y2 Y3 Y4 U12 U13 U23 D12 D13)
      (AEVAL (LIST 'CHECK_FACTORS FAC1 FAC2 FAC3 FAC4 0))
      (COND
       ((AND (EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (RETURN
         (AEVAL
          (LIST 'PLUS
                (LIST 'ELLINT_2ND (LIST 'MINUS 'INFINITY) 0 FAC1 FAC2 FAC3
                      FAC4)
                (LIST 'ELLINT_2ND 0 'INFINITY FAC1 FAC2 FAC3 FAC4))))))
      (COND
       ((EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
        (PROGN
         (SETQ X1 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC1))))
         (SETQ X2 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC2))))
         (SETQ X3 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC3))))
         (SETQ X4 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC4))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ X1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) UPLIM)))))
         (SETQ X2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) UPLIM)))))
         (SETQ X3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) UPLIM)))))
         (SETQ X4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) UPLIM)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY)))
        (PROGN
         (SETQ Y1 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC1)))))
         (SETQ Y2 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC2)))))
         (SETQ Y3 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC3)))))
         (SETQ Y4 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC4)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ Y1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) LOWLIM)))))
         (SETQ Y2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) LOWLIM)))))
         (SETQ Y3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) LOWLIM)))))
         (SETQ Y4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) LOWLIM)))))
         (AEVAL 'NIL))))
      (SETQ U23
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X2 X3 Y1 Y4)
                     (LIST 'TIMES Y2 Y3 X1 X4))))
      (COND
       ((EVALEQUAL (AEVAL U23) 0)
        (COND
         ((EVALNEQ (AEVAL (LIST 'SECOND FAC2)) 0)
          (PROGN
           (SETQ D12
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC2))
                          (LIST 'TIMES (LIST 'FIRST FAC2)
                                (LIST 'SECOND FAC1)))))
           (SETQ D13
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC4))
                          (LIST 'TIMES (LIST 'FIRST FAC4)
                                (LIST 'SECOND FAC2)))))
           (SETQ U12
                   (AEVAL (LIST 'ELLINT_2ND LOWLIM UPLIM FAC2 FAC1 FAC3 FAC4)))
           (RETURN
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'PLUS (LIST 'SECOND FAC1)
                               (LIST 'TIMES (LIST 'SECOND FAC4)
                                     (LIST 'QUOTIENT D12 D13)))
                         (LIST 'QUOTIENT U12 (LIST 'SECOND FAC2)))
                   (LIST 'TIMES (LIST 'QUOTIENT D12 D13)
                         (LIST 'ELLINT_1ST LOWLIM UPLIM FAC1 FAC2 FAC3
                               FAC4)))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ D12
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC3))
                          (LIST 'TIMES (LIST 'FIRST FAC3)
                                (LIST 'SECOND FAC1)))))
           (SETQ D13
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC4))
                          (LIST 'TIMES (LIST 'FIRST FAC4)
                                (LIST 'SECOND FAC3)))))
           (SETQ U12
                   (AEVAL (LIST 'ELLINT_2ND LOWLIM UPLIM FAC3 FAC1 FAC3 FAC4)))
           (RETURN
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'PLUS (LIST 'SECOND FAC1)
                               (LIST 'TIMES (LIST 'SECOND FAC4)
                                     (LIST 'QUOTIENT D12 D13)))
                         (LIST 'QUOTIENT U12 (LIST 'SECOND FAC3)))
                   (LIST 'TIMES (LIST 'QUOTIENT D12 D13)
                         (LIST 'ELLINT_1ST LOWLIM UPLIM FAC1 FAC2 FAC3
                               FAC4)))))
           (AEVAL 'NIL))))))
      (SETQ U12
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X2 Y3 Y4)
                     (LIST 'TIMES Y1 Y2 X3 X4))))
      (SETQ U13
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X3 Y2 Y4)
                     (LIST 'TIMES Y1 Y3 X2 X4))))
      (COND
       ((AND (EVALNEQ (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALNEQ (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (PROGN
         (SETQ U12
                 (AEVAL (LIST 'QUOTIENT U12 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U13
                 (AEVAL (LIST 'QUOTIENT U13 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U23
                 (AEVAL (LIST 'QUOTIENT U23 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (AEVAL 'NIL))))
      (SETQ D12
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC2))
                     (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC1)))))
      (COND
       ((AND (EVALNEQ (AEVAL U23) 0)
             (EVALNEQ (AEVAL (LIST 'EXPT U12 2)) (AEVAL (LIST 'EXPT U13 2))))
        (SETQ D13
                (AEVAL
                 (LIST 'DIFFERENCE
                       (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC3))
                       (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC1))))))
       (T
        (PROGN
         (SETQ Y2 (AEVAL U13))
         (SETQ U13 (AEVAL U23))
         (SETQ U23 (AEVAL Y2))
         (SETQ D13
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC3))
                        (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC2)))))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'TIMES 2 D12 D13
                    (LIST 'QUOTIENT
                          (LIST 'RD (LIST 'EXPT U12 2) (LIST 'EXPT U13 2)
                                (LIST 'EXPT U23 2))
                          3))
              (LIST 'TIMES 2 X1
                    (LIST 'QUOTIENT Y1 (LIST 'TIMES X4 Y4 U23)))))))) 
(PUT 'ELLINT_3RD 'NUMBER-OF-ARGS 7) 
(FLAG '(ELLINT_3RD) 'OPFN) 
(PUT 'ELLINT_3RD 'DEFINED-ON-LINE '862) 
(PUT 'ELLINT_3RD 'DEFINED-IN-FILE 'ELLIPFN/EFNUMERIC.RED) 
(PUT 'ELLINT_3RD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE ELLINT_3RD (LOWLIM UPLIM FAC1 FAC2 FAC3 FAC4 FAC5)
    (PROG (X1 X2 X3 X4 Y1 Y2 Y3 Y4 U12 U13 U23 D12 D13 D14 D15 D25 U15 S15 Q15)
      (AEVAL (LIST 'CHECK_FACTORS FAC1 FAC2 FAC3 FAC4 0))
      (COND
       ((AND (EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (AEVAL
         (LIST 'PLUS
               (LIST 'ELLINT_3RD (LIST 'MINUS 'INFINITY) 0 FAC1 FAC2 FAC3 FAC4
                     FAC5)
               (LIST 'ELLINT_3RD 0 'INFINITY FAC1 FAC2 FAC3 FAC4 FAC5)))))
      (COND
       ((EVALEQUAL (AEVAL UPLIM) (AEVAL 'INFINITY))
        (PROGN
         (SETQ X1 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC1))))
         (SETQ X2 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC2))))
         (SETQ X3 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC3))))
         (SETQ X4 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC4))))
         (SETK 'X5 (AEVAL (LIST 'SQRT (LIST 'SECOND FAC5))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ X1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) UPLIM)))))
         (SETQ X2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) UPLIM)))))
         (SETQ X3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) UPLIM)))))
         (SETQ X4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) UPLIM)))))
         (SETK 'X5
               (AEVAL
                (LIST 'SQRT
                      (LIST 'PLUS (LIST 'FIRST FAC5)
                            (LIST 'TIMES (LIST 'SECOND FAC5) UPLIM)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY)))
        (PROGN
         (SETQ Y1 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC1)))))
         (SETQ Y2 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC2)))))
         (SETQ Y3 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC3)))))
         (SETQ Y4 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC4)))))
         (SETK 'Y5 (AEVAL (LIST 'SQRT (LIST 'MINUS (LIST 'SECOND FAC5)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ Y1
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC1)
                              (LIST 'TIMES (LIST 'SECOND FAC1) LOWLIM)))))
         (SETQ Y2
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC2)
                              (LIST 'TIMES (LIST 'SECOND FAC2) LOWLIM)))))
         (SETQ Y3
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC3)
                              (LIST 'TIMES (LIST 'SECOND FAC3) LOWLIM)))))
         (SETQ Y4
                 (AEVAL
                  (LIST 'SQRT
                        (LIST 'PLUS (LIST 'FIRST FAC4)
                              (LIST 'TIMES (LIST 'SECOND FAC4) LOWLIM)))))
         (SETK 'Y5
               (AEVAL
                (LIST 'SQRT
                      (LIST 'PLUS (LIST 'FIRST FAC5)
                            (LIST 'TIMES (LIST 'SECOND FAC5) LOWLIM)))))
         (AEVAL 'NIL))))
      (COND
       ((OR (EVALEQUAL (AEVAL X1) 0) (EVALEQUAL (AEVAL Y1) 0))
        (COND
         ((AND (EVALNEQ (AEVAL X2) 0) (EVALNEQ (AEVAL Y2) 0)
               (EVALNEQ (AEVAL (LIST 'SECOND FAC2)) 0))
          (PROGN
           (SETQ D12
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC2))
                          (LIST 'TIMES (LIST 'FIRST FAC2)
                                (LIST 'SECOND FAC1)))))
           (SETQ D25
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC5))
                          (LIST 'TIMES (LIST 'FIRST FAC5)
                                (LIST 'SECOND FAC2)))))
           (SETQ U12
                   (AEVAL
                    (LIST 'ELLINT_3RD LOWLIM UPLIM FAC2 FAC1 FAC3 FAC4 FAC5)))
           (RETURN
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'PLUS (LIST 'SECOND FAC1)
                               (LIST 'TIMES (LIST 'SECOND FAC5)
                                     (LIST 'QUOTIENT D12 D25)))
                         (LIST 'QUOTIENT U12 (LIST 'SECOND FAC2)))
                   (LIST 'TIMES (LIST 'QUOTIENT D12 D25)
                         (LIST 'ELLINT_1ST LOWLIM UPLIM FAC1 FAC2 FAC3
                               FAC4)))))
           (AEVAL 'NIL)))
         ((AND (EVALNEQ (AEVAL X3) 0) (EVALNEQ (AEVAL Y3) 0)
               (EVALNEQ (AEVAL (LIST 'SECOND FAC3)) 0))
          (PROGN
           (SETQ D13
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC3))
                          (LIST 'TIMES (LIST 'FIRST FAC3)
                                (LIST 'SECOND FAC1)))))
           (SETK 'D35
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC4))
                        (LIST 'TIMES (LIST 'FIRST FAC4) (LIST 'SECOND FAC3)))))
           (SETQ U12
                   (AEVAL
                    (LIST 'ELLINT_3RD LOWLIM UPLIM FAC3 FAC1 FAC2 FAC4 FAC5)))
           (RETURN
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'PLUS (LIST 'SECOND FAC1)
                               (LIST 'TIMES (LIST 'SECOND FAC5)
                                     (LIST 'QUOTIENT D13 'D35)))
                         (LIST 'QUOTIENT U12 (LIST 'SECOND FAC3)))
                   (LIST 'TIMES (LIST 'QUOTIENT D13 'D35)
                         (LIST 'ELLINT_1ST LOWLIM UPLIM FAC1 FAC2 FAC3
                               FAC4)))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ D14
                   (AEVAL
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC4))
                          (LIST 'TIMES (LIST 'FIRST FAC4)
                                (LIST 'SECOND FAC1)))))
           (SETK 'D45
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES (LIST 'FIRST FAC4) (LIST 'SECOND FAC5))
                        (LIST 'TIMES (LIST 'FIRST FAC5) (LIST 'SECOND FAC4)))))
           (SETQ U12
                   (AEVAL
                    (LIST 'ELLINT_3RD LOWLIM UPLIM FAC4 FAC1 FAC2 FAC3 FAC5)))
           (RETURN
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'PLUS (LIST 'SECOND FAC1)
                               (LIST 'TIMES (LIST 'SECOND FAC5)
                                     (LIST 'QUOTIENT D14 'D45)))
                         (LIST 'QUOTIENT U12 (LIST 'SECOND FAC4)))
                   (LIST 'TIMES (LIST 'QUOTIENT D14 'D45)
                         (LIST 'ELLINT_1ST LOWLIM UPLIM FAC1 FAC2 FAC3
                               FAC4)))))
           (AEVAL 'NIL))))))
      (SETQ U12
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X2 Y3 Y4)
                     (LIST 'TIMES Y1 Y2 X3 X4))))
      (SETQ U13
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X1 X3 Y2 Y4)
                     (LIST 'TIMES Y1 Y3 X2 X4))))
      (SETQ U23
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES X2 X3 Y1 Y4)
                     (LIST 'TIMES Y2 Y3 X1 X4))))
      (SETQ S15
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES X2 X3 X4
                           (LIST 'QUOTIENT (LIST 'EXPT 'Y5 2) X1))
                     (LIST 'TIMES Y2 Y3 Y4
                           (LIST 'QUOTIENT (LIST 'EXPT 'X5 2) Y1)))))
      (COND
       ((AND (EVALNEQ (AEVAL UPLIM) (AEVAL 'INFINITY))
             (EVALNEQ (AEVAL LOWLIM) (AEVAL (LIST 'MINUS 'INFINITY))))
        (PROGN
         (SETQ U12
                 (AEVAL (LIST 'QUOTIENT U12 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U13
                 (AEVAL (LIST 'QUOTIENT U13 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ U23
                 (AEVAL (LIST 'QUOTIENT U23 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (SETQ S15
                 (AEVAL (LIST 'QUOTIENT S15 (LIST 'DIFFERENCE UPLIM LOWLIM))))
         (AEVAL 'NIL))))
      (SETQ D12
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC2))
                     (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC1)))))
      (SETQ D13
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC3))
                     (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC1)))))
      (SETQ D14
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC4))
                     (LIST 'TIMES (LIST 'FIRST FAC4) (LIST 'SECOND FAC1)))))
      (SETQ D15
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC1) (LIST 'SECOND FAC5))
                     (LIST 'TIMES (LIST 'FIRST FAC5) (LIST 'SECOND FAC1)))))
      (SETQ D25
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'FIRST FAC2) (LIST 'SECOND FAC5))
                     (LIST 'TIMES (LIST 'FIRST FAC5) (LIST 'SECOND FAC2)))))
      (SETK 'D35
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'FIRST FAC3) (LIST 'SECOND FAC5))
                   (LIST 'TIMES (LIST 'FIRST FAC5) (LIST 'SECOND FAC3)))))
      (SETK 'D45
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'FIRST FAC4) (LIST 'SECOND FAC5))
                   (LIST 'TIMES (LIST 'FIRST FAC5) (LIST 'SECOND FAC4)))))
      (SETQ U15
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'EXPT U12 2)
                     (LIST 'TIMES D13 D14 (LIST 'QUOTIENT D25 D15)))))
      (SETQ Q15
              (AEVAL
               (LIST 'TIMES
                     (LIST 'QUOTIENT (LIST 'EXPT (LIST 'TIMES 'X5 'Y5) 2)
                           (LIST 'EXPT (LIST 'TIMES X1 Y1) 2))
                     U15)))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'TIMES 2 D12 D13 D14
                    (LIST 'QUOTIENT
                          (LIST 'RJ (LIST 'EXPT U12 2) (LIST 'EXPT U13 2)
                                (LIST 'EXPT U23 2) U15)
                          (LIST 'TIMES 3 D15)))
              (LIST 'TIMES 2 (LIST 'RC (LIST 'EXPT S15 2) Q15))))))) 
(ENDMODULE) 