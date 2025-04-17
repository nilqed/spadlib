(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NUMINT)) 
(FLUID '(*NOEQUIV ACCURACY* SINGULARITIES*)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC)) 
(PUT 'INTRDEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'INTRDEVAL 'DEFINED-ON-LINE '47) 
(PUT 'INTRDEVAL 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTRDEVAL (U)
    (PROG (E VARS Y P SINGULARITIES* IMODE R PROTFG* M W)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (ACCURACYCONTROL U 3 20))
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (COND
       ((AND (EQUAL (LENGTH U) 3)
             (OR (ATOM (CAR U)) (NOT (EQ (CAAR U) 'EQUAL))))
        (SETQ U (LIST (LIST 'EQUAL (CAR U) (CONS '*INTERVAL* (CDR U))))))
       ((AND (NOT (ATOM U)) (EQCAR (CAR U) 'LIST))
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CDAR U))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                        NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND ((NOT (EQCAR X 'EQUAL)) (TYPERR X "interval bounds")))
            (SETQ IMODE 'INFINITY)
            (COND
             ((NOT *ROUNDED) (SETDMODE 'ROUNDED (SETQ W (SETQ *ROUNDED T)))))
            (SETQ Y (REVALNUMINTERVAL (CADDR X) IMODE))
            (COND (W (SETDMODE 'ROUNDED (SETQ W (SETQ *ROUNDED NIL)))))
            (SETQ IMODE T)
            (SETQ VARS (CONS (CADR X) VARS))
            (SETQ P (CONS (CONS (CADR X) (CONS (CAR Y) (CADR Y))) P))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND ((NULL VARS) (REDERR "Missing interval bounds")))
      (SETQ M *MSG)
      (COND (*TRNUMERIC (SETQ *MSG NIL)) (T (SETQ PROTFG* T)))
      (SETQ R (INTRD0 E VARS P))
      (SETQ ERFG* NIL)
      (SETQ *MSG M)
      ((LAMBDA (DMODE*) (SETQ R (REVAL1 R T))) '|:RD:|)
      (RETURN R))) 
(PUT 'NUM_INT 'PSOPFN 'INTRDEVAL) 
(PUT 'INTRD0 'NUMBER-OF-ARGS 3) 
(PUT 'INTRD0 'DEFINED-ON-LINE '80) 
(PUT 'INTRD0 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRD0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRD0 (E VARS P)
    (COND ((CDR VARS) (INTRD2 E VARS P))
          (T
           (PROG (FCN X LO HI)
             (SETQ FCN E)
             (SETQ X (CAR VARS))
             (SETQ LO (CAR (CDAR P)))
             (SETQ HI (CDR (CDAR P)))
             (RETURN (INTRD1 FCN X LO HI 0)))))) 
(PUT 'INTRD1 'NUMBER-OF-ARGS 5) 
(PUT 'INTRD1 'DEFINED-ON-LINE '90) 
(PUT 'INTRD1 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRD1 (FCN X LO HI N)
    (PROG (W R1 R2 RES)
      (SETQ N (PLUS N 1))
      (COND
       (*TRNUMERIC
        (PROGN
         (WRITEPRI N 'FIRST)
         (WRITEPRI " level integrate :" NIL)
         (WRITEPRI (MKQUOTE FCN) NIL)
         (WRITEPRI " over " NIL)
         (WRITEPRI (MKQUOTE (MKINTERVAL LO HI)) 'LAST)
         NIL)))
      (COND
       ((EVALGREATERPX LO HI)
        (PROGN
         (SETQ W LO)
         (SETQ LO HI)
         (SETQ HI W)
         (SETQ FCN (LIST 'MINUS FCN)))))
      (COND
       ((EQUAL LO MINUS-INFINITY*)
        (COND
         ((EVALGREATERPX HI (MINUS 1))
          (PROGN
           (SETQ R1 (INTRD1 FCN X (MINUS 1) HI N))
           (SETQ R2 (INTRD1 (SUBST (LIST 'MINUS X) X FCN) X 1 'INFINITY N))
           (SETQ RES (REVAL1 (LIST 'PLUS R1 R2) T))
           NIL))
         (T
          (PROGN
           (SETQ RES
                   (INTRD1 (SUBST (LIST 'MINUS X) X FCN) X
                    (REVAL1 (LIST 'MINUS LO) T) 'INFINITY N))
           NIL)))))
      (COND (RES (GO FIN)))
      (COND
       ((EQUAL HI 'INFINITY)
        (COND
         ((EVALGREATERPX 1 LO)
          (PROGN
           (SETQ R1 (INTRD1 FCN X LO 1 N))
           (SETQ R2 (INTRD1 (INTTRANS FCN 1 X) X 0 1 N))
           (SETQ RES (REVAL1 (LIST 'PLUS R1 R2) T))
           NIL))
         (T (PROGN (SETQ RES (INTRD1 (INTTRANS FCN LO X) X 0 1 N)) NIL)))))
      (COND (RES (GO FIN)))
      (SETQ RES (INTRD1A FCN X LO HI (LIST (CONS X (CONS LO HI)))))
     FIN
      (COND
       (*TRNUMERIC
        (PROGN
         (WRITEPRI N 'FIRST)
         (WRITEPRI " level integral :" NIL)
         (WRITEPRI (MKQUOTE RES) 'LAST))))
      (RETURN RES))) 
(PUT 'INTTRANS 'NUMBER-OF-ARGS 3) 
(PUT 'INTTRANS 'DEFINED-ON-LINE '149) 
(PUT 'INTTRANS 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTTRANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTTRANS (U A X) (REVAL1 (INTTRANS1 U A X) T)) 
(PUT 'INTTRANS1 'NUMBER-OF-ARGS 3) 
(FLAG '(INTTRANS1) 'OPFN) 
(PUT 'INTTRANS1 'DEFINED-ON-LINE '152) 
(PUT 'INTTRANS1 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTTRANS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTTRANS1 (U A X)
    (PROGN
     (SETQ U
             (AEVAL
              (LIST 'TIMES
                    (LIST 'SUB
                          (LIST 'EQUAL X
                                (LIST 'QUOTIENT A (LIST 'DIFFERENCE 1 '&T)))
                          U)
                    (LIST 'QUOTIENT A
                          (LIST 'EXPT (LIST 'DIFFERENCE A '&T) 2)))))
     (AEVAL (LIST 'SUB (LIST 'EQUAL '&T X) U)))) 
(PUT 'INTRD1A 'NUMBER-OF-ARGS 5) 
(PUT 'INTRD1A 'DEFINED-ON-LINE '159) 
(PUT 'INTRD1A 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRD1A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRD1A (FCN X LO HI P)
    (PROG (U W ACC UU LOO HII OLDMODE CBOUND ORD)
      (SETQ ORD 0)
      (COND ((EVALGREATERP HI LO) (PROGN (SETQ LOO LO) (SETQ HII HI)))
            (T (PROGN (SETQ LOO HI) (SETQ HII LOO))))
      ((LAMBDA (DMODE* *ROUNDED)
         (SETQ U (REVAL1 (LIST 'INT (REVAL1 FCN T) X) T)))
       NIL NIL)
      (SETQ OLDMODE (SWITCH-MODE-RD NIL))
      ((LAMBDA (*MSG *PROTFG)
         (SETQ W
                 (ERRORSET
                  (LIST 'BOUNDSEVAL
                        (MKQUOTE
                         (LIST U (LIST 'EQUAL X (LIST '*INTERVAL* LOO HII)))))
                  NIL NIL)))
       NIL T)
      (SWITCH-MODE-RD OLDMODE)
      (COND
       ((AND (NOT (SMEMQ 'INT U)) (NOT (ERRORP W)))
        (PROGN
         (COND (*TRNUMERIC (PRIN2T "Using bounded antiderivative")))
         (SETQ OLDMODE (SWITCH-MODE-RD NIL))
         (SETQ U (SIMP U))
         (SETQ W
                 (PREPSQ
                  (ADDSQ (SUBSQ U (LIST (CONS X HI)))
                         (NEGSQ (SUBSQ U (LIST (CONS X LO)))))))
         (SWITCH-MODE-RD OLDMODE)
         (RETURN W)
         NIL)))
      (COND (*TRNUMERIC (PRIN2T "No bounded antiderivative found")))
      (SETQ W NIL)
      (SETQ OLDMODE (SWITCH-MODE-RD NIL))
      (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
      (SETQ CBOUND (|::QUOTIENT| ACC (|:DIFFERENCE| HII LOO)))
      (SETQ ORD 20)
     CHEBLOOP
      ((LAMBDA (*MSG *PROTFG)
         (SETQ U
                 (ERRORSET
                  (LIST 'CHEBCOEFFS (MKQUOTE FCN) (MKQUOTE X) (MKQUOTE LOO)
                        (MKQUOTE HII) (MKQUOTE ORD))
                  NIL NIL)))
       NIL T)
      (COND ((ERRORP U) (GO CHEBEXIT)) (T (SETQ U (CAR U))))
      (SETQ UU (REVERSE U))
      (COND
       ((INT-CHEBCONVERGES UU ACC CBOUND)
        (PROGN
         (SETQ U (CHEBINT U NIL LOO HII))
         (SETQ W
                 (REVAL1
                  (LIST 'DIFFERENCE (CHEBEVAL U NIL LOO HII HI)
                        (CHEBEVAL U NIL LOO HII LO))
                  NIL))
         NIL)))
      (COND
       ((AND (NULL W) (LESSP ORD 60))
        (PROGN (SETQ ORD (PLUS ORD 20)) (GO CHEBLOOP))))
     CHEBEXIT
      (SWITCH-MODE-RD OLDMODE)
      (COND
       (W
        (PROGN
         (COND
          (*TRNUMERIC
           (PROGN
            (PRIN2 "Using Chebyshev approximation of order ")
            (PRIN2T ORD))))
         (RETURN W)
         NIL)))
      (COND
       (*TRNUMERIC
        (PROGN
         (PRIN2T "No usable Chebyshev approximation found")
         (PRIN2T "Starting adaptive multilevel quadrature")
         NIL)))
      (RETURN (INTRD2 FCN (LIST (CONS X (CONS LO HI))) P)))) 
(PUT 'INT-CHEBCONVERGES 'NUMBER-OF-ARGS 3) 
(PUT 'INT-CHEBCONVERGES 'DEFINED-ON-LINE '217) 
(PUT 'INT-CHEBCONVERGES 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INT-CHEBCONVERGES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INT-CHEBCONVERGES (U ACC AB)
    (PROG (MX)
      (SETQ MX (INT-CHEBMAX U NIL))
      (SETQ MX (|:TIMESN| MX ACC))
      (RETURN
       (AND ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CAR U)) MX)
            ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CADR U)) MX)
            ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CADDR U))
             MX)
            ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CAR U)) AB)
            ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CADR U)) AB)
            ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF (CADDR U))
             AB))))) 
(PUT 'INT-CHEBMAX 'NUMBER-OF-ARGS 2) 
(PUT 'INT-CHEBMAX 'DEFINED-ON-LINE '230) 
(PUT 'INT-CHEBMAX 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INT-CHEBMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INT-CHEBMAX (U MX)
    (COND ((NULL U) MX)
          (T
           ((LAMBDA (W)
              (INT-CHEBMAX (CDR U)
               (COND (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) W MX) W)
                     (T MX))))
            (ABSF (CAR U)))))) 
(PUT 'INTRD2 'NUMBER-OF-ARGS 3) 
(PUT 'INTRD2 'DEFINED-ON-LINE '238) 
(PUT 'INTRD2 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRD2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRD2 (E VARS P)
    (PROG (ACC R OLDMODE CALLEE *NOEQUIV)
      (SETQ VARS NIL)
      (SETQ OLDMODE (SWITCH-MODE-RD NIL))
      (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
      (SETQ E (REVAL1 E T))
      (SETQ CALLEE (COND ((NULL (CDR P)) 'INTRDUNI) (T 'INTRDMULTI)))
      (SETQ R
              (ERRORSET (LIST CALLEE (MKQUOTE E) (MKQUOTE P) (MKQUOTE ACC)) NIL
                        NIL))
      (SWITCH-MODE-RD OLDMODE)
      (COND ((ERRORP R) (REDERR "Cannot numerically evaluate integral"))
            (T (RETURN (CAR R)))))) 
(PUT 'INTEVALUATE1 'NUMBER-OF-ARGS 3) 
(PUT 'INTEVALUATE1 'DEFINED-ON-LINE '251) 
(PUT 'INTEVALUATE1 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTEVALUATE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEVALUATE1 (E X V)
    ((LAMBDA (A)
       (COND ((ATOM A) (PROGN (SETQ SINGULARITIES* (CONS V SINGULARITIES*)) 0))
             (T (CAR A))))
     (EVALUATE* E (LIST X) (LIST V)))) 
(PUT 'INTEVALUATE 'NUMBER-OF-ARGS 3) 
(PUT 'INTEVALUATE 'DEFINED-ON-LINE '255) 
(PUT 'INTEVALUATE 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTEVALUATE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEVALUATE (E X V)
    ((LAMBDA (A)
       (COND ((ATOM A) (PROGN (SETQ SINGULARITIES* (CONS V SINGULARITIES*)) 0))
             (T (CAR A))))
     (EVALUATE* E X V))) 
(PUT 'INTRDUNI 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDUNI 'DEFINED-ON-LINE '259) 
(PUT 'INTRDUNI 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDUNI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDUNI (E P ACC)
    (PROG (LO HI X U EPS I IL INT N K)
      (SETQ N 0)
      (SETQ K 0)
      (SETQ X (CAR P))
      (SETQ P (CDR P))
      (SETQ LO (CADR X))
      (SETQ HI (CDDR X))
      (SETQ X (CAR X))
      (SETQ LO (FORCE-TO-DM LO))
      (SETQ HI (FORCE-TO-DM HI))
      (COND ((EQUAL HI LO) (RETURN (FORCE-TO-DM NIL))))
      (SETQ IL
              (LIST
               (INTRDMKINTERVAL E X (CONS LO (INTEVALUATE1 E X LO))
                (CONS HI (INTEVALUATE1 E X HI)) 1)))
      (SETQ INT (CAR (LASTPAIR (CAR IL))))
     LOOP
      (SETQ K (ADD1 K))
      (SETQ N (ADD1 N))
      (COND ((EQUAL (REMAINDER N 25) 0) (INTRDMESS N IL)))
      (SETQ I (CAR IL))
      (SETQ IL (CDR IL))
      (SETQ U (INTRDINTERSECT E X I))
      (COND
       ((NULL U)
        (PROGN
         (SETQ IL (CONS I IL))
         (INTRDABORTMSG (LIST (CAR (CADR I))) (LIST X) (INTCOLLECTEPS IL))
         (GO READY))))
      (PROG (Q)
        (SETQ Q U)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q) (SETQ IL (INTRDSORTIN Q IL))) (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (COND (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) K 5) (GO LOOP)))
      (SETQ INT (INTCOLLECTINT IL))
      (SETQ K 0)
      (SETQ EPS (INTCOLLECTEPS IL))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) EPS
         (|:TIMESN| ACC (ABSF INT)))
        (GO LOOP)))
     READY
      (RETURN (INTCOLLECTINT IL)))) 
(PUT 'INTRDABORTMSG 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDABORTMSG 'DEFINED-ON-LINE '295) 
(PUT 'INTRDABORTMSG 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDABORTMSG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDABORTMSG (PT VARS EPS)
    (PROGN
     (WRITEPRI "requested accuracy cannot be reached within iteration limit"
               'ONLY)
     (WRITEPRI "critical area of function near to " 'FIRST)
     (WRITEPRI
      (MKQUOTE
       (CONS 'LIST
             (PROG (U FORALL-RESULT FORALL-ENDPTR)
               (SETQ U (PAIR VARS PT))
               (COND ((NULL U) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (U)
                                   (LIST 'EQUAL (CAR U) (PREPF (CDR U))))
                                 (CAR U))
                                NIL)))
              LOOPLABEL
               (SETQ U (CDR U))
               (COND ((NULL U) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (U) (LIST 'EQUAL (CAR U) (PREPF (CDR U))))
                         (CAR U))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      'LAST)
     (WRITEPRI "current error estimate: " 'FIRST)
     (WRITEPRI (MKQUOTE (PREPF EPS)) 'LAST)
     NIL)) 
(PUT 'INTCOLLECTINT 'NUMBER-OF-ARGS 1) 
(PUT 'INTCOLLECTINT 'DEFINED-ON-LINE '305) 
(PUT 'INTCOLLECTINT 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTCOLLECTINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTCOLLECTINT (IL)
    ((LAMBDA (R)
       (PROGN
        (PROG (I)
          (SETQ I (CDR IL))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I) (SETQ R (|:PLUSN| (CAR (LASTPAIR I)) R))) (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        R))
     (CAR (LASTPAIR (CAR IL))))) 
(PUT 'INTCOLLECTEPS 'NUMBER-OF-ARGS 1) 
(PUT 'INTCOLLECTEPS 'DEFINED-ON-LINE '309) 
(PUT 'INTCOLLECTEPS 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTCOLLECTEPS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTCOLLECTEPS (IL)
    ((LAMBDA (R)
       (PROGN
        (PROG (I)
          (SETQ I (CDR IL))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I) (SETQ R (|:PLUSN| (CAR I) R))) (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        R))
     (CAR (CAR IL)))) 
(PUT 'INTRDSORTIN 'NUMBER-OF-ARGS 2) 
(PUT 'INTRDSORTIN 'DEFINED-ON-LINE '313) 
(PUT 'INTRDSORTIN 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDSORTIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTRDSORTIN (U L)
    (COND ((NULL L) (LIST U))
          (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (CAR U) (CAAR L))
           (CONS (CAR L) (INTRDSORTIN U (CDR L))))
          (T (CONS U L)))) 
(PUT 'INTRDINTERSECT 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDINTERSECT 'DEFINED-ON-LINE '319) 
(PUT 'INTRDINTERSECT 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDINTERSECT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDINTERSECT (E X I)
    (PROG (D PLO PMI PHI LEV)
      (SETQ I (CDR I))
      (SETQ PLO (CAR I))
      (SETQ I (CDR I))
      (SETQ PMI (CAR I))
      (SETQ I (CDR I))
      (SETQ PHI (CAR I))
      (SETQ I (CDR I))
      (SETQ D (CAR I))
      (SETQ D (|::QUOTIENT| D 2))
      (SETQ LEV (PLUS (CADR I) 1))
      (COND ((GREATERP LEV ITERATIONS*) (RETURN NIL)))
      (RETURN
       (LIST (INTRDMKINTERVAL E X PLO PMI LEV)
             (INTRDMKINTERVAL E X PMI PHI LEV))))) 
(PUT 'INTRDMKINTERVAL 'NUMBER-OF-ARGS 5) 
(PUT 'INTRDMKINTERVAL 'DEFINED-ON-LINE '332) 
(PUT 'INTRDMKINTERVAL 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDMKINTERVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDMKINTERVAL (E X LO HI LEV)
    (PROG (EPS IT IS MID MI FLO FHI FMID D U)
      (SETQ D (|:DIFFERENCE| (CAR HI) (CAR LO)))
      (SETQ MID (|::QUOTIENT| (|:PLUSN| (CAR HI) (CAR LO)) 2))
      (SETQ FLO (CDR LO))
      (SETQ FHI (CDR HI))
      (SETQ FMID (INTEVALUATE1 E X MID))
      (SETQ MI (CONS MID FMID))
      (COND
       ((SETQ U (INTRDUNISINGULAR LO MI HI))
        (PROGN (SETQ FLO (CAR U)) (SETQ FMID (CADR U)) (SETQ FHI (CADDR U)))))
      (SETQ IT
              (|:TIMESN| D
                         (|::QUOTIENT| (|:PLUSN| FLO (|:TIMESN| 2 FMID) FHI)
                          4)))
      (SETQ IS
              (|:TIMESN| D
                         (|::QUOTIENT|
                          (|:PLUSN| (|:TIMESN| 4 FLO) (|:TIMESN| 16 FMID)
                                    (|:TIMESN| 4 FHI))
                          24)))
      (SETQ EPS (ABSF (|:DIFFERENCE| IS IT)))
      (RETURN (LIST EPS LO MI HI D LEV IS)))) 
(PUT 'INTRDUNISINGULAR 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDUNISINGULAR 'DEFINED-ON-LINE '350) 
(PUT 'INTRDUNISINGULAR 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDUNISINGULAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDUNISINGULAR (LO MI HI)
    (COND
     (SINGULARITIES*
      (PROG (SLO SMI SHI FL FM FH U)
        (SETQ SLO (MEMBER (CAR LO) SINGULARITIES*))
        (SETQ SMI (MEMBER (CAR MI) SINGULARITIES*))
        (SETQ SHI (MEMBER (CAR HI) SINGULARITIES*))
        (COND ((AND (NOT SLO) (NOT SMI) (NOT SHI)) (RETURN NIL)))
        (COND ((AND SLO SMI SHI) (REDERR "too many singularities")))
        (SETQ FL (CDR LO))
        (SETQ FM (CDR MI))
        (SETQ FH (CDR HI))
        (SETQ U (|:TIMESN| 2 (|:PLUSN| FL FM FH)))
        (RETURN
         (LIST (COND (SLO U) (T FL)) (COND (SMI U) (T FM))
               (COND (SHI U) (T FH)))))))) 
(PUT 'INTRDMULTI 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDMULTI 'DEFINED-ON-LINE '366) 
(PUT 'INTRDMULTI 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDMULTI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDMULTI (E P ACC)
    (PROG (VARS U EPS I IL INT N K DIM)
      (SETQ N 0)
      (SETQ K 0)
      (SETQ DIM 0)
      (COND
       ((SMEMQ 'INFINITY P)
        (REDERR "no support for infinity in multivariate integrals")))
      (SETQ DIM (LENGTH P))
      (SETQ IL (INTRDMKINITCUBE E P))
      (SETQ VARS (CAR IL))
      (SETQ IL (CDR IL))
     LOOP
      (SETQ K (ADD1 K))
      (SETQ N (ADD1 N))
      (COND ((EQUAL (REMAINDER N 25) 0) (INTRDMESS N IL)))
      (SETQ I (CAR IL))
      (SETQ IL (CDR IL))
      (SETQ U (INTRDREFINECUBE E VARS I))
      (COND
       ((NULL U)
        (PROGN
         (SETQ IL (CONS I IL))
         (INTRDABORTMSG (CAR (CADR I)) VARS (INTCOLLECTEPS IL))
         (GO READY))))
      (PROG (Q)
        (SETQ Q U)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q) (SETQ IL (INTRDSORTIN Q IL))) (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (COND (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) K 5) (GO LOOP)))
      (SETQ INT (INTCOLLECTINT IL))
      (SETQ K 0)
      (SETQ EPS (INTCOLLECTEPS IL))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) EPS
         (|:TIMESN| ACC (ABSF INT)))
        (GO LOOP)))
     READY
      (RETURN (INTCOLLECTINT IL)))) 
(PUT 'INTRDMKINITCUBE 'NUMBER-OF-ARGS 2) 
(PUT 'INTRDMKINITCUBE 'DEFINED-ON-LINE '397) 
(PUT 'INTRDMKINITCUBE 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDMKINITCUBE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTRDMKINITCUBE (E P)
    (PROG (VOL VARS IL LO HI X Y)
      (SETQ VOL 1)
      (PROG (Z)
        (SETQ Z P)
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (PROGN
            (SETQ VARS (CONS (CAR Z) VARS))
            (SETQ X (FORCE-TO-DM (CADR Z)))
            (SETQ Y (FORCE-TO-DM (CDDR Z)))
            (SETQ LO (CONS X LO))
            (SETQ HI (CONS Y HI))
            (SETQ VOL (|:TIMESN| (ABSF (|:DIFFERENCE| X Y)) VOL))
            NIL))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (SETQ LO (CONS LO (INTEVALUATE E VARS LO)))
      (SETQ HI (CONS HI (INTEVALUATE E VARS HI)))
      (SETQ IL (LIST (INTRDMKCUBE E VARS LO HI VOL 1)))
      (RETURN (CONS VARS IL)))) 
(PUT 'INTRDREFINECUBE 'NUMBER-OF-ARGS 3) 
(PUT 'INTRDREFINECUBE 'DEFINED-ON-LINE '414) 
(PUT 'INTRDREFINECUBE 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDREFINECUBE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDREFINECUBE (E VARS I)
    (PROG (PLO PHI LO HI NLO NHI VOL X Y XNEW M LEV)
      (SETQ M 0)
      (SETQ LEV 0)
      (SETQ I (CDR I))
      (SETQ PLO (CAR I))
      (SETQ I (CDR I))
      (SETQ PHI (CAR I))
      (SETQ I (CDR I))
      (SETQ VOL (|::QUOTIENT| (CAR I) 2))
      (SETQ LEV (ADD1 (CADR I)))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) LEV ITERATIONS*)
        (RETURN NIL)))
      (SETQ LO (CAR PLO))
      (SETQ HI (CAR PHI))
      (SETQ M 1)
      (SETQ X (|:DIFFERENCE| (CAR HI) (CAR LO)))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((|:MINUSP| (|:DIFFERENCE| (LENGTH LO) J)) (RETURN NIL)))
        (COND
         (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) X
           (SETQ Y (|:DIFFERENCE| (NTH HI J) (NTH LO J))))
          (PROGN (SETQ M J) (SETQ X Y))))
        (SETQ J (|:PLUS| J 1))
        (GO LAB))
      (SETQ NLO (APPEND LO NIL))
      (SETQ NHI (APPEND HI NIL))
      (SETQ XNEW (|::QUOTIENT| (|:PLUSN| (NTH HI M) (NTH LO M)) 2))
      (SETCAR (PNTH NLO M) XNEW)
      (SETCAR (PNTH NHI M) XNEW)
      (SETQ NLO (CONS NLO (INTEVALUATE E VARS NLO)))
      (SETQ NHI (CONS NHI (INTEVALUATE E VARS NHI)))
      (RETURN
       (LIST (INTRDMKCUBE E VARS PLO NHI VOL LEV)
             (INTRDMKCUBE E VARS NLO PHI VOL LEV))))) 
(PUT 'INTRDMKCUBE 'NUMBER-OF-ARGS 6) 
(PUT 'INTRDMKCUBE 'DEFINED-ON-LINE '437) 
(PUT 'INTRDMKCUBE 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDMKCUBE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTRDMKCUBE (E VARS LO HI VOL LEV)
    (PROG (U EPS IT IS MID FMI FLO FHI)
      (SETQ FLO (CDR LO))
      (SETQ FHI (CDR HI))
      (SETQ MID (LIST+LIST (CAR LO) (CAR HI)))
      (SETQ MID (SCAL*LIST (|::QUOTIENT| 1 2) MID))
      (SETQ FMI (INTEVALUATE E VARS MID))
      (COND
       ((SETQ U (INTRDUNISINGULAR LO (CONS MID FMI) HI))
        (PROGN (SETQ FLO (CAR U)) (SETQ FMI (CADR U)) (SETQ FHI (CADDR U)))))
      (SETQ IS (|:PLUSN| FLO FHI))
      (SETQ IT (|:TIMESN| IS (|::QUOTIENT| VOL 2)))
      (SETQ IS
              (|:TIMESN| (|:PLUSN| (|:TIMESN| 2 FMI) IS) (|::QUOTIENT| VOL 4)))
      (SETQ EPS (ABSF (|:DIFFERENCE| IS IT)))
      (RETURN (LIST EPS LO HI VOL LEV IS)))) 
(PUT 'INTRDMESS 'NUMBER-OF-ARGS 2) 
(PUT 'INTRDMESS 'DEFINED-ON-LINE '451) 
(PUT 'INTRDMESS 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'INTRDMESS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTRDMESS (N IL)
    (COND
     (*TRNUMERIC
      (PROGN
       (WRITEPRI (TIMES 2 N) 'FIRST)
       (WRITEPRI " intervals, integral=" NIL)
       (WRITEPRI (MKQUOTE (PREPF (INTCOLLECTINT IL))) NIL)
       (WRITEPRI ", error estimate=" NIL)
       (WRITEPRI (MKQUOTE (PREPF (INTCOLLECTEPS IL))) NIL)
       NIL)))) 
(PUT 'PRINXT 'NUMBER-OF-ARGS 1) 
(PUT 'PRINXT 'DEFINED-ON-LINE '459) 
(PUT 'PRINXT 'DEFINED-IN-FILE 'NUMERIC/NUMINT.RED) 
(PUT 'PRINXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINXT (L)
    (PROG (I)
      (SETQ I 0)
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((NOT (EQCAR X '|:RD:|)) (PRIN2 X)) ((ATOM (CDR X)) (PRIN2 X))
                 (T
                  (PROGN
                   (PRIN2 (TIMES (FLOAT (CADR X)) (EXPT 10.0 (CDDR X))))
                   (TAB (SETQ I (PLUS 20 I)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI))) 
(ENDMODULE) 