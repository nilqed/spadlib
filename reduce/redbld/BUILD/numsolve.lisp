(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NUMSOLVE)) 
(FLUID '(*NOEQUIV ACCURACY* *EXPTEXPAND)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC ERFG*)) 
(PUT 'RDSOLVEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RDSOLVEEVAL 'DEFINED-ON-LINE '37) 
(PUT 'RDSOLVEEVAL 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'RDSOLVEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDSOLVEEVAL (U)
    (PROG (E VARS Y Z P R ERFG MODE ALL *EXPTEXPAND N)
      (SETQ N 0)
      (SETQ ERFG ERFG*)
      (SETQ ERFG* NIL)
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
      (SETQ U (ACCURACYCONTROL U 6 50))
      (COND ((SETQ ALL (MEMQ 'ALL U)) (SETQ U (DELETE 'ALL U))))
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (SETQ E (COND ((EQCAR E 'LIST) (CDR E)) (T (LIST E))))
      (SETQ E
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F E)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (COND
                                     ((EQEXPR (SETQ F (REVAL1 F T)))
                                      (*EQN2A F))
                                     (T F)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (COND ((EQEXPR (SETQ F (REVAL1 F T))) (*EQN2A F))
                                  (T F)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N (LENGTH E))
      (COND
       ((EQCAR (CAR U) 'LIST)
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
            (COND
             ((EQCAR X 'EQUAL)
              (PROGN
               (SETQ Z (CADR X))
               (SETQ Y (CADDR X))
               (COND ((EQCAR Y '*INTERVAL*) (SETQ MODE 'I)))
               NIL))
             (T (PROGN (SETQ Z X) (SETQ Y (RANDOM 100)))))
            (SETQ VARS (CONS Z VARS))
            (SETQ P (CONS Y P))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VARS (REVERSIP VARS))
      (SETQ P (REVERSIP P))
      (COND
       ((NOT (EQUAL N (LENGTH VARS)))
        (PROGN (REDERR "numbers of variables and functions don't match"))))
      (COND
       ((OR (AND (EQUAL MODE 'I) (GREATERP (LENGTH VARS) 1))
            (AND (NEQ MODE 'I) ALL))
        (REDERR "mode for num_solve not implemented")))
      (SETQ R
              (COND ((EQUAL MODE 'I) (RD-SOLVE-INTERV E VARS P N ALL))
                    (T (RDNEWTON0 E VARS P N))))
      (SETQ ERFG* ERFG)
      (RETURN R))) 
(PUT 'NUM_SOLVE 'PSOPFN 'RDSOLVEEVAL) 
(PUT 'RD-SOLVE-INTERV 'NUMBER-OF-ARGS 5) 
(PUT 'RD-SOLVE-INTERV 'DEFINED-ON-LINE '86) 
(PUT 'RD-SOLVE-INTERV 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'RD-SOLVE-INTERV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RD-SOLVE-INTERV (E VARS P N ALL)
    ((LAMBDA (*ROUNDBF)
       (PROG (U FU L FL X ACC R DE W OLDMODE)
         (SETQ N NIL)
         (COND
          ((GREATERP (LENGTH P) 1) (TYPERR (CONS 'LIST P) "single interval")))
         (SETQ P (CAR P))
         (SETQ E (CAR E))
         (SETQ X (CAR VARS))
         (SETQ W (LIST 'BOUNDSEVAL (MKQUOTE (LIST E (LIST 'EQUAL X P)))))
         (COND
          ((NOT (MEMQ DMODE* '(|:RD:| |:CR:|)))
           (PROGN (SETQ OLDMODE T) (SETDMODE 'ROUNDED T))))
         (COND ((ERRORP (ERRORSET W NIL NIL)) (TYPERR E "bounded expression")))
         (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
         (SETQ L (EVALUATE (CADR P) NIL NIL))
         (SETQ U (EVALUATE (CADDR P) NIL NIL))
         (SETQ FL (EVALUATEUNI E X L))
         (SETQ FU (EVALUATEUNI E X U))
         (COND
          ((NOT ALL)
           (COND ((|:ZEROP| FL) (RETURN (PREPF L)))
                 ((|:ZEROP| FU) (RETURN (PREPF U)))
                 (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
                   (|:TIMESN| FL FU) 0)
                  (SETQ R (REGULA-FALSI E X L FL U FU ACC))))))
         (COND ((NULL R) (SETQ DE (REVAL1 (LIST 'DF E X) T))))
         (COND
          ((AND (NULL R) (NOT ALL))
           (SETQ R (RD-SOLVE-TRYNEWTON E DE X L FL U FU ACC))))
         (COND
          ((NULL R) (SETQ R (RD-SOLVE-HARDCASE E DE X L FL U FU ACC ALL))))
         (COND (OLDMODE (SETDMODE 'ROUNDED NIL)))
         (COND (R (RETURN R)))
         (REDERR "not applicable")))
     *ROUNDBF)) 
(PUT 'MAKE-RDSOLVE-RESULT1 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-RDSOLVE-RESULT1 'DEFINED-ON-LINE '114) 
(PUT 'MAKE-RDSOLVE-RESULT1 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'MAKE-RDSOLVE-RESULT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-RDSOLVE-RESULT1 (X U)
    (CONS 'LIST
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R U)
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (R) (LIST 'EQUAL X (PREPF R))) (CAR R))
                             NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (R) (LIST 'EQUAL X (PREPF R))) (CAR R))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'RD-SOLVE-TRYNEWTON 'NUMBER-OF-ARGS 8) 
(PUT 'RD-SOLVE-TRYNEWTON 'DEFINED-ON-LINE '117) 
(PUT 'RD-SOLVE-TRYNEWTON 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'RD-SOLVE-TRYNEWTON 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE RD-SOLVE-TRYNEWTON (E DE X L FL U FU ACC)
    (PROG (R INVDE)
      (SETQ INVDE (LIST 'QUOTIENT 1 DE))
      (PROGN
       (COND
        (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
          (|:TIMESN| FU (EVALUATEUNI DE X U)) 0)
         (SETQ R
                 (RDNEWTON2 (LIST E) (LIST (LIST INVDE)) (LIST X) ACC (LIST U)
                  'ROOT L U))))
       (COND
        ((AND (NULL R)
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
               (|:TIMESN| FL (EVALUATEUNI DE X L)) 0))
         (SETQ R
                 (RDNEWTON2 (LIST E) (LIST (LIST INVDE)) (LIST X) ACC (LIST L)
                  'ROOT L U))))
       (COND
        ((AND R (SETQ R (CAR R))
              ((LAMBDA (A B) (NOT (|:MINUSP| (|:DIFFERENCE| B A)))) L R)
              ((LAMBDA (A B) (NOT (|:MINUSP| (|:DIFFERENCE| B A)))) R U))
         (SETQ R (MAKE-RDSOLVE-RESULT1 X (LIST R))))
        (T (SETQ R NIL)))
       NIL)
      (RETURN R))) 
(PUT 'REGULA-FALSI 'NUMBER-OF-ARGS 7) 
(PUT 'REGULA-FALSI 'DEFINED-ON-LINE '131) 
(PUT 'REGULA-FALSI 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'REGULA-FALSI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE REGULA-FALSI (E X L FL U FU ACC)
    (MAKE-RDSOLVE-RESULT1 X (LIST (REGULA-FALSI1 E X L FL U FU ACC 0 1)))) 
(PUT 'REGULA-FALSI1 'NUMBER-OF-ARGS 9) 
(PUT 'REGULA-FALSI1 'DEFINED-ON-LINE '135) 
(PUT 'REGULA-FALSI1 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'REGULA-FALSI1 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE REGULA-FALSI1 (E X L FL U FU ACC MODE1 MODE2)
    (PROG (M FM)
      (COND ((EQUAL MODE1 MODE2) (SETQ M (|::QUOTIENT| (|:PLUSN| U L) 2)))
            (T
             (SETQ M
                     (|:PLUSN|
                      (|:TIMESN| L (|::QUOTIENT| FU (|:DIFFERENCE| FU FL)))
                      (|:TIMESN| U (|::QUOTIENT| FL (|:DIFFERENCE| FL FU)))))))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (|:DIFFERENCE| U L)
         ACC)
        (RETURN M)))
      (SETQ FM (EVALUATEUNI E X M))
      (COND ((|:ZEROP| FM) (RETURN M)))
      (RETURN
       (COND
        (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (|:TIMESN| FL FM) 0)
         (REGULA-FALSI1 E X L FL M FM ACC NIL MODE1))
        (T (REGULA-FALSI1 E X M FM U FU ACC T MODE1)))))) 
(PUT 'MKMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'MKMINUS 'DEFINED-ON-LINE '149) 
(PUT 'MKMINUS 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'MKMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKMINUS (U) (LIST 'MINUS U)) 
(PUT 'EVALUATEUNI 'NUMBER-OF-ARGS 3) 
(PUT 'EVALUATEUNI 'DEFINED-ON-LINE '151) 
(PUT 'EVALUATEUNI 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'EVALUATEUNI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EVALUATEUNI (E X P) (EVALUATE E (LIST X) (LIST P))) 
(PUT 'DMBOUNDSUNI 'NUMBER-OF-ARGS 4) 
(PUT 'DMBOUNDSUNI 'DEFINED-ON-LINE '154) 
(PUT 'DMBOUNDSUNI 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'DMBOUNDSUNI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DMBOUNDSUNI (E X L U)
    (PROG (I)
      (SETQ I
              (BOUNDSEVAL
               (LIST E
                     (LIST 'EQUAL X (LIST '*INTERVAL* (PREPF L) (PREPF U))))))
      (RETURN (CONS (CAR (SIMP (CADR I))) (CAR (SIMP (CADDR I))))))) 
(PUT 'RD-SOLVE-HARDCASE 'NUMBER-OF-ARGS 9) 
(PUT 'RD-SOLVE-HARDCASE 'DEFINED-ON-LINE '160) 
(PUT 'RD-SOLVE-HARDCASE 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'RD-SOLVE-HARDCASE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE RD-SOLVE-HARDCASE (E DE X L FL U FU ACC ALL)
    (PROG (IL1 IL2 PT IV R RR B B1 Q D LEV)
      (SETQ LEV 0)
      (SETQ IL1 (LIST (CONS (CONS L FL) (CONS U FU))))
     MAIN_LOOP
      (SETQ LEV (|:PLUSN| LEV 1))
      (SETQ IL2 NIL)
      (COND ((NULL IL1) (GO DONE)))
      (SETQ IL1 (REVERSE IL1))
      (RD-SOLVE-HARDCASE-PRINT LEV IL1)
     LOOP
      (COND ((NULL IL1) (GO BOTTOM)))
      (SETQ IV (CAR IL1))
      (SETQ IL1 (CDR IL1))
      (SETQ L (CAAR IV))
      (SETQ FL (CDAR IV))
      (SETQ U (CADR IV))
      (SETQ FU (CDDR IV))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
         (SETQ D (|:DIFFERENCE| U L)) 0)
        (GO LOOP)))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF FL) ACC)
        (PROGN (SETQ PT L) (GO ROOT_FOUND))))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF FU) ACC)
        (PROGN (SETQ PT U) (GO ROOT_FOUND))))
      (SETQ B (DMBOUNDSUNI DE X L U))
      (COND
       ((OR
         (AND ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) FL 0)
              (NOT
               ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
                (SETQ B1 (CAR B)) 0)))
         (AND ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) FL 0)
              (NOT
               ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
                (SETQ B1 (CDR B)) 0)))
         (NOT
          ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
           (SETQ PT (|:DIFFERENCE| L (|::QUOTIENT| FL B1))) U)))
        (GO LOOP)))
      (COND ((EQUAL PT L) (GO PRECERR)))
      (SETQ Q (EVALUATEUNI E X PT))
      (COND
       ((AND (NOT ALL)
             ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (|:TIMESN| Q FL)
              0))
        (RETURN (REGULA-FALSI E X L FL PT Q ACC))))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF Q) ACC)
        (GO ROOT_FOUND)))
      (SETQ L PT)
      (SETQ FL Q)
      (COND
       ((OR
         (AND ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) FU 0)
              (NOT
               ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
                (SETQ B1 (CDR B)) 0)))
         (AND ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) FU 0)
              (NOT
               ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
                (SETQ B1 (CAR B)) 0)))
         (NOT
          ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
           (SETQ PT (|:DIFFERENCE| U (|::QUOTIENT| FU B1))) L)))
        (GO LOOP)))
      (COND ((EQUAL PT U) (GO PRECERR)))
      (SETQ Q (EVALUATEUNI E X PT))
      (COND
       ((AND (NOT ALL)
             ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (|:TIMESN| Q FU)
              0))
        (RETURN (REGULA-FALSI E X PT Q U FU ACC))))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF Q) ACC)
        (GO ROOT_FOUND)))
      (SETQ U PT)
      (SETQ FU Q)
      (SETQ PT (|::QUOTIENT| (|:PLUSN| L U) 2))
      (SETQ Q (EVALUATEUNI E X PT))
      (COND
       ((AND (NOT ALL)
             ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (|:TIMESN| Q FU)
              0))
        (RETURN (REGULA-FALSI E X L FL PT Q ACC))))
      (COND
       ((NOT ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF Q) ACC))
        (PROGN
         (SETQ IL2
                 (FIND-ROOT-ADDINTERVAL (CONS PT Q) (CONS U FU)
                  (FIND-ROOT-ADDINTERVAL (CONS L FL) (CONS PT Q) IL2)))
         (GO LOOP)
         NIL)))
     ROOT_FOUND
      (SETQ R (CONS PT R))
      (COND ((NOT ALL) (GO DONE)))
      (SETQ RR (FIND-ROOT-RANGE E X PT ACC))
      (SETQ IL2
              (FIND-ROOT-ADDINTERVAL (CDR RR) (CONS U FU)
               (FIND-ROOT-ADDINTERVAL (CONS L FL) (CAR RR) IL2)))
      (GO LOOP)
     BOTTOM
      (SETQ IL1 IL2)
      (GO MAIN_LOOP)
     DONE
      (SETQ R
              (SORT R
                    (FUNCTION (LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))))))
      (RETURN (MAKE-RDSOLVE-RESULT1 X R))
     PRECERR
      (REDERR "precision not sufficient for finding all roots"))) 
(PUT 'RD-SOLVE-HARDCASE-PRINT 'NUMBER-OF-ARGS 2) 
(PUT 'RD-SOLVE-HARDCASE-PRINT 'DEFINED-ON-LINE '221) 
(PUT 'RD-SOLVE-HARDCASE-PRINT 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'RD-SOLVE-HARDCASE-PRINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RD-SOLVE-HARDCASE-PRINT (LEV IL1)
    (COND
     (*TRNUMERIC
      (PROGN
       (PRIN2T
        (LIST "recursion level:" LEV "remaining intervals:" (LENGTH IL1)))
       (WRITEPRI
        (MKQUOTE
         (CONS 'LIST
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I IL1)
                 (COND ((NULL I) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (I)
                                     (LIST 'LIST
                                           (LIST '*INTERVAL* (PREPF (CAAR I))
                                                 (PREPF (CADR I)))
                                           (PREPF
                                            (|:DIFFERENCE| (CADR I)
                                                           (CAAR I)))))
                                   (CAR I))
                                  NIL)))
                LOOPLABEL
                 (SETQ I (CDR I))
                 (COND ((NULL I) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (I)
                             (LIST 'LIST
                                   (LIST '*INTERVAL* (PREPF (CAAR I))
                                         (PREPF (CADR I)))
                                   (PREPF (|:DIFFERENCE| (CADR I) (CAAR I)))))
                           (CAR I))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        'ONLY)
       NIL)))) 
(PUT 'FIND-ROOT-ADDINTERVAL 'NUMBER-OF-ARGS 3) 
(PUT 'FIND-ROOT-ADDINTERVAL 'DEFINED-ON-LINE '231) 
(PUT 'FIND-ROOT-ADDINTERVAL 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'FIND-ROOT-ADDINTERVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-ROOT-ADDINTERVAL (L H IL)
    (PROGN
     (COND
      (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (CAR L) (CAR H))
       (SETQ IL (CONS (CONS L H) IL))))
     IL)) 
(PUT 'FIND-ROOT-RANGE 'NUMBER-OF-ARGS 4) 
(PUT 'FIND-ROOT-RANGE 'DEFINED-ON-LINE '234) 
(PUT 'FIND-ROOT-RANGE 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED) 
(PUT 'FIND-ROOT-RANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-ROOT-RANGE (E X P ACC)
    ((LAMBDA (PL PU FL FU)
       (PROGN
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (NOT
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
               (ABSF
                (SETQ FL
                        (EVALUATEUNI E X
                         (SETQ PL (|:DIFFERENCE| PL (|::QUOTIENT| ACC 2))))))
               ACC)))
            (RETURN NIL)))
         NIL
          (GO WHILELABEL))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (NOT
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
               (ABSF
                (SETQ FU
                        (EVALUATEUNI E X
                         (SETQ PU (|:PLUSN| PU (|::QUOTIENT| ACC 2))))))
               ACC)))
            (RETURN NIL)))
         NIL
          (GO WHILELABEL))
        (CONS (CONS PL FL) (CONS PU FU))))
     P P NIL NIL)) 
(COND
 ((ERRORP (ERRORSET '(|:DIFFERENCE| NIL NIL) NIL NIL))
  (PROGN
   (PROGN
    (PUT '|:DIFFERENCE| 'NUMBER-OF-ARGS 2)
    (PUT '|:DIFFERENCE| 'DEFINED-ON-LINE '245)
    (PUT '|:DIFFERENCE| 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED)
    (PUT '|:DIFFERENCE| 'PROCEDURE_TYPE
         '(ARROW (TIMES GENERAL GENERAL) GENERAL))
    (DE |:DIFFERENCE| (U V)
        (COND ((NULL U) (|:MINUS| V)) ((NULL V) U) ((EQUAL U V) NIL)
              ((AND (ATOM U) (ATOM V)) (DIFFERENCE U V))
              (T (DCOMBINE U V 'DIFFERENCE)))))
   (PROGN
    (PUT '|:QUOTIENT| 'NUMBER-OF-ARGS 2)
    (PUT '|:QUOTIENT| 'DEFINED-ON-LINE '251)
    (PUT '|:QUOTIENT| 'DEFINED-IN-FILE 'NUMERIC/NUMSOLVE.RED)
    (PUT '|:QUOTIENT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL))
    (DE |:QUOTIENT| (U V)
        (COND ((OR (NULL U) (EQUAL U 0)) NIL)
              ((OR (NULL V) (EQUAL V 0)) (RERROR 'POLY 12 "Zero divisor"))
              ((AND (ATOM U) (ATOM V))
               (COND ((NULL DMODE*) (QUOTIENT U V))
                     (T (DCOMBINE U (|:RECIP| V) 'TIMES))))
              (T (DCOMBINE U V 'QUOTIENT)))))
   NIL))) 
(ENDMODULE) 