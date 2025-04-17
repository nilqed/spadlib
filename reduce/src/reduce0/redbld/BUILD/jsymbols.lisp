(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'JSYMBOLS)) 
(LOAD-PACKAGE 'MATRIX) 
(LOAD-PACKAGE 'SOLVE) 
(LOAD-PACKAGE 'INEQ) 
(FLAG '(THREEJSYMBOL SIXJSYMBOL CLEBSCH_GORDON) 'SPECFN) 
(DEFLIST '((THREEJSYMBOL 3) (CLEBSCH_GORDON 3) (SIXJSYMBOL 2)) 'NUMBER-OF-ARGS) 
(PUT 'CLEAN_UP_SQRT 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAN_UP_SQRT 'DEFINED-ON-LINE '55) 
(PUT 'CLEAN_UP_SQRT 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'CLEAN_UP_SQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAN_UP_SQRT (INPUT)
    (PROG (NUM DENOM ANSWER)
      (COND
       ((OR (NOT (PAIRP INPUT)) (NEQ (CAR INPUT) 'QUOTIENT))
        (SETQ ANSWER INPUT))
       (T
        (PROGN
         (SETQ NUM (CADR INPUT))
         (SETQ DENOM (CADDR INPUT))
         (SETQ NUM (COLLECT_SQRT NUM))
         (SETQ DENOM (COLLECT_SQRT DENOM))
         (SETQ ANSWER (LIST 'QUOTIENT NUM DENOM))
         NIL)))
      (RETURN ANSWER))) 
(FLAG '(CLEAN_UP_SQRT) 'OPFN) 
(PUT 'COLLECT_SQRT 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECT_SQRT 'DEFINED-ON-LINE '74) 
(PUT 'COLLECT_SQRT 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'COLLECT_SQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECT_SQRT (INPUT)
    (PROG (SQRT_ARGS EXTRA_BIT MINUST ANSWER)
      (SETQ SQRT_ARGS (LIST))
      (SETQ EXTRA_BIT (LIST))
      (COND ((NOT (PAIRP INPUT)) (SETQ ANSWER INPUT))
            (T
             (PROGN
              (COND
               ((EQUAL (CAR INPUT) 'MINUS)
                (PROGN
                 (SETQ MINUST T)
                 (COND ((PAIRP (CADR INPUT)) (SETQ INPUT (CADR INPUT)))
                       (T (RETURN INPUT))))))
              (COND ((EQUAL (CAR INPUT) 'TIMES) (SETQ INPUT (CDR INPUT)))
                    (T (SETQ INPUT (LIST INPUT))))
              (PROG (ELT)
                (SETQ ELT INPUT)
               LAB
                (COND ((NULL ELT) (RETURN NIL)))
                ((LAMBDA (ELT)
                   (PROGN
                    (COND
                     ((EQCAR ELT 'SQRT)
                      (SETQ SQRT_ARGS (APPEND SQRT_ARGS (LIST (CADR ELT)))))
                     (T (SETQ EXTRA_BIT (APPEND EXTRA_BIT (LIST ELT)))))
                    NIL))
                 (CAR ELT))
                (SETQ ELT (CDR ELT))
                (GO LAB))
              (COND
               ((EQUAL SQRT_ARGS (LIST)) (PROGN (SETQ ANSWER EXTRA_BIT) NIL))
               (T
                (PROGN
                 (SETQ SQRT_ARGS
                         (REVAL1
                          (APPEND (LIST 'SQRT_OF)
                                  (LIST (APPEND (LIST 'TIMES) SQRT_ARGS)))
                          T))
                 (SETQ ANSWER (APPEND (LIST SQRT_ARGS) EXTRA_BIT))
                 NIL)))
              (SETQ ANSWER (APPEND (LIST 'TIMES) ANSWER))
              (COND (MINUST (SETQ ANSWER (LIST 'MINUS ANSWER))))
              NIL)))
      (RETURN ANSWER))) 
(FLAG '(LISTP) 'OPFN) 
(AEVAL (OPERATOR (LIST 'SQRT_OF 'ODDEXPT))) 
(AEVAL (LET '((REPLACEBY (SQRT_OF (~ X)) (WHEN (SQRT X) (NUMBERP X)))))) 
(PUT 'THREEJSYMBOL 'NUMBER-OF-ARGS 3) 
(FLAG '(THREEJSYMBOL) 'OPFN) 
(PUT 'THREEJSYMBOL 'DEFINED-ON-LINE '112) 
(PUT 'THREEJSYMBOL 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'THREEJSYMBOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE THREEJSYMBOL (U1 U2 U3)
    (COND
     ((AND (BOOLVALUE* (REVALX (LIST 'LISTP U1)))
           (BOOLVALUE* (REVALX (LIST 'LISTP U2)))
           (BOOLVALUE* (REVALX (LIST 'LISTP U3))))
      (PROG (J1 J2 J3 M1 M2 M3 TMP LOWER UPPER BETTER BEST)
        (AEVAL (MATRIX (LIST (LIST 'RANGE 6 2))))
        (SETQ J1 (AEVAL (LIST 'FIRST U1)))
        (SETQ M1 (AEVAL (LIST 'SECOND U1)))
        (SETQ J2 (AEVAL (LIST 'FIRST U2)))
        (SETQ M2 (AEVAL (LIST 'SECOND U2)))
        (SETQ J3 (AEVAL (LIST 'FIRST U3)))
        (SETQ M3 (AEVAL (LIST 'SECOND U3)))
        (COND
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS M1 M2 M3))))
               (EVALNEQ (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 J2 J3))))
               (EVALLESSP (AEVAL TMP) (MINUS 1)))
          (RETURN 0))
         ((AND
           (EVALNUMBERP
            (SETQ TMP (AEVAL (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)))))
           (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND
           (EVALNUMBERP
            (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE J1 J2) J3))))
           (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND
           (EVALNUMBERP
            (SETQ TMP (AEVAL (LIST 'PLUS J2 (LIST 'DIFFERENCE J3 J1)))))
           (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 M1))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J1 M1))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J2 M2))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J2 M2))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J3 M3))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         ((AND (EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J3 M3))))
               (EVALLESSP (AEVAL TMP) 0))
          (RETURN 0))
         (T
          (PROGN
           (SETK 'RANGE
                 (AEVAL
                  (LIST 'MAT (LIST 0 'INFINITY) (LIST 0 'INFINITY)
                        (LIST 0 'INFINITY) (LIST 0 'INFINITY)
                        (LIST 0 'INFINITY) (LIST 0 'INFINITY))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 1 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 1 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J1 M1))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J2 M2))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J3 J2 M1))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J1) M2))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 1 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 1 2) (AEVAL UPPER))
           (COND
            ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN (SETQ BETTER (AEVAL UPPER)) (SETQ BEST (AEVAL 0))))
            (T
             (PROGN
              (SETQ BETTER
                      (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (SETQ BEST (AEVAL 1)))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 2 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 2 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J2 (LIST 'DIFFERENCE J1 J3)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J2 M2))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 M1))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J3 J1 M2))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J1 J2) M1))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 2 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 2 2) (AEVAL UPPER))
           (COND
            ((EVALNEQ (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN
              (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (COND
               ((OR (EVALEQUAL (AEVAL BETTER) (AEVAL 'INFINITY))
                    (EVALGREATERP (AEVAL BETTER) (AEVAL TMP)))
                (PROGN (SETQ BETTER (AEVAL TMP)) (SETQ BEST (AEVAL 2))))))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 3 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 3 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J3 (LIST 'DIFFERENCE J2 J1)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J3 M3))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J2 M2))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 J2 M3))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J3) M2))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 3 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 3 2) (AEVAL UPPER))
           (COND
            ((EVALNEQ (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN
              (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (COND
               ((OR (EVALEQUAL (AEVAL BETTER) (AEVAL 'INFINITY))
                    (EVALGREATERP (AEVAL BETTER) (AEVAL TMP)))
                (PROGN (SETQ BETTER (AEVAL TMP)) (SETQ BEST (AEVAL 3))))))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 4 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 4 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J1 (LIST 'DIFFERENCE J3 J2)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J1 M1))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J3 M3))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J2 J3 M1))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J3 J1) M3))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 4 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 4 2) (AEVAL UPPER))
           (COND
            ((EVALNEQ (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN
              (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (COND
               ((OR (EVALEQUAL (AEVAL BETTER) (AEVAL 'INFINITY))
                    (EVALGREATERP (AEVAL BETTER) (AEVAL TMP)))
                (PROGN (SETQ BETTER (AEVAL TMP)) (SETQ BEST (AEVAL 4))))))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 5 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 5 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J2 (LIST 'DIFFERENCE J3 J1)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J2 M2))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J3 M3))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 J3 M2))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J3 J2) M3))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 5 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 5 2) (AEVAL UPPER))
           (COND
            ((EVALNEQ (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN
              (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (COND
               ((OR (EVALEQUAL (AEVAL BETTER) (AEVAL 'INFINITY))
                    (EVALGREATERP (AEVAL BETTER) (AEVAL TMP)))
                (PROGN (SETQ BETTER (AEVAL TMP)) (SETQ BEST (AEVAL 5))))))))
           (SETQ LOWER (AEVAL (LIST 'RANGE 6 1)))
           (SETQ UPPER (AEVAL (LIST 'RANGE 6 2)))
           (COND
            ((EVALNUMBERP
              (SETQ TMP (AEVAL (LIST 'PLUS J3 (LIST 'DIFFERENCE J1 J2)))))
             (SETQ UPPER (AEVAL TMP))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'DIFFERENCE J3 M3))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J1 M1))))
             (COND
              ((EVALEQUAL (AEVAL UPPER) (AEVAL 'INFINITY))
               (SETQ UPPER (AEVAL TMP)))
              (T (SETQ UPPER (AEVAL (LIST 'MIN UPPER TMP)))))))
           (COND
            ((EVALNUMBERP (SETQ TMP (AEVAL (LIST 'PLUS J2 J1 M3))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (COND
            ((EVALNUMBERP
              (SETQ TMP
                      (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE J1 J3) M1))))
             (AEVAL (LIST 'EQUAL LOWER (LIST 'MAX LOWER (LIST 'MINUS TMP))))))
           (SETK (LIST 'RANGE 6 1) (AEVAL LOWER))
           (SETK (LIST 'RANGE 6 2) (AEVAL UPPER))
           (COND
            ((EVALNEQ (AEVAL UPPER) (AEVAL 'INFINITY))
             (PROGN
              (SETQ TMP (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE UPPER LOWER) 1)))
              (COND
               ((OR (EVALEQUAL (AEVAL BETTER) (AEVAL 'INFINITY))
                    (EVALGREATERP (AEVAL BETTER) (AEVAL TMP)))
                (PROGN (SETQ BETTER (AEVAL TMP)) (SETQ BEST (AEVAL 6))))))))
           (COND
            ((EVALEQUAL (AEVAL BEST) 1)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE J1 J2) M3))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J1 J2 J3 M1 M2 M3)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J1 J2 J3 M1 M2 M3)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            ((EVALEQUAL (AEVAL BEST) 2)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'PLUS (LIST 'PLUS J1 J2 J3)
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J1)
                                       M3)))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J2 J1 J3 M2 M1 M3)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J2 J1 J3 M2 M1 M3)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            ((EVALEQUAL (AEVAL BEST) 3)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'PLUS (LIST 'PLUS J1 J2 J3)
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J3 J2)
                                       M1)))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J3 J2 J1 M3 M2 M1)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J3 J2 J1 M3 M2 M1)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            ((EVALEQUAL (AEVAL BEST) 4)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'PLUS (LIST 'PLUS J1 J2 J3)
                                 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J1 J3)
                                       M2)))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J1 J3 J2 M1 M3 M2)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J1 J3 J2 M1 M3 M2)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            ((EVALEQUAL (AEVAL BEST) 5)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J3) M1))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J2 J3 J1 M2 M3 M1)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J2 J3 J1 M2 M3 M1)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            ((EVALEQUAL (AEVAL BEST) 6)
             (RETURN
              (AEVAL
               (LIST 'TIMES
                     (LIST '|*3J-SYMBOL:SIGN*|
                           (LIST 'DIFFERENCE (LIST 'DIFFERENCE J3 J1) M2))
                     (LIST 'CLEAN_UP_SQRT
                           (PROG (K FORALL-RESULT)
                             (SETQ K (AEVAL* (LIST 'RANGE BEST 1)))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE (AEVAL* (LIST 'RANGE BEST 2))
                                      K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (COND
                                             ((BOOLVALUE*
                                               (REVALX (LIST 'EVENP K)))
                                              (AEVAL*
                                               (LIST '|*3J-SYMBOL:ONE-TERM*| K
                                                     J3 J1 J2 M3 M1 M2)))
                                             (T
                                              (AEVAL*
                                               (LIST 'MINUS
                                                     (LIST
                                                      '|*3J-SYMBOL:ONE-TERM*| K
                                                      J3 J1 J2 M3 M1 M2)))))
                                            FORALL-RESULT)))
                             (SETQ K
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      K))
                             (GO LAB1)))))))
            (T
             (RETURN
              (AEVAL
               (LPRI
                (LIST "ThreeJSymbol({" (SECOND U1) "," (THIRD U1) "},{"
                      (SECOND U2) "," (THIRD U2) "},{" (SECOND U3) ","
                      (THIRD U3) "}) % symbol best left as is;")))))))))))
     (T (AEVAL " the argument must be of the form: {j1,m1},{j2,m2},{j3,m3}")))) 
(PUT '|*3J-SYMBOL:SIGN*| 'NUMBER-OF-ARGS 1) 
(FLAG '(|*3J-SYMBOL:SIGN*|) 'OPFN) 
(PUT '|*3J-SYMBOL:SIGN*| 'DEFINED-ON-LINE '307) 
(PUT '|*3J-SYMBOL:SIGN*| 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT '|*3J-SYMBOL:SIGN*| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |*3J-SYMBOL:SIGN*| (U)
    (COND ((BOOLVALUE* (REVALX 'ROUNDED)) (AEVAL (LIST 'EXPT (MINUS 1) U)))
          (T
           (AEVAL
            (LIST 'EXPT (MINUS 1)
                  (LIST 'QUOTIENT
                        (LIST 'REMAINDER (LIST 'NUM U)
                              (LIST 'TIMES 2 (LIST 'DEN U)))
                        (LIST 'DEN U))))))) 
(PUT '|*3J-SYMBOL:ONE-TERM*| 'NUMBER-OF-ARGS 7) 
(FLAG '(|*3J-SYMBOL:ONE-TERM*|) 'OPFN) 
(PUT '|*3J-SYMBOL:ONE-TERM*| 'DEFINED-ON-LINE '310) 
(PUT '|*3J-SYMBOL:ONE-TERM*| 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT '|*3J-SYMBOL:ONE-TERM*| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE |*3J-SYMBOL:ONE-TERM*| (K J1 J2 J3 M1 M2 M3)
    (LIST 'SQRT
          (LIST 'SIMPLIFY_FACTORIAL
                (LIST 'QUOTIENT
                      (LIST 'TIMES
                            (LIST 'FACTORIAL
                                  (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)))
                            (LIST 'FACTORIAL
                                  (LIST 'PLUS J3 (LIST 'DIFFERENCE J1 J2)))
                            (LIST 'FACTORIAL
                                  (LIST 'PLUS J2 (LIST 'DIFFERENCE J3 J1)))
                            (LIST 'FACTORIAL (LIST 'PLUS J1 M1))
                            (LIST 'FACTORIAL (LIST 'DIFFERENCE J1 M1))
                            (LIST 'FACTORIAL (LIST 'PLUS J2 M2))
                            (LIST 'FACTORIAL (LIST 'DIFFERENCE J2 M2))
                            (LIST 'FACTORIAL (LIST 'PLUS J3 M3))
                            (LIST 'FACTORIAL (LIST 'DIFFERENCE J3 M3)))
                      (LIST 'TIMES (LIST 'FACTORIAL (LIST 'PLUS J1 J2 J3 1))
                            (LIST 'EXPT
                                  (LIST 'TIMES (LIST 'FACTORIAL K)
                                        (LIST 'FACTORIAL
                                              (LIST 'PLUS J1
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE J2
                                                                J3)
                                                          K)))
                                        (LIST 'FACTORIAL
                                              (LIST 'PLUS
                                                    (LIST 'DIFFERENCE J3 J2) M1
                                                    K))
                                        (LIST 'FACTORIAL
                                              (LIST 'PLUS
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'DIFFERENCE J3
                                                                J1)
                                                          M2)
                                                    K))
                                        (LIST 'FACTORIAL
                                              (LIST 'DIFFERENCE
                                                    (LIST 'DIFFERENCE J1 M1)
                                                    K))
                                        (LIST 'FACTORIAL
                                              (LIST 'PLUS J2
                                                    (LIST 'DIFFERENCE M2 K))))
                                  2)))))) 
(AEVAL (OPERATOR (LIST 'CLEBSCH_GORDAN))) 
(AEVAL
 (LET
  (LIST
   (LIST 'REPLACEBY
         (LIST 'CLEBSCH_GORDAN (LIST 'LIST (LIST '~ 'J1) (LIST '~ 'M1))
               (LIST 'LIST (LIST '~ 'J2) (LIST '~ 'M2))
               (LIST 'LIST (LIST '~ 'J3) (LIST '~ 'M3)))
         (LIST 'TIMES
               (LIST 'THREEJSYMBOL (LIST 'LIST (LIST '~ 'J1) (LIST '~ 'M1))
                     (LIST 'LIST (LIST '~ 'J2) (LIST '~ 'M2))
                     (LIST 'LIST (LIST '~ 'J3) (LIST 'MINUS (LIST '~ 'M3))))
               (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 'J3) 1)
                     (LIST 'QUOTIENT 1 2))
               (LIST 'EXPT (MINUS 1)
                     (LIST 'PLUS (LIST 'DIFFERENCE 'J1 'J2) 'M3))))))) 
(AEVAL (OPERATOR (LIST 'SIXJSYMBOL))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (SIXJSYMBOL (LIST (~ J1) (~ J2) (~ J3)) (LIST (~ L1) (~ L2) (~ L3)))
      (PROG (NNN MMM *FACTOR *EXP SIGNUM)
        (COND
         ((EVALEQUAL (AEVAL (LIST 'NECESS_6J 'J1 'J2 'J3 'L1 'L2 'L3))
                     (AEVAL 'NIL))
          (RETURN (AEVAL 'NIL))))
        (AEVAL (ON (LIST 'FACTOR)))
        (SETQ MMM (AEVAL (LIST 'RACAH_W 'J1 'J2 'J3 'L1 'L2 'L3)))
        (SETQ NNN
                (AEVAL
                 (LIST 'TIMES (LIST 'SQUARE_RACAH_DELTA 'J1 'J2 'J3)
                       (LIST 'SQUARE_RACAH_DELTA 'J1 'L2 'L3)
                       (LIST 'SQUARE_RACAH_DELTA 'L1 'J2 'L3)
                       (LIST 'SQUARE_RACAH_DELTA 'L1 'L2 'J3)
                       (LIST 'EXPT MMM 2))))
        (SETQ NNN (AEVAL (LIST 'SIMPLIFY_FACTORIAL NNN)))
        (SETQ SIGNUM (AEVAL (LIST 'SIGN MMM)))
        (COND
         ((EVALNUMBERP (AEVAL SIGNUM))
          (RETURN (AEVAL (LIST 'TIMES SIGNUM (LIST 'SQRT NNN)))))
         (T
          (RETURN
           (AEVAL (LIST 'TIMES (LIST 'SIGN NNN) (LIST 'SQRT NNN)))))))))))) 
(PUT 'SQUARE_RACAH_DELTA 'NUMBER-OF-ARGS 3) 
(FLAG '(SQUARE_RACAH_DELTA) 'OPFN) 
(PUT 'SQUARE_RACAH_DELTA 'DEFINED-ON-LINE '361) 
(PUT 'SQUARE_RACAH_DELTA 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'SQUARE_RACAH_DELTA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SQUARE_RACAH_DELTA (A B C)
    (LIST 'SIMPLIFY_FACTORIAL
          (LIST 'TIMES (LIST 'FACTORIAL (LIST 'PLUS A (LIST 'DIFFERENCE B C)))
                (LIST 'FACTORIAL (LIST 'PLUS (LIST 'DIFFERENCE A B) C))
                (LIST 'QUOTIENT
                      (LIST 'FACTORIAL (LIST 'PLUS (LIST 'MINUS A) B C))
                      (LIST 'FACTORIAL (LIST 'PLUS A B C 1)))))) 
(PUT 'RACAH_W 'NUMBER-OF-ARGS 6) 
(FLAG '(RACAH_W) 'OPFN) 
(PUT 'RACAH_W 'DEFINED-ON-LINE '365) 
(PUT 'RACAH_W 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'RACAH_W 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RACAH_W (J1 J2 J3 L1 L2 L3)
    (PROG (MINI MAXI INTERV)
      (SETQ MINI
              (AEVAL
               (LIST 'MIN (LIST 'PLUS J1 J2 L1 L2) (LIST 'PLUS J2 J3 L2 L3)
                     (LIST 'PLUS J3 J1 L3 L1))))
      (SETQ MAXI
              (AEVAL
               (LIST 'MAX 0 (LIST 'PLUS J1 J2 J3) (LIST 'PLUS J1 L2 L3)
                     (LIST 'PLUS L1 J2 L3) (LIST 'PLUS L1 L2 J3))))
     AAA
      (COND
       ((EVALNUMBERP (AEVAL (LIST 'DIFFERENCE MINI MAXI)))
        (RETURN
         (PROG (K FORALL-RESULT)
           (SETQ K (AEVAL* MAXI))
           (SETQ FORALL-RESULT 0)
          LAB1
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MINI) K))
             (RETURN FORALL-RESULT)))
           (SETQ FORALL-RESULT
                   (AEVAL*
                    (LIST 'PLUS
                          (AEVAL*
                           (LIST 'TIMES (LIST 'EXPT (MINUS 1) K)
                                 (LIST 'SIMPLIFY_FACTORIAL
                                       (LIST 'QUOTIENT
                                             (LIST 'FACTORIAL (LIST 'PLUS K 1))
                                             (LIST 'TIMES
                                                   (LIST 'FACTORIAL
                                                         (LIST 'DIFFERENCE
                                                               (LIST
                                                                'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE K
                                                                 J1)
                                                                J2)
                                                               J3))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'DIFFERENCE
                                                               (LIST
                                                                'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE K
                                                                 J1)
                                                                L2)
                                                               L3))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'DIFFERENCE
                                                               (LIST
                                                                'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE K
                                                                 L1)
                                                                J2)
                                                               L3))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'DIFFERENCE
                                                               (LIST
                                                                'DIFFERENCE
                                                                (LIST
                                                                 'DIFFERENCE K
                                                                 L1)
                                                                L2)
                                                               J3))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'PLUS J1 J2 L1
                                                               (LIST
                                                                'DIFFERENCE L2
                                                                K)))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'PLUS J2 J3 L2
                                                               (LIST
                                                                'DIFFERENCE L3
                                                                K)))
                                                   (LIST 'FACTORIAL
                                                         (LIST 'PLUS J3 J1 L3
                                                               (LIST
                                                                'DIFFERENCE L1
                                                                K))))))))
                          FORALL-RESULT)))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB1))))
       (T
        (PROGN
         (SETQ INTERV (AEVAL (LIST 'FINDINTERVAL J1 J2 J3 L1 L2 L3)))
         (COND
          ((EVALEQUAL (AEVAL INTERV) (AEVAL (LIST 'LIST)))
           (RETURN
            (AEVAL
             (LIST 'SUM
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'K)
                         (LIST 'SIMPLIFY_FACTORIAL
                               (LIST 'QUOTIENT
                                     (LIST 'FACTORIAL (LIST 'PLUS 'K 1))
                                     (LIST 'TIMES
                                           (LIST 'FACTORIAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'DIFFERENCE
                                                                   'K J1)
                                                             J2)
                                                       J3))
                                           (LIST 'FACTORIAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'DIFFERENCE
                                                                   'K J1)
                                                             L2)
                                                       L3))
                                           (LIST 'FACTORIAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'DIFFERENCE
                                                                   'K L1)
                                                             J2)
                                                       L3))
                                           (LIST 'FACTORIAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'DIFFERENCE
                                                                   'K L1)
                                                             L2)
                                                       J3))
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS J1 J2 L1
                                                       (LIST 'DIFFERENCE L2
                                                             'K)))
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS J2 J3 L2
                                                       (LIST 'DIFFERENCE L3
                                                             'K)))
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS J3 J1 L3
                                                       (LIST 'DIFFERENCE L1
                                                             'K)))))))
                   'K MAXI MINI))))
          (T
           (PROGN
            (SETQ MAXI (AEVAL (LIST 'FIRST (LIST 'FIRST INTERV))))
            (SETQ MINI
                    (AEVAL
                     (LIST 'PLUS (LIST 'SECOND (LIST 'FIRST INTERV)) MAXI)))
            (GO AAA))))
         (AEVAL 'NIL)))))) 
(PUT 'NECESS_6J 'NUMBER-OF-ARGS 6) 
(FLAG '(NECESS_6J) 'OPFN) 
(PUT 'NECESS_6J 'DEFINED-ON-LINE '404) 
(PUT 'NECESS_6J 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'NECESS_6J 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NECESS_6J (J1 J2 J3 L1 L2 L3)
    (PROG (NNN *ROUNDED DMODE*)
      (AEVAL (OFF (LIST 'ROUNDED)))
      (SETQ NNN (AEVAL (LIST 'PLUS J1 J2 J3)))
      (COND
       ((EVALNUMBERP (AEVAL NNN))
        (COND ((NOT (FIXP (REVALX NNN))) (RETURN (AEVAL 'NIL))))))
      (SETQ NNN (AEVAL (LIST 'PLUS L1 L2 J3)))
      (COND
       ((EVALNUMBERP (AEVAL NNN))
        (COND ((NOT (FIXP (REVALX NNN))) (RETURN (AEVAL 'NIL))))))
      (SETQ NNN (AEVAL (LIST 'PLUS J1 L2 L3)))
      (COND
       ((EVALNUMBERP (AEVAL NNN))
        (COND ((NOT (FIXP (REVALX NNN))) (RETURN (AEVAL 'NIL))))))
      (SETQ NNN (AEVAL (LIST 'PLUS L1 J2 L3)))
      (COND
       ((EVALNUMBERP (AEVAL NNN))
        (COND ((NOT (FIXP (REVALX NNN))) (RETURN (AEVAL 'NIL))))))
      (COND
       ((OR (NOT (EVALNUMBERP (AEVAL J1))) (NOT (EVALNUMBERP (AEVAL J2)))
            (NOT (EVALNUMBERP (AEVAL J3))) (NOT (EVALNUMBERP (AEVAL L1)))
            (NOT (EVALNUMBERP (AEVAL L2))) (NOT (EVALNUMBERP (AEVAL L3))))
        (RETURN (AEVAL 'T))))
      (COND
       ((AND (EVALGEQ (AEVAL (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3))) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE J1 J2) J3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'MINUS J1) J2 J3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS L1 (LIST 'DIFFERENCE L2 J3))) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE L1 L2) J3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'MINUS L1) L2 J3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS J1 (LIST 'DIFFERENCE L2 L3))) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE J1 L2) L3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'MINUS J1) L2 L3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS L1 (LIST 'DIFFERENCE J2 L3))) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE L1 J2) L3)) 0)
             (EVALGEQ (AEVAL (LIST 'PLUS (LIST 'MINUS L1) J2 L3)) 0))
        (RETURN (AEVAL 'T)))))) 
(PUT 'ACONDS-6J 'NUMBER-OF-ARGS 6) 
(FLAG '(ACONDS-6J) 'OPFN) 
(PUT 'ACONDS-6J 'DEFINED-ON-LINE '431) 
(PUT 'ACONDS-6J 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'ACONDS-6J 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ACONDS-6J (J1 J2 J3 L1 L2 L3)
    (LIST 'LIST (LIST 'GEQ (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J1 J2) J3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS J1) J2 J3) 0)
          (LIST 'GEQ (LIST 'PLUS L1 (LIST 'DIFFERENCE L2 J3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE L1 L2) J3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS L1) L2 J3) 0)
          (LIST 'GEQ (LIST 'PLUS J1 (LIST 'DIFFERENCE L2 L3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J1 L2) L3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS J1) L2 L3) 0)
          (LIST 'GEQ (LIST 'PLUS L1 (LIST 'DIFFERENCE J2 L3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE L1 J2) L3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS L1) J2 L3) 0)
          (LIST 'GEQ (LIST 'PLUS '=K 1) 0)
          (LIST 'GEQ
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'DIFFERENCE '=K J1) J2) J3)
                0)
          (LIST 'GEQ
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'DIFFERENCE '=K J1) L2) L3)
                0)
          (LIST 'GEQ
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'DIFFERENCE '=K L1) J2) L3)
                0)
          (LIST 'GEQ
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE (LIST 'DIFFERENCE '=K L1) L2) J3)
                0)
          (LIST 'GEQ (LIST 'PLUS J1 J2 L1 (LIST 'DIFFERENCE L2 '=K)) 0)
          (LIST 'GEQ (LIST 'PLUS J2 J3 L2 (LIST 'DIFFERENCE L3 '=K)) 0)
          (LIST 'GEQ (LIST 'PLUS J3 J1 L3 (LIST 'DIFFERENCE L1 '=K)) 0))) 
(PUT 'CONDS-6J 'NUMBER-OF-ARGS 6) 
(FLAG '(CONDS-6J) 'OPFN) 
(PUT 'CONDS-6J 'DEFINED-ON-LINE '456) 
(PUT 'CONDS-6J 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'CONDS-6J 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONDS-6J (J1 J2 J3 L1 L2 L3)
    (LIST 'LIST (LIST 'GEQ (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J1 J2) J3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS J1) J2 J3) 0)
          (LIST 'GEQ (LIST 'PLUS L1 (LIST 'DIFFERENCE L2 J3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE L1 L2) J3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS L1) L2 J3) 0)
          (LIST 'GEQ (LIST 'PLUS J1 (LIST 'DIFFERENCE L2 L3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J1 L2) L3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS J1) L2 L3) 0)
          (LIST 'GEQ (LIST 'PLUS L1 (LIST 'DIFFERENCE J2 L3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE L1 J2) L3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS L1) J2 L3) 0) (LIST 'GEQ '=K 0)
          (LIST 'GEQ (LIST 'PLUS J1 J2 L1 L2 (LIST 'DIFFERENCE 1 '=K)) 0)
          (LIST 'GEQ
                (LIST 'PLUS J1 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J3) '=K))
                0)
          (LIST 'GEQ
                (LIST 'PLUS L1 (LIST 'DIFFERENCE (LIST 'DIFFERENCE L2 J3) '=K))
                0)
          (LIST 'GEQ
                (LIST 'PLUS J1 (LIST 'DIFFERENCE (LIST 'DIFFERENCE L2 L3) '=K))
                0)
          (LIST 'GEQ
                (LIST 'PLUS L1 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 L3) '=K))
                0)
          (LIST 'GEQ
                (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'MINUS J1) L1) J3 L3 '=K)
                0)
          (LIST 'GEQ
                (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'MINUS J2) L2) J3 L3 '=K)
                0))) 
(PUT 'CONDS-3J 'NUMBER-OF-ARGS 6) 
(FLAG '(CONDS-3J) 'OPFN) 
(PUT 'CONDS-3J 'DEFINED-ON-LINE '481) 
(PUT 'CONDS-3J 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'CONDS-3J 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONDS-3J (J1 J2 J3 M1 M2 M3)
    (LIST 'LIST (LIST 'EQUAL (LIST 'PLUS M1 M2 M3) 0)
          (LIST 'GEQ (LIST 'PLUS J1 (LIST 'DIFFERENCE J2 J3)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J1 J2) J3) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'MINUS J1) J2 J3) 0) (LIST 'GEQ '=K 0)
          (LIST 'GEQ
                (LIST 'PLUS J1 (LIST 'DIFFERENCE (LIST 'DIFFERENCE J2 J3) '=K))
                0)
          (LIST 'GEQ (LIST 'DIFFERENCE (LIST 'DIFFERENCE J1 M1) '=K) 0)
          (LIST 'GEQ (LIST 'PLUS J3 (LIST 'DIFFERENCE M2 '=K)) 0)
          (LIST 'GEQ (LIST 'PLUS (LIST 'DIFFERENCE J3 J2) M1 '=K) 0)
          (LIST 'GEQ
                (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'DIFFERENCE J3 J1) M2) '=K)
                0))) 
(PUT 'FINDINTERVAL 'NUMBER-OF-ARGS 6) 
(FLAG '(FINDINTERVAL) 'OPFN) 
(PUT 'FINDINTERVAL 'DEFINED-ON-LINE '497) 
(PUT 'FINDINTERVAL 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FINDINTERVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FINDINTERVAL (J1 J2 J3 L1 L2 L3)
    (PROG (INTERV SVARS)
      (SETQ SVARS
              (AEVAL
               (CONS 'LIST
                     (SOLVEVARS
                      (LIST (SIMP J1) (SIMP J2) (SIMP J3) (SIMP L1) (SIMP L2)
                            (SIMP L3))))))
      (SETQ INTERV
              (AEVAL
               (LIST 'REVERSE
                     (LIST 'INEQ_SOLVE (LIST 'ACONDS-6J J1 J2 J3 L1 L2 L3)
                           (LIST 'CONS '=K SVARS) (LIST 'EQUAL 'RECORD 'T)))))
      (RETURN
       (AEVAL (LIST 'FINDINTERVAL1 (LIST 'PART (LIST 'FIRST INTERV)) 0))))) 
(AEVAL 'NIL) 
(PUT 'FINDINTERVAL1 'NUMBER-OF-ARGS 2) 
(PUT 'FINDINTERVAL1 'DEFINED-ON-LINE '509) 
(PUT 'FINDINTERVAL1 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FINDINTERVAL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDINTERVAL1 (MAXIS MINIS)
    ((LAMBDA (FIXEDINTS DIFFF)
       (PROGN
        (COND
         ((AND (EQCAR MAXIS 'EQUAL) (EQ (CADR MAXIS) '=K))
          (SETQ MAXIS (CADDR MAXIS))))
        (COND
         ((NOT (EQCAR MAXIS '*INTERVAL*)) (LIST 'LIST (LIST 'LIST MAXIS 0)))
         (T
          (PROGN
           (SETQ MINIS (CADDR MAXIS))
           (SETQ MAXIS (CADR MAXIS))
           (COND ((EQCAR MAXIS 'MAX) (SETQ MAXIS (CDR MAXIS)))
                 (T (SETQ MAXIS (LIST MAXIS))))
           (COND ((EQCAR MINIS 'MIN) (SETQ MINIS (CDR MINIS)))
                 (T (SETQ MINIS (LIST MINIS))))
           (PROG (XX)
             (SETQ XX MAXIS)
            LAB
             (COND ((NULL XX) (RETURN NIL)))
             ((LAMBDA (XX)
                (PROG (YY)
                  (SETQ YY MINIS)
                 LAB
                  (COND ((NULL YY) (RETURN NIL)))
                  ((LAMBDA (YY)
                     (COND
                      ((NUMBERP
                        (SETQ DIFFF (REVAL1 (LIST 'DIFFERENCE YY XX) T)))
                       (SETQ FIXEDINTS
                               (CONS (LIST 'LIST XX DIFFF) FIXEDINTS)))))
                   (CAR YY))
                  (SETQ YY (CDR YY))
                  (GO LAB)))
              (CAR XX))
             (SETQ XX (CDR XX))
             (GO LAB))
           (CONS 'LIST FIXEDINTS))))))
     NIL NIL)) 
(FLAG '(FINDINTERVAL1) 'OPFN) 
(PUT 'FANCY-CLEBSCH_GORDON 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-CLEBSCH_GORDON 'DEFINED-ON-LINE '533) 
(PUT 'FANCY-CLEBSCH_GORDON 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FANCY-CLEBSCH_GORDON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-CLEBSCH_GORDON (U)
    (PROG (A J1 M1 J2 M2 J M)
      (SETQ U (CDR U))
      (SETQ J1 (CADR (CAR U)))
      (SETQ M1 (CADDR (CAR U)))
      (SETQ U (CDR U))
      (SETQ J2 (CADR (CAR U)))
      (SETQ M2 (CADDR (CAR U)))
      (SETQ U (CDR U))
      (SETQ J (CADR (CAR U)))
      (SETQ M (CADDR (CAR U)))
      (SETQ A (LIST J1 M1 J2 M2 '|\|| J1 J2 J M))
      (RETURN
       (FANCY-IN-BRACKETS (LIST 'FANCY-INPRINT (MKQUOTE 'TIMES) 0 (MKQUOTE A))
        '|(| '|)|)))) 
(PUT 'CLEBSCH_GORDON 'FANCY-PRIFN 'FANCY-CLEBSCH_GORDON) 
(PUT 'FANCY-THREEJSYMBOL 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-THREEJSYMBOL 'DEFINED-ON-LINE '546) 
(PUT 'FANCY-THREEJSYMBOL 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FANCY-THREEJSYMBOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-THREEJSYMBOL (U)
    (FANCY-MATPRI2 (LIST (CDR (CADR U)) (CDR (CADDR U))) NIL NIL)) 
(PUT 'THREEJSYMBOL 'FANCY-PRIFN 'FANCY-THREEJSYMBOL) 
(PUT 'FANCY-SIXJSYMBOL 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-SIXJSYMBOL 'DEFINED-ON-LINE '551) 
(PUT 'FANCY-SIXJSYMBOL 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FANCY-SIXJSYMBOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-SIXJSYMBOL (U)
    (FANCY-MATPRI2 (LIST (CDR (CADR U)) (CDR (CADDR U))) NIL '("{" . "}"))) 
(PUT 'SIXJSYMBOL 'FANCY-PRIFN 'FANCY-SIXJSYMBOL) 
(PUT 'FANCY-NINEJSYMBOL 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-NINEJSYMBOL 'DEFINED-ON-LINE '556) 
(PUT 'FANCY-NINEJSYMBOL 'DEFINED-IN-FILE 'SPECFN/JSYMBOLS.RED) 
(PUT 'FANCY-NINEJSYMBOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-NINEJSYMBOL (U)
    (FANCY-MATPRI2 (LIST (CDR (CADR U)) (CDR (CADDR U)) (CDR (CADDDR U))) NIL
     '("{" . "}"))) 
(PUT 'NINEJSYMBOL 'FANCY-PRIFN 'FANCY-NINEJSYMBOL) 
(ENDMODULE) 