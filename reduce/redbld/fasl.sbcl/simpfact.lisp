(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPFACT)) 
(AEVAL (OPERATOR (LIST 'SIMPLIFY_FACTORIAL1))) 
(AEVAL (OPERATOR (LIST 'SIMPLIFY_FACTORIAL))) 
(AEVAL (OPERATOR (LIST 'INT_SIMPLIFY_FACTORIAL))) 
(AEVAL
 (LET
  '((REPLACEBY (SIMPLIFY_FACTORIAL (~ X))
     (SIMPLIFY_FACTORIAL1 (NUM X) (DEN X)))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (SIMPLIFY_FACTORIAL1 (~ X) (~ Z))
      (INT_SIMPLIFY_FACTORIAL (QUOTIENT X Z))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (SIMPLIFY_FACTORIAL1 (PLUS (~ X) (~ Y)) (~ Z))
      (PLUS (SIMPLIFY_FACTORIAL1 X Z) (SIMPLIFY_FACTORIAL1 Y Z))))))) 
(AEVAL 'NIL) 
(PUT 'INT_SIMPLIFY_FACTORIAL 'NUMBER-OF-ARGS 1) 
(PUT 'INT_SIMPLIFY_FACTORIAL 'DEFINED-ON-LINE '51) 
(PUT 'INT_SIMPLIFY_FACTORIAL 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'INT_SIMPLIFY_FACTORIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT_SIMPLIFY_FACTORIAL (U)
    (PROG (MINUS_NUM MINUS_DENOM TEST_EXPT)
      (COND ((OR (NOT (PAIRP U)) (NEQ (CAR U) 'QUOTIENT)) U)
            (T
             (PROGN
              (COND ((OR (ATOM (CADR U)) (ATOM (CADDR U))) U)
                    (T
                     (PROGN
                      (COND
                       ((EQ (CAR (CADR U)) 'MINUS)
                        (PROGN
                         (SETCAR (CDR U) (CADR (CADR U)))
                         (SETQ MINUS_NUM T)
                         NIL)))
                      (COND
                       ((EQ (CAR (CADDR U)) 'MINUS)
                        (PROGN
                         (SETCAR (CDDR U) (CADR (CADDR U)))
                         (SETQ MINUS_DENOM T)
                         NIL)))
                      (COND
                       ((EQ (CAR (CADR U)) 'FACTORIAL)
                        (SETCAR (CDR U) (LIST 'TIMES (CADR U)))))
                      (COND
                       ((EQ (CAR (CADDR U)) 'FACTORIAL)
                        (SETCAR (CDDR U) (LIST 'TIMES (CADDR U)))))
                      (COND
                       ((OR (EQ (CAR (CADR U)) 'ODDEXPT)
                            (EQ (CAR (CADR U)) 'EXPT)
                            (EQ (CAR (CADR U)) 'SQRT))
                        (SETCAR (CDR U) (LIST 'TIMES (CADR U)))))
                      (COND
                       ((OR (EQ (CAR (CADDR U)) 'ODDEXPT)
                            (EQ (CAR (CADDR U)) 'EXPT)
                            (EQ (CAR (CADDR U)) 'SQRT))
                        (SETCAR (CDDR U) (LIST 'TIMES (CADDR U)))))
                      (COND
                       ((OR (TEST_FOR_EXPT (CADR U)) (TEST_FOR_EXPT (CADDR U)))
                        (PROGN
                         (SETQ TEST_EXPT T)
                         (CONVERT_TO_ODDEXPT (CADR U))
                         (CONVERT_TO_ODDEXPT (CADDR U))
                         NIL)))
                      (COND
                       ((TEST_FOR_FACTS (CADR U) (CADDR U))
                        (GOTHRU_NUMERATOR (CADR U) (CADDR U))))
                      (COND
                       (MINUS_NUM (SETCAR (CDR U) (LIST 'MINUS (CADR U)))))
                      (COND
                       (MINUS_DENOM (SETCAR (CDDR U) (LIST 'MINUS (CADDR U)))))
                      (SETCAR (CDR U) (REVAL1 (CADR U) T))
                      (SETCAR (CDDR U) (REVAL1 (CADDR U) T))
                      NIL)))
              (SETQ U (AEVAL (LIST 'SUB (LIST 'EQUAL 'ODDEXPT 'EXPT) U)))
              NIL)))
      (RETURN U))) 
(FLAG '(INT_SIMPLIFY_FACTORIAL) 'OPFN) 
(PUT 'TEST_FOR_EXPT 'NUMBER-OF-ARGS 1) 
(PUT 'TEST_FOR_EXPT 'DEFINED-ON-LINE '115) 
(PUT 'TEST_FOR_EXPT 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'TEST_FOR_EXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST_FOR_EXPT (INPUT)
    (PROG (FOUND_EXPT NOT_FOUND)
      (SETQ NOT_FOUND T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND INPUT NOT_FOUND)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (PAIRP (CAR INPUT))
                (OR (EQUAL (CAAR INPUT) 'EXPT) (EQUAL (CAAR INPUT) 'SQRT)))
           (PROGN (SETQ FOUND_EXPT T) (SETQ NOT_FOUND NIL) NIL)))
         (SETQ INPUT (CDR INPUT))
         NIL)
        (GO WHILELABEL))
      (RETURN FOUND_EXPT))) 
(FLAG '(TEST_FOR_EXPT) 'BOOLEAN) 
(PUT 'CONVERT_TO_ODDEXPT 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT_TO_ODDEXPT 'DEFINED-ON-LINE '135) 
(PUT 'CONVERT_TO_ODDEXPT 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'CONVERT_TO_ODDEXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT_TO_ODDEXPT (INPUT)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND ((NOT INPUT) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (PAIRP (CAR INPUT)) (EQUAL (CAAR INPUT) 'EXPT))
           (SETCAR (CAR INPUT) 'ODDEXPT)))
         (COND
          ((AND (PAIRP (CAR INPUT)) (EQUAL (CAAR INPUT) 'SQRT))
           (PROGN
            (SETCAR (CAR INPUT) 'ODDEXPT)
            (SETCDR (CAR INPUT) (LIST (CADAR INPUT) (LIST 'QUOTIENT 1 2)))
            NIL)))
         (SETQ INPUT (CDR INPUT))
         NIL)
        (GO WHILELABEL)))) 
(PUT 'GOTHRU_NUMERATOR 'NUMBER-OF-ARGS 2) 
(PUT 'GOTHRU_NUMERATOR 'DEFINED-ON-LINE '155) 
(PUT 'GOTHRU_NUMERATOR 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'GOTHRU_NUMERATOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GOTHRU_NUMERATOR (NUM DENOM)
    (PROG (CHANGE ORIGNUM ORIGDENOM)
      (SETQ CHANGE 0)
      (SETQ ORIGNUM NUM)
      (SETQ ORIGDENOM DENOM)
      (PROG ()
       WHILELABEL
        (COND ((NOT NUM) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (PAIRP (CAR NUM)) (EQ (CAAR NUM) 'ODDEXPT))
           (PROGN
            (COND
             ((AND (PAIRP (CADAR NUM)) (EQ (CAADAR NUM) 'FACTORIAL))
              (SETQ CHANGE (PLUS CHANGE (GOTHRU_DENOMINATOR NUM DENOM)))))
            NIL))
          ((AND (PAIRP (CAR NUM)) (EQ (CAAR NUM) 'FACTORIAL))
           (PROGN
            (SETQ CHANGE (PLUS CHANGE (GOTHRU_DENOMINATOR NUM DENOM)))
            NIL)))
         (SETQ NUM (CDR NUM))
         NIL)
        (GO WHILELABEL))
      (COND
       ((AND (NOT NUM) (NOT (EQN CHANGE 0)))
        (PROGN
         (COND
          ((TEST_FOR_FACTS ORIGNUM ORIGDENOM)
           (GOTHRU_NUMERATOR ORIGNUM ORIGDENOM)))
         NIL))))) 
(PUT 'GOTHRU_DENOMINATOR 'NUMBER-OF-ARGS 2) 
(PUT 'GOTHRU_DENOMINATOR 'DEFINED-ON-LINE '195) 
(PUT 'GOTHRU_DENOMINATOR 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'GOTHRU_DENOMINATOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GOTHRU_DENOMINATOR (NUM DENOM)
    (PROG (TEST CHANGE)
      (SETQ CHANGE 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND DENOM (EQUAL CHANGE 0))) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (PAIRP (CAR DENOM)) (EQ (CAAR DENOM) 'ODDEXPT))
           (PROGN
            (COND
             ((AND (PAIRP (CADAR DENOM)) (EQ (CAADAR DENOM) 'FACTORIAL))
              (PROGN
               (SETQ TEST (ODDEXPT_TEST NUM DENOM CHANGE))
               (SETQ CHANGE (PLUS CHANGE TEST))
               NIL)))
            NIL))
          ((AND (PAIRP (CAR DENOM)) (EQ (CAAR DENOM) 'FACTORIAL))
           (PROGN
            (SETQ TEST (ODDEXPT_TEST NUM DENOM CHANGE))
            (SETQ CHANGE (PLUS CHANGE TEST))
            NIL)))
         (SETQ DENOM (CDR DENOM))
         NIL)
        (GO WHILELABEL))
      (RETURN CHANGE))) 
(PUT 'ODDEXPT_TEST 'NUMBER-OF-ARGS 3) 
(PUT 'ODDEXPT_TEST 'DEFINED-ON-LINE '229) 
(PUT 'ODDEXPT_TEST 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'ODDEXPT_TEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODDEXPT_TEST (NUM DENOM CHANGE)
    (PROG (TEST)
      (COND
       ((AND (EQ (CAAR NUM) 'ODDEXPT) (NEQ (CAAR DENOM) 'ODDEXPT))
        (PROGN (SETQ TEST (COMPARE_NUMODDEXPTFACTORIAL NUM DENOM CHANGE)) NIL))
       ((AND (NEQ (CAAR NUM) 'ODDEXPT) (EQ (CAAR DENOM) 'ODDEXPT))
        (PROGN
         (SETQ TEST (COMPARE_DENOMODDEXPTFACTORIAL NUM DENOM CHANGE))
         NIL))
       ((AND (EQ (CAAR NUM) 'ODDEXPT) (EQ (CAAR DENOM) 'ODDEXPT))
        (PROGN
         (SETQ TEST (COMPARE_BOTHODDEXPTFACTORIAL NUM DENOM CHANGE))
         NIL))
       (T (SETQ TEST (COMPARE_FACTORIAL NUM DENOM CHANGE))))
      (RETURN TEST))) 
(PUT 'COMPARE_FACTORIAL 'NUMBER-OF-ARGS 3) 
(PUT 'COMPARE_FACTORIAL 'DEFINED-ON-LINE '250) 
(PUT 'COMPARE_FACTORIAL 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'COMPARE_FACTORIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPARE_FACTORIAL (NUM DENOM CHANGE)
    (PROG (NUMSIMP DENOMSIMP DIFF)
      (COND
       ((NUMBERP (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CADAR DENOM)) T))
        (PROGN
         (SETQ CHANGE (PLUS CHANGE 1))
         (SETQ DIFF (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CADAR DENOM)) T))
         (COND
          ((GREATERP DIFF 0)
           (PROGN
            (SETQ NUMSIMP
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETCAR NUM (CONS 'TIMES NUMSIMP))
            (SETCAR DENOM 1)
            NIL))
          (T
           (PROGN
            (SETQ DIFF (MINUS DIFF))
            (SETQ DENOMSIMP
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (REVAL1 (LIST 'PLUS (CADAR NUM) I) T)
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETCAR DENOM (CONS 'TIMES DENOMSIMP))
            (SETCAR NUM 1)
            NIL)))
         NIL)))
      (RETURN CHANGE))) 
(PUT 'COMPARE_NUMODDEXPTFACTORIAL 'NUMBER-OF-ARGS 3) 
(PUT 'COMPARE_NUMODDEXPTFACTORIAL 'DEFINED-ON-LINE '286) 
(PUT 'COMPARE_NUMODDEXPTFACTORIAL 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'COMPARE_NUMODDEXPTFACTORIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPARE_NUMODDEXPTFACTORIAL (NUM DENOM CHANGE)
    (PROG (DIFF)
      (COND
       ((NUMBERP
         (REVAL1 (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CADAR DENOM)) T))
        (PROGN
         (COND
          ((SQRT_TEST NUM)
           (PROGN
            (PROGN
             (SETQ DIFF
                     (REVAL1
                      (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CADAR DENOM)) T))
             (SETQ CHANGE (PLUS CHANGE 1))
             (COND ((GREATERP DIFF 0) (SIMPLIFY_SQRT1 NUM DENOM DIFF))
                   (T (SIMPLIFY_SQRT2 NUM DENOM DIFF)))
             NIL)
            NIL))
          ((NOT_INT_OR_SQRT NUM) (PROGN NIL))
          ((EQN (DIFFERENCE (CADDAR NUM) 1) 1)
           (PROGN
            (SETCAR NUM (CAR (LIST (CADAR NUM))))
            (SETQ DIFF (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CADAR DENOM)) T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (PROGN (SIMPLIFY1 NUM DENOM DIFF) NIL))
                  (T (SIMPLIFY2 NUM DENOM DIFF)))
            NIL))
          (T
           (PROGN
            (SETCAR NUM
                    (LIST (CAAR NUM) (CADAR NUM)
                          (DIFFERENCE (CAR (CDDAR NUM)) 1)))
            (SETQ DIFF
                    (REVAL1 (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CADAR DENOM))
                            T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (PROGN (SIMPLIFY1 NUM DENOM DIFF) NIL))
                  (T (SIMPLIFY2 (CDAR NUM) DENOM DIFF)))
            NIL)))
         NIL)))
      (RETURN CHANGE))) 
(PUT 'SIMPLIFY_SQRT1 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT1 'DEFINED-ON-LINE '334) 
(PUT 'SIMPLIFY_SQRT1 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT1 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (CDAR NUM) (CAR (LIST (CONS 'TIMES NUMSIMP))))
      (SETCAR DENOM (LIST 'ODDEXPT (CAR DENOM) (LIST 'QUOTIENT 1 2))))) 
(PUT 'SIMPLIFY_SQRT2 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT2 'DEFINED-ON-LINE '344) 
(PUT 'SIMPLIFY_SQRT2 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT2 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR DENOM
              (REVAL1
               (LIST 'TIMES (CAR NUM) (CAR (LIST (CONS 'TIMES DENOMSIMP)))) T))
      (SETCAR NUM 1))) 
(PUT 'SIMPLIFY1 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY1 'DEFINED-ON-LINE '355) 
(PUT 'SIMPLIFY1 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY1 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR NUM (CONS (CAR (LIST (CONS 'TIMES NUMSIMP))) (CDR NUM)))
      (SETCAR DENOM 1))) 
(PUT 'SIMPLIFY2 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY2 'DEFINED-ON-LINE '365) 
(PUT 'SIMPLIFY2 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY2 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR DENOM (CONS (CAR (LIST (CONS 'TIMES DENOMSIMP))) (CDR DENOM)))
      (SETCAR DENOM 1))) 
(PUT 'COMPARE_DENOMODDEXPTFACTORIAL 'NUMBER-OF-ARGS 3) 
(PUT 'COMPARE_DENOMODDEXPTFACTORIAL 'DEFINED-ON-LINE '376) 
(PUT 'COMPARE_DENOMODDEXPTFACTORIAL 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'COMPARE_DENOMODDEXPTFACTORIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPARE_DENOMODDEXPTFACTORIAL (NUM DENOM CHANGE)
    (PROG (DIFF)
      (COND
       ((NUMBERP
         (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CAR (CDADAR DENOM))) T))
        (PROGN
         (COND
          ((SQRT_TEST DENOM)
           (PROGN
            (PROGN
             (SETQ DIFF
                     (REVAL1
                      (LIST 'DIFFERENCE (CADAR NUM) (CAR (CDADAR DENOM))) T))
             (SETQ CHANGE (PLUS CHANGE 1))
             (COND ((GREATERP DIFF 0) (SIMPLIFY_SQRT3 NUM DENOM DIFF))
                   (T (SIMPLIFY_SQRT4 NUM DENOM DIFF)))
             NIL)
            NIL))
          ((NOT_INT_OR_SQRT DENOM) (PROGN NIL))
          ((EQN (DIFFERENCE (CADDAR DENOM) 1) 1)
           (PROGN
            (SETCAR DENOM (CAR (LIST (CADAR DENOM))))
            (SETQ DIFF (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CADAR DENOM)) T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY3 NUM DENOM DIFF))
                  (T (SIMPLIFY4 NUM DENOM DIFF)))
            NIL))
          (T
           (PROGN
            (SETCAR DENOM
                    (LIST (CAAR DENOM) (CADAR DENOM)
                          (DIFFERENCE (CAR (CDDAR DENOM)) 1)))
            (SETQ DIFF
                    (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CAR (CDADAR DENOM)))
                            T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY3 NUM (CDAR DENOM) DIFF))
                  (T (SIMPLIFY4 NUM DENOM DIFF)))
            NIL)))
         NIL)))
      (RETURN CHANGE))) 
(PUT 'SQRT_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'SQRT_TEST 'DEFINED-ON-LINE '423) 
(PUT 'SQRT_TEST 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SQRT_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRT_TEST (INPUT)
    (PROG ()
      (COND ((EQUAL (CADDAR INPUT) '(QUOTIENT 1 2)) (RETURN T))
            (T (RETURN NIL))))) 
(FLAG '(SQRT_TEST) 'BOOLEAN) 
(PUT 'NOT_INT_OR_SQRT 'NUMBER-OF-ARGS 1) 
(PUT 'NOT_INT_OR_SQRT 'DEFINED-ON-LINE '436) 
(PUT 'NOT_INT_OR_SQRT 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'NOT_INT_OR_SQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOT_INT_OR_SQRT (INPUT)
    (PROG ()
      (COND
       ((AND (PAIRP (CADDAR INPUT)) (EQUAL (CAR (CADDAR INPUT)) 'QUOTIENT)
             (NEQ (CDR (CADDAR INPUT)) '(1 2)))
        (RETURN T))
       (T (RETURN NIL))))) 
(FLAG '(NOT_INT_OR_SQRT) 'BOOLEAN) 
(PUT 'SIMPLIFY_SQRT3 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT3 'DEFINED-ON-LINE '450) 
(PUT 'SIMPLIFY_SQRT3 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT3 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR NUM
              (REVAL1
               (LIST 'TIMES (CAR DENOM) (CAR (LIST (CONS 'TIMES NUMSIMP)))) T))
      (SETCAR DENOM 1))) 
(PUT 'SIMPLIFY_SQRT4 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT4 'DEFINED-ON-LINE '460) 
(PUT 'SIMPLIFY_SQRT4 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT4 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((EQUAL DIFF 0) (SETCAR DENOM 1))
            (T (SETCAR (CDAR DENOM) (CAR (LIST (CONS 'TIMES DENOMSIMP))))))
      (SETCAR NUM (LIST 'ODDEXPT (CAR NUM) (LIST 'QUOTIENT 1 2))))) 
(PUT 'SIMPLIFY3 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY3 'DEFINED-ON-LINE '472) 
(PUT 'SIMPLIFY3 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY3 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR NUM (CONS (CAR (LIST (CONS 'TIMES NUMSIMP))) (CDR NUM)))
      (SETCAR NUM 1))) 
(PUT 'SIMPLIFY4 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY4 'DEFINED-ON-LINE '482) 
(PUT 'SIMPLIFY4 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY4 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR DENOM (CONS (CAR (LIST (CONS 'TIMES DENOMSIMP))) (CDR DENOM)))
      (SETCAR NUM 1))) 
(PUT 'COMPARE_BOTHODDEXPTFACTORIAL 'NUMBER-OF-ARGS 3) 
(PUT 'COMPARE_BOTHODDEXPTFACTORIAL 'DEFINED-ON-LINE '493) 
(PUT 'COMPARE_BOTHODDEXPTFACTORIAL 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'COMPARE_BOTHODDEXPTFACTORIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPARE_BOTHODDEXPTFACTORIAL (NUM DENOM CHANGE)
    (PROG (DIFF)
      (COND
       ((NUMBERP
         (REVAL1 (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CAR (CDADAR DENOM))) T))
        (PROGN
         (COND
          ((AND (SQRT_TEST NUM) (SQRT_TEST DENOM))
           (PROGN
            (PROGN
             (SETQ DIFF
                     (REVAL1
                      (LIST 'DIFFERENCE (CAR (CDADAR NUM))
                            (CAR (CDADAR DENOM)))
                      T))
             (SETQ CHANGE (PLUS CHANGE 1))
             (COND ((GREATERP DIFF 0) (SIMPLIFY_SQRT5 NUM DENOM DIFF))
                   (T (SIMPLIFY_SQRT6 NUM DENOM DIFF)))
             NIL)
            NIL))
          ((OR (NOT_INT_OR_SQRT NUM) (NOT_INT_OR_SQRT DENOM)) (PROGN NIL))
          ((SQRT_TEST DENOM)
           (PROGN
            (SETQ DIFF
                    (REVAL1
                     (LIST 'DIFFERENCE (CADR (CADAR NUM)) (CADR (CADAR DENOM)))
                     T))
            (COND ((GREATERP DIFF 0) (SIMPLIFY_SQRT5 NUM DENOM DIFF))
                  (T (SIMPLIFY_SQRT6 NUM DENOM DIFF)))
            NIL))
          ((SQRT_TEST NUM)
           (PROGN
            (SETQ DIFF
                    (REVAL1
                     (LIST 'DIFFERENCE (CADR (CADAR NUM)) (CADR (CADAR DENOM)))
                     T))
            (COND ((GREATERP DIFF 0) (SIMPLIFY_SQRT7 NUM DENOM DIFF))
                  (T (SIMPLIFY_SQRT8 NUM DENOM DIFF)))
            NIL))
          ((AND (EQN (DIFFERENCE (CADDAR NUM) 1) 1)
                (EQN (DIFFERENCE (CADDAR DENOM) 1) 1))
           (PROGN
            (SETCAR NUM (CAR (LIST (CADAR NUM))))
            (SETCAR DENOM (CAR (LIST (CADAR DENOM))))
            (SETQ DIFF (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CADAR DENOM)) T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY5 NUM DENOM DIFF))
                  (T (SIMPLIFY6 NUM DENOM DIFF)))
            NIL))
          ((AND (EQN (DIFFERENCE (CADDAR NUM) 1) 1)
                (NOT (EQN (DIFFERENCE (CADDAR DENOM) 1) 1)))
           (PROGN
            (SETCAR NUM (CAR (LIST (CADAR NUM))))
            (SETCAR DENOM
                    (LIST (CAAR DENOM) (CADAR DENOM)
                          (DIFFERENCE (CAR (CDDAR DENOM)) 1)))
            (SETQ DIFF
                    (REVAL1 (LIST 'DIFFERENCE (CADAR NUM) (CAR (CDADAR DENOM)))
                            T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY5 NUM (CDAR DENOM) DIFF))
                  (T (SIMPLIFY6 NUM DENOM DIFF)))
            NIL))
          ((AND (NEQ (DIFFERENCE (CADDAR NUM) 1) 1)
                (EQ (DIFFERENCE (CADDAR DENOM) 1) 1))
           (PROGN
            (SETCAR NUM
                    (LIST (CAAR NUM) (CADAR NUM)
                          (DIFFERENCE (CAR (CDDAR NUM)) 1)))
            (SETCAR DENOM (CAR (LIST (CADAR DENOM))))
            (SETQ DIFF
                    (REVAL1 (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CADAR DENOM))
                            T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY5 NUM DENOM DIFF))
                  (T (SIMPLIFY6 (CDAR NUM) DENOM DIFF)))
            NIL))
          ((AND (NEQ (DIFFERENCE (CADDAR NUM) 1) 1)
                (NEQ (DIFFERENCE (CADDAR DENOM) 1) 1))
           (PROGN
            (SETCAR NUM
                    (LIST (CAAR NUM) (CADAR NUM)
                          (DIFFERENCE (CAR (CDDAR NUM)) 1)))
            (SETCAR DENOM
                    (LIST (CAAR DENOM) (CADAR DENOM)
                          (DIFFERENCE (CAR (CDDAR DENOM)) 1)))
            (SETQ DIFF
                    (REVAL1
                     (LIST 'DIFFERENCE (CAR (CDADAR NUM)) (CAR (CDADAR DENOM)))
                     T))
            (SETQ CHANGE (PLUS CHANGE 1))
            (COND ((GREATERP DIFF 0) (SIMPLIFY5 NUM (CDAR DENOM) DIFF))
                  (T (SIMPLIFY6 (CDAR NUM) DENOM DIFF)))
            NIL)))
         NIL)))
      (RETURN CHANGE))) 
(PUT 'SIMPLIFY_SQRT5 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT5 'DEFINED-ON-LINE '578) 
(PUT 'SIMPLIFY_SQRT5 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT5 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT5 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR NUM
              (LIST 'TIMES
                    (LIST 'ODDEXPT (CADAR DENOM)
                          (LIST 'PLUS (CADDAR NUM)
                                (LIST 'MINUS (LIST 'QUOTIENT 1 2))))
                    (LIST 'ODDEXPT (CAR (LIST (CONS 'TIMES NUMSIMP)))
                          (CADDAR NUM))))
      (SETCAR DENOM 1))) 
(PUT 'SIMPLIFY_SQRT6 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT6 'DEFINED-ON-LINE '590) 
(PUT 'SIMPLIFY_SQRT6 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT6 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT6 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR DENOM
              (LIST 'ODDEXPT (CAR (LIST (CONS 'TIMES DENOMSIMP)))
                    (LIST 'QUOTIENT 1 2)))
      (SETCAR (CDDAR NUM)
              (LIST 'PLUS (CADDAR NUM) (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))) 
(PUT 'SIMPLIFY_SQRT7 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT7 'DEFINED-ON-LINE '601) 
(PUT 'SIMPLIFY_SQRT7 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT7 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT7 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR DENOM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR NUM
              (LIST 'ODDEXPT (CAR (LIST (CONS 'TIMES NUMSIMP)))
                    (LIST 'QUOTIENT 1 2)))
      (SETCAR (CDDAR DENOM)
              (LIST 'PLUS (CADDAR DENOM) (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))) 
(PUT 'SIMPLIFY_SQRT8 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY_SQRT8 'DEFINED-ON-LINE '611) 
(PUT 'SIMPLIFY_SQRT8 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY_SQRT8 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_SQRT8 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CAR (CDADAR NUM)) I) T)
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR DENOM
              (LIST 'TIMES
                    (LIST 'ODDEXPT (CADAR NUM)
                          (LIST 'PLUS (CADDAR DENOM)
                                (LIST 'MINUS (LIST 'QUOTIENT 1 2))))
                    (LIST 'ODDEXPT (CAR (LIST (CONS 'TIMES DENOMSIMP)))
                          (CADDAR DENOM))))
      (SETCAR NUM 1))) 
(PUT 'SIMPLIFY5 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY5 'DEFINED-ON-LINE '624) 
(PUT 'SIMPLIFY5 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY5 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY5 (NUM DENOM DIFF)
    (PROG (NUMSIMP)
      (SETQ NUMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR DENOM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR NUM (CONS (CAR (LIST (CONS 'TIMES NUMSIMP))) (CDR NUM))))) 
(PUT 'SIMPLIFY6 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLIFY6 'DEFINED-ON-LINE '633) 
(PUT 'SIMPLIFY6 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'SIMPLIFY6 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY6 (NUM DENOM DIFF)
    (PROG (DENOMSIMP)
      (SETQ DIFF (MINUS DIFF))
      (SETQ DENOMSIMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T)
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIFF I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (REVAL1 (LIST 'PLUS (CADAR NUM) I) T) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCDR DENOM (CONS (CAR (LIST (CONS 'TIMES DENOMSIMP))) (CDR DENOM))))) 
(PUT 'TEST_FOR_FACTS 'NUMBER-OF-ARGS 2) 
(PUT 'TEST_FOR_FACTS 'DEFINED-ON-LINE '643) 
(PUT 'TEST_FOR_FACTS 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'TEST_FOR_FACTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TEST_FOR_FACTS (NUM DENOM)
    (PROG (TEST)
      (COND ((AND (TEST_NUM NUM) (TEST_DENOM DENOM)) (SETQ TEST T)))
      (RETURN TEST))) 
(FLAG '(TEST_FOR_FACTS) 'BOOLEAN) 
(PUT 'TEST_NUM 'NUMBER-OF-ARGS 1) 
(PUT 'TEST_NUM 'DEFINED-ON-LINE '659) 
(PUT 'TEST_NUM 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'TEST_NUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST_NUM (NUM)
    (PROG (TEST)
      (SETQ TEST NIL)
      (COND
       ((OR (EQCAR NUM 'TIMES) (EQCAR NUM 'ODDEXPT))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND NUM (NOT TEST))) (RETURN NIL)))
          (PROGN
           (COND
            ((AND (PAIRP (CAR NUM)) (EQ (CAAR NUM) 'FACTORIAL)) (SETQ TEST T))
            ((AND (PAIRP (CAR NUM)) (EQ (CAAR NUM) 'ODDEXPT))
             (COND
              ((AND (PAIRP (CADAR NUM)) (EQ (CAADAR NUM) 'FACTORIAL))
               (SETQ TEST T)))))
           (SETQ NUM (CDR NUM))
           NIL)
          (GO WHILELABEL))))
      (RETURN TEST))) 
(FLAG '(TEST_NUM) 'BOOLEAN) 
(PUT 'TEST_DENOM 'NUMBER-OF-ARGS 1) 
(PUT 'TEST_DENOM 'DEFINED-ON-LINE '683) 
(PUT 'TEST_DENOM 'DEFINED-IN-FILE 'SPECFN/SIMPFACT.RED) 
(PUT 'TEST_DENOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST_DENOM (DENOM)
    (PROG (TEST)
      (SETQ TEST NIL)
      (COND
       ((OR (EQCAR DENOM 'TIMES) (EQCAR DENOM 'ODDEXPT))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND DENOM (NOT TEST))) (RETURN NIL)))
          (PROGN
           (COND
            ((AND (PAIRP (CAR DENOM)) (EQ (CAAR DENOM) 'FACTORIAL))
             (SETQ TEST T))
            ((AND (PAIRP (CAR DENOM)) (EQ (CAAR DENOM) 'ODDEXPT))
             (COND
              ((AND (PAIRP (CADAR DENOM)) (EQ (CAADAR DENOM) 'FACTORIAL))
               (SETQ TEST T)))))
           (SETQ DENOM (CDR DENOM))
           NIL)
          (GO WHILELABEL))))
      (RETURN TEST))) 
(FLAG '(TEST_DENOM) 'BOOLEAN) 
(ENDMODULE) 