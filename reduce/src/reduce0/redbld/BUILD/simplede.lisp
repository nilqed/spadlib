(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPLEDE)) 
(FLUID '(*PRECISE FPS_SEARCH_DEPTH *PROTFG |PS:ORDER-LIMIT|)) 
(GLOBAL '(INCONSISTENT*)) 
(SHARE (LIST 'FPS_SEARCH_DEPTH)) 
(SETQ FPS_SEARCH_DEPTH (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 5)) 
(SWITCH (LIST 'TRACEFPS)) 
(DEFLIST '((BA SIMPIDEN) (INFSUM SIMPIDEN)) 'SIMPFN) 
(ARRAYFN 'ALGEBRAIC (LIST (LIST 'DFF 50))) 
(PUT 'SIMPLEDE 'PSOPFN 'SIMPLEDEEVAL) 
(PUT 'SIMPLEDEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLEDEEVAL 'DEFINED-ON-LINE '45) 
(PUT 'SIMPLEDEEVAL 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SIMPLEDEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLEDEEVAL (U)
    (PROG (RES USEVAR)
      (COND
       ((EQUAL (LENGTH U) 2)
        (PROGN
         (SETQ USEVAR 'Y)
         (SETQ RES (INT_SIMPLEDE (CAR U) (CADR U)))
         (COND
          ((EQ RES (MINUS 1)) (RETURN (SIMPLEDEEXIT (CAR U) (CADR U) 'Y))))
         NIL))
       ((EQUAL (LENGTH U) 3)
        (PROGN
         (SETQ USEVAR (CADDR U))
         (SETQ RES (INT_SIMPLEDE (CAR U) (CADR U)))
         (COND
          ((EQ RES (MINUS 1)) (RETURN (SIMPLEDEEXIT (CAR U) (CADR U) USEVAR))))
         NIL))
       (T (REDERR "Wrong number of Arguments for simplede")))
      (SETQ RES (SUBLIS '((ODDEXPT . EXPT) (BA . A) (NN . K)) RES))
      (RETURN
       (COND
        ((EQUAL (REVAL1 USEVAR T) USEVAR)
         (SUBLIS (LIST (CONS 'FF USEVAR)) RES))
        (T (SUBLIS (LIST (CONS 'FF (INTERN (GENSYM)))) RES)))))) 
(PUT 'INT_SIMPLEDE 'NUMBER-OF-ARGS 2) 
(FLAG '(INT_SIMPLEDE) 'OPFN) 
(PUT 'INT_SIMPLEDE 'DEFINED-ON-LINE '65) 
(PUT 'INT_SIMPLEDE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'INT_SIMPLEDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INT_SIMPLEDE (F X)
    (PROG (CAP_A DEGREE0FDE CAP_F J CAP_J NNN S IND DEQ EQQ REQQ AK TERMS LIST1
           LIST2 NMAX CAP_M CAP_R II M LEADCOEFF M0 LEN CAP_S RESULT PARAMETERS
           SOLVED *ALLFAC *PROTFG)
      (SETQ *PROTFG (AEVAL 'T))
      (SETQ NMAX (AEVAL FPS_SEARCH_DEPTH))
      (AEVAL (CLEAR (LIST 'A)))
      (AEVAL (OPERATOR (LIST 'A)))
      (AEVAL (OFF (LIST 'ALLFAC)))
      (AEVAL (DEPEND (LIST 'FF X)))
      (SETK (LIST 'DFF 0) (AEVAL F))
      (PROG (DEGREEOFDE)
        (SETQ DEGREEOFDE 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NMAX) DEGREEOFDE))
          (RETURN NIL)))
        (PROGN
         (SETK (LIST 'DFF DEGREEOFDE)
               (AEVAL* (LIST 'DF (LIST 'DFF (DIFFERENCE DEGREEOFDE 1)) X)))
         (SETQ EQQ
                 (AEVAL*
                  (LIST 'PLUS (LIST 'DFF DEGREEOFDE)
                        (PROG (J FORALL-RESULT)
                          (SETQ J 0)
                          (SETQ FORALL-RESULT 0)
                         LAB1
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                            (RETURN FORALL-RESULT)))
                          (SETQ FORALL-RESULT
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (AEVAL*
                                          (LIST 'TIMES (LIST 'A J)
                                                (LIST 'DFF J)))
                                         FORALL-RESULT)))
                          (SETQ J (PLUS2 J 1))
                          (GO LAB1)))))
         (SETQ EQQ (AEVAL* (LIST 'RECURSIONSIMPLIFY EQQ)))
         (SETQ EQQ (AEVAL* (LIST 'NUM EQQ)))
         (SETQ TERMS (AEVAL* (LIST 'LIST)))
         (SETQ LIST1 (AEVAL* (LIST 'CONVERTTOLIST EQQ (PLUS DEGREEOFDE 1))))
         (WHILE (EVALNEQ (AEVAL* LIST1) (AEVAL* (LIST 'LIST)))
                (PROGN
                 (SETQ LIST2 (AEVAL* (LIST 'LIST)))
                 (SETQ J (AEVAL* (LIST 'FASTPART LIST1 1)))
                 (SETQ CAP_J (AEVAL* J))
                 (SETQ LEN (AEVAL* (LIST 'FASTLENGTH LIST1)))
                 (PROG (I)
                   (SETQ I 2)
                  LAB
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LEN) I))
                     (RETURN NIL)))
                   (COND
                    ((BOOLVALUE*
                      (REVALX
                       (LIST 'TYPE_RATPOLY
                             (LIST 'QUOTIENT J (LIST 'FASTPART LIST1 I)) X)))
                     (SETQ CAP_J
                             (AEVAL*
                              (LIST 'PLUS CAP_J (LIST 'FASTPART LIST1 I)))))
                    (T
                     (SETQ LIST2
                             (AEVAL*
                              (LIST 'CONS (LIST 'FASTPART LIST1 I) LIST2)))))
                   (SETQ I
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            I))
                   (GO LAB))
                 (SETQ TERMS (AEVAL* (LIST 'CONS CAP_J TERMS)))
                 (SETQ LIST1 (AEVAL* (LIST 'REVERSE LIST2)))
                 (AEVAL* 'NIL)))
         (SETQ IND
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 0)
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS (AEVAL* (LIST 'A J)) NIL)))
                  LOOPLABEL
                   (SETQ J (PLUS2 J 1))
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                     (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'A J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ S (AEVAL* (LIST 'SAVESOLVE TERMS IND)))
         (COND ((EVALEQUAL (AEVAL* S) (AEVAL* (LIST 'LIST))) (AEVAL* 'NIL))
               (T
                (PROGN
                 (COND
                  ((BOOLVALUE* (REVALX *TRACEFPS))
                   (PROGN
                    (ASSGNPRI (AEVAL* "Solution: ") NIL 'FIRST)
                    (ASSGNPRI (AEVAL* S) NIL 'LAST))))
                 (SETQ RESULT (AEVAL* DEGREEOFDE))
                 (SETQ NMAX (AEVAL* 0)))))
         (AEVAL* 'NIL))
        (SETQ DEGREEOFDE
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 DEGREEOFDE))
        (GO LAB))
      (SETK 'DEGREEOFDE (AEVAL RESULT))
      (COND
       ((EVALEQUAL (AEVAL NMAX) 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (ASSGNPRI (AEVAL " successful search for DE") NIL 'ONLY)))))
       (T (RETURN (MINUS 1))))
      (PROG (SS)
        (SETQ SS (GETRLIST (AEVAL (LIST 'FIRST S))))
       LAB
        (COND ((NULL SS) (RETURN NIL)))
        ((LAMBDA (SS)
           (PROGN
            (SETQ SS
                    (AEVAL
                     (LIST 'SUB (LIST 'EQUAL (LIST 'A 'DEGREEOFDE) 1) SS)))
            (AEVAL (LIST 'SETZE (LIST 'LHS SS) (LIST 'RHS SS)))))
         (CAR SS))
        (SETQ SS (CDR SS))
        (GO LAB))
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ DEQ
              (AEVAL
               (LIST 'PLUS (LIST 'DF 'FF X 'DEGREEOFDE)
                     (PROG (J FORALL-RESULT)
                       (SETQ J 0)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:|
                          (LIST 'DIFFERENCE
                                (AEVAL* (LIST 'DIFFERENCE 'DEGREEOFDE 1)) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'A J)
                                             (LIST 'DF 'FF X J)))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1)))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (SETQ DEQ (AEVAL (LIST 'NUM DEQ)))
      (RETURN (AEVAL DEQ)))) 
(PUT 'FPS 'SIMPFN 'SIMP-FPS) 
(PUT 'SIMP-FPS 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-FPS 'DEFINED-ON-LINE '129) 
(PUT 'SIMP-FPS 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SIMP-FPS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-FPS (U)
    (PROG (GENS RES *FACTOR *PRECISE)
      (COND
       ((EQUAL (LENGTH U) 2)
        (PROGN
         (SETQ RES (PSALG (CAR U) (CADR U)))
         (COND ((EQ RES (MINUS 1)) (RETURN (FPSEXIT (CAR U) (CADR U) 0))))
         (RETURN (SUBLIS '((ODDEXPT . EXPT) (BA . A) (NN . K)) (SIMP RES)))))
       ((EQUAL (LENGTH U) 3)
        (PROGN
         (SETQ GENS (GENSYM))
         (SETQ RES (PSALG (SUBLIS (LIST (CONS (CADR U) GENS)) (CAR U)) GENS))
         (COND
          ((EQ RES (MINUS 1)) (RETURN (FPSEXIT (CAR U) (CADR U) (CADDR U)))))
         (SETQ RES (SUBLIS '((ODDEXPT . EXPT) (BA . A) (NN . K)) RES))
         (SETQ RES
                 (SUBF (CAR (SIMP RES))
                       (LIST
                        (LIST GENS 'PLUS (CADR U) (LIST 'MINUS (CADDR U))))))
         (RETURN RES)
         NIL))
       (T (REDERR "Wrong number of Arguments for FPS"))))) 
(PUT 'ASYMPTPOWERSERIES 'NUMBER-OF-ARGS 2) 
(FLAG '(ASYMPTPOWERSERIES) 'OPFN) 
(PUT 'ASYMPTPOWERSERIES 'DEFINED-ON-LINE '148) 
(PUT 'ASYMPTPOWERSERIES 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'ASYMPTPOWERSERIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMPTPOWERSERIES (F X)
    (LIST 'SUB (LIST 'EQUAL X (LIST 'QUOTIENT 1 X))
          (LIST 'FPS (LIST 'SUB (LIST 'EQUAL X (LIST 'QUOTIENT 1 X)) F) X))) 
(PUT 'FPSEXIT 'NUMBER-OF-ARGS 3) 
(PUT 'FPSEXIT 'DEFINED-ON-LINE '151) 
(PUT 'FPSEXIT 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'FPSEXIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FPSEXIT (A B Z)
    (PROGN
     (SETQ ERFG* NIL)
     (CONS (LIST (CONS (CONS (LIST 'FPS A B Z) 1) 1)) 1))) 
(PUT 'SIMPLEDEEXIT 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPLEDEEXIT 'DEFINED-ON-LINE '155) 
(PUT 'SIMPLEDEEXIT 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SIMPLEDEEXIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLEDEEXIT (A B Z)
    (PROGN
     (SETQ ERFG* NIL)
     (CONS (LIST (CONS (CONS (LIST 'SIMPLEDE A B Z) 1) 1)) 1))) 
(PUT 'PSALG 'NUMBER-OF-ARGS 2) 
(FLAG '(PSALG) 'OPFN) 
(PUT 'PSALG 'DEFINED-ON-LINE '159) 
(PUT 'PSALG 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'PSALG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PSALG (F X)
    (PROG (CAP_A DEGREE0FDE CAP_F J CAP_J NNN S IND DEQ EQQ REQQ AK TERMS LIST1
           LIST2 NMAX CAP_M CAP_R II M LEADCOEFF M0 LEN CAP_S RESULT PARAMETERS
           SOLVED *ALLFAC *PROTFG)
      (SETQ F (AEVAL (LIST 'RECURSIONSIMPLIFY F)))
      (SETQ *PROTFG (AEVAL 'T))
      (SETQ NMAX (AEVAL FPS_SEARCH_DEPTH))
      (AEVAL (CLEAR (LIST 'A)))
      (AEVAL (OPERATOR (LIST 'A)))
      (AEVAL (OFF (LIST 'ALLFAC)))
      (AEVAL (DEPEND (LIST 'FF X)))
      (SETK (LIST 'DFF 0) (AEVAL F))
      (COND ((BOOLVALUE* (REVALX (LIST 'POLYNOMQ F X))) (RETURN (AEVAL F))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY F X)))
        (RETURN (AEVAL (LIST 'RATALGO F X)))))
      (AEVAL (CLEARRULES (LIST 'SPECIAL*POCHHAMMER*RULES)))
      (AEVAL (CLEARRULES (LIST 'SPEC_FACTORIAL)))
      (AEVAL (CLEARRULES (LIST 'SPEC_POCHHAMMER)))
      (PROG (DEGREEOFDE)
        (SETQ DEGREEOFDE 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NMAX) DEGREEOFDE))
          (RETURN NIL)))
        (PROGN
         (SETK (LIST 'DFF DEGREEOFDE)
               (AEVAL* (LIST 'DF (LIST 'DFF (DIFFERENCE DEGREEOFDE 1)) X)))
         (SETQ EQQ
                 (AEVAL*
                  (LIST 'PLUS (LIST 'DFF DEGREEOFDE)
                        (PROG (J FORALL-RESULT)
                          (SETQ J 0)
                          (SETQ FORALL-RESULT 0)
                         LAB1
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                            (RETURN FORALL-RESULT)))
                          (SETQ FORALL-RESULT
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (AEVAL*
                                          (LIST 'TIMES (LIST 'A J)
                                                (LIST 'DFF J)))
                                         FORALL-RESULT)))
                          (SETQ J (PLUS2 J 1))
                          (GO LAB1)))))
         (SETQ EQQ (AEVAL* (LIST 'RECURSIONSIMPLIFY EQQ)))
         (SETQ EQQ (AEVAL* (LIST 'NUM EQQ)))
         (SETQ TERMS (AEVAL* (LIST 'LIST)))
         (SETQ LIST1 (AEVAL* (LIST 'CONVERTTOLIST EQQ (PLUS DEGREEOFDE 1))))
         (WHILE (EVALNEQ (AEVAL* LIST1) (AEVAL* (LIST 'LIST)))
                (PROGN
                 (SETQ LIST2 (AEVAL* (LIST 'LIST)))
                 (SETQ J (AEVAL* (LIST 'FASTPART LIST1 1)))
                 (SETQ CAP_J (AEVAL* J))
                 (SETQ LEN (AEVAL* (LIST 'FASTLENGTH LIST1)))
                 (PROG (I)
                   (SETQ I 2)
                  LAB
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LEN) I))
                     (RETURN NIL)))
                   (COND
                    ((BOOLVALUE*
                      (REVALX
                       (LIST 'TYPE_RATPOLY
                             (LIST 'QUOTIENT J (LIST 'FASTPART LIST1 I)) X)))
                     (SETQ CAP_J
                             (AEVAL*
                              (LIST 'PLUS CAP_J (LIST 'FASTPART LIST1 I)))))
                    (T
                     (SETQ LIST2
                             (AEVAL*
                              (LIST 'CONS (LIST 'FASTPART LIST1 I) LIST2)))))
                   (SETQ I
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            I))
                   (GO LAB))
                 (SETQ TERMS (AEVAL* (LIST 'CONS CAP_J TERMS)))
                 (SETQ LIST1 (AEVAL* (LIST 'REVERSE LIST2)))
                 (AEVAL* 'NIL)))
         (SETQ IND
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 0)
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS (AEVAL* (LIST 'A J)) NIL)))
                  LOOPLABEL
                   (SETQ J (PLUS2 J 1))
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE DEGREEOFDE 1) J))
                     (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'A J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ S (AEVAL* (LIST 'SAVESOLVE TERMS IND)))
         (COND ((EVALEQUAL (AEVAL* S) (AEVAL* (LIST 'LIST))) (AEVAL* 'NIL))
               (T
                (PROGN
                 (COND
                  ((BOOLVALUE* (REVALX *TRACEFPS))
                   (PROGN
                    (ASSGNPRI (AEVAL* "Solution: ") NIL 'FIRST)
                    (ASSGNPRI (AEVAL* S) NIL 'LAST))))
                 (SETQ RESULT (AEVAL* DEGREEOFDE))
                 (SETQ NMAX (AEVAL* 0)))))
         (AEVAL* 'NIL))
        (SETQ DEGREEOFDE
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 DEGREEOFDE))
        (GO LAB))
      (SETK 'DEGREEOFDE (AEVAL RESULT))
      (COND
       ((EVALEQUAL (AEVAL NMAX) 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (ASSGNPRI (AEVAL " successful search for DE") NIL 'ONLY)))))
       (T (RETURN (MINUS 1))))
      (PROG (SS)
        (SETQ SS (GETRLIST (AEVAL (LIST 'FIRST S))))
       LAB
        (COND ((NULL SS) (RETURN NIL)))
        ((LAMBDA (SS)
           (PROGN
            (SETQ SS
                    (AEVAL
                     (LIST 'SUB (LIST 'EQUAL (LIST 'A 'DEGREEOFDE) 1) SS)))
            (AEVAL (LIST 'SETZE (LIST 'LHS SS) (LIST 'RHS SS)))))
         (CAR SS))
        (SETQ SS (CDR SS))
        (GO LAB))
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ DEQ
              (AEVAL
               (LIST 'PLUS (LIST 'DF 'FF X 'DEGREEOFDE)
                     (PROG (J FORALL-RESULT)
                       (SETQ J 0)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:|
                          (LIST 'DIFFERENCE
                                (AEVAL* (LIST 'DIFFERENCE 'DEGREEOFDE 1)) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'A J)
                                             (LIST 'DF 'FF X J)))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1)))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (SETQ DEQ (AEVAL (LIST 'NUM DEQ)))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (ASSGNPRI (AEVAL "Differential equation is: ") NIL 'FIRST)
         (ASSGNPRI (AEVAL DEQ) NIL 'LAST))))
      (AEVAL (FACTOR (LIST 'BA)))
      (SETK 'REQ
            (AEVAL
             (LIST 'WHEREEXP (LIST 'LIST 'SUBST_RULES)
                   (LIST 'PSSUBST DEQ X 'BA 'NN))))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (ASSGNPRI (AEVAL "Recurrence equation is: ") NIL 'FIRST)
         (ASSGNPRI (AEVAL 'REQ) NIL 'LAST))))
      (SETQ IND (AEVAL (LIST 'LIST)))
      (PROG (II)
        (SETQ II (MINUS 50))
       LAB
        (COND ((MINUSP (DIFFERENCE 50 II)) (RETURN NIL)))
        (COND
         ((NOT
           (EVALEQUAL
            (AEVAL* (LIST 'COEFFN 'REQ (LIST 'BA (LIST 'PLUS 'NN II)) 1)) 0))
          (SETQ IND (AEVAL* (LIST 'CONS II IND)))))
        (SETQ II (PLUS2 II 1))
        (GO LAB))
      (SETQ CAP_M (AEVAL (LIST 'FIRST IND)))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (ASSGNPRI (AEVAL " M, ind, parameters : ") NIL 'FIRST)
         (ASSGNPRI (AEVAL CAP_M) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL IND) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL PARAMETERS) NIL 'LAST))))
      (SETQ LEADCOEFF
              (AEVAL
               (LIST 'NUM
                     (LIST 'COEFFN 'REQ (LIST 'BA (LIST 'PLUS 'NN CAP_M)) 1))))
      (SETQ NNN (AEVAL (LIST 'FASTLENGTH IND)))
      (AEVAL (LET '(SPECIAL*POCHHAMMER*RULES)))
      (AEVAL (LET '(SPEC_FACTORIAL)))
      (AEVAL (LET '(SPEC_POCHHAMMER)))
      (SETQ RESULT (AEVAL 0))
      (COND
       ((EVALEQUAL (AEVAL NNN) 1)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (ASSGNPRI (AEVAL "fps with finite number of non-zero coefficients")
                     NIL 'ONLY)))
         (SETQ CAP_R
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'SUB
                              (LIST 'EQUAL 'NN
                                    (LIST 'PLUS 'NN
                                          (LIST 'DIFFERENCE 1 CAP_M)))
                              (LIST 'MINUS
                                    (LIST 'REDUCT 'REQ
                                          (LIST 'BA (LIST 'PLUS 'NN CAP_M)))))
                        (LIST 'TIMES
                              (LIST 'SUB
                                    (LIST 'EQUAL 'NN
                                          (LIST 'PLUS 'NN
                                                (LIST 'DIFFERENCE 1 CAP_M)))
                                    (LIST 'LCOF 'REQ
                                          (LIST 'BA (LIST 'PLUS 'NN CAP_M))))
                              (LIST 'BA 'NN)))))
         (SETQ LEADCOEFF
                 (AEVAL
                  (LIST 'SUB
                        (LIST 'EQUAL 'NN
                              (LIST 'PLUS 'NN (LIST 'DIFFERENCE 1 CAP_M)))
                        LEADCOEFF)))
         (SETQ RESULT (AEVAL (LIST 'CONSTANTRE CAP_R LEADCOEFF 0 'NN X)))
         (COND
          ((EVALEQUAL (AEVAL RESULT) (AEVAL 'FAILED)) (SETQ RESULT (AEVAL 0))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL NNN) 2)
        (PROGN
         (SETQ M
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'DIFFERENCE (LIST 'FIRST IND)
                              (LIST 'SECOND IND)))))
         (SETQ CAP_R
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'SUB
                              (LIST 'EQUAL 'NN
                                    (LIST 'PLUS 'NN
                                          (LIST 'DIFFERENCE M CAP_M)))
                              (LIST 'MINUS
                                    (LIST 'REDUCT 'REQ
                                          (LIST 'BA (LIST 'PLUS 'NN CAP_M)))))
                        (LIST 'TIMES
                              (LIST 'SUB
                                    (LIST 'EQUAL 'NN
                                          (LIST 'PLUS 'NN
                                                (LIST 'DIFFERENCE M CAP_M)))
                                    (LIST 'LCOF 'REQ
                                          (LIST 'BA (LIST 'PLUS 'NN CAP_M))))
                              (LIST 'BA 'NN)))))
         (SETQ LEADCOEFF
                 (AEVAL
                  (LIST 'SUB
                        (LIST 'EQUAL 'NN
                              (LIST 'PLUS 'NN (LIST 'DIFFERENCE M CAP_M)))
                        LEADCOEFF)))
         (SETQ RESULT (AEVAL (LIST 'HYPERGEOMRE M CAP_R LEADCOEFF 0 'NN X))))))
      (COND
       ((EVALEQUAL (AEVAL RESULT) 0)
        (PROGN
         (SETQ TERMS
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 0)
                  STARTOVER
                   (COND
                    ((|AMINUSP:|
                      (LIST 'DIFFERENCE
                            (AEVAL* (LIST 'DIFFERENCE 'DEGREEOFDE 1)) J))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (COND
                            ((FREEOF (REVALX (LIST 'A J)) (REVALX X))
                             (AEVAL* (LIST 'LIST)))
                            (T (AEVAL* (LIST 'LIST 'T)))))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ J
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            J))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((|AMINUSP:|
                      (LIST 'DIFFERENCE
                            (AEVAL* (LIST 'DIFFERENCE 'DEGREEOFDE 1)) J))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            (COND
                             ((FREEOF (REVALX (LIST 'A J)) (REVALX X))
                              (AEVAL* (LIST 'LIST)))
                             (T (AEVAL* (LIST 'LIST 'T))))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            J))
                   (GO LOOPLABEL)))
         (COND
          ((EVALEQUAL (AEVAL TERMS) (AEVAL (LIST 'LIST)))
           (PROGN
            (SETK 'REQ
                  (AEVAL
                   (LIST 'PLUS (LIST 'BA (LIST 'PLUS 'K 'DEGREEOFDE))
                         (PROG (J FORALL-RESULT)
                           (SETQ J 0)
                           (SETQ FORALL-RESULT 0)
                          LAB1
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE
                                    (AEVAL* (LIST 'DIFFERENCE 'DEGREEOFDE 1))
                                    J))
                             (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (AEVAL*
                                    (LIST 'PLUS
                                          (AEVAL*
                                           (LIST 'TIMES
                                                 (LIST 'BA (LIST 'PLUS 'K J))
                                                 (LIST 'A J)))
                                          FORALL-RESULT)))
                           (SETQ J
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    J))
                           (GO LAB1)))))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL "DE has constant coefficients") NIL 'ONLY)
               (PROGN
                (ASSGNPRI (AEVAL "DE = ") NIL 'FIRST)
                (ASSGNPRI (AEVAL DEQ) NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL "RE = ") NIL 'FIRST)
                (ASSGNPRI (AEVAL 'REQ) NIL 'LAST))
               (AEVAL 'NIL))))
            (SETQ S (AEVAL 0))
            (SETK 'III (AEVAL 0))
            (WHILE
             (FREEOF (REVALX 'REQ) (REVALX (LIST 'BA (LIST 'PLUS 'K 'III))))
             (PROGN
              (SETQ S
                      (AEVAL*
                       (LIST 'PLUS S
                             (LIST 'TIMES (LIST 'LIMIT (LIST 'DFF 'III) X 0)
                                   (LIST 'EXPT X 'III)))))
              (SETK 'III (AEVAL* (LIST 'PLUS 'III 1)))))
            (SETQ M0 (AEVAL 'III))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL "i was found : ") NIL 'FIRST)
               (ASSGNPRI (AEVAL 'III) NIL 'LAST))))
            (COND
             ((EVALLEQ (AEVAL M0) (AEVAL (LIST 'DIFFERENCE 'DEGREEOFDE 1)))
              (PROGN
               (SETQ S
                       (AEVAL
                        (LIST 'SOLVE_LIN_REC 'REQ
                              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                (SETQ I (AEVAL* M0))
                                (COND
                                 ((|AMINUSP:|
                                   (LIST 'DIFFERENCE
                                         (AEVAL*
                                          (LIST 'DIFFERENCE 'DEGREEOFDE 1))
                                         I))
                                  (RETURN (MAKELIST NIL))))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 (AEVAL*
                                                  (LIST 'EQUAL (LIST 'BA I)
                                                        (LIST 'LIMIT
                                                              (LIST 'DFF I) X
                                                              0)))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ I
                                        ((LAMBDA (FORALL-RESULT)
                                           (AEVAL*
                                            (LIST 'PLUS FORALL-RESULT 1)))
                                         I))
                                (COND
                                 ((|AMINUSP:|
                                   (LIST 'DIFFERENCE
                                         (AEVAL*
                                          (LIST 'DIFFERENCE 'DEGREEOFDE 1))
                                         I))
                                  (RETURN (CONS 'LIST FORALL-RESULT))))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         (AEVAL*
                                          (LIST 'EQUAL (LIST 'BA I)
                                                (LIST 'LIMIT (LIST 'DFF I) X
                                                      0)))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))
               (COND
                ((BOOLVALUE* (REVALX *TRACEFPS))
                 (PROGN
                  (ASSGNPRI (AEVAL "solution : ") NIL 'FIRST)
                  (ASSGNPRI (AEVAL S) NIL 'LAST))))
               (SETQ S (AEVAL (LIST 'SUB (LIST 'EQUAL 'N 'NN) S)))
               (SETQ RESULT
                       (AEVAL
                        (LIST 'INFSUM
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT S (LIST 'FACTORIAL 'NN))
                                    (LIST 'EXPT X 'NN))
                              'NN 0 'INFINITY)))))
             (T (SETQ RESULT (AEVAL S))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (COND
       ((OR (EVALEQUAL (AEVAL RESULT) 0)
            (NOT (FREEOF (REVALX RESULT) (REVALX 'FAILED))))
        (RETURN (MINUS 1))))
      (SETQ ERFG* NIL)
      (SETQ RESULT (AEVAL RESULT))
      (SETQ RESULT
              (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'HGSPEC_POCHHAMMER) RESULT)))
      (SETQ RESULT (AEVAL (LIST 'VERBESSERE RESULT 'NIL)))
      (RETURN (AEVAL RESULT)))) 
(FLAG '(VERBESSERE) 'OPFN) 
(PUT 'VERBESSERE 'NUMBER-OF-ARGS 2) 
(PUT 'VERBESSERE 'DEFINED-ON-LINE '320) 
(PUT 'VERBESSERE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'VERBESSERE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VERBESSERE (X UU)
    (PROGN
     (COND
      ((EQCAR X 'PLUS)
       (CONS 'PLUS
             (PROG (XX FORALL-RESULT FORALL-ENDPTR)
               (SETQ XX (CDR X))
               (COND ((NULL XX) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (XX) (VERBESSERE XX NIL)) (CAR XX))
                                NIL)))
              LOOPLABEL
               (SETQ XX (CDR XX))
               (COND ((NULL XX) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (XX) (VERBESSERE XX NIL)) (CAR XX)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      ((NOT (EQCAR X 'INFSUM)) X)
      (T
       (PROGN
        (COND
         ((AND (EQCAR X 'INFSUM) (EQCAR (CADR X) 'QUOTIENT))
          (SETQ X
                  (LIST 'INFSUM
                        (LIST 'QUOTIENT (SIMPLIFY_EXPT (CADR (CADR X)))
                              (SIMPLIFY_EXPT (CADDR (CADR X))))))))
        (SETQ UU (CADR X))
        (COND
         ((AND (EQCAR X 'INFSUM) (EQCAR (CADR X) 'QUOTIENT))
          (PROGN (SETQ UU (INT_SIMPLIFY_FACTORIAL (AUXCOPY (CADR X)))))))
        (LIST 'INFSUM UU 'NN 0 'INFINITY)))))) 
(PUT 'ZERLEGE 'NUMBER-OF-ARGS 1) 
(PUT 'ZERLEGE 'DEFINED-ON-LINE '334) 
(PUT 'ZERLEGE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'ZERLEGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZERLEGE (U)
    (COND
     ((AND (FIXP U) (GREATERP U 0) (OR (LESSP U 10000) *IFACTOR))
      (PROGN
       (SETQ U (ZFACTOR U))
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J U)
        STARTOVER
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (J)
                    (PROG (JJ FORALL-RESULT FORALL-ENDPTR)
                      (SETQ JJ 1)
                      (COND ((MINUSP (DIFFERENCE (CDR J) JJ)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (CAR J) NIL)))
                     LOOPLABEL
                      (SETQ JJ (PLUS2 JJ 1))
                      (COND
                       ((MINUSP (DIFFERENCE (CDR J) JJ))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (CAR J) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR J)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ J (CDR J))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (J)
                    (PROG (JJ FORALL-RESULT FORALL-ENDPTR)
                      (SETQ JJ 1)
                      (COND ((MINUSP (DIFFERENCE (CDR J) JJ)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (CAR J) NIL)))
                     LOOPLABEL
                      (SETQ JJ (PLUS2 JJ 1))
                      (COND
                       ((MINUSP (DIFFERENCE (CDR J) JJ))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (CAR J) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR J)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ J (CDR J))
         (GO LOOPLABEL))))
     (T (LIST U)))) 
(PUT 'SIMPLIFY_EXPT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLIFY_EXPT 'DEFINED-ON-LINE '341) 
(PUT 'SIMPLIFY_EXPT 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SIMPLIFY_EXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY_EXPT (U)
    (PROG (UU EXPTLIST NONEXPTLIST ASSO NUMB EXPO)
      (SETQ UU U)
      (COND ((EQCAR U 'TIMES) (SETQ U (CDR U))))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (PAIRP (CAR U)) (OR (EQ (CAAR U) 'EXPT) (EQ (CAAR U) 'SQRT)))
           (PROGN
            (COND ((NUMBERP (CADAR U)) (SETQ NUMB (ZERLEGE (CADAR U))))
                  (T (SETQ NUMB (LIST (CADAR U)))))
            (SETQ EXPO
                    (COND ((EQ (CAAR U) 'SQRT) '((QUOTIENT 1 2)))
                          (T (CDDAR U))))
            (PROG ()
             WHILELABEL
              (COND ((NOT NUMB) (RETURN NIL)))
              (PROGN
               (COND
                ((SETQ ASSO (ATSOC (CAR NUMB) EXPTLIST))
                 (SETQ EXPTLIST
                         (CONS
                          (CONS (CAR NUMB)
                                (LIST (LIST 'PLUS (CAR EXPO) (CADR ASSO))))
                          (DELASC (CAR NUMB) EXPTLIST))))
                (T (SETQ EXPTLIST (CONS (CONS (CAR NUMB) EXPO) EXPTLIST))))
               (SETQ NUMB (CDR NUMB))
               NIL)
              (GO WHILELABEL))
            NIL))
          ((AND (IDP (CAR U)) (SETQ ASSO (ATSOC (CAR U) EXPTLIST)))
           (PROGN
            (SETQ EXPTLIST
                    (CONS (CONS (CAR U) (LIST (LIST 'PLUS 1 (CADR ASSO))))
                          (DELASC (CAR U) EXPTLIST)))))
          (T (SETQ NONEXPTLIST (CONS (CAR U) NONEXPTLIST))))
         (SETQ U (CDR U))
         NIL)
        (GO WHILELABEL))
      (COND ((NULL EXPTLIST) (RETURN UU)))
      (PROG (X)
        (SETQ X EXPTLIST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ NONEXPTLIST (CONS (CONS 'ODDEXPT X) NONEXPTLIST)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS (CAR UU) NONEXPTLIST)))) 
(FLUID '(RSOLVE**)) 
(PUT 'HYPERGEOMRE 'NUMBER-OF-ARGS 6) 
(FLAG '(HYPERGEOMRE) 'OPFN) 
(PUT 'HYPERGEOMRE 'DEFINED-ON-LINE '378) 
(PUT 'HYPERGEOMRE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'HYPERGEOMRE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERGEOMRE (M CAP_R LEADCOEFF DFFPOINTER K X)
    (PROG (DENR FRACT II M0 M1 C0 CK S C DF2 Q R2 LTERM NN S0 LEADCOEFF2)
      (SETQ DENR (AEVAL (LIST 'SOLVE LEADCOEFF K)))
      (SETQ M0 (AEVAL (LIST 'LIST)))
      (PROG (XX)
        (SETQ XX (GETRLIST (AEVAL DENR)))
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (COND
            ((BOOLVALUE* (REVALX (LIST 'TYPE_RATIONAL (LIST 'RHS XX))))
             (SETQ M0 (AEVAL (LIST 'CONS (LIST 'PLUS (LIST 'RHS XX) 1) M0))))))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (COND
       ((NOT (EVALEQUAL (AEVAL M0) (AEVAL (LIST 'LIST))))
        (SETQ M0 (AEVAL (LIST 'MAX M0))))
       (T (SETQ M0 (AEVAL 0))))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (ASSGNPRI (AEVAL "RE is of hypergeometric type") NIL 'ONLY)
         (PROGN
          (ASSGNPRI (AEVAL "Symmetry number mm := ") NIL 'FIRST)
          (ASSGNPRI (AEVAL M) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "RE: for all k >= ") NIL 'FIRST)
          (ASSGNPRI (AEVAL M0) NIL NIL)
          (ASSGNPRI (AEVAL ": a (") NIL NIL)
          (ASSGNPRI (AEVAL K) NIL NIL)
          (ASSGNPRI (AEVAL " + ") NIL NIL)
          (ASSGNPRI (AEVAL M) NIL NIL)
          (ASSGNPRI (AEVAL ") = ") NIL NIL)
          (ASSGNPRI (AEVAL (LIST 'TIMES CAP_R (LIST 'A K))) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "leadcoeff := ") NIL 'FIRST)
          (ASSGNPRI (AEVAL LEADCOEFF) NIL 'LAST))
         (AEVAL 'NIL))))
      (SETQ FRACT (AEVAL (LIST 'LIST)))
      (PROG (XX)
        (SETQ XX (GETRLIST (AEVAL DENR)))
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (COND
            ((BOOLVALUE* (REVALX (LIST 'TYPE_FRACTION (LIST 'RHS XX))))
             (SETQ FRACT
                     (AEVAL (LIST 'CONS (LIST 'DEN (LIST 'RHS XX)) FRACT))))))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (COND
       ((NOT (EVALEQUAL (AEVAL FRACT) (AEVAL (LIST 'LIST))))
        (PROGN
         (SETQ Q (AEVAL (LIST 'FIRST FRACT)))
         (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
               (AEVAL
                (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT X Q))
                      (LIST 'DFF DFFPOINTER))))
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "RE modified to nn= ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'QUOTIENT K Q)) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "=> f := ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))) NIL
                       'LAST)))))
         (SETQ S
                 (AEVAL
                  (LIST 'HYPERGEOMRE (LIST 'TIMES Q M)
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'QUOTIENT K Q)) CAP_R)
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'QUOTIENT K Q))
                              LEADCOEFF)
                        (LIST 'PLUS DFFPOINTER 10) K X)))
         (RETURN
          (AEVAL
           (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT X (LIST 'QUOTIENT 1 Q))) S)))
         (AEVAL 'NIL))))
      (COND
       ((EVALLESSP (AEVAL M0) 0)
        (PROGN
         (SETQ NN
                 (AEVAL
                  (LIST 'PLUS (LIST 'MINUS M0)
                        (LIST 'REMAINDER (LIST 'MINUS M0) M))))
         (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
               (SETQ DF2
                       (AEVAL
                        (LIST 'TIMES (LIST 'EXPT X NN)
                              (LIST 'DFF DFFPOINTER)))))
         (COND
          ((BOOLVALUE* (REVALX *TRACEFPS))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "working with ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'EXPT X NN)) NIL NIL)
             (ASSGNPRI (AEVAL "*f") NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "=> f :=") NIL 'FIRST)
             (ASSGNPRI (AEVAL DF2) NIL 'LAST)))))
         (SETQ S
                 (AEVAL
                  (LIST 'HYPERGEOMRE M
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K NN))
                              CAP_R)
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K NN))
                              LEADCOEFF)
                        (LIST 'PLUS DFFPOINTER 10) K X)))
         (RETURN (AEVAL (LIST 'UPDATE_COEFF S X (LIST 'MINUS NN)))))))
      (COND
       ((EVALGREATERP (AEVAL M0) 0)
        (PROGN
         (SETQ M1 (AEVAL (LIST 'LIST)))
         (PROG (XX)
           (SETQ XX (GETRLIST (AEVAL DENR)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (COND
               ((BOOLVALUE* (REVALX (LIST 'TYPE_RATIONAL (LIST 'RHS XX))))
                (SETQ M1
                        (AEVAL
                         (LIST 'APPEND
                               (LIST 'LIST (LIST 'PLUS (LIST 'RHS XX) 1))
                               M1))))))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (SETQ M1 (AEVAL (LIST 'MIN M1)))
         (COND
          ((EVALGREATERP (AEVAL M1) 0)
           (PROGN
            (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
                  (SETQ DF2
                          (AEVAL
                           (LIST 'TIMES (LIST 'EXPT X (LIST 'MINUS M1))
                                 (LIST 'DFF DFFPOINTER)))))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "a(k) = 0 for k < ") NIL 'FIRST)
                (ASSGNPRI (AEVAL M1) NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL "working with ") NIL 'FIRST)
                (ASSGNPRI (AEVAL (LIST 'EXPT X (LIST 'MINUS M1))) NIL NIL)
                (ASSGNPRI (AEVAL "*f") NIL 'LAST))
               (PROGN
                (ASSGNPRI (AEVAL "=> f :=") NIL 'FIRST)
                (ASSGNPRI (AEVAL DF2) NIL 'LAST)))))
            (SETQ S
                    (AEVAL
                     (LIST 'HYPERGEOMRE M
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K M1)) CAP_R)
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K M1))
                                 LEADCOEFF)
                           (LIST 'PLUS DFFPOINTER 10) K X)))
            (RETURN (AEVAL (LIST 'UPDATE_COEFF S X M1)))
            (AEVAL 'NIL)))))))
      (COND
       ((BOOLVALUE*
         (REVALX
          (PAIRP
           (ERRORSET*
            (LIST 'SIMPTAYLOR
                  (MKQUOTE
                   (LIST (MKQUOTE (LIST 'DFF DFFPOINTER)) (MKQUOTE X) 0 1)))
            NIL))))
        (PROGN
         (SETQ LTERM
                 (AEVAL
                  (LIST 'NUM
                        (LIST 'TAYLORTOSTANDARD
                              (LIST 'TAYLOR (LIST 'DFF DFFPOINTER) X 0 1)))))
         (SETQ NN (AEVAL 0))
         (COND
          ((BOOLVALUE*
            (COND ((MEMBER (LIST 'LOG X) (KERNELS (*Q2F (SIMP LTERM)))) T)
                  (T NIL)))
           (PROGN
            (COND
             ((EVALGEQ (AEVAL (LIST 'PLUS DFFPOINTER 10))
                       (AEVAL (LIST 'FIRST (LIST 'LENGTH 'DFF))))
              (RETURN (AEVAL 'FAILED))))
            (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))
                  (AEVAL (LIST 'DIFFERENCE (LIST 'DFF DFFPOINTER) LTERM)))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL "=> f :=") NIL 'FIRST)
               (ASSGNPRI (AEVAL (LIST 'DFF (LIST 'PLUS DFFPOINTER 10))) NIL
                         'LAST))))
            (SETQ S
                    (AEVAL
                     (LIST 'HYPERGEOMRE M 'R
                           (LIST 'TIMES LEADCOEFF (LIST 'DIFFERENCE K NN))
                           (LIST 'PLUS DFFPOINTER 10) K X)))
            (RETURN
             (COND ((EVALEQUAL (AEVAL S) (AEVAL 'FAILED)) (AEVAL S))
                   (T (AEVAL (LIST 'PLUS LTERM S)))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (SETQ S (AEVAL 0))
      (SETQ S0 (AEVAL 0))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS M0 (LIST 'DIFFERENCE M 1)))
                 I))
          (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP I 0)
           (SETK (LIST 'DFF (LIST 'PLUS DFFPOINTER I))
                 (AEVAL*
                  (LIST 'DF
                        (LIST 'DFF (LIST 'PLUS DFFPOINTER (DIFFERENCE I 1)))
                        X)))))
         (SETQ C0
                 (AEVAL*
                  (LIST 'LIMIT (LIST 'DFF (LIST 'PLUS DFFPOINTER I)) X 0)))
         (COND
          ((AND (BOOLVALUE* (REVALX (LISTP (REVAL1 C0 T))))
                (EVALEQUAL (AEVAL* (LIST 'FASTPART C0 0)) (AEVAL* 'LIMIT)))
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL* "Could not find the limit of: ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* (LIST 'DFF (LIST 'PLUS DFFPOINTER I))) NIL
                         NIL)
               (ASSGNPRI (AEVAL* ",") NIL NIL)
               (ASSGNPRI (AEVAL* X) NIL NIL)
               (ASSGNPRI (AEVAL* ",") NIL NIL)
               (ASSGNPRI 0 NIL 'LAST))))
            (AEVAL* (REDERR (REVALX "Problem using limit operator")))))
          (T
           (PROGN
            (SETQ C0 (AEVAL* (LIST 'QUOTIENT C0 (LIST 'FACTORIAL I))))
            (COND
             ((BOOLVALUE* (REVALX *TRACEFPS))
              (PROGN
               (ASSGNPRI (AEVAL* " a(") NIL 'FIRST)
               (ASSGNPRI I NIL NIL)
               (ASSGNPRI (AEVAL* ") = ") NIL NIL)
               (ASSGNPRI (AEVAL* C0) NIL 'LAST))))
            (COND
             ((NOT (EVALEQUAL (AEVAL* C0) 0))
              (PROGN
               (SETQ S0
                       (AEVAL*
                        (LIST 'PLUS S0 (LIST 'TIMES C0 (LIST 'EXPT X I)))))
               (COND
                ((EVALLESSP I (AEVAL* M0))
                 (SETQ S
                         (AEVAL*
                          (LIST 'PLUS S (LIST 'TIMES C0 (LIST 'EXPT X I))))))
                (T
                 (PROGN
                  (SETQ CK
                          (AEVAL*
                           (LIST 'HYPERGEOMRSOLVE
                                 (LIST 'SUB
                                       (LIST 'EQUAL K
                                             (LIST 'PLUS (LIST 'TIMES M K) I))
                                       CAP_R)
                                 K C0)))
                  (COND
                   ((BOOLVALUE* (REVALX *TRACEFPS))
                    (PROGN
                     (ASSGNPRI (AEVAL* " ck = ") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* CK) NIL 'LAST))))
                  (SETQ C (AEVAL* 1))
                  (SETQ CK (AEVAL* (LIST 'QUOTIENT CK C)))
                  (SETQ CK
                          (AEVAL*
                           (LIST 'WHEREEXP (LIST 'LIST 'HGSPEC_POCHHAMMER)
                                 CK)))
                  (COND
                   ((EVALEQUAL (AEVAL* CK) 0)
                    (SETQ S
                            (AEVAL*
                             (LIST 'PLUS S
                                   (LIST 'TIMES C0 (LIST 'EXPT X I))))))
                   ((EVALEQUAL (AEVAL* 'RSOLVE**) (AEVAL* 'FINITE))
                    (SETQ S
                            (AEVAL*
                             (LIST 'PLUS S
                                   (LIST 'TIMES C
                                         (LIST 'SUM
                                               (LIST 'TIMES CK
                                                     (LIST 'EXPT X
                                                           (LIST 'PLUS
                                                                 (LIST 'TIMES M
                                                                       K)
                                                                 I)))
                                               K))))))
                   (T
                    (SETQ S
                            (AEVAL*
                             (LIST 'PLUS S
                                   (LIST 'TIMES C
                                         (LIST 'INFSUM
                                               (LIST 'TIMES CK
                                                     (LIST 'EXPT X
                                                           (LIST 'PLUS
                                                                 (LIST 'TIMES M
                                                                       K)
                                                                 I))))))))))
                  (COND
                   ((BOOLVALUE* (REVALX *TRACEFPS))
                    (PROGN
                     (ASSGNPRI (AEVAL* " S = ") NIL 'FIRST)
                     (ASSGNPRI (AEVAL* S) NIL 'LAST))))
                  (AEVAL* 'NIL)))))))))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL S)))) 
(AEVAL (LET '((EQUAL (INFSUM 0) 0)))) 
(FLAG '(SAVESOLVE TYPE_FRACTION TYPE_RATIONAL) 'OPFN) 
(PUT 'CONVERTTOLIST 'NUMBER-OF-ARGS 2) 
(FLAG '(CONVERTTOLIST) 'OPFN) 
(PUT 'CONVERTTOLIST 'DEFINED-ON-LINE '505) 
(PUT 'CONVERTTOLIST 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'CONVERTTOLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONVERTTOLIST (EXPRESS LEN)
    (PROGN
     (SETQ LEN (AEVAL (LIST 'FASTLENGTH EXPRESS)))
     (PROG (I FORALL-RESULT FORALL-ENDPTR)
       (SETQ I 1)
       (COND
        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LEN) I))
         (RETURN (MAKELIST NIL))))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS (AEVAL* (LIST 'FASTPART EXPRESS I)) NIL)))
      LOOPLABEL
       (SETQ I
               ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                I))
       (COND
        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LEN) I))
         (RETURN (CONS 'LIST FORALL-RESULT))))
       (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'FASTPART EXPRESS I)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'TYPE_FRACTION 'NUMBER-OF-ARGS 1) 
(PUT 'TYPE_FRACTION 'DEFINED-ON-LINE '510) 
(PUT 'TYPE_FRACTION 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'TYPE_FRACTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPE_FRACTION (NUM)
    ((LAMBDA (NUM1)
       (COND
        ((AND (PAIRP NUM1) (FIXP (CAR NUM1)) (FIXP (CDR NUM1))
              (NOT (ONEP (CDR NUM1))))
         NUM)
        (T NIL)))
     (SIMP NUM))) 
(PUT 'TYPE_RATIONAL 'NUMBER-OF-ARGS 1) 
(PUT 'TYPE_RATIONAL 'DEFINED-ON-LINE '515) 
(PUT 'TYPE_RATIONAL 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'TYPE_RATIONAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPE_RATIONAL (NUM)
    ((LAMBDA (NUM1)
       (COND
        ((AND (PAIRP NUM1) (OR (FIXP (CAR NUM1)) (NULL (CAR NUM1)))
              (FIXP (CDR NUM1)))
         T)
        (T NIL)))
     (SIMP NUM))) 
(PUT 'TYPE_RATPOLY 'NUMBER-OF-ARGS 2) 
(FLAG '(TYPE_RATPOLY) 'OPFN) 
(PUT 'TYPE_RATPOLY 'DEFINED-ON-LINE '519) 
(PUT 'TYPE_RATPOLY 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'TYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPE_RATPOLY (EXPRN VAR)
    (COND
     ((AND (BOOLVALUE* (REVALX (LIST 'POLYNOMQ (LIST 'DEN EXPRN) VAR)))
           (BOOLVALUE* (REVALX (LIST 'POLYNOMQ (LIST 'NUM EXPRN) VAR))))
      (AEVAL 'T))
     (T (AEVAL 'NIL)))) 
(PUT 'SAVESOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'SAVESOLVE 'DEFINED-ON-LINE '523) 
(PUT 'SAVESOLVE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SAVESOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAVESOLVE (X Y)
    (PROG (*CRAMER)
      (ON (LIST 'CRAMER))
      (RETURN
       (PROGN
        (SWITCH (LIST 'SOLVEINCONSISTENT))
        (ON (LIST 'SOLVEINCONSISTENT))
        (SETQ INCONSISTENT* NIL)
        (COND
         ((AND
           (PAIRP
            (SETQ X (ERRORSET* (LIST 'SOLVEEVAL (MKQUOTE (LIST X Y))) NIL)))
           (NOT INCONSISTENT*))
          (PROGN
           (SETQ X (CAR X))
           (COND ((EQUAL X '(LIST)) X) ((EQCAR (CADR X) 'EQUAL) (LIST 'LIST X))
                 (T X))))
         (T (LIST 'LIST))))))) 
(PUT 'SETZE 'NUMBER-OF-ARGS 2) 
(FLAG '(SETZE) 'OPFN) 
(PUT 'SETZE 'DEFINED-ON-LINE '541) 
(PUT 'SETZE 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'SETZE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETZE (X Y) (LET (LIST (LIST 'EQUAL X Y)))) 
(PUT 'POLYNOMQ 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMQ 'DEFINED-ON-LINE '544) 
(PUT 'POLYNOMQ 'DEFINED-IN-FILE 'SPECFN/SIMPLEDE.RED) 
(PUT 'POLYNOMQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMQ (X VAR)
    (COND ((NOT (FIXP (CDR (SIMP X)))) NIL)
          (T
           (PROG (KERNS KERN)
             (SETQ KERNS (KERNELS (*Q2F (SIMP X))))
            AA
             (COND ((NULL KERNS) (RETURN T)))
             (SETQ KERN (FIRST KERNS))
             (SETQ KERNS (CDR KERNS))
             (COND ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                   (T (GO AA))))))) 
(FLAG '(POLYNOMQ) 'OPFN) 
(FLAG '(POLYNOMQ TYPE_RATPOLY) 'BOOLEAN) 
(AEVAL (OPERATOR (LIST 'UPDATE_COEFF))) 
(SETK 'UPDATE_COEFF_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'PLUS
                         (LIST 'UPDATE_COEFF 'A (LIST '~ 'X) (LIST '~ 'M))
                         (LIST 'UPDATE_COEFF 'B (LIST '~ 'X) (LIST '~ 'M))))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST 'TIMES (LIST '~ 'C) (LIST '~ 'A))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'TIMES 'C
                               (LIST 'UPDATE_COEFF 'A (LIST '~ 'X)
                                     (LIST '~ 'M)))
                         (LIST 'FREEOF 'C 'X)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST 'MINUS (LIST '~ 'A)) (LIST '~ 'X)
                         (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'UPDATE_COEFF 'A (LIST '~ 'X) (LIST '~ 'M))))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF
                         (LIST 'QUOTIENT (LIST '~ 'A) (LIST '~ 'C))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'UPDATE_COEFF 'A (LIST '~ 'X)
                                     (LIST '~ 'M))
                               'C)
                         (LIST 'AND (LIST 'FREEOF 'C 'X) (LIST 'NEQ 'C 1))))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST '~ 'X) (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'EXPT 'X (LIST 'PLUS 'M 1)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST '~ 'C) (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'TIMES 'C (LIST 'EXPT 'X 'M))
                         (LIST 'FREEOF 'C 'X)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST 'INFSUM (LIST '~ 'XX))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'INFSUM (LIST 'UPDATE_COEFF 'XX 'X 'M)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF
                         (LIST 'TIMES (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                               (LIST '~ 'XX))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'EXPT 'X (LIST 'PLUS 'J 'M 1))
                         (LIST 'EQUAL 'X 'XX)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF
                         (LIST 'TIMES (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                               (LIST 'EXPT (LIST '~ 'XX) (LIST '~ 'JJ)))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'EXPT 'X (LIST 'PLUS 'J 'JJ 'M))
                         (LIST 'EQUAL 'X 'XX)))
             (LIST 'REPLACEBY
                   (LIST 'UPDATE_COEFF (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                         (LIST '~ 'X) (LIST '~ 'M))
                   (LIST 'EXPT 'X (LIST 'PLUS 'J 'M)))))) 
(AEVAL (LET '(UPDATE_COEFF_RULES))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SETK 'FPS*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FPS (LIST 'DILOG (LIST '~ 'X)) (LIST '~ 'X) 1)
                   (LIST 'INFSUM
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'K)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'DIFFERENCE 'X 1) 'K)
                                     (LIST 'EXPT 'K 2)))
                         'K 1 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FPS (LIST 'POLYLOG (LIST '~ 'S) (LIST '~ 'X))
                         (LIST '~ 'X) 0)
                   (LIST 'INFSUM
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'K)
                               (LIST 'QUOTIENT (LIST 'EXPT 'X 'K)
                                     (LIST 'EXPT 'K 'S)))
                         'K 1 'INFINITY))))) 
(LET '(FPS*RULES)) 
(CLEAR (LIST 'FPS*RULES)) 
(ENDMODULE) 