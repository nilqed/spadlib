(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ZEILBERG)) 
(CREATE-PACKAGE '(ZEILBERG) '(CONTRIB SUM)) 
(FLUID '(ZB_VERSION)) 
(SETQ ZB_VERSION "package zeilberg, version 1.1, Feb. 15, 1995") 
(GLOBAL '(INCONSISTENT*)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SHARE (LIST 'ZB_ORDER)) 
(SETQ ZB_ORDER (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (AEVAL 5))) 
(SETK 'GOSPER_REPRESENTATION (AEVAL 'NIL)) 
(SETK 'ZEILBERGER_REPRESENTATION (AEVAL 'NIL)) 
((LAMBDA (*MSG) (AEVAL (OPERATOR (LIST 'HYPERGEOM 'POCHHAMMER)))) NIL) 
(OPERATOR (LIST 'LOCAL_GAMMA 'LOCAL_PROD)) 
(SETK 'ONERULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'GAMMA (LIST '~ 'ZB_X)) 1)
             (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'ZB_X) (LIST '~ 'ZB_Y))
                   1)
             (LIST 'REPLACEBY (LIST 'FACTORIAL (LIST '~ 'ZB_X)) 1)
             (LIST 'REPLACEBY
                   (LIST 'POCHHAMMER (LIST '~ 'ZB_X) (LIST '~ 'ZB_Y)) 1)))) 
(SETK 'ONERULES2
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'SUMM (LIST '~ 'ZB_X)) 1)
             (LIST 'REPLACEBY
                   (LIST 'HYPERGEOM (LIST '~ 'ZB_X1) (LIST '~ 'ZB_X2)
                         (LIST '~ 'ZB_X3))
                   1)))) 
(SETK 'GAMMATOFACTORIAL
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'GAMMA (LIST '~ 'A))
                   (LIST 'FACTORIAL (LIST 'DIFFERENCE 'A 1)))))) 
(SETK 'ZB_BINOMIALRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'N) 0) 1)
             (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'N) (LIST '~ 'N)) 1)
             (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'WHEN 0
                         (LIST 'AND (LIST 'FIXP 'K) (LIST 'LESSP 'K 0))))
             (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'WHEN 0
                         (LIST 'AND (LIST 'FIXP (LIST 'DIFFERENCE 'N 'K))
                               (LIST 'LESSP (LIST 'DIFFERENCE 'N 'K) 0))))
             (LIST 'REPLACEBY (LIST 'BINOMIAL (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'FACTORIAL 'N)
                               (LIST 'TIMES (LIST 'FACTORIAL 'K)
                                     (LIST 'FACTORIAL
                                           (LIST 'DIFFERENCE 'N 'K))))
                         (LIST 'AND (LIST 'FIXP 'K) (LIST 'FIXP 'N))))))) 
(LET '(ZB_BINOMIALRULES)) 
(SWITCH
 (LIST (LIST 'EQUAL 'ZB_FACTOR 'ON) 'ZB_TIMER 'ZB_PROOF 'ZB_TRACE
       'ZB_INHOMOGENEOUS)) 
(SETK 'ZB_DIRECTION (AEVAL 'DOWN)) 
(PUT 'GOSPER* 'NUMBER-OF-ARGS 2) 
(PUT 'GOSPER* 'DEFINED-ON-LINE '122) 
(PUT 'GOSPER* 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GOSPER* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GOSPER* (U V)
    (PROG (X)
      (SETQ X
              (COND ((EQUAL (LENGTH V) 1) (GOSPER0 U (REVAL1 (CAR V) NIL)))
                    ((EQUAL (LENGTH V) 2)
                     (GOSPERBORDERS U (REVAL1 (CAR V) NIL) 0
                      (REVAL1 (CADR V) NIL)))
                    ((EQUAL (LENGTH V) 3)
                     (GOSPERBORDERS U (REVAL1 (CAR V) NIL)
                      (REVAL1 (CADR V) NIL) (REVAL1 (CADDR V) NIL)))
                    (T (REDERR "Illegal number of arguments to SUM"))))
      (RETURN (SIMP X)))) 
(PUT 'GOSPER-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GOSPER-EVAL 'DEFINED-ON-LINE '149) 
(PUT 'GOSPER-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GOSPER-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GOSPER-EVAL (U)
    (PROGN
     ((LAMBDA (*ARGNOCHK) (ARGNOCHK (CONS 'GOSPER U))) T)
     (PREPSQ* (GOSPER* (CAR U) (CDR U))))) 
(PUT 'GOSPER 'NUMBER-OF-ARGS 2) 
(PUT 'GOSPER 'PSOPFN 'GOSPER-EVAL) 
(PUT 'GOSPERBORDERS 'NUMBER-OF-ARGS 4) 
(FLAG '(GOSPERBORDERS) 'OPFN) 
(PUT 'GOSPERBORDERS 'DEFINED-ON-LINE '157) 
(PUT 'GOSPERBORDERS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GOSPERBORDERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GOSPERBORDERS (FUNC K K0 K1)
    (PROG (TMP GOSPER2 *FACTOR)
      (SETQ GOSPER2 (AEVAL (LIST 'GOSPER0 FUNC K)))
      (SETQ TMP
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'SUB (LIST 'EQUAL K K1) GOSPER2)
                     (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K0 1))
                           GOSPER2))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_FACTOR))
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (RETURN (AEVAL (LIST 'QUOTIENT (LIST 'NUM TMP) (LIST 'DEN TMP))))))
       (T (RETURN (AEVAL TMP)))))) 
(PUT 'GOSPER0 'NUMBER-OF-ARGS 2) 
(FLAG '(GOSPER0) 'OPFN) 
(PUT 'GOSPER0 'DEFINED-ON-LINE '173) 
(PUT 'GOSPER0 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GOSPER0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GOSPER0 (FUNC K)
    (PROG (TMP)
      (SETQ TMP (AEVAL (LIST 'GOSPER1 FUNC K)))
      (COND
       ((EVALEQUAL (AEVAL TMP) (AEVAL 'ZB_GANCFSE))
        (AEVAL
         (REDERR (REVALX "Gosper algorithm: no closed form solution exists"))))
       (T (RETURN (AEVAL TMP)))))) 
(PUT 'GOSPER1 'NUMBER-OF-ARGS 2) 
(FLAG '(GOSPER1) 'OPFN) 
(PUT 'GOSPER1 'DEFINED-ON-LINE '183) 
(PUT 'GOSPER1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GOSPER1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GOSPER1 (FUNC K)
    (PROG (DEXP GEXP D G DG DEGREE DOWNMAX FACJ PARTJ J JJ P R1 R2 POLYNOMIALS
           SOL *EXP *FACTOR EQUATIONS EQUATIONLIST F VARLIST S L)
      (AEVAL
       (CLEAR
        (LIST 'GOSPER_REPRESENTATION 'ZEILBERGER_REPRESENTATION
              'RATIONAL_CERTIFICATE)))
      (AEVAL (ON (LIST 'EXP)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'POLYNOMQQ FUNC K)))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (ASSGNPRI (AEVAL "Gosper algorithm applicable") NIL 'ONLY)))
         (SETQ POLYNOMIALS (AEVAL (LIST 'LIST FUNC 1 1)))))
       (T
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (AEVAL (OFF (LIST 'EXP)))
         (SETQ DG
                 (AEVAL
                  (LIST 'SIMPLIFY_COMBINATORIAL
                        (LIST 'QUOTIENT FUNC
                              (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                                    FUNC)))))
         (COND ((EVALEQUAL (AEVAL DG) 0) (RETURN 0)))
         (SETQ D (AEVAL (LIST 'NUM DG)))
         (SETQ G (AEVAL (LIST 'DEN DG)))
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
            (ASSGNPRI (AEVAL K) NIL NIL)
            (ASSGNPRI (AEVAL ")/a(") NIL NIL)
            (ASSGNPRI (AEVAL K) NIL NIL)
            (ASSGNPRI (AEVAL "-1):=") NIL NIL)
            (ASSGNPRI (AEVAL DG) NIL 'LAST))))
         (COND
          ((NOT
            (AND (BOOLVALUE* (REVALX (LIST 'POLYNOMQ4 D K)))
                 (BOOLVALUE* (REVALX (LIST 'POLYNOMQ4 G K)))))
           (AEVAL (REDERR (REVALX "Gosper algorithm not applicable")))))
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (ASSGNPRI (AEVAL "Gosper algorithm applicable") NIL 'ONLY)))
         (SETQ POLYNOMIALS (AEVAL (LIST 'DETERMINE_POLYNOMIALS2 D G K)))
         (SETQ D (AEVAL 'NIL))
         (SETQ G (AEVAL 'NIL))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (ASSGNPRI (AEVAL "flag: determine polynomials") NIL 'ONLY)
         (AEVAL (SHOWTIME 'NIL)))))
      (SETQ P (AEVAL (LIST 'FIRST POLYNOMIALS)))
      (SETQ R1 (AEVAL (LIST 'SECOND POLYNOMIALS)))
      (SETQ R2 (AEVAL (LIST 'THIRD POLYNOMIALS)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "p:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL P) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "q:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL R1) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "r:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL R2) NIL 'LAST)))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (AEVAL (ON (LIST 'EXP)))
      (SETQ DEGREE (AEVAL (LIST 'MAXDEGF R1 R2 P K)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "degreebound:=") NIL 'FIRST)
         (ASSGNPRI (AEVAL DEGREE) NIL 'LAST))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (ASSGNPRI (AEVAL "flag: maxdegf") NIL 'ONLY)
         (AEVAL (SHOWTIME 'NIL)))))
      (COND ((EVALLESSP (AEVAL DEGREE) 0) (RETURN (AEVAL 'ZB_GANCFSE))))
      (SETQ F
              (PROG (J FORALL-RESULT)
                (SETQ J 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DEGREE) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES (LIST 'ZB_F J) (LIST 'EXPT K J)))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1)))
      (SETQ EQUATIONS
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES
                                 (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                                       R1)
                                 F)
                           (LIST 'TIMES R2
                                 (LIST 'SUB
                                       (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                                       F)))
                     P)))
      (AEVAL (ON (LIST 'EXP)))
      (SETQ EQUATIONLIST (AEVAL (LIST 'COEFF EQUATIONS K)))
      (SETQ VARLIST
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 0)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DEGREE) J))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (LIST 'ZB_F J)) NIL)))
               LOOPLABEL
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DEGREE) J))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'ZB_F J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH EQUATIONLIST)))
      (SETQ DOWNMAX
              (AEVAL (LIST 'MAX (LIST 'PLUS (LIST 'DIFFERENCE DEGREE L) 1) 0)))
      (SETQ SOL (AEVAL (LIST 'LIST)))
      (SETK 'SOL2 (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J (AEVAL* DEGREE))
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE (AEVAL* DOWNMAX) J)))
          (RETURN NIL)))
        (PROGN
         (AEVAL* (OFF (LIST 'FACTOR)))
         (SETQ PARTJ
                 (AEVAL*
                  (LIST 'SUB SOL
                        (LIST 'PART EQUATIONLIST
                              (LIST 'PLUS (LIST 'DIFFERENCE L DEGREE) J)))))
         (AEVAL* (ON (LIST 'EXP)))
         (SETQ JJ (AEVAL* DEGREE))
         (WHILE
          (AND (FREEOF (REVALX PARTJ) (REVALX (LIST 'ZB_F JJ)))
               (EVALGEQ (AEVAL* JJ) 0))
          (SETQ JJ (AEVAL* (LIST 'DIFFERENCE JJ 1))))
         (SETQ FACJ (AEVAL* (LIST 'COEFF PARTJ (LIST 'ZB_F JJ))))
         (AEVAL* (OFF (LIST 'EXP)))
         (AEVAL* (ON (LIST 'FACTOR)))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'ARGLENGTH FACJ)) 2)
           (PROGN
            (SETK 'SOLJ
                  (AEVAL*
                   (LIST 'LIST
                         (LIST 'EQUAL (LIST 'ZB_F JJ)
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (LIST 'PART FACJ 1)
                                           (LIST 'PART FACJ 2)))))))
            (AEVAL* (OFF (LIST 'FACTOR)))
            (SETQ SOL (AEVAL* (LIST 'APPEND 'SOLJ (LIST 'SUB 'SOLJ SOL))))
            (SETK 'SOL2
                  (AEVAL*
                   (LIST 'APPEND (LIST 'LIST (LIST 'ZB_F JJ)) 'SOL2))))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT)
                   (AEVAL* (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'ARGLENGTH SOL)) (AEVAL DEGREE))
        (PROGN
         (SETK 'TMP (AEVAL 'T))
         (SETQ J (AEVAL 0))
         (WHILE
          (AND (BOOLVALUE* (REVALX 'TMP)) (EVALLEQ (AEVAL* J) (AEVAL* DEGREE)))
          (PROGN
           (COND
            ((FREEOF (REVALX 'SOL2) (REVALX (LIST 'ZB_F J)))
             (PROGN
              (SETQ SOL
                      (AEVAL*
                       (LIST 'APPEND
                             (LIST 'LIST (LIST 'EQUAL (LIST 'ZB_F J) 0))
                             (LIST 'SUB (LIST 'EQUAL (LIST 'ZB_F J) 0) SOL))))
              (SETK 'TMP (AEVAL* 'NIL)))))
           (SETQ J (AEVAL* (LIST 'PLUS J 1)))
           (AEVAL* 'NIL))))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (ASSGNPRI (AEVAL "flag: sol") NIL 'ONLY)
         (AEVAL (SHOWTIME 'NIL)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SUB SOL EQUATIONS)) 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_PROOF))
           (SETK 'GOSPER_REPRESENTATION (AEVAL (LIST 'LIST P R1 R2 'NIL)))))
         (RETURN (AEVAL 'ZB_GANCFSE)))))
      (SETQ F (AEVAL (LIST 'SUB SOL F)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_PROOF))
        (SETK 'GOSPER_REPRESENTATION (AEVAL (LIST 'LIST P R1 R2 F)))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_PROOF))
        (COND
         ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'DOWN))
          (PROGN
           (SETK 'RATIONAL_CERTIFICATE
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'QUOTIENT
                              (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) R1)
                              P)
                        F)))
           (SETQ S (AEVAL (LIST 'TIMES 'RATIONAL_CERTIFICATE FUNC)))))
         (T
          (PROGN
           (SETK 'RATIONAL_CERTIFICATE
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                              (LIST 'QUOTIENT R2 P))
                        F)))
           (SETQ S
                   (AEVAL
                    (LIST 'TIMES 'RATIONAL_CERTIFICATE
                          (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                                FUNC))))))))
       (T
        (SETQ S
                (AEVAL
                 (LIST 'TIMES
                       (LIST 'QUOTIENT
                             (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) R1) P)
                       F FUNC)))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "f:=") NIL 'FIRST)
         (ASSGNPRI (AEVAL F) NIL 'LAST))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (ASSGNPRI (AEVAL "flag: simplify comb von sol:=") NIL 'ONLY)
         (AEVAL (SHOWTIME 'NIL)))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_FACTOR))
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (SETQ S (AEVAL (LIST 'QUOTIENT (LIST 'NUM S) (LIST 'DEN S))))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (ASSGNPRI (AEVAL "flag: num(s)/den(s) under factor") NIL 'ONLY)
         (AEVAL (SHOWTIME 'NIL)))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "Gosper algorithm successful") NIL 'ONLY)))
      (COND
       ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'DOWN)) (RETURN (AEVAL S)))
       (T
        (RETURN
         (AEVAL (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K 1)) S))))))) 
(PUT 'SUMRECURSION-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SUMRECURSION-EVAL 'DEFINED-ON-LINE '333) 
(PUT 'SUMRECURSION-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMRECURSION-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUMRECURSION-EVAL (U)
    (PROGN
     (COND
      ((EQUAL (LENGTH U) 3)
       (SUMRECURSION0 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) 1 ZB_ORDER))
      ((EQUAL (LENGTH U) 4)
       (SUMRECURSION0 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) (REVAL1 (CADDDR U) NIL)
        (REVAL1 (CADDDR U) NIL)))
      (T (REDERR "illegal number of arguments"))))) 
(PUT 'SUMRECURSION 'PSOPFN 'SUMRECURSION-EVAL) 
(PUT 'SUMRECURSION0 'NUMBER-OF-ARGS 5) 
(FLAG '(SUMRECURSION0) 'OPFN) 
(PUT 'SUMRECURSION0 'DEFINED-ON-LINE '358) 
(PUT 'SUMRECURSION0 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMRECURSION0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUMRECURSION0 (FUNC SECUNDUS N MINI MAXI)
    (PROG (*FACTOR *EXP)
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN
       (AEVAL (LIST 'PART (LIST 'SUMRECURSION1 FUNC SECUNDUS N MINI MAXI) 1))))) 
(PUT 'SUMRECURSION1 'NUMBER-OF-ARGS 5) 
(FLAG '(SUMRECURSION1) 'OPFN) 
(PUT 'SUMRECURSION1 'DEFINED-ON-LINE '365) 
(PUT 'SUMRECURSION1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMRECURSION1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUMRECURSION1 (FUNC SECUNDUS N MINI MAXI)
    (PROG (RESULT1 ANK B C D G BC DG J JJ INHOMOGENEOUS K AA BB *FACTOR *EXP
           ORDER1)
      (AEVAL
       (CLEAR
        (LIST 'GOSPER_REPRESENTATION 'ZEILBERGER_REPRESENTATION
              'RATIONAL_CERTIFICATE)))
      (SETQ RESULT1 (AEVAL (MINUS 1)))
      (AEVAL (ON (LIST 'EXP)))
      (SETQ INHOMOGENEOUS (AEVAL 'NIL))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'ARGLENGTH SECUNDUS)) 3)
             (EVALEQUAL (AEVAL (LIST 'PART SECUNDUS 0)) (AEVAL 'LIST)))
        (AEVAL (REDERR (REVALX "not yet implemented.")))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'ARGLENGTH SECUNDUS)) 3)
             (EVALEQUAL (AEVAL (LIST 'PART SECUNDUS 0)) (AEVAL 'LIST)))
        (PROGN
         (SETQ K (AEVAL (LIST 'PART SECUNDUS 1)))
         (SETQ AA (AEVAL (LIST 'PART SECUNDUS 2)))
         (SETQ BB (AEVAL (LIST 'PART SECUNDUS 3)))
         (SETQ INHOMOGENEOUS (AEVAL 'T))))
       (T
        (PROGN
         (SETQ K (AEVAL SECUNDUS))
         (SETQ AA (AEVAL 0))
         (SETQ BB (AEVAL 0)))))
      (SETQ ANK (AEVAL FUNC))
      (SETK 'TESTER (AEVAL FUNC))
      (SETK 'TESTER (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'ONERULES) 'TESTER)))
      (COND
       ((EVALNEQ (AEVAL 'TESTER) 0)
        (PROGN
         (SETK 'TESTER2 (AEVAL 'TESTER))
         (SETK 'TESTER
               (AEVAL
                (LIST 'QUOTIENT 'TESTER
                      (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                            'TESTER))))
         (COND
          ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY 'TESTER K))))
           (AEVAL (REDERR (REVALX "algorithm not applicable")))))
         (SETK 'TESTER2
               (AEVAL
                (LIST 'QUOTIENT 'TESTER2
                      (LIST 'SUB (LIST 'EQUAL N (LIST 'DIFFERENCE N 1))
                            'TESTER2))))
         (COND
          ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY 'TESTER2 N))))
           (AEVAL (REDERR (REVALX "algorithm not applicable")))))
         (AEVAL 'NIL))))
      (SETQ BC
              (AEVAL
               (LIST 'SIMPLIFY_COMBINATORIAL
                     (LIST 'QUOTIENT ANK
                           (LIST 'SUB (LIST 'EQUAL N (LIST 'DIFFERENCE N 1))
                                 ANK)))))
      (SETQ B (AEVAL (LIST 'NUM BC)))
      (SETQ C (AEVAL (LIST 'DEN BC)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL ")/F(") NIL NIL)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL "-1,") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "):=") NIL NIL)
         (ASSGNPRI (AEVAL BC) NIL 'LAST))))
      (AEVAL (ON (LIST 'EXP)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY BC N))))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "not rational") NIL 'ONLY)
            (ASSGNPRI (AEVAL "Zeilberger algorithm not applicable") NIL 'ONLY)
            (AEVAL 'NIL))))
         (RETURN (AEVAL (LIST 'EXTENDED_SUMRECURSION1 ANK K N)))
         (AEVAL 'NIL))))
      (SETQ DG
              (AEVAL
               (LIST 'SIMPLIFY_COMBINATORIAL
                     (LIST 'QUOTIENT ANK
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                                 ANK)))))
      (SETQ D (AEVAL (LIST 'NUM DG)))
      (SETQ G (AEVAL (LIST 'DEN DG)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL ")/F(") NIL NIL)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "-1):=") NIL NIL)
         (ASSGNPRI (AEVAL DG) NIL 'LAST))))
      (AEVAL (ON (LIST 'EXP)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY DG K))))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "not rational") NIL 'ONLY)
            (ASSGNPRI (AEVAL "Zeilberger algorithm not applicable") NIL 'ONLY)
            (AEVAL 'NIL))))
         (RETURN (AEVAL (LIST 'EXTENDED_SUMRECURSION1 ANK K N)))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "Zeilberger algorithm applicable") NIL 'ONLY)))
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ ORDER1 (AEVAL 0))
      (PROG (J)
        (SETQ J (AEVAL* MINI))
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MAXI) J)) (RETURN NIL)))
        (COND
         ((EVALEQUAL (AEVAL* RESULT1) (MINUS 1))
          (SETQ RESULT1
                  (AEVAL*
                   (LIST 'SUMRECURSION2 ANK B C D G K N AA BB INHOMOGENEOUS
                         J))))
         ((EVALEQUAL (AEVAL* ORDER1) 0)
          (SETQ ORDER1 (AEVAL* (LIST 'DIFFERENCE J 1)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL RESULT1) (MINUS 1))
        (AEVAL
         (REDERR (REVALX "Zeilberger algorithm fails. Enlarge zb_order")))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_FACTOR))
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (COND
          ((BOOLVALUE*
            (REVALX
             (MINUSP
              (CDAR (CAR (CADR (REVAL1 (PREPSQ (CADR RESULT1)) NIL)))))))
           (SETQ RESULT1 (AEVAL (LIST 'MINUS RESULT1)))))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'LIST RESULT1 ORDER1))))) 
(PUT 'SUMRECURSION2 'NUMBER-OF-ARGS 11) 
(FLAG '(SUMRECURSION2) 'OPFN) 
(PUT 'SUMRECURSION2 'DEFINED-ON-LINE '458) 
(PUT 'SUMRECURSION2 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMRECURSION2 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SUMRECURSION2 (ANK B C D G K N AA BB INHOMOGENEOUS ORDER1)
    (PROG (J JJ P R10 R20 R1 R2 P1 POLYNOMIALS GG RECURSION RECURSION2
           EQUATIONS F VARLIST R12 SUMME S Z *FACTOR *EXP)
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "applying Zeilberger algorithm for order:=") NIL
                   'FIRST)
         (ASSGNPRI (AEVAL ORDER1) NIL 'LAST))))
      (SETK 'P0
            (AEVAL
             (LIST 'PLUS
                   (PROG (J FORALL-RESULT)
                     (SETQ J 0)
                     (SETQ FORALL-RESULT 1)
                    LAB1
                     (COND
                      ((|AMINUSP:|
                        (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ORDER1 1))
                              J))
                       (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (AEVAL*
                              (LIST 'TIMES
                                    (AEVAL*
                                     (LIST 'SUB
                                           (LIST 'EQUAL N
                                                 (LIST 'DIFFERENCE N J))
                                           B))
                                    FORALL-RESULT)))
                     (SETQ J
                             ((LAMBDA (FORALL-RESULT)
                                (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                              J))
                     (GO LAB1))
                   (PROG (J FORALL-RESULT)
                     (SETQ J 1)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND
                      ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J))
                       (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (AEVAL*
                              (LIST 'PLUS
                                    (AEVAL*
                                     (LIST 'TIMES (LIST 'ZB_SIGMA J)
                                           (PROG (JJ FORALL-RESULT)
                                             (SETQ JJ 0)
                                             (SETQ FORALL-RESULT 1)
                                            LAB1
                                             (COND
                                              ((|AMINUSP:|
                                                (LIST 'DIFFERENCE
                                                      (AEVAL*
                                                       (LIST 'DIFFERENCE J 1))
                                                      JJ))
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (AEVAL*
                                                      (LIST 'TIMES
                                                            (AEVAL*
                                                             (LIST 'SUB
                                                                   (LIST 'EQUAL
                                                                         N
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          N
                                                                          JJ))
                                                                   C))
                                                            FORALL-RESULT)))
                                             (SETQ JJ
                                                     ((LAMBDA (FORALL-RESULT)
                                                        (AEVAL*
                                                         (LIST 'PLUS
                                                               FORALL-RESULT
                                                               1)))
                                                      JJ))
                                             (GO LAB1))
                                           (PROG (JJ FORALL-RESULT)
                                             (SETQ JJ (AEVAL* J))
                                             (SETQ FORALL-RESULT 1)
                                            LAB1
                                             (COND
                                              ((|AMINUSP:|
                                                (LIST 'DIFFERENCE
                                                      (AEVAL*
                                                       (LIST 'DIFFERENCE ORDER1
                                                             1))
                                                      JJ))
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (AEVAL*
                                                      (LIST 'TIMES
                                                            (AEVAL*
                                                             (LIST 'SUB
                                                                   (LIST 'EQUAL
                                                                         N
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          N
                                                                          JJ))
                                                                   B))
                                                            FORALL-RESULT)))
                                             (SETQ JJ
                                                     ((LAMBDA (FORALL-RESULT)
                                                        (AEVAL*
                                                         (LIST 'PLUS
                                                               FORALL-RESULT
                                                               1)))
                                                      JJ))
                                             (GO LAB1))))
                                    FORALL-RESULT)))
                     (SETQ J
                             ((LAMBDA (FORALL-RESULT)
                                (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                              J))
                     (GO LAB1)))))
      (SETQ R12
              (AEVAL
               (LIST 'TIMES D
                     (LIST 'QUOTIENT
                           (PROG (J FORALL-RESULT)
                             (SETQ J 0)
                             (SETQ FORALL-RESULT 1)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE
                                      (AEVAL* (LIST 'DIFFERENCE ORDER1 1)) J))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'TIMES
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'LIST
                                                         (LIST 'EQUAL N
                                                               (LIST
                                                                'DIFFERENCE N
                                                                J))
                                                         (LIST 'EQUAL K
                                                               (LIST
                                                                'DIFFERENCE K
                                                                1)))
                                                   B))
                                            FORALL-RESULT)))
                             (SETQ J
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      J))
                             (GO LAB1))
                           (LIST 'TIMES G
                                 (PROG (J FORALL-RESULT)
                                   (SETQ J 0)
                                   (SETQ FORALL-RESULT 1)
                                  LAB1
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE
                                            (AEVAL*
                                             (LIST 'DIFFERENCE ORDER1 1))
                                            J))
                                     (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (AEVAL*
                                            (LIST 'TIMES
                                                  (AEVAL*
                                                   (LIST 'SUB
                                                         (LIST 'EQUAL N
                                                               (LIST
                                                                'DIFFERENCE N
                                                                J))
                                                         B))
                                                  FORALL-RESULT)))
                                   (SETQ J
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            J))
                                   (GO LAB1)))))))
      (SETQ R12 (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL R12)))
      (SETQ R10 (AEVAL (LIST 'NUM R12)))
      (SETQ R20 (AEVAL (LIST 'DEN R12)))
      (AEVAL (OFF (LIST 'FACTOR)))
      (SETQ POLYNOMIALS (AEVAL (LIST 'DETERMINE_POLYNOMIALS R10 R20 K)))
      (SETQ P1 (AEVAL (LIST 'FIRST POLYNOMIALS)))
      (SETQ P (AEVAL (LIST 'TIMES P1 'P0)))
      (SETQ R1 (AEVAL (LIST 'SECOND POLYNOMIALS)))
      (SETQ R2 (AEVAL (LIST 'THIRD POLYNOMIALS)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "p:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL P) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "q:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL R1) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "r:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL R2) NIL 'LAST)))))
      (SETK 'DEGREE (AEVAL (LIST 'MAXDEGF R1 R2 P K)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "degreebound:=") NIL 'FIRST)
         (ASSGNPRI (AEVAL 'DEGREE) NIL 'LAST))))
      (COND ((EVALLESSP (AEVAL 'DEGREE) 0) (RETURN (MINUS 1))))
      (SETQ F
              (PROG (J FORALL-RESULT)
                (SETQ J 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'DEGREE) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES (LIST 'ZB_F J) (LIST 'EXPT K J)))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1)))
      (SETQ EQUATIONS
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES
                                 (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                                       R1)
                                 F)
                           (LIST 'TIMES R2
                                 (LIST 'SUB
                                       (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                                       F)))
                     P)))
      (AEVAL (ON (LIST 'EXP)))
      (SETK 'EQUATIONLIST (AEVAL (LIST 'COEFF EQUATIONS K)))
      (SETK 'VA
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J 0)
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'DEGREE) J))
                (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR (CONS (AEVAL* (LIST 'ZB_F J)) NIL)))
             LOOPLABEL
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'DEGREE) J))
                (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'ZB_F J)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETK 'VB
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J 1)
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J))
                (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS (AEVAL* (LIST 'ZB_SIGMA J)) NIL)))
             LOOPLABEL
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J))
                (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'ZB_SIGMA J)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETQ VARLIST (AEVAL (LIST 'APPEND 'VA 'VB)))
      (SETK 'SOL (AEVAL (LIST 'SAVESOLVE 'EQUATIONLIST VARLIST)))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SUB 'SOL EQUATIONS)) 0) (RETURN (MINUS 1))))
      (SETQ F (AEVAL (LIST 'SUB 'SOL F)))
      (SETQ F
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'ARBCOMPLEX (LIST '~ 'X)) 0))
                     F)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "f:=") NIL 'FIRST)
         (ASSGNPRI (AEVAL F) NIL 'LAST))))
      (SETQ P (AEVAL (LIST 'SUB 'SOL P)))
      (SETQ P
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'ARBCOMPLEX (LIST '~ 'X)) 0))
                     P)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "p:=") NIL 'FIRST)
         (ASSGNPRI (AEVAL P) NIL 'LAST))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_PROOF))
        (SETK 'ZEILBERGER_REPRESENTATION (AEVAL (LIST 'LIST P R1 R2 F)))))
      (COND
       ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'DOWN))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_PROOF))
           (SETK 'RATIONAL_CERTIFICATE
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'QUOTIENT
                              (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) R1)
                              P)
                        F)))))))
       (T
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_PROOF))
           (SETK 'RATIONAL_CERTIFICATE
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                              (LIST 'QUOTIENT R2 P))
                        F))))))))
      (SETK 'VA (AEVAL (LIST 'SUB 'SOL 'VA)))
      (SETK 'VB (AEVAL (LIST 'SUB 'SOL 'VB)))
      (SETK 'N0 (AEVAL (LIST 'DIFFERENCE ORDER1 1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'DEGREE) J)) (RETURN NIL)))
        (SETK 'N0
              (AEVAL*
               (LIST 'MAX
                     (LIST 'TESTNONNEGINTROOTS (LIST 'DEN (LIST 'PART 'VA J))
                           N)
                     'N0)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J)) (RETURN NIL)))
        (SETK 'N0
              (AEVAL*
               (LIST 'MAX
                     (LIST 'TESTNONNEGINTROOTS (LIST 'DEN (LIST 'PART 'VB J))
                           N)
                     'N0)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETK 'N0
            (AEVAL
             (LIST 'MAX
                   (LIST 'TESTNONNEGINTROOTS
                         (LIST 'DEN
                               (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS N 1)) 'Q))
                         N)
                   'N0)))
      (SETK 'N0
            (AEVAL
             (LIST 'MAX
                   (LIST 'TESTNONNEGINTROOTS
                         (LIST 'NUM
                               (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS N 1)) P))
                         N)
                   'N0)))
      (COND
       ((EVALGEQ (AEVAL 'N0) (AEVAL ORDER1))
        (PROGN
         (ASSGNPRI (AEVAL "recursion valid for n>=") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'PLUS 'N0 1)) NIL 'LAST))))
      (SETK 'ZB_TESTNONNEGINTROOTS
            (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE 'N0 ORDER1) 1)))
      (SETQ RECURSION
              (AEVAL
               (LIST 'PLUS (LIST 'SUMM N)
                     (PROG (J FORALL-RESULT)
                       (SETQ J 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'PART 'VB J)
                                             (LIST 'SUMM
                                                   (LIST 'DIFFERENCE N J))))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1)))))
      (SETQ RECURSION (AEVAL (LIST 'NUM RECURSION)))
      (SETQ RECURSION
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'ARBCOMPLEX (LIST '~ 'LOCAL_X)) 1))
                     RECURSION)))
      (COND
       ((OR (BOOLVALUE* (REVALX *ZB_PROOF)) (BOOLVALUE* INHOMOGENEOUS))
        (PROGN
         (SETQ GG
                 (AEVAL
                  (LIST 'TIMES F
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                              (LIST 'TIMES R2
                                    (LIST 'QUOTIENT ANK
                                          (LIST 'TIMES P1
                                                (PROG (J FORALL-RESULT)
                                                  (SETQ J 0)
                                                  (SETQ FORALL-RESULT 1)
                                                 LAB1
                                                  (COND
                                                   ((|AMINUSP:|
                                                     (LIST 'DIFFERENCE
                                                           (AEVAL*
                                                            (LIST 'DIFFERENCE
                                                                  ORDER1 1))
                                                           J))
                                                    (RETURN FORALL-RESULT)))
                                                  (SETQ FORALL-RESULT
                                                          (AEVAL*
                                                           (LIST 'TIMES
                                                                 (AEVAL*
                                                                  (LIST 'SUB
                                                                        (LIST
                                                                         'EQUAL
                                                                         N
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          N J))
                                                                        B))
                                                                 FORALL-RESULT)))
                                                  (SETQ J
                                                          ((LAMBDA
                                                               (FORALL-RESULT)
                                                             (AEVAL*
                                                              (LIST 'PLUS
                                                                    FORALL-RESULT
                                                                    1)))
                                                           J))
                                                  (GO LAB1)))))))))
         (SETQ GG (AEVAL (LIST 'SUB 'SOL GG)))
         (SETK 'PROOF (AEVAL GG))
         (SETQ GG
                 (AEVAL
                  (LIST 'SUB
                        (LIST 'LIST (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                              (LIST 'EQUAL N (LIST 'PLUS N 1)))
                        GG)))
         (SETQ GG (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL GG)))
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "G:=") NIL 'FIRST)
            (ASSGNPRI (AEVAL GG) NIL 'LAST))))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* INHOMOGENEOUS)
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (COND
          ((BOOLVALUE* (REVALX *ZB_INHOMOGENEOUS))
           (PROGN
            (SETQ RECURSION
                    (AEVAL
                     (LIST 'LIST RECURSION
                           (LIST 'SIMPLIFY_COMBINATORIAL
                                 (LIST 'DIFFERENCE
                                       (LIST 'SUB
                                             (LIST 'EQUAL K (LIST 'PLUS BB 1))
                                             GG)
                                       (LIST 'SUB (LIST 'EQUAL K AA) GG))))))
            (SETK 'TEMPO
                  (AEVAL
                   (LIST 'SIMPLIFY_COMBINATORIAL
                         (LIST 'DIFFERENCE (LIST 'SUB (LIST 'EQUAL K BB) GG)
                               (LIST 'SUB
                                     (LIST 'EQUAL K (LIST 'DIFFERENCE AA 1))
                                     GG)))))
            (AEVAL 'NIL)))
          (T
           (PROGN
            (SETQ RECURSION
                    (AEVAL
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES GG
                                 (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                                       RECURSION))
                           (LIST 'TIMES
                                 (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1))
                                       GG)
                                 RECURSION))))
            (SETQ RECURSION (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL RECURSION)))
            (SETQ RECURSION (AEVAL (LIST 'NUM RECURSION))))))
         (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "Zeilberger algorithm successful") NIL 'ONLY)))
      (COND
       ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'DOWN))
        (RETURN (AEVAL RECURSION)))
       (T
        (RETURN
         (AEVAL
          (LIST 'SUB (LIST 'EQUAL N (LIST 'PLUS N ORDER1)) RECURSION))))))) 
(PUT 'TESTNONNEGINTROOTS 'NUMBER-OF-ARGS 2) 
(FLAG '(TESTNONNEGINTROOTS) 'OPFN) 
(PUT 'TESTNONNEGINTROOTS 'DEFINED-ON-LINE '576) 
(PUT 'TESTNONNEGINTROOTS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TESTNONNEGINTROOTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTNONNEGINTROOTS (TERM1 N)
    (PROG (N0 L J N1)
      (SETQ N0 (AEVAL (MINUS 1)))
      (SETQ TERM1 (AEVAL (LIST 'OLD_FACTORIZE TERM1)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH TERM1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETK 'F (AEVAL* (LIST 'PART TERM1 J)))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'DEG 'F N)) 1)
           (SETQ N1
                   (AEVAL*
                    (LIST 'MINUS
                          (LIST 'QUOTIENT (LIST 'PART (LIST 'COEFF 'F N) 1)
                                (LIST 'PART (LIST 'COEFF 'F N) 2)))))))
         (COND ((FIXP (REVALX N1)) (SETQ N0 (AEVAL* (LIST 'MAX N1 N0))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL N0)))) 
(PUT 'HYPERSUM-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'HYPERSUM-EVAL 'DEFINED-ON-LINE '596) 
(PUT 'HYPERSUM-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERSUM-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPERSUM-EVAL (U)
    (PROGN
     (COND
      ((EQUAL (LENGTH U) 4)
       (HYPERSUM1 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) (REVAL1 (CADDDR U) NIL) 1 ZB_ORDER))
      ((EQUAL (LENGTH U) 5)
       (HYPERSUM1 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) (REVAL1 (CADDDR U) NIL) (CAR (CDDDDR U))
        (CAR (CDDDDR U))))
      (T (REDERR "illegal number of arguments"))))) 
(PUT 'HYPERSUM 'PSOPFN 'HYPERSUM-EVAL) 
(PUT 'RECURSION_TO_CLOSED_FORM 'NUMBER-OF-ARGS 4) 
(FLAG '(RECURSION_TO_CLOSED_FORM) 'OPFN) 
(PUT 'RECURSION_TO_CLOSED_FORM 'DEFINED-ON-LINE '609) 
(PUT 'RECURSION_TO_CLOSED_FORM 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'RECURSION_TO_CLOSED_FORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RECURSION_TO_CLOSED_FORM (RECURSION STARTL N M)
    (PROG (AJ RECJ LIST1 P Q TMP J NONHYP ORDER1 *FACTOR *EXP)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ LIST1 (AEVAL (LIST 'LIST)))
      (SETQ ORDER1 (AEVAL (LIST 'ARGLENGTH STARTL)))
      (SETQ P (AEVAL (LIST 'PART (LIST 'COEFF RECURSION (LIST 'SUMM N)) 2)))
      (SETQ Q
              (AEVAL
               (LIST 'PART
                     (LIST 'COEFF RECURSION
                           (LIST 'SUMM (LIST 'DIFFERENCE N ORDER1)))
                     2)))
      (SETQ NONHYP (AEVAL 0))
      (COND
       ((NOT
         (AND (FREEOF (REVALX 'SUMM) (REVALX P))
              (FREEOF (REVALX 'SUMM) (REVALX Q))))
        (PROGN
         (SETQ NONHYP (AEVAL 1))
         (ASSGNPRI (AEVAL "no hypergeometric solution found") NIL 'ONLY)
         (RETURN (AEVAL RECURSION))
         (AEVAL 'NIL))))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J)) (RETURN NIL)))
        (PROGN
         (SETQ AJ (AEVAL* (LIST 'PART STARTL J)))
         (COND
          ((EVALEQUAL (AEVAL* AJ) 0)
           (SETQ LIST1 (AEVAL* (LIST 'APPEND LIST1 (LIST 'LIST 0)))))
          (T
           (PROGN
            (SETQ RECJ
                    (AEVAL*
                     (LIST 'PLUS
                           (LIST 'TIMES
                                 (LIST 'SUB
                                       (LIST 'EQUAL N
                                             (LIST 'PLUS (LIST 'TIMES N ORDER1)
                                                   (LIST 'DIFFERENCE J 1)))
                                       P)
                                 (LIST 'SUMM N))
                           (LIST 'TIMES
                                 (LIST 'SUB
                                       (LIST 'EQUAL N
                                             (LIST 'PLUS (LIST 'TIMES N ORDER1)
                                                   (LIST 'DIFFERENCE J 1)))
                                       Q)
                                 (LIST 'SUMM (LIST 'DIFFERENCE N 1))))))
            (SETQ TMP (AEVAL* (LIST 'RECTOPOCH RECJ N 1 M)))
            (SETQ TMP
                    (AEVAL*
                     (LIST 'LIST
                           (LIST 'TIMES AJ
                                 (LIST 'SUB
                                       (LIST 'EQUAL N
                                             (LIST 'QUOTIENT
                                                   (LIST 'PLUS
                                                         (LIST 'DIFFERENCE N J)
                                                         1)
                                                   ORDER1))
                                       TMP)))))
            (SETQ LIST1 (AEVAL* (LIST 'APPEND LIST1 TMP)))
            (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL ORDER1) 1) (RETURN (AEVAL (LIST 'PART LIST1 1))))
       (T (RETURN (AEVAL LIST1)))))) 
(PUT 'HYPERSUM1 'NUMBER-OF-ARGS 6) 
(FLAG '(HYPERSUM1) 'OPFN) 
(PUT 'HYPERSUM1 'DEFINED-ON-LINE '646) 
(PUT 'HYPERSUM1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERSUM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERSUM1 (UPPER LOWER Z N MINI MAXI)
    (PROG (TMP1 TMP J JJ AJ ORDER1 RECURSION TERM1 *EXP STARTL)
      (AEVAL (OFF (LIST 'EXP)))
      (SETQ TMP (AEVAL (LIST 'HYPERRECURSION1 UPPER LOWER Z N MINI MAXI)))
      (SETQ RECURSION (AEVAL (LIST 'PART TMP 1)))
      (SETQ ORDER1 (AEVAL (LIST 'PART TMP 2)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "recursion for underlying hypergeometric term:=") NIL
                   'FIRST)
         (ASSGNPRI (AEVAL RECURSION) NIL 'LAST))))
      (SETQ STARTL (AEVAL (LIST 'LIST 1)))
      (COND
       ((EVALGREATERP (AEVAL ORDER1) 1)
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ORDER1 1)) J))
             (RETURN NIL)))
           (PROGN
            (SETQ AJ
                    (AEVAL*
                     (LIST 'SUB (LIST 'EQUAL N J)
                           (PROG (JJ FORALL-RESULT)
                             (SETQ JJ 0)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* J) JJ))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'PLUS
                                            (AEVAL*
                                             (LIST 'HYPERTERM UPPER LOWER Z
                                                   JJ))
                                            FORALL-RESULT)))
                             (SETQ JJ
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      JJ))
                             (GO LAB1)))))
            (SETQ AJ (AEVAL* (LIST 'SIMPLIFY_COMBINATORIAL AJ)))
            (SETQ STARTL (AEVAL* (LIST 'APPEND STARTL (LIST 'LIST AJ))))
            (AEVAL* 'NIL))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'RECURSION_TO_CLOSED_FORM RECURSION STARTL N 0))))) 
(PUT 'SUMMATION 'NUMBER-OF-ARGS 3) 
(FLAG '(SUMMATION) 'OPFN) 
(PUT 'SUMMATION 'DEFINED-ON-LINE '675) 
(PUT 'SUMMATION 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMMATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUMMATION (F K N)
    (PROG (L LOCALHYPERSUM UPPER LOWER Z TERM I TMP STARTL AJ PIECEWISETERM
           PIECEWISESEQ F1 PARTJ RECURSION COUNTER M TMPTERM PREFACTOR INIT HT
           INITIAL INITIALNUMBER SUMMAND J GAMMASUMMAND *EXP)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ HT (AEVAL (LIST 'SUMTOHYPER F K)))
      (SETQ PREFACTOR (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'ONERULES2) HT)))
      (SETQ HT (AEVAL (LIST 'QUOTIENT HT PREFACTOR)))
      (SETQ UPPER (AEVAL (LIST 'PART HT 1)))
      (SETQ LOWER (AEVAL (LIST 'PART HT 2)))
      (SETQ Z (AEVAL (LIST 'PART HT 3)))
      (SETQ F1 (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL F)))
      (SETQ TMP (AEVAL (LIST 'SUMRECURSION1 F1 K N 1 ZB_ORDER)))
      (SETQ RECURSION (AEVAL (LIST 'PART TMP 1)))
      (SETK 'ORDER1 (AEVAL (LIST 'PART TMP 2)))
      (COND
       ((AND (EVALEQUAL (AEVAL 'ORDER1) 1)
             (EVALEQUAL (AEVAL 'ZB_TESTNONNEGINTROOTS) 0))
        (PROGN
         (RETURN
          (AEVAL
           (LIST 'RECURSION_TO_CLOSED_FORM RECURSION (LIST 'LIST PREFACTOR) N
                 0)))
         (AEVAL 'NIL))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH UPPER)))
      (SETQ INITIALNUMBER (AEVAL 0))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ PARTJ (AEVAL* (LIST 'PART UPPER J)))
         (SETQ TMP (AEVAL* (LIST 'COEFF PARTJ N)))
         (COND
          ((EVALEQUAL (AEVAL* (LIST 'ARGLENGTH TMP)) 2)
           (COND
            ((AND (FIXP (REVALX (LIST 'PART TMP 2)))
                  (EVALLESSP (AEVAL* (LIST 'PART TMP 2)) 0)
                  (FIXP (REVALX (LIST 'PART TMP 1))))
             (SETQ INITIALNUMBER (AEVAL* (LIST 'MINUS PARTJ)))))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL INITIALNUMBER) 0)
        (AEVAL (REDERR (REVALX "no reccurent evaluation possible")))))
      (SETQ STARTL (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J (AEVAL* 'ZB_TESTNONNEGINTROOTS))
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE
                 (AEVAL*
                  (LIST 'PLUS (LIST 'DIFFERENCE 'ORDER1 1)
                        'ZB_TESTNONNEGINTROOTS))
                 J))
          (RETURN NIL)))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL* "prefactor:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL* PREFACTOR) NIL 'LAST))
         (PROGN
          (ASSGNPRI
           (AEVAL* "sum(hyperterm(UPPER,LOWER,z,k),k,0,initialnumber):=") NIL
           'FIRST)
          (ASSGNPRI
           (AEVAL*
            (LIST 'SUM (LIST 'HYPERTERM UPPER LOWER Z K) K 0 INITIALNUMBER))
           NIL 'LAST))
         (SETQ AJ
                 (AEVAL*
                  (LIST 'SUB (LIST 'EQUAL N J)
                        (LIST 'TIMES PREFACTOR
                              (LIST 'SUM (LIST 'HYPERTERM UPPER LOWER Z K) K 0
                                    INITIALNUMBER)))))
         (SETQ AJ (AEVAL* (LIST 'SIMPLIFY_COMBINATORIAL AJ)))
         (SETQ STARTL (AEVAL* (LIST 'APPEND STARTL (LIST 'LIST AJ))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROGN
       (ASSGNPRI (AEVAL "startl:=") NIL 'FIRST)
       (ASSGNPRI (AEVAL STARTL) NIL 'LAST))
      (SETQ TERM
              (AEVAL
               (LIST 'RECURSION_TO_CLOSED_FORM RECURSION STARTL N
                     'ZB_TESTNONNEGINTROOTS)))
      (PROGN
       (ASSGNPRI (AEVAL "term:=") NIL 'FIRST)
       (ASSGNPRI (AEVAL TERM) NIL 'LAST))
      (COND ((FREEOF (REVALX TERM) (REVALX 'SUMM)) (RETURN (AEVAL TERM)))
            ((FREEOF (REVALX PREFACTOR) (REVALX N))
             (SETQ RECURSION (AEVAL TERM)))
            (T (SETQ RECURSION (AEVAL (LIST 'SUMRECURSION F K N)))))
      (COND ((BOOLVALUE* (REVALX *ZB_TRACE)) (SETQ COUNTER (AEVAL 0))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH RECURSION)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) I)) (RETURN NIL)))
        (PROGN
         (SETQ TERM
                 (AEVAL*
                  (LIST 'QUOTIENT (LIST 'PART RECURSION I)
                        (LIST 'WHEREEXP (LIST 'LIST 'ONERULES2)
                              (LIST 'PART RECURSION I)))))
         (SETQ TERM (AEVAL* (LIST 'PART TERM 1)))
         (SETQ M (AEVAL* (LIST 'PART TERM 1)))
         (SETQ COUNTER (AEVAL* (LIST 'MAX COUNTER (LIST 'DIFFERENCE M TERM))))
         (AEVAL* 'NIL))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "calculating initial values") NIL 'ONLY)))
      (SETQ INITIALNUMBER (AEVAL 0))
      (SETQ L (AEVAL (LIST 'ARGLENGTH UPPER)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) I)) (RETURN NIL)))
        (PROGN
         (SETQ TMP
                 (AEVAL* (LIST 'PART (LIST 'COEFF (LIST 'PART I UPPER) N) 2)))
         (COND
          ((AND (FIXP (REVALX TMP)) (EVALLESSP (AEVAL* TMP) 0))
           (SETQ INITIALNUMBER (AEVAL* (LIST 'PART UPPER I)))))
         (COND
          ((EVALEQUAL (AEVAL* INITIALNUMBER) 0)
           (AEVAL* (REDERR (REVALX "no initialization found")))))
         (COND
          ((EVALEQUAL (AEVAL* 'ZB_TESTNONNEGINTROOTS) 0) (AEVAL* 'ERRORSET)))
         (AEVAL* 'NIL))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETQ TMP (AEVAL (LIST 'SUB (LIST 'EQUAL N 0) PREFACTOR))))) 
(PUT 'RECORDER 'NUMBER-OF-ARGS 2) 
(FLAG '(RECORDER) 'OPFN) 
(PUT 'RECORDER 'DEFINED-ON-LINE '780) 
(PUT 'RECORDER 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'RECORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RECORDER (F N)
    (PROG ()
      (SETK 'PA (AEVAL (LIST 'PATTERNARGUMENTS F 'SUMM (LIST 'LIST))))
      (SETK 'PA (AEVAL (LIST 'SUB (LIST 'EQUAL N 0) 'PA)))
      (RETURN (AEVAL (LIST 'MINUS (LIST 'MIN 'PA)))))) 
(PUT 'RECTOPOCH 'NUMBER-OF-ARGS 4) 
(FLAG '(RECTOPOCH) 'OPFN) 
(PUT 'RECTOPOCH 'DEFINED-ON-LINE '787) 
(PUT 'RECTOPOCH 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'RECTOPOCH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RECTOPOCH (F N ORDER1 M)
    (PROG (DENNUM DENDEN CASES1 K NUME DENO *EXP *GCD)
      (AEVAL (ON (LIST 'EXP)))
      (AEVAL (ON (LIST 'GCD)))
      (SETQ DENO
              (AEVAL
               (LIST 'MINUS (LIST 'PART (LIST 'COEFF F (LIST 'SUMM N)) 2))))
      (SETQ NUME
              (AEVAL
               (LIST 'PART
                     (LIST 'COEFF F (LIST 'SUMM (LIST 'DIFFERENCE N ORDER1)))
                     2)))
      (COND
       ((EVALGREATERP (AEVAL ORDER1) 1)
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ORDER1 1)) J))
             (RETURN NIL)))
           (COND
            ((NOT
              (FREEOF (REVALX F) (REVALX (LIST 'SUMM (LIST 'DIFFERENCE N J)))))
             (AEVAL* (REDERR (REVALX "no hypergeometric solution")))))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB))
         (SETQ CASES1 (AEVAL (LIST 'LIST)))
         (PROG (J)
           (SETQ J 0)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE ORDER1 1)) J))
             (RETURN NIL)))
           (SETQ CASES1
                   (AEVAL*
                    (LIST 'APPEND
                          (LIST 'LIST
                                (LIST 'SUB
                                      (LIST 'EQUAL N
                                            (LIST 'QUOTIENT
                                                  (LIST 'DIFFERENCE N J)
                                                  ORDER1))
                                      (LIST 'RECTOPOCH
                                            (LIST 'PLUS
                                                  (LIST 'TIMES (LIST 'SUMM N)
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL N
                                                                    (LIST 'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           ORDER1
                                                                           N)
                                                                          J))
                                                              NUME))
                                                  (LIST 'TIMES
                                                        (LIST 'SUMM
                                                              (LIST 'DIFFERENCE
                                                                    N 1))
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL N
                                                                    (LIST 'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           ORDER1
                                                                           N)
                                                                          J))
                                                              DENO)))
                                            N 1 M)))
                          CASES1)))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB))
         (RETURN (AEVAL CASES1)))))
      (SETK 'LCR2 (AEVAL (LIST 'FIRST (LIST 'REVERSE (LIST 'COEFF DENO N)))))
      (SETK 'LCR1 (AEVAL (LIST 'FIRST (LIST 'REVERSE (LIST 'COEFF NUME N)))))
      (SETQ NUME (AEVAL (LIST 'QUOTIENT NUME 'LCR1)))
      (SETQ DENNUM (AEVAL (LIST 'DEN NUME)))
      (SETQ NUME (AEVAL (LIST 'NUM NUME)))
      (SETQ DENO (AEVAL (LIST 'QUOTIENT DENO 'LCR2)))
      (SETQ DENDEN (AEVAL (LIST 'DEN DENO)))
      (SETQ DENO (AEVAL (LIST 'NUM DENO)))
      (SETQ DENO (AEVAL (LIST 'OLD_FACTORIZE DENO)))
      (SETQ NUME (AEVAL (LIST 'OLD_FACTORIZE NUME)))
      (SETQ DENO
              (AEVAL
               (LIST 'SETPART* DENO 1
                     (AEVAL (LIST 'QUOTIENT (LIST 'PART DENO 1) DENDEN)))))
      (SETQ NUME
              (AEVAL
               (LIST 'SETPART* NUME 1
                     (AEVAL (LIST 'QUOTIENT (LIST 'PART NUME 1) DENNUM)))))
      (SETQ DENO (AEVAL (LIST 'REFACTORS DENO N)))
      (SETQ NUME
              (AEVAL (LIST 'APPEND (LIST 'LIST 1) (LIST 'REFACTORS NUME N))))
      (SETK 'TMP (AEVAL (LIST 'LIST)))
      (SETK 'L (AEVAL (LIST 'ARGLENGTH NUME)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'L) J)) (RETURN NIL)))
        (SETK 'TMP
              (AEVAL*
               (LIST 'APPEND 'TMP
                     (LIST 'LIST (LIST 'PLUS (LIST 'PART NUME J) M)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ NUME (AEVAL 'TMP))
      (SETK 'TMP (AEVAL (LIST 'LIST)))
      (SETK 'L (AEVAL (LIST 'ARGLENGTH DENO)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'L) J)) (RETURN NIL)))
        (SETK 'TMP
              (AEVAL*
               (LIST 'APPEND 'TMP
                     (LIST 'LIST (LIST 'PLUS (LIST 'PART DENO J) M)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ DENO (AEVAL 'TMP))
      (RETURN
       (AEVAL
        (LIST 'TIMES
              (LIST 'HYPERTERM NUME DENO (LIST 'QUOTIENT 'LCR1 'LCR2)
                    (LIST 'DIFFERENCE N M))
              (LIST 'QUOTIENT (LIST 'FACTORIAL (LIST 'DIFFERENCE N M))
                    (LIST 'POCHHAMMER (LIST 'PLUS M 1)
                          (LIST 'DIFFERENCE N M)))))))) 
(PUT 'REFACTORS 'NUMBER-OF-ARGS 2) 
(FLAG '(REFACTORS) 'OPFN) 
(PUT 'REFACTORS 'DEFINED-ON-LINE '848) 
(PUT 'REFACTORS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'REFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REFACTORS (TERM1 N)
    (PROG (A L I C D G POL DEGREE *EXP *FACTOR DENPOL NUMPOL)
     DENPOL
      (AEVAL (ON (LIST 'EXP)))
      (SETQ G (AEVAL (LIST 'LIST)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH TERM1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) I)) (RETURN NIL)))
        (PROGN
         (SETQ POL (AEVAL* (LIST 'PART TERM1 I)))
         (AEVAL* (ON (LIST 'EXP)))
         (COND
          ((NOT (FREEOF (REVALX POL) (REVALX N)))
           (PROGN
            (SETQ NUMPOL (AEVAL* (LIST 'NUM POL)))
            (SETQ DENPOL (AEVAL* (LIST 'DEN POL)))
            (SETQ DEGREE (AEVAL* (LIST 'DEG NUMPOL N)))
            (COND
             ((EVALEQUAL (AEVAL* DEGREE) 1)
              (PROGN
               (SETQ D
                       (AEVAL*
                        (LIST 'QUOTIENT (LIST 'PART (LIST 'COEFF NUMPOL N) 2)
                              DENPOL)))
               (SETQ C
                       (AEVAL*
                        (LIST 'PLUS
                              (LIST 'QUOTIENT
                                    (LIST 'QUOTIENT
                                          (LIST 'PART (LIST 'COEFF NUMPOL N) 1)
                                          D)
                                    DENPOL)
                              1)))
               (PROGN
                (PROG (J)
                  (SETQ J 1)
                 LAB
                  (COND
                   ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* DEGREE) J))
                    (RETURN NIL)))
                  (SETQ G (AEVAL* (LIST 'APPEND G (LIST 'LIST C))))
                  (SETQ J
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           J))
                  (GO LAB)))
               (AEVAL* 'NIL)))
             (T
              (AEVAL*
               (LIST 'NEWREDERR (LIST 'LIST POL " does not factorize.")))))))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL G)))) 
(PUT 'HYPERRECURSION-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'HYPERRECURSION-EVAL 'DEFINED-ON-LINE '878) 
(PUT 'HYPERRECURSION-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERRECURSION-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPERRECURSION-EVAL (U)
    (PROGN
     (COND
      ((EQUAL (LENGTH U) 4)
       (HYPERRECURSION0 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) (REVAL1 (CADDDR U) NIL) 1 ZB_ORDER))
      ((EQUAL (LENGTH U) 5)
       (HYPERRECURSION0 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
        (REVAL1 (CADDR U) NIL) (REVAL1 (CADDDR U) NIL) (CAR (CDDDDR U))
        (CAR (CDDDDR U))))
      (T (REDERR "illegal number of arguments"))))) 
(PUT 'HYPERRECURSION 'PSOPFN 'HYPERRECURSION-EVAL) 
(PUT 'HYPERRECURSION0 'NUMBER-OF-ARGS 6) 
(FLAG '(HYPERRECURSION0) 'OPFN) 
(PUT 'HYPERRECURSION0 'DEFINED-ON-LINE '899) 
(PUT 'HYPERRECURSION0 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERRECURSION0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERRECURSION0 (UPPER LOWER Z N MINI MAXI)
    (PROG (*FACTOR *EXP)
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN
       (AEVAL
        (LIST 'PART (LIST 'HYPERRECURSION1 UPPER LOWER Z N MINI MAXI) 1))))) 
(PUT 'HYPERRECURSION1 'NUMBER-OF-ARGS 6) 
(FLAG '(HYPERRECURSION1) 'OPFN) 
(PUT 'HYPERRECURSION1 'DEFINED-ON-LINE '907) 
(PUT 'HYPERRECURSION1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERRECURSION1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERRECURSION1 (UPPER LOWER Z N MINI MAXI)
    (PROG (TESTER RESULT1 B C D G BC DG UPL LOL FUNC J GOON X LISTE *FACTOR
           *EXP ORDER1)
      (AEVAL
       (CLEAR
        (LIST 'GOSPER_REPRESENTATION 'ZEILBERGER_REPRESENTATION
              'RATIONAL_CERTIFICATE)))
      (SETQ RESULT1 (AEVAL (MINUS 1)))
      (SETQ UPL (AEVAL (LIST 'ARGLENGTH UPPER)))
      (SETQ LOL (AEVAL (LIST 'ARGLENGTH LOWER)))
      (SETQ GOON (AEVAL 'T))
      (SETQ LISTE (AEVAL LOWER))
      (WHILE (BOOLVALUE* GOON)
             (PROGN
              (COND
               ((EVALEQUAL (AEVAL* LISTE) (AEVAL* (LIST 'LIST)))
                (SETQ GOON (AEVAL* 'NIL)))
               (T
                (PROGN
                 (SETQ X (AEVAL* (LIST 'FIRST LISTE)))
                 (COND
                  ((AND (FIXP (REVALX X)) (EVALLESSP (AEVAL* X) 1))
                   (SETQ GOON (AEVAL* 'NIL)))
                  (T (SETQ LISTE (AEVAL* (LIST 'REST LISTE))))))))))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'ARGLENGTH LISTE)) 0)
        (AEVAL (REDERR (REVALX "some lower index is a nonpositive integer")))))
      (SETQ FUNC (AEVAL (LIST 'HYPERTERM UPPER LOWER Z 'LOCAL_K)))
      (SETQ TESTER (AEVAL FUNC))
      (SETQ TESTER (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'ONERULES) TESTER)))
      (COND
       ((EVALNEQ (AEVAL TESTER) 0)
        (PROGN
         (SETQ TESTER
                 (AEVAL
                  (LIST 'QUOTIENT TESTER
                        (LIST 'SUB (LIST 'EQUAL N (LIST 'DIFFERENCE N 1))
                              TESTER))))
         (COND
          ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY TESTER N))))
           (AEVAL (REDERR (REVALX "algorithm not applicable")))))
         (AEVAL 'NIL))))
      (SETQ BC
              (AEVAL
               (LIST 'SIMPLIFY_COMBINATORIAL
                     (LIST 'QUOTIENT FUNC
                           (LIST 'SUB (LIST 'EQUAL N (LIST 'DIFFERENCE N 1))
                                 FUNC)))))
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ B (AEVAL (LIST 'NUM BC)))
      (SETQ C (AEVAL (LIST 'DEN BC)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL ",local_k)/F(") NIL NIL)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL "-1,local_k):=") NIL NIL)
         (ASSGNPRI (AEVAL (LIST 'QUOTIENT B C)) NIL 'LAST))))
      (AEVAL (ON (LIST 'EXP)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY BC N))))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "not rational") NIL 'ONLY)
            (ASSGNPRI (AEVAL "Zeilberger algorithm not applicable") NIL
                      'ONLY))))
         (RETURN (AEVAL (LIST 'EXTENDED_HYPERRECURSION1 UPPER LOWER Z N)))
         (AEVAL 'NIL))))
      (SETQ DG
              (AEVAL
               (LIST 'TIMES
                     (PROG (J FORALL-RESULT)
                       (SETQ J 1)
                       (SETQ FORALL-RESULT 1)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* UPL) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'TIMES
                                      (AEVAL*
                                       (LIST 'PLUS
                                             (LIST 'DIFFERENCE 'LOCAL_K 1)
                                             (LIST 'PART UPPER J)))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1))
                     (LIST 'QUOTIENT Z
                           (LIST 'TIMES
                                 (PROG (J FORALL-RESULT)
                                   (SETQ J 1)
                                   (SETQ FORALL-RESULT 1)
                                  LAB1
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE (AEVAL* LOL) J))
                                     (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (AEVAL*
                                            (LIST 'TIMES
                                                  (AEVAL*
                                                   (LIST 'PLUS
                                                         (LIST 'DIFFERENCE
                                                               'LOCAL_K 1)
                                                         (LIST 'PART LOWER J)))
                                                  FORALL-RESULT)))
                                   (SETQ J
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            J))
                                   (GO LAB1))
                                 'LOCAL_K)))))
      (SETQ D (AEVAL (LIST 'NUM DG)))
      (SETQ G (AEVAL (LIST 'DEN DG)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
          (ASSGNPRI (AEVAL N) NIL NIL)
          (ASSGNPRI (AEVAL ",local_k)/F(") NIL NIL)
          (ASSGNPRI (AEVAL N) NIL NIL)
          (ASSGNPRI (AEVAL ",local_k-1):=") NIL NIL)
          (ASSGNPRI (AEVAL (LIST 'QUOTIENT D G)) NIL 'LAST))
         (ASSGNPRI (AEVAL "Zeilberger algorithm applicable") NIL 'ONLY))))
      (SETQ ORDER1 (AEVAL 0))
      (PROG (J)
        (SETQ J (AEVAL* MINI))
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MAXI) J)) (RETURN NIL)))
        (COND
         ((EVALEQUAL (AEVAL* RESULT1) (MINUS 1))
          (SETQ RESULT1
                  (AEVAL*
                   (LIST 'SUMRECURSION2 FUNC B C D G 'LOCAL_K N 0 0 'NIL J))))
         ((EVALEQUAL (AEVAL* ORDER1) 0)
          (SETQ ORDER1 (AEVAL* (LIST 'DIFFERENCE J 1)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL RESULT1) (MINUS 1))
        (AEVAL
         (REDERR (REVALX "Zeilberger algorithm fails. Enlarge zb_order")))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_FACTOR))
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (COND
          ((BOOLVALUE*
            (REVALX
             (MINUSP
              (CDAR (CAR (CADR (REVAL1 (PREPSQ (CADR RESULT1)) NIL)))))))
           (SETQ RESULT1 (AEVAL (LIST 'MINUS RESULT1)))))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'LIST RESULT1 ORDER1))))) 
(PUT 'DETERMINE_POLYNOMIALS 'NUMBER-OF-ARGS 3) 
(FLAG '(DETERMINE_POLYNOMIALS) 'OPFN) 
(PUT 'DETERMINE_POLYNOMIALS 'DEFINED-ON-LINE '1007) 
(PUT 'DETERMINE_POLYNOMIALS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'DETERMINE_POLYNOMIALS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DETERMINE_POLYNOMIALS (R10 R20 K)
    (PROG (TMP R1DIVR2 P R1 R2 J JJ GAMMA1 *EXP *FACTOR)
      (AEVAL (ON (LIST 'EXP)))
      (SETK 'MAXSHIFT1 (AEVAL (LIST 'MAXSHIFT R10 R20 K)))
      (SETQ P (AEVAL 1))
      (SETQ R1 (AEVAL R10))
      (SETQ R2 (AEVAL R20))
      (PROG (JJ)
        (SETQ JJ 0)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'MAXSHIFT1) JJ)) (RETURN NIL)))
        (PROGN
         (SETQ GAMMA1
                 (AEVAL*
                  (LIST 'GCD R1
                        (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K JJ)) R2))))
         (COND
          ((EVALNEQ (AEVAL* GAMMA1) 1)
           (PROGN
            (SETQ R1 (AEVAL* (LIST 'QUOTIENT R1 GAMMA1)))
            (SETQ R2
                    (AEVAL*
                     (LIST 'QUOTIENT R2
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K JJ))
                                 GAMMA1))))
            (SETQ P
                    (AEVAL*
                     (LIST 'TIMES P
                           (PROG (J FORALL-RESULT)
                             (SETQ J 0)
                             (SETQ FORALL-RESULT 1)
                            LAB1
                             (COND
                              ((|AMINUSP:|
                                (LIST 'DIFFERENCE
                                      (AEVAL* (LIST 'DIFFERENCE JJ 1)) J))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (AEVAL*
                                      (LIST 'TIMES
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL K
                                                         (LIST 'DIFFERENCE K
                                                               J))
                                                   GAMMA1))
                                            FORALL-RESULT)))
                             (SETQ J
                                     ((LAMBDA (FORALL-RESULT)
                                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                      J))
                             (GO LAB1))))))))
         (AEVAL* 'NIL))
        (SETQ JJ
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 JJ))
        (GO LAB))
      (RETURN (AEVAL (LIST 'LIST P R1 R2))))) 
(PUT 'DETERMINE_POLYNOMIALS2 'NUMBER-OF-ARGS 3) 
(FLAG '(DETERMINE_POLYNOMIALS2) 'OPFN) 
(PUT 'DETERMINE_POLYNOMIALS2 'DEFINED-ON-LINE '1036) 
(PUT 'DETERMINE_POLYNOMIALS2 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'DETERMINE_POLYNOMIALS2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DETERMINE_POLYNOMIALS2 (R10 R20 K)
    (PROG (*EXP *FACTOR F1 F2 ORDER1 ORDER2 MA LEADJ LEADJJ JJ J R1 R2 P)
      (AEVAL (ON (LIST 'FACTOR)))
      (AEVAL (OFF (LIST 'EXP)))
      (SETQ P (AEVAL 1))
      (SETQ R1 (AEVAL R10))
      (SETQ R2 (AEVAL R20))
      (SETQ F1 (AEVAL (LIST 'OLD_FACTORIZE R1)))
      (SETQ F2 (AEVAL (LIST 'OLD_FACTORIZE R2)))
      (SETQ ORDER1 (AEVAL (LIST 'ARGLENGTH F1)))
      (SETQ ORDER2 (AEVAL (LIST 'ARGLENGTH F2)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J)) (RETURN NIL)))
        (PROG (JJ)
          (SETQ JJ 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER2) JJ)) (RETURN NIL)))
          (PROGN
           (SETK 'COMPLIST
                 (AEVAL*
                  (LIST 'COMPPOL (LIST 'PART F1 J) (LIST 'PART F2 JJ) K)))
           (SETK 'COMP (AEVAL* (LIST 'PART 'COMPLIST 1)))
           (SETQ LEADJ (AEVAL* (LIST 'PART 'COMPLIST 2)))
           (SETQ LEADJJ (AEVAL* (LIST 'PART 'COMPLIST 3)))
           (COND
            ((EVALGREATERP (AEVAL* 'COMP) (MINUS 1))
             (PROGN
              (SETK 'GAMMA1 (AEVAL* (LIST 'PART F1 J)))
              (SETK 'GAMMA2 (AEVAL* (LIST 'PART F2 JJ)))
              (SETQ R1 (AEVAL* (LIST 'QUOTIENT R1 'GAMMA1)))
              (SETQ R2
                      (AEVAL*
                       (LIST 'QUOTIENT R2
                             (LIST 'SUB
                                   (LIST 'EQUAL K (LIST 'DIFFERENCE K 'COMP))
                                   'GAMMA1))))
              (SETQ P
                      (AEVAL*
                       (LIST 'TIMES P
                             (PROG (JJ FORALL-RESULT)
                               (SETQ JJ 0)
                               (SETQ FORALL-RESULT 1)
                              LAB1
                               (COND
                                ((|AMINUSP:|
                                  (LIST 'DIFFERENCE
                                        (AEVAL* (LIST 'DIFFERENCE 'COMP 1))
                                        JJ))
                                 (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'TIMES
                                              (AEVAL*
                                               (LIST 'SUB
                                                     (LIST 'EQUAL K
                                                           (LIST 'DIFFERENCE K
                                                                 JJ))
                                                     'GAMMA1))
                                              FORALL-RESULT)))
                               (SETQ JJ
                                       ((LAMBDA (FORALL-RESULT)
                                          (AEVAL*
                                           (LIST 'PLUS FORALL-RESULT 1)))
                                        JJ))
                               (GO LAB1)))))
              (SETQ F1 (AEVAL* (LIST 'SETPART* F1 J (AEVAL* 1))))
              (SETQ F2 (AEVAL* (LIST 'SETPART* F2 JJ (AEVAL* 1))))
              (AEVAL* 'NIL)))))
          (SETQ JJ
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   JJ))
          (GO LAB))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (AEVAL (ON (LIST 'EXP)))
      (RETURN (AEVAL (LIST 'LIST P R1 R2))))) 
(PUT 'MAXSHIFT 'NUMBER-OF-ARGS 3) 
(FLAG '(MAXSHIFT) 'OPFN) 
(PUT 'MAXSHIFT 'DEFINED-ON-LINE '1075) 
(PUT 'MAXSHIFT 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'MAXSHIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAXSHIFT (P1 P2 K)
    (PROG (F1 F2 ORDER1 ORDER2 MA J JJ)
      (SETQ MA (AEVAL (MINUS 1)))
      (SETQ F1 (AEVAL (LIST 'OLD_FACTORIZE P1)))
      (SETQ F2 (AEVAL (LIST 'OLD_FACTORIZE P2)))
      (SETQ ORDER1 (AEVAL (LIST 'ARGLENGTH F1)))
      (SETQ ORDER2 (AEVAL (LIST 'ARGLENGTH F2)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER1) J)) (RETURN NIL)))
        (PROG (JJ)
          (SETQ JJ 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* ORDER2) JJ)) (RETURN NIL)))
          (SETQ MA
                  (AEVAL*
                   (LIST 'MAX MA
                         (LIST 'COMPPOL (LIST 'PART F1 J) (LIST 'PART F2 JJ)
                               K))))
          (SETQ JJ
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   JJ))
          (GO LAB))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL MA)))) 
(PUT 'MAXDEGF 'NUMBER-OF-ARGS 4) 
(FLAG '(MAXDEGF) 'OPFN) 
(PUT 'MAXDEGF 'DEFINED-ON-LINE '1091) 
(PUT 'MAXDEGF 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'MAXDEGF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAXDEGF (R1 R2 P K)
    (PROG (L DP HOLD HOLD2 *EXP *FACTOR)
      (AEVAL (ON (LIST 'EXP)))
      (SETK 'PMINUS
            (AEVAL
             (LIST 'DIFFERENCE (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) R1)
                   R2)))
      (SETK 'PPLUS
            (AEVAL
             (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) R1) R2)))
      (SETK 'LPLUS (AEVAL (LIST 'DEG 'PPLUS K)))
      (SETK 'LMINUS (AEVAL (LIST 'DEG 'PMINUS K)))
      (COND ((EVALEQUAL (AEVAL 'PMINUS) 0) (SETK 'LMINUS (AEVAL (MINUS 1)))))
      (SETQ DP (AEVAL (LIST 'DEG P K)))
      (COND
       ((EVALLEQ (AEVAL 'LPLUS) (AEVAL 'LMINUS))
        (RETURN (AEVAL (LIST 'MAX (LIST 'DIFFERENCE DP 'LMINUS) 0))))
       (T
        (PROGN
         (SETK 'EL
               (AEVAL
                (LIST 'PART (LIST 'COEFF 'PPLUS K) (LIST 'PLUS 'LPLUS 1))))
         (AEVAL 'NIL))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'ARGLENGTH (LIST 'COEFF 'PMINUS K)))
                   (AEVAL 'LPLUS))
        (SETK 'DLMINUS1 (AEVAL 0)))
       (T
        (SETK 'DLMINUS1 (AEVAL (LIST 'PART (LIST 'COEFF 'PMINUS K) 'LPLUS)))))
      (SETQ HOLD
              (AEVAL
               (LIST 'MINUS (LIST 'TIMES 2 (LIST 'QUOTIENT 'DLMINUS1 'EL)))))
      (SETQ HOLD2 (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE DP 'LPLUS) 1)))
      (COND
       ((AND (FIXP (REVALX HOLD)) (EVALGEQ (AEVAL HOLD) 0))
        (RETURN (AEVAL (LIST 'MAX HOLD HOLD2))))
       (T (RETURN (AEVAL (LIST 'MAX HOLD2 0))))))) 
(PUT 'COMPPOL 'NUMBER-OF-ARGS 3) 
(FLAG '(COMPPOL) 'OPFN) 
(PUT 'COMPPOL 'DEFINED-ON-LINE '1126) 
(PUT 'COMPPOL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'COMPPOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPPOL (F G K)
    (PROG (NN A B C D J *EXP)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ NN (AEVAL (LIST 'DEG F K)))
      (COND
       ((OR (EVALEQUAL (AEVAL NN) 0)
            (EVALNEQ (AEVAL NN) (AEVAL (LIST 'DEG G K))))
        (RETURN (AEVAL (LIST 'LIST (MINUS 1) 1 1)))))
      (SETQ A (AEVAL (LIST 'PART (LIST 'COEFF F K) (LIST 'PLUS NN 1))))
      (SETQ B (AEVAL (LIST 'PART (LIST 'COEFF F K) NN)))
      (SETQ C (AEVAL (LIST 'PART (LIST 'COEFF G K) (LIST 'PLUS NN 1))))
      (SETQ D (AEVAL (LIST 'PART (LIST 'COEFF G K) NN)))
      (SETQ J
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'QUOTIENT
                           (LIST 'QUOTIENT
                                 (LIST 'DIFFERENCE (LIST 'TIMES B C)
                                       (LIST 'TIMES A D))
                                 NN)
                           A)
                     C)))
      (COND
       ((NOT (FIXP (REVALX J))) (RETURN (AEVAL (LIST 'LIST (MINUS 1) 1 1))))
       ((EVALLESSP (AEVAL J) 0) (RETURN (AEVAL (LIST 'LIST (MINUS 1) 1 1)))))
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'TIMES C F)
                (LIST 'TIMES A
                      (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K J)) G))))
         0)
        (RETURN (AEVAL (LIST 'LIST J A C))))
       (T (RETURN (AEVAL (LIST 'LIST (MINUS 1) 1 1))))))) 
(PUT 'HYPERTERM 'NUMBER-OF-ARGS 4) 
(FLAG '(HYPERTERM) 'OPFN) 
(PUT 'HYPERTERM 'DEFINED-ON-LINE '1148) 
(PUT 'HYPERTERM 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HYPERTERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERTERM (UPPER LOWER Z K)
    (PROG (LU LL)
      (SETQ LU (AEVAL (LIST 'ARGLENGTH UPPER)))
      (SETQ LL (AEVAL (LIST 'ARGLENGTH LOWER)))
      (RETURN
       (AEVAL
        (LIST 'TIMES
              (PROG (J FORALL-RESULT)
                (SETQ J 1)
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LU) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'TIMES
                               (AEVAL*
                                (LIST 'POCHHAMMER (LIST 'PART UPPER J) K))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1))
              (LIST 'QUOTIENT (LIST 'EXPT Z K)
                    (LIST 'TIMES
                          (PROG (J FORALL-RESULT)
                            (SETQ J 1)
                            (SETQ FORALL-RESULT 1)
                           LAB1
                            (COND
                             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LL) J))
                              (RETURN FORALL-RESULT)))
                            (SETQ FORALL-RESULT
                                    (AEVAL*
                                     (LIST 'TIMES
                                           (AEVAL*
                                            (LIST 'POCHHAMMER
                                                  (LIST 'PART LOWER J) K))
                                           FORALL-RESULT)))
                            (SETQ J
                                    ((LAMBDA (FORALL-RESULT)
                                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                     J))
                            (GO LAB1))
                          (LIST 'FACTORIAL K)))))))) 
(PUT 'SIMPLIFY_COMBINATORIAL 'NUMBER-OF-ARGS 1) 
(FLAG '(SIMPLIFY_COMBINATORIAL) 'OPFN) 
(PUT 'SIMPLIFY_COMBINATORIAL 'DEFINED-ON-LINE '1158) 
(PUT 'SIMPLIFY_COMBINATORIAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SIMPLIFY_COMBINATORIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY_COMBINATORIAL (TERM1) (LIST 'SIMPLIFY_GAMMA (LIST 'TOGAMMA TERM1))) 
(PUT 'TOGAMMA 'NUMBER-OF-ARGS 1) 
(FLAG '(TOGAMMA) 'OPFN) 
(PUT 'TOGAMMA 'DEFINED-ON-LINE '1169) 
(PUT 'TOGAMMA 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TOGAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOGAMMA (TERM1)
    (PROG ()
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'PROD (LIST '~ 'TERM) (LIST '~ 'K)
                                       (LIST '~ 'M1) (LIST '~ 'M2))
                                 (LIST 'PRODUCTTOPOCHHAMMER 'TERM 'K 'M1 'M2)))
                     TERM1)))
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'LOCAL_PROD (LIST '~ 'TERM) (LIST '~ 'K)
                                       (LIST '~ 'M1) (LIST '~ 'M2))
                                 (LIST 'PROD (LIST '~ 'TERM) (LIST '~ 'K)
                                       (LIST '~ 'M1) (LIST '~ 'M2))))
                     TERM1)))
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'POCHHAMMER 0 (LIST '~ 'K))
                                 0))
                     TERM1)))
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'POCHHAMMER (LIST '~ 'N) (LIST '~ 'K))
                                 (LIST 'QUOTIENT
                                       (LIST 'GAMMA
                                             (LIST 'PLUS (LIST '~ 'N)
                                                   (LIST '~ 'K)))
                                       (LIST 'GAMMA (LIST '~ 'N)))))
                     TERM1)))
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'BINOMIAL (LIST '~ 'N) (LIST '~ 'K))
                                 (LIST 'QUOTIENT (LIST 'FACTORIAL (LIST '~ 'N))
                                       (LIST 'TIMES
                                             (LIST 'FACTORIAL
                                                   (LIST 'DIFFERENCE
                                                         (LIST '~ 'N)
                                                         (LIST '~ 'K)))
                                             (LIST 'FACTORIAL (LIST '~ 'K))))))
                     TERM1)))
      (SETQ TERM1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'FACTORIAL (LIST '~ 'K))
                                 (LIST 'GAMMA (LIST 'PLUS (LIST '~ 'K) 1))))
                     TERM1)))
      (RETURN (AEVAL TERM1)))) 
(PUT 'RATSIMPLIFY_GAMMA 'NUMBER-OF-ARGS 1) 
(FLAG '(RATSIMPLIFY_GAMMA) 'OPFN) 
(PUT 'RATSIMPLIFY_GAMMA 'DEFINED-ON-LINE '1187) 
(PUT 'RATSIMPLIFY_GAMMA 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'RATSIMPLIFY_GAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATSIMPLIFY_GAMMA (TERM1)
    (PROG (*EXP *FACTOR DENO NUME LN LD DEGA NUGA DEREST NUREST LNUREST LNUGA
           LDEREST LDEGA JJ J SP TERM2 TMP)
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ DENO (AEVAL (LIST 'DEN TERM1)))
      (SETQ NUME (AEVAL (LIST 'NUM TERM1)))
      (SETQ NUREST (AEVAL (LIST 'LIST)))
      (SETQ NUGA (AEVAL (LIST 'LIST)))
      (SETQ DEREST (AEVAL (LIST 'LIST)))
      (SETQ DEGA (AEVAL (LIST 'LIST)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'ARGLENGTH DENO)) 0)
        (PROGN
         (COND
          ((NOT (EVALEQUAL (AEVAL (LIST 'PART DENO 0)) (AEVAL 'TIMES)))
           (COND
            ((NOT (FREEOF (REVALX DENO) (REVALX 'GAMMA)))
             (PROGN
              (SETQ DENO (AEVAL (LIST 'STRIP_POWER DENO)))
              (SETQ TMP (AEVAL (LIST 'PART DENO 1)))
              (COND
               ((NOT (EVALEQUAL (AEVAL (LIST 'PART TMP 0)) (AEVAL 'GAMMA)))
                (RETURN (AEVAL TERM1)))
               (T (SETQ DEGA (AEVAL DENO))))))
            (T (SETQ DEREST (AEVAL (LIST 'STRIP_POWER DENO))))))
          (T
           (PROGN
            (SETQ LD (AEVAL (LIST 'ARGLENGTH DENO)))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LD) J)) (RETURN NIL)))
              (PROGN
               (SETQ SP (AEVAL* (LIST 'STRIP_POWER (LIST 'PART DENO J))))
               (SETQ TMP (AEVAL* (LIST 'PART SP 1)))
               (COND
                ((AND (NOT (FREEOF (REVALX TMP) (REVALX 'GAMMA)))
                      (EVALEQUAL (AEVAL* (LIST 'PART TMP 0)) (AEVAL* 'GAMMA)))
                 (SETQ DEGA (AEVAL* (LIST 'APPEND DEGA SP))))
                (T (SETQ DEREST (AEVAL* (LIST 'APPEND DEREST SP)))))
               (AEVAL* 'NIL))
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (GO LAB))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))
       (T (SETQ DEREST (AEVAL (LIST 'LIST DENO)))))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'ARGLENGTH NUME)) 0)
        (PROGN
         (COND
          ((NOT (EVALEQUAL (AEVAL (LIST 'PART NUME 0)) (AEVAL 'TIMES)))
           (COND
            ((NOT (FREEOF (REVALX NUME) (REVALX 'GAMMA)))
             (PROGN
              (SETQ NUME (AEVAL (LIST 'STRIP_POWER NUME)))
              (SETQ TMP (AEVAL (LIST 'PART NUME 1)))
              (COND
               ((NOT (EVALEQUAL (AEVAL (LIST 'PART TMP 0)) (AEVAL 'GAMMA)))
                (RETURN (AEVAL TERM1)))
               (T (SETQ NUGA (AEVAL NUME))))))
            (T (SETQ NUREST (AEVAL (LIST 'STRIP_POWER NUME))))))
          (T
           (PROGN
            (SETQ LN (AEVAL (LIST 'ARGLENGTH NUME)))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LN) J)) (RETURN NIL)))
              (PROGN
               (SETQ SP (AEVAL* (LIST 'STRIP_POWER (LIST 'PART NUME J))))
               (SETQ TMP (AEVAL* (LIST 'PART SP 1)))
               (COND
                ((AND (NOT (FREEOF (REVALX TMP) (REVALX 'GAMMA)))
                      (EVALEQUAL (AEVAL* (LIST 'PART TMP 0)) (AEVAL* 'GAMMA)))
                 (SETQ NUGA (AEVAL* (LIST 'APPEND NUGA SP))))
                (T (SETQ NUREST (AEVAL* (LIST 'APPEND NUREST SP)))))
               (AEVAL* 'NIL))
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (GO LAB))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))
       (T (SETQ NUREST (AEVAL (LIST 'LIST NUME)))))
      (SETQ LDEGA (AEVAL (LIST 'ARGLENGTH DEGA)))
      (SETQ LDEREST (AEVAL (LIST 'ARGLENGTH DEREST)))
      (SETQ LNUGA (AEVAL (LIST 'ARGLENGTH NUGA)))
      (SETQ LNUREST (AEVAL (LIST 'ARGLENGTH NUREST)))
      (COND
       ((EVALGREATERP (AEVAL LDEGA) 0)
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LDEGA) J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (AEVAL* (LIST 'PART DEGA J)))
            (SETQ TMP (AEVAL* (LIST 'PART TMP 1)))
            (PROG (JJ)
              (SETQ JJ 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LDEREST) JJ))
                (RETURN NIL)))
              (COND
               ((EVALEQUAL
                 (AEVAL* (LIST 'DIFFERENCE (LIST 'PART DEREST JJ) TMP)) 0)
                (PROGN
                 (SETQ DEREST (AEVAL* (LIST 'SETPART* DEREST JJ (AEVAL* 1))))
                 (SETQ TMP (AEVAL* (LIST 'PLUS TMP 1)))
                 (SETQ DEGA
                         (AEVAL*
                          (LIST 'SETPART* DEGA J (AEVAL* (LIST 'GAMMA TMP)))))
                 (AEVAL* 'NIL))))
              (SETQ JJ
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       JJ))
              (GO LAB))
            (PROG (JJ)
              (SETQ JJ 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LNUREST) JJ))
                (RETURN NIL)))
              (COND
               ((EVALEQUAL
                 (AEVAL* (LIST 'DIFFERENCE (LIST 'PART NUREST JJ) TMP))
                 (MINUS 1))
                (PROGN
                 (SETQ NUREST (AEVAL* (LIST 'SETPART* NUREST JJ (AEVAL* 1))))
                 (SETQ TMP (AEVAL* (LIST 'DIFFERENCE TMP 1)))
                 (SETQ DEGA
                         (AEVAL*
                          (LIST 'SETPART* DEGA J (AEVAL* (LIST 'GAMMA TMP)))))
                 (AEVAL* 'NIL))))
              (SETQ JJ
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       JJ))
              (GO LAB))
            (AEVAL* 'NIL))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB)))))
      (COND
       ((EVALGREATERP (AEVAL LNUGA) 0)
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LNUGA) J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (AEVAL* (LIST 'PART NUGA J)))
            (SETQ TMP (AEVAL* (LIST 'PART TMP 1)))
            (PROG (JJ)
              (SETQ JJ 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LNUREST) JJ))
                (RETURN NIL)))
              (COND
               ((EVALEQUAL
                 (AEVAL* (LIST 'DIFFERENCE (LIST 'PART NUREST JJ) TMP)) 0)
                (PROGN
                 (SETQ NUREST (AEVAL* (LIST 'SETPART* NUREST JJ (AEVAL* 1))))
                 (SETQ TMP (AEVAL* (LIST 'PLUS TMP 1)))
                 (SETQ NUGA
                         (AEVAL*
                          (LIST 'SETPART* NUGA J (AEVAL* (LIST 'GAMMA TMP)))))
                 (AEVAL* 'NIL))))
              (SETQ JJ
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       JJ))
              (GO LAB))
            (PROG (JJ)
              (SETQ JJ 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LDEREST) JJ))
                (RETURN NIL)))
              (COND
               ((EVALEQUAL
                 (AEVAL* (LIST 'DIFFERENCE (LIST 'PART DEREST JJ) TMP))
                 (MINUS 1))
                (PROGN
                 (SETQ DEREST (AEVAL* (LIST 'SETPART* DEREST JJ (AEVAL* 1))))
                 (SETQ TMP (AEVAL* (LIST 'DIFFERENCE TMP 1)))
                 (SETQ NUGA
                         (AEVAL*
                          (LIST 'SETPART* NUGA J (AEVAL* (LIST 'GAMMA TMP)))))
                 (AEVAL* 'NIL))))
              (SETQ JJ
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       JJ))
              (GO LAB))
            (AEVAL* 'NIL))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB)))))
      (SETQ TERM2 (AEVAL 1))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LNUGA) J)) (RETURN NIL)))
        (SETQ TERM2 (AEVAL* (LIST 'TIMES TERM2 (LIST 'PART NUGA J))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LNUREST) J)) (RETURN NIL)))
        (SETQ TERM2 (AEVAL* (LIST 'TIMES TERM2 (LIST 'PART NUREST J))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LDEGA) J)) (RETURN NIL)))
        (SETQ TERM2 (AEVAL* (LIST 'QUOTIENT TERM2 (LIST 'PART DEGA J))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LDEREST) J)) (RETURN NIL)))
        (SETQ TERM2 (AEVAL* (LIST 'QUOTIENT TERM2 (LIST 'PART DEREST J))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND ((EVALEQUAL (AEVAL TERM2) (AEVAL TERM1)) (RETURN (AEVAL TERM2)))
            (T (RETURN (AEVAL (LIST 'RATSIMPLIFY_GAMMA TERM2))))))) 
(PUT 'STRIP_POWER 'NUMBER-OF-ARGS 1) 
(FLAG '(STRIP_POWER) 'OPFN) 
(PUT 'STRIP_POWER 'DEFINED-ON-LINE '1326) 
(PUT 'STRIP_POWER 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'STRIP_POWER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRIP_POWER (TERM1)
    (PROG (J *FACTOR LIST1)
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ LIST1 (AEVAL (LIST 'LIST)))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'ARGLENGTH TERM1)) 2)
            (EVALNEQ (AEVAL (LIST 'PART TERM1 0)) (AEVAL 'EXPT))
            (NOT (FIXP (REVALX (LIST 'PART TERM1 2)))))
        (RETURN (AEVAL (LIST 'LIST TERM1))))
       (T
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'PART TERM1 2)) J))
            (RETURN NIL)))
          (SETQ LIST1
                  (AEVAL*
                   (LIST 'APPEND LIST1 (LIST 'LIST (LIST 'PART TERM1 1)))))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))))
      (RETURN (AEVAL LIST1)))) 
(PUT 'SIMPLIFY_GAMMA 'NUMBER-OF-ARGS 1) 
(FLAG '(SIMPLIFY_GAMMA) 'OPFN) 
(PUT 'SIMPLIFY_GAMMA 'DEFINED-ON-LINE '1352) 
(PUT 'SIMPLIFY_GAMMA 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SIMPLIFY_GAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY_GAMMA (TERM1)
    (PROG (*EXP *FACTOR *GCD HIGH HIGHL HIGHLENGTH J)
      (COND ((FREEOF (REVALX TERM1) (REVALX 'GAMMA)) (RETURN (AEVAL TERM1))))
      (SETQ HIGHL (AEVAL (LIST 'LIST)))
      (SETQ HIGHL (AEVAL (LIST 'HIGHEST_GAMMA_ORDER TERM1 HIGHL)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TIMER))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "flag:highl:=") NIL 'FIRST)
          (ASSGNPRI (AEVAL HIGHL) NIL NIL)
          (ASSGNPRI (AEVAL "at ") NIL 'LAST))
         (AEVAL (SHOWTIME 'NIL)))))
      (COND
       ((EVALEQUAL (AEVAL HIGHL) (AEVAL (LIST 'LIST))) (RETURN (AEVAL TERM1))))
      (SETQ TERM1 (AEVAL (LIST 'MATCHGAMMASHIFT TERM1 HIGHL)))
      (RETURN (AEVAL TERM1)))) 
(PUT 'MATCHGAMMASHIFT 'NUMBER-OF-ARGS 2) 
(FLAG '(MATCHGAMMASHIFT) 'OPFN) 
(PUT 'MATCHGAMMASHIFT 'DEFINED-ON-LINE '1385) 
(PUT 'MATCHGAMMASHIFT 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'MATCHGAMMASHIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATCHGAMMASHIFT (TERM1 HIGHL)
    (PROG (DENO NUME *FACTOR)
      (SETQ NUME (AEVAL (LIST 'NUM TERM1)))
      (SETQ DENO (AEVAL (LIST 'DEN TERM1)))
      (SETQ NUME
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'GAMMA (LIST '~ 'LOCAL_X))
                                 (LIST 'LISTSHIFT_GAMMA (LIST '~ 'LOCAL_X)
                                       HIGHL)))
                     NUME)))
      (SETQ NUME
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'LOCAL_GAMMA (LIST '~ 'LOCAL_X))
                                 (LIST 'GAMMA (LIST '~ 'LOCAL_X))))
                     NUME)))
      (COND ((EVALEQUAL (AEVAL NUME) 0) (RETURN 0)))
      (SETQ DENO
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'GAMMA (LIST '~ 'LOCAL_X))
                                 (LIST 'LISTSHIFT_GAMMA (LIST '~ 'LOCAL_X)
                                       HIGHL)))
                     DENO)))
      (SETQ DENO
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'LOCAL_GAMMA (LIST '~ 'LOCAL_X))
                                 (LIST 'GAMMA (LIST '~ 'LOCAL_X))))
                     DENO)))
      (RETURN (AEVAL (LIST 'QUOTIENT NUME DENO))))) 
(PUT 'HIGHEST_GAMMA_ORDER 'NUMBER-OF-ARGS 2) 
(FLAG '(HIGHEST_GAMMA_ORDER) 'OPFN) 
(PUT 'HIGHEST_GAMMA_ORDER 'DEFINED-ON-LINE '1400) 
(PUT 'HIGHEST_GAMMA_ORDER 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'HIGHEST_GAMMA_ORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HIGHEST_GAMMA_ORDER (TERM1 HIGHL)
    (PROG (JJJ JJ J MAX TERM1LENGTH LOCALHIGHL LOCALHIGHLLENGTH NEW)
      (SETQ TERM1LENGTH (AEVAL (LIST 'ARGLENGTH TERM1)))
      (COND
       ((OR (EVALLESSP (AEVAL TERM1LENGTH) 1)
            (FREEOF (REVALX TERM1) (REVALX 'GAMMA)))
        (RETURN (AEVAL HIGHL))))
      (SETQ NEW (AEVAL 1))
      (SETK 'HIGHLLENGTH (AEVAL (LIST 'ARGLENGTH HIGHL)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART TERM1 0)) (AEVAL 'GAMMA))
        (PROGN
         (COND
          ((EVALNEQ (AEVAL TERM1LENGTH) 1)
           (AEVAL (REDERR (REVALX "gamma has illegal number of arguments")))))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'HIGHLLENGTH) J))
             (RETURN NIL)))
           (COND
            ((FIXP
              (REVALX
               (LIST 'DIFFERENCE (LIST 'PART HIGHL J) (LIST 'PART TERM1 1))))
             (PROGN
              (COND
               ((EVALLESSP
                 (AEVAL*
                  (LIST 'DIFFERENCE (LIST 'PART HIGHL J) (LIST 'PART TERM1 1)))
                 0)
                (SETQ HIGHL
                        (AEVAL*
                         (LIST 'SETPART* HIGHL J
                               (AEVAL* (LIST 'PART TERM1 1)))))))
              (SETQ NEW (AEVAL* 0))
              (AEVAL* 'NIL))))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB))
         (COND
          ((EVALEQUAL (AEVAL NEW) 1)
           (SETQ HIGHL
                   (AEVAL
                    (LIST 'APPEND HIGHL (LIST 'LIST (LIST 'PART TERM1 1)))))))
         (AEVAL 'NIL)))
       (T
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* TERM1LENGTH) J))
            (RETURN NIL)))
          (PROGN
           (SETQ LOCALHIGHL
                   (AEVAL*
                    (LIST 'HIGHEST_GAMMA_ORDER (LIST 'PART TERM1 J)
                          (LIST 'LIST))))
           (SETQ LOCALHIGHLLENGTH (AEVAL* (LIST 'ARGLENGTH LOCALHIGHL)))
           (PROG (JJJ)
             (SETQ JJJ 1)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LOCALHIGHLLENGTH) JJJ))
               (RETURN NIL)))
             (PROGN
              (SETK 'HIGHLLENGTH (AEVAL* (LIST 'ARGLENGTH HIGHL)))
              (SETQ NEW (AEVAL* 1))
              (PROG (JJ)
                (SETQ JJ 1)
               LAB
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'HIGHLLENGTH) JJ))
                  (RETURN NIL)))
                (COND
                 ((FIXP
                   (REVALX
                    (LIST 'DIFFERENCE (LIST 'PART HIGHL JJ)
                          (LIST 'PART LOCALHIGHL JJJ))))
                  (PROGN
                   (COND
                    ((EVALLESSP
                      (AEVAL*
                       (LIST 'DIFFERENCE (LIST 'PART HIGHL JJ)
                             (LIST 'PART LOCALHIGHL JJJ)))
                      0)
                     (SETQ HIGHL
                             (AEVAL*
                              (LIST 'SETPART* HIGHL JJ
                                    (AEVAL* (LIST 'PART LOCALHIGHL JJJ)))))))
                   (SETQ NEW (AEVAL* 0)))))
                (SETQ JJ
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         JJ))
                (GO LAB))
              (COND
               ((EVALEQUAL (AEVAL* NEW) 1)
                (SETQ HIGHL
                        (AEVAL*
                         (LIST 'APPEND HIGHL
                               (LIST 'LIST (LIST 'PART LOCALHIGHL JJJ))))))))
             (SETQ JJJ
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      JJJ))
             (GO LAB))
           (AEVAL* 'NIL))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))))
      (RETURN (AEVAL HIGHL)))) 
(PUT 'GAMMASHIFT 'NUMBER-OF-ARGS 2) 
(FLAG '(GAMMASHIFT) 'OPFN) 
(PUT 'GAMMASHIFT 'DEFINED-ON-LINE '1446) 
(PUT 'GAMMASHIFT 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'GAMMASHIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GAMMASHIFT (TERM1 HIGHL)
    (PROG (LHIGHL TERM2 XX NMINUSXX J JJ N)
      (COND ((FREEOF (REVALX TERM1) (REVALX 'GAMMA)) (RETURN (AEVAL TERM1))))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'ARGLENGTH TERM1)) 1)
        (RETURN
         (AEVAL
          (LIST 'MAP (LIST 'GAMMASHIFT (LIST '~ 'ZBGLOBAL) HIGHL) TERM1)))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART TERM1 0)) (AEVAL 'GAMMA))
        (PROGN
         (SETQ LHIGHL (AEVAL (LIST 'ARGLENGTH HIGHL)))
         (SETQ TERM2 (AEVAL TERM1))
         (SETQ JJ (AEVAL 1))
         (WHILE
          (AND (EVALEQUAL (AEVAL* TERM1) (AEVAL* TERM2))
               (EVALLEQ (AEVAL* JJ) (AEVAL* LHIGHL)))
          (PROGN
           (SETQ XX (AEVAL* (LIST 'PART TERM1 1)))
           (SETQ N (AEVAL* (LIST 'PART HIGHL JJ)))
           (SETQ NMINUSXX (AEVAL* (LIST 'DIFFERENCE N XX)))
           (COND ((EVALEQUAL (AEVAL* NMINUSXX) 0) (SETQ TERM1 (AEVAL* 0))))
           (COND
            ((AND (FIXP (REVALX NMINUSXX)) (EVALNEQ (AEVAL* NMINUSXX) 0))
             (COND
              ((EVALGREATERP (AEVAL* NMINUSXX) 0)
               (SETQ TERM2
                       (AEVAL*
                        (LIST 'QUOTIENT (LIST 'GAMMA N)
                              (PROG (J FORALL-RESULT)
                                (SETQ J 1)
                                (SETQ FORALL-RESULT 1)
                               LAB1
                                (COND
                                 ((|AMINUSP:|
                                   (LIST 'DIFFERENCE (AEVAL* NMINUSXX) J))
                                  (RETURN FORALL-RESULT)))
                                (SETQ FORALL-RESULT
                                        (AEVAL*
                                         (LIST 'TIMES
                                               (AEVAL* (LIST 'DIFFERENCE N J))
                                               FORALL-RESULT)))
                                (SETQ J
                                        ((LAMBDA (FORALL-RESULT)
                                           (AEVAL*
                                            (LIST 'PLUS FORALL-RESULT 1)))
                                         J))
                                (GO LAB1))))))
              (T
               (SETQ TERM2
                       (AEVAL*
                        (LIST 'TIMES (LIST 'GAMMA N)
                              (PROG (J FORALL-RESULT)
                                (SETQ J 1)
                                (SETQ FORALL-RESULT 1)
                               LAB1
                                (COND
                                 ((|AMINUSP:|
                                   (LIST 'DIFFERENCE
                                         (AEVAL* (LIST 'MINUS NMINUSXX)) J))
                                  (RETURN FORALL-RESULT)))
                                (SETQ FORALL-RESULT
                                        (AEVAL*
                                         (LIST 'TIMES
                                               (AEVAL* (LIST 'DIFFERENCE XX J))
                                               FORALL-RESULT)))
                                (SETQ J
                                        ((LAMBDA (FORALL-RESULT)
                                           (AEVAL*
                                            (LIST 'PLUS FORALL-RESULT 1)))
                                         J))
                                (GO LAB1)))))))))
           (SETQ JJ (AEVAL* (LIST 'PLUS JJ 1)))
           (AEVAL* 'NIL)))
         (RETURN (AEVAL TERM2))))
       (T
        (RETURN
         (AEVAL
          (LIST 'MAP (LIST 'GAMMASHIFT (LIST '~ 'ZBGLOBAL) HIGHL) TERM1))))))) 
(PUT 'SHIFT_GAMMA 'NUMBER-OF-ARGS 2) 
(FLAG '(SHIFT_GAMMA) 'OPFN) 
(PUT 'SHIFT_GAMMA 'DEFINED-ON-LINE '1479) 
(PUT 'SHIFT_GAMMA 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SHIFT_GAMMA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SHIFT_GAMMA (XX N)
    (PROG (NMINUSX J)
      (SETQ NMINUSX (AEVAL (LIST 'DIFFERENCE N XX)))
      (COND
       ((NOT (FIXP (REVALX NMINUSX))) (RETURN (AEVAL (LIST 'LOCAL_GAMMA XX)))))
      (COND
       ((EVALGREATERP (AEVAL NMINUSX) 0)
        (RETURN
         (AEVAL
          (LIST 'QUOTIENT (LIST 'LOCAL_GAMMA N)
                (PROG (J FORALL-RESULT)
                  (SETQ J 1)
                  (SETQ FORALL-RESULT 1)
                 LAB1
                  (COND
                   ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* NMINUSX) J))
                    (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (AEVAL*
                           (LIST 'TIMES (AEVAL* (LIST 'DIFFERENCE N J))
                                 FORALL-RESULT)))
                  (SETQ J
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           J))
                  (GO LAB1))))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES (LIST 'LOCAL_GAMMA N)
                (PROG (J FORALL-RESULT)
                  (SETQ J 1)
                  (SETQ FORALL-RESULT 1)
                 LAB1
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'MINUS NMINUSX)) J))
                    (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (AEVAL*
                           (LIST 'TIMES (AEVAL* (LIST 'DIFFERENCE XX J))
                                 FORALL-RESULT)))
                  (SETQ J
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           J))
                  (GO LAB1))))))))) 
(PUT 'LISTSHIFT_GAMMA 'NUMBER-OF-ARGS 2) 
(FLAG '(LISTSHIFT_GAMMA) 'OPFN) 
(PUT 'LISTSHIFT_GAMMA 'DEFINED-ON-LINE '1492) 
(PUT 'LISTSHIFT_GAMMA 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'LISTSHIFT_GAMMA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTSHIFT_GAMMA (XX HIGHL)
    (PROG (LHIGHL NMINUSX J N RET)
      (SETQ LHIGHL (AEVAL (LIST 'ARGLENGTH HIGHL)))
      (SETQ RET (AEVAL (LIST 'LOCAL_GAMMA XX)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* LHIGHL) J)) (RETURN NIL)))
        (PROGN
         (SETQ N (AEVAL* (LIST 'PART HIGHL J)))
         (SETQ NMINUSX (AEVAL* (LIST 'DIFFERENCE N XX)))
         (COND
          ((FIXP (REVALX NMINUSX))
           (PROGN
            (COND
             ((EVALGREATERP (AEVAL* NMINUSX) 0)
              (SETQ RET
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'LOCAL_GAMMA N)
                             (PROG (J FORALL-RESULT)
                               (SETQ J 1)
                               (SETQ FORALL-RESULT 1)
                              LAB1
                               (COND
                                ((|AMINUSP:|
                                  (LIST 'DIFFERENCE (AEVAL* NMINUSX) J))
                                 (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'TIMES
                                              (AEVAL* (LIST 'DIFFERENCE N J))
                                              FORALL-RESULT)))
                               (SETQ J
                                       ((LAMBDA (FORALL-RESULT)
                                          (AEVAL*
                                           (LIST 'PLUS FORALL-RESULT 1)))
                                        J))
                               (GO LAB1))))))
             (T
              (SETQ RET
                      (AEVAL*
                       (LIST 'TIMES (LIST 'LOCAL_GAMMA N)
                             (PROG (J FORALL-RESULT)
                               (SETQ J 1)
                               (SETQ FORALL-RESULT 1)
                              LAB1
                               (COND
                                ((|AMINUSP:|
                                  (LIST 'DIFFERENCE
                                        (AEVAL* (LIST 'MINUS NMINUSX)) J))
                                 (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'TIMES
                                              (AEVAL* (LIST 'DIFFERENCE XX J))
                                              FORALL-RESULT)))
                               (SETQ J
                                       ((LAMBDA (FORALL-RESULT)
                                          (AEVAL*
                                           (LIST 'PLUS FORALL-RESULT 1)))
                                        J))
                               (GO LAB1)))))))
            (AEVAL* 'NIL)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL RET)))) 
(PUT 'PRODUCTTOPOCHHAMMER 'NUMBER-OF-ARGS 4) 
(FLAG '(PRODUCTTOPOCHHAMMER) 'OPFN) 
(PUT 'PRODUCTTOPOCHHAMMER 'DEFINED-ON-LINE '1513) 
(PUT 'PRODUCTTOPOCHHAMMER 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'PRODUCTTOPOCHHAMMER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRODUCTTOPOCHHAMMER (TERM K M1 M2)
    (PROG (FEHLER AR CO AA BB LISTE TLENGTH J PA)
      (SETQ FEHLER (AEVAL 'NIL))
      (COND
       ((EVALNEQ (AEVAL (LIST 'DEN TERM)) 1)
        (RETURN
         (AEVAL
          (LIST 'QUOTIENT (LIST 'PRODUCTTOPOCHHAMMER (LIST 'NUM TERM) K M1 M2)
                (LIST 'PRODUCTTOPOCHHAMMER (LIST 'DEN TERM) K M1 M2))))))
      (SETQ LISTE (AEVAL (LIST 'OLD_FACTORIZE TERM)))
      (SETQ TLENGTH (AEVAL (LIST 'ARGLENGTH LISTE)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* TLENGTH) J)) (RETURN NIL)))
        (PROGN
         (SETQ PA (AEVAL* (LIST 'PART LISTE J)))
         (SETQ CO (AEVAL* (LIST 'COEFF PA K)))
         (SETQ AR (AEVAL* (LIST 'ARGLENGTH CO)))
         (COND ((EVALGREATERP (AEVAL* AR) 2) (SETQ FEHLER (AEVAL* 'T)))
               ((EVALLESSP (AEVAL* AR) 2)
                (SETQ LISTE
                        (AEVAL*
                         (LIST 'SETPART* LISTE J
                               (AEVAL*
                                (LIST 'EXPT PA
                                      (LIST 'PLUS (LIST 'DIFFERENCE M2 M1)
                                            1)))))))
               (T
                (PROGN
                 (SETQ AA (AEVAL* (LIST 'PART CO 2)))
                 (SETQ BB (AEVAL* (LIST 'DIFFERENCE (LIST 'QUOTIENT PA AA) K)))
                 (COND
                  ((EVALEQUAL (AEVAL* BB) 0)
                   (SETQ LISTE
                           (AEVAL*
                            (LIST 'SETPART* LISTE J
                                  (AEVAL*
                                   (LIST 'POCHHAMMER
                                         (LIST 'PLUS M1 (LIST 'PART CO 1))
                                         (LIST 'PLUS (LIST 'DIFFERENCE M2 M1)
                                               1)))))))
                  (T
                   (SETQ LISTE
                           (AEVAL*
                            (LIST 'SETPART* LISTE J
                                  (AEVAL*
                                   (LIST 'TIMES
                                         (LIST 'EXPT AA
                                               (LIST 'DIFFERENCE M2 M1))
                                         (LIST 'QUOTIENT
                                               (LIST 'POCHHAMMER BB
                                                     (LIST 'PLUS M2 1))
                                               (LIST 'POCHHAMMER BB
                                                     M1)))))))))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((BOOLVALUE* FEHLER) (RETURN (AEVAL (LIST 'LOCAL_PROD TERM K M1 M2)))))
      (RETURN
       (PROG (J FORALL-RESULT)
         (SETQ J 1)
         (SETQ FORALL-RESULT 1)
        LAB1
         (COND
          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* TLENGTH) J))
           (RETURN FORALL-RESULT)))
         (SETQ FORALL-RESULT
                 (AEVAL*
                  (LIST 'TIMES (AEVAL* (LIST 'PART LISTE J)) FORALL-RESULT)))
         (SETQ J
                 ((LAMBDA (FORALL-RESULT)
                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                  J))
         (GO LAB1))))) 
(PUT 'EXTENDED_GOSPER-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EXTENDED_GOSPER-EVAL 'DEFINED-ON-LINE '1551) 
(PUT 'EXTENDED_GOSPER-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_GOSPER-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTENDED_GOSPER-EVAL (U)
    ((LAMBDA (ABC)
       (PROGN
        (SETQ ABC
                (PROGN
                 (COND
                  ((EQUAL (LENGTH U) 2)
                   (EXTENDED_GOSPER1 (REVAL1 (CAR U) NIL)
                    (REVAL1 (CADR U) NIL)))
                  ((EQUAL (LENGTH U) 3)
                   (EXTENDED_GOSPER2 (REVAL1 (CAR U) NIL) (REVAL1 (CADR U) NIL)
                    (REVAL1 (CADDR U) NIL)))
                  ((EQUAL (LENGTH U) 4)
                   (EXTENDED_GOSPERBORDERS (REVAL1 (CAR U) NIL)
                    (REVAL1 (CADR U) NIL) (REVAL1 (CADDR U) NIL)
                    (REVAL1 (CADDDR U) NIL)))
                  (T (REDERR "illegal number of arguments")))))
        (COND ((EQCAR ABC '*SQ) (LIST '*SQ (CADR ABC) NIL)) (T ABC))))
     NIL)) 
(PUT 'EXTENDED_GOSPER 'PSOPFN 'EXTENDED_GOSPER-EVAL) 
(PUT 'EXTENDED_GOSPERBORDERS 'NUMBER-OF-ARGS 4) 
(FLAG '(EXTENDED_GOSPERBORDERS) 'OPFN) 
(PUT 'EXTENDED_GOSPERBORDERS 'DEFINED-ON-LINE '1565) 
(PUT 'EXTENDED_GOSPERBORDERS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_GOSPERBORDERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_GOSPERBORDERS (TERM1 K K0 K1)
    (PROG (TMP GOSPER2 *FACTOR)
      (SETQ GOSPER2 (AEVAL (LIST 'EXTENDED_GOSPER1 TERM1 K)))
      (COND
       ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'UP))
        (SETQ GOSPER2
                (AEVAL (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) GOSPER2)))))
      (SETQ TMP
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'SUB (LIST 'EQUAL K K1) GOSPER2)
                     (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K0 1))
                           GOSPER2))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_FACTOR))
        (PROGN
         (AEVAL (ON (LIST 'FACTOR)))
         (RETURN (AEVAL (LIST 'QUOTIENT (LIST 'NUM TMP) (LIST 'DEN TMP))))))
       (T (RETURN (AEVAL TMP)))))) 
(PUT 'EXTENDED_GOSPER2 'NUMBER-OF-ARGS 3) 
(FLAG '(EXTENDED_GOSPER2) 'OPFN) 
(PUT 'EXTENDED_GOSPER2 'DEFINED-ON-LINE '1581) 
(PUT 'EXTENDED_GOSPER2 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_GOSPER2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_GOSPER2 (TERM1 K M)
    (PROG (*EXP *FACTOR S TMP)
      (SETQ TMP
              (AEVAL
               (LIST 'GOSPER1
                     (LIST 'SUB (LIST 'EQUAL K (LIST 'TIMES K M)) TERM1) K)))
      (COND
       ((EVALEQUAL (AEVAL TMP) (AEVAL 'ZB_GANCFSE))
        (AEVAL
         (LIST 'NEWREDERR
               (LIST 'LIST "extended Gosper algorithm (Koepf): no " M
                     "-fold hypergeometric solution")))))
      (SETQ S (AEVAL (LIST 'SUB (LIST 'EQUAL K (LIST 'QUOTIENT K M)) TMP)))
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN (AEVAL S)))) 
(PUT 'EXTENDED_GOSPER1 'NUMBER-OF-ARGS 2) 
(FLAG '(EXTENDED_GOSPER1) 'OPFN) 
(PUT 'EXTENDED_GOSPER1 'DEFINED-ON-LINE '1593) 
(PUT 'EXTENDED_GOSPER1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_GOSPER1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_GOSPER1 (TERM1 K)
    (PROG (SOL *FACTOR J L PARTJ S M TMP)
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "Koepf extension of Gosper algorithm entered...") NIL
                  'ONLY)))
      (SETK 'LIST1 (AEVAL (LIST 'ARGUMENTLIST TERM1 (LIST 'LIST))))
      (COND
       ((EVALEQUAL (AEVAL 'LIST1) (AEVAL (LIST 'LIST)))
        (RETURN (AEVAL (LIST 'GOSPER0 TERM1 K)))))
      (SETK 'LIST2
            (PROG (PARTJ FORALL-RESULT FORALL-ENDPTR)
              (SETQ PARTJ (GETRLIST (AEVAL 'LIST1)))
              (COND ((NULL PARTJ) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (PARTJ)
                                  (AEVAL (LIST 'LINEARFACTOR PARTJ K)))
                                (CAR PARTJ))
                               NIL)))
             LOOPLABEL
              (SETQ PARTJ (CDR PARTJ))
              (COND ((NULL PARTJ) (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PARTJ) (AEVAL (LIST 'LINEARFACTOR PARTJ K)))
                        (CAR PARTJ))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETQ M (AEVAL (LIST 'LCML 'LIST2)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "linearizing integer with respect to ") NIL 'FIRST)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL " is ") NIL NIL)
         (ASSGNPRI (AEVAL M) NIL 'LAST))))
      (SETQ S (AEVAL (LIST 'EXTENDED_GOSPER2 TERM1 K M)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "s(") NIL 'FIRST)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "):=") NIL NIL)
         (ASSGNPRI (AEVAL S) NIL 'LAST))))
      (SETQ SOL
              (PROG (J FORALL-RESULT)
                (SETQ J 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE M 1)) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'SUB
                                      (LIST 'EQUAL K (LIST 'DIFFERENCE K J))
                                      S))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1)))
      (COND
       ((EVALEQUAL (AEVAL 'ZB_DIRECTION) (AEVAL 'UP))
        (SETQ SOL (AEVAL (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) SOL)))))
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN (AEVAL SOL)))) 
(PUT 'EXTENDED_SUMRECURSION-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EXTENDED_SUMRECURSION-EVAL 'DEFINED-ON-LINE '1615) 
(PUT 'EXTENDED_SUMRECURSION-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_SUMRECURSION-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTENDED_SUMRECURSION-EVAL (U)
    ((LAMBDA (ABC)
       (PROGN
        (SETQ ABC
                (PROGN
                 (COND
                  ((EQUAL (LENGTH U) 3)
                   (EXTENDED_SUMRECURSION0 (REVAL1 (CAR U) NIL)
                    (REVAL1 (CADR U) NIL) (REVAL1 (CADDR U) NIL)))
                  ((EQUAL (LENGTH U) 5)
                   (EXTENDED_SUMRECURSION20 (REVAL1 (CAR U) NIL)
                    (REVAL1 (CADR U) NIL) (REVAL1 (CADDR U) NIL)
                    (REVAL1 (CADDDR U) NIL) (CAR (CDDDDR U))))
                  (T (REDERR "illegal number of arguments")))))
        (COND ((EQCAR ABC '*SQ) (LIST '*SQ (CADR ABC) NIL)) (T ABC))))
     NIL)) 
(PUT 'EXTENDED_SUMRECURSION 'PSOPFN 'EXTENDED_SUMRECURSION-EVAL) 
(PUT 'EXTENDED_SUMRECURSION0 'NUMBER-OF-ARGS 3) 
(FLAG '(EXTENDED_SUMRECURSION0) 'OPFN) 
(PUT 'EXTENDED_SUMRECURSION0 'DEFINED-ON-LINE '1628) 
(PUT 'EXTENDED_SUMRECURSION0 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_SUMRECURSION0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_SUMRECURSION0 (TERM1 K N)
    (PROG (*FACTOR *EXP)
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN (AEVAL (LIST 'PART (LIST 'EXTENDED_SUMRECURSION1 TERM1 K N) 1))))) 
(PUT 'EXTENDED_SUMRECURSION1 'NUMBER-OF-ARGS 3) 
(FLAG '(EXTENDED_SUMRECURSION1) 'OPFN) 
(PUT 'EXTENDED_SUMRECURSION1 'DEFINED-ON-LINE '1637) 
(PUT 'EXTENDED_SUMRECURSION1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_SUMRECURSION1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_SUMRECURSION1 (TERM1 K N)
    (PROG (M J L PARTJ S *EXP DG BC)
      (AEVAL (ON (LIST 'EXP)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "Koepf extension of Zeilberger algorithm entered...")
                  NIL 'ONLY)))
      (SETK 'LIST1 (AEVAL (LIST 'ARGUMENTLIST TERM1 (LIST 'LIST))))
      (COND
       ((EVALEQUAL (AEVAL 'LIST1) (AEVAL (LIST 'LIST)))
        (RETURN (AEVAL (LIST 'SUMRECURSION1 TERM1 K N 1 ZB_ORDER)))))
      (SETK 'LISTK
            (PROG (PARTJ FORALL-RESULT FORALL-ENDPTR)
              (SETQ PARTJ (GETRLIST (AEVAL 'LIST1)))
              (COND ((NULL PARTJ) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (PARTJ)
                                  (AEVAL (LIST 'LINEARFACTOR PARTJ K)))
                                (CAR PARTJ))
                               NIL)))
             LOOPLABEL
              (SETQ PARTJ (CDR PARTJ))
              (COND ((NULL PARTJ) (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PARTJ) (AEVAL (LIST 'LINEARFACTOR PARTJ K)))
                        (CAR PARTJ))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETK 'LISTN
            (PROG (PARTJ FORALL-RESULT FORALL-ENDPTR)
              (SETQ PARTJ (GETRLIST (AEVAL 'LIST1)))
              (COND ((NULL PARTJ) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (PARTJ)
                                  (AEVAL (LIST 'LINEARFACTOR PARTJ N)))
                                (CAR PARTJ))
                               NIL)))
             LOOPLABEL
              (SETQ PARTJ (CDR PARTJ))
              (COND ((NULL PARTJ) (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PARTJ) (AEVAL (LIST 'LINEARFACTOR PARTJ N)))
                        (CAR PARTJ))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETQ L (AEVAL (LIST 'LCML 'LISTK)))
      (SETQ M (AEVAL (LIST 'LCML 'LISTN)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "linearizing integer with respect to ") NIL 'FIRST)
          (ASSGNPRI (AEVAL K) NIL NIL)
          (ASSGNPRI (AEVAL " is ") NIL NIL)
          (ASSGNPRI (AEVAL L) NIL 'LAST))
         (PROGN
          (ASSGNPRI (AEVAL "linearizing integer with respect to ") NIL 'FIRST)
          (ASSGNPRI (AEVAL N) NIL NIL)
          (ASSGNPRI (AEVAL " is ") NIL NIL)
          (ASSGNPRI (AEVAL M) NIL 'LAST))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALEQUAL (AEVAL M) 1) (EVALEQUAL (AEVAL L) 1))
        (PROGN
         (SETQ BC
                 (AEVAL
                  (LIST 'SIMPLIFY_COMBINATORIAL
                        (LIST 'QUOTIENT TERM1
                              (LIST 'SUB (LIST 'EQUAL N (LIST 'DIFFERENCE N 1))
                                    TERM1)))))
         (SETK 'GLOBALBC (AEVAL BC))
         (COND
          ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY BC N))))
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *ZB_TRACE))
              (PROGN
               (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
               (ASSGNPRI (AEVAL N) NIL NIL)
               (ASSGNPRI (AEVAL ",local_k)/F(") NIL NIL)
               (ASSGNPRI (AEVAL N) NIL NIL)
               (ASSGNPRI (AEVAL "-1,local_k):=") NIL NIL)
               (ASSGNPRI (AEVAL BC) NIL 'LAST))))
            (AEVAL (REDERR (REVALX "Zeilberger algorithm not applicable"))))))
         (SETQ DG
                 (AEVAL
                  (LIST 'SIMPLIFY_COMBINATORIAL
                        (LIST 'QUOTIENT TERM1
                              (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE K 1))
                                    TERM1)))))
         (AEVAL (ON (LIST 'EXP)))
         (COND
          ((NOT (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY DG K))))
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *ZB_TRACE))
              (PROGN
               (ASSGNPRI (AEVAL "F(") NIL 'FIRST)
               (ASSGNPRI (AEVAL N) NIL NIL)
               (ASSGNPRI (AEVAL ",") NIL NIL)
               (ASSGNPRI (AEVAL K) NIL NIL)
               (ASSGNPRI (AEVAL ")/F(") NIL NIL)
               (ASSGNPRI (AEVAL N) NIL NIL)
               (ASSGNPRI (AEVAL ",") NIL NIL)
               (ASSGNPRI (AEVAL K) NIL NIL)
               (ASSGNPRI (AEVAL "-1):=") NIL NIL)
               (ASSGNPRI (AEVAL DG) NIL 'LAST))))
            (AEVAL (REDERR (REVALX "Zeilberger algorithm not applicable"))))))
         (RETURN (AEVAL (LIST 'SUMRECURSION1 TERM1 K N 1 ZB_ORDER))))))
      (RETURN (AEVAL (LIST 'EXTENDED_SUMRECURSION2 TERM1 K N M L))))) 
(PUT 'EXTENDED_SUMRECURSION20 'NUMBER-OF-ARGS 5) 
(FLAG '(EXTENDED_SUMRECURSION20) 'OPFN) 
(PUT 'EXTENDED_SUMRECURSION20 'DEFINED-ON-LINE '1677) 
(PUT 'EXTENDED_SUMRECURSION20 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_SUMRECURSION20 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_SUMRECURSION20 (TERM1 K N M L)
    (PROG (*FACTOR *EXP)
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN
       (AEVAL (LIST 'PART (LIST 'EXTENDED_SUMRECURSION2 TERM1 K N M L) 1))))) 
(PUT 'EXTENDED_SUMRECURSION2 'NUMBER-OF-ARGS 5) 
(FLAG '(EXTENDED_SUMRECURSION2) 'OPFN) 
(PUT 'EXTENDED_SUMRECURSION2 'DEFINED-ON-LINE '1684) 
(PUT 'EXTENDED_SUMRECURSION2 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_SUMRECURSION2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_SUMRECURSION2 (TERM1 K N M L)
    (PROG (TERM2 TMPTERM RULE *FACTOR *EXP ORDER1)
      (SETQ TERM2
              (AEVAL
               (LIST 'SUB
                     (LIST 'LIST (LIST 'EQUAL K (LIST 'TIMES K L))
                           (LIST 'EQUAL N (LIST 'TIMES N M)))
                     TERM1)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "applying Zeilberger algorithm to F(") NIL 'FIRST)
         (ASSGNPRI (AEVAL N) NIL NIL)
         (ASSGNPRI (AEVAL ",") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "):=") NIL NIL)
         (ASSGNPRI (AEVAL TERM2) NIL 'LAST))))
      (SETQ TMPTERM (AEVAL (LIST 'SUMRECURSION1 TERM2 K N 1 ZB_ORDER)))
      (SETQ ORDER1 (AEVAL (LIST 'TIMES M (LIST 'PART TMPTERM 2))))
      (SETQ TMPTERM (AEVAL (LIST 'PART TMPTERM 1)))
      (SETQ TMPTERM
              (AEVAL
               (LIST 'SUB (LIST 'LIST (LIST 'EQUAL N (LIST 'QUOTIENT N M)))
                     TMPTERM)))
      (SETQ RULE
              (AEVAL
               (LIST 'LIST
                     (LIST 'REPLACEBY
                           (LIST 'SUMM
                                 (LIST 'QUOTIENT (LIST '~ 'NN) (LIST '~ 'MM)))
                           (LIST 'WHEN (LIST 'SUMM 'NN)
                                 (LIST 'EQUAL 'MM M))))))
      (SETQ TMPTERM
              (AEVAL (LIST 'NUM (LIST 'WHEREEXP (LIST 'LIST RULE) TMPTERM))))
      (AEVAL (OFF (LIST 'FACTOR)))
      (SETQ TMPTERM (AEVAL TMPTERM))
      (COND ((BOOLVALUE* (REVALX *ZB_FACTOR)) (AEVAL (ON (LIST 'FACTOR)))))
      (RETURN (AEVAL (LIST 'LIST TMPTERM ORDER1))))) 
(PUT 'EXTENDED_HYPERRECURSION-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EXTENDED_HYPERRECURSION-EVAL 'DEFINED-ON-LINE '1702) 
(PUT 'EXTENDED_HYPERRECURSION-EVAL 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_HYPERRECURSION-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTENDED_HYPERRECURSION-EVAL (U)
    ((LAMBDA (ABC)
       (PROGN
        (SETQ ABC
                (PROGN
                 (COND
                  ((EQUAL (LENGTH U) 4)
                   (EXTENDED_HYPERRECURSION0 (REVAL1 (CAR U) NIL)
                    (REVAL1 (CADR U) NIL) (REVAL1 (CADDR U) NIL)
                    (REVAL1 (CADDDR U) NIL)))
                  (T (REDERR "illegal number of arguments")))))
        (COND ((EQCAR ABC '*SQ) (LIST '*SQ (CADR ABC) NIL)) (T ABC))))
     NIL)) 
(PUT 'EXTENDED_HYPERRECURSION 'PSOPFN 'EXTENDED_HYPERRECURSION-EVAL) 
(PUT 'EXTENDED_HYPERRECURSION0 'NUMBER-OF-ARGS 4) 
(FLAG '(EXTENDED_HYPERRECURSION0) 'OPFN) 
(PUT 'EXTENDED_HYPERRECURSION0 'DEFINED-ON-LINE '1717) 
(PUT 'EXTENDED_HYPERRECURSION0 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_HYPERRECURSION0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_HYPERRECURSION0 (UPPER LOWER X N)
    (LIST 'PART (LIST 'EXTENDED_HYPERRECURSION1 UPPER LOWER X N) 1)) 
(PUT 'EXTENDED_HYPERRECURSION1 'NUMBER-OF-ARGS 4) 
(FLAG '(EXTENDED_HYPERRECURSION1) 'OPFN) 
(PUT 'EXTENDED_HYPERRECURSION1 'DEFINED-ON-LINE '1720) 
(PUT 'EXTENDED_HYPERRECURSION1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'EXTENDED_HYPERRECURSION1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTENDED_HYPERRECURSION1 (UPPER LOWER X N)
    (LIST 'EXTENDED_SUMRECURSION1 (LIST 'HYPERTERM UPPER LOWER X 'LOCAL_K)
          'LOCAL_K N)) 
(PUT 'LINEARFACTOR 'NUMBER-OF-ARGS 2) 
(FLAG '(LINEARFACTOR) 'OPFN) 
(PUT 'LINEARFACTOR 'DEFINED-ON-LINE '1724) 
(PUT 'LINEARFACTOR 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'LINEARFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINEARFACTOR (TERM1 N)
    (PROG (P CO)
      (SETQ CO (AEVAL (LIST 'COEFF TERM1 N)))
      (COND ((EVALEQUAL (AEVAL (LIST 'ARGLENGTH CO)) 1) (RETURN 1)))
      (SETQ P (AEVAL (LIST 'DEN (LIST 'PART CO 2))))
      (COND
       ((OR (EVALGREATERP (AEVAL (LIST 'ARGLENGTH CO)) 2)
            (NOT (FIXP (REVALX P))))
        (AEVAL (REDERR (REVALX "Extended Gosper algorithm not applicable")))))
      (RETURN (AEVAL P)))) 
(PUT 'LCML 'NUMBER-OF-ARGS 1) 
(FLAG '(LCML) 'OPFN) 
(PUT 'LCML 'DEFINED-ON-LINE '1735) 
(PUT 'LCML 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'LCML 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LCML (LIST1)
    (PROG (P1 L)
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (SETQ P1 (AEVAL (LIST 'PART LIST1 1)))
      (COND ((EVALEQUAL (AEVAL L) 1) (RETURN (AEVAL P1))))
      (COND
       ((EVALEQUAL (AEVAL L) 2)
        (RETURN (AEVAL (LIST 'LCM P1 (LIST 'PART LIST1 2))))))
      (RETURN (AEVAL (LIST 'LCM P1 (LIST 'LCML (LIST 'REST LIST1))))))) 
(PUT 'ARGUMENTLIST 'NUMBER-OF-ARGS 2) 
(FLAG '(ARGUMENTLIST) 'OPFN) 
(PUT 'ARGUMENTLIST 'DEFINED-ON-LINE '1746) 
(PUT 'ARGUMENTLIST 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'ARGUMENTLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ARGUMENTLIST (TERM1 LIST1)
    (PROG (HEAD1 J L)
      (SETQ L (AEVAL (LIST 'ARGLENGTH TERM1)))
      (COND ((EVALLESSP (AEVAL L) 1) (RETURN (AEVAL LIST1))))
      (SETQ HEAD1 (AEVAL (LIST 'PART TERM1 0)))
      (COND
       ((OR (EVALEQUAL (AEVAL HEAD1) (AEVAL 'GAMMA))
            (EVALEQUAL (AEVAL HEAD1) (AEVAL 'FACTORIAL)))
        (SETQ LIST1
                (AEVAL
                 (LIST 'APPEND LIST1 (LIST 'LIST (LIST 'PART TERM1 1))))))
       ((OR (EVALEQUAL (AEVAL HEAD1) (AEVAL 'POCHHAMMER))
            (EVALEQUAL (AEVAL HEAD1) (AEVAL 'BINOMIAL)))
        (SETQ LIST1
                (AEVAL
                 (LIST 'APPEND LIST1
                       (LIST 'LIST (LIST 'PART TERM1 1)
                             (LIST 'PART TERM1 2))))))
       (T
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
          (SETQ LIST1 (AEVAL* (LIST 'ARGUMENTLIST (LIST 'PART TERM1 J) LIST1)))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))))
      (RETURN (AEVAL LIST1)))) 
(PUT 'NEGINTOCCURS 'NUMBER-OF-ARGS 1) 
(FLAG '(NEGINTOCCURS) 'OPFN) 
(PUT 'NEGINTOCCURS 'DEFINED-ON-LINE '1775) 
(PUT 'NEGINTOCCURS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'NEGINTOCCURS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEGINTOCCURS (LIST1)
    (PROG (L TMP TMP2 J)
      (SETQ TMP2 (AEVAL 'NIL))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (COND ((EVALEQUAL (AEVAL L) 0) (RETURN (AEVAL 'NIL))))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ TMP (AEVAL* (LIST 'PART LIST1 J)))
         (COND
          ((AND (FIXP (REVALX TMP)) (EVALLESSP (AEVAL* TMP) 0))
           (SETQ TMP2 (AEVAL* 'T)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL TMP2)))) 
(PUT 'SUMTOHYPER 'NUMBER-OF-ARGS 2) 
(FLAG '(SUMTOHYPER) 'OPFN) 
(PUT 'SUMTOHYPER 'DEFINED-ON-LINE '1789) 
(PUT 'SUMTOHYPER 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SUMTOHYPER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUMTOHYPER (ANK K)
    (PROG (DE RAT NUMERATOR DENOMINATOR NUMFACTORS DENFACTORS LC L NUMLIST
           OLDNUMLIST OLDDENLIST TMP TMP2 NUMDEGREE DENLIST DENDEGREE I J LCDEN
           LCNUM *EXP *FACTOR *GCD GCDTERM)
      (AEVAL (ON (LIST 'EXP)))
      (AEVAL (ON (LIST 'GCD)))
      (SETQ ANK (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL ANK)))
      (SETQ DE
              (AEVAL
               (LIST 'SIMPLIFY_COMBINATORIAL
                     (LIST 'QUOTIENT
                           (LIST 'SUB (LIST 'EQUAL K (LIST 'PLUS K 1)) ANK)
                           ANK))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "a(") NIL 'FIRST)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "+1)/a(") NIL NIL)
         (ASSGNPRI (AEVAL K) NIL NIL)
         (ASSGNPRI (AEVAL "):=") NIL NIL)
         (ASSGNPRI (AEVAL DE) NIL 'LAST))))
      (SETQ NUMERATOR (AEVAL (LIST 'NUM DE)))
      (SETQ DENOMINATOR (AEVAL (LIST 'DEN DE)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'POLYNOMQ4 NUMERATOR K))))
        (AEVAL
         (REDERR (REVALX "cannot be converted into hypergeometric form")))))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'POLYNOMQ4 DENOMINATOR K))))
        (AEVAL
         (REDERR (REVALX "cannot be converted into hypergeometric form")))))
      (SETQ NUMERATOR (AEVAL NUMERATOR))
      (SETQ DENOMINATOR (AEVAL DENOMINATOR))
      (SETQ NUMFACTORS (AEVAL (LIST 'OLD_FACTORIZE NUMERATOR)))
      (SETQ DENFACTORS (AEVAL (LIST 'OLD_FACTORIZE DENOMINATOR)))
      (SETQ LCNUM (AEVAL (LIST 'LCOF NUMERATOR K)))
      (SETQ LCDEN (AEVAL (LIST 'LCOF DENOMINATOR K)))
      (COND ((EVALEQUAL (AEVAL LCNUM) 0) (SETQ LCNUM (AEVAL NUMERATOR))))
      (COND ((EVALEQUAL (AEVAL LCDEN) 0) (SETQ LCDEN (AEVAL DENOMINATOR))))
      (SETQ LC (AEVAL (LIST 'QUOTIENT LCNUM LCDEN)))
      (COND
       ((FREEOF (REVALX (LIST 'FIRST NUMFACTORS)) (REVALX K))
        (SETQ NUMFACTORS (AEVAL (LIST 'REST NUMFACTORS)))))
      (SETQ NUMLIST (AEVAL (LIST 'LIST)))
      (SETK 'LEN (AEVAL (LIST 'LENGTH NUMFACTORS)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'LEN) I)) (RETURN NIL)))
        (PROGN
         (SETK 'FIR (AEVAL* (LIST 'FIRST NUMFACTORS)))
         (COND
          ((NOT (FREEOF (REVALX 'FIR) (REVALX K)))
           (PROGN
            (SETK 'NEW
                  (AEVAL*
                   (LIST 'MINUS
                         (LIST 'PART (LIST 'FIRST (LIST 'SOLVE 'FIR K)) 2))))
            (SETQ NUMLIST (AEVAL* (LIST 'APPEND NUMLIST (LIST 'LIST 'NEW))))
            (AEVAL* 'NIL))))
         (SETQ NUMFACTORS (AEVAL* (LIST 'REST NUMFACTORS)))
         (AEVAL* 'NIL))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'MAXINT (AEVAL (LIST 'MAXPOSINT NUMLIST)))
      (SETK 'LEN (AEVAL (LIST 'LENGTH DENFACTORS)))
      (SETQ DENLIST (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'LEN) J)) (RETURN NIL)))
        (PROGN
         (SETK 'FIR (AEVAL* (LIST 'FIRST DENFACTORS)))
         (COND
          ((NOT (FREEOF (REVALX 'FIR) (REVALX K)))
           (PROGN
            (COND
             ((OR (NOT (BOOLVALUE* (REVALX (LIST 'POLYNOMQ4 'FIR K))))
                  (EVALGREATERP (AEVAL* (LIST 'DEG 'FIR K)) 2))
              (AEVAL* (REDERR (REVALX "not yet implemented"))))
             (T (SETQ TMP (AEVAL* (LIST 'SOLVE 'FIR K)))))
            (PROG (JJ)
              (SETQ JJ 1)
             LAB
              (COND
               ((|AMINUSP:|
                 (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH TMP)) JJ))
                (RETURN NIL)))
              (SETQ DENLIST
                      (AEVAL*
                       (LIST 'APPEND DENLIST
                             (LIST 'LIST
                                   (LIST 'MINUS
                                         (LIST 'PART (LIST 'PART TMP JJ)
                                               2))))))
              (SETQ JJ
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       JJ))
              (GO LAB))
            (AEVAL* 'NIL))))
         (SETQ DENFACTORS (AEVAL* (LIST 'REST DENFACTORS)))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETK 'MININT (AEVAL (LIST 'MINNEGINT DENLIST)))
      (COND
       ((EVALLEQ (AEVAL 'MININT) 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *ZB_TRACE))
           (PROGN
            (ASSGNPRI (AEVAL "shifting by ") NIL 'FIRST)
            (ASSGNPRI (AEVAL (LIST 'DIFFERENCE 1 'MININT)) NIL 'LAST))))
         (SETQ NUMLIST
                 (AEVAL
                  (LIST 'SUB
                        (LIST 'EQUAL K
                              (LIST 'PLUS K (LIST 'DIFFERENCE 1 'MININT)))
                        NUMLIST)))
         (COND
          ((EVALGREATERP (AEVAL (LIST 'NUMBEROFZEROS NUMLIST)) 0)
           (AEVAL (REDERR (REVALX "not yet implemented")))))))
       (T
        (PROGN
         (COND
          ((EVALGEQ (AEVAL 'MAXINT) 0)
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *ZB_TRACE))
              (PROGN
               (ASSGNPRI (AEVAL "shifting by ") NIL 'FIRST)
               (ASSGNPRI (AEVAL (LIST 'DIFFERENCE 1 'MAXINT)) NIL 'LAST))))
            (SETQ DENLIST
                    (AEVAL
                     (LIST 'SUB
                           (LIST 'EQUAL K
                                 (LIST 'PLUS K (LIST 'DIFFERENCE 1 'MAXINT)))
                           DENLIST)))
            (COND
             ((EVALGREATERP (AEVAL (LIST 'NUMBEROFZEROS DENLIST)) 0)
              (AEVAL (REDERR (REVALX "not yet implemented")))))
            (SETK 'MININT (AEVAL 'MAXINT))
            (AEVAL 'NIL)))))))
      (SETK 'SHIFTNUMBER (AEVAL (LIST 'DIFFERENCE 1 'MININT)))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (ASSGNPRI (AEVAL "calculating initial value") NIL 'ONLY)))
      (SETQ OLDDENLIST (AEVAL DENLIST))
      (SETQ DENLIST (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH OLDDENLIST)) J))
          (RETURN NIL)))
        (SETQ DENLIST
                (AEVAL*
                 (LIST 'APPEND
                       (LIST 'LIST
                             (LIST 'PLUS (LIST 'PART OLDDENLIST J)
                                   (LIST 'DIFFERENCE 1 'MININT)))
                       DENLIST)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (SETQ OLDNUMLIST (AEVAL NUMLIST))
      (SETQ NUMLIST (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH OLDNUMLIST)) J))
          (RETURN NIL)))
        (SETQ NUMLIST
                (AEVAL*
                 (LIST 'APPEND
                       (LIST 'LIST
                             (LIST 'PLUS (LIST 'PART OLDNUMLIST J)
                                   (LIST 'DIFFERENCE 1 'MININT)))
                       NUMLIST)))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND
       ((OR
         (EVALEQUAL
          (AEVAL
           (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE 1 'MININT))
                 (LIST 'DEN ANK)))
          0)
         (EVALEQUAL
          (AEVAL (LIST 'SUB (LIST 'EQUAL 'POCHHAMMER 'POCH) (LIST 'DEN ANK)))
          0))
        (SETQ TMP (AEVAL (LIST 'LIMIT ANK K (LIST 'DIFFERENCE 1 'MININT)))))
       (T
        (SETQ TMP
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL K (LIST 'DIFFERENCE 1 'MININT))
                       ANK)))))
      (COND
       ((MEMBER 1 (REVALX DENLIST))
        (PROGN
         (SETK 'TMPLIST (AEVAL (LIST 'LIST)))
         (SETK 'DONE (AEVAL 0))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH DENLIST)) I))
             (RETURN NIL)))
           (COND
            ((OR (NOT (EVALEQUAL (AEVAL* (LIST 'PART DENLIST I)) 1))
                 (BOOLVALUE* (REVALX 'DONE)))
             (SETK 'TMPLIST
                   (AEVAL*
                    (LIST 'APPEND 'TMPLIST
                          (LIST 'LIST (LIST 'PART DENLIST I))))))
            (T (SETK 'DONE (AEVAL* 1))))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (SETQ DENLIST (AEVAL 'TMPLIST))
         (AEVAL 'NIL)))
       (T (SETQ NUMLIST (AEVAL (LIST 'APPEND NUMLIST (LIST 'LIST 1))))))
      (SETQ TMP
              (AEVAL
               (LIST 'TIMES (LIST 'SIMPLIFY_COMBINATORIAL TMP)
                     (LIST 'HYPERGEOM NUMLIST DENLIST LC))))
      (COND
       ((BOOLVALUE* (REVALX *ZB_TRACE))
        (PROGN
         (ASSGNPRI (AEVAL "finished conversion in hypergeometric notation") NIL
                   'ONLY)
         (ASSGNPRI (AEVAL TMP) NIL 'ONLY)
         (AEVAL 'NIL))))
      (RETURN (AEVAL TMP)))) 
(PUT 'REMOVE_REDUNTANT_ELEMENTS 'NUMBER-OF-ARGS 2) 
(FLAG '(REMOVE_REDUNTANT_ELEMENTS) 'OPFN) 
(PUT 'REMOVE_REDUNTANT_ELEMENTS 'DEFINED-ON-LINE '1908) 
(PUT 'REMOVE_REDUNTANT_ELEMENTS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'REMOVE_REDUNTANT_ELEMENTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_REDUNTANT_ELEMENTS (DENLIST NUMLIST)
    (PROG (J JJ JJJ LN LD TMP)
      (SETQ LN (AEVAL (LIST 'ARGLENGTH NUMLIST)))
      (SETQ LD (AEVAL (LIST 'ARGLENGTH DENLIST)))
      (COND
       ((AND (EVALGREATERP (AEVAL LN) 0) (EVALGREATERP (AEVAL LD) 0))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH NUMLIST)) J))
             (RETURN NIL)))
           (PROG (JJ)
             (SETQ JJ 1)
            LAB
             (COND
              ((|AMINUSP:|
                (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH DENLIST)) JJ))
               (RETURN NIL)))
             (COND
              ((EVALEQUAL (AEVAL* (LIST 'PART NUMLIST J))
                          (AEVAL* (LIST 'PART DENLIST JJ)))
               (PROGN
                (SETQ TMP (AEVAL* DENLIST))
                (SETQ DENLIST (AEVAL* (LIST 'LIST)))
                (PROG (JJJ)
                  (SETQ JJJ 1)
                 LAB
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE JJ 1)) JJJ))
                    (RETURN NIL)))
                  (SETQ DENLIST
                          (AEVAL*
                           (LIST 'APPEND DENLIST
                                 (LIST 'LIST (LIST 'PART TMP JJJ)))))
                  (SETQ JJJ
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           JJJ))
                  (GO LAB))
                (PROG (JJJ)
                  (SETQ JJJ (AEVAL* (LIST 'PLUS JJ 1)))
                 LAB
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH TMP)) JJJ))
                    (RETURN NIL)))
                  (SETQ DENLIST
                          (AEVAL*
                           (LIST 'APPEND DENLIST
                                 (LIST 'LIST (LIST 'PART TMP JJJ)))))
                  (SETQ JJJ
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           JJJ))
                  (GO LAB))
                (SETQ TMP (AEVAL* NUMLIST))
                (SETQ NUMLIST (AEVAL* (LIST 'LIST)))
                (PROG (JJJ)
                  (SETQ JJJ 1)
                 LAB
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE J 1)) JJJ))
                    (RETURN NIL)))
                  (SETQ NUMLIST
                          (AEVAL*
                           (LIST 'APPEND NUMLIST
                                 (LIST 'LIST (LIST 'PART TMP JJJ)))))
                  (SETQ JJJ
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           JJJ))
                  (GO LAB))
                (PROG (JJJ)
                  (SETQ JJJ (AEVAL* (LIST 'PLUS J 1)))
                 LAB
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH TMP)) JJJ))
                    (RETURN NIL)))
                  (SETQ NUMLIST
                          (AEVAL*
                           (LIST 'APPEND NUMLIST
                                 (LIST 'LIST (LIST 'PART TMP JJJ)))))
                  (SETQ JJJ
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           JJJ))
                  (GO LAB))
                (SETQ JJ (AEVAL* (LIST 'ARGLENGTH DENLIST))))))
             (SETQ JJ
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      JJ))
             (GO LAB))
           (SETQ J
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    J))
           (GO LAB)))))
      (RETURN (AEVAL (LIST 'LIST DENLIST NUMLIST))))) 
(PUT 'TRIM 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIM) 'OPFN) 
(PUT 'TRIM 'DEFINED-ON-LINE '1935) 
(PUT 'TRIM 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TRIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIM (U)
    (COND ((EVALEQUAL (AEVAL U) (AEVAL (LIST 'LIST))) (AEVAL (LIST 'LIST)))
          ((MEMBER (REVALX (LIST 'FIRST U)) (REVALX (LIST 'REST U)))
           (AEVAL (LIST 'TRIM (LIST 'REST U))))
          (T (AEVAL (LIST 'CONS (LIST 'FIRST U) (LIST 'TRIM (LIST 'REST U))))))) 
(PUT 'MAXPOSINT 'NUMBER-OF-ARGS 1) 
(FLAG '(MAXPOSINT) 'OPFN) 
(PUT 'MAXPOSINT 'DEFINED-ON-LINE '1942) 
(PUT 'MAXPOSINT 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'MAXPOSINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAXPOSINT (LIST1)
    (PROG (PARTJ L J TMP)
      (SETQ TMP (AEVAL (MINUS 1)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ PARTJ (AEVAL* (LIST 'PART LIST1 J)))
         (COND
          ((AND (FIXP (REVALX PARTJ)) (EVALGEQ (AEVAL* PARTJ) 0))
           (SETQ TMP (AEVAL* (LIST 'MAX TMP PARTJ)))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL TMP)))) 
(PUT 'MINNEGINT 'NUMBER-OF-ARGS 1) 
(FLAG '(MINNEGINT) 'OPFN) 
(PUT 'MINNEGINT 'DEFINED-ON-LINE '1957) 
(PUT 'MINNEGINT 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'MINNEGINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINNEGINT (LIST1)
    (PROG (PARTJ L J TMP)
      (SETQ TMP (AEVAL 1))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ PARTJ (AEVAL* (LIST 'PART LIST1 J)))
         (COND
          ((AND (FIXP (REVALX PARTJ)) (EVALLEQ (AEVAL* PARTJ) 0))
           (SETQ TMP (AEVAL* (LIST 'MIN TMP PARTJ)))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL TMP)))) 
(PUT 'BINOM 'NUMBER-OF-ARGS 2) 
(FLAG '(BINOM) 'OPFN) 
(PUT 'BINOM 'DEFINED-ON-LINE '1975) 
(PUT 'BINOM 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'BINOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BINOM (N K)
    (PROG (I)
      (COND
       ((FIXP (REVALX N))
        (COND
         ((EVALGREATERP (AEVAL N) 0)
          (RETURN
           (AEVAL
            (LIST 'QUOTIENT (LIST 'FACTORIAL N)
                  (LIST 'TIMES (LIST 'FACTORIAL K)
                        (LIST 'FACTORIAL (LIST 'DIFFERENCE N K)))))))
         ((EVALLESSP (AEVAL N) 0)
          (AEVAL (REDERR (REVALX "negative integer argument"))))
         (T (RETURN (AEVAL (LIST 'DELTA 0 K))))))
       ((FIXP (REVALX K))
        (RETURN
         (AEVAL
          (LIST 'QUOTIENT
                (PROG (I FORALL-RESULT)
                  (SETQ I 0)
                  (SETQ FORALL-RESULT 1)
                 LAB1
                  (COND
                   ((|AMINUSP:|
                     (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE K 1)) I))
                    (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (AEVAL*
                           (LIST 'TIMES (AEVAL* (LIST 'DIFFERENCE N I))
                                 FORALL-RESULT)))
                  (SETQ I
                          ((LAMBDA (FORALL-RESULT)
                             (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                           I))
                  (GO LAB1))
                (LIST 'FACTORIAL K)))))
       (T (RETURN (AEVAL (LIST 'BINOMIAL N K))))))) 
(PUT 'NUMBEROFZEROS 'NUMBER-OF-ARGS 1) 
(FLAG '(NUMBEROFZEROS) 'OPFN) 
(PUT 'NUMBEROFZEROS 'DEFINED-ON-LINE '1994) 
(PUT 'NUMBEROFZEROS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'NUMBEROFZEROS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMBEROFZEROS (LIST1)
    (PROG (C L J)
      (SETQ C (AEVAL 0))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (COND
         ((EVALEQUAL (AEVAL* (LIST 'PART LIST1 J)) 0)
          (SETQ C (AEVAL* (LIST 'PLUS C 1)))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL C)))) 
(PUT 'PATTERNARGUMENTS 'NUMBER-OF-ARGS 3) 
(FLAG '(PATTERNARGUMENTS) 'OPFN) 
(PUT 'PATTERNARGUMENTS 'DEFINED-ON-LINE '2004) 
(PUT 'PATTERNARGUMENTS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'PATTERNARGUMENTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PATTERNARGUMENTS (TERM1 PATTERN LIST1)
    (PROG (J L)
      (COND ((FREEOF (REVALX TERM1) (REVALX PATTERN)) (RETURN (AEVAL LIST1))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH TERM1)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART TERM1 0)) (AEVAL PATTERN))
        (RETURN
         (AEVAL (LIST 'APPEND LIST1 (LIST 'LIST (LIST 'PART TERM1 1))))))
       (T
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
          (SETQ LIST1
                  (AEVAL*
                   (LIST 'PATTERNARGUMENTS (LIST 'PART TERM1 J) PATTERN
                         LIST1)))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))))
      (RETURN (AEVAL LIST1)))) 
(PUT 'REMOVE_PART 'NUMBER-OF-ARGS 2) 
(FLAG '(REMOVE_PART) 'OPFN) 
(PUT 'REMOVE_PART 'DEFINED-ON-LINE '2017) 
(PUT 'REMOVE_PART 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'REMOVE_PART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_PART (LIST1 J)
    (PROG (JJ L LIST2)
      (SETQ LIST2 (AEVAL (LIST 'LIST)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (PROG (JJ)
        (SETQ JJ 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE J 1)) JJ))
          (RETURN NIL)))
        (SETQ LIST2 (AEVAL* (LIST 'APPEND (LIST 'PART LIST1 JJ) LIST2)))
        (SETQ JJ
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 JJ))
        (GO LAB))
      (PROG (JJ)
        (SETQ JJ (AEVAL* (LIST 'PLUS J 1)))
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) JJ)) (RETURN NIL)))
        (SETQ LIST2 (AEVAL* (LIST 'APPEND (LIST 'PART LIST1 JJ) LIST2)))
        (SETQ JJ
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 JJ))
        (GO LAB))
      (RETURN (AEVAL LIST2)))) 
(PUT 'REMOVE_NONLINEAR_PARTS 'NUMBER-OF-ARGS 2) 
(FLAG '(REMOVE_NONLINEAR_PARTS) 'OPFN) 
(PUT 'REMOVE_NONLINEAR_PARTS 'DEFINED-ON-LINE '2029) 
(PUT 'REMOVE_NONLINEAR_PARTS 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'REMOVE_NONLINEAR_PARTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_NONLINEAR_PARTS (LIST1 K)
    (PROG (J LIST2 *EXP)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ LIST2 (AEVAL LIST1))
      (WHILE (EVALNEQ (AEVAL* LIST1) (AEVAL* (LIST 'LIST)))
             (PROGN
              (COND
               ((EVALGREATERP (AEVAL* (LIST 'DEG (LIST 'FIRST LIST1) K)) 1)
                (AEVAL* (REDERR (REVALX "nonlinear argument in gamma"))))
               ((EVALEQUAL (AEVAL* (LIST 'DEG (LIST 'FIRST LIST1) K)) 0)
                (SETQ LIST2 (AEVAL* (LIST 'REST LIST2)))))
              (SETQ LIST1 (AEVAL* (LIST 'REST LIST1)))))
      (RETURN (AEVAL LIST2)))) 
(PUT 'CLOSEDFORM_INITIALIZATION 'NUMBER-OF-ARGS 3) 
(FLAG '(CLOSEDFORM_INITIALIZATION) 'OPFN) 
(PUT 'CLOSEDFORM_INITIALIZATION 'DEFINED-ON-LINE '2048) 
(PUT 'CLOSEDFORM_INITIALIZATION 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'CLOSEDFORM_INITIALIZATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CLOSEDFORM_INITIALIZATION (F K N)
    (PROG (CO J L GA MINI MAXI *EXP BA B A TMPMAX TMPMIN)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ F (AEVAL (LIST 'DEN (LIST 'SIMPLIFY_COMBINATORIAL F))))
      (SETQ MINI (AEVAL 'NIL))
      (SETQ MAXI (AEVAL 'NIL))
      (SETQ GA (AEVAL (LIST 'PATTERNARGUMENTS F 'GAMMA (LIST 'LIST))))
      (SETQ GA (AEVAL (LIST 'REMOVE_NONLINEAR_PARTS GA K)))
      (SETQ L (AEVAL (LIST 'ARGLENGTH GA)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ CO (AEVAL* (LIST 'COEFF (LIST 'PART GA J) K)))
         (SETQ A (AEVAL* (LIST 'PART CO 2)))
         (SETQ B (AEVAL* (LIST 'PART CO 1)))
         (SETQ BA (AEVAL* (LIST 'MINUS (LIST 'QUOTIENT B A))))
         (COND
          ((AND (EVALNUMBERP (AEVAL* A)) (FIXP (REVALX BA)))
           (COND
            ((EVALGREATERP (AEVAL* A) 0)
             (COND
              ((EVALEQUAL (AEVAL* MAXI) (AEVAL* 'NIL)) (SETQ MAXI (AEVAL* BA)))
              (T (SETQ MAXI (AEVAL* (LIST 'MAX MAXI BA))))))
            ((EVALEQUAL (AEVAL* MINI) (AEVAL* 'NIL)) (SETQ MINI (AEVAL* BA)))
            (T (SETQ MINI (AEVAL* (LIST 'MIN MINI BA))))))
          ((NOT (FREEOF (REVALX BA) (REVALX N)))
           (PROGN
            (COND ((EVALGREATERP (AEVAL* A) 0) (SETQ TMPMAX (AEVAL* BA)))
                  (T (SETQ TMPMIN (AEVAL* BA))))
            (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (COND ((EVALEQUAL (AEVAL MAXI) (AEVAL 'NIL)) (SETQ MAXI (AEVAL TMPMAX))))
      (COND ((EVALEQUAL (AEVAL MINI) (AEVAL 'NIL)) (SETQ MINI (AEVAL TMPMIN))))
      (RETURN (AEVAL (LIST 'LIST MAXI MINI))))) 
(PUT 'SIMPLIFY_GAMMA2 'NUMBER-OF-ARGS 1) 
(FLAG '(SIMPLIFY_GAMMA2) 'OPFN) 
(PUT 'SIMPLIFY_GAMMA2 'DEFINED-ON-LINE '2094) 
(PUT 'SIMPLIFY_GAMMA2 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SIMPLIFY_GAMMA2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY_GAMMA2 (TERM1)
    (PROG (P L J JJ JJJ W1 W2 LIST1 CHANGED)
      (SETQ LIST1 (AEVAL (LIST 'PATTERNARGUMENTS TERM1 'GAMMA (LIST 'LIST))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (SETQ CHANGED (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ CHANGED (AEVAL* 'NIL))
         (SETQ W1 (AEVAL* (LIST 'PART LIST1 J)))
         (SETQ JJ (AEVAL* 0))
         (WHILE
          (AND (NOT (BOOLVALUE* CHANGED)) (EVALLESSP (AEVAL* JJ) (AEVAL* L)))
          (PROGN
           (SETQ JJ (AEVAL* (LIST 'PLUS JJ 1)))
           (SETQ W2 (AEVAL* (LIST 'PART LIST1 JJ)))
           (SETQ P (AEVAL* (LIST 'DIFFERENCE W1 (LIST 'TIMES 2 W2))))
           (COND
            ((FIXP (REVALX P))
             (PROGN
              (COND
               ((EVALEQUAL (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'TIMES 2 'PI)
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'EXPT 2
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 W2)
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'GAMMA W2)
                                           (LIST 'GAMMA
                                                 (LIST 'PLUS W2
                                                       (LIST 'QUOTIENT 1 2)))))
                               TERM1))))
               ((EVALGREATERP (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (PROG (JJJ FORALL-RESULT)
                                             (SETQ JJJ 1)
                                             (SETQ FORALL-RESULT 1)
                                            LAB1
                                             (COND
                                              ((|AMINUSP:|
                                                (LIST 'DIFFERENCE (AEVAL* P)
                                                      JJJ))
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (AEVAL*
                                                      (LIST 'TIMES
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   W1 JJJ))
                                                            FORALL-RESULT)))
                                             (SETQ JJJ
                                                     ((LAMBDA (FORALL-RESULT)
                                                        (AEVAL*
                                                         (LIST 'PLUS
                                                               FORALL-RESULT
                                                               1)))
                                                      JJJ))
                                             (GO LAB1))
                                           (LIST 'EXPT (LIST 'TIMES 2 'PI)
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'EXPT 2
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 W2)
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'GAMMA W2)
                                           (LIST 'GAMMA
                                                 (LIST 'PLUS W2
                                                       (LIST 'QUOTIENT 1 2)))))
                               TERM1))))
               (T
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (LIST 'QUOTIENT 1
                                                 (PROG (JJJ FORALL-RESULT)
                                                   (SETQ JJJ 0)
                                                   (SETQ FORALL-RESULT 1)
                                                  LAB1
                                                   (COND
                                                    ((|AMINUSP:|
                                                      (LIST 'DIFFERENCE
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'MINUS
                                                                         P)
                                                                   1))
                                                            JJJ))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'TIMES
                                                                  (AEVAL*
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    W1 JJJ))
                                                                  FORALL-RESULT)))
                                                   (SETQ JJJ
                                                           ((LAMBDA
                                                                (FORALL-RESULT)
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     FORALL-RESULT
                                                                     1)))
                                                            JJJ))
                                                   (GO LAB1)))
                                           (LIST 'EXPT (LIST 'TIMES 2 'PI)
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'EXPT 2
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 2 W2)
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'GAMMA W2)
                                           (LIST 'GAMMA
                                                 (LIST 'PLUS W2
                                                       (LIST 'QUOTIENT 1 2)))))
                               TERM1)))))
              (SETQ CHANGED (AEVAL* 1))))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL TERM1))))) 
(PUT 'SIMPLIFY_GAMMAN 'NUMBER-OF-ARGS 2) 
(FLAG '(SIMPLIFY_GAMMAN) 'OPFN) 
(PUT 'SIMPLIFY_GAMMAN 'DEFINED-ON-LINE '2143) 
(PUT 'SIMPLIFY_GAMMAN 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SIMPLIFY_GAMMAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFY_GAMMAN (TERM1 N)
    (PROG (SUBST P L J JJ JJJ JJJJ W1 W2 LIST1 CHANGED)
      (SETQ LIST1 (AEVAL (LIST 'PATTERNARGUMENTS TERM1 'GAMMA (LIST 'LIST))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (SETQ CHANGED (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ CHANGED (AEVAL* 'NIL))
         (SETQ W1 (AEVAL* (LIST 'PART LIST1 J)))
         (SETQ JJ (AEVAL* 0))
         (WHILE
          (AND (NOT (BOOLVALUE* CHANGED)) (EVALLESSP (AEVAL* JJ) (AEVAL* L)))
          (PROGN
           (SETQ JJ (AEVAL* (LIST 'PLUS JJ 1)))
           (SETQ W2 (AEVAL* (LIST 'PART LIST1 JJ)))
           (SETQ P (AEVAL* (LIST 'DIFFERENCE W1 (LIST 'TIMES N W2))))
           (COND
            ((FIXP (REVALX P))
             (PROGN
              (SETQ SUBST
                      (AEVAL*
                       (LIST 'TIMES
                             (LIST 'EXPT (LIST 'TIMES 2 'PI)
                                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                         (LIST 'DIFFERENCE 1 N)))
                             (LIST 'EXPT N
                                   (LIST 'DIFFERENCE (LIST 'TIMES N W2)
                                         (LIST 'QUOTIENT 1 2)))
                             (PROG (JJJJ FORALL-RESULT)
                               (SETQ JJJJ 0)
                               (SETQ FORALL-RESULT 1)
                              LAB1
                               (COND
                                ((|AMINUSP:|
                                  (LIST 'DIFFERENCE
                                        (AEVAL* (LIST 'DIFFERENCE N 1)) JJJJ))
                                 (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'TIMES
                                              (AEVAL*
                                               (LIST 'GAMMA
                                                     (LIST 'PLUS W2
                                                           (LIST 'QUOTIENT JJJJ
                                                                 N))))
                                              FORALL-RESULT)))
                               (SETQ JJJJ
                                       ((LAMBDA (FORALL-RESULT)
                                          (AEVAL*
                                           (LIST 'PLUS FORALL-RESULT 1)))
                                        JJJJ))
                               (GO LAB1)))))
              (COND
               ((EVALEQUAL (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB (LIST 'EQUAL (LIST 'GAMMA W1) SUBST)
                               TERM1))))
               ((EVALGREATERP (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (PROG (JJJ FORALL-RESULT)
                                             (SETQ JJJ 1)
                                             (SETQ FORALL-RESULT 1)
                                            LAB1
                                             (COND
                                              ((|AMINUSP:|
                                                (LIST 'DIFFERENCE (AEVAL* P)
                                                      JJJ))
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (AEVAL*
                                                      (LIST 'TIMES
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   W1 JJJ))
                                                            FORALL-RESULT)))
                                             (SETQ JJJ
                                                     ((LAMBDA (FORALL-RESULT)
                                                        (AEVAL*
                                                         (LIST 'PLUS
                                                               FORALL-RESULT
                                                               1)))
                                                      JJJ))
                                             (GO LAB1))
                                           SUBST))
                               TERM1))))
               (T
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (LIST 'QUOTIENT 1
                                                 (PROG (JJJ FORALL-RESULT)
                                                   (SETQ JJJ 0)
                                                   (SETQ FORALL-RESULT 1)
                                                  LAB1
                                                   (COND
                                                    ((|AMINUSP:|
                                                      (LIST 'DIFFERENCE
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'MINUS
                                                                         P)
                                                                   1))
                                                            JJJ))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'TIMES
                                                                  (AEVAL*
                                                                   (LIST 'PLUS
                                                                         W1
                                                                         JJJ))
                                                                  FORALL-RESULT)))
                                                   (SETQ JJJ
                                                           ((LAMBDA
                                                                (FORALL-RESULT)
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     FORALL-RESULT
                                                                     1)))
                                                            JJJ))
                                                   (GO LAB1)))
                                           SUBST))
                               TERM1)))))
              (SETQ CHANGED (AEVAL* 1))))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL TERM1))))) 
(OPERATOR (LIST 'ZB_SUBST)) 
(PUT 'SIMPLIFY_GAMMA3 'NUMBER-OF-ARGS 1) 
(FLAG '(SIMPLIFY_GAMMA3) 'OPFN) 
(PUT 'SIMPLIFY_GAMMA3 'DEFINED-ON-LINE '2183) 
(PUT 'SIMPLIFY_GAMMA3 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SIMPLIFY_GAMMA3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY_GAMMA3 (TERM1)
    (PROG (SUBST P L J JJ JJJ W1 W2 LIST1 CHANGED)
      (SETQ LIST1 (AEVAL (LIST 'PATTERNARGUMENTS TERM1 'GAMMA (LIST 'LIST))))
      (SETQ L (AEVAL (LIST 'ARGLENGTH LIST1)))
      (SETQ CHANGED (AEVAL (LIST 'LIST)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* L) J)) (RETURN NIL)))
        (PROGN
         (SETQ CHANGED (AEVAL* 'NIL))
         (SETQ W1 (AEVAL* (LIST 'PART LIST1 J)))
         (SETQ JJ (AEVAL* 0))
         (WHILE
          (AND (NOT (BOOLVALUE* CHANGED)) (EVALLESSP (AEVAL* JJ) (AEVAL* L)))
          (PROGN
           (SETQ JJ (AEVAL* (LIST 'PLUS JJ 1)))
           (SETQ W2 (AEVAL* (LIST 'PART LIST1 JJ)))
           (SETQ P (AEVAL* (LIST 'DIFFERENCE W1 (LIST 'TIMES 3 W2))))
           (COND
            ((FIXP (REVALX P))
             (PROGN
              (SETQ SUBST
                      (AEVAL*
                       (LIST 'TIMES (LIST 'EXPT (LIST 'TIMES 2 'PI) (MINUS 1))
                             (LIST 'EXPT 3
                                   (LIST 'DIFFERENCE (LIST 'TIMES 3 W2)
                                         (LIST 'QUOTIENT 1 2)))
                             (LIST 'GAMMA W2)
                             (LIST 'GAMMA (LIST 'PLUS W2 (LIST 'QUOTIENT 1 3)))
                             (LIST 'GAMMA
                                   (LIST 'PLUS W2 (LIST 'QUOTIENT 2 3))))))
              (COND
               ((EVALEQUAL (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'ZB_SUBST J))
                               TERM1))))
               ((EVALGREATERP (AEVAL* P) 0)
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (PROG (JJJ FORALL-RESULT)
                                             (SETQ JJJ 1)
                                             (SETQ FORALL-RESULT 1)
                                            LAB1
                                             (COND
                                              ((|AMINUSP:|
                                                (LIST 'DIFFERENCE (AEVAL* P)
                                                      JJJ))
                                               (RETURN FORALL-RESULT)))
                                             (SETQ FORALL-RESULT
                                                     (AEVAL*
                                                      (LIST 'TIMES
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   W1 JJJ))
                                                            FORALL-RESULT)))
                                             (SETQ JJJ
                                                     ((LAMBDA (FORALL-RESULT)
                                                        (AEVAL*
                                                         (LIST 'PLUS
                                                               FORALL-RESULT
                                                               1)))
                                                      JJJ))
                                             (GO LAB1))
                                           (LIST 'ZB_SUBST J)))
                               TERM1))))
               (T
                (SETQ TERM1
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL (LIST 'GAMMA W1)
                                     (LIST 'TIMES
                                           (LIST 'QUOTIENT 1
                                                 (PROG (JJJ FORALL-RESULT)
                                                   (SETQ JJJ 0)
                                                   (SETQ FORALL-RESULT 1)
                                                  LAB1
                                                   (COND
                                                    ((|AMINUSP:|
                                                      (LIST 'DIFFERENCE
                                                            (AEVAL*
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'MINUS
                                                                         P)
                                                                   1))
                                                            JJJ))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'TIMES
                                                                  (AEVAL*
                                                                   (LIST 'PLUS
                                                                         W1
                                                                         JJJ))
                                                                  FORALL-RESULT)))
                                                   (SETQ JJJ
                                                           ((LAMBDA
                                                                (FORALL-RESULT)
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     FORALL-RESULT
                                                                     1)))
                                                            JJJ))
                                                   (GO LAB1)))
                                           (LIST 'ZB_SUBST J)))
                               TERM1)))))
              (SETQ TERM1
                      (AEVAL*
                       (LIST 'SUB (LIST 'EQUAL (LIST 'ZB_SUBST J) SUBST)
                             TERM1)))
              (SETQ CHANGED (AEVAL* 1))))))))
        (SETQ J
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 J))
        (GO LAB))
      (RETURN (AEVAL (LIST 'SIMPLIFY_COMBINATORIAL TERM1))))) 
(PUT 'NEWREDERR 'NUMBER-OF-ARGS 1) 
(PUT 'NEWREDERR 'DEFINED-ON-LINE '2224) 
(PUT 'NEWREDERR 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'NEWREDERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEWREDERR (U)
    (PROGN
     (TERPRI* T)
     (PRIN2* "***** ")
     (COND
      ((EQCAR U 'LIST)
       (PROG (XX)
         (SETQ XX (CDR U))
        LAB
         (COND ((NULL XX) (RETURN NIL)))
         ((LAMBDA (XX) (NEWREDERR1 XX)) (CAR XX))
         (SETQ XX (CDR XX))
         (GO LAB)))
      (T (NEWREDERR1 U)))
     (TERPRI* NIL)
     (SETQ ERFG* T)
     (ERROR1))) 
(PUT 'NEWREDERR1 'NUMBER-OF-ARGS 1) 
(PUT 'NEWREDERR1 'DEFINED-ON-LINE '2231) 
(PUT 'NEWREDERR1 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'NEWREDERR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEWREDERR1 (U)
    (COND
     ((AND (NOT (ATOM U)) (ATOM (CAR U)) (CDR U) (ATOM (CADR U))
           (NULL (CDDR U)))
      (PROGN (PRIN2* (CAR U)) (PRIN2* " ") (PRIN2* (CADR U))))
     (T (MAPRIN U)))) 
(FLAG '(NEWREDERR) 'OPFN) 
(PUT 'POLYNOMQQ 'PSOPFN 'POLYNOMQQQ) 
(PUT 'POLYNOMQ4 'NUMBER-OF-ARGS 2) 
(FLAG '(POLYNOMQ4) 'OPFN) 
(PUT 'POLYNOMQ4 'DEFINED-ON-LINE '2246) 
(PUT 'POLYNOMQ4 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'POLYNOMQ4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMQ4 (EXPR1 K)
    (PROG (*EXP)
      (AEVAL (ON (LIST 'EXP)))
      (RETURN (AEVAL (LIST 'POLYNOMQQ EXPR1 K))))) 
(PUT 'TYPE_RATPOLY 'NUMBER-OF-ARGS 2) 
(FLAG '(TYPE_RATPOLY) 'OPFN) 
(PUT 'TYPE_RATPOLY 'DEFINED-ON-LINE '2254) 
(PUT 'TYPE_RATPOLY 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPE_RATPOLY (EXPR1 VAR)
    (PROG (DENO NUME)
      (SETQ DENO (AEVAL (LIST 'DEN EXPR1)))
      (SETQ NUME (AEVAL (LIST 'NUM EXPR1)))
      (COND
       ((AND (BOOLVALUE* (REVALX (LIST 'POLYNOMQQ DENO VAR)))
             (BOOLVALUE* (REVALX (LIST 'POLYNOMQQ NUME VAR))))
        (RETURN (AEVAL 'T)))
       (T (RETURN (AEVAL 'NIL)))))) 
(FLAG '(TYPE_RATPOLY) 'BOOLEAN) 
(PUT 'TTTYPE_RATPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'TTTYPE_RATPOLY 'DEFINED-ON-LINE '2264) 
(PUT 'TTTYPE_RATPOLY 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TTTYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TTTYPE_RATPOLY (U XX)
    ((LAMBDA (XX)
       (COND ((FIXP XX) T) ((NOT (EQCAR XX '*SQ)) NIL)
             (T
              (AND
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CAR (CADR XX)) 1)) (REVAL1 (CADR U) T)))
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CDR (CADR XX)) 1))
                      (REVAL1 (CADR U) T)))))))
     (REVAL1 (CAR U) NIL))) 
(FLAG '(TTTYPE_RATPOLY) 'BOOLEAN) 
(FLAG '(SAVESOLVE) 'OPFN) 
(PUT 'SAVESOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'SAVESOLVE 'DEFINED-ON-LINE '2281) 
(PUT 'SAVESOLVE 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'SAVESOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAVESOLVE (X Y)
    (PROGN
     (SWITCH (LIST 'SOLVEINCONSISTENT))
     (ON (LIST 'SOLVEINCONSISTENT))
     (SETQ INCONSISTENT* NIL)
     (COND
      ((AND
        (PAIRP (SETQ X (ERRORSET* (LIST 'SOLVEEVAL (MKQUOTE (LIST X Y))) NIL)))
        (NOT INCONSISTENT*) (NOT (EQUAL X '((LIST)))))
       (PROGN
        (SETQ X (CAR X))
        (COND ((EQCAR (CADR X) 'EQUAL) (LIST 'LIST X)) (T X))))
      (T (PROGN (SETQ ERFG* NIL) (LIST 'LIST)))))) 
(PUT 'POLYNOMQ 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMQ 'DEFINED-ON-LINE '2294) 
(PUT 'POLYNOMQ 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'POLYNOMQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMQ (X VAR)
    (COND ((NOT (FIXP (CDR (SIMP X)))) NIL)
          (T
           (PROG (KERNS KERN AA)
             (SETQ KERNS (KERNELS (*Q2F (SIMP X))))
            AA
             (COND ((NULL KERNS) (RETURN T)))
             (SETQ KERN (FIRST KERNS))
             (SETQ KERNS (CDR KERNS))
             (COND ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                   (T (GO AA))))))) 
(FLAG '(POLYNOMQ) 'OPFN) 
(FLAG '(POLYNOMQ TYPE_RATPOLY) 'BOOLEAN) 
(PUT 'POLYNOMQQQ 'NUMBER-OF-ARGS 1) 
(PUT 'POLYNOMQQQ 'DEFINED-ON-LINE '2314) 
(PUT 'POLYNOMQQQ 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'POLYNOMQQQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLYNOMQQQ (X)
    ((LAMBDA (XX)
       (COND ((NOT (FIXP (CDR (SETQ XX (CADR (REVAL1 (CAR X) NIL)))))) NIL)
             (T
              (PROG (KERNS KERN AA VAR)
                (SETQ VAR (REVAL1 (CADR X) T))
                (SETQ KERNS (KERNELS (*Q2F XX)))
               AA
                (COND ((NULL KERNS) (RETURN T)))
                (SETQ KERN (FIRST KERNS))
                (SETQ KERNS (CDR KERNS))
                (COND
                 ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                 (T (GO AA)))))))
     X)) 
(PUT 'POLYNOMQQ 'PSOPFN 'POLYNOMQQQ) 
(PUT 'POLYNOMQQQ 'NUMBER-OF-ARGS 1) 
(PUT 'POLYNOMQQQ 'DEFINED-ON-LINE '2333) 
(PUT 'POLYNOMQQQ 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'POLYNOMQQQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLYNOMQQQ (X)
    ((LAMBDA (XX)
       (COND ((FIXP XX) T) ((NOT (ONEP (CDR (SETQ XX (CADR XX))))) NIL)
             (T
              (PROG (KERNS KERN AA VAR FFORM MVV DEGG)
                (SETQ FFORM (SFP (CAAAR (CAR XX))))
                (SETQ VAR (REVAL1 (CADR X) T))
                (COND
                 (FFORM
                  (PROGN
                   (SETQ XX (CAR XX))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (NEQ XX 1)) (RETURN NIL)))
                     (PROGN
                      (SETQ MVV (CAAAR XX))
                      (SETQ DEGG (CDAAR XX))
                      (SETQ XX (CDAR XX))
                      (COND
                       ((OR (ATOM MVV) (ATOM (CAR MVV)))
                        (PROGN
                         (COND
                          ((NOT (FREEOF MVV VAR))
                           (PROGN
                            (SETQ XX 1)
                            (SETQ KERNS (LIST (LIST 'SIN VAR))))))))
                       (T
                        (SETQ KERNS
                                (APPEND (APPEND (KERNELS MVV) (KERNELS DEGG))
                                        KERNS)))))
                     (GO WHILELABEL))))
                 (T (SETQ KERNS (KERNELS (*Q2F XX)))))
               AA
                (COND ((NULL KERNS) (RETURN T)))
                (SETQ KERN (FIRST KERNS))
                (SETQ KERNS (CDR KERNS))
                (COND
                 ((AND (NOT (EQ KERN VAR)) (DEPENDS KERN VAR)) (RETURN NIL))
                 (T (GO AA)))))))
     (REVAL1 (CAR X) NIL))) 
(PUT 'POLYNOMQQ 'PSOPFN 'POLYNOMQQQ) 
(PUT 'TTTTYPE_RATPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-ON-LINE '2360) 
(PUT 'TTTTYPE_RATPOLY 'DEFINED-IN-FILE 'SUM/ZEILBERG.RED) 
(PUT 'TTTTYPE_RATPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TTTTYPE_RATPOLY (U)
    ((LAMBDA (XX)
       (COND ((FIXP XX) T) ((NOT (EQCAR XX '*SQ)) NIL)
             (T
              (AND
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CAR (CADR XX)) 1)) (REVAL1 (CADR U) T)))
               (POLYNOMQQQ
                (LIST (MK*SQ (CONS (CDR (CADR XX)) 1))
                      (REVAL1 (CADR U) T)))))))
     (REVAL1 (CAR U) NIL))) 
(FLAG '(TYPE_RATPOLY) 'BOOLEAN) 
(PUT 'TYPE_RATPOLY 'PSOPFN 'TTTTYPE_RATPOLY) 
(ENDMODULE) 