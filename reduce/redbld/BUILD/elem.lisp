(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ELEM)) 
(FLUID
 '(**SQRT *COMPLEX *KEEPSQRTS *PRECISE *PRECISE_COMPLEX *ROUNDED DMODE*
   *ELEM-INHERIT)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(DEFLIST '((SQRT OUTER-SIMPSQRT)) 'SIMPFN) 
(PUT 'MKSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'MKSQRT 'DEFINED-ON-LINE '57) 
(PUT 'MKSQRT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'MKSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKSQRT (U)
    (COND ((NOT *KEEPSQRTS) (LIST 'EXPT U (LIST 'QUOTIENT 1 2)))
          (T
           (PROGN
            (COND
             ((NULL **SQRT)
              (PROGN
               (SETQ **SQRT T)
               (AEVAL
                (FORALL
                 (LIST '(X) 'T '(LET00 '((EQUAL (EXPT (SQRT X) 2) X)))))))))
            (LIST 'SQRT U))))) 
(FORALL
 (LIST '(X) 'T
       '(LET00 '((EQUAL (DF (SQRT X) X) (QUOTIENT (SQRT X) (TIMES 2 X))))))) 
(PUT 'SIGN-OF 'NUMBER-OF-ARGS 1) 
(PUT 'SIGN-OF 'DEFINED-ON-LINE '68) 
(PUT 'SIGN-OF 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIGN-OF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIGN-OF (U) ((LAMBDA (S) (AND (NUMBERP S) S)) (CAR (SIMP-SIGN (LIST U))))) 
(PUT 'SIMP-SIGN1 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN1 'DEFINED-ON-LINE '72) 
(PUT 'SIMP-SIGN1 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN1 (U)
    (PROG (S N)
      (SETQ S
              (COND
               ((ATOM U)
                (PROGN
                 (COND (*MODULAR (SIMPIDEN (LIST 'SIGN U)))
                       ((NUMBERP U)
                        (CONS
                         (COND ((GREATERP U 0) 1) ((LESSP U 0) (MINUS 1))
                               (T NIL))
                         1))
                       (T (SIMP-SIGN2 U)))))
               ((EQ (CAR U) '|:RD:|)
                (CONS
                 (COND ((|RD:ZEROP| U) NIL) ((|RD:MINUSP| U) (MINUS 1)) (T 1))
                 1))
               ((EQ (CAR U) 'MINUS) (NEGSQ (SIMP-SIGN1 (CADR U))))
               ((EQ (CAR U) 'TIMES) (SIMP-SIGN-TIMES U))
               ((EQ (CAR U) 'QUOTIENT) (SIMP-SIGN-QUOT U))
               ((EQ (CAR U) 'PLUS) (SIMP-SIGN-PLUS U))
               ((EQ (CAR U) 'EXPT) (SIMP-SIGN-EXPT U))
               ((EQ (CAR U) 'SQRT) (SIMP-SIGN-SQRT U)) (T (SIMP-SIGN2 U))))
      (SETQ N (CAR S))
      (COND
       ((OR (NOT (NUMBERP N)) (EQUAL N 1) (EQUAL N (MINUS 1)) (EQUAL N 0))
        (RETURN S)))
      (TYPERR N "sign value"))) 
(PUT 'SIMP-SIGN2 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN2 'DEFINED-ON-LINE '95) 
(PUT 'SIMP-SIGN2 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN2 (U)
    ((LAMBDA (X) (COND (X (CONS X 1)) (T (SIMPIDEN (LIST 'SIGN U)))))
     (RD-SIGN U))) 
(PUT 'SIMP-SIGN 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN 'DEFINED-ON-LINE '99) 
(PUT 'SIMP-SIGN 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN (U) (SIMP-SIGN1 (REVAL1 (CAR U) T))) 
(DE SQ-IS-SIGN (U)
    ((LAMBDA (NU)
       (AND (EQUAL (CDR U) 1)
            (OR (EQUAL NU 1) (EQUAL NU (MINUS 1)) (EQUAL NU 0))))
     (CAR U))) 
(PUT 'SQ-IS-SIGN 'NUMBER-OF-ARGS 1) 
(PUT 'SQ-IS-SIGN 'DEFINED-ON-LINE '102) 
(PUT 'SQ-IS-SIGN 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SQ-IS-SIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SQ-IS-SIGN 'INLINE
      '(LAMBDA (U)
         ((LAMBDA (NU)
            (AND (EQUAL (CDR U) 1)
                 (OR (EQUAL NU 1) (EQUAL NU (MINUS 1)) (EQUAL NU 0))))
          (CAR U)))) 
(PUT 'SIMP-SIGN-TIMES 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN-TIMES 'DEFINED-ON-LINE '106) 
(PUT 'SIMP-SIGN-TIMES 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN-TIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN-TIMES (W)
    (PROG (N S X)
      (SETQ N 1)
      (PROG (F)
        (SETQ F (CDR W))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ X (SIMP-SIGN1 F))
            (COND
             (((LAMBDA (NU)
                 (AND (EQUAL (CDR X) 1)
                      (OR (EQUAL NU 1) (EQUAL NU (MINUS 1)) (EQUAL NU 0))))
               (CAR X))
              (SETQ N (TIMES N (CAR X))))
             (T (SETQ S (CONS F S))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ S
              (COND ((NULL S) '(1 . 1))
                    (T
                     (SIMP-SIGN2
                      (COND ((CDR S) (CONS 'TIMES (REVERSIP S)))
                            (T (CAR S)))))))
      (RETURN (MULTSQ (CONS N 1) S)))) 
(PUT 'SIMP-SIGN-QUOT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN-QUOT 'DEFINED-ON-LINE '119) 
(PUT 'SIMP-SIGN-QUOT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN-QUOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN-QUOT (W)
    (PROG (X Y FLG Z)
      (COND
       ((EQCAR (CADR W) 'MINUS)
        (PROGN
         (SETQ FLG T)
         (SETQ W (LIST (CAR W) (CADR (CADR W)) (CADDR W))))))
      (SETQ X (SIMP-SIGN1 (CADR W)))
      (SETQ Y (SIMP-SIGN1 (CADDR W)))
      (COND
       ((OR
         ((LAMBDA (NU)
            (AND (EQUAL (CDR X) 1)
                 (OR (EQUAL NU 1) (EQUAL NU (MINUS 1)) (EQUAL NU 0))))
          (CAR X))
         ((LAMBDA (NU)
            (AND (EQUAL (CDR Y) 1)
                 (OR (EQUAL NU 1) (EQUAL NU (MINUS 1)) (EQUAL NU 0))))
          (CAR Y)))
        (SETQ Z (MULTSQ X (INVSQ Y))))
       (T (SETQ Z (SIMP-SIGN2 W))))
      (RETURN (COND (FLG (NEGSQ Z)) (T Z))))) 
(PUT 'SIMP-SIGN-PLUS 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN-PLUS 'DEFINED-ON-LINE '130) 
(PUT 'SIMP-SIGN-PLUS 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN-PLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN-PLUS (W)
    (PROG (N M X Q)
      (PROG (F)
        (SETQ F (CDR W))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((NULL Q)
             (PROGN
              (SETQ X (SIMP-SIGN1 F))
              (SETQ M
                      (COND
                       (((LAMBDA (NU)
                           (AND (EQUAL (CDR X) 1)
                                (OR (EQUAL NU 1) (EQUAL NU (MINUS 1))
                                    (EQUAL NU 0))))
                         (CAR X))
                        (CAR X))))
              (COND ((OR (NULL M) (AND N (NEQ M N))) (SETQ Q T)))
              (SETQ N M)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN (COND ((NULL Q) (CONS N 1)) (T (SIMP-SIGN2 W)))))) 
(PUT 'SIMP-SIGN-EXPT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN-EXPT 'DEFINED-ON-LINE '142) 
(PUT 'SIMP-SIGN-EXPT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN-EXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN-EXPT (W)
    ((LAMBDA (EX)
       (COND
        ((AND (FIXP EX) (EVENP EX) (NOT (OR *COMPLEX *PRECISE_COMPLEX)))
         (CONS 1 1))
        (T
         ((LAMBDA (SB)
            (COND
             ((AND (FIXP EX)
                   ((LAMBDA (NU)
                      (AND (EQUAL (CDR SB) 1)
                           (OR (EQUAL NU 1) (EQUAL NU (MINUS 1))
                               (EQUAL NU 0))))
                    (CAR SB)))
              (CONS
               (COND ((AND (NOT (EVENP EX)) (LESSP (CAR SB) 0)) (MINUS 1))
                     (T 1))
               1))
             ((AND (FIXP EX) (NOT (OR *COMPLEX *PRECISE_COMPLEX))) SB)
             ((AND (EQUAL EX '(QUOTIENT 1 2)) (EQUAL SB (CONS 1 1)))
              (CONS 1 1))
             ((AND (EQUAL SB (CONS 1 1)) (REALVALUEDP EX)) (CONS 1 1))
             (T (SIMP-SIGN2 W))))
          (SIMP-SIGN1 (CADR W))))))
     (CADDR W))) 
(PUT 'SIMP-SIGN-SQRT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-SIGN-SQRT 'DEFINED-ON-LINE '154) 
(PUT 'SIMP-SIGN-SQRT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-SIGN-SQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-SIGN-SQRT (W)
    ((LAMBDA (U) (COND ((EQUAL U (CONS 1 1)) U) (T (SIMP-SIGN2 W))))
     (SIMP-SIGN-EXPT (LIST 'EXPT (CADR W) '(QUOTIENT 1 2))))) 
(FLUID '(RD-SIGN*)) 
(PUT 'RD-SIGN 'NUMBER-OF-ARGS 1) 
(PUT 'RD-SIGN 'DEFINED-ON-LINE '160) 
(PUT 'RD-SIGN 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'RD-SIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RD-SIGN (U)
    (COND ((AND (PAIRP RD-SIGN*) (EQUAL U (CAR RD-SIGN*))) (CDR RD-SIGN*))
          ((OR *COMPLEX *ROUNDED (NOT (CONSTANT_EXPRP U))) NIL)
          (T
           ((LAMBDA (ALGLIST*)
              (PROG (X Y DMODE*)
                (SETDMODE 'ROUNDED T)
                (SETQ X (REVAL1 U NIL))
                (COND
                 ((AND (EVALNUMBERP X) (EQUAL 0 (REVAL1 (LIST 'IMPART X) T)))
                  (SETQ Y
                          (COND ((EVALGREATERP X 0) 1) ((EVALEQUAL X 0) 0)
                                (T (MINUS 1))))))
                (SETDMODE 'ROUNDED NIL)
                (SETQ RD-SIGN* (CONS U Y))
                (RETURN Y)))
            ALGLIST*)))) 
(FLAG '(RD-SIGN) 'OPFN) 
(OPERATOR (LIST 'SIGN)) 
(PUT 'SIGN 'SIMPFN 'SIMP-SIGN) 
(SETK 'SIGN_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SIGN (LIST 'SINH (LIST '~ 'X)))
                   (LIST 'WHEN (LIST 'SIGN 'X)
                         (LIST 'OR (LIST 'NUMBERP (LIST 'SIGN 'X))
                               (LIST 'REALVALUEDP 'X))))
             (LIST 'REPLACEBY (LIST 'SIGN (LIST 'COSH (LIST '~ 'X)))
                   (LIST 'WHEN 1 (LIST 'REALVALUEDP 'X)))
             (LIST 'REPLACEBY (LIST 'SIGN (LIST 'TANH (LIST '~ 'X)))
                   (LIST 'WHEN (LIST 'SIGN 'X)
                         (LIST 'OR (LIST 'NUMBERP (LIST 'SIGN 'X))
                               (LIST 'REALVALUEDP 'X))))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'ABS (LIST '~ 'X)) 2)
                   (LIST 'WHEN (LIST 'EXPT 'X 2)
                         (LIST 'OR (LIST 'SYMBOLIC (LIST 'NOT '*PRECISE))
                               (LIST 'REALVALUEDP 'X))))))) 
(LET '(SIGN_RULES)) 
(REMFLAG '(I) 'RESERVED) 
(LET (LIST (LIST 'EQUAL (LIST 'EXPT 'I 2) (MINUS 1)))) 
(FLAG '(E I NIL PI) 'RESERVED) 
(FLAG '(POSITIVE NEGATIVE INFINITY) 'RESERVED) 
(FORALL (LIST '(X) 'T '(LET00 '((EQUAL (EXPT 10 (LOG10 X)) X))))) 
(FORALL
 (LIST '(A B X) '(EVALEQUAL (AEVAL 'A) (AEVAL 'B))
       '(LET00 '((EQUAL (EXPT A (LOGB X B)) X))))) 
(LET '((REPLACEBY (DF (LOG (~ X)) (~ X)) (QUOTIENT 1 X)))) 
(LET
 '((REPLACEBY (DF (LOG (QUOTIENT (~ X) (~ Y))) (~ Z))
    (DIFFERENCE (DF (LOG X) Z) (DF (LOG Y) Z))))) 
(LET '((REPLACEBY (DF (LOG10 (~ X)) (~ X)) (QUOTIENT 1 (TIMES X (LOG 10)))))) 
(LET
 '((REPLACEBY (DF (LOGB (~ X) (~ A)) (~ X))
    (DIFFERENCE (QUOTIENT 1 (TIMES X (LOG A)))
                (TIMES (QUOTIENT (LOGB X A) (TIMES A (LOG A))) (DF A X)))))) 
(DEFLIST
 '((ACOS SIMPIDEN) (ASIN SIMPIDEN) (ATAN SIMPIDEN) (ACOSH SIMPIDEN)
   (ASINH SIMPIDEN) (ATANH SIMPIDEN) (ACOT SIMPIDEN) (COS SIMPIDEN)
   (SIN SIMPIDEN) (TAN SIMPIDEN) (SEC SIMPIDEN) (SECH SIMPIDEN) (CSC SIMPIDEN)
   (CSCH SIMPIDEN) (COT SIMPIDEN) (ACOT SIMPIDEN) (COTH SIMPIDEN)
   (ACOTH SIMPIDEN) (COSH SIMPIDEN) (SINH SIMPIDEN) (TANH SIMPIDEN)
   (ASEC SIMPIDEN) (ACSC SIMPIDEN) (ASECH SIMPIDEN) (ACSCH SIMPIDEN)
   (ACOSD SIMPIDEN) (ASIND SIMPIDEN) (ATAND SIMPIDEN) (ACOTD SIMPIDEN)
   (COSD SIMPIDEN) (SIND SIMPIDEN) (TAND SIMPIDEN) (SECD SIMPIDEN)
   (CSCD SIMPIDEN) (COTD SIMPIDEN) (ACOTD SIMPIDEN) (ASECD SIMPIDEN)
   (ACSCD SIMPIDEN) (ATAN2D SIMPIDEN))
 'SIMPFN) 
(FLAG
 '(ACOS ASIN ATAN ACOSH ACOT ASINH ATANH COS SIN TAN COSH SINH TANH CSC CSCH
        SEC SECH COT ACOT COTH ACOTH ASEC ACSC ASECH ACSCH ACOSD ASIND ATAND
        ACOTD COSD SIND TAND CSCD SECD COTD ACOTD ASECD ACSCD)
 'FULL) 
(FLAG
 '(ACOTH ACSC ACSCH ASIN ASINH ATAN ATANH SIN TAN CSC CSCH SINH TANH COT COTH
         SIND ASIND TAND ATAND COTD CSCD ACSCD)
 'ODD) 
(FLAG '(COS SEC COSD SECD SECH COSH) 'EVEN) 
(FLAG '(COT COTD COTH CSC CSCD CSCH ACOTH) 'NONZERO) 
(DEFLIST
 '((SEC 1) (SED 1) (SECH 1) (CSC 1) (CSCD 1) (CSCH 1) (COT 1) (COTD 1)
   (COTH 1))
 'NUMBER-OF-ARGS) 
(PUT 'ATAN2D 'NUMBER-OF-ARGS 2) 
(LET
 (LIST (LIST 'EQUAL (LIST 'COS 0) 1) (LIST 'REPLACEBY (LIST 'COSD 0) 1)
       (LIST 'REPLACEBY (LIST 'SIND 15)
             (LIST 'TIMES (LIST 'QUOTIENT (LIST 'SQRT 2) 4)
                   (LIST 'DIFFERENCE (LIST 'SQRT 3) 1)))
       (LIST 'EQUAL (LIST 'SIN (LIST 'QUOTIENT 'PI 12))
             (LIST 'TIMES (LIST 'QUOTIENT (LIST 'SQRT 2) 4)
                   (LIST 'DIFFERENCE (LIST 'SQRT 3) 1)))
       (LIST 'EQUAL (LIST 'SIN (LIST 'TIMES 5 (LIST 'QUOTIENT 'PI 12)))
             (LIST 'TIMES (LIST 'QUOTIENT (LIST 'SQRT 2) 4)
                   (LIST 'PLUS (LIST 'SQRT 3) 1)))
       (LIST 'REPLACEBY (LIST 'SIND 75)
             (LIST 'TIMES (LIST 'QUOTIENT (LIST 'SQRT 2) 4)
                   (LIST 'PLUS (LIST 'SQRT 3) 1)))
       (LIST 'EQUAL (LIST 'SIN (LIST 'QUOTIENT 'PI 6)) (LIST 'QUOTIENT 1 2))
       (LIST 'REPLACEBY (LIST 'SIND 30) (LIST 'QUOTIENT 1 2))
       (LIST 'EQUAL (LIST 'SIN (LIST 'QUOTIENT 'PI 4))
             (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
       (LIST 'REPLACEBY (LIST 'SIND 45) (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
       (LIST 'EQUAL (LIST 'SIN (LIST 'QUOTIENT 'PI 3))
             (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
       (LIST 'REPLACEBY (LIST 'SIND 60) (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
       (LIST 'EQUAL (LIST 'COS (LIST 'QUOTIENT 'PI 2)) 0)
       (LIST 'EQUAL (LIST 'SIN (LIST 'QUOTIENT 'PI 2)) 1)
       (LIST 'EQUAL (LIST 'SIN 'PI) 0) (LIST 'EQUAL (LIST 'COS 'PI) (MINUS 1))
       (LIST 'EQUAL (LIST 'COSH 0) 1) (LIST 'EQUAL (LIST 'SECH 0) 1)
       (LIST 'REPLACEBY (LIST 'SINH 'I) (LIST 'TIMES 'I (LIST 'SIN 1)))
       (LIST 'REPLACEBY (LIST 'COSH 'I) (LIST 'COS 1))
       (LIST 'REPLACEBY (LIST 'ACOSH 0)
             (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2)))
       (LIST 'REPLACEBY (LIST 'ACOSH 1) 0)
       (LIST 'REPLACEBY (LIST 'ACOSH (MINUS 1)) (LIST 'TIMES 'I 'PI))
       (LIST 'REPLACEBY (LIST 'ACOTH 0)
             (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))))) 
(FORALL
 (LIST '(X) 'T
       '(LET00
         '((EQUAL (COS (ACOS X)) X) (EQUAL (SIN (ASIN X)) X)
           (EQUAL (TAN (ATAN X)) X) (EQUAL (COSD (ACOSD X)) X)
           (EQUAL (SIND (ASIND X)) X) (EQUAL (TAND (ATAND X)) X)
           (EQUAL (COSH (ACOSH X)) X) (EQUAL (SINH (ASINH X)) X)
           (EQUAL (TANH (ATANH X)) X) (EQUAL (COT (ACOT X)) X)
           (EQUAL (COTH (ACOTH X)) X) (EQUAL (SEC (ASEC X)) X)
           (EQUAL (COTD (ACOTD X)) X) (EQUAL (SECD (ASECD X)) X)
           (EQUAL (CSCD (ACSCD X)) X) (EQUAL (CSC (ACSC X)) X)
           (EQUAL (SECH (ASECH X)) X) (EQUAL (CSCH (ACSCH X)) X))))) 
(FORALL
 (LIST '(X) 'T
       '(LET00
         '((EQUAL (ACOS (MINUS X)) (DIFFERENCE PI (ACOS X)))
           (EQUAL (ASEC (MINUS X)) (DIFFERENCE PI (ASEC X)))
           (EQUAL (ACOT (MINUS X)) (DIFFERENCE PI (ACOT X)))
           (EQUAL (ACOSD (MINUS X)) (DIFFERENCE 180 (ACOSD X)))
           (EQUAL (ASECD (MINUS X)) (DIFFERENCE 180 (ASECD X)))
           (EQUAL (ACOTD (MINUS X)) (DIFFERENCE 180 (ACOTD X))))))) 
(LET
 '((REPLACEBY (SIN (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (SIN
       (PLUS (QUOTIENT W D)
             (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                    PI))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (SIN (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (SIN (TIMES (DIFFERENCE 1 (QUOTIENT K D)) PI))
     (AND (RATNUMP (QUOTIENT K D)) (GREATERP (QUOTIENT K D) (QUOTIENT 1 2)))))
   (REPLACEBY (COS (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (COS
       (PLUS (QUOTIENT W D)
             (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                    PI))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COS (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (MINUS (COS (TIMES (DIFFERENCE 1 (QUOTIENT K D)) PI)))
     (AND (RATNUMP (QUOTIENT K D)) (GREATERP (QUOTIENT K D) (QUOTIENT 1 2)))))
   (REPLACEBY (CSC (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (CSC
       (PLUS (QUOTIENT W D)
             (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                    PI))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (CSC (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (CSC (TIMES (DIFFERENCE 1 (QUOTIENT K D)) PI))
     (AND (RATNUMP (QUOTIENT K D)) (GREATERP (QUOTIENT K D) (QUOTIENT 1 2)))))
   (REPLACEBY (SEC (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (SEC
       (PLUS (QUOTIENT W D)
             (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                    PI))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (SEC (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (MINUS (SEC (TIMES (DIFFERENCE 1 (QUOTIENT K D)) PI)))
     (AND (RATNUMP (QUOTIENT K D)) (GREATERP (QUOTIENT K D) (QUOTIENT 1 2)))))
   (REPLACEBY (TAN (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TAN
      (PLUS (QUOTIENT W D)
            (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                   PI)))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COT (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (COT
      (PLUS (QUOTIENT W D)
            (TIMES (DIFFERENCE (QUOTIENT K D) (FIX (REPART (QUOTIENT K D))))
                   PI)))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 1))
      (REPLACEBY RP (REPART (QUOTIENT K D)))))))) 
(LET
 '((REPLACEBY (SIND (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP
                (LIST 'FIX
                      (LIST 'REPART
                            (LIST 'QUOTIENT 'K (LIST 'TIMES 'D 180)))))))
        1)
       (T (MINUS 1)))
      (SIND
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                                180)))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COSD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP
                (LIST 'FIX
                      (LIST 'REPART
                            (LIST 'QUOTIENT 'K (LIST 'TIMES 'D 180)))))))
        1)
       (T (MINUS 1)))
      (COSD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                                180)))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (TAND (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TAND
      (PLUS (QUOTIENT W D)
            (DIFFERENCE (QUOTIENT K D)
                        (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                               180))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COTD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (COTD
      (PLUS (QUOTIENT W D)
            (DIFFERENCE (QUOTIENT K D)
                        (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                               180))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (CSCD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP
                (LIST 'FIX
                      (LIST 'REPART
                            (LIST 'QUOTIENT 'K (LIST 'TIMES 'D 180)))))))
        1)
       (T (MINUS 1)))
      (CSCD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                                180)))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (SECD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP
                (LIST 'FIX
                      (LIST 'REPART
                            (LIST 'QUOTIENT 'K (LIST 'TIMES 'D 180)))))))
        1)
       (T (MINUS 1)))
      (SECD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (FIX (REPART (QUOTIENT K (TIMES D 180))))
                                180)))))
     (WHERE (AND (RATNUMP RP) (GEQ (ABS RP) 180))
      (REPLACEBY RP (REPART (QUOTIENT K D)))))))) 
(LET
 '((REPLACEBY (SIN (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES (SIGN (REPART (QUOTIENT K D)))
            (COS (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (COS (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES (SIGN (REPART (QUOTIENT K D)))
             (SIN (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D)))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (CSC (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES (SIGN (REPART (QUOTIENT K D)))
            (SEC (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (SEC (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES (SIGN (REPART (QUOTIENT K D)))
             (CSC (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D)))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (TAN (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS (COT (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (COT (QUOTIENT (PLUS (~ X) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS (TAN (PLUS (QUOTIENT X D) (TIMES I PI (IMPART (QUOTIENT K D))))))
     (AND (FREEOF X PI)
          (EQUAL (ABS (REPART (QUOTIENT K D))) (QUOTIENT 1 2))))))) 
(LET
 '((REPLACEBY (SIND (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (SIND
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (SIGN (REPART (QUOTIENT K D))) 180)))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COSD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (COSD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (SIGN (REPART (QUOTIENT K D))) 180)))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (TAND (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TAND
      (PLUS (QUOTIENT W D)
            (DIFFERENCE (QUOTIENT K D)
                        (TIMES (SIGN (REPART (QUOTIENT K D))) 180))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (COTD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (COTD
      (PLUS (QUOTIENT W D)
            (DIFFERENCE (QUOTIENT K D)
                        (TIMES (SIGN (REPART (QUOTIENT K D))) 180))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (SECD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (SECD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (SIGN (REPART (QUOTIENT K D))) 180)))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D))))))
   (REPLACEBY (CSCD (QUOTIENT (PLUS (~ (~ W)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (CSCD
       (PLUS (QUOTIENT W D)
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES (SIGN (REPART (QUOTIENT K D))) 180)))))
     (WHERE (AND (RATNUMP RP) (GREATERP (ABS RP) 90))
      (REPLACEBY RP (REPART (QUOTIENT K D)))))))) 
(LET
 '((REPLACEBY (SIND (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES (SIGN (REPART (QUOTIENT K D)))
            (COSD (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90)))
   (REPLACEBY (COSD (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES (SIGN (REPART (QUOTIENT K D)))
             (SIND (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D)))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90)))
   (REPLACEBY (CSCD (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (TIMES (SIGN (REPART (QUOTIENT K D)))
            (SECD (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90)))
   (REPLACEBY (SECD (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES (SIGN (REPART (QUOTIENT K D)))
             (CSCD (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D)))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90)))
   (REPLACEBY (TAND (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS (COTD (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90)))
   (REPLACEBY (COTD (QUOTIENT (PLUS (~ (~ X)) (~ (~ K))) (~ (~ D))))
    (WHEN
     (MINUS (TAND (PLUS (QUOTIENT X D) (TIMES I (IMPART (QUOTIENT K D))))))
     (EQUAL (ABS (REPART (QUOTIENT K D))) 90))))) 
(SETQ *ELEM-INHERIT T) 
(PUT 'KNOWLEDGE_ABOUT 'NUMBER-OF-ARGS 3) 
(PUT 'KNOWLEDGE_ABOUT 'DEFINED-ON-LINE '541) 
(PUT 'KNOWLEDGE_ABOUT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'KNOWLEDGE_ABOUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE KNOWLEDGE_ABOUT (OP ARG TOP)
    (COND
     ((OR (EQ DMODE* '|:RD:|) (EQ DMODE* '|:CR:|) (NULL *ELEM-INHERIT)) NIL)
     (T
      ((LAMBDA (VARSTACK*)
         (PROG (R OLD)
           (SETQ OLD (GET TOP 'OPMTCH))
           (PUT TOP 'OPMTCH NIL)
           (SETQ R (ERRORSET* (LIST 'AEVAL (MKQUOTE (LIST OP ARG))) NIL))
           (PUT TOP 'OPMTCH OLD)
           (RETURN
            (AND (NOT (ERRORP R)) (NOT (SMEMQ OP (CAR R)))
                 (NOT (SMEMQ TOP (CAR R)))))))
       NIL)))) 
(FLAG '(KNOWLEDGE_ABOUT) 'OPFN) 
(PUT 'TRIGQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'TRIGQUOT 'DEFINED-ON-LINE '559) 
(PUT 'TRIGQUOT 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'TRIGQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGQUOT (N D)
    (PROG (M U V W)
      (SETQ U (COND ((EQCAR N 'MINUS) (PROGN (SETQ M T) (CADR N))) (T N)))
      (SETQ V
              (COND ((EQCAR D 'MINUS) (PROGN (SETQ M (NOT M)) (CADR D)))
                    (T D)))
      (COND
       ((AND (PAIRP U) (PAIRP V))
        (COND
         ((AND (EQ (CAR U) 'SIN) (EQ (CAR V) 'COS) (EQUAL (CADR U) (CADR V)))
          (SETQ W 'TAN))
         ((AND (EQ (CAR U) 'COS) (EQ (CAR V) 'SIN) (EQUAL (CADR U) (CADR V)))
          (SETQ W 'COT)))))
      (COND ((NULL W) (RETURN (LIST 'QUOTIENT N D))))
      (SETQ W (LIST W (CADR U)))
      (RETURN (COND (M (LIST 'MINUS W)) (T W))))) 
(FLAG '(TRIGQUOT) 'OPFN) 
(LET
 '((REPLACEBY (COS (~ X))
    (WHEN (SIN (PLUS X (QUOTIENT PI 2)))
     (AND (NOT (EQUAL X 0)) (FREEOF (QUOTIENT (PLUS X (QUOTIENT PI 2)) PI) PI)
          (KNOWLEDGE_ABOUT SIN (PLUS X (QUOTIENT PI 2)) COS))))
   (REPLACEBY (COS (~ X))
    (WHEN (MINUS (SIN (DIFFERENCE X (QUOTIENT PI 2))))
     (AND (NOT (EQUAL X 0))
          (FREEOF (QUOTIENT (DIFFERENCE X (QUOTIENT PI 2)) PI) PI)
          (KNOWLEDGE_ABOUT SIN (DIFFERENCE X (QUOTIENT PI 2)) COS))))
   (REPLACEBY (TAN (~ X))
    (WHEN (TRIGQUOT (SIN X) (COS X)) (KNOWLEDGE_ABOUT SIN X TAN)))
   (REPLACEBY (COT (~ X))
    (WHEN (TRIGQUOT (COS X) (SIN X)) (KNOWLEDGE_ABOUT SIN X COT)))
   (REPLACEBY (SEC (~ X))
    (WHEN (QUOTIENT 1 (COS X)) (KNOWLEDGE_ABOUT COS X SEC)))
   (REPLACEBY (CSC (~ X))
    (WHEN (QUOTIENT 1 (SIN X)) (KNOWLEDGE_ABOUT SIN X CSC))))) 
(LET
 '((REPLACEBY (ASIN (~ X))
    (WHEN (DIFFERENCE (QUOTIENT PI 2) (ACOS X)) (KNOWLEDGE_ABOUT ACOS X ASIN)))
   (REPLACEBY (ACOT (~ X))
    (WHEN (DIFFERENCE (QUOTIENT PI 2) (ATAN X)) (KNOWLEDGE_ABOUT ATAN X ACOT)))
   (REPLACEBY (ACSC (~ X))
    (WHEN (ASIN (QUOTIENT 1 X)) (KNOWLEDGE_ABOUT ASIN (QUOTIENT 1 X) ACSC)))
   (REPLACEBY (ASEC (~ X))
    (WHEN (ACOS (QUOTIENT 1 X)) (KNOWLEDGE_ABOUT ACOS (QUOTIENT 1 X) ASEC)))
   (REPLACEBY (ACSCH (~ X))
    (WHEN (QUOTIENT (ACSC (MINUS (TIMES I X))) I)
     (KNOWLEDGE_ABOUT ACSC (MINUS (TIMES I X)) ACSCH)))
   (REPLACEBY (ASECH (~ X))
    (WHEN (QUOTIENT (ASEC X) I) (KNOWLEDGE_ABOUT ASEC X ASECH))))) 
(LET
 '((REPLACEBY (COSD (~ X))
    (WHEN (SIND (DIFFERENCE 90 X))
     (AND (NEQ X 0) (KNOWLEDGE_ABOUT SIND (DIFFERENCE 90 X) COSD))))
   (REPLACEBY (TAND (~ X))
    (WHEN (TRIGQUOT (SIND X) (COSD X)) (KNOWLEDGE_ABOUT SIND X COSD)))
   (REPLACEBY (COTD (~ X))
    (WHEN (TRIGQUOT (COSD X) (SIND X)) (KNOWLEDGE_ABOUT SIND X COSD)))
   (REPLACEBY (SECD (~ X))
    (WHEN (QUOTIENT 1 (COSD X)) (KNOWLEDGE_ABOUT COSD X SIND)))
   (REPLACEBY (CSCD (~ X))
    (WHEN (QUOTIENT 1 (SIND X)) (KNOWLEDGE_ABOUT SIND X COSD))))) 
(LET
 '((REPLACEBY (ASIND (~ X))
    (WHEN (DIFFERENCE 90 (ACOSD X)) (KNOWLEDGE_ABOUT ACOSD X ASIND)))
   (REPLACEBY (ACOTD (~ X))
    (WHEN (DIFFERENCE 90 (ATAND X)) (KNOWLEDGE_ABOUT ATAND X ACOTD)))
   (REPLACEBY (ACSCD (~ X))
    (WHEN (ASIND (QUOTIENT 1 X)) (KNOWLEDGE_ABOUT ASIND (QUOTIENT 1 X) ACSCD)))
   (REPLACEBY (ASECD (~ X))
    (WHEN (ACOSD (QUOTIENT 1 X))
     (KNOWLEDGE_ABOUT ACOSD (QUOTIENT 1 X) ASECD))))) 
(LET
 '((REPLACEBY (SINH (TIMES I (~ X)))
    (WHEN (TIMES I (SIN X)) (KNOWLEDGE_ABOUT SIN X SINH)))
   (REPLACEBY (SINH (TIMES I (QUOTIENT (~ X) (~ N))))
    (WHEN (TIMES I (SIN (QUOTIENT X N)))
     (KNOWLEDGE_ABOUT SIN (QUOTIENT X N) SINH)))
   (REPLACEBY (COSH (TIMES I (~ X)))
    (WHEN (COS X) (KNOWLEDGE_ABOUT COS X COSH)))
   (REPLACEBY (COSH (TIMES I (QUOTIENT (~ X) (~ N))))
    (WHEN (COS (QUOTIENT X N)) (KNOWLEDGE_ABOUT COS (QUOTIENT X N) COSH)))
   (REPLACEBY (COSH (~ X))
    (WHEN (MINUS (TIMES I (SINH (PLUS X (TIMES I (QUOTIENT PI 2))))))
     (AND (NOT (EQUAL X 0))
          (FREEOF (QUOTIENT (PLUS X (TIMES I (QUOTIENT PI 2))) PI) PI)
          (KNOWLEDGE_ABOUT SINH (PLUS X (TIMES I (QUOTIENT PI 2))) COSH))))
   (REPLACEBY (COSH (~ X))
    (WHEN (TIMES I (SINH (DIFFERENCE X (TIMES I (QUOTIENT PI 2)))))
     (AND (NOT (EQUAL X 0))
          (FREEOF (QUOTIENT (DIFFERENCE X (TIMES I (QUOTIENT PI 2))) PI) PI)
          (KNOWLEDGE_ABOUT SINH (DIFFERENCE X (TIMES I (QUOTIENT PI 2)))
                           COSH))))
   (REPLACEBY (TANH (~ X))
    (WHEN (QUOTIENT (SINH X) (COSH X)) (KNOWLEDGE_ABOUT SINH X TANH)))
   (REPLACEBY (COTH (~ X))
    (WHEN (QUOTIENT (COSH X) (SINH X)) (KNOWLEDGE_ABOUT SINH X COTH)))
   (REPLACEBY (SECH (~ X))
    (WHEN (QUOTIENT 1 (COSH X)) (KNOWLEDGE_ABOUT COSH X SECH)))
   (REPLACEBY (CSCH (~ X))
    (WHEN (QUOTIENT 1 (SINH X)) (KNOWLEDGE_ABOUT SINH X CSCH))))) 
(LET
 '((REPLACEBY (ACSCH (~ X))
    (WHEN (ASINH (QUOTIENT 1 X)) (KNOWLEDGE_ABOUT ASINH (QUOTIENT 1 X) ACSCH)))
   (REPLACEBY (ASECH (~ X))
    (WHEN (ACOSH (QUOTIENT 1 X)) (KNOWLEDGE_ABOUT ACOSH (QUOTIENT 1 X) ASECH)))
   (REPLACEBY (ASINH (~ X))
    (WHEN (MINUS (TIMES I (ASIN (TIMES I X))))
     (AND (FREEOF (TIMES I X) I) (KNOWLEDGE_ABOUT ASIN (TIMES I X) ASINH)))))) 
(LET
 '((REPLACEBY (SINH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (SINH
       (PLUS (QUOTIENT W D)
             (TIMES
              (DIFFERENCE (QUOTIENT K D)
                          (TIMES I (FIX (IMPART (QUOTIENT K D)))))
              PI))))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D))))))
   (REPLACEBY (SINH (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (SINH (TIMES (DIFFERENCE I (QUOTIENT K D)) PI))
     (AND (RATNUMP (TIMES I (QUOTIENT K D)))
          (GREATERP (ABS (TIMES I (QUOTIENT K D))) (QUOTIENT 1 2)))))
   (REPLACEBY (COSH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (COSH
       (PLUS (QUOTIENT W D)
             (TIMES
              (DIFFERENCE (QUOTIENT K D)
                          (TIMES I (FIX (IMPART (QUOTIENT K D)))))
              PI))))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D))))))
   (REPLACEBY (COSH (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (MINUS (COSH (TIMES (DIFFERENCE I (QUOTIENT K D)) PI)))
     (AND (RATNUMP (TIMES I (QUOTIENT K D)))
          (GREATERP (ABS (TIMES I (QUOTIENT K D))) (QUOTIENT 1 2)))))
   (REPLACEBY (CSCH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (CSCH
       (PLUS (QUOTIENT W D)
             (TIMES
              (DIFFERENCE (QUOTIENT K D)
                          (TIMES I (FIX (IMPART (QUOTIENT K D)))))
              PI))))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D))))))
   (REPLACEBY (CSCH (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (CSCH (TIMES (DIFFERENCE I (QUOTIENT K D)) PI))
     (AND (RATNUMP (TIMES I (QUOTIENT K D)))
          (GREATERP (ABS (TIMES I (QUOTIENT K D))) (QUOTIENT 1 2)))))
   (REPLACEBY (SECH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'EVENP (LIST 'FIX (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
        1)
       (T (MINUS 1)))
      (SECH
       (PLUS (QUOTIENT W D)
             (TIMES
              (DIFFERENCE (QUOTIENT K D)
                          (TIMES I (FIX (IMPART (QUOTIENT K D)))))
              PI))))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D))))))
   (REPLACEBY (SECH (TIMES (~ (~ K)) (QUOTIENT PI (~ (~ D)))))
    (WHEN (MINUS (SECH (TIMES (DIFFERENCE I (QUOTIENT K D)) PI)))
     (AND (RATNUMP (TIMES I (QUOTIENT K D)))
          (GREATERP (ABS (TIMES I (QUOTIENT K D))) (QUOTIENT 1 2)))))
   (REPLACEBY (TANH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TANH
      (PLUS (QUOTIENT W D)
            (TIMES
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES I (FIX (IMPART (QUOTIENT K D)))))
             PI)))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D))))))
   (REPLACEBY (COTH (QUOTIENT (PLUS (~ (~ W)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (COTH
      (PLUS (QUOTIENT W D)
            (TIMES
             (DIFFERENCE (QUOTIENT K D)
                         (TIMES I (FIX (IMPART (QUOTIENT K D)))))
             PI)))
     (WHERE (AND (RATNUMP IP) (GEQ (ABS IP) 1))
      (REPLACEBY IP (IMPART (QUOTIENT K D)))))))) 
(LET
 '((REPLACEBY (SINH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES I (SIGN (IMPART (QUOTIENT K D)))
            (COSH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D))))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (COSH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (TIMES I (SIGN (IMPART (QUOTIENT K D)))
            (SINH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D))))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (CSCH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES I (SIGN (IMPART (QUOTIENT K D)))
             (SECH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D)))))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (SECH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN
     (MINUS
      (TIMES I (SIGN (IMPART (QUOTIENT K D)))
             (CSCH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D)))))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (TANH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN (COTH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D)))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2))))
   (REPLACEBY (COTH (QUOTIENT (PLUS (~ (~ X)) (TIMES (~ (~ K)) PI)) (~ (~ D))))
    (WHEN (TANH (PLUS (QUOTIENT X D) (TIMES PI (REPART (QUOTIENT K D)))))
     (EQUAL (ABS (IMPART (QUOTIENT K D))) (QUOTIENT 1 2)))))) 
(SETK 'ACOS_RULES
      (AEVAL
       (CONS 'LIST
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J 0)
              STARTOVER
               (COND ((MINUSP (DIFFERENCE 12 J)) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (U)
                          ((LAMBDA (W)
                             ((LAMBDA (Q)
                                (COND
                                 ((AND (EQCAR Q 'ACOS) (EQUAL (CADR Q) W))
                                  (LIST (LIST 'REPLACEBY Q U)))))
                              (REVAL1 (LIST 'ACOS W) T)))
                           (REVAL1 (LIST 'COS U) T)))
                        (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES 'PI J) 12) T)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ J (PLUS2 J 1))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((MINUSP (DIFFERENCE 12 J)) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (U)
                          ((LAMBDA (W)
                             ((LAMBDA (Q)
                                (COND
                                 ((AND (EQCAR Q 'ACOS) (EQUAL (CADR Q) W))
                                  (LIST (LIST 'REPLACEBY Q U)))))
                              (REVAL1 (LIST 'ACOS W) T)))
                           (REVAL1 (LIST 'COS U) T)))
                        (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES 'PI J) 12) T)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ J (PLUS2 J 1))
               (GO LOOPLABEL))))) 
(LET '(ACOS_RULES)) 
(CLEAR (LIST 'ACOS_RULES)) 
(SETK 'ATAN_RULES
      (AEVAL
       (CONS 'LIST
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J 0)
              STARTOVER
               (COND ((MINUSP (DIFFERENCE 5 J)) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (U)
                          ((LAMBDA (W)
                             ((LAMBDA (Q)
                                (COND
                                 ((AND (EQCAR Q 'ATAN) (EQUAL (CADR Q) W))
                                  (LIST (LIST 'REPLACEBY Q U)))))
                              (REVAL1 (LIST 'ATAN W) T)))
                           (REVAL1 (LIST 'TAN U) T)))
                        (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES 'PI J) 12) T)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ J (PLUS2 J 1))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((MINUSP (DIFFERENCE 5 J)) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (U)
                          ((LAMBDA (W)
                             ((LAMBDA (Q)
                                (COND
                                 ((AND (EQCAR Q 'ATAN) (EQUAL (CADR Q) W))
                                  (LIST (LIST 'REPLACEBY Q U)))))
                              (REVAL1 (LIST 'ATAN W) T)))
                           (REVAL1 (LIST 'TAN U) T)))
                        (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES 'PI J) 12) T)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ J (PLUS2 J 1))
               (GO LOOPLABEL))))) 
(LET '(ATAN_RULES)) 
(CLEAR (LIST 'ATAN_RULES)) 
(PROG (J)
  (SETQ J 1)
 LAB
  (COND ((MINUSP (DIFFERENCE 2 J)) (RETURN NIL)))
  (PROG (OLDDMODE *MSG)
    (SETK 'ACOSD_RULES
          (AEVAL*
           (CONS 'LIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 0)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE 12 J)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (U)
                              ((LAMBDA (W)
                                 ((LAMBDA (Q)
                                    (COND
                                     ((AND (EQCAR Q 'ACOSD) (EQUAL (CADR Q) W))
                                      (LIST (LIST 'REPLACEBY Q U)))))
                                  (REVAL1 (LIST 'ACOSD W) T)))
                               (REVAL1 (LIST 'COSD U) T)))
                            (TIMES 15 J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ J (PLUS2 J 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((MINUSP (DIFFERENCE 12 J)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (U)
                              ((LAMBDA (W)
                                 ((LAMBDA (Q)
                                    (COND
                                     ((AND (EQCAR Q 'ACOSD) (EQUAL (CADR Q) W))
                                      (LIST (LIST 'REPLACEBY Q U)))))
                                  (REVAL1 (LIST 'ACOSD W) T)))
                               (REVAL1 (LIST 'COSD U) T)))
                            (TIMES 15 J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J (PLUS2 J 1))
                   (GO LOOPLABEL)))))
    (SETK 'ATAND_RULES
          (AEVAL*
           (CONS 'LIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 0)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE 5 J)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (U)
                              ((LAMBDA (W)
                                 ((LAMBDA (Q)
                                    (COND
                                     ((AND (EQCAR Q 'ATAND) (EQUAL (CADR Q) W))
                                      (LIST (LIST 'REPLACEBY Q U)))))
                                  (REVAL1 (LIST 'ATAND W) T)))
                               (REVAL1 (LIST 'TAND U) T)))
                            (TIMES 15 J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ J (PLUS2 J 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((MINUSP (DIFFERENCE 5 J)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (U)
                              ((LAMBDA (W)
                                 ((LAMBDA (Q)
                                    (COND
                                     ((AND (EQCAR Q 'ATAND) (EQUAL (CADR Q) W))
                                      (LIST (LIST 'REPLACEBY Q U)))))
                                  (REVAL1 (LIST 'ATAND W) T)))
                               (REVAL1 (LIST 'TAND U) T)))
                            (TIMES 15 J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J (PLUS2 J 1))
                   (GO LOOPLABEL)))))
    (PROGN
     (COND
      ((EQUAL J 1)
       (PROGN
        (SETQ OLDDMODE DMODE*)
        (COND (OLDDMODE (SETDMODE OLDDMODE NIL)))))
      (T (SETDMODE 'RATIONAL T)))
     NIL)
    (AEVAL* (LET '(ACOSD_RULES)))
    (AEVAL* (CLEAR (LIST 'ACOSD_RULES)))
    (AEVAL* (LET '(ATAND_RULES)))
    (AEVAL* (CLEAR (LIST 'ATAND_RULES)))
    (PROGN
     (COND
      ((EQUAL J 2)
       (PROGN
        (COND (OLDDMODE (SETDMODE OLDDMODE T)) (T (SETDMODE 'RATIONAL NIL))))))
     NIL))
  (SETQ J (PLUS2 J 1))
  (GO LAB)) 
(PUT '*BAR 'NUMBER-OF-ARGS 1) 
(PUT '*BAR 'DEFINED-ON-LINE '785) 
(PUT '*BAR 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT '*BAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *BAR (U)
    (COND
     ((OR (STRINGP U) (IDP U))
      (LIST2STRING
       (APPEND (EXPLODE2 "\\overline {") (APPEND (EXPLODE2 U) (EXPLODE2 '})))))
     (T (TYPERR U "a string or identifier")))) 
(PUT 'COMPLEX_CONJUGATES 'STAT 'RLIS) 
(PUT 'COMPLEX_CONJUGATES 'NUMBER-OF-ARGS 1) 
(PUT 'COMPLEX_CONJUGATES 'DEFINED-ON-LINE '797) 
(PUT 'COMPLEX_CONJUGATES 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'COMPLEX_CONJUGATES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPLEX_CONJUGATES (U)
    (PROG (V CONJV FSYM RS)
      (PROG (PR)
        (SETQ PR U)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (COND
             ((OR (ATOM PR)
                  (NOT
                   (AND (EQUAL (CAR PR) 'LIST) (EQUAL (LENGTH (CDR PR)) 2))))
              (TYPERR U "a 2-element list")))
            (COND ((NOT (IDP (SETQ V (CADR PR)))) (TYPERR V "identifier")))
            (COND
             ((NOT (IDP (SETQ CONJV (CADDR PR)))) (TYPERR CONJV "identifier")))
            (SETQ RS
                    (CONS (LIST 'LIST (LIST 'REPLACEBY (LIST 'CONJ V) CONJV))
                          RS))
            (COND
             ((GET V 'SIMPFN)
              (PROGN
               (COND ((NOT (GET CONJV 'SIMPFN)) (PUT CONJV 'SIMPFN 'SIMPIDEN)))
               (COND
                ((NOT (SETQ FSYM (GET V 'FANCY-FUNCTIONSYMBOL)))
                 (PUT CONJV 'FANCY-FUNCTIONSYMBOL (*BAR V)))
                (T (PUT CONJV 'FANCY-FUNCTIONSYMBOL (*BAR FSYM))))
               NIL))
             ((NOT (SETQ FSYM (GET V 'FANCY-SPECIAL-SYMBOL)))
              (PUT CONJV 'FANCY-SPECIAL-SYMBOL (*BAR V)))
             (T (PUT CONJV 'FANCY-SPECIAL-SYMBOL (*BAR FSYM))))
            NIL))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (SETQ RS (CONS 'LIST RS))
      (AEVAL (LET (LIST RS))))) 
(SETK (LIST 'REPART 'PI) (AEVAL 'PI)) 
(SETK (LIST 'IMPART 'PI) (AEVAL 0)) 
(FORALL
 (LIST '(X) 'T
       '(LET00
         (LIST
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOS 'X) 'X)
                (LIST 'MINUS
                      (LIST 'QUOTIENT
                            (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))
                            (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ASIN 'X) 'X)
                (LIST 'QUOTIENT
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))
                      (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ATAN 'X) 'X)
                (LIST 'QUOTIENT 1 (LIST 'PLUS 1 (LIST 'EXPT 'X 2))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOSH 'X) 'X)
                (LIST 'TIMES (LIST 'SQRT (LIST 'DIFFERENCE 'X 1))
                      (LIST 'QUOTIENT (LIST 'SQRT (LIST 'PLUS 'X 1))
                            (LIST 'DIFFERENCE (LIST 'EXPT 'X 2) 1))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOT 'X) 'X)
                (LIST 'MINUS
                      (LIST 'QUOTIENT 1 (LIST 'PLUS 1 (LIST 'EXPT 'X 2)))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ASINH 'X) 'X)
                (LIST 'QUOTIENT (LIST 'SQRT (LIST 'PLUS (LIST 'EXPT 'X 2) 1))
                      (LIST 'PLUS (LIST 'EXPT 'X 2) 1)))
          (LIST 'EQUAL (LIST 'DF (LIST 'ATANH 'X) 'X)
                (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOTH 'X) 'X)
                (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2))))
          (LIST 'EQUAL (LIST 'DF (LIST 'COS 'X) 'X)
                (LIST 'MINUS (LIST 'SIN 'X)))
          (LIST 'EQUAL (LIST 'DF (LIST 'SIN 'X) 'X) (LIST 'COS 'X))
          (LIST 'EQUAL (LIST 'DF (LIST 'SEC 'X) 'X)
                (LIST 'TIMES (LIST 'SEC 'X) (LIST 'TAN 'X)))
          (LIST 'EQUAL (LIST 'DF (LIST 'CSC 'X) 'X)
                (LIST 'MINUS (LIST 'TIMES (LIST 'CSC 'X) (LIST 'COT 'X))))
          (LIST 'EQUAL (LIST 'DF (LIST 'TAN 'X) 'X)
                (LIST 'PLUS 1 (LIST 'EXPT (LIST 'TAN 'X) 2)))
          (LIST 'EQUAL (LIST 'DF (LIST 'SINH 'X) 'X) (LIST 'COSH 'X))
          (LIST 'EQUAL (LIST 'DF (LIST 'COSH 'X) 'X) (LIST 'SINH 'X))
          (LIST 'EQUAL (LIST 'DF (LIST 'SECH 'X) 'X)
                (LIST 'MINUS (LIST 'TIMES (LIST 'SECH 'X) (LIST 'TANH 'X))))
          (LIST 'EQUAL (LIST 'DF (LIST 'TANH 'X) 'X)
                (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'TANH 'X) 2)))
          (LIST 'EQUAL (LIST 'DF (LIST 'CSCH 'X) 'X)
                (LIST 'MINUS (LIST 'TIMES (LIST 'CSCH 'X) (LIST 'COTH 'X))))
          (LIST 'EQUAL (LIST 'DF (LIST 'COT 'X) 'X)
                (LIST 'DIFFERENCE (MINUS 1) (LIST 'EXPT (LIST 'COT 'X) 2)))
          (LIST 'EQUAL (LIST 'DF (LIST 'COTH 'X) 'X)
                (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'COTH 'X) 2)))
          (LIST 'EQUAL (LIST 'DF (LIST 'COSD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'TIMES (LIST 'QUOTIENT 'PI 180) (LIST 'SIND 'X))))
          (LIST 'EQUAL (LIST 'DF (LIST 'SIND 'X) 'X)
                (LIST 'TIMES (LIST 'QUOTIENT 'PI 180) (LIST 'COSD 'X)))
          (LIST 'EQUAL (LIST 'DF (LIST 'TAND 'X) 'X)
                (LIST 'TIMES (LIST 'QUOTIENT 'PI 180)
                      (LIST 'PLUS 1 (LIST 'EXPT (LIST 'TAND 'X) 2))))
          (LIST 'EQUAL (LIST 'DF (LIST 'COTD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'TIMES (LIST 'QUOTIENT 'PI 180)
                            (LIST 'PLUS 1 (LIST 'EXPT (LIST 'COTD 'X) 2)))))
          (LIST 'EQUAL (LIST 'DF (LIST 'SECD 'X) 'X)
                (LIST 'TIMES (LIST 'QUOTIENT 'PI 180) (LIST 'SECD 'X)
                      (LIST 'TAND 'X)))
          (LIST 'EQUAL (LIST 'DF (LIST 'CSCD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'TIMES (LIST 'QUOTIENT 'PI 180) (LIST 'CSCD 'X)
                            (LIST 'COTD 'X))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOSD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'TIMES (LIST 'QUOTIENT 180 'PI)
                            (LIST 'QUOTIENT
                                  (LIST 'SQRT
                                        (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))
                                  (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2))))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ASIND 'X) 'X)
                (LIST 'TIMES (LIST 'QUOTIENT 180 'PI)
                      (LIST 'QUOTIENT
                            (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))
                            (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2)))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ATAND 'X) 'X)
                (LIST 'QUOTIENT 180
                      (LIST 'TIMES 'PI (LIST 'PLUS 1 (LIST 'EXPT 'X 2)))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACOTD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'QUOTIENT 180
                            (LIST 'TIMES 'PI
                                  (LIST 'PLUS 1 (LIST 'EXPT 'X 2))))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ACSCD 'X) 'X)
                (LIST 'MINUS
                      (LIST 'QUOTIENT 180
                            (LIST 'TIMES 'PI 'X
                                  (LIST 'SQRT
                                        (LIST 'DIFFERENCE (LIST 'EXPT 'X 2)
                                              1))))))
          (LIST 'EQUAL (LIST 'DF (LIST 'ASECD 'X) 'X)
                (LIST 'QUOTIENT 180
                      (LIST 'TIMES 'PI (LIST 'EXPT 'X 2)
                            (LIST 'SQRT
                                  (LIST 'DIFFERENCE 1
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT 'X 2))))))))))) 
(LET
 '((REPLACEBY (DF (ACSC (~ X)) X)
    (MINUS
     (QUOTIENT 1
               (TIMES (EXPT X 2)
                      (SQRT (DIFFERENCE 1 (QUOTIENT 1 (EXPT X 2))))))))
   (REPLACEBY (DF (ASEC (~ X)) X)
    (QUOTIENT 1
              (TIMES (EXPT X 2)
                     (SQRT (DIFFERENCE 1 (QUOTIENT 1 (EXPT X 2)))))))
   (REPLACEBY (DF (ACSCH (~ X)) X)
    (MINUS
     (QUOTIENT 1 (TIMES (EXPT X 2) (SQRT (PLUS 1 (QUOTIENT 1 (EXPT X 2))))))))
   (REPLACEBY (DF (ASECH (~ X)) X)
    (MINUS
     (QUOTIENT 1
               (TIMES (EXPT X 2) (SQRT (DIFFERENCE (QUOTIENT 1 X) 1))
                      (SQRT (PLUS (QUOTIENT 1 X) 1)))))))) 
(PUT 'ATAN2 'SIMPFN 'SIMPIDEN) 
(LET
 '((REPLACEBY (DF (ATAN2 (~ Y) (~ X)) (~ Z))
    (QUOTIENT (DIFFERENCE (TIMES X (DF Y Z)) (TIMES Y (DF X Z)))
              (PLUS (EXPT X 2) (EXPT Y 2))))
   (REPLACEBY (DF (ATAN2D (~ Y) (~ X)) (~ Z))
    (TIMES (QUOTIENT 180 PI)
           (QUOTIENT (DIFFERENCE (TIMES X (DF Y Z)) (TIMES Y (DF X Z)))
                     (PLUS (EXPT X 2) (EXPT Y 2))))))) 
(PUT 'SIMP-ATAN2 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-ATAN2 'DEFINED-ON-LINE '894) 
(PUT 'SIMP-ATAN2 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMP-ATAN2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-ATAN2 (U)
    (PROGN
     (COND
      ((NEQ (LENGTH U) 2)
       (RERROR 'ALG 17 (LIST "Wrong number of arguments to" 'ATAN2))))
     ((LAMBDA (VAL)
        (COND (VAL VAL)
              (T
               (PROG (X Y Z V W *COMPLEX)
                 (SETQ Y (REVAL1 (CAR U) T))
                 (SETQ X (REVAL1 (CADR U) T))
                 (COND
                  ((AND (NULL (CAR (SIMPIMPART (LIST X))))
                        (NULL (CAR (SIMPIMPART (LIST Y)))))
                   (RETURN (SIMPATAN2R Y X))))
                 (SETQ U (LIST Y X))
                 (COND
                  ((EQUAL X 0)
                   (PROGN
                    (SETQ Z (SIMP-SIGN1 (PREPSQ (SIMPREPART (LIST Y)))))
                    (COND
                     ((NULL (CAR Z))
                      (SETQ Z (SIMP-SIGN1 (REVAL1 (LIST 'QUOTIENT Y 'I) T)))))
                    (COND
                     ((AND (EQUAL (CDR Z) 1) (FIXP (CAR Z)))
                      (RETURN (MULTSQ Z (SIMP (LIST 'QUOTIENT 'PI 2))))))))
                  ((EQUAL Y 0)
                   (PROGN
                    (SETQ Z (SIMP-SIGN1 (PREPSQ (SIMPREPART (LIST X)))))
                    (COND
                     ((NULL (CAR Z))
                      (SETQ Z (SIMP-SIGN1 (REVAL1 (LIST 'QUOTIENT X 'I) T)))))
                    (COND
                     ((AND (EQUAL (CDR Z) 1) (FIXP (CAR Z)))
                      (COND ((EQUAL (CAR Z) 1) (RETURN (CONS NIL 1)))
                            (T (RETURN (SIMP 'PI))))))))
                  (T
                   (PROGN
                    (SETQ Z
                            (SIMP*
                             (LIST 'PLUS (LIST 'EXPT X 2) (LIST 'EXPT Y 2))))
                    (COND
                     ((NULL (CAR Z))
                      (RERROR 'ALG 212
                              "Essential singularity encountered in atan")))
                    (SETQ X
                            (LIST 'QUOTIENT X
                                  (LIST 'SQRT (SETQ Z (PREPSQ Z)))))
                    (SETQ Y (LIST 'QUOTIENT Y (LIST 'SQRT Z)))
                    (SETQ Z (SIMP-SIGN1 (PREPSQ (SIMPREPART (LIST X)))))
                    (COND
                     ((NULL (CAR Z))
                      (SETQ Z (SIMP-SIGN1 (REVAL1 (LIST 'QUOTIENT X 'I) T)))))
                    (SETQ V (SIMP-SIGN1 (PREPSQ (SIMPREPART (LIST Y)))))
                    (COND
                     ((NULL (CAR V))
                      (SETQ V (SIMP-SIGN1 (REVAL1 (LIST 'QUOTIENT Y 'I) T)))))
                    (COND
                     ((AND (EQUAL (CDR Z) 1) (FIXP (CAR Z)) (EQUAL (CDR V) 1)
                           (FIXP (CAR V)))
                      (PROGN
                       (SETQ W
                               (SIMP
                                (LIST 'ATAN
                                      (PREPSQ
                                       (RATIONALIZESQ
                                        (SIMP (LIST 'QUOTIENT Y X)))))))
                       (COND ((EQUAL (CAR Z) 1) (RETURN W))
                             ((EQUAL (CAR V) 1) (RETURN (ADDSQ (SIMP 'PI) W)))
                             (T (RETURN (ADDSQ W (NEGSQ (SIMP 'PI))))))))))))
                 (RETURN (SIMPIDEN (LIST 'ATAN2 (CAR U) (CADR U))))))))
      (VALUECHK 'ATAN2 U)))) 
(PUT 'ATAN2 'SIMPFN 'SIMP-ATAN2) 
(PUT 'SIMPATAN2R 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPATAN2R 'DEFINED-ON-LINE '944) 
(PUT 'SIMPATAN2R 'DEFINED-IN-FILE 'ALG/ELEM.RED) 
(PUT 'SIMPATAN2R 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPATAN2R (Y X)
    (PROG (Z V W)
      (COND
       ((EQUAL X 0)
        (PROGN
         (SETQ Z (SIMP-SIGN1 Y))
         (COND ((NULL (CAR Z)) (RERROR 'ALG 211 "atan2(0, 0) formed"))
               (T (RETURN (MULTSQ (SIMP (LIST 'QUOTIENT 'PI 2)) (INVSQ Z)))))))
       ((EQUAL Y 0)
        (PROGN
         (SETQ Z (SIMP-SIGN1 X))
         (RETURN
          (MULTSQ (ADDSQ (CONS 1 1) (MULTSQ (CONS (MINUS 1) 1) (INVSQ Z)))
                  (SIMP (LIST 'QUOTIENT 'PI 2))))))
       (T
        (PROGN
         (SETQ Z (SIMP-SIGN1 X))
         (SETQ V (SIMP-SIGN1 Y))
         (COND
          ((AND (EQUAL (CDR Z) 1) (FIXP (CAR Z)) (EQUAL (CDR V) 1)
                (FIXP (CAR V)))
           (PROGN
            (SETQ W (SIMP (LIST 'ATAN (LIST 'QUOTIENT Y X))))
            (COND ((EQUAL (CAR Z) 1) (RETURN W))
                  ((EQUAL (CAR V) 1) (RETURN (ADDSQ (SIMP 'PI) W)))
                  (T (RETURN (ADDSQ W (NEGSQ (SIMP 'PI))))))))))))
      (RETURN (SIMPIDEN (LIST 'ATAN2 Y X))))) 
(LET
 '((REPLACEBY (ATAN2D (~ Y) 0)
    (WHEN (TIMES (SIGN Y) 90) (AND (NEQ Y 0) (FIXP (SIGN Y)))))
   (REPLACEBY (ATAN2D 0 (~ X))
    (WHEN (COND ((EVALEQUAL (AEVAL (LIST 'SIGN 'X)) 1) 0) (T 180))
     (AND (NEQ X 0) (FIXP (SIGN X)))))
   (REPLACEBY (ATAN2D (~ Y) (~ X))
    (WHEN
     (PLUS
      (COND ((EVALEQUAL (AEVAL (LIST 'SIGN 'X)) 1) 0)
            ((EVALEQUAL (AEVAL (LIST 'SIGN 'Y)) 1) 180) (T (MINUS 180)))
      (ATAND (QUOTIENT Y X)))
     (AND (FIXP (SIGN X)) (FIXP (SIGN Y))))))) 
(FORALL
 (LIST '(X Y) 'T
       '(LET00
         '((EQUAL (DF (EXPT X Y) X) (TIMES Y (EXPT X (DIFFERENCE Y 1))))
           (EQUAL (DF (EXPT X Y) Y) (TIMES (LOG X) (EXPT X Y))))))) 
(OPERATOR (LIST 'DILOG 'ERF 'ERFI 'ERFC)) 
(LET
 (LIST
  (LIST 'LIST
        (LIST 'REPLACEBY (LIST 'DILOG 0) (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 6))
        (LIST 'REPLACEBY (LIST 'DILOG 1) 0)
        (LIST 'REPLACEBY (LIST 'DILOG 2)
              (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 12)))
        (LIST 'REPLACEBY (LIST 'DILOG (MINUS 1))
              (LIST 'DIFFERENCE (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 4)
                    (LIST 'TIMES 'I 'PI (LIST 'LOG 2))))))) 
(LET
 '((REPLACEBY (DF (DILOG (~ X)) (~ X))
    (MINUS (QUOTIENT (LOG X) (DIFFERENCE X 1)))))) 
(LET '((EQUAL (ERF 0) 0))) 
(FORALL (LIST '(X) 'T '(LET00 '((EQUAL (ERF (MINUS X)) (MINUS (ERF X))))))) 
(FORALL
 (LIST '(X) 'T
       '(LET00
         '((EQUAL (DF (ERF X) X)
                  (TIMES 2 (SQRT PI)
                         (QUOTIENT (EXPT E (MINUS (EXPT X 2))) PI))))))) 
(LET
 '((REPLACEBY (ERF (~ X))
    (WHEN (|COMPUTE:INT:FUNCTIONS| X ERF)
     (AND (NUMBERP X) (LESSP (ABS X) 5) (SYMBOLIC *ROUNDED)))))) 
(LET '((REPLACEBY (ERFC (~ X)) (DIFFERENCE 1 (ERF X))))) 
(LET '((REPLACEBY (ERFI (~ Z)) (MINUS (TIMES I (ERF (TIMES I Z))))))) 
(FORALL (LIST '(X) 'T '(LET00 '((EQUAL (EXP X) (EXPT E X)))))) 
(LET
 (LIST (LIST 'EQUAL (LIST 'EXPT 'E (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))) 'I)
       (LIST 'EQUAL (LIST 'EXPT 'E (LIST 'TIMES 'I 'PI)) (MINUS 1)))) 
(FORALL (LIST '(X) 'T '(LET00 '((EQUAL (DF (ABS X) X) (QUOTIENT (ABS X) X)))))) 
(SETK 'INVTRIGRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SIN (LIST 'ATAN (LIST '~ 'U)))
                   (LIST 'QUOTIENT 'U
                         (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY (LIST 'COS (LIST 'ATAN (LIST '~ 'U)))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'TIMES 2 (LIST 'ATAN (LIST '~ 'U))))
                   (LIST 'TIMES 2
                         (LIST 'QUOTIENT 'U (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'TIMES 2 (LIST 'ATAN (LIST '~ 'U))))
                   (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2))
                         (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ATAN (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATAN 'U)))
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2))
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATAN 'U)))
                                     2
                                     (LIST 'QUOTIENT 'U
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ATAN (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATAN 'U)))
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2))
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATAN 'U)))
                                     2
                                     (LIST 'QUOTIENT 'U
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY (LIST 'SIN (LIST 'ACOS (LIST '~ 'U)))
                   (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY (LIST 'COS (LIST 'ASIN (LIST '~ 'U)))
                   (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'TIMES 2 (LIST 'ACOS (LIST '~ 'U))))
                   (LIST 'TIMES 2 'U
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'TIMES 2 (LIST 'ACOS (LIST '~ 'U))))
                   (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'EXPT 'U 2)) 1))
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'TIMES 2 (LIST 'ASIN (LIST '~ 'U))))
                   (LIST 'TIMES 2 'U
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'TIMES 2 (LIST 'ASIN (LIST '~ 'U))))
                   (LIST 'DIFFERENCE 1 (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ACOS (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOS 'U)))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))
                                           1))
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOS 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ACOS (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOS 'U)))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))
                                           1))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOS 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ASIN (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASIN 'U)))
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASIN 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ASIN (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASIN 'U)))
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASIN 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))))) 
(LET
 '((REPLACEBY (SIND (ATAND (~ U))) (QUOTIENT U (SQRT (PLUS 1 (EXPT U 2)))))
   (REPLACEBY (COSD (ATAND (~ U))) (QUOTIENT 1 (SQRT (PLUS 1 (EXPT U 2)))))
   (REPLACEBY (SIND (TIMES 2 (ATAND (~ U))))
    (TIMES 2 (QUOTIENT U (PLUS 1 (EXPT U 2)))))
   (REPLACEBY (COSD (TIMES 2 (ATAND (~ U))))
    (QUOTIENT (DIFFERENCE 1 (EXPT U 2)) (PLUS 1 (EXPT U 2))))
   (REPLACEBY (SIND (TIMES (~ N) (ATAND (~ U))))
    (WHEN
     (PLUS
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ATAND U)))
             (QUOTIENT (DIFFERENCE 1 (EXPT U 2)) (PLUS 1 (EXPT U 2))))
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ATAND U))) 2
             (QUOTIENT U (PLUS 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2))))
   (REPLACEBY (COSD (TIMES (~ N) (ATAND (~ U))))
    (WHEN
     (DIFFERENCE
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ATAND U)))
             (QUOTIENT (DIFFERENCE 1 (EXPT U 2)) (PLUS 1 (EXPT U 2))))
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ATAND U))) 2
             (QUOTIENT U (PLUS 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2))))
   (REPLACEBY (SIND (ACOSD (~ U))) (SQRT (DIFFERENCE 1 (EXPT U 2))))
   (REPLACEBY (COSD (ASIND (~ U))) (SQRT (DIFFERENCE 1 (EXPT U 2))))
   (REPLACEBY (SIND (TIMES 2 (ACOSD (~ U))))
    (TIMES 2 U (SQRT (DIFFERENCE 1 (EXPT U 2)))))
   (REPLACEBY (COSD (TIMES 2 (ACOSD (~ U))))
    (DIFFERENCE (TIMES 2 (EXPT U 2)) 1))
   (REPLACEBY (SIND (TIMES 2 (ASIND (~ U))))
    (TIMES 2 U (SQRT (DIFFERENCE 1 (EXPT U 2)))))
   (REPLACEBY (COSD (TIMES 2 (ASIND (~ U))))
    (DIFFERENCE 1 (TIMES 2 (EXPT U 2))))
   (REPLACEBY (SIND (TIMES (~ N) (ACOSD (~ U))))
    (WHEN
     (PLUS
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ACOSD U)))
             (DIFFERENCE (TIMES 2 (EXPT U 2)) 1))
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ACOSD U))) 2 U
             (SQRT (DIFFERENCE 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2))))
   (REPLACEBY (COSD (TIMES (~ N) (ACOSD (~ U))))
    (WHEN
     (DIFFERENCE
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ACOSD U)))
             (DIFFERENCE (TIMES 2 (EXPT U 2)) 1))
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ACOSD U))) 2 U
             (SQRT (DIFFERENCE 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2))))
   (REPLACEBY (SIND (TIMES (~ N) (ASIND (~ U))))
    (WHEN
     (PLUS
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ASIND U)))
             (DIFFERENCE 1 (TIMES 2 (EXPT U 2))))
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ASIND U))) 2 U
             (SQRT (DIFFERENCE 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2))))
   (REPLACEBY (COSD (TIMES (~ N) (ASIND (~ U))))
    (WHEN
     (DIFFERENCE
      (TIMES (COSD (TIMES (DIFFERENCE N 2) (ASIND U)))
             (DIFFERENCE 1 (TIMES 2 (EXPT U 2))))
      (TIMES (SIND (TIMES (DIFFERENCE N 2) (ASIND U))) 2 U
             (SQRT (DIFFERENCE 1 (EXPT U 2)))))
     (AND (FIXP N) (GREATERP N 2)))))) 
(SETK 'INVTRIGRULES2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'QUOTIENT (LIST 'ATAN (LIST '~ 'X)) 2))
                   (LIST 'SIN
                         (LIST 'ATAN
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'SQRT
                                                 (LIST 'PLUS 1
                                                       (LIST 'EXPT 'X 2)))
                                           1)
                                     'X))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'QUOTIENT (LIST 'ATAN (LIST '~ 'X)) 2))
                   (LIST 'COS
                         (LIST 'ATAN
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'SQRT
                                                 (LIST 'PLUS 1
                                                       (LIST 'EXPT 'X 2)))
                                           1)
                                     'X))))))) 
(LET '(INVTRIGRULES2)) 
(SETK 'INVHYPRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SINH (LIST 'ATANH (LIST '~ 'U)))
                   (LIST 'QUOTIENT 'U
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY (LIST 'COSH (LIST 'ATANH (LIST '~ 'U)))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'SINH (LIST 'TIMES 2 (LIST 'ATANH (LIST '~ 'U))))
                   (LIST 'TIMES 2
                         (LIST 'QUOTIENT 'U
                               (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'COSH (LIST 'TIMES 2 (LIST 'ATANH (LIST '~ 'U))))
                   (LIST 'QUOTIENT (LIST 'PLUS 1 (LIST 'EXPT 'U 2))
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ATANH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATANH 'U)))
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATANH 'U)))
                                     2
                                     (LIST 'QUOTIENT 'U
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ATANH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATANH 'U)))
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ATANH 'U)))
                                     2
                                     (LIST 'QUOTIENT 'U
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY (LIST 'SINH (LIST 'ACOSH (LIST '~ 'U)))
                   (LIST 'TIMES (LIST 'SQRT (LIST 'DIFFERENCE 'U 1))
                         (LIST 'SQRT (LIST 'PLUS 'U 1))))
             (LIST 'REPLACEBY (LIST 'COSH (LIST 'ASINH (LIST '~ 'U)))
                   (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SINH (LIST 'TIMES 2 (LIST 'ACOSH (LIST '~ 'U))))
                   (LIST 'TIMES 2 'U (LIST 'SQRT (LIST 'DIFFERENCE 'U 1))
                         (LIST 'SQRT (LIST 'PLUS 'U 1))))
             (LIST 'REPLACEBY
                   (LIST 'COSH (LIST 'TIMES 2 (LIST 'ACOSH (LIST '~ 'U))))
                   (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'EXPT 'U 2)) 1))
             (LIST 'REPLACEBY
                   (LIST 'SINH (LIST 'TIMES 2 (LIST 'ASINH (LIST '~ 'U))))
                   (LIST 'TIMES 2 'U
                         (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
             (LIST 'REPLACEBY
                   (LIST 'COSH (LIST 'TIMES 2 (LIST 'ASINH (LIST '~ 'U))))
                   (LIST 'PLUS 1 (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ACOSH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOSH 'U)))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))
                                           1))
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOSH 'U)))
                                     2 'U (LIST 'SQRT (LIST 'DIFFERENCE 'U 1))
                                     (LIST 'SQRT (LIST 'PLUS 'U 1))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ACOSH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOSH 'U)))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))
                                           1))
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ACOSH 'U)))
                                     2 'U (LIST 'SQRT (LIST 'DIFFERENCE 'U 1))
                                     (LIST 'SQRT (LIST 'PLUS 'U 1))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ASINH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASINH 'U)))
                                     (LIST 'PLUS 1
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASINH 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ASINH (LIST '~ 'U))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASINH 'U)))
                                     (LIST 'PLUS 1
                                           (LIST 'TIMES 2 (LIST 'EXPT 'U 2))))
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 2)
                                                 (LIST 'ASINH 'U)))
                                     2 'U
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'U 2)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 2))))
             (LIST 'REPLACEBY (LIST 'ATANH (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ACOSH
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'X 2))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'X 2))))
                               (LIST 'QUOTIENT (LIST 'SIGN 'X) 2))
                         (LIST 'AND (LIST 'SYMBOLIC (LIST 'NOT '*COMPLEX))
                               (LIST 'NEQ (LIST 'EXPT 'X 2) 1)
                               (LIST 'FREEOF
                                     (LIST 'ACOSH
                                           (LIST 'QUOTIENT
                                                 (LIST 'PLUS 1
                                                       (LIST 'EXPT 'X 2))
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'X 2))))
                                     'ACOSH))))))) 
(LET '(INVTRIGRULES INVHYPRULES)) 
(SETK 'TRIG_IMAG_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'SINH (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN (LIST 'COSH (LIST 'QUOTIENT 'X 'Y))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'SIN (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN (LIST 'COS (LIST 'QUOTIENT 'X 'Y))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'ASIN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'ASINH (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'ATANH (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'AND (LIST 'EQUAL (LIST 'IMPART 'Y) 0)
                               (LIST 'NOT
                                     (LIST 'AND
                                           (LIST 'OR (LIST 'EQUAL 'X 1)
                                                 (LIST 'EQUAL 'X
                                                       (LIST 'MINUS 1)))
                                           (LIST 'EQUAL 'Y 1))))))
             (LIST 'REPLACEBY
                   (LIST 'ASINH
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'ASIN (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))
             (LIST 'REPLACEBY
                   (LIST 'ATANH
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'Y)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'ATAN (LIST 'QUOTIENT 'X 'Y)))
                         (LIST 'EQUAL (LIST 'IMPART 'Y) 0)))))) 
(LET '(TRIG_IMAG_RULES)) 
(OPERATOR (LIST 'ARBINT)) 
(LET
 '((LIST
    (REPLACEBY (COS (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN
      (COS
       (PLUS
        (TIMES (COND ((BOOLVALUE* (REVALX (LIST 'EVENP 'N))) 0) (T 1)) PI
               (ARBINT I))
        X))
      (FIXP N)))
    (REPLACEBY (SIN (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN
      (SIN
       (PLUS
        (TIMES (COND ((BOOLVALUE* (REVALX (LIST 'EVENP 'N))) 0) (T 1)) PI
               (ARBINT I))
        X))
      (FIXP N)))
    (REPLACEBY (TAN (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN (TAN X) (FIXP N)))
    (REPLACEBY (SEC (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN
      (SEC
       (PLUS
        (TIMES (COND ((BOOLVALUE* (REVALX (LIST 'EVENP 'N))) 0) (T 1)) PI
               (ARBINT I))
        X))
      (FIXP N)))
    (REPLACEBY (CSC (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN
      (CSC
       (PLUS
        (TIMES (COND ((BOOLVALUE* (REVALX (LIST 'EVENP 'N))) 0) (T 1)) PI
               (ARBINT I))
        X))
      (FIXP N)))
    (REPLACEBY (COT (PLUS (TIMES (~ N) PI (ARBINT (~ I))) (~ (~ X))))
     (WHEN (COT X) (FIXP N)))
    (REPLACEBY (EXP (PLUS (TIMES (~ N) I PI (ARBINT (~ K))) (~ (~ X))))
     (WHEN
      (EXP
       (PLUS
        (TIMES (COND ((BOOLVALUE* (REVALX (LIST 'EVENP 'N))) 0) (T 1)) I PI
               (ARBINT K))
        X))
      (FIXP N)))))) 
(LET
 '((REPLACEBY (COSD (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (COSD
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 360)) 360))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 360))))
   (REPLACEBY (SIND (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (SIND
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 360)) 360))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 360))))
   (REPLACEBY (SECD (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (SECD
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 360)) 360))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 360))))
   (REPLACEBY (CSCD (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (CSCD
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 360)) 360))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 360))))
   (REPLACEBY (TAND (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (TAND
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 180)) 180))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 180))))
   (REPLACEBY (COTD (PLUS (TIMES (~ N) (ARBINT (~ K))) (~ (~ X))))
    (WHEN
     (COTD
      (PLUS X
            (TIMES (DIFFERENCE N (TIMES (FIX (QUOTIENT N 180)) 180))
                   (ARBINT K))))
     (AND (FIXP N) (GEQ (ABS N) 180)))))) 
(ENDMODULE) 