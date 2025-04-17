(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRIGSMP2)) 
(SHARE (LIST 'HYP_PREFERENCE 'TRIG_PREFERENCE)) 
(FLUID '(*COMPLEX DMODE*)) 
(FLUID '(DEPL*)) 
(PUT 'TRIGSIMP* 'NUMBER-OF-ARGS 1) 
(PUT 'TRIGSIMP* 'DEFINED-ON-LINE '54) 
(PUT 'TRIGSIMP* 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGSIMP* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIGSIMP* (U)
    ((LAMBDA (DEPL*) (TRIGSIMP (REVAL1 (CAR U) T) (REVLIS (CDR U)))) DEPL*)) 
(PUT 'TRIGSIMP 'PSOPFN 'TRIGSIMP*) 
(PUT 'TRIGSIMP 'NUMBER-OF-ARGS 2) 
(PUT 'TRIGSIMP 'DEFINED-ON-LINE '61) 
(PUT 'TRIGSIMP 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGSIMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGSIMP (F OPTIONS)
    (COND ((ATOM F) F)
          ((EQ (CAR F) 'EQUAL)
           (CONS 'EQUAL
                 (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FF (CDR F))
                   (COND ((NULL FF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (FF) (TRIGSIMP FF OPTIONS))
                                     (CAR FF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ FF (CDR FF))
                   (COND ((NULL FF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (FF) (TRIGSIMP FF OPTIONS)) (CAR FF))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQ (CAR F) 'LIST)
           (CONS 'LIST
                 (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FF (CDR F))
                   (COND ((NULL FF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (FF) (TRIGSIMP FF OPTIONS))
                                     (CAR FF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ FF (CDR FF))
                   (COND ((NULL FF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (FF) (TRIGSIMP FF OPTIONS)) (CAR FF))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQ (CAR F) 'MAT)
           (CONS 'MAT
                 (PROG (FF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FF (CDR F))
                   (COND ((NULL FF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (FF)
                                       (PROG (FFF FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ FFF FF)
                                         (COND ((NULL FFF) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (FFF)
                                                             (TRIGSIMP FFF
                                                              OPTIONS))
                                                           (CAR FFF))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ FFF (CDR FFF))
                                         (COND
                                          ((NULL FFF) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (FFF)
                                                     (TRIGSIMP FFF OPTIONS))
                                                   (CAR FFF))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL)))
                                     (CAR FF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ FF (CDR FF))
                   (COND ((NULL FF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (FF)
                               (PROG (FFF FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ FFF FF)
                                 (COND ((NULL FFF) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (FFF)
                                                     (TRIGSIMP FFF OPTIONS))
                                                   (CAR FFF))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ FFF (CDR FFF))
                                 (COND ((NULL FFF) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (FFF)
                                             (TRIGSIMP FFF OPTIONS))
                                           (CAR FFF))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR FF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          ((EQ (CAR F) 'TAYLOR*) (TAYAPPLYOPFN2 F (FUNCTION TRIGSIMP) OPTIONS))
          (T (TRIGSIMP1 F OPTIONS)))) 
(PUT 'TRIGSIMP1 'NUMBER-OF-ARGS 2) 
(PUT 'TRIGSIMP1 'DEFINED-ON-LINE '76) 
(PUT 'TRIGSIMP1 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGSIMP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGSIMP1 (F OPTIONS)
    (PROG (DNAME TRIGPREFERENCE HYPPREFERENCE TANPREFERENCE TANHPREFERENCE
           DIRECTION MODE KEEPALLTRIG ONLYTAN OPT_ARGS)
      (SETQ ONLYTAN
              (NOT
               (OR (SMEMBER 'SIN F) (SMEMBER 'COS F) (SMEMBER 'SINH F)
                   (SMEMBER 'COSH F) (SMEMBER 'CSC F) (SMEMBER 'SEC F)
                   (SMEMBER 'CSCH F) (SMEMBER 'SECH F))))
      (COND
       ((AND ONLYTAN
             (NOT
              (OR (SMEMBER 'TAN F) (SMEMBER 'COT F) (SMEMBER 'TANH F)
                  (SMEMBER 'COTH F) (SMEMBER 'EXP F) (SMEMBER 'E F))))
        (RETURN F)))
      (COND
       ((SETQ DNAME (GET DMODE* 'DNAME))
        (PROGN (OFF (LIST DNAME)) (SETQ F (PREPSQ (SIMP* F))))))
      (PROG (U)
        (SETQ U OPTIONS)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (COND
            ((MEMQ U '(SIN COS))
             (COND
              (TRIGPREFERENCE
               (OR (EQ U TRIGPREFERENCE)
                   (REDERR "Incompatible options: use either sin or cos.")))
              (T (SETQ TRIGPREFERENCE U))))
            ((MEMQ U '(SINH COSH))
             (COND
              (HYPPREFERENCE
               (OR (EQ U HYPPREFERENCE)
                   (REDERR "Incompatible options: use either sinh or cosh.")))
              (T (SETQ HYPPREFERENCE U))))
            ((EQ U 'TAN) (SETQ TANPREFERENCE T))
            ((EQ U 'TANH) (SETQ TANHPREFERENCE T))
            ((MEMQ U '(EXPAND COMBINE COMPACT))
             (COND
              (DIRECTION
               (OR (EQ U DIRECTION)
                   (REDERR
                    "Incompatible options: use either expand or combine or compact.")))
              (T (SETQ DIRECTION U))))
            ((MEMQ U '(HYP TRIG EXPON))
             (COND
              (MODE
               (OR (EQ U MODE)
                   (REDERR
                    "Incompatible options: use either hyp or trig or expon.")))
              (T (SETQ MODE U))))
            ((EQ U 'KEEPALLTRIG) (SETQ KEEPALLTRIG T))
            ((AND (EQCAR U 'QUOTIENT) (NOT (MEMBER U OPT_ARGS)))
             (SETQ OPT_ARGS (CONS U OPT_ARGS)))
            (T
             (REDERR
              (LIST "Option" U "invalid." " Allowed options are"
                    "sin or cos, tan, cosh or sinh, tanh,"
                    "expand or combine or compact,"
                    "hyp or trig or expon, keepalltrig.")))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (COND
       (TRIGPREFERENCE
        (COND
         (TANPREFERENCE
          (SETQ TRIGPREFERENCE
                  (COND ((EQ TRIGPREFERENCE 'SIN) 'COS) (T 'SIN))))))
       (T (SETQ TRIGPREFERENCE 'SIN)))
      (SETQ TRIG_PREFERENCE
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) TRIGPREFERENCE))
      (COND
       (HYPPREFERENCE
        (COND
         (TANHPREFERENCE
          (SETQ HYPPREFERENCE
                  (COND ((EQ HYPPREFERENCE 'SINH) 'COSH) (T 'SINH))))))
       (T (SETQ HYPPREFERENCE 'SINH)))
      (SETQ HYP_PREFERENCE
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) HYPPREFERENCE))
      (OR DIRECTION (SETQ DIRECTION 'EXPAND))
      (COND ((EQ TRIGPREFERENCE 'SIN) (AEVAL (LET '(TRIG_NORMALIZE2SIN*))))
            (T (AEVAL (LET '(TRIG_NORMALIZE2COS*)))))
      (COND ((EQ HYPPREFERENCE 'SINH) (AEVAL (LET '(TRIG_NORMALIZE2SINH*))))
            (T (AEVAL (LET '(TRIG_NORMALIZE2COSH*)))))
      (COND
       ((OR (NOT KEEPALLTRIG) (MEMQ DIRECTION '(COMBINE COMPACT)))
        (SETQ F (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRIG_STANDARDIZE*) F)))))
      (COND
       (MODE
        (SETQ F
                (COND
                 ((EQ MODE 'TRIG)
                  (BEHANDLE
                   (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'HYP2TRIG*) F))))
                 ((EQ MODE 'HYP)
                  (PROGN
                   (SETQ F (BEHANDLE F))
                   (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRIG2HYP*) F))))
                 ((EQ MODE 'EXPON)
                  (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRIG2EXP*) F)))))))
      (COND
       ((EQ DIRECTION 'EXPAND)
        (PROG (U)
          (AEVAL (LET '(TRIG_EXPAND_ADDITION*)))
          (SETQ U (SUBS_SYMBOLIC_MULTIPLES (REVAL1 F T) OPT_ARGS))
          (SETQ F (CAR U))
          (AEVAL (LET '(TRIG_EXPAND_MULTIPLICATION*)))
          (SETQ F (AEVAL (LIST 'SUB (CADR U) F)))
          (AEVAL
           (CLEARRULES
            (LIST 'TRIG_EXPAND_ADDITION* 'TRIG_EXPAND_MULTIPLICATION*)))))
       ((EQ DIRECTION 'COMBINE)
        (PROGN
         (SETQ F (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRIG_COMBINE*) F)))
         (COND
          ((AND ONLYTAN KEEPALLTRIG)
           (SETQ F (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'SUBTAN*) F))))))))
      (AEVAL
       (CLEARRULES
        (LIST 'TRIG_NORMALIZE2SIN* 'TRIG_NORMALIZE2COS* 'TRIG_NORMALIZE2SINH*
              'TRIG_NORMALIZE2COSH*)))
      (COND
       ((EQ DIRECTION 'COMPACT)
        (PROGN
         (AEVAL (LOAD_PACKAGE (LIST 'COMPACT)))
         (SETQ F
                 (AEVAL
                  (LIST 'WHEREEXP
                        (LIST 'LIST 'TRIG_EXPAND_ADDITION*
                              'TRIG_EXPAND_MULTIPLICATION*)
                        F)))
         (SETQ F
                 (AEVAL
                  (LIST 'COMPACT F
                        (LIST 'LIST
                              (LIST 'EQUAL
                                    (LIST 'PLUS (LIST 'EXPT (LIST 'SIN 'X) 2)
                                          (LIST 'EXPT (LIST 'COS 'X) 2))
                                    1))))))))
      (COND
       (TANPREFERENCE
        (SETQ F
                (COND
                 ((EQ TRIGPREFERENCE 'SIN)
                  (AEVAL
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'X))
                                     (LIST 'TIMES (LIST 'COS 'X)
                                           (LIST 'TAN 'X))))
                         F)))
                 (T
                  (AEVAL
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'X))
                                     (LIST 'QUOTIENT (LIST 'SIN 'X)
                                           (LIST 'TAN 'X))))
                         F)))))))
      (COND
       (TANHPREFERENCE
        (SETQ F
                (COND
                 ((EQ HYPPREFERENCE 'SINH)
                  (AEVAL
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'SINH (LIST '~ 'X))
                                     (LIST 'TIMES (LIST 'COSH 'X)
                                           (LIST 'TANH 'X))))
                         F)))
                 (T
                  (AEVAL
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'COSH (LIST '~ 'X))
                                     (LIST 'QUOTIENT (LIST 'SINH 'X)
                                           (LIST 'TANH 'X))))
                         F)))))))
      (COND (DNAME (PROGN (ON (LIST DNAME)) (SETQ F (PREPSQ (SIMP* F))))))
      (RETURN F))) 
(PUT 'MORE_VARIABLES 'NUMBER-OF-ARGS 2) 
(PUT 'MORE_VARIABLES 'DEFINED-ON-LINE '222) 
(PUT 'MORE_VARIABLES 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'MORE_VARIABLES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MORE_VARIABLES (A B)
    (GREATERP (LENGTH (FIND_INDETS A NIL)) (LENGTH (FIND_INDETS B NIL)))) 
(PUT 'FIND_INDETS 'NUMBER-OF-ARGS 2) 
(PUT 'FIND_INDETS 'DEFINED-ON-LINE '225) 
(PUT 'FIND_INDETS 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'FIND_INDETS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND_INDETS (TERM VARS)
    (COND ((NUMBERP TERM) VARS)
          ((ATOM TERM) (COND ((NOT (MEMQ TERM VARS)) (CONS TERM VARS))))
          ((CDR TERM)
           (PROGN
            (SETQ TERM (CDR TERM))
            (SETQ VARS (FIND_INDETS (CAR TERM) VARS))
            (COND ((CDR TERM) (FIND_INDETS (CDR TERM) VARS)) (T VARS))))
          (T (FIND_INDETS (CAR TERM) VARS)))) 
(AEVAL (OPERATOR (LIST 'AUXILIARY_SYMBOLIC_VAR*))) 
(PUT 'SUBS_SYMBOLIC_MULTIPLES 'NUMBER-OF-ARGS 2) 
(PUT 'SUBS_SYMBOLIC_MULTIPLES 'DEFINED-ON-LINE '242) 
(PUT 'SUBS_SYMBOLIC_MULTIPLES 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'SUBS_SYMBOLIC_MULTIPLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBS_SYMBOLIC_MULTIPLES (TERM OPT_ARGS)
    (COND ((EQUAL TERM 0) '(0 (LIST)))
          (T
           (PROG (ARG_LIST UNSUBS J)
             (SETQ OPT_ARGS (UNION OPT_ARGS NIL))
             (SETQ ARG_LIST (GET_TRIG_ARGUMENTS TERM OPT_ARGS))
             (SETQ ARG_LIST
                     (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                       (SETQ ARG ARG_LIST)
                       (COND ((NULL ARG) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ARG) (SIMP* ARG)) (CAR ARG))
                                        NIL)))
                      LOOPLABEL
                       (SETQ ARG (CDR ARG))
                       (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (ARG) (SIMP* ARG)) (CAR ARG))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ J 0)
             (PROG ()
              WHILELABEL
               (COND ((NOT ARG_LIST) (RETURN NIL)))
               (PROG (X X_NU X_LCM)
                 (SETQ J (PLUS J 1))
                 (SETQ X (CAR ARG_LIST))
                 (SETQ X_LCM (CDR (SETQ X_NU (NUMBERGET X))))
                 (PROG (TAIL)
                   (SETQ TAIL ARG_LIST)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (CDR TAIL)) (RETURN NIL)))
                     (PROG (Y Q Y_DEN)
                       (SETQ Y (CADR TAIL))
                       (SETQ Q (MULTSQ X (INVSQ Y)))
                       (COND
                        ((AND (ATOM (CAR Q)) (ATOM (CDR Q)))
                         (PROGN
                          (SETQ Y_DEN (INTEGER_CONTENT (CDR Y)))
                          (SETQ X_LCM
                                  (QUOTIENT (TIMES X_LCM Y_DEN)
                                            (GCDN X_LCM Y_DEN)))
                          (SETCDR TAIL (CDDR TAIL))))
                        (T (SETQ TAIL (CDR TAIL)))))
                     (GO WHILELABEL)))
                 (SETQ ARG_LIST (CDR ARG_LIST))
                 (COND
                  ((NEQ X_LCM 1)
                   (PROGN
                    (SETQ X (MULTSQ X (INVSQ X_NU)))
                    (COND
                     ((NOT (OR (ATOM (CAR X)) (ATOM (CAR (CAR X)))))
                      (PROGN
                       (SETQ X (*Q2A1 X *NOSQ))
                       (SETQ DEPL*
                               (APPEND DEPL*
                                       (SUBLIS
                                        (LIST
                                         (CONS (REVAL1 X T)
                                               (LIST 'AUXILIARY_SYMBOLIC_VAR*
                                                     J)))
                                        DEPL*)))
                       (SETQ TERM
                               (AEVAL*
                                (LIST 'WHEREEXP
                                      (LIST 'LIST
                                            (LIST 'REPLACEBY X
                                                  (LIST 'TIMES
                                                        (LIST
                                                         'AUXILIARY_SYMBOLIC_VAR*
                                                         J)
                                                        X_LCM)))
                                      TERM)))
                       (SETQ UNSUBS
                               (CONS
                                (AEVAL*
                                 (LIST 'EQUAL (LIST 'AUXILIARY_SYMBOLIC_VAR* J)
                                       (LIST 'QUOTIENT X X_LCM)))
                                UNSUBS)))))
                    NIL))))
               (GO WHILELABEL))
             (RETURN (LIST TERM (CONS 'LIST UNSUBS))))))) 
(PUT 'BEHANDLE 'NUMBER-OF-ARGS 1) 
(PUT 'BEHANDLE 'DEFINED-ON-LINE '301) 
(PUT 'BEHANDLE 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'BEHANDLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BEHANDLE (EX)
    (PROG (N D)
      (SETQ EX (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'POW2QUOT*) EX)))
      (SETQ EX (SIMP* EX))
      (SETQ N (MK*SQ (CONS (CAR EX) 1)))
      (SETQ D (MK*SQ (CONS (CDR EX) 1)))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT (LIST 'WHEREEXP (LIST 'LIST 'EXP2TRIG1*) N)
              (LIST 'WHEREEXP (LIST 'LIST 'EXP2TRIG2*) D)))))) 
(PUT 'GET_TRIG_ARGUMENTS 'NUMBER-OF-ARGS 2) 
(PUT 'GET_TRIG_ARGUMENTS 'DEFINED-ON-LINE '317) 
(PUT 'GET_TRIG_ARGUMENTS 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'GET_TRIG_ARGUMENTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_TRIG_ARGUMENTS (TERM ARGS)
    (COND ((ATOM TERM) ARGS)
          (T
           (PROG (F R)
             (SETQ F (CAR TERM))
             (COND
              ((EQUAL F '*SQ)
               (PROGN (SETQ TERM (REVAL1 TERM T)) (SETQ F (CAR TERM)))))
             (SETQ R (CDR TERM))
             (COND
              ((MEMQ F '(SIN COS SINH COSH))
               (RETURN
                (COND ((NOT (MEMBER (SETQ R (CAR R)) ARGS)) (CONS R ARGS))
                      (T ARGS)))))
             (PROG (J)
               (SETQ J R)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J) (SETQ ARGS (GET_TRIG_ARGUMENTS J ARGS))) (CAR J))
               (SETQ J (CDR J))
               (GO LAB))
             (RETURN ARGS))))) 
(PUT 'NUMBERGET 'SIMPFN 'NUMBERGET-SIMPFN) 
(PUT 'NUMBERGET-SIMPFN 'NUMBER-OF-ARGS 1) 
(PUT 'NUMBERGET-SIMPFN 'DEFINED-ON-LINE '338) 
(PUT 'NUMBERGET-SIMPFN 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'NUMBERGET-SIMPFN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMBERGET-SIMPFN (P) (NUMBERGET (SIMP* (CAR P)))) 
(PUT 'NUMBERGET 'NUMBER-OF-ARGS 1) 
(PUT 'NUMBERGET 'DEFINED-ON-LINE '344) 
(PUT 'NUMBERGET 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'NUMBERGET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMBERGET (P)
    (PROG (N D G)
      (SETQ N (INTEGER_CONTENT (CAR P)))
      (SETQ D (INTEGER_CONTENT (CDR P)))
      (SETQ G (GCDN N D))
      (RETURN (CONS (QUOTIENT N G) (QUOTIENT D G))))) 
(PUT 'INTEGER_CONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'INTEGER_CONTENT 'DEFINED-ON-LINE '359) 
(PUT 'INTEGER_CONTENT 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'INTEGER_CONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTEGER_CONTENT (P)
    (COND ((ATOM P) (OR P 0))
          ((ATOM (CDR P))
           (COND ((CDR P) (GCDN (INTEGER_CONTENT (CDAR P)) (CDR P)))
                 (T (INTEGER_CONTENT (CDAR P)))))
          (T
           (INTEGER_CONTENT1 (CDR (CDR P))
            (GCDN (INTEGER_CONTENT (CDAR P))
                  (INTEGER_CONTENT (CDAR (CDR P)))))))) 
(PUT 'INTEGER_CONTENT1 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGER_CONTENT1 'DEFINED-ON-LINE '370) 
(PUT 'INTEGER_CONTENT1 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'INTEGER_CONTENT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGER_CONTENT1 (P A)
    (COND ((EQUAL A 1) 1) ((ATOM P) (COND (P (GCDN P A)) (T A)))
          (T
           (INTEGER_CONTENT1 (CDR P)
            (GCDN (REMAINDER (INTEGER_CONTENT (CDAR P)) A) A))))) 
(FLAG '(DEGREE) 'OPFN) 
(PUT 'DEGREE 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREE 'DEFINED-ON-LINE '383) 
(PUT 'DEGREE 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'DEGREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREE (P X)
    (COND
     ((CAR (SETQ P (SIMP* P)))
      (DIFFERENCE (NUMRDEG (CAR P) X) (NUMRDEG (CDR P) X)))
     (T 'INF))) 
(PUT 'BALANCED 'NUMBER-OF-ARGS 2) 
(PUT 'BALANCED 'DEFINED-ON-LINE '389) 
(PUT 'BALANCED 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'BALANCED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BALANCED (P X)
    (PROGN
     (SETQ P (SIMP* P))
     (EQUAL (NUMRDEG (CAR P) X) (TIMES 2 (NUMRDEG (CDR P) X))))) 
(PUT 'COORDINATED 'NUMBER-OF-ARGS 2) 
(PUT 'COORDINATED 'DEFINED-ON-LINE '396) 
(PUT 'COORDINATED 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'COORDINATED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COORDINATED (P X)
    (PROG (KORD* EVENDEG COORD)
      (SETQ KORD* (LIST (SETQ X (*A2K X))))
      (SETQ P (REORDER (CAR (SIMP* P))))
      (COND
       ((OR (OR (ATOM P) (ATOM (CAR P))) (NOT (EQ (CAAAR P) X))) (RETURN T)))
      (SETQ EVENDEG (EQUAL (REMAINDER (CDAAR P) 2) 0))
      (SETQ P (CDR P))
      (SETQ COORD T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P COORD)) (RETURN NIL)))
        (COND
         ((OR (OR (ATOM P) (ATOM (CAR P))) (NOT (EQ (CAAAR P) X)))
          (PROGN (SETQ COORD (EQ EVENDEG T)) (SETQ P NIL)))
         (T
          (PROGN
           (SETQ COORD (EQ EVENDEG (EQUAL (REMAINDER (CDAAR P) 2) 0)))
           (SETQ P (CDR P)))))
        (GO WHILELABEL))
      (RETURN COORD))) 
(FLAG '(BALANCED COORDINATED) 'BOOLEAN) 
(PUT 'TRIG2ORD 'NUMBER-OF-ARGS 3) 
(FLAG '(TRIG2ORD) 'OPFN) 
(PUT 'TRIG2ORD 'DEFINED-ON-LINE '419) 
(PUT 'TRIG2ORD 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIG2ORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIG2ORD (P X Y)
    (COND
     ((OR (NOT (BALANCED (REVALX P) (REVALX X)))
          (NOT (BALANCED (REVALX P) (REVALX Y))))
      (AEVAL (REDERR (REVALX "trig2ord error: polynomial not balanced."))))
     ((OR (NOT (COORDINATED (REVALX P) (REVALX X)))
          (NOT (COORDINATED (REVALX P) (REVALX Y))))
      (AEVAL (REDERR (REVALX "trig2ord error: polynomial not coordinated."))))
     (T
      (AEVAL
       (LIST 'SUB (LIST 'EQUAL X (LIST 'SQRT X)) (LIST 'EQUAL Y (LIST 'SQRT Y))
             (LIST 'TIMES (LIST 'EXPT X (LIST 'DEGREE P X))
                   (LIST 'EXPT Y (LIST 'DEGREE P Y)) P)))))) 
(PUT 'ORD2TRIG 'NUMBER-OF-ARGS 3) 
(FLAG '(ORD2TRIG) 'OPFN) 
(PUT 'ORD2TRIG 'DEFINED-ON-LINE '426) 
(PUT 'ORD2TRIG 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'ORD2TRIG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ORD2TRIG (P X Y)
    (LIST 'TIMES (LIST 'EXPT X (LIST 'MINUS (LIST 'DEGREE P X)))
          (LIST 'EXPT Y (LIST 'MINUS (LIST 'DEGREE P Y)))
          (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT X 2))
                (LIST 'EQUAL Y (LIST 'EXPT Y 2)) P))) 
(PUT 'SUBPOLY2TRIG 'NUMBER-OF-ARGS 2) 
(FLAG '(SUBPOLY2TRIG) 'OPFN) 
(PUT 'SUBPOLY2TRIG 'DEFINED-ON-LINE '429) 
(PUT 'SUBPOLY2TRIG 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'SUBPOLY2TRIG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBPOLY2TRIG (P X)
    (PROG (R D)
      (SETQ D (AEVAL (LIST 'DEGREE (LIST 'DEN P) X)))
      (SETQ R
              (AEVAL
               (LIST 'SUB
                     (LIST 'EQUAL X
                           (LIST 'PLUS (LIST 'COS X)
                                 (LIST 'TIMES 'I (LIST 'SIN X))))
                     (LIST 'TIMES P (LIST 'EXPT X D)))))
      (RETURN
       (AEVAL
        (LIST 'TIMES R
              (LIST 'EXPT
                    (LIST 'DIFFERENCE (LIST 'COS X)
                          (LIST 'TIMES 'I (LIST 'SIN X)))
                    D)))))) 
(PUT 'SUBPOLY2HYP 'NUMBER-OF-ARGS 2) 
(FLAG '(SUBPOLY2HYP) 'OPFN) 
(PUT 'SUBPOLY2HYP 'DEFINED-ON-LINE '436) 
(PUT 'SUBPOLY2HYP 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'SUBPOLY2HYP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBPOLY2HYP (P X)
    (PROG (R D)
      (SETQ D (AEVAL (LIST 'DEGREE (LIST 'DEN P) X)))
      (SETQ R
              (AEVAL
               (LIST 'SUB
                     (LIST 'EQUAL X (LIST 'PLUS (LIST 'COSH X) (LIST 'SINH X)))
                     (LIST 'TIMES P (LIST 'EXPT X D)))))
      (RETURN
       (AEVAL
        (LIST 'TIMES R
              (LIST 'EXPT (LIST 'DIFFERENCE (LIST 'COSH X) (LIST 'SINH X))
                    D)))))) 
(PUT 'VARGET 'NUMBER-OF-ARGS 1) 
(FLAG '(VARGET) 'OPFN) 
(PUT 'VARGET 'DEFINED-ON-LINE '443) 
(PUT 'VARGET 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'VARGET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARGET (P)
    (PROG (VAR)
      (COND
       ((NOT (BOOLVALUE* (SETQ VAR (REVALX (LIST 'MAINVAR (LIST 'NUM P))))))
        (AEVAL
         (REDERR (REVALX "TrigGCD/Factorize error: no variable specified.")))))
      (COND
       ((NOT (EVALNUMBERP (AEVAL (LIST 'QUOTIENT P VAR))))
        (AEVAL
         (REDERR
          (REVALX
           "TrigGCD/Factorize error: last arg must be [number*]variable.")))))
      (RETURN (AEVAL VAR)))) 
(PUT 'TRIGARGCHECK 'NUMBER-OF-ARGS 3) 
(PUT 'TRIGARGCHECK 'DEFINED-ON-LINE '455) 
(PUT 'TRIGARGCHECK 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGARGCHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIGARGCHECK (P VAR NU)
    (PROG (DF_ARG_VAR)
      (PROG (ARG)
        (SETQ ARG (GET_TRIG_ARGUMENTS P NIL))
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (COND
            ((AND (BOOLVALUE* (SETQ DF_ARG_VAR (REVALX (LIST 'DF ARG VAR))))
                  (NOT (FIXP (REVALX (LIST 'QUOTIENT DF_ARG_VAR NU)))))
             (AEVAL
              (REDERR
               (REVALX "TrigGCD/Factorize error: basis not possible."))))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB)))) 
(FLAG '(SUB2POLY) 'OPFN) 
(PUT 'SUB2POLY 'NUMBER-OF-ARGS 5) 
(PUT 'SUB2POLY 'DEFINED-ON-LINE '465) 
(PUT 'SUB2POLY 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'SUB2POLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUB2POLY (P VAR NU X Y)
    (PROGN
     (TRIGARGCHECK P VAR NU)
     (SETQ P (TRIGSIMP1 P NIL))
     (SETQ P
             (AEVAL
              (LIST 'SUB
                    (LIST 'EQUAL (LIST 'SIN VAR)
                          (LIST 'SIN (LIST 'QUOTIENT VAR NU)))
                    (LIST 'EQUAL (LIST 'COS VAR)
                          (LIST 'COS (LIST 'QUOTIENT VAR NU)))
                    (LIST 'EQUAL (LIST 'SINH VAR)
                          (LIST 'SINH (LIST 'QUOTIENT VAR NU)))
                    (LIST 'EQUAL (LIST 'COSH VAR)
                          (LIST 'COSH (LIST 'QUOTIENT VAR NU)))
                    P)))
     (SETQ P (TRIGSIMP1 P NIL))
     (AEVAL
      (LIST 'SUB
            (LIST 'EQUAL (LIST 'SIN VAR)
                  (LIST 'QUOTIENT (LIST 'DIFFERENCE X (LIST 'QUOTIENT 1 X))
                        (LIST 'TIMES 2 'I)))
            (LIST 'EQUAL (LIST 'COS VAR)
                  (LIST 'QUOTIENT (LIST 'PLUS X (LIST 'QUOTIENT 1 X)) 2))
            (LIST 'EQUAL (LIST 'SINH VAR)
                  (LIST 'QUOTIENT (LIST 'DIFFERENCE Y (LIST 'QUOTIENT 1 Y)) 2))
            (LIST 'EQUAL (LIST 'COSH VAR)
                  (LIST 'QUOTIENT (LIST 'PLUS Y (LIST 'QUOTIENT 1 Y)) 2))
            P)))) 
(PUT 'TRIGGCD 'NUMBER-OF-ARGS 3) 
(FLAG '(TRIGGCD) 'OPFN) 
(PUT 'TRIGGCD 'DEFINED-ON-LINE '482) 
(PUT 'TRIGGCD 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGGCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIGGCD (P Q X)
    (PROG (NOT_COMPLEX VAR NU F)
      (COND ((SETQ NOT_COMPLEX (NOT *COMPLEX)) (ON (LIST 'COMPLEX))))
      (SETQ VAR (AEVAL (LIST 'VARGET X)))
      (SETQ NU (AEVAL (LIST 'NUMBERGET X)))
      (SETQ P (AEVAL (LIST 'SUB2POLY P VAR NU 'XX_X 'YY_Y)))
      (SETQ Q (AEVAL (LIST 'SUB2POLY Q VAR NU 'XX_X 'YY_Y)))
      (COND
       ((NOT
         (AND (BALANCED (REVALX P) (REVALX 'XX_X))
              (BALANCED (REVALX Q) (REVALX 'XX_X))
              (COORDINATED (REVALX P) (REVALX 'XX_X))
              (COORDINATED (REVALX Q) (REVALX 'XX_X))
              (BALANCED (REVALX P) (REVALX 'YY_Y))
              (BALANCED (REVALX Q) (REVALX 'YY_Y))
              (COORDINATED (REVALX P) (REVALX 'YY_Y))
              (COORDINATED (REVALX Q) (REVALX 'YY_Y))))
        (SETQ F (AEVAL 1)))
       (T
        (PROG (H *NOPOWERS *IFACTOR)
          (SETQ *NOPOWERS T)
          (SETQ P (AEVAL (LIST 'TRIG2ORD P 'XX_X 'YY_Y)))
          (SETQ Q (AEVAL (LIST 'TRIG2ORD Q 'XX_X 'YY_Y)))
          (SETQ H (AEVAL (LIST 'GCD (LIST 'NUM P) (LIST 'NUM Q))))
          (SETQ H
                  (AEVAL
                   (LIST 'QUOTIENT (LIST 'ORD2TRIG H 'XX_X 'YY_Y)
                         (LIST 'LCM (LIST 'DEN P) (LIST 'DEN Q)))))
          (SETQ H (AEVAL (LIST 'SUBPOLY2TRIG H 'XX_X)))
          (SETQ H (AEVAL (LIST 'SUBPOLY2HYP H 'YY_Y)))
          (SETQ H
                  (AEVAL
                   (LIST 'SUB (LIST 'EQUAL 'XX_X (LIST 'TIMES VAR NU))
                         (LIST 'EQUAL 'YY_Y (LIST 'TIMES VAR NU)) H)))
          (SETQ H (AEVAL (TRIGSIMP1 H NIL)))
          (SETQ H (AEVAL (LIST 'FACTORIZE (LIST 'NUM H))))
          (COND
           ((EVALNUMBERP (AEVAL (LIST 'FIRST H)))
            (SETQ H (AEVAL (LIST 'REST H)))))
          (SETQ F
                  (PROG (R FORALL-RESULT)
                    (SETQ R (GETRLIST (AEVAL H)))
                    (SETQ FORALL-RESULT 1)
                   LAB1
                    (COND ((NULL R) (RETURN FORALL-RESULT)))
                    (SETQ FORALL-RESULT
                            (AEVAL*
                             (LIST 'TIMES ((LAMBDA (R) (AEVAL R)) (CAR R))
                                   FORALL-RESULT)))
                    (SETQ R (CDR R))
                    (GO LAB1))))))
      (COND (NOT_COMPLEX (OFF (LIST 'COMPLEX))))
      (RETURN (AEVAL F)))) 
(PUT 'TRIGFACTORIZE 'NUMBER-OF-ARGS 2) 
(FLAG '(TRIGFACTORIZE) 'OPFN) 
(PUT 'TRIGFACTORIZE 'DEFINED-ON-LINE '516) 
(PUT 'TRIGFACTORIZE 'DEFINED-IN-FILE 'TRIGSIMP/TRIGSMP2.RED) 
(PUT 'TRIGFACTORIZE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGFACTORIZE (P X)
    (PROG (NOT_COMPLEX VAR NU Q FACTORS)
      (COND ((SETQ NOT_COMPLEX (NOT *COMPLEX)) (ON (LIST 'COMPLEX))))
      (SETQ VAR (AEVAL (LIST 'VARGET X)))
      (SETQ NU (AEVAL (LIST 'NUMBERGET X)))
      (SETQ Q (AEVAL (LIST 'SUB2POLY P VAR NU 'XX_X 'YY_Y)))
      (COND
       ((NOT
         (AND (BALANCED (REVALX Q) (REVALX 'XX_X))
              (COORDINATED (REVALX Q) (REVALX 'XX_X))
              (BALANCED (REVALX Q) (REVALX 'YY_Y))
              (COORDINATED (REVALX Q) (REVALX 'YY_Y))))
        (SETQ FACTORS
                (COND ((BOOLVALUE* (REVALX *NOPOWERS)) (AEVAL (LIST 'LIST P)))
                      (T (AEVAL (LIST 'LIST (LIST 'LIST P 1)))))))
       (T
        (PROG (POW CONTENT)
          (COND ((BOOLVALUE* (REVALX (NOT *NOPOWERS))) (SETQ POW (AEVAL 1))))
          (SETQ Q (AEVAL (LIST 'TRIG2ORD Q 'XX_X 'YY_Y)))
          (SETQ CONTENT (AEVAL (LIST 'QUOTIENT 1 (LIST 'DEN Q))))
          (SETQ FACTORS (AEVAL (LIST 'LIST)))
          (PROG (FAC)
            (SETQ FAC (GETRLIST (AEVAL (LIST 'FACTORIZE (LIST 'NUM Q)))))
           LAB
            (COND ((NULL FAC) (RETURN NIL)))
            ((LAMBDA (FAC)
               (PROGN
                (COND
                 ((BOOLVALUE* POW)
                  (PROGN
                   (SETQ POW (AEVAL (LIST 'SECOND FAC)))
                   (SETQ FAC (AEVAL (LIST 'FIRST FAC))))))
                (SETQ FAC (AEVAL (LIST 'ORD2TRIG FAC 'XX_X 'YY_Y)))
                (SETQ FAC (AEVAL (LIST 'SUBPOLY2TRIG FAC 'XX_X)))
                (SETQ FAC (AEVAL (LIST 'SUBPOLY2HYP FAC 'YY_Y)))
                (SETQ FAC
                        (AEVAL
                         (LIST 'SUB (LIST 'EQUAL 'XX_X (LIST 'TIMES VAR NU))
                               (LIST 'EQUAL 'YY_Y (LIST 'TIMES VAR NU)) FAC)))
                (SETQ FAC (AEVAL (TRIGSIMP1 FAC NIL)))
                (COND
                 ((FREEOF (REVALX FAC) (REVALX VAR))
                  (SETQ CONTENT
                          (AEVAL
                           (LIST 'TIMES CONTENT
                                 (COND
                                  ((EVALGREATERP (AEVAL POW) 1)
                                   (AEVAL (LIST 'EXPT FAC POW)))
                                  (T (AEVAL FAC)))))))
                 (T
                  (PROG (*NOPOWERS)
                    (PROG (U)
                      (SETQ U (GETRLIST (AEVAL (LIST 'FACTORIZE FAC))))
                     LAB
                      (COND ((NULL U) (RETURN NIL)))
                      ((LAMBDA (U)
                         (COND
                          ((FREEOF (REVALX U) (REVALX VAR))
                           (PROGN
                            (SETQ U
                                    (AEVAL
                                     (LIST 'EXPT (LIST 'FIRST U)
                                           (LIST 'SECOND U))))
                            (SETQ CONTENT
                                    (AEVAL
                                     (LIST 'TIMES CONTENT
                                           (COND
                                            ((EVALGREATERP (AEVAL POW) 1)
                                             (AEVAL (LIST 'EXPT U POW)))
                                            (T (AEVAL U))))))
                            (SETQ FAC (AEVAL (LIST 'QUOTIENT FAC U)))))))
                       (CAR U))
                      (SETQ U (CDR U))
                      (GO LAB))
                    (SETQ FACTORS
                            (AEVAL
                             (LIST 'CONS
                                   (COND
                                    ((BOOLVALUE* POW)
                                     (AEVAL (LIST 'LIST FAC POW)))
                                    (T (AEVAL FAC)))
                                   FACTORS))))))))
             (CAR FAC))
            (SETQ FAC (CDR FAC))
            (GO LAB))
          (COND
           ((EVALNEQ (AEVAL CONTENT) 1)
            (SETQ FACTORS
                    (AEVAL
                     (LIST 'CONS
                           (COND
                            ((BOOLVALUE* (REVALX *NOPOWERS)) (AEVAL CONTENT))
                            (T (AEVAL (LIST 'LIST CONTENT 1))))
                           FACTORS))))))))
      (COND (NOT_COMPLEX (OFF (LIST 'COMPLEX))))
      (RETURN (AEVAL FACTORS)))) 
(ENDMODULE) 