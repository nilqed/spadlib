(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPLEX)) 
(COND
 ((NOT (GET 'LEQ 'SIMPFN))
  (PROGN (AEVAL (OPERATOR (LIST 'LEQ))) (AEVAL (OPERATOR (LIST 'GEQ))) NIL))) 
(DE SMPLX_PREPSQ (U)
    (COND ((ATOM U) U) ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
          ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '*SQ))
           (LIST 'MINUS (CAR (CADADR U))))
          ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '|:RD:|)) U)
          ((EQUAL (CAR U) '|:RD:|) U) ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U))))) 
(PUT 'SMPLX_PREPSQ 'NUMBER-OF-ARGS 1) 
(PUT 'SMPLX_PREPSQ 'DEFINED-ON-LINE '53) 
(PUT 'SMPLX_PREPSQ 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SMPLX_PREPSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SMPLX_PREPSQ 'INLINE
      '(LAMBDA (U)
         (COND ((ATOM U) U) ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
               ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '*SQ))
                (LIST 'MINUS (CAR (CADADR U))))
               ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '|:RD:|)) U)
               ((EQUAL (CAR U) '|:RD:|) U)
               ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))) 
(DE FAST_ROW_DIM (IN_MAT) (IDIFFERENCE (LENGTH IN_MAT) 1)) 
(PUT 'FAST_ROW_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'FAST_ROW_DIM 'DEFINED-ON-LINE '66) 
(PUT 'FAST_ROW_DIM 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_ROW_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FAST_ROW_DIM 'INLINE '(LAMBDA (IN_MAT) (IDIFFERENCE (LENGTH IN_MAT) 1))) 
(DE FAST_COLUMN_DIM (IN_MAT) (LENGTH (CADR IN_MAT))) 
(PUT 'FAST_COLUMN_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'FAST_COLUMN_DIM 'DEFINED-ON-LINE '74) 
(PUT 'FAST_COLUMN_DIM 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_COLUMN_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FAST_COLUMN_DIM 'INLINE '(LAMBDA (IN_MAT) (LENGTH (CADR IN_MAT)))) 
(DE FAST_STACK_ROWS (IN_MAT ROW_LIST)
    (CONS 'MAT (LIST (NTH (CDR IN_MAT) ROW_LIST)))) 
(PUT 'FAST_STACK_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'FAST_STACK_ROWS 'DEFINED-ON-LINE '82) 
(PUT 'FAST_STACK_ROWS 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_STACK_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'FAST_STACK_ROWS 'INLINE
      '(LAMBDA (IN_MAT ROW_LIST)
         (CONS 'MAT (LIST (NTH (CDR IN_MAT) ROW_LIST))))) 
(DE FAST_GETMAT (MATRI I J) (FAST_UNCHECKED_GETMATELEM (LIST MATRI I J))) 
(PUT 'FAST_GETMAT 'NUMBER-OF-ARGS 3) 
(PUT 'FAST_GETMAT 'DEFINED-ON-LINE '91) 
(PUT 'FAST_GETMAT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_GETMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FAST_GETMAT 'INLINE
      '(LAMBDA (MATRI I J) (FAST_UNCHECKED_GETMATELEM (LIST MATRI I J)))) 
(DE FAST_MY_LETMTR (U V Y)
    (RPLACA
     (PNTH
      (NTH (CDR Y)
           (CAR
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR U))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (COND ((FIXP J) J) (T (REVAL1 J T))))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                        (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
      (CADR
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J (CDR U))
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                           (CAR J))
                          NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T)))) (CAR J))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     V)) 
(PUT 'FAST_MY_LETMTR 'NUMBER-OF-ARGS 3) 
(PUT 'FAST_MY_LETMTR 'DEFINED-ON-LINE '99) 
(PUT 'FAST_MY_LETMTR 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_MY_LETMTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FAST_MY_LETMTR 'INLINE
      '(LAMBDA (U V Y)
         (RPLACA
          (PNTH
           (NTH (CDR Y)
                (CAR
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (CDR U))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (COND ((FIXP J) J) (T (REVAL1 J T))))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
           (CADR
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR U))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (COND ((FIXP J) J) (T (REVAL1 J T))))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                        (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
          V))) 
(SWITCH (LIST 'FASTSIMPLEX)) 
(ON1 'FASTSIMPLEX) 
(PUT 'SIMPLEX 'PSOPFN 'SIMPLEX0) 
(PUT 'SIMPLEX0 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLEX0 'DEFINED-ON-LINE '108) 
(PUT 'SIMPLEX0 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SIMPLEX0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLEX0 (INPUT)
    (COND (*FASTSIMPLEX (FS_SIMPLEX1 INPUT)) (T (SIMPLEX1 INPUT)))) 
(PUT 'SIMPLEX1 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLEX1 'DEFINED-ON-LINE '112) 
(PUT 'SIMPLEX1 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SIMPLEX1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLEX1 (INPUT)
    (PROG (MAX_OR_MIN OBJECTIVE EQUATION_LIST TMP A B OBJ_MAT X A1 PHASE1_OBJ
           IB XB BINV SIMP_CALC PHASE1_OBJ_VALUE BIG SUM STOP WORK
           I_TURNED_ROUNDED_ON ANS_LIST OPTIMAL_VALUE M N K I ELL NO_COEFFS
           NO_VARIABLES EQUATION_VARIABLES)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ K 0)
      (SETQ I 0)
      (SETQ ELL 0)
      (SETQ NO_COEFFS 0)
      (SETQ NO_VARIABLES 0)
      (SETQ EQUATION_VARIABLES 0)
      (SETQ MAX_OR_MIN (REVAL1 (CAR INPUT) T))
      (SETQ OBJECTIVE (REVAL1 (CADR INPUT) T))
      (SETQ EQUATION_LIST (NORMALIZE-EQUATIONL (REVAL1 (CADDR INPUT) T)))
      (COND
       ((NOT *ROUNDED)
        (PROGN (SETQ I_TURNED_ROUNDED_ON T) (ON (LIST 'ROUNDED)) NIL)))
      (COND
       ((AND (NEQ MAX_OR_MIN 'MAX) (NEQ MAX_OR_MIN 'MIN))
        (REDERR
         "Error in simplex(first argument): must be either max or min.")))
      (COND
       ((AND (PAIRP EQUATION_LIST) (EQUAL (CAR EQUATION_LIST) 'LIST))
        (SETQ EQUATION_LIST (CDR EQUATION_LIST)))
       (T (REDERR "Error in simplex(third argument): must be a list.")))
      (SETQ TMP (UNIQUE_EQUATION_LIST EQUATION_LIST))
      (SETQ EQUATION_LIST (CAR TMP))
      (SETQ EQUATION_VARIABLES (CADR TMP))
      (SETQ EQUATION_LIST (MAKE_EQUATIONS_POSITIVE EQUATION_LIST))
      (SETQ EQUATION_LIST
              (ADD_NOT_DEFINED_VARIABLES OBJECTIVE EQUATION_LIST
               EQUATION_VARIABLES))
      (SETQ TMP (INITIALISE MAX_OR_MIN OBJECTIVE EQUATION_LIST))
      (SETQ A (CAR TMP))
      (SETQ B (CADR TMP))
      (SETQ OBJ_MAT (CADDR TMP))
      (SETQ X (CADDDR TMP))
      (SETQ NO_VARIABLES (CAR (CDDDDR TMP)))
      (SETQ TMP (CHECK_MINUS_B A B))
      (SETQ A (CAR TMP))
      (SETQ B (CADR TMP))
      (SETQ M (IDIFFERENCE (LENGTH A) 1))
      (SETQ N (SETQ NO_COEFFS (LENGTH (CADR A))))
      (SETQ TMP (CREATE_PHASE1_A1_AND_OBJ_AND_IB A))
      (SETQ A1 (CAR TMP))
      (SETQ PHASE1_OBJ (CADR TMP))
      (SETQ IB (CADDR TMP))
      (SETQ XB (COPY_MAT B))
      (SETQ BINV (FAST_MAKE_IDENTITY (IDIFFERENCE (LENGTH A) 1)))
      (SETQ SIMP_CALC (SIMPLEX_CALCULATION PHASE1_OBJ A1 B IB BINV XB))
      (SETQ PHASE1_OBJ_VALUE (CAR SIMP_CALC))
      (SETQ XB (CADR SIMP_CALC))
      (SETQ BINV (CADDDR SIMP_CALC))
      (COND
       ((NEQ (GET_NUM_PART PHASE1_OBJ_VALUE) 0)
        (REDERR "Error in simplex: Problem has no feasible solution.")))
      (PROG (ELL)
        (SETQ ELL 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M ELL)) (RETURN NIL)))
        (COND ((LEQ (NTH IB ELL) N) (PROGN NIL))
              (T
               (PROGN
                (SETQ BIG (MINUS 1))
                (SETQ K 0)
                (SETQ STOP NIL)
                (SETQ I 1)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND (LEQ I N) (NOT STOP))) (RETURN NIL)))
                  (PROGN
                   (SETQ SUM
                           (GET_NUM_PART
                            ((LAMBDA (U)
                               (COND ((ATOM U) U)
                                     ((AND (EQUAL (CAR U) 'MINUS)
                                           (ATOM (CADR U)))
                                      U)
                                     ((AND (EQUAL (CAR U) 'MINUS)
                                           (EQUAL (CAADR U) '*SQ))
                                      (LIST 'MINUS (CAR (CADADR U))))
                                     ((AND (EQUAL (CAR U) 'MINUS)
                                           (EQUAL (CAADR U) '|:RD:|))
                                      U)
                                     ((EQUAL (CAR U) '|:RD:|) U)
                                     ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                             (FAST_UNCHECKED_GETMATELEM
                              (LIST
                               (REVAL1
                                (LIST 'TIMES
                                      (CONS 'MAT (LIST (NTH (CDR BINV) ELL)))
                                      (FAST_AUGMENT_COLUMNS A I))
                                T)
                               1 1)))))
                   (COND ((LEQ (ABS SUM) BIG) (SETQ STOP T))
                         (T (PROGN (SETQ BIG (ABS SUM)) (SETQ K I) NIL)))
                   (SETQ I (PLUS I 1))
                   NIL)
                  (GO WHILELABEL))
                (COND ((GEQ BIG 0) (PROGN NIL))
                      (T
                       (REDERR
                        (LIST "Error in simplex: constraint" K
                              " is redundant."))))
                (SETQ WORK (FAST_AUGMENT_COLUMNS A K))
                (SETQ BINV (PHIPRM BINV WORK ELL))
                (SETCAR (PNTH IB ELL) K)
                NIL)))
        (SETQ ELL (PLUS2 ELL 1))
        (GO LAB))
      (SETQ SIMP_CALC (SIMPLEX_CALCULATION OBJ_MAT A B IB BINV XB))
      (SETQ OPTIMAL_VALUE (CAR SIMP_CALC))
      (SETQ XB (CADR SIMP_CALC))
      (SETQ IB (CADDR SIMP_CALC))
      (SETQ ANS_LIST (MAKE_ANSWER_LIST XB IB NO_COEFFS X NO_VARIABLES))
      (COND (I_TURNED_ROUNDED_ON (OFF (LIST 'ROUNDED))))
      (COND
       ((EQUAL MAX_OR_MIN 'MAX)
        (SETQ OPTIMAL_VALUE
                ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                 (LIST 'MINUS OPTIMAL_VALUE)))))
      (RETURN (LIST 'LIST OPTIMAL_VALUE (CONS 'LIST ANS_LIST))))) 
(FLAG '(SIMPLEX1) 'OPFN) 
(PUT 'NORMALIZE-EQUATIONL 'NUMBER-OF-ARGS 1) 
(PUT 'NORMALIZE-EQUATIONL 'DEFINED-ON-LINE '285) 
(PUT 'NORMALIZE-EQUATIONL 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'NORMALIZE-EQUATIONL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMALIZE-EQUATIONL (EQL)
    (COND
     ((EQCAR EQL 'LIST)
      (CONS 'LIST
            (PROG (EQU FORALL-RESULT FORALL-ENDPTR)
              (SETQ EQU (CDR EQL))
              (COND ((NULL EQU) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EQU) (NORMALIZE-EQUATION EQU))
                                (CAR EQU))
                               NIL)))
             LOOPLABEL
              (SETQ EQU (CDR EQU))
              (COND ((NULL EQU) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (EQU) (NORMALIZE-EQUATION EQU)) (CAR EQU))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T EQL))) 
(PUT 'NORMALIZE-EQUATION 'NUMBER-OF-ARGS 1) 
(PUT 'NORMALIZE-EQUATION 'DEFINED-ON-LINE '290) 
(PUT 'NORMALIZE-EQUATION 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'NORMALIZE-EQUATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMALIZE-EQUATION (EQU)
    (PROG (LHS B)
      (SETQ LHS (CAR (ADDSQ (SIMP (CADR EQU)) (NEGSQ (SIMP (CADDR EQU))))))
      (SETQ B (NEGF (ABSSUMMAND LHS)))
      (RETURN (LIST (CAR EQU) (PREPF (ADDF LHS B)) (PREPF B))))) 
(PUT 'ABSSUMMAND 'NUMBER-OF-ARGS 1) 
(PUT 'ABSSUMMAND 'DEFINED-ON-LINE '297) 
(PUT 'ABSSUMMAND 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'ABSSUMMAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ABSSUMMAND (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) F) ((CDR F) (ABSSUMMAND (CDR F))))) 
(PUT 'UNIQUE_EQUATION_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUE_EQUATION_LIST 'DEFINED-ON-LINE '300) 
(PUT 'UNIQUE_EQUATION_LIST 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'UNIQUE_EQUATION_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUE_EQUATION_LIST (EQUATION_LIST)
    (PROG (UNIQUE_EQUATION_LIST COEFF_LIST)
      (PROG (EQUATION)
        (SETQ EQUATION EQUATION_LIST)
       LAB
        (COND ((NULL EQUATION) (RETURN NIL)))
        ((LAMBDA (EQUATION)
           (PROGN
            (COND
             ((NOT (INTERSECTION (LIST EQUATION) UNIQUE_EQUATION_LIST))
              (PROGN
               (SETQ UNIQUE_EQUATION_LIST
                       (APPEND UNIQUE_EQUATION_LIST (LIST EQUATION)))
               (SETQ COEFF_LIST
                       (UNION COEFF_LIST (GET_COEFFS (CADR EQUATION))))
               NIL)))
            NIL))
         (CAR EQUATION))
        (SETQ EQUATION (CDR EQUATION))
        (GO LAB))
      (RETURN (LIST UNIQUE_EQUATION_LIST COEFF_LIST)))) 
(PUT 'SIMPLEX_GET_INVERSE_COMPOP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLEX_GET_INVERSE_COMPOP 'DEFINED-ON-LINE '319) 
(PUT 'SIMPLEX_GET_INVERSE_COMPOP 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SIMPLEX_GET_INVERSE_COMPOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLEX_GET_INVERSE_COMPOP (U)
    (COND ((EQ U 'GEQ) 'LEQ) ((EQ U 'LEQ) 'GEQ) (T U))) 
(PUT 'MAKE_EQUATIONS_POSITIVE 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_EQUATIONS_POSITIVE 'DEFINED-ON-LINE '325) 
(PUT 'MAKE_EQUATIONS_POSITIVE 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'MAKE_EQUATIONS_POSITIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_EQUATIONS_POSITIVE (EQUATION_LIST)
    (PROG (EQUATION FORALL-RESULT FORALL-ENDPTR)
      (SETQ EQUATION EQUATION_LIST)
      (COND ((NULL EQUATION) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (EQUATION)
                          (COND
                           ((AND (PAIRP (CADR EQUATION))
                                 (EQUAL (CAADR EQUATION) 'MINUS)
                                 (PAIRP (CADDR EQUATION))
                                 (EQUAL (CAADDR EQUATION) 'MINUS))
                            (LIST (SIMPLEX_GET_INVERSE_COMPOP (CAR EQUATION))
                                  (COND
                                   ((ATOM (CADR EQUATION))
                                    (LIST 'MINUS (CADR EQUATION)))
                                   ((EQUAL (CAR (CADR EQUATION)) 'MINUS)
                                    (CADR (CADR EQUATION)))
                                   (T (LIST 'MINUS (CADR EQUATION))))
                                  ((LAMBDA (U)
                                     (COND ((ATOM U) (LIST 'MINUS U))
                                           ((EQUAL (CAR U) 'MINUS) (CADR U))
                                           (T (LIST 'MINUS U))))
                                   (CADDR EQUATION))))
                           (T EQUATION)))
                        (CAR EQUATION))
                       NIL)))
     LOOPLABEL
      (SETQ EQUATION (CDR EQUATION))
      (COND ((NULL EQUATION) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (EQUATION)
                  (COND
                   ((AND (PAIRP (CADR EQUATION))
                         (EQUAL (CAADR EQUATION) 'MINUS)
                         (PAIRP (CADDR EQUATION))
                         (EQUAL (CAADDR EQUATION) 'MINUS))
                    (LIST (SIMPLEX_GET_INVERSE_COMPOP (CAR EQUATION))
                          (COND
                           ((ATOM (CADR EQUATION))
                            (LIST 'MINUS (CADR EQUATION)))
                           ((EQUAL (CAR (CADR EQUATION)) 'MINUS)
                            (CADR (CADR EQUATION)))
                           (T (LIST 'MINUS (CADR EQUATION))))
                          ((LAMBDA (U)
                             (COND ((ATOM U) (LIST 'MINUS U))
                                   ((EQUAL (CAR U) 'MINUS) (CADR U))
                                   (T (LIST 'MINUS U))))
                           (CADDR EQUATION))))
                   (T EQUATION)))
                (CAR EQUATION))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'ADD_NOT_DEFINED_VARIABLES 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_NOT_DEFINED_VARIABLES 'DEFINED-ON-LINE '341) 
(PUT 'ADD_NOT_DEFINED_VARIABLES 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'ADD_NOT_DEFINED_VARIABLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_NOT_DEFINED_VARIABLES (OBJECTIVE EQUATION_LIST EQUATION_VARIABLES)
    (PROG (OBJ_VARIABLES)
      (SETQ OBJ_VARIABLES (GET_COEFFS OBJECTIVE))
      (COND
       ((EQUAL (LENGTH OBJ_VARIABLES) (LENGTH EQUATION_VARIABLES))
        (RETURN EQUATION_LIST)))
      (PROG (VARIABLE)
        (SETQ VARIABLE OBJ_VARIABLES)
       LAB
        (COND ((NULL VARIABLE) (RETURN NIL)))
        ((LAMBDA (VARIABLE)
           (PROGN
            (COND
             ((NOT (INTERSECTION (LIST VARIABLE) EQUATION_VARIABLES))
              (PROGN
               (PRIN2 "*** Warning: variable ")
               (PRIN2 VARIABLE)
               (PRIN2T " not defined in input. Has been defined as >=0.")
               (SETQ EQUATION_LIST
                       (APPEND EQUATION_LIST (LIST (LIST 'GEQ VARIABLE 0))))
               NIL)))
            NIL))
         (CAR VARIABLE))
        (SETQ VARIABLE (CDR VARIABLE))
        (GO LAB))
      (RETURN EQUATION_LIST))) 
(PUT 'INITIALISE 'NUMBER-OF-ARGS 3) 
(PUT 'INITIALISE 'DEFINED-ON-LINE '368) 
(PUT 'INITIALISE 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'INITIALISE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INITIALISE (MAX_OR_MIN OBJECTIVE EQUATION_LIST)
    (PROG (MORE_INIT A B OBJ_MAT X NO_VARIABLES)
      (SETQ NO_VARIABLES 0)
      (COND
       ((EQUAL MAX_OR_MIN 'MAX)
        (SETQ OBJECTIVE (REVAL1 (LIST 'TIMES OBJECTIVE (MINUS 1)) T))))
      (SETQ MORE_INIT (MORE_INITIALISE OBJECTIVE EQUATION_LIST))
      (SETQ A (CAR MORE_INIT))
      (SETQ B (CADR MORE_INIT))
      (SETQ OBJ_MAT (CADDR MORE_INIT))
      (SETQ X (CADDDR MORE_INIT))
      (SETQ NO_VARIABLES (CAR (CDDDDR MORE_INIT)))
      (RETURN (LIST A B OBJ_MAT X NO_VARIABLES)))) 
(PUT 'MORE_INITIALISE 'NUMBER-OF-ARGS 2) 
(PUT 'MORE_INITIALISE 'DEFINED-ON-LINE '392) 
(PUT 'MORE_INITIALISE 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'MORE_INITIALISE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MORE_INITIALISE (OBJECTIVE EQUATION_LIST)
    (PROG (NON_SLACK_VARIABLE_LIST OBJ_MAT NO_OF_NON_SLACKS TMP VARIABLE_LIST
           SLACK_EQUATIONS A B X)
      (SETQ NON_SLACK_VARIABLE_LIST
              (GET_PRELIMINARY_VARIABLE_LIST EQUATION_LIST))
      (SETQ NO_OF_NON_SLACKS (LENGTH NON_SLACK_VARIABLE_LIST))
      (SETQ TMP (ADD_SLACKS_TO_EQUATIONS EQUATION_LIST))
      (SETQ SLACK_EQUATIONS (CAR TMP))
      (SETQ B (CADR TMP))
      (SETQ VARIABLE_LIST (UNION NON_SLACK_VARIABLE_LIST (CADDR TMP)))
      (SETQ TMP (GET_X_AND_OBJ_MAT OBJECTIVE VARIABLE_LIST))
      (SETQ X (CAR TMP))
      (SETQ OBJ_MAT (CADR TMP))
      (SETQ A (SIMP_GET_A SLACK_EQUATIONS VARIABLE_LIST))
      (RETURN (LIST A B OBJ_MAT X NO_OF_NON_SLACKS)))) 
(PUT 'CHECK_MINUS_B 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_MINUS_B 'DEFINED-ON-LINE '412) 
(PUT 'CHECK_MINUS_B 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'CHECK_MINUS_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_MINUS_B (A B)
    (PROG ()
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (ROW_DIM B) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((LESSP (GET_NUM_PART (REVAL1 (GETMAT B I 1) T)) 0)
           (PROGN
            (SETQ B (MULT_ROWS B I (MINUS 1)))
            (SETQ A (MULT_ROWS A I (MINUS 1)))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST A B)))) 
(PUT 'CREATE_PHASE1_A1_AND_OBJ_AND_IB 'NUMBER-OF-ARGS 1) 
(PUT 'CREATE_PHASE1_A1_AND_OBJ_AND_IB 'DEFINED-ON-LINE '431) 
(PUT 'CREATE_PHASE1_A1_AND_OBJ_AND_IB 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'CREATE_PHASE1_A1_AND_OBJ_AND_IB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CREATE_PHASE1_A1_AND_OBJ_AND_IB (A)
    (PROG (PHASE1_OBJ A1 IB COLUMN_DIM_A1 COLUMN_DIM_A ROW_DIM_A1 I)
      (SETQ COLUMN_DIM_A1 0)
      (SETQ COLUMN_DIM_A 0)
      (SETQ ROW_DIM_A1 0)
      (SETQ I 0)
      (SETQ COLUMN_DIM_A (LENGTH (CADR A)))
      (SETQ A1
              (FAST_MATRIX_AUGMENT
               (LIST A (FAST_MAKE_IDENTITY (IDIFFERENCE (LENGTH A) 1)))))
      (SETQ COLUMN_DIM_A1 (LENGTH (CADR A1)))
      (SETQ ROW_DIM_A1 (IDIFFERENCE (LENGTH A1) 1))
      (SETQ PHASE1_OBJ (MKMATRIX 1 (LENGTH (CADR A1))))
      (PROG (I)
        (SETQ I (PLUS COLUMN_DIM_A 1))
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH (CADR A1)) I)) (RETURN NIL)))
        (FAST_SETMAT PHASE1_OBJ 1 I 1)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ IB
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I (PLUS COLUMN_DIM_A 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CADR A1)) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CADR A1)) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS I NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST A1 PHASE1_OBJ IB)))) 
(PUT 'SIMPLEX_CALCULATION 'NUMBER-OF-ARGS 6) 
(PUT 'SIMPLEX_CALCULATION 'DEFINED-ON-LINE '449) 
(PUT 'SIMPLEX_CALCULATION 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SIMPLEX_CALCULATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLEX_CALCULATION (OBJ_MAT A B IB BINV XB)
    (PROG (RS1 SB RS2 RS3 U CONTINUE OBJ_VALUE K ITER ELL)
      (SETQ K 0)
      (SETQ ITER 0)
      (SETQ ELL 0)
      (SETQ OBJ_VALUE (COMPUTE_OBJECTIVE (GET_CB OBJ_MAT IB) XB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CONTINUE 'OPTIMAL)) (RETURN NIL)))
        (PROGN
         (SETQ RS1 (RSTEP1 A OBJ_MAT BINV IB))
         (SETQ SB (CAR RS1))
         (SETQ K (CADR RS1))
         (SETQ U (CADDR RS1))
         (SETQ CONTINUE (CADDDR RS1))
         (COND
          ((NEQ CONTINUE 'OPTIMAL)
           (PROGN
            (SETQ RS2 (RSTEP2 XB SB))
            (SETQ ELL (CADR RS2))
            (SETQ RS3 (RSTEP3 XB OBJ_MAT B BINV A IB K ELL))
            (SETQ ITER (PLUS ITER 1))
            (SETQ BINV (CAR RS3))
            (SETQ OBJ_VALUE (CADR RS3))
            (SETQ XB (CADDR RS3))
            NIL)))
         NIL)
        (GO WHILELABEL))
      (RETURN (LIST OBJ_VALUE XB IB BINV)))) 
(PUT 'GET_PRELIMINARY_VARIABLE_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'GET_PRELIMINARY_VARIABLE_LIST 'DEFINED-ON-LINE '480) 
(PUT 'GET_PRELIMINARY_VARIABLE_LIST 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'GET_PRELIMINARY_VARIABLE_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_PRELIMINARY_VARIABLE_LIST (EQUATION_LIST)
    (PROG (VARIABLE_LIST)
      (PROG (EQUATION)
        (SETQ EQUATION EQUATION_LIST)
       LAB
        (COND ((NULL EQUATION) (RETURN NIL)))
        ((LAMBDA (EQUATION)
           (SETQ VARIABLE_LIST
                   (UNION VARIABLE_LIST (GET_COEFFS (CADR EQUATION)))))
         (CAR EQUATION))
        (SETQ EQUATION (CDR EQUATION))
        (GO LAB))
      (RETURN VARIABLE_LIST))) 
(PUT 'ADD_SLACKS_TO_EQUATIONS 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_SLACKS_TO_EQUATIONS 'DEFINED-ON-LINE '493) 
(PUT 'ADD_SLACKS_TO_EQUATIONS 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'ADD_SLACKS_TO_EQUATIONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_SLACKS_TO_EQUATIONS (EQUATION_LIST)
    (PROG (SLACK_LIST RHS_MAT SLACK_VARIABLE SLACK_VARIABLE_LIST I ROW)
      (SETQ I 0)
      (SETQ ROW 0)
      (SETQ RHS_MAT (MKMATRIX (LENGTH EQUATION_LIST) 1))
      (SETQ ROW 1)
      (PROG (EQUATION)
        (SETQ EQUATION EQUATION_LIST)
       LAB
        (COND ((NULL EQUATION) (RETURN NIL)))
        ((LAMBDA (EQUATION)
           (PROGN
            (COND
             ((NOT (NUMBERP (REVAL1 (CADDR EQUATION) T)))
              (PROGN
               (PRIN2 "***** Error in simplex(third argument): ")
               (REDERR "right hand side of each inequality must be a number")
               NIL))
             (T (FAST_SETMAT RHS_MAT ROW 1 (CADDR EQUATION))))
            (SETQ ROW (PLUS ROW 1))
            (COND
             ((EQUAL (CAR EQUATION) 'GEQ)
              (PROGN
               (SETQ I (PLUS I 1))
               (SETQ SLACK_VARIABLE (MKID 'SL_VAR I))
               (SETQ EQUATION
                       (LIST 'PLUS (LIST 'MINUS (MKID 'SL_VAR I))
                             (CADR EQUATION)))
               (SETQ SLACK_VARIABLE_LIST
                       (APPEND SLACK_VARIABLE_LIST (LIST SLACK_VARIABLE)))
               NIL))
             ((EQUAL (CAR EQUATION) 'LEQ)
              (PROGN
               (SETQ I (PLUS I 1))
               (SETQ SLACK_VARIABLE (MKID 'SL_VAR I))
               (SETQ EQUATION (LIST 'PLUS (MKID 'SL_VAR I) (CADR EQUATION)))
               (SETQ SLACK_VARIABLE_LIST
                       (APPEND SLACK_VARIABLE_LIST (LIST SLACK_VARIABLE)))
               NIL))
             ((EQUAL (CAR EQUATION) 'EQUAL) (SETQ EQUATION (CADR EQUATION)))
             (T
              (PROGN
               (PRIN2 "***** Error in simplex(third argument):")
               (REDERR "inequalities must contain either >=, <=, or =.")
               NIL)))
            (SETQ SLACK_LIST (APPEND SLACK_LIST (LIST EQUATION)))
            NIL))
         (CAR EQUATION))
        (SETQ EQUATION (CDR EQUATION))
        (GO LAB))
      (RETURN (LIST SLACK_LIST RHS_MAT SLACK_VARIABLE_LIST)))) 
(FLAG '(ADD_SLACKS_TO_LIST) 'OPFN) 
(PUT 'SIMP_GET_A 'NUMBER-OF-ARGS 2) 
(PUT 'SIMP_GET_A 'DEFINED-ON-LINE '550) 
(PUT 'SIMP_GET_A 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SIMP_GET_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMP_GET_A (SLACK_EQUATIONS VARIABLE_LIST)
    (PROG (A SLACK_ELT VAR_ELT ROW COL LENGTH_SLACK_EQUATIONS
           LENGTH_VARIABLE_LIST)
      (SETQ ROW 0)
      (SETQ COL 0)
      (SETQ LENGTH_SLACK_EQUATIONS 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ LENGTH_SLACK_EQUATIONS (LENGTH SLACK_EQUATIONS))
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ A (MKMATRIX (LENGTH SLACK_EQUATIONS) (LENGTH VARIABLE_LIST)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LENGTH_SLACK_EQUATIONS ROW)) (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE LENGTH_VARIABLE_LIST COL)) (RETURN NIL)))
           (PROGN
            (SETQ SLACK_ELT (NTH SLACK_EQUATIONS ROW))
            (SETQ VAR_ELT (NTH VARIABLE_LIST COL))
            (FAST_SETMAT A ROW COL
             ((LAMBDA (U)
                (COND ((ATOM U) U)
                      ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                      ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '*SQ))
                       (LIST 'MINUS (CAR (CADADR U))))
                      ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '|:RD:|))
                       U)
                      ((EQUAL (CAR U) '|:RD:|) U)
                      ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
              (AEVAL* (LIST 'COEFFN SLACK_ELT VAR_ELT 1))))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN A))) 
(PUT 'GET_X_AND_OBJ_MAT 'NUMBER-OF-ARGS 2) 
(PUT 'GET_X_AND_OBJ_MAT 'DEFINED-ON-LINE '575) 
(PUT 'GET_X_AND_OBJ_MAT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'GET_X_AND_OBJ_MAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_X_AND_OBJ_MAT (OBJECTIVE VARIABLE_LIST)
    (PROG (X OBJ_MAT I LENGTH_VARIABLE_LIST TMP)
      (SETQ I 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ TMP 0)
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ X (MKMATRIX LENGTH_VARIABLE_LIST 1))
      (SETQ OBJ_MAT (MKMATRIX 1 LENGTH_VARIABLE_LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VARIABLE_LIST) I)) (RETURN NIL)))
        (PROGN
         (FAST_SETMAT X I 1 (NTH VARIABLE_LIST I))
         (SETQ TMP (NTH VARIABLE_LIST I))
         (FAST_SETMAT OBJ_MAT 1 I (AEVAL* (LIST 'COEFFN OBJECTIVE TMP 1)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST X OBJ_MAT)))) 
(PUT 'GET_CB 'NUMBER-OF-ARGS 2) 
(PUT 'GET_CB 'DEFINED-ON-LINE '597) 
(PUT 'GET_CB 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'GET_CB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_CB (OBJ_MAT IB) (FAST_AUGMENT_COLUMNS OBJ_MAT IB)) 
(PUT 'COMPUTE_OBJECTIVE 'NUMBER-OF-ARGS 2) 
(PUT 'COMPUTE_OBJECTIVE 'DEFINED-ON-LINE '606) 
(PUT 'COMPUTE_OBJECTIVE 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'COMPUTE_OBJECTIVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPUTE_OBJECTIVE (CB XB)
    (FAST_UNCHECKED_GETMATELEM (LIST (REVAL1 (LIST 'TIMES CB XB) T) 1 1))) 
(PUT 'RSTEP1 'NUMBER-OF-ARGS 4) 
(PUT 'RSTEP1 'DEFINED-ON-LINE '614) 
(PUT 'RSTEP1 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'RSTEP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RSTEP1 (A OBJ_MAT BINV IB)
    (PROG (U SB SUM I_IN_IB I J M N K VKMIN)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ K 0)
      (SETQ VKMIN 0)
      (SETQ M (IDIFFERENCE (LENGTH A) 1))
      (SETQ N (LENGTH (CADR A)))
      (SETQ U (MKMATRIX M 1))
      (SETQ SB (MKMATRIX M 1))
      (SETQ U
              (REVAL1
               (LIST 'TIMES (LIST 'MINUS (AEVAL (LIST 'TP BINV)))
                     (AEVAL (LIST 'TP (GET_CB OBJ_MAT IB))))
               T))
      (SETQ K 0)
      (SETQ VKMIN (EXPT 10 10))
      (SETQ I 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ I_IN_IB NIL)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
           (PROGN (COND ((EQUAL I (NTH IB J)) (SETQ I_IN_IB T))) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND
          ((NOT I_IN_IB)
           (PROGN
            (SETQ SUM
                    (|SPECRD:PLUS|
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (FAST_UNCHECKED_GETMATELEM (LIST OBJ_MAT 1 I)))
                     (TWO_COLUMN_SCALAR_PRODUCT (FAST_AUGMENT_COLUMNS A I) U)))
            (COND ((GEQ (GET_NUM_PART SUM) (GET_NUM_PART VKMIN)) (PROGN NIL))
                  (T (PROGN (SETQ VKMIN SUM) (SETQ K I) NIL)))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((LESSP (GET_NUM_PART VKMIN) 0)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
           (PROGN
            (SETQ SUM 0)
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
              (SETQ SUM
                      (|SPECRD:PLUS| SUM
                       (|SPECRD:TIMES|
                        (FAST_UNCHECKED_GETMATELEM (LIST BINV I J))
                        (FAST_UNCHECKED_GETMATELEM (LIST A J K)))))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (FAST_SETMAT SB I 1 SUM)
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (RETURN (LIST SB K U NIL))
         NIL))
       (T (RETURN (LIST SB K U 'OPTIMAL)))))) 
(PUT 'RSTEP2 'NUMBER-OF-ARGS 2) 
(PUT 'RSTEP2 'DEFINED-ON-LINE '679) 
(PUT 'RSTEP2 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'RSTEP2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RSTEP2 (XB SB)
    (PROG (RATIO ELL SIGB)
      (SETQ ELL 0)
      (SETQ SIGB 0)
      (SETQ SIGB (TIMES 1 (EXPT 10 30)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((MINUSP (DIFFERENCE (IDIFFERENCE (LENGTH SB) 1) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((LEQ
            (GET_NUM_PART
             ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
              (FAST_UNCHECKED_GETMATELEM (LIST SB I 1))))
            0)
           (PROGN NIL))
          (T
           (PROGN
            (SETQ RATIO
                    (|SPECRD:QUOTIENT|
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (FAST_UNCHECKED_GETMATELEM (LIST XB I 1)))
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (FAST_UNCHECKED_GETMATELEM (LIST SB I 1)))))
            (COND ((GEQ (GET_NUM_PART RATIO) (GET_NUM_PART SIGB)) (PROGN NIL))
                  (T (PROGN (SETQ SIGB RATIO) (SETQ ELL I) NIL)))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((EQUAL ELL 0) (REDERR "Error in simplex: The problem is unbounded.")))
      (RETURN (LIST SIGB ELL)))) 
(PUT 'RSTEP3 'NUMBER-OF-ARGS 8) 
(PUT 'RSTEP3 'DEFINED-ON-LINE '711) 
(PUT 'RSTEP3 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'RSTEP3 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE RSTEP3 (XB OBJ_MAT B BINV A IB K ELL)
    (PROG (WORK)
      (SETQ WORK (FAST_AUGMENT_COLUMNS A K))
      (SETQ BINV (PHIPRM BINV WORK ELL))
      (SETQ XB (REVAL1 (LIST 'TIMES BINV B) T))
      (SETCAR (PNTH IB ELL) K)
      (SETQ OBJ_MAT (COMPUTE_OBJECTIVE (GET_CB OBJ_MAT IB) XB))
      (RETURN (LIST BINV OBJ_MAT XB)))) 
(PUT 'PHIPRM 'NUMBER-OF-ARGS 3) 
(PUT 'PHIPRM 'DEFINED-ON-LINE '729) 
(PUT 'PHIPRM 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'PHIPRM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PHIPRM (BINV D ELL)
    (PROG (SUM TEMP M J)
      (SETQ M 0)
      (SETQ J 0)
      (SETQ M (LENGTH (CADR BINV)))
      (SETQ SUM (SCALAR_PRODUCT (CONS 'MAT (LIST (NTH (CDR BINV) ELL))) D))
      (COND
       ((NOT (ZEROP (GET_NUM_PART SUM))) (SETQ SUM (|SPECRD:QUOTIENT| 1 SUM))))
      (SETQ BINV (FAST_MULT_ROWS BINV ELL SUM))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
        (PROGN
         (COND ((EQUAL J ELL) (PROGN NIL))
               (T
                (PROGN
                 (SETQ TEMP
                         (FAST_UNCHECKED_GETMATELEM
                          (LIST
                           (REVAL1
                            (LIST 'TIMES (CONS 'MAT (LIST (NTH (CDR BINV) J)))
                                  D)
                            T)
                           1 1)))
                 (SETQ BINV (FAST_ADD_ROWS BINV ELL J (LIST 'MINUS TEMP)))
                 NIL)))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN BINV))) 
(PUT 'MAKE_ANSWER_LIST 'NUMBER-OF-ARGS 5) 
(PUT 'MAKE_ANSWER_LIST 'DEFINED-ON-LINE '759) 
(PUT 'MAKE_ANSWER_LIST 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'MAKE_ANSWER_LIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE_ANSWER_LIST (XB IB NO_COEFFS X NO_VARIABLES)
    (PROG (X_MAT ANS_LIST I)
      (SETQ I 0)
      (SETQ X_MAT (MKMATRIX 1 NO_COEFFS))
      (SETQ I 1)
      (PROG (ELT)
        (SETQ ELT IB)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NEQ (FAST_UNCHECKED_GETMATELEM (LIST XB I 1)) 0)
              (FAST_SETMAT X_MAT 1 ELT
               (FAST_UNCHECKED_GETMATELEM (LIST XB I 1)))))
            (SETQ I (PLUS I 1))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ ANS_LIST
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NO_VARIABLES I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (LIST 'EQUAL
                                       ((LAMBDA (N)
                                          (COND ((FIXP N) N) (T (REVAL1 N T))))
                                        (FAST_UNCHECKED_GETMATELEM
                                         (LIST X I 1)))
                                       (GET_NUM_PART
                                        ((LAMBDA (N)
                                           (COND ((FIXP N) N)
                                                 (T (REVAL1 N T))))
                                         (FAST_UNCHECKED_GETMATELEM
                                          (LIST X_MAT 1 I)))))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE NO_VARIABLES I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (LIST 'EQUAL
                               ((LAMBDA (N)
                                  (COND ((FIXP N) N) (T (REVAL1 N T))))
                                (FAST_UNCHECKED_GETMATELEM (LIST X I 1)))
                               (GET_NUM_PART
                                ((LAMBDA (N)
                                   (COND ((FIXP N) N) (T (REVAL1 N T))))
                                 (FAST_UNCHECKED_GETMATELEM
                                  (LIST X_MAT 1 I)))))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN ANS_LIST))) 
(PUT 'FAST_ADD_ROWS 'NUMBER-OF-ARGS 4) 
(PUT 'FAST_ADD_ROWS 'DEFINED-ON-LINE '785) 
(PUT 'FAST_ADD_ROWS 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_ADD_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FAST_ADD_ROWS (IN_MAT R1 R2 MULT1)
    (PROG (NEW_MAT FAST_GETMATEL I COLDIM)
      (SETQ I 0)
      (SETQ COLDIM 0)
      (SETQ COLDIM (LENGTH (CADR IN_MAT)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (COND
       ((EQUAL (COND ((FIXP MULT1) MULT1) (T (REVAL1 MULT1 T))) 0)
        (RETURN NEW_MAT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE COLDIM I)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT
            (EQUAL
             (SETQ FAST_GETMATEL
                     ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                      (FAST_UNCHECKED_GETMATELEM (LIST NEW_MAT R1 I))))
             0))
           (FAST_SETMAT NEW_MAT R2 I
            (|SPECRD:PLUS|
             (|SPECRD:TIMES|
              (COND ((ATOM MULT1) MULT1)
                    ((AND (EQUAL (CAR MULT1) 'MINUS) (ATOM (CADR MULT1)))
                     MULT1)
                    ((AND (EQUAL (CAR MULT1) 'MINUS)
                          (EQUAL (CAADR MULT1) '*SQ))
                     (LIST 'MINUS (CAR (CADADR MULT1))))
                    ((AND (EQUAL (CAR MULT1) 'MINUS)
                          (EQUAL (CAADR MULT1) '|:RD:|))
                     MULT1)
                    ((EQUAL (CAR MULT1) '|:RD:|) MULT1)
                    ((EQUAL (CAR MULT1) '*SQ) (PREPSQ (CADR MULT1))))
              (COND ((ATOM FAST_GETMATEL) FAST_GETMATEL)
                    ((AND (EQUAL (CAR FAST_GETMATEL) 'MINUS)
                          (ATOM (CADR FAST_GETMATEL)))
                     FAST_GETMATEL)
                    ((AND (EQUAL (CAR FAST_GETMATEL) 'MINUS)
                          (EQUAL (CAADR FAST_GETMATEL) '*SQ))
                     (LIST 'MINUS (CAR (CADADR FAST_GETMATEL))))
                    ((AND (EQUAL (CAR FAST_GETMATEL) 'MINUS)
                          (EQUAL (CAADR FAST_GETMATEL) '|:RD:|))
                     FAST_GETMATEL)
                    ((EQUAL (CAR FAST_GETMATEL) '|:RD:|) FAST_GETMATEL)
                    ((EQUAL (CAR FAST_GETMATEL) '*SQ)
                     (PREPSQ (CADR FAST_GETMATEL)))))
             ((LAMBDA (U)
                (COND ((ATOM U) U)
                      ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                      ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '*SQ))
                       (LIST 'MINUS (CAR (CADADR U))))
                      ((AND (EQUAL (CAR U) 'MINUS) (EQUAL (CAADR U) '|:RD:|))
                       U)
                      ((EQUAL (CAR U) '|:RD:|) U)
                      ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
              (FAST_UNCHECKED_GETMATELEM (LIST IN_MAT R2 I)))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'FAST_AUGMENT_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'FAST_AUGMENT_COLUMNS 'DEFINED-ON-LINE '807) 
(PUT 'FAST_AUGMENT_COLUMNS 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_AUGMENT_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAST_AUGMENT_COLUMNS (IN_MAT COL_LIST)
    (COND
     ((ATOM COL_LIST)
      (CONS 'MAT
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 1)
              (COND
               ((MINUSP (DIFFERENCE (IDIFFERENCE (LENGTH IN_MAT) 1) I))
                (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               (LIST
                                (FAST_UNCHECKED_GETMATELEM
                                 (LIST IN_MAT I COL_LIST)))
                               NIL)))
             LOOPLABEL
              (SETQ I (PLUS2 I 1))
              (COND
               ((MINUSP (DIFFERENCE (IDIFFERENCE (LENGTH IN_MAT) 1) I))
                (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       (LIST
                        (FAST_UNCHECKED_GETMATELEM (LIST IN_MAT I COL_LIST)))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T
      (CONS 'MAT
            (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
              (SETQ ROW (CDR IN_MAT))
              (COND ((NULL ROW) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ROW)
                                  (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ ELT COL_LIST)
                                    (COND ((NULL ELT) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (ELT)
                                                        (NTH ROW ELT))
                                                      (CAR ELT))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ ELT (CDR ELT))
                                    (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (ELT) (NTH ROW ELT))
                                              (CAR ELT))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR ROW))
                               NIL)))
             LOOPLABEL
              (SETQ ROW (CDR ROW))
              (COND ((NULL ROW) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (ROW)
                          (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                            (SETQ ELT COL_LIST)
                            (COND ((NULL ELT) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (ELT) (NTH ROW ELT))
                                              (CAR ELT))
                                             NIL)))
                           LOOPLABEL
                            (SETQ ELT (CDR ELT))
                            (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ELT) (NTH ROW ELT)) (CAR ELT))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                        (CAR ROW))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT 'FAST_MATRIX_AUGMENT 'NUMBER-OF-ARGS 1) 
(PUT 'FAST_MATRIX_AUGMENT 'DEFINED-ON-LINE '817) 
(PUT 'FAST_MATRIX_AUGMENT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_MATRIX_AUGMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FAST_MATRIX_AUGMENT (MAT_LIST)
    (PROG (LL NEW_LIST)
      (COND ((EQUAL (LENGTH MAT_LIST) 1) (RETURN MAT_LIST))
            (T
             (PROGN
              (SETQ NEW_LIST (LIST))
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND
                 ((MINUSP
                   (DIFFERENCE (IDIFFERENCE (LENGTH (CAR MAT_LIST)) 1) I))
                  (RETURN NIL)))
                (PROGN
                 (SETQ LL (LIST))
                 (PROG (MAT1)
                   (SETQ MAT1 MAT_LIST)
                  LAB
                   (COND ((NULL MAT1) (RETURN NIL)))
                   ((LAMBDA (MAT1) (SETQ LL (APPEND LL (NTH (CDR MAT1) I))))
                    (CAR MAT1))
                   (SETQ MAT1 (CDR MAT1))
                   (GO LAB))
                 (SETQ NEW_LIST (APPEND NEW_LIST (LIST LL)))
                 NIL)
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              (RETURN (CONS 'MAT NEW_LIST))
              NIL))))) 
(PUT 'FAST_SETMAT 'NUMBER-OF-ARGS 4) 
(PUT 'FAST_SETMAT 'DEFINED-ON-LINE '839) 
(PUT 'FAST_SETMAT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_SETMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FAST_SETMAT (MATRI I J VAL)
    ((LAMBDA (G122)
       (RPLACA
        (PNTH
         (NTH (CDR MATRI)
              (CAR
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDR G122))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (COND ((FIXP J) J) (T (REVAL1 J T))))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                           (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
         (CADR
          (PROG (J FORALL-RESULT FORALL-ENDPTR)
            (SETQ J (CDR G122))
            (COND ((NULL J) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                              (CAR J))
                             NIL)))
           LOOPLABEL
            (SETQ J (CDR J))
            (COND ((NULL J) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                      (CAR J))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))
        VAL))
     (LIST MATRI I J))) 
(PUT 'FAST_UNCHECKED_GETMATELEM 'NUMBER-OF-ARGS 1) 
(PUT 'FAST_UNCHECKED_GETMATELEM 'DEFINED-ON-LINE '847) 
(PUT 'FAST_UNCHECKED_GETMATELEM 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_UNCHECKED_GETMATELEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FAST_UNCHECKED_GETMATELEM (U) (NTH (NTH (CDR (CAR U)) (CADR U)) (CADDR U))) 
(PUT 'FAST_MULT_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'FAST_MULT_ROWS 'DEFINED-ON-LINE '852) 
(PUT 'FAST_MULT_ROWS 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_MULT_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FAST_MULT_ROWS (IN_MAT ROW_LIST MULT1)
    (PROG (NEW_LIST NEW_ROW ROW_NO)
      (SETQ ROW_NO 0)
      (SETQ ROW_NO 1)
      (PROG (ROW)
        (SETQ ROW (CDR IN_MAT))
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((NEQ ROW_NO ROW_LIST)
              (SETQ NEW_LIST (APPEND NEW_LIST (LIST ROW))))
             (T
              (PROGN
               (SETQ NEW_ROW
                       (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                         (SETQ ELT ROW)
                         (COND ((NULL ELT) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (ELT)
                                             ((LAMBDA (N)
                                                (COND ((FIXP N) N)
                                                      (T (REVAL1 N T))))
                                              (LIST 'TIMES MULT1 ELT)))
                                           (CAR ELT))
                                          NIL)))
                        LOOPLABEL
                         (SETQ ELT (CDR ELT))
                         (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELT)
                                     ((LAMBDA (N)
                                        (COND ((FIXP N) N) (T (REVAL1 N T))))
                                      (LIST 'TIMES MULT1 ELT)))
                                   (CAR ELT))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ NEW_LIST (APPEND NEW_LIST (LIST NEW_ROW)))
               NIL)))
            (SETQ ROW_NO (PLUS ROW_NO 1))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN (CONS 'MAT NEW_LIST)))) 
(PUT 'FAST_MAKE_IDENTITY 'NUMBER-OF-ARGS 1) 
(PUT 'FAST_MAKE_IDENTITY 'DEFINED-ON-LINE '876) 
(PUT 'FAST_MAKE_IDENTITY 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'FAST_MAKE_IDENTITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FAST_MAKE_IDENTITY (SQ_SIZE)
    (CONS 'MAT
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I 1)
            (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J 1)
                               (COND
                                ((MINUSP (DIFFERENCE SQ_SIZE J)) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                (COND ((EQUAL I J) 1) (T 0))
                                                NIL)))
                              LOOPLABEL
                               (SETQ J (PLUS2 J 1))
                               (COND
                                ((MINUSP (DIFFERENCE SQ_SIZE J))
                                 (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS (COND ((EQUAL I J) 1) (T 0)) NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             NIL)))
           LOOPLABEL
            (SETQ I (PLUS2 I 1))
            (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                       (COND ((MINUSP (DIFFERENCE SQ_SIZE J)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS (COND ((EQUAL I J) 1) (T 0))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (PLUS2 J 1))
                       (COND
                        ((MINUSP (DIFFERENCE SQ_SIZE J))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS (COND ((EQUAL I J) 1) (T 0)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'TWO_COLUMN_SCALAR_PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'TWO_COLUMN_SCALAR_PRODUCT 'DEFINED-ON-LINE '885) 
(PUT 'TWO_COLUMN_SCALAR_PRODUCT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'TWO_COLUMN_SCALAR_PRODUCT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TWO_COLUMN_SCALAR_PRODUCT (COL1 COL2)
    (PROG (SUM)
      (SETQ SUM 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH (CDR COL1)) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (EQUAL (CAR (NTH (CDR COL1) I)) 0)
               (EQUAL (CAR (NTH (CDR COL2) I)) 0))
           (PROGN NIL))
          (T
           (SETQ SUM
                   (|SPECRD:PLUS| SUM
                    (|SPECRD:TIMES|
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (CAR (NTH (CDR COL1) I)))
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (CAR (NTH (CDR COL2) I))))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN SUM))) 
(PUT 'SCALAR_PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'SCALAR_PRODUCT 'DEFINED-ON-LINE '907) 
(PUT 'SCALAR_PRODUCT 'DEFINED-IN-FILE 'LINALG/SIMPLEX.RED) 
(PUT 'SCALAR_PRODUCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SCALAR_PRODUCT (ROW COL)
    (PROG (SUM)
      (SETQ SUM 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH (CADR ROW)) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (EQUAL (NTH (CADR ROW) I) 0) (EQUAL (CAR (NTH (CDR COL) I)) 0))
           (PROGN NIL))
          (T
           (SETQ SUM
                   (|SPECRD:PLUS| SUM
                    (|SPECRD:TIMES|
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (NTH (CADR ROW) I))
                     ((LAMBDA (U)
                        (COND ((ATOM U) U)
                              ((AND (EQUAL (CAR U) 'MINUS) (ATOM (CADR U))) U)
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '*SQ))
                               (LIST 'MINUS (CAR (CADADR U))))
                              ((AND (EQUAL (CAR U) 'MINUS)
                                    (EQUAL (CAADR U) '|:RD:|))
                               U)
                              ((EQUAL (CAR U) '|:RD:|) U)
                              ((EQUAL (CAR U) '*SQ) (PREPSQ (CADR U)))))
                      (CAR (NTH (CDR COL) I))))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN SUM))) 
(ENDMODULE) 