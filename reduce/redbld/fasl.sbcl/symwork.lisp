(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SYMWORK)) 
(SWITCH (LIST 'OUTERZEROSCHECK)) 
(PUT 'CORRECT_DIAGONAL_P 'NUMBER-OF-ARGS 3) 
(PUT 'CORRECT_DIAGONAL_P 'DEFINED-ON-LINE '57) 
(PUT 'CORRECT_DIAGONAL_P 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'CORRECT_DIAGONAL_P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CORRECT_DIAGONAL_P (MATRIXX REPRESENTATION MATS)
    (PROG (BASIS DIAG)
      (SETQ BASIS (MK_SYM_BASIS REPRESENTATION))
      (SETQ DIAG (MK+MAT*MAT*MAT (MK+HERMITEAN+MATRIX BASIS) MATRIXX BASIS))
      (COND ((EQUAL+MATRICES+P DIAG MATS) (RETURN T))))) 
(PUT 'GET_NR_IRRED_REPS 'NUMBER-OF-ARGS 1) 
(PUT 'GET_NR_IRRED_REPS 'DEFINED-ON-LINE '76) 
(PUT 'GET_NR_IRRED_REPS 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'GET_NR_IRRED_REPS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_NR_IRRED_REPS (GROUP)
    (PROG ()
      (COND (*COMPLEX (RETURN (GET*NR*COMPLEX*IRRED*REPS GROUP)))
            (T (RETURN (GET*NR*REAL*IRRED*REPS GROUP)))))) 
(PUT 'GET_DIM_IRRED_REPS 'NUMBER-OF-ARGS 2) 
(PUT 'GET_DIM_IRRED_REPS 'DEFINED-ON-LINE '84) 
(PUT 'GET_DIM_IRRED_REPS 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'GET_DIM_IRRED_REPS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_DIM_IRRED_REPS (GROUP NR)
    (PROG (REP)
      (COND (*COMPLEX (SETQ REP (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR)))
            (T (SETQ REP (GET*REAL*IRREDUCIBLE*REP GROUP NR))))
      (RETURN (GET_DIMENSION_IN REP)))) 
(PUT 'GET_GROUP_OUT 'NUMBER-OF-ARGS 1) 
(PUT 'GET_GROUP_OUT 'DEFINED-ON-LINE '103) 
(PUT 'GET_GROUP_OUT 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'GET_GROUP_OUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_GROUP_OUT (REPRESENTATION)
    (PROG (GROUP FOUND EINTRAG REPL)
      (SETQ FOUND NIL)
      (SETQ REPL (CDR REPRESENTATION))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT FOUND) (GREATERP (LENGTH REPL) 1))) (RETURN NIL)))
        (PROGN
         (SETQ EINTRAG (CAR REPL))
         (SETQ REPL (CDR REPL))
         (COND ((IDP EINTRAG) (PROGN (SETQ GROUP EINTRAG) (SETQ FOUND T) NIL)))
         NIL)
        (GO WHILELABEL))
      (COND (FOUND (RETURN GROUP)) (T (REDERR "group identifier missing"))))) 
(PUT 'GET_REPMATRIX_OUT 'NUMBER-OF-ARGS 2) 
(PUT 'GET_REPMATRIX_OUT 'DEFINED-ON-LINE '123) 
(PUT 'GET_REPMATRIX_OUT 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'GET_REPMATRIX_OUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_REPMATRIX_OUT (ELEM REPRESENTATION)
    (PROG (REPL FOUND MATELEM EINTRAG)
      (SETQ FOUND NIL)
      (SETQ REPL (CDR REPRESENTATION))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NULL FOUND) (GREATERP (LENGTH REPL) 0))) (RETURN NIL)))
        (PROGN
         (SETQ EINTRAG (CAR REPL))
         (SETQ REPL (CDR REPL))
         (COND
          ((EQCAR EINTRAG 'EQUAL)
           (PROGN
            (COND
             ((NOT (EQUAL (LENGTH EINTRAG) 3)) (REDERR "incomplete equation")))
            (COND
             ((EQUAL (CADR EINTRAG) ELEM)
              (PROGN (SETQ FOUND T) (SETQ MATELEM (CADDR EINTRAG)) NIL)))
            NIL)))
         NIL)
        (GO WHILELABEL))
      (COND (FOUND (RETURN MATELEM))
            (T (REDERR "representation matrix for one generator missing"))))) 
(PUT 'MK_REP_RELATION 'NUMBER-OF-ARGS 2) 
(PUT 'MK_REP_RELATION 'DEFINED-ON-LINE '149) 
(PUT 'MK_REP_RELATION 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_REP_RELATION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_REP_RELATION (REPRESENTATION GENERATORS)
    (PROG (G MATG RES)
      (SETQ RES
              (PROG (G FORALL-RESULT FORALL-ENDPTR)
                (SETQ G GENERATORS)
                (COND ((NULL G) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (G)
                                    (PROGN
                                     (SETQ MATG
                                             (MK+INNER+MAT
                                              (GET_REPMATRIX_OUT G
                                               REPRESENTATION)))
                                     (COND
                                      ((NOT (UNITARIAN+P MATG))
                                       (REDERR
                                        "please give an orthogonal or unitarian matrix")))
                                     (LIST G MATG)))
                                  (CAR G))
                                 NIL)))
               LOOPLABEL
                (SETQ G (CDR G))
                (COND ((NULL G) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (G)
                            (PROGN
                             (SETQ MATG
                                     (MK+INNER+MAT
                                      (GET_REPMATRIX_OUT G REPRESENTATION)))
                             (COND
                              ((NOT (UNITARIAN+P MATG))
                               (REDERR
                                "please give an orthogonal or unitarian matrix")))
                             (LIST G MATG)))
                          (CAR G))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK_CHARACTER 'NUMBER-OF-ARGS 1) 
(PUT 'MK_CHARACTER 'DEFINED-ON-LINE '172) 
(PUT 'MK_CHARACTER 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_CHARACTER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_CHARACTER (REPRESENTATION)
    (PROG (GROUP ELEM CHAR)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (SETQ CHAR
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM (GET*ELEMENTS GROUP))
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM)
                                    (LIST ELEM
                                          (MK+TRACE
                                           (GET_REP_MATRIX_IN ELEM
                                            REPRESENTATION))))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM)
                            (LIST ELEM
                                  (MK+TRACE
                                   (GET_REP_MATRIX_IN ELEM REPRESENTATION))))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CHAR (APPEND (LIST GROUP) CHAR))
      (RETURN CHAR))) 
(PUT 'MK_MULTIPLICITY 'NUMBER-OF-ARGS 2) 
(PUT 'MK_MULTIPLICITY 'DEFINED-ON-LINE '188) 
(PUT 'MK_MULTIPLICITY 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_MULTIPLICITY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_MULTIPLICITY (REPRESENTATION NR)
    (PROG (MULTNR CHAR1 GROUP)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND
       (*COMPLEX
        (SETQ CHAR1 (MK_CHARACTER (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR))))
       (T (SETQ CHAR1 (MK_CHARACTER (GET*REAL*IRREDUCIBLE*REP GROUP NR)))))
      (SETQ MULTNR (CHAR_PROD CHAR1 (MK_CHARACTER REPRESENTATION)))
      (COND
       ((AND (NOT *COMPLEX) (GET*REAL*COMP*CHARTYPE*P GROUP NR))
        (SETQ MULTNR (MULTSQ MULTNR (CONS 1 2)))))
      (RETURN (CHANGE+SQ+TO+INT MULTNR)))) 
(PUT 'CHAR_PROD 'NUMBER-OF-ARGS 2) 
(PUT 'CHAR_PROD 'DEFINED-ON-LINE '207) 
(PUT 'CHAR_PROD 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'CHAR_PROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHAR_PROD (CHAR1 CHAR2)
    (PROG (GROUP ELEMS SUM G PRODUCT)
      (SETQ GROUP (GET_CHAR_GROUP CHAR1))
      (COND
       ((NOT (EQUAL GROUP (GET_CHAR_GROUP CHAR2)))
        (REDERR "no product for two characters of different groups")))
      (COND
       ((AND (NOT (AVAILABLE*P GROUP)) (NOT (STORING*P GROUP)))
        (REDERR "strange group in character product")))
      (SETQ ELEMS (GET*ELEMENTS GROUP))
      (SETQ SUM (CONS NIL 1))
      (PROG (G)
        (SETQ G ELEMS)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (SETQ PRODUCT
                    (MULTSQ (GET_CHAR_VALUE CHAR1 G)
                            (GET_CHAR_VALUE CHAR2 (GET*INVERSE GROUP G))))
            (SETQ SUM (ADDSQ SUM PRODUCT))
            NIL))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (RETURN (MULTSQ SUM (INVSQ (CHANGE+INT+TO+SQ (GET*ORDER GROUP))))))) 
(PUT 'MK_PROJ_ISO 'NUMBER-OF-ARGS 2) 
(PUT 'MK_PROJ_ISO 'DEFINED-ON-LINE '229) 
(PUT 'MK_PROJ_ISO 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PROJ_ISO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_PROJ_ISO (REPRESENTATION NR)
    (PROG (GROUP ELEMS G CHARNR DIMEN MAPPING FACT)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND ((NOT (AVAILABLE*P GROUP)) (REDERR "strange group in projection")))
      (COND
       ((NOT (|IRR:NR:P| NR GROUP))
        (REDERR "incorrect number of representation")))
      (SETQ ELEMS (GET*ELEMENTS GROUP))
      (COND
       (*COMPLEX
        (SETQ CHARNR (MK_CHARACTER (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR))))
       (T (SETQ CHARNR (MK_CHARACTER (GET*REAL*IRREDUCIBLE*REP GROUP NR)))))
      (SETQ DIMEN (GET_DIMENSION_IN REPRESENTATION))
      (SETQ MAPPING (MK+NULL+MAT DIMEN DIMEN))
      (PROG (G)
        (SETQ G ELEMS)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (SETQ MAPPING
                    (MK+MAT+PLUS+MAT MAPPING
                     (MK+SCAL+MULT+MAT
                      (GET_CHAR_VALUE CHARNR (GET*INVERSE GROUP G))
                      (GET_REP_MATRIX_IN G REPRESENTATION))))
            NIL))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ FACT
              (MULTSQ (CHANGE+INT+TO+SQ (GET_CHAR_DIM CHARNR))
                      (INVSQ (CHANGE+INT+TO+SQ (GET*ORDER GROUP)))))
      (SETQ MAPPING (MK+SCAL+MULT+MAT FACT MAPPING))
      (COND
       ((AND (NOT *COMPLEX) (GET*REAL*COMP*CHARTYPE*P GROUP NR))
        (SETQ MAPPING (MK+SCAL+MULT+MAT (CONS 1 2) MAPPING))))
      (RETURN MAPPING))) 
(PUT 'MK_PROJ_FIRST 'NUMBER-OF-ARGS 2) 
(PUT 'MK_PROJ_FIRST 'DEFINED-ON-LINE '266) 
(PUT 'MK_PROJ_FIRST 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PROJ_FIRST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_PROJ_FIRST (REPRESENTATION NR)
    (PROG (GROUP ELEMS G IRRREP DIMEN MAPPING FACT CHARNR IRRDIM)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND ((NOT (AVAILABLE*P GROUP)) (REDERR "strange group in projection")))
      (COND
       ((NOT (|IRR:NR:P| NR GROUP))
        (REDERR "incorrect number of representation")))
      (SETQ ELEMS (GET*ELEMENTS GROUP))
      (COND (*COMPLEX (SETQ IRRREP (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR)))
            (T (SETQ IRRREP (GET*REAL*IRREDUCIBLE*REP GROUP NR))))
      (SETQ DIMEN (GET_DIMENSION_IN REPRESENTATION))
      (SETQ MAPPING (MK+NULL+MAT DIMEN DIMEN))
      (PROG (G)
        (SETQ G ELEMS)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (SETQ MAPPING
                    (MK+MAT+PLUS+MAT MAPPING
                     (MK+SCAL+MULT+MAT
                      (GET_REP_MATRIX_ENTRY IRRREP (GET*INVERSE GROUP G) 1 1)
                      (GET_REP_MATRIX_IN G REPRESENTATION))))
            NIL))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ IRRDIM (GET_DIMENSION_IN IRRREP))
      (SETQ FACT
              (MULTSQ (CHANGE+INT+TO+SQ IRRDIM)
                      (INVSQ (CHANGE+INT+TO+SQ (GET*ORDER GROUP)))))
      (SETQ MAPPING (MK+SCAL+MULT+MAT FACT MAPPING))
      (RETURN MAPPING))) 
(PUT 'MK_MAPPING 'NUMBER-OF-ARGS 3) 
(PUT 'MK_MAPPING 'DEFINED-ON-LINE '300) 
(PUT 'MK_MAPPING 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_MAPPING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_MAPPING (REPRESENTATION NR COUNT)
    (PROG (GROUP ELEMS G IRRREP DIMEN MAPPING FACT IRRDIM)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND ((NOT (AVAILABLE*P GROUP)) (REDERR "strange group in projection")))
      (COND
       ((NOT (|IRR:NR:P| NR GROUP))
        (REDERR "incorrect number of representation")))
      (SETQ ELEMS (GET*ELEMENTS GROUP))
      (COND (*COMPLEX (SETQ IRRREP (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR)))
            (T (SETQ IRRREP (GET*REAL*IRREDUCIBLE*REP GROUP NR))))
      (SETQ DIMEN (GET_DIMENSION_IN REPRESENTATION))
      (SETQ MAPPING (MK+NULL+MAT DIMEN DIMEN))
      (PROG (G)
        (SETQ G ELEMS)
       LAB
        (COND ((NULL G) (RETURN NIL)))
        ((LAMBDA (G)
           (PROGN
            (SETQ MAPPING
                    (MK+MAT+PLUS+MAT MAPPING
                     (MK+SCAL+MULT+MAT
                      (GET_REP_MATRIX_ENTRY IRRREP (GET*INVERSE GROUP G) 1
                       COUNT)
                      (GET_REP_MATRIX_IN G REPRESENTATION))))
            NIL))
         (CAR G))
        (SETQ G (CDR G))
        (GO LAB))
      (SETQ IRRDIM (GET_DIMENSION_IN IRRREP))
      (SETQ FACT
              (MULTSQ (CHANGE+INT+TO+SQ IRRDIM)
                      (INVSQ (CHANGE+INT+TO+SQ (GET*ORDER GROUP)))))
      (SETQ MAPPING (MK+SCAL+MULT+MAT FACT MAPPING))
      (RETURN MAPPING))) 
(PUT 'MK_PART_SYM 'NUMBER-OF-ARGS 2) 
(PUT 'MK_PART_SYM 'DEFINED-ON-LINE '334) 
(PUT 'MK_PART_SYM 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PART_SYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_PART_SYM (REPRESENTATION NR)
    (PROG (UNITLIST VECLIST2 MAPPING V)
      (SETQ UNITLIST (GEN+CAN+BAS (GET_DIMENSION_IN REPRESENTATION)))
      (SETQ MAPPING (MK_PROJ_ISO REPRESENTATION NR))
      (SETQ VECLIST2
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V UNITLIST)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V)) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK+INTERNAL+MAT (GRAM+SCHMID VECLIST2))))) 
(PUT 'MK_PART_SYM1 'NUMBER-OF-ARGS 2) 
(PUT 'MK_PART_SYM1 'DEFINED-ON-LINE '346) 
(PUT 'MK_PART_SYM1 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PART_SYM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_PART_SYM1 (REPRESENTATION NR)
    (PROG (UNITLIST VECLIST2 MAPPING V GROUP)
      (SETQ UNITLIST (GEN+CAN+BAS (GET_DIMENSION_IN REPRESENTATION)))
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND
       ((AND (NOT *COMPLEX) (GET*REAL*COMP*CHARTYPE*P GROUP NR))
        (PROGN (SETQ MAPPING (MK_PROJ_ISO REPRESENTATION NR)) NIL))
       (T (SETQ MAPPING (MK_PROJ_FIRST REPRESENTATION NR))))
      (SETQ VECLIST2
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V UNITLIST)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V)) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VECLIST2 (MK+RESIMP+MAT VECLIST2))
      (RETURN (MK+INTERNAL+MAT (GRAM+SCHMID VECLIST2))))) 
(PUT 'MK_PART_SYMNEXT 'NUMBER-OF-ARGS 4) 
(PUT 'MK_PART_SYMNEXT 'DEFINED-ON-LINE '366) 
(PUT 'MK_PART_SYMNEXT 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PART_SYMNEXT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_PART_SYMNEXT (REPRESENTATION NR COUNT MAT1)
    (PROG (VECLIST1 VECLIST2 MAPPING V)
      (SETQ MAPPING (MK_MAPPING REPRESENTATION NR COUNT))
      (SETQ VECLIST1 (MAT+VECLIST MAT1))
      (SETQ VECLIST2
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VECLIST1)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (MK+MAT+MULT+VEC MAPPING V)) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK+INTERNAL+MAT VECLIST2)))) 
(PUT 'MK_SYM_BASIS 'NUMBER-OF-ARGS 1) 
(PUT 'MK_SYM_BASIS 'DEFINED-ON-LINE '380) 
(PUT 'MK_SYM_BASIS 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_SYM_BASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_SYM_BASIS (REPRESENTATION)
    (PROG (NR ANZ GROUP DIMEN MATS MATELS MAT1 MAT2)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (SETQ ANZ (GET_NR_IRRED_REPS GROUP))
      (SETQ MATS
              (PROG (NR FORALL-RESULT FORALL-ENDPTR)
                (SETQ NR 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE ANZ NR)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (COND
                         ((NOT (NULL (MK_MULTIPLICITY REPRESENTATION NR)))
                          (PROGN
                           (COND
                            ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                             (SETQ MAT1 (MK_PART_SYM REPRESENTATION NR)))
                            (T (SETQ MAT1 (MK_PART_SYM1 REPRESENTATION NR))))
                           (COND
                            ((AND (NOT *COMPLEX)
                                  (GET*REAL*COMP*CHARTYPE*P GROUP NR))
                             (PROGN (SETQ MATELS (LIST MAT1)) NIL))
                            (T
                             (PROGN
                              (COND
                               ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                                (PROGN (SETQ MATELS (LIST MAT1)) NIL))
                               (T
                                (PROGN
                                 (SETQ MATELS
                                         (PROG (DIMEN FORALL-RESULT
                                                FORALL-ENDPTR)
                                           (SETQ DIMEN 2)
                                           (COND
                                            ((MINUSP
                                              (DIFFERENCE
                                               (GET_DIM_IRRED_REPS GROUP NR)
                                               DIMEN))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (MK_PART_SYMNEXT
                                                             REPRESENTATION NR
                                                             DIMEN MAT1)
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ DIMEN (PLUS2 DIMEN 1))
                                           (COND
                                            ((MINUSP
                                              (DIFFERENCE
                                               (GET_DIM_IRRED_REPS GROUP NR)
                                               DIMEN))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (MK_PART_SYMNEXT
                                                     REPRESENTATION NR DIMEN
                                                     MAT1)
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ MATELS (APPEND (LIST MAT1) MATELS))
                                 NIL)))
                              NIL)))
                           MATELS))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ NR (PLUS2 NR 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((MINUSP (DIFFERENCE ANZ NR)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (COND
                         ((NOT (NULL (MK_MULTIPLICITY REPRESENTATION NR)))
                          (PROGN
                           (COND
                            ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                             (SETQ MAT1 (MK_PART_SYM REPRESENTATION NR)))
                            (T (SETQ MAT1 (MK_PART_SYM1 REPRESENTATION NR))))
                           (COND
                            ((AND (NOT *COMPLEX)
                                  (GET*REAL*COMP*CHARTYPE*P GROUP NR))
                             (PROGN (SETQ MATELS (LIST MAT1)) NIL))
                            (T
                             (PROGN
                              (COND
                               ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                                (PROGN (SETQ MATELS (LIST MAT1)) NIL))
                               (T
                                (PROGN
                                 (SETQ MATELS
                                         (PROG (DIMEN FORALL-RESULT
                                                FORALL-ENDPTR)
                                           (SETQ DIMEN 2)
                                           (COND
                                            ((MINUSP
                                              (DIFFERENCE
                                               (GET_DIM_IRRED_REPS GROUP NR)
                                               DIMEN))
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (MK_PART_SYMNEXT
                                                             REPRESENTATION NR
                                                             DIMEN MAT1)
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ DIMEN (PLUS2 DIMEN 1))
                                           (COND
                                            ((MINUSP
                                              (DIFFERENCE
                                               (GET_DIM_IRRED_REPS GROUP NR)
                                               DIMEN))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (MK_PART_SYMNEXT
                                                     REPRESENTATION NR DIMEN
                                                     MAT1)
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (SETQ MATELS (APPEND (LIST MAT1) MATELS))
                                 NIL)))
                              NIL)))
                           MATELS))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ NR (PLUS2 NR 1))
                (GO LOOPLABEL)))
      (COND ((LESSP (LENGTH MATS) 1) (REDERR "no mats in mk!_sym!_basis")))
      (SETQ MAT2 (CAR MATS))
      (PROG (MAT1)
        (SETQ MAT1 (CDR MATS))
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1) (SETQ MAT2 (ADD+TWO+MATS MAT2 MAT1))) (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (RETURN MAT2))) 
(PUT 'MK_PART_SYM_ALL 'NUMBER-OF-ARGS 2) 
(PUT 'MK_PART_SYM_ALL 'DEFINED-ON-LINE '420) 
(PUT 'MK_PART_SYM_ALL 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_PART_SYM_ALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_PART_SYM_ALL (REPRESENTATION NR)
    (PROG (GROUP DIMEN MATELS MAT1 MAT2)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (COND
       ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
        (SETQ MAT1 (MK_PART_SYM REPRESENTATION NR)))
       (T
        (PROGN
         (SETQ MAT1 (MK_PART_SYM1 REPRESENTATION NR))
         (COND
          ((AND (NOT *COMPLEX) (GET*REAL*COMP*CHARTYPE*P GROUP NR))
           (PROGN (SETQ MAT1 MAT1) NIL))
          (T
           (PROGN
            (COND
             ((GREATERP (GET_DIM_IRRED_REPS GROUP NR) 1)
              (PROGN
               (SETQ MATELS
                       (PROG (DIMEN FORALL-RESULT FORALL-ENDPTR)
                         (SETQ DIMEN 2)
                         (COND
                          ((MINUSP
                            (DIFFERENCE (GET_DIM_IRRED_REPS GROUP NR) DIMEN))
                           (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          (MK_PART_SYMNEXT REPRESENTATION NR
                                           DIMEN MAT1)
                                          NIL)))
                        LOOPLABEL
                         (SETQ DIMEN (PLUS2 DIMEN 1))
                         (COND
                          ((MINUSP
                            (DIFFERENCE (GET_DIM_IRRED_REPS GROUP NR) DIMEN))
                           (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  (MK_PART_SYMNEXT REPRESENTATION NR DIMEN
                                   MAT1)
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (PROG (MAT2)
                 (SETQ MAT2 MATELS)
                LAB
                 (COND ((NULL MAT2) (RETURN NIL)))
                 ((LAMBDA (MAT2) (SETQ MAT1 (ADD+TWO+MATS MAT1 MAT2)))
                  (CAR MAT2))
                 (SETQ MAT2 (CDR MAT2))
                 (GO LAB))
               NIL)))
            NIL)))
         NIL)))
      (RETURN MAT1))) 
(PUT 'MK_DIAGONAL 'NUMBER-OF-ARGS 2) 
(PUT 'MK_DIAGONAL 'DEFINED-ON-LINE '449) 
(PUT 'MK_DIAGONAL 'DEFINED-IN-FILE 'SYMMETRY/SYMWORK.RED) 
(PUT 'MK_DIAGONAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK_DIAGONAL (MATRIX1 REPRESENTATION)
    (PROG (NR ANZ MATS GROUP MAT1 DIAMATS MATDIA DIMEN)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (SETQ ANZ (GET_NR_IRRED_REPS GROUP))
      (SETQ MATS
              (PROG (NR FORALL-RESULT FORALL-ENDPTR)
                (SETQ NR 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE ANZ NR)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (COND
                         ((NOT (NULL (MK_MULTIPLICITY REPRESENTATION NR)))
                          (PROGN
                           (COND
                            ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                             (SETQ MAT1 (MK_PART_SYM REPRESENTATION NR)))
                            (T (SETQ MAT1 (MK_PART_SYM1 REPRESENTATION NR))))
                           (SETQ MATDIA
                                   (MK+MAT*MAT*MAT (MK+HERMITEAN+MATRIX MAT1)
                                    MATRIX1 MAT1))
                           (COND
                            ((AND (NOT *COMPLEX)
                                  (GET*REAL*COMP*CHARTYPE*P GROUP NR))
                             (PROGN (SETQ DIAMATS (LIST MATDIA)) NIL))
                            (T
                             (PROGN
                              (SETQ DIAMATS
                                      (PROG (DIMEN FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ DIMEN 1)
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE
                                            (GET_DIM_IRRED_REPS GROUP NR)
                                            DIMEN))
                                          (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS MATDIA NIL)))
                                       LOOPLABEL
                                        (SETQ DIMEN (PLUS2 DIMEN 1))
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE
                                            (GET_DIM_IRRED_REPS GROUP NR)
                                            DIMEN))
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS MATDIA NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                              NIL)))
                           DIAMATS))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ NR (PLUS2 NR 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((MINUSP (DIFFERENCE ANZ NR)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (COND
                         ((NOT (NULL (MK_MULTIPLICITY REPRESENTATION NR)))
                          (PROGN
                           (COND
                            ((EQUAL (GET_DIM_IRRED_REPS GROUP NR) 1)
                             (SETQ MAT1 (MK_PART_SYM REPRESENTATION NR)))
                            (T (SETQ MAT1 (MK_PART_SYM1 REPRESENTATION NR))))
                           (SETQ MATDIA
                                   (MK+MAT*MAT*MAT (MK+HERMITEAN+MATRIX MAT1)
                                    MATRIX1 MAT1))
                           (COND
                            ((AND (NOT *COMPLEX)
                                  (GET*REAL*COMP*CHARTYPE*P GROUP NR))
                             (PROGN (SETQ DIAMATS (LIST MATDIA)) NIL))
                            (T
                             (PROGN
                              (SETQ DIAMATS
                                      (PROG (DIMEN FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ DIMEN 1)
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE
                                            (GET_DIM_IRRED_REPS GROUP NR)
                                            DIMEN))
                                          (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS MATDIA NIL)))
                                       LOOPLABEL
                                        (SETQ DIMEN (PLUS2 DIMEN 1))
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE
                                            (GET_DIM_IRRED_REPS GROUP NR)
                                            DIMEN))
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS MATDIA NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                              NIL)))
                           DIAMATS))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ NR (PLUS2 NR 1))
                (GO LOOPLABEL)))
      (SETQ MATS (MK+BLOCK+DIAGONAL+MAT MATS))
      (COND
       (*OUTERZEROSCHECK
        (COND
         ((NOT (CORRECT_DIAGONAL_P MATRIX1 REPRESENTATION MATS))
          (REDERR "wrong diagonalisation")))))
      (RETURN MATS))) 
(ENDMODULE) 