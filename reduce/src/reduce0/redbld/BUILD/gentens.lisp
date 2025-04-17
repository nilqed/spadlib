(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GENTENS)) 
(REMFLAG (LIST 'MINUS) 'INTFN) 
(FLUID '(YCOORD* YMAX* YMIN* OBRKP*)) 
(GLOBAL '(DIMEX* SGN* SIGNAT* SPACES* NUMINDXL* PAIR_ID_NUM*)) 
(SETQ PAIR_ID_NUM*
        '((|0| . 0) (|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
          (|7| . 7) (|8| . 8) (|9| . 9) (|10| . 10) (|11| . 11) (|12| . 12)
          (|13| . 13))) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON*)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(RLISTAT '(TENSOR REM_TENSOR REM_VALUE_TENS)) 
(FLAG '(MAKE_BLOC_DIAGONAL) 'OPFN) 
(PUT 'MAKE_BLOC_DIAGONAL 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_BLOC_DIAGONAL 'DEFINED-ON-LINE '65) 
(PUT 'MAKE_BLOC_DIAGONAL 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MAKE_BLOC_DIAGONAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_BLOC_DIAGONAL (TE)
    (PROGN (PUT TE 'BLOC_DIAGONAL 'SYMB_BELONG_SEVERAL_SPACES) T)) 
(PUT 'REM_VALUE_TENS 'NUMBER-OF-ARGS 1) 
(PUT 'REM_VALUE_TENS 'DEFINED-ON-LINE '70) 
(PUT 'REM_VALUE_TENS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REM_VALUE_TENS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_VALUE_TENS (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (COND ((ATOM X) (REMPROP X 'KVALUE))
                ((LISTP X)
                 (PROG (KVAL TENS VARL IND)
                   (SETQ TENS (CAR X))
                   (SETQ KVAL (GET TENS 'KVALUE))
                   (REMPROP TENS 'KVALUE)
                   (SETQ VARL (|SPLITLIST:| X 'LIST))
                   (SETQ IND
                           (COND ((NULL VARL) (CDR X))
                                 (T (SETDIFF (CDR X) VARL))))
                   (SETQ VARL (COND (VARL (CAR VARL))))
                   (SETQ IND
                           ((LAMBDA (Y)
                              (MKINDXLIST
                               (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ Z Y)
                                 (COND ((NULL Z) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (Z) (REVALIND Z))
                                                   (CAR Z))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ Z (CDR Z))
                                 (COND ((NULL Z) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Z) (REVALIND Z)) (CAR Z))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
                            IND))
                   (SETQ KVAL
                           (DELETE
                            (ASSOC
                             (COND (VARL (CONS TENS (CONS VARL IND)))
                                   (T (CONS TENS IND)))
                             KVAL)
                            KVAL))
                   (PUT TENS 'KVALUE KVAL)))))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'REM_TENSOR1 'NUMBER-OF-ARGS 1) 
(PUT 'REM_TENSOR1 'DEFINED-ON-LINE '90) 
(PUT 'REM_TENSOR1 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REM_TENSOR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_TENSOR1 (X)
    (PROGN
     (REMFLAG (LIST X) 'TENSOR)
     (ELIM_NAMES X)
     (REMPROP X 'KVALUE)
     (REMKLIST X)
     (REMPROP X 'SIMPFN)
     (REMPROP X 'PRIFN)
     (REMPROP X 'FANCY-PPRIFN)
     (REMPROP X 'PARTIC_TENS)
     (REMPROP X 'BELONG_TO_SPACE)
     (REMPROP X 'BLOC_DIAGONAL)
     (REMPROP X 'SYMTREE)
     (REMFLAG (LIST X) 'FULL)
     (REMFLAG (LIST X) 'SIMP0FN)
     (REMFLAG (LIST X) 'LISTARGP)
     (REMFLAG (LIST X) 'GENERIC)
     (REMFLAG (LIST X) 'SYMMETRIC)
     (REMFLAG (LIST X) 'ANTISYMMETRIC)
     ((LAMBDA (Y) (COND (Y (SETQ EPSILON* (DELETE Y EPSILON*)))))
      (ASSOC X EPSILON*))
     NIL)) 
(PUT 'ELIM_NAMES 'NUMBER-OF-ARGS 1) 
(PUT 'ELIM_NAMES 'DEFINED-ON-LINE '110) 
(PUT 'ELIM_NAMES 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'ELIM_NAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ELIM_NAMES (U)
    (COND ((EQUAL (GET U 'PARTIC_TENS) 'SIMPDELT) (REMPROP 'DELTA 'NAME))
          ((EQUAL (GET U 'PARTIC_TENS) 'SIMPDEL) (REMPROP 'DEL 'NAME))
          ((EQUAL (GET U 'PARTIC_TENS) 'SIMPETA) (REMPROP 'ETA 'NAME))
          ((EQUAL (GET U 'PARTIC_TENS) 'SIMPEPSI) (REMPROP 'EPSILON 'NAME))
          ((EQUAL (GET U 'PARTIC_TENS) 'METRIC) (REMPROP 'METRIC 'NAME)))) 
(PUT 'TENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'TENSOR 'DEFINED-ON-LINE '123) 
(PUT 'TENSOR 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'TENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENSOR (U)
    (PROG ()
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
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((OR (GET X 'AVALUE)
                 (AND (FLAGP X 'RESERVED) (NULL (FLAGP X 'TENSOR)))
                 (GETRTYPE X) (EQ (GETTYPE X) 'PROCEDURE)
                 (MEMQ X (LIST 'SIN 'COS 'TAN 'ATAN 'ACOS 'ASIN 'INT 'DF)))
             (RERROR 'CANTENS 1 (LIST X "may not be defined as tensor")))
            (T (MAKE_TENSOR X T))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN T))) 
(PUT 'MAKE_TENSOR 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_TENSOR 'DEFINED-ON-LINE '137) 
(PUT 'MAKE_TENSOR 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MAKE_TENSOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_TENSOR (U V)
    (PROGN
     (COND
      ((AND V (FLAGP U 'TENSOR))
       (LPRI (LIST "*** Warning: " U "redefined as generic tensor "))))
     (REM_TENSOR (LIST U))
     (FLAG (LIST U) 'TENSOR)
     (FLAG (LIST U) 'LISTARGP)
     (PUT U 'SIMPFN 'SIMPTENSOR)
     (FLAG (LIST U) 'SIMP0FN)
     (PUT U 'PRIFN 'INDVARPRT)
     (PUT U 'FANCY-PPRIFN 'XINDVARPRT_TENS)
     (FLAG (LIST U) 'FULL))) 
(PUT 'REM_TENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'REM_TENSOR 'DEFINED-ON-LINE '150) 
(PUT 'REM_TENSOR 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REM_TENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_TENSOR (U)
    (PROGN
     (SETQ U
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X U)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (COND ((FLAGP X 'TENSOR) (REM_TENSOR1 X)))) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'TENSORP 'NUMBER-OF-ARGS 1) 
(PUT 'TENSORP 'DEFINED-ON-LINE '157) 
(PUT 'TENSORP 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'TENSORP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENSORP (U) (AND (NOT (ATOM U)) (FLAGP (CAR U) 'TENSOR))) 
(PUT '|TENSORP:| 'NUMBER-OF-ARGS 1) 
(PUT '|TENSORP:| 'DEFINED-ON-LINE '161) 
(PUT '|TENSORP:| 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT '|TENSORP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |TENSORP:| (U)
    (PROG (NT)
      (SETQ NT 0)
      (PROGN
       (PROG ()
        WHILELABEL
         (COND ((NOT U) (RETURN NIL)))
         (COND ((TENSORP (CAR U)) (SETQ NT (PLUS NT 1))))
         (GO WHILELABEL))
       (SETQ U (CDR U)))
      (RETURN NT))) 
(FLAG (LIST 'MAKE_TENSOR_BELONG_SPACE) 'OPFN) 
(PUT 'MAKE_TENSOR_BELONG_SPACE 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_TENSOR_BELONG_SPACE 'DEFINED-ON-LINE '172) 
(PUT 'MAKE_TENSOR_BELONG_SPACE 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MAKE_TENSOR_BELONG_SPACE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_TENSOR_BELONG_SPACE (TE SP)
    (COND (*ONESPACE NIL)
          ((FLAGP TE 'TENSOR)
           (COND
            ((EQ (GET TE 'PARTIC_TENS) 'SIMPEPSI)
             (PROGN
              (SETQ EPSILON*
                      (UNION (LIST (CONS TE SP))
                             (DELETE (ASSOC TE EPSILON*) EPSILON*)))
              (PUT TE 'BELONG_TO_SPACE SP)))
            (T (PUT TE 'BELONG_TO_SPACE SP)))))) 
(RLISTAT '(MAKE_TENSOR_BELONG_ANYSPACE)) 
(PUT 'MAKE_TENSOR_BELONG_ANYSPACE 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_TENSOR_BELONG_ANYSPACE 'DEFINED-ON-LINE '191) 
(PUT 'MAKE_TENSOR_BELONG_ANYSPACE 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MAKE_TENSOR_BELONG_ANYSPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_TENSOR_BELONG_ANYSPACE (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (PROGN
           (REMPROP X 'BELONG_TO_SPACE)
           ((LAMBDA (Y) (COND (Y (SETQ EPSILON* (DELETE Y EPSILON*)))))
            (ASSOC X EPSILON*))))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'SIMPTENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTENSOR 'DEFINED-ON-LINE '201) 
(PUT 'SIMPTENSOR 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'SIMPTENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTENSOR (U)
    (PROG (X IND FUNC VARL BOOL LSYM)
      (SETQ VARL (|SPLITLIST:| U 'LIST))
      (COND
       ((NULL VARL)
        ((LAMBDA (Z) (COND (Z (PROGN (SETQ VARL Z) (SETQ BOOL T) NIL))))
         (EXTRACT_VARS (CDR U)))))
      (SETQ IND (COND ((NULL VARL) (CDR U)) (T (SETDIFF (CDR U) VARL))))
      (SETQ VARL (COND (BOOL (CONS 'LIST VARL)) (VARL (CAR VARL))))
      (SETQ VARL (REVAL1 VARL T))
      (SETQ X
              ((LAMBDA (Y)
                 (MKINDXLIST
                  (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                    (SETQ Z Y)
                    (COND ((NULL Z) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (Z) (REVALIND Z)) (CAR Z))
                                          NIL)))
                   LOOPLABEL
                    (SETQ Z (CDR Z))
                    (COND ((NULL Z) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (Z) (REVALIND Z)) (CAR Z)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
               IND))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (REVAL1 J T)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (REVAL1 J T)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X
              ((LAMBDA (Y)
                 (MKINDXLIST
                  (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                    (SETQ Z Y)
                    (COND ((NULL Z) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (Z) (REVALIND Z)) (CAR Z))
                                          NIL)))
                   LOOPLABEL
                    (SETQ Z (CDR Z))
                    (COND ((NULL Z) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (Z) (REVALIND Z)) (CAR Z)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
               X))
      (SETQ X (CONS (CAR U) X))
      (SETQ IND (SPLIT_COV_CONT_IDS (CDR X)))
      (NUM_IDS_RANGE IND (CAR U))
      (MK_DUMMY_IDS IND)
      (VERIFY_TENS_IDS IND)
      (COND
       (((LAMBDA (X) (COND (X (APPLY1 X IND)))) (GET (CAR U) 'BLOC_DIAGONAL))
        (RETURN (CONS NIL 1))))
      (RETURN
       (COND
        ((SETQ FUNC (GET (CAR X) 'PARTIC_TENS))
         (COND
          ((FLAGP (CAR U) 'GENERIC)
           (COND ((NEQ FUNC 'SIMPDELT) (APPLY2 FUNC X VARL))
                 (T (CONS (APPLY2 FUNC X VARL) 1))))
          (T (CONS (APPLY1 FUNC X) 1))))
        ((FLAGP (CAR X) 'SYMMETRIC)
         (MKSQ
          (CONS (CAR X)
                (COND ((NULL VARL) (CONT_BEFORE_COV (ORDN (CDR X))))
                      (T (CONS VARL (CONT_BEFORE_COV (ORDN (CDR X)))))))
          1))
        ((FLAGP (CAR X) 'ANTISYMMETRIC)
         (COND
          ((REPEATS
            (COND
             ((NULL (AFFINEP U))
              ((LAMBDA (Y) (APPEND (CAR Y) (CADR Y)))
               (SPLIT_COV_CONT_IDS (CDR X))))
             (T (CDR X))))
           (CONS NIL 1))
          (T
           ((LAMBDA (Z)
              (COND
               ((NOT (PERMP Z (CDR X)))
                (NEGSQ
                 (MKSQ (CONS (CAR X) (COND (VARL (CONS VARL Z)) (T Z))) 1)))
               (T (MKSQ (CONS (CAR X) (COND (VARL (CONS VARL Z)) (T Z))) 1))))
            (CONT_BEFORE_COV (ORDN (CDR X)))))))
        ((SETQ LSYM (GET (CAR U) 'SYMTREE))
         (COND ((SYMTREE_ZEROP (CDR X) LSYM) (CONS NIL 1))
               (T
                (MKSQ (COND (VARL (CONS (CAR X) (CONS VARL (CDR X)))) (T X))
                      1))))
        (T (MKSQ (COND (VARL (CONS (CAR X) (CONS VARL (CDR X)))) (T X)) 1)))))) 
(PUT 'SPLIT_COV_CONT_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT_COV_CONT_IDS 'DEFINED-ON-LINE '313) 
(PUT 'SPLIT_COV_CONT_IDS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'SPLIT_COV_CONT_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT_COV_CONT_IDS (U)
    (PROG (XCOV XCONT)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         ((LAMBDA (Y)
            (COND ((EQCAR Y 'MINUS) (SETQ XCOV (CONS (CADR Y) XCOV)))
                  (T (SETQ XCONT (CONS Y XCONT)))))
          (CAR U))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN (LIST (REVERSIP XCOV) (REVERSIP XCONT))))) 
(PUT 'VERIFY_TENS_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'VERIFY_TENS_IDS 'DEFINED-ON-LINE '323) 
(PUT 'VERIFY_TENS_IDS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'VERIFY_TENS_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VERIFY_TENS_IDS (U)
    (PROG (COV CNT)
      (SETQ COV (CAR U))
      (SETQ CNT (CADR U))
      (COND
       ((OR (REPEATS (EXTRACT_DUMMY_IDS COV))
            (REPEATS (EXTRACT_DUMMY_IDS CNT)))
        (RERROR 'CANTENS 2
                (LIST (LIST (CAR U) (CADR U))
                      "are inconsistent lists of indices")))
       (T (RETURN T))))) 
(RLISTAT '(MAKE_VARIABLES REMOVE_VARIABLES)) 
(PUT 'MAKE_VARIABLES 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_VARIABLES 'DEFINED-ON-LINE '342) 
(PUT 'MAKE_VARIABLES 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MAKE_VARIABLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_VARIABLES (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (FLAG (LIST X) 'VARIABLE)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'REMOVE_VARIABLES 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE_VARIABLES 'DEFINED-ON-LINE '348) 
(PUT 'REMOVE_VARIABLES 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REMOVE_VARIABLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE_VARIABLES (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (REMFLAG (LIST X) 'VARIABLE)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     T)) 
(PUT 'EXTRACT_VARS 'NUMBER-OF-ARGS 1) 
(PUT 'EXTRACT_VARS 'DEFINED-ON-LINE '354) 
(PUT 'EXTRACT_VARS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'EXTRACT_VARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRACT_VARS (U)
    (COND ((NULL U) NIL)
          ((FLAGP (|RAISEIND:| (CAR U)) 'VARIABLE)
           (CONS (CAR U) (EXTRACT_VARS (CDR U))))
          (T (EXTRACT_VARS (CDR U))))) 
(PUT 'SELECT_VARS 'NUMBER-OF-ARGS 1) 
(PUT 'SELECT_VARS 'DEFINED-ON-LINE '360) 
(PUT 'SELECT_VARS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'SELECT_VARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SELECT_VARS (U)
    (PROG (VARL IND BOOL)
      (SETQ VARL (|SPLITLIST:| U 'LIST))
      (COND
       ((NULL VARL)
        ((LAMBDA (Z) (COND (Z (PROGN (SETQ VARL Z) (SETQ BOOL T) NIL))))
         (EXTRACT_VARS (CDR U)))))
      (SETQ IND (COND ((NULL VARL) (CDR U)) (T (SETDIFF (CDR U) VARL))))
      (SETQ VARL (COND (BOOL (CONS 'LIST VARL)) (VARL (CAR VARL))))
      (RETURN (LIST IND VARL)))) 
(PUT 'SYMB_BELONG_SEVERAL_SPACES 'NUMBER-OF-ARGS 1) 
(PUT 'SYMB_BELONG_SEVERAL_SPACES 'DEFINED-ON-LINE '374) 
(PUT 'SYMB_BELONG_SEVERAL_SPACES 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'SYMB_BELONG_SEVERAL_SPACES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYMB_BELONG_SEVERAL_SPACES (IND)
    (COND (*ONESPACE NIL)
          (T
           (PROG (X SP)
             (SETQ X (CLEAN_NUMID (FLATTENS1 IND)))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND X
                       (OR (NULL (GET (CAR X) 'SPACE))
                           (EQ (GET (CAR X) 'SPACE) 'WHOLESPACE))))
                 (RETURN NIL)))
               (SETQ X (CDR X))
               (GO WHILELABEL))
             (COND ((NULL X) (RETURN NIL))
                   (T
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND X
                              (OR (NULL (GET (CAR X) 'SPACE))
                                  (EQ (GET (CAR X) 'SPACE) 'WHOLESPACE))))
                        (RETURN NIL)))
                      (SETQ X (CDR X))
                      (GO WHILELABEL))))
             (SETQ SP (GET (CAR X) 'SPACE))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND X
                       (OR (NULL (GET (CAR X) 'SPACE))
                           (EQ (GET (CAR X) 'SPACE) 'WHOLESPACE)
                           (EQ (GET (CAR X) 'SPACE) SP))))
                 (RETURN NIL)))
               (SETQ X (CDR X))
               (GO WHILELABEL))
             (RETURN (COND ((NULL X) NIL) (T T))))))) 
(PUT 'NUM_IDS_RANGE 'NUMBER-OF-ARGS 2) 
(PUT 'NUM_IDS_RANGE 'DEFINED-ON-LINE '395) 
(PUT 'NUM_IDS_RANGE 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'NUM_IDS_RANGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_IDS_RANGE (IND TENS)
    (COND
     (*ONESPACE
      (COND
       ((OUT_OF_RANGE IND DIMEX* NIL)
        (RERROR 'CANTENS 3 "numeric indices out of range"))
       (T NIL)))
     ((NULL NUMINDXL*)
      (COND
       ((OUT_OF_RANGE IND (GET_DIM_SPACE (GET TENS 'BELONG_TO_SPACE))
         (GET_SIGN_SPACE (GET TENS 'BELONG_TO_SPACE)))
        (RERROR 'CANTENS 3 "numeric indices out of range"))
       (T NIL)))
     (T
      ((LAMBDA (X INT)
         (COND
          ((NULL (LST_BELONG_INTERVAL X INT))
           (RERROR 'CANTENS 3
                   "numeric indices do not belong to (sub)-space"))))
       (EXTRACT_NUMID (FLATTENS1 IND))
       (SUBLA NUMINDXL* (GET TENS 'BELONG_TO_SPACE)))))) 
(PUT 'RESTORE_TENS_IDX 'NUMBER-OF-ARGS 2) 
(PUT 'RESTORE_TENS_IDX 'DEFINED-ON-LINE '415) 
(PUT 'RESTORE_TENS_IDX 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'RESTORE_TENS_IDX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RESTORE_TENS_IDX (U V)
    (COND ((NULL U) NIL)
          ((NULL (MEMQ (CAR U) DUMMY_ID*))
           (CONS (CAR U) (RESTORE_TENS_IDX (CDR U) (CDR V))))
          ((AND (ATOM (CAR U)) (ATOM (CAR V)))
           (CONS (CAR U) (RESTORE_TENS_IDX (CDR U) (CDR V))))
          (T (CONS (LIST 'MINUS U) (RESTORE_TENS_IDX (CDR U) (CDR V)))))) 
(PUT 'CLEAN_NUMID 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAN_NUMID 'DEFINED-ON-LINE '431) 
(PUT 'CLEAN_NUMID 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'CLEAN_NUMID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAN_NUMID (U)
    (COND ((NULL U) NIL) ((*ID2NUM (CAR U)) (CLEAN_NUMID (CDR U)))
          (T (CONS (CAR U) (CLEAN_NUMID (CDR U)))))) 
(PUT 'EXTRACT_NUM_ID 'NUMBER-OF-ARGS 1) 
(PUT 'EXTRACT_NUM_ID 'DEFINED-ON-LINE '440) 
(PUT 'EXTRACT_NUM_ID 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'EXTRACT_NUM_ID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRACT_NUM_ID (U)
    (COND ((NULL U) NIL)
          ((|CHARNUMP:| (CAR U)) (CONS (CAR U) (EXTRACT_NUM_ID (CDR U))))
          (T (EXTRACT_NUM_ID (CDR U))))) 
(PUT 'EXTRACT_NUMID 'NUMBER-OF-ARGS 1) 
(PUT 'EXTRACT_NUMID 'DEFINED-ON-LINE '447) 
(PUT 'EXTRACT_NUMID 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'EXTRACT_NUMID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRACT_NUMID (U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND (X (CONS X (EXTRACT_NUMID (CDR U))))
                    (T (EXTRACT_NUMID (CDR U)))))
            (*ID2NUM (CAR U)))))) 
(PUT 'MKINDXLIST 'NUMBER-OF-ARGS 1) 
(PUT 'MKINDXLIST 'DEFINED-ON-LINE '456) 
(PUT 'MKINDXLIST 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'MKINDXLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKINDXLIST (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND
                           ((FIXP J)
                            (COND
                             ((AND (LEQ J 15) (GEQ J 0))
                              (GETV INTS-AS-SYMBOLS* J))
                             (T (INTERN (LIST2STRING (EXPLODE J))))))
                           ((AND (PAIRP J) (FIXP (CADR J)))
                            (LIST 'MINUS
                                  (COND
                                   ((AND (LEQ (CADR J) 15) (GEQ (CADR J) 0))
                                    (GETV INTS-AS-SYMBOLS* (CADR J)))
                                   (T
                                    (INTERN
                                     (LIST2STRING (EXPLODE (CADR J))))))))
                           (T J)))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND
                   ((FIXP J)
                    (COND
                     ((AND (LEQ J 15) (GEQ J 0)) (GETV INTS-AS-SYMBOLS* J))
                     (T (INTERN (LIST2STRING (EXPLODE J))))))
                   ((AND (PAIRP J) (FIXP (CADR J)))
                    (LIST 'MINUS
                          (COND
                           ((AND (LEQ (CADR J) 15) (GEQ (CADR J) 0))
                            (GETV INTS-AS-SYMBOLS* (CADR J)))
                           (T (INTERN (LIST2STRING (EXPLODE (CADR J))))))))
                   (T J)))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT '*ID2NUM 'NUMBER-OF-ARGS 1) 
(PUT '*ID2NUM 'DEFINED-ON-LINE '463) 
(PUT '*ID2NUM 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT '*ID2NUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *ID2NUM (U)
    (PROG (X) (COND ((SETQ X (ASSOC U PAIR_ID_NUM*)) (RETURN (CDR X)))))) 
(PUT 'NUM_INDLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'NUM_INDLISTP 'DEFINED-ON-LINE '470) 
(PUT 'NUM_INDLISTP 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'NUM_INDLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_INDLISTP (U)
    (NUMLIS
     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
       (SETQ Y U)
       (COND ((NULL Y) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y)) NIL)))
      LOOPLABEL
       (SETQ Y (CDR Y))
       (COND ((NULL Y) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'OUT_OF_RANGE 'NUMBER-OF-ARGS 3) 
(PUT 'OUT_OF_RANGE 'DEFINED-ON-LINE '475) 
(PUT 'OUT_OF_RANGE 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'OUT_OF_RANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OUT_OF_RANGE (U DIM SIGN)
    (COND
     ((FIXP DIM)
      (PROG (LU SIGN_SPACE)
        (SETQ LU (EXTRACT_NUMID (FLATTENS1 U)))
        (SETQ SIGN_SPACE (COND ((NULL SIGN) SIGNAT*) (T SIGN)))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (AND LU
                  (COND ((EQUAL SIGN_SPACE 1) (LESSP (CAR LU) DIM))
                        ((EQUAL SIGN_SPACE 0) (LEQ (CAR LU) DIM)))))
            (RETURN NIL)))
          (SETQ LU (CDR LU))
          (GO WHILELABEL))
        (RETURN (COND (LU T) (T NIL))))))) 
(PUT 'REVALIND 'NUMBER-OF-ARGS 1) 
(PUT 'REVALIND 'DEFINED-ON-LINE '493) 
(PUT 'REVALIND 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REVALIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALIND (U)
    (PROG (X Y ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ X SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ U (SUBST '|0| 0 U))
      (SETQ Y (PREPSQ (SIMP U)))
      (SETQ SUBFG* X)
      (RETURN Y))) 
(PUT 'REVALINDL 'NUMBER-OF-ARGS 1) 
(PUT 'REVALINDL 'DEFINED-ON-LINE '505) 
(PUT 'REVALINDL 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'REVALINDL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALINDL (U)
    (PROG (IND FORALL-RESULT FORALL-ENDPTR)
      (SETQ IND U)
      (COND ((NULL IND) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (IND) (REVALIND IND)) (CAR IND)) NIL)))
     LOOPLABEL
      (SETQ IND (CDR IND))
      (COND ((NULL IND) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (IND) (REVALIND IND)) (CAR IND)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'INDVARPRT 'NUMBER-OF-ARGS 1) 
(PUT 'INDVARPRT 'DEFINED-ON-LINE '508) 
(PUT 'INDVARPRT 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'INDVARPRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDVARPRT (U)
    (COND
     ((NULL *NAT)
      (PROGN
       (PRIN2* (CAR U))
       (PRIN2* "(")
       (COND ((CDDR U) (INPRINT '*COMMA* 0 (CDR U))) (T (MAPRIN (CADR U))))
       (PRIN2* ")")))
     (T
      (PROG (X Y Y2 ARGS SPACEIT L MAXPOSN* OLDY)
        (SETQ L 0)
        (SETQ MAXPOSN* 0)
        (SETQ OLDY 0)
        (SETQ L
                (PLUS (FLATSIZEC (FLATINDXL U))
                      (DIFFERENCE (LENGTH (CDR U)) 1)))
        (COND
         ((GREATERP L (DIFFERENCE (DIFFERENCE (LINELENGTH NIL) SPARE*) POSN*))
          (TERPRI* T)))
        (SETQ Y YCOORD*)
        (SETQ MAXPOSN* 0)
        (PRIN2* (CAR U))
        (SETQ SPACEIT
                (COND
                 ((MEMQ (GET (CAR U) 'PARTIC_TENS) (LIST 'SIMPDELT 'SIMPDEL))
                  (PROGN (SETQ X POSN*) NIL))
                 (T T)))
        (PROG (J)
          (SETQ J (CDR U))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          (PROGN
           (SETQ OLDY YCOORD*)
           (SETQ YCOORD*
                   (PLUS Y
                         (COND ((OR (ATOM (CAR J)) (EQCAR (CAR J) '~)) 1)
                               (T (MINUS 1)))))
           (COND
            ((AND (NULL SPACEIT) (NEQ OLDY YCOORD*))
             (PROGN
              (COND ((GREATERP POSN* MAXPOSN*) (SETQ MAXPOSN* POSN*)))
              (SETQ POSN* X)
              NIL)))
           (COND ((GREATERP YCOORD* YMAX*) (SETQ YMAX* YCOORD*)))
           (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
           (COND ((OR (ATOM (CAR J)) (EQCAR (CAR J) '~)) (MAPRINT (CAR J) 0))
                 ((EQCAR (CAR J) 'MINUS) (MAPRINT (CADAR J) 0))
                 (T (SETQ ARGS (CAR J))))
           (COND ((CDR J) (PRIN2* " "))))
          (SETQ J (CDR J))
          (GO LAB))
        (COND
         ((NULL (CDR U))
          (PROGN
           (SETQ YCOORD* (PLUS Y 1))
           (COND ((GREATERP YCOORD* YMAX*) (SETQ YMAX* YCOORD*)))
           (COND ((LESSP YCOORD* YMIN*) (SETQ YMIN* YCOORD*)))
           (MAPRINT '|()| 0))))
        (SETQ YCOORD* Y)
        (COND
         ((AND (GREATERP MAXPOSN* 0) (LESSP POSN* MAXPOSN*))
          (SETQ POSN* MAXPOSN*)))
        (COND
         (ARGS
          (PROGN
           (PRIN2* "(")
           (SETQ OBRKP* NIL)
           (SETQ Y2 ORIG*)
           (SETQ ORIG* (COND ((LESSP POSN* 18) POSN*) (T (PLUS ORIG* 3))))
           (COND ((CDR ARGS) (INPRINT '*COMMA* 0 (CDR (REVAL1 ARGS T)))))
           (SETQ OBRKP* T)
           (SETQ ORIG* Y2)
           (PRIN2* ")")
           NIL))))))) 
(PUT 'INDVARPRT 'EXPT 'INBRACKETS) 
(FLUID '(FANCY-POS* FANCY-TEXPOS FANCY-LINE*)) 
(PUT 'XINDVARPRT_TENS 'NUMBER-OF-ARGS 2) 
(PUT 'XINDVARPRT_TENS 'DEFINED-ON-LINE '564) 
(PUT 'XINDVARPRT_TENS 'DEFINED-IN-FILE 'ASSIST/GENTENS.RED) 
(PUT 'XINDVARPRT_TENS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XINDVARPRT_TENS (L P)
    (PROG (POS TPOS FL W)
      (SETQ POS FANCY-POS*)
      (SETQ TPOS FANCY-TEXPOS)
      (SETQ FL FANCY-LINE*)
      (SETQ W
              (COND
               ((NOT (GREATERP (GET 'EXPT 'INFIX) P))
                (FANCY-IN-BRACKETS (LIST 'XINDVARPRT_TENS (MKQUOTE L) 0) '|(|
                 '|)|))
               (T
                (PROG (W X S ARGS SPACEIT)
                  (SETQ SPACEIT T)
                  ((LAMBDA (FANCY_LOWER_DIGITS)
                     (SETQ W (FANCY-PREFIX-OPERATOR (CAR L))))
                   NIL)
                  (COND
                   ((MEMQ (GET (CAR L) 'PARTIC_TENS) (LIST 'SIMPDELT 'SIMPDEL))
                    (SETQ SPACEIT NIL)))
                  (COND ((EQ W 'FAILED) (RETURN W)))
                  (SETQ L (CDR L))
                  (COND
                   (L
                    (PROGN
                     (PROG ()
                      WHILELABEL
                       (COND ((NOT (AND L (NEQ W 'FAILED))) (RETURN NIL)))
                       (PROGN
                        (COND
                         ((OR (ATOM (CAR L)) (EQCAR (CAR L) '~))
                          (COND ((EQ S '^) (SETQ X (CONS (CAR L) X)))
                                (T
                                 (PROGN
                                  (COND
                                   (S
                                    (PROGN
                                     (COND (SPACEIT (FANCY-PRIN2* "{}" 0)))
                                     (SETQ W
                                             (FANCY-PRINT-INDEXLIST1
                                              (REVERSIP X) S NIL)))))
                                  (SETQ X (LIST (CAR L)))
                                  (SETQ S '^)))))
                         (T
                          (COND
                           ((EQCAR (CAR L) 'MINUS)
                            (COND ((EQ S '_) (SETQ X (CONS (CADAR L) X)))
                                  (T
                                   (PROGN
                                    (COND
                                     (S
                                      (PROGN
                                       (COND (SPACEIT (FANCY-PRIN2* "{}" 0)))
                                       (SETQ W
                                               (FANCY-PRINT-INDEXLIST1
                                                (REVERSIP X) S NIL)))))
                                    (SETQ X (LIST (CADAR L)))
                                    (SETQ S '_)))))
                           (T (SETQ ARGS (CAR L))))))
                        (SETQ L (CDR L)))
                       (GO WHILELABEL))
                     (COND
                      (X
                       (PROGN
                        (COND (SPACEIT (FANCY-PRIN2* "{}" 0)))
                        (SETQ W (FANCY-PRINT-INDEXLIST1 (REVERSIP X) S NIL))
                        (COND ((EQ W 'FAILED) (RETURN W))))))
                     (COND
                      (ARGS
                       (SETQ W (FANCY-PRINT-FUNCTION-ARGUMENTS (CDR ARGS)))))
                     NIL))
                   (T
                    (PROGN
                     (SETQ W
                             (FANCY-PRINT-INDEXLIST1 (LIST '|(| '|)|) '^
                              NIL)))))
                  (RETURN W)))))
      (COND
       ((EQ W 'FAILED) (SETQ FANCY-LINE* FL) (SETQ FANCY-TEXPOS TPOS)
        (SETQ FANCY-POS* POS)))
      (RETURN W))) 
(ENDMODULE) 