(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SYMCHECK)) 
(PUT '|REPRESENTATION:P| 'NUMBER-OF-ARGS 1) 
(PUT '|REPRESENTATION:P| 'DEFINED-ON-LINE '47) 
(PUT '|REPRESENTATION:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|REPRESENTATION:P| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |REPRESENTATION:P| (REP)
    (PROG (GROUP ELEM MATS MAT1 DIM1)
      (COND ((LESSP (LENGTH REP) 0) (REDERR "list too short")))
      (COND ((NOT (OUTER+LIST+P REP)) (REDERR "argument should be a list")))
      (COND
       ((LESSP (LENGTH REP) 2) (REDERR "empty list is not a representation")))
      (SETQ GROUP (GET_GROUP_OUT REP))
      (COND
       ((NOT (OR (AVAILABLE*P GROUP) (STORING*P GROUP)))
        (REDERR "one element must be an identifier of an available group")))
      (SETQ MATS
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM (GET*GENERATORS GROUP))
                (COND ((NULL ELEM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (GET_REPMATRIX_OUT ELEM REP))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (GET_REPMATRIX_OUT ELEM REP))
                          (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (MAT1)
        (SETQ MAT1 MATS)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1)
           (COND
            ((NOT (ALG+MATRIX+P MAT1))
             (REDERR "there should be a matrix for each generator"))))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (SETQ MATS
              (PROG (MAT1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ MAT1 MATS)
                (COND ((NULL MAT1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (MAT1) (MK+INNER+MAT MAT1))
                                  (CAR MAT1))
                                 NIL)))
               LOOPLABEL
                (SETQ MAT1 (CDR MAT1))
                (COND ((NULL MAT1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (MAT1) (MK+INNER+MAT MAT1)) (CAR MAT1))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (MAT1)
        (SETQ MAT1 MATS)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1)
           (COND
            ((NOT (SQUARED+MATRIX+P MAT1))
             (REDERR "matrices should be squared"))))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (SETQ MAT1 (CAR MATS))
      (SETQ MATS (CDR MATS))
      (SETQ DIM1 (GET+ROW+NR MAT1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (LENGTH MATS) 0)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (EQUAL DIM1 (GET+ROW+NR (CAR MATS))))
           (REDERR "representation matrices must have the same dimension")))
         (SETQ MAT1 (CAR MATS))
         (SETQ MATS (CDR MATS))
         NIL)
        (GO WHILELABEL))
      (RETURN T))) 
(PUT '|IRR:NR:P| 'NUMBER-OF-ARGS 2) 
(PUT '|IRR:NR:P| 'DEFINED-ON-LINE '79) 
(PUT '|IRR:NR:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|IRR:NR:P| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |IRR:NR:P| (NR GROUP)
    (PROG ()
      (COND ((NOT (FIXP NR)) (REDERR "nr should be an integer")))
      (COND
       ((AND (GREATERP NR 0) (LEQ NR (GET_NR_IRRED_REPS GROUP))) (RETURN T))))) 
(PUT '|SYMMETRY:P| 'NUMBER-OF-ARGS 2) 
(PUT '|SYMMETRY:P| 'DEFINED-ON-LINE '88) 
(PUT '|SYMMETRY:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|SYMMETRY:P| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SYMMETRY:P| (MATRIX1 REPRESENTATION)
    (PROG (GROUP GLIST SYMMETRYP REPMAT)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (SETQ GLIST (GET*GENERATORS GROUP))
      (SETQ SYMMETRYP T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SYMMETRYP (GREATERP (LENGTH GLIST) 0))) (RETURN NIL)))
        (PROGN
         (SETQ REPMAT (GET_REP_MATRIX_IN (CAR GLIST) REPRESENTATION))
         (COND
          ((NOT
            (EQUAL+MATRICES+P (MK+MAT+MULT+MAT REPMAT MATRIX1)
             (MK+MAT+MULT+MAT MATRIX1 REPMAT)))
           (SETQ SYMMETRYP NIL)))
         (SETQ GLIST (CDR GLIST))
         NIL)
        (GO WHILELABEL))
      (RETURN SYMMETRYP))) 
(PUT '|IDENTIFIER:LIST:P| 'NUMBER-OF-ARGS 1) 
(PUT '|IDENTIFIER:LIST:P| 'DEFINED-ON-LINE '114) 
(PUT '|IDENTIFIER:LIST:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|IDENTIFIER:LIST:P| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |IDENTIFIER:LIST:P| (IDLIST)
    (PROG ()
      (COND
       ((GREATERP (LENGTH IDLIST) 0)
        (PROGN
         (COND
          ((IDP (CAR IDLIST)) (RETURN (|IDENTIFIER:LIST:P| (CDR IDLIST)))))
         NIL))
       (T (RETURN T))))) 
(PUT '|GENERATOR:LIST:P| 'NUMBER-OF-ARGS 2) 
(PUT '|GENERATOR:LIST:P| 'DEFINED-ON-LINE '125) 
(PUT '|GENERATOR:LIST:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|GENERATOR:LIST:P| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |GENERATOR:LIST:P| (GROUP GENERATORL)
    (PROG (ELEMENT RES)
      (SETQ RES T)
      (COND
       ((LESSP (LENGTH GENERATORL) 1)
        (REDERR "there should be a list of generators")))
      (COND
       ((LESSP (LENGTH (GET*GENERATORS GROUP)) 1)
        (REDERR "there are no group generators stored")))
      (COND ((NOT (|IDENTIFIER:LIST:P| GENERATORL)) (RETURN NIL)))
      (PROG (ELEMENT)
        (SETQ ELEMENT GENERATORL)
       LAB
        (COND ((NULL ELEMENT) (RETURN NIL)))
        ((LAMBDA (ELEMENT)
           (COND ((NOT (G*GENERATER*P GROUP ELEMENT)) (SETQ RES NIL))))
         (CAR ELEMENT))
        (SETQ ELEMENT (CDR ELEMENT))
        (GO LAB))
      (RETURN RES))) 
(PUT '|RELATION:LIST:P| 'NUMBER-OF-ARGS 2) 
(PUT '|RELATION:LIST:P| 'DEFINED-ON-LINE '142) 
(PUT '|RELATION:LIST:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|RELATION:LIST:P| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RELATION:LIST:P| (GROUP RELATIONS)
    (PROG ()
      (COND
       ((LESSP (LENGTH (GET*GENERATORS GROUP)) 1)
        (REDERR "there are no group generators stored")))
      (RETURN
       (AND (|RELATION:PART:P| GROUP (CAR RELATIONS))
            (|RELATION:PART:P| GROUP (CADR RELATIONS)))))) 
(PUT '|RELATION:PART:P| 'NUMBER-OF-ARGS 2) 
(PUT '|RELATION:PART:P| 'DEFINED-ON-LINE '151) 
(PUT '|RELATION:PART:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|RELATION:PART:P| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RELATION:PART:P| (GROUP RELATIONPART)
    (PROG (GENERATORS RES ELEMENT)
      (SETQ RES T)
      (SETQ GENERATORS (GET*GENERATORS GROUP))
      (COND
       ((LESSP (LENGTH GENERATORS) 1)
        (REDERR "there are no group generators stored")))
      (COND ((LESSP (LENGTH RELATIONPART) 1) (REDERR "wrong relation given")))
      (COND ((NOT (|IDENTIFIER:LIST:P| RELATIONPART)) (RETURN NIL)))
      (SETQ GENERATORS (APPEND (LIST 'ID) GENERATORS))
      (PROG (ELEMENT)
        (SETQ ELEMENT RELATIONPART)
       LAB
        (COND ((NULL ELEMENT) (RETURN NIL)))
        ((LAMBDA (ELEMENT)
           (COND ((NOT (MEMQ ELEMENT GENERATORS)) (SETQ RES NIL))))
         (CAR ELEMENT))
        (SETQ ELEMENT (CDR ELEMENT))
        (GO LAB))
      (RETURN RES))) 
(PUT '|GROUP:TABLE:P| 'NUMBER-OF-ARGS 2) 
(PUT '|GROUP:TABLE:P| 'DEFINED-ON-LINE '168) 
(PUT '|GROUP:TABLE:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|GROUP:TABLE:P| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |GROUP:TABLE:P| (GROUP GTABLE)
    (PROG (ROW)
      (COND
       ((NOT (EQUAL (GET+MAT+ENTRY GTABLE 1 1) 'GROUPTABLE))
        (REDERR "first diagonal entry in a group table must be grouptable")))
      (PROG (ROW)
        (SETQ ROW GTABLE)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (COND
            ((NOT (|GROUP:ELEMTS:P| GROUP (CDR ROW)))
             (REDERR "this should be a group table"))))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (PROG (ROW)
        (SETQ ROW (MK+TRANSPOSE+MATRIX GTABLE))
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (COND
            ((NOT (|GROUP:ELEMTS:P| GROUP (CDR ROW)))
             (REDERR "this should be a group table"))))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN T))) 
(PUT '|GROUP:ELEMTS:P| 'NUMBER-OF-ARGS 2) 
(PUT '|GROUP:ELEMTS:P| 'DEFINED-ON-LINE '184) 
(PUT '|GROUP:ELEMTS:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|GROUP:ELEMTS:P| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |GROUP:ELEMTS:P| (GROUP ELEMS)
    (PROG () (RETURN (EQUAL+LISTS+P (GET*ELEMENTS GROUP) ELEMS)))) 
(PUT '|CHECK:COMPLETE:REP:P| 'NUMBER-OF-ARGS 1) 
(PUT '|CHECK:COMPLETE:REP:P| 'DEFINED-ON-LINE '190) 
(PUT '|CHECK:COMPLETE:REP:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|CHECK:COMPLETE:REP:P| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |CHECK:COMPLETE:REP:P| (GROUP)
    (PROG (NR J SUM DIME ORDER1 SUMREAL CHARS COMPLEXCASE)
      (SETQ NR (GET*NR*COMPLEX*IRRED*REPS GROUP))
      (SETQ SUM (CONS NIL 1))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
        (PROGN
         (SETQ DIME
                 (CHANGE+INT+TO+SQ
                  (GET_DIMENSION_IN (GET*COMPLEX*IRREDUCIBLE*REP GROUP J))))
         (SETQ SUM (ADDSQ SUM (MULTSQ DIME DIME)))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ ORDER1 (CHANGE+INT+TO+SQ (GET*ORDER GROUP)))
      (COND
       ((NOT (NULL (CAR (ADDSQ SUM (NEGSQ ORDER1)))))
        (REDERR "one complex irreducible representation missing or
                    is not irreducible")))
      (SETQ SUM (CONS NIL 1))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
        (PROGN
         (SETQ DIME
                 (CHANGE+INT+TO+SQ
                  (GET_DIMENSION_IN (GET*COMPLEX*IRREDUCIBLE*REP GROUP J))))
         (SETQ SUM (ADDSQ SUM DIME))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ CHARS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (GET*COMPLEX*CHARACTER GROUP J) NIL)))
               LOOPLABEL
                (SETQ J (PLUS2 J 1))
                (COND ((MINUSP (DIFFERENCE NR J)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (GET*COMPLEX*CHARACTER GROUP J) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*COMPLEX (PROGN (SETQ COMPLEXCASE T) NIL))
            (T (PROGN (SETQ COMPLEXCASE NIL) (ON (LIST 'COMPLEX)) NIL)))
      (COND
       ((NOT (|ORTHOGONAL:CHARACTERS:P| CHARS))
        (REDERR "characters are not orthogonal")))
      (COND ((NULL COMPLEXCASE) (OFF (LIST 'COMPLEX))))
      (SETQ NR (GET*NR*REAL*IRRED*REPS GROUP))
      (SETQ SUMREAL (CONS NIL 1))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
        (PROGN
         (SETQ DIME
                 (CHANGE+INT+TO+SQ
                  (GET_DIMENSION_IN (GET*REAL*IRREDUCIBLE*REP GROUP J))))
         (SETQ SUMREAL (ADDSQ SUMREAL DIME))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ CHARS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (GET*REAL*CHARACTER GROUP J) NIL)))
               LOOPLABEL
                (SETQ J (PLUS2 J 1))
                (COND ((MINUSP (DIFFERENCE NR J)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (GET*REAL*CHARACTER GROUP J) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NOT (|ORTHOGONAL:CHARACTERS:P| CHARS))
        (REDERR "characters are not orthogonal")))
      (COND
       ((NOT (NULL (CAR (ADDSQ SUM (NEGSQ SUMREAL)))))
        (REDERR "list real irreducible representation incomplete or wrong")))
      (RETURN T))) 
(PUT '|ORTHOGONAL:CHARACTERS:P| 'NUMBER-OF-ARGS 1) 
(PUT '|ORTHOGONAL:CHARACTERS:P| 'DEFINED-ON-LINE '244) 
(PUT '|ORTHOGONAL:CHARACTERS:P| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ORTHOGONAL:CHARACTERS:P| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ORTHOGONAL:CHARACTERS:P| (CHARS)
    (PROG (CHARS1 CHARS2 CHAR1 CHAR2)
      (SETQ CHARS1 CHARS)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (LENGTH CHARS1) 0)) (RETURN NIL)))
        (PROGN
         (SETQ CHAR1 (CAR CHARS1))
         (SETQ CHARS1 (CDR CHARS1))
         (SETQ CHARS2 CHARS1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (GREATERP (LENGTH CHARS2) 0)) (RETURN NIL)))
           (PROGN
            (SETQ CHAR2 (CAR CHARS2))
            (SETQ CHARS2 (CDR CHARS2))
            (COND
             ((NOT (EQUAL (CHANGE+SQ+TO+ALGNULL (CHAR_PROD CHAR1 CHAR2)) 0))
              (REDERR "not orthogonal")))
            NIL)
           (GO WHILELABEL))
         NIL)
        (GO WHILELABEL))
      (RETURN T))) 
(PUT '|WRITE:TO:FILE| 'NUMBER-OF-ARGS 2) 
(PUT '|WRITE:TO:FILE| 'DEFINED-ON-LINE '266) 
(PUT '|WRITE:TO:FILE| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|WRITE:TO:FILE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |WRITE:TO:FILE| (GROUP FILENAME)
    (PROG (NR J)
      (COND ((NOT (AVAILABLE*P GROUP)) (REDERR "group is not available")))
      (OUT (LIST FILENAME))
      (RPRINT (LIST 'OFF 'ECHO))
      (RPRINT 'SYMBOLIC)
      (RPRINT
       (LIST 'SET*ELEMS*GROUP (MKQUOTE GROUP) (MKQUOTE (GET*ELEMENTS GROUP))))
      (RPRINT
       (LIST 'SET*GENERATORS (MKQUOTE GROUP) (MKQUOTE (GET*GENERATORS GROUP))))
      (RPRINT
       (LIST 'SET*RELATIONS (MKQUOTE GROUP)
             (MKQUOTE (GET*GENERATOR*RELATIONS GROUP))))
      (RPRINT
       (LIST 'SET*GROUPTABLE (MKQUOTE GROUP)
             (MKQUOTE (GET GROUP 'GROUPTABLE))))
      (RPRINT
       (LIST 'SET*INVERSE (MKQUOTE GROUP) (MKQUOTE (GET GROUP 'INVERSE))))
      (RPRINT
       (LIST 'SET*ELEMASGEN (MKQUOTE GROUP)
             (MKQUOTE (GET GROUP 'ELEM_IN_GENERATORS))))
      (RPRINT
       (LIST 'SET*GROUP (MKQUOTE GROUP) (MKQUOTE (GET GROUP 'EQUICLASSES))))
      (SETQ NR (GET*NR*COMPLEX*IRRED*REPS GROUP))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
        (PROGN
         (RPRINT
          (LIST 'SET*REPRESENTATION (MKQUOTE GROUP)
                (MKQUOTE (CDR (GET*COMPLEX*IRREDUCIBLE*REP GROUP J)))
                (MKQUOTE 'COMPLEX)))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ NR (GET*NR*REAL*IRRED*REPS GROUP))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NR J)) (RETURN NIL)))
        (PROGN
         (RPRINT
          (LIST 'SET*REPRESENTATION (MKQUOTE GROUP)
                (MKQUOTE (GET GROUP (MKID 'REALREP J))) (MKQUOTE 'REAL)))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RPRINT (LIST 'SET*AVAILABLE (MKQUOTE GROUP)))
      (RPRINT 'ALGEBRAIC)
      (RPRINT 'END)
      (SHUT (LIST FILENAME)))) 
(PUT 'MK_RELATION_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'MK_RELATION_LIST 'DEFINED-ON-LINE '314) 
(PUT 'MK_RELATION_LIST 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT 'MK_RELATION_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_RELATION_LIST (RELATIONS)
    (PROG (TWOLIST EQREL)
      (COND ((NOT (OUTER+LIST+P RELATIONS)) (REDERR "this should be a list")))
      (SETQ TWOLIST
              (PROG (EQREL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQREL (MK+INNER+LIST RELATIONS))
                (COND ((NULL EQREL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQREL) (CHANGE_EQ_TO_LISTS EQREL))
                                  (CAR EQREL))
                                 NIL)))
               LOOPLABEL
                (SETQ EQREL (CDR EQREL))
                (COND ((NULL EQREL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQREL) (CHANGE_EQ_TO_LISTS EQREL))
                          (CAR EQREL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN TWOLIST))) 
(PUT 'CHANGE_EQ_TO_LISTS 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE_EQ_TO_LISTS 'DEFINED-ON-LINE '326) 
(PUT 'CHANGE_EQ_TO_LISTS 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT 'CHANGE_EQ_TO_LISTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE_EQ_TO_LISTS (EQREL)
    (PROG ()
      (COND
       ((NOT (OUTER+EQUATION+P EQREL)) (REDERR "equations should be given")))
      (RETURN
       (LIST (MK_SIDE_TO_LIST (REVAL1 (CADR EQREL) T))
             (MK_SIDE_TO_LIST (REVAL1 (CADDR EQREL) T)))))) 
(PUT 'MK_SIDE_TO_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'MK_SIDE_TO_LIST 'DEFINED-ON-LINE '334) 
(PUT 'MK_SIDE_TO_LIST 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT 'MK_SIDE_TO_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_SIDE_TO_LIST (IDENTIFIERS)
    (PROG (I)
      (COND ((IDP IDENTIFIERS) (RETURN (LIST IDENTIFIERS))))
      (COND ((EQCAR IDENTIFIERS 'PLUS) (REDERR "no addition in this group")))
      (COND
       ((EQCAR IDENTIFIERS 'EXPT)
        (RETURN
         (PROG (I FORALL-RESULT FORALL-ENDPTR)
           (SETQ I 1)
           (COND ((MINUSP (DIFFERENCE (CADDR IDENTIFIERS) I)) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR (CONS (CADR IDENTIFIERS) NIL)))
          LOOPLABEL
           (SETQ I (PLUS2 I 1))
           (COND
            ((MINUSP (DIFFERENCE (CADDR IDENTIFIERS) I))
             (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS (CADR IDENTIFIERS) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (COND
       ((EQCAR IDENTIFIERS 'TIMES)
        (REDERR "no multiplication with * in this group")))
      (COND
       ((EQCAR IDENTIFIERS '@)
        (RETURN
         (APPEND (MK_SIDE_TO_LIST (CADR IDENTIFIERS))
                 (MK_SIDE_TO_LIST (CADDR IDENTIFIERS)))))))) 
(PUT '|ALG:PRINT:GROUP| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:PRINT:GROUP| 'DEFINED-ON-LINE '355) 
(PUT '|ALG:PRINT:GROUP| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:PRINT:GROUP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:PRINT:GROUP| (GROUP)
    (PROG () (RETURN (MK+OUTER+LIST (GET*ELEMENTS GROUP))))) 
(PUT '|ALG:GENERATORS| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:GENERATORS| 'DEFINED-ON-LINE '361) 
(PUT '|ALG:GENERATORS| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:GENERATORS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:GENERATORS| (GROUP)
    (PROG () (RETURN (APPEND (LIST 'LIST) (GET*GENERATORS GROUP))))) 
(PUT '|ALG:CHARACTERS| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:CHARACTERS| 'DEFINED-ON-LINE '367) 
(PUT '|ALG:CHARACTERS| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:CHARACTERS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:CHARACTERS| (GROUP)
    (PROG (NR I CHARLIST CHARI)
      (SETQ NR (GET_NR_IRRED_REPS GROUP))
      (SETQ CHARLIST
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (COND
                                  (*COMPLEX (GET*COMPLEX*CHARACTER GROUP I))
                                  (T (GET*REAL*CHARACTER GROUP I)))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE NR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (COND (*COMPLEX (GET*COMPLEX*CHARACTER GROUP I))
                               (T (GET*REAL*CHARACTER GROUP I)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CHARLIST
              (PROG (CHARI FORALL-RESULT FORALL-ENDPTR)
                (SETQ CHARI CHARLIST)
                (COND ((NULL CHARI) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CHARI)
                                    (|ALG:PRINT:CHARACTER| CHARI))
                                  (CAR CHARI))
                                 NIL)))
               LOOPLABEL
                (SETQ CHARI (CDR CHARI))
                (COND ((NULL CHARI) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (CHARI) (|ALG:PRINT:CHARACTER| CHARI))
                          (CAR CHARI))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK+OUTER+LIST CHARLIST)))) 
(PUT '|ALG:IRR:REPS| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:IRR:REPS| 'DEFINED-ON-LINE '382) 
(PUT '|ALG:IRR:REPS| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:IRR:REPS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:IRR:REPS| (GROUP)
    (PROG (REPI REPS NR I)
      (SETQ NR (GET_NR_IRRED_REPS GROUP))
      (SETQ REPS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (COND
                                  (*COMPLEX
                                   (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR))
                                  (T (GET*REAL*IRREDUCIBLE*REP GROUP I)))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE NR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (COND
                          (*COMPLEX (GET*COMPLEX*IRREDUCIBLE*REP GROUP NR))
                          (T (GET*REAL*IRREDUCIBLE*REP GROUP I)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ REPS
              (PROG (REPI FORALL-RESULT FORALL-ENDPTR)
                (SETQ REPI REPS)
                (COND ((NULL REPI) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (REPI) (|ALG:PRINT:REP| REPI))
                                  (CAR REPI))
                                 NIL)))
               LOOPLABEL
                (SETQ REPI (CDR REPI))
                (COND ((NULL REPI) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (REPI) (|ALG:PRINT:REP| REPI)) (CAR REPI))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK+OUTER+LIST REPS)))) 
(PUT '|ALG:PRINT:REP| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:PRINT:REP| 'DEFINED-ON-LINE '397) 
(PUT '|ALG:PRINT:REP| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:PRINT:REP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:PRINT:REP| (REPRESENTATION)
    (PROG (PAIR REPR GROUP MAT1 G)
      (SETQ GROUP (GET_GROUP_IN REPRESENTATION))
      (SETQ REPR (ELI_GROUP_IN REPRESENTATION))
      (SETQ REPR
              (PROG (PAIR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PAIR REPR)
                (COND ((NULL PAIR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PAIR)
                                    (PROGN
                                     (SETQ MAT1 (CADR PAIR))
                                     (SETQ G (CAR PAIR))
                                     (SETQ MAT1 (MK+OUTER+MAT MAT1))
                                     (MK+EQUATION G MAT1)))
                                  (CAR PAIR))
                                 NIL)))
               LOOPLABEL
                (SETQ PAIR (CDR PAIR))
                (COND ((NULL PAIR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PAIR)
                            (PROGN
                             (SETQ MAT1 (CADR PAIR))
                             (SETQ G (CAR PAIR))
                             (SETQ MAT1 (MK+OUTER+MAT MAT1))
                             (MK+EQUATION G MAT1)))
                          (CAR PAIR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ REPR (APPEND (LIST GROUP) REPR))
      (RETURN (MK+OUTER+LIST REPR)))) 
(PUT '|ALG:CAN:DECOMP| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:CAN:DECOMP| 'DEFINED-ON-LINE '414) 
(PUT '|ALG:CAN:DECOMP| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:CAN:DECOMP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:CAN:DECOMP| (REPRESENTATION)
    (PROG (NR NRIRR INTS I SUM)
      (SETQ NRIRR (GET_NR_IRRED_REPS (GET_GROUP_IN REPRESENTATION)))
      (SETQ INTS
              (PROG (NR FORALL-RESULT FORALL-ENDPTR)
                (SETQ NR 1)
                (COND ((MINUSP (DIFFERENCE NRIRR NR)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (MK_MULTIPLICITY REPRESENTATION NR)
                                      NIL)))
               LOOPLABEL
                (SETQ NR (PLUS2 NR 1))
                (COND ((MINUSP (DIFFERENCE NRIRR NR)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (MK_MULTIPLICITY REPRESENTATION NR) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SUM (CONS NIL 1))
      (SETQ INTS
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE (LENGTH INTS) I)) (RETURN NIL)))
                (SETQ SUM
                        (ADDSQ SUM
                               (MULTSQ (CHANGE+INT+TO+SQ (NTH INTS I))
                                       (SIMP (MKID 'TETA I)))))
                (SETQ I (PLUS2 I 1))
                (GO LAB)))
      (RETURN (MK+EQUATION 'TETA (PREPSQ SUM))))) 
(PUT '|ALG:PRINT:CHARACTER| 'NUMBER-OF-ARGS 1) 
(PUT '|ALG:PRINT:CHARACTER| 'DEFINED-ON-LINE '432) 
(PUT '|ALG:PRINT:CHARACTER| 'DEFINED-IN-FILE 'SYMMETRY/SYMCHECK.RED) 
(PUT '|ALG:PRINT:CHARACTER| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |ALG:PRINT:CHARACTER| (CHARACTER)
    (PROG (GROUP RES EQUILISTS)
      (SETQ GROUP (GET_CHAR_GROUP CHARACTER))
      (SETQ RES (GET*ALL*EQUI*CLASSES GROUP))
      (SETQ RES
              (PROG (EQUILISTS FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQUILISTS RES)
                (COND ((NULL EQUILISTS) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQUILISTS)
                                    (MK+OUTER+LIST EQUILISTS))
                                  (CAR EQUILISTS))
                                 NIL)))
               LOOPLABEL
                (SETQ EQUILISTS (CDR EQUILISTS))
                (COND ((NULL EQUILISTS) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQUILISTS) (MK+OUTER+LIST EQUILISTS))
                          (CAR EQUILISTS))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES
              (PROG (EQUILISTS FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQUILISTS RES)
                (COND ((NULL EQUILISTS) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQUILISTS)
                                    (MK+OUTER+LIST
                                     (LIST EQUILISTS
                                           (PREPSQ
                                            (GET_CHAR_VALUE CHARACTER
                                             (CADR EQUILISTS))))))
                                  (CAR EQUILISTS))
                                 NIL)))
               LOOPLABEL
                (SETQ EQUILISTS (CDR EQUILISTS))
                (COND ((NULL EQUILISTS) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQUILISTS)
                            (MK+OUTER+LIST
                             (LIST EQUILISTS
                                   (PREPSQ
                                    (GET_CHAR_VALUE CHARACTER
                                     (CADR EQUILISTS))))))
                          (CAR EQUILISTS))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES (APPEND (LIST GROUP) RES))
      (RETURN (MK+OUTER+LIST RES)))) 
(ENDMODULE) 