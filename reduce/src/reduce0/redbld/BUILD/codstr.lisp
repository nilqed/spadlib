(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GSTRUCTR)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(FLUID '(COUNTR SVAR *VARLIS)) 
(GLOBAL '(*ALGPRI)) 
(GLOBAL '(VARNAM*)) 
(SWITCH (LIST 'SAVESTRUCTR 'ALGPRI)) 
(PUT 'GSTRUCTR 'STAT 'GSTRUCTRSTAT) 
(PUT 'GSTRUCTRSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GSTRUCTRSTAT 'DEFINED-ON-LINE '82) 
(PUT 'GSTRUCTRSTAT 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'GSTRUCTRSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GSTRUCTRSTAT NIL
    (PROG (X Y)
      (FLAG '(NAME) 'DELIM)
      (COND ((EQCAR (SETQ X (XREAD T)) 'PROGN) (SETQ X (CDR X)))
            (T (SETQ X (LIST X))))
      (COND ((EQUAL CURSYM* 'NAME) (SETQ Y (XREAD T))))
      (REMFLAG '(NAME) 'DELIM)
      (RETURN (LIST 'GSTRUCTR X Y)))) 
(PUT 'GSTRUCTR 'FORMFN 'FORMGSTRUCTR) 
(PUT 'FORMGSTRUCTR 'NUMBER-OF-ARGS 3) 
(PUT 'FORMGSTRUCTR 'DEFINED-ON-LINE '97) 
(PUT 'FORMGSTRUCTR 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'FORMGSTRUCTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMGSTRUCTR (U VARS MODE)
    (LIST 'GSTRUCTR (MKQUOTE (CADR U)) (MKQUOTE (CADDR U)))) 
(PUT 'GSTRUCTR 'NUMBER-OF-ARGS 2) 
(PUT 'GSTRUCTR 'DEFINED-ON-LINE '100) 
(PUT 'GSTRUCTR 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'GSTRUCTR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GSTRUCTR (ASSSET NAME)
    (PROG ()
      (SETQ *VARLIS NIL)
      (SETQ COUNTR 0)
      (PROG (ASS)
        (SETQ ASS ASSSET)
       LAB
        (COND ((NULL ASS) (RETURN NIL)))
        ((LAMBDA (ASS)
           (COND
            ((NOT (PAIRP ASS))
             (COND
              ((EQUAL (GET ASS 'RTYPE) 'MATRIX)
               (PREPSTRUCTR (CADR (GET ASS 'AVALUE)) NAME ASS))
              (T (REDERR (LIST ASS "is not a matrix")))))
            (T (PREPSTRUCTR (CADDR ASS) NAME (CADR ASS)))))
         (CAR ASS))
        (SETQ ASS (CDR ASS))
        (GO LAB))
      (COND (*ALGPRI (PRINT*VARLIS))
            (T
             (RETURN
              (REMREDUNDANCY
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (REVERSIP* *VARLIS))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X) (LIST 'SETQ (CADR X) (CDDR X)))
                                   (CAR X))
                                  NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (LIST 'SETQ (CADR X) (CDDR X))) (CAR X))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'PREPSTRUCTR 'NUMBER-OF-ARGS 3) 
(PUT 'PREPSTRUCTR 'DEFINED-ON-LINE '116) 
(PUT 'PREPSTRUCTR 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSTRUCTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREPSTRUCTR (U NAME FVAR)
    (PROG (I J)
      (COND (NAME (SETQ SVAR NAME)) (T (SETQ SVAR VARNAM*)))
      (SETQ U (REVAL1 U NIL))
      (COND
       ((FLAGPCAR U 'STRUCT)
        (PROGN
         (SETQ I 0)
         (SETQ U
                 (CONS (CAR U)
                       (PROG (SCOPE_ROW FORALL-RESULT FORALL-ENDPTR)
                         (SETQ SCOPE_ROW (CDR U))
                         (COND ((NULL SCOPE_ROW) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (SCOPE_ROW)
                                             (PROGN
                                              (SETQ I (PLUS I 1))
                                              (SETQ J 0)
                                              (PROG (COLUMN FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ COLUMN SCOPE_ROW)
                                                (COND
                                                 ((NULL COLUMN) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA
                                                                      (COLUMN)
                                                                    (PROGN
                                                                     (SETQ J
                                                                             (PLUS
                                                                              J
                                                                              1))
                                                                     (SETQ *VARLIS
                                                                             (CONS
                                                                              (CONS
                                                                               NIL
                                                                               (CONS
                                                                                (LIST
                                                                                 FVAR
                                                                                 I
                                                                                 J)
                                                                                (PREPSQ
                                                                                 (PREPSTRUCT*SQ
                                                                                  COLUMN))))
                                                                              *VARLIS))))
                                                                  (CAR COLUMN))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ COLUMN (CDR COLUMN))
                                                (COND
                                                 ((NULL COLUMN)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (COLUMN)
                                                            (PROGN
                                                             (SETQ J
                                                                     (PLUS J
                                                                           1))
                                                             (SETQ *VARLIS
                                                                     (CONS
                                                                      (CONS NIL
                                                                            (CONS
                                                                             (LIST
                                                                              FVAR
                                                                              I
                                                                              J)
                                                                             (PREPSQ
                                                                              (PREPSTRUCT*SQ
                                                                               COLUMN))))
                                                                      *VARLIS))))
                                                          (CAR COLUMN))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                           (CAR SCOPE_ROW))
                                          NIL)))
                        LOOPLABEL
                         (SETQ SCOPE_ROW (CDR SCOPE_ROW))
                         (COND ((NULL SCOPE_ROW) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SCOPE_ROW)
                                     (PROGN
                                      (SETQ I (PLUS I 1))
                                      (SETQ J 0)
                                      (PROG (COLUMN FORALL-RESULT
                                             FORALL-ENDPTR)
                                        (SETQ COLUMN SCOPE_ROW)
                                        (COND ((NULL COLUMN) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (COLUMN)
                                                            (PROGN
                                                             (SETQ J
                                                                     (PLUS J
                                                                           1))
                                                             (SETQ *VARLIS
                                                                     (CONS
                                                                      (CONS NIL
                                                                            (CONS
                                                                             (LIST
                                                                              FVAR
                                                                              I
                                                                              J)
                                                                             (PREPSQ
                                                                              (PREPSTRUCT*SQ
                                                                               COLUMN))))
                                                                      *VARLIS))))
                                                          (CAR COLUMN))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ COLUMN (CDR COLUMN))
                                        (COND
                                         ((NULL COLUMN)
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (COLUMN)
                                                    (PROGN
                                                     (SETQ J (PLUS J 1))
                                                     (SETQ *VARLIS
                                                             (CONS
                                                              (CONS NIL
                                                                    (CONS
                                                                     (LIST FVAR
                                                                           I J)
                                                                     (PREPSQ
                                                                      (PREPSTRUCT*SQ
                                                                       COLUMN))))
                                                              *VARLIS))))
                                                  (CAR COLUMN))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))))
                                   (CAR SCOPE_ROW))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
       ((GETRTYPE U) (TYPERR U "STRUCTR argument"))
       (T
        (SETQ *VARLIS
                (CONS (CONS NIL (CONS FVAR (PREPSQ (PREPSTRUCT*SQ U))))
                      *VARLIS)))))) 
(PUT 'PRINT*VARLIS 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT*VARLIS 'DEFINED-ON-LINE '144) 
(PUT 'PRINT*VARLIS 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PRINT*VARLIS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT*VARLIS NIL
    (PROG ()
      (COND (*FORT (SETQ *VARLIS (REVERSIP* *VARLIS))))
      (COND
       ((NOT *FORT)
        (PROGN
         (PROG (X)
           (SETQ X (REVERSE *VARLIS))
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (COND
               ((NULL (CAR X))
                (PROGN
                 (ASSGNPRI (CDDR X) (LIST (CADR X)) T)
                 (COND ((NOT (FLAGPCAR (CDDR X) 'STRUCT)) (TERPRI)))
                 (COND ((NULL *NAT) (TERPRI)))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (COND ((EQUAL COUNTR 0) (RETURN NIL)))
         (PRIN2T "   where"))))
      (PROG (X)
        (SETQ X *VARLIS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((OR *FORT (CAR X))
             (PROGN
              (TERPRI* T)
              (COND ((NULL *FORT) (PRIN2* "      ")))
              (ASSGNPRI (CDDR X) (LIST (CADR X)) T)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (*SAVESTRUCTR
        (PROGN
         (COND
          ((ARRAYP SVAR)
           (PROGN
            (PUT SVAR 'ARRAY (MKARRAY1 (LIST (PLUS COUNTR 1)) 'ALGEBRAIC))
            (PUT SVAR 'DIMENSION (LIST (PLUS COUNTR 1))))))
         (PROG (X)
           (SETQ X *VARLIS)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (COND
               ((CAR X)
                (SETK2 (CADR X)
                 (MK*SQ (CONS (LIST (CONS (CONS (CAR X) 1) 1)) 1))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))))))) 
(PUT 'PREPSTRUCT*SQ 'NUMBER-OF-ARGS 1) 
(PUT 'PREPSTRUCT*SQ 'DEFINED-ON-LINE '174) 
(PUT 'PREPSTRUCT*SQ 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSTRUCT*SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPSTRUCT*SQ (U)
    (COND
     ((EQCAR U '*SQ)
      (CONS (PREPSTRUCTF (CAR (CADR U))) (PREPSTRUCTF (CDR (CADR U)))))
     (T U))) 
(PUT 'PREPSTRUCTF 'NUMBER-OF-ARGS 1) 
(PUT 'PREPSTRUCTF 'DEFINED-ON-LINE '179) 
(PUT 'PREPSTRUCTF 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSTRUCTF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPSTRUCTF (U)
    (COND ((NULL U) NIL) ((OR (ATOM U) (ATOM (CAR U))) U)
          (T
           (PROG (X Y)
             (SETQ X (CAAAR U))
             (COND
              ((SFP X)
               (COND ((SETQ Y (ASSOC X *VARLIS)) (SETQ X (CADR Y)))
                     (T
                      (SETQ X
                              (PREPSTRUCTK (PREPSQ* (CONS (PREPSTRUCTF X) 1))
                               (PREPSTRUCTVAR) X)))))
              ((AND (NOT (ATOM X)) (NOT (ATOMLIS (CDR X))))
               (COND ((SETQ Y (ASSOC X *VARLIS)) (SETQ X (CADR Y)))
                     (T (SETQ X (PREPSTRUCTK X (PREPSTRUCTVAR) X))))))
             (RETURN
              (CONS (CONS (CONS X (CDAAR U)) (PREPSTRUCTF (CDAR U)))
                    (PREPSTRUCTF (CDR U)))))))) 
(PUT 'PREPSTRUCTK 'NUMBER-OF-ARGS 3) 
(PUT 'PREPSTRUCTK 'DEFINED-ON-LINE '199) 
(PUT 'PREPSTRUCTK 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSTRUCTK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREPSTRUCTK (U ID V)
    (PROG (X)
      (COND
       ((SETQ X (PREPSUBCHK1 U *VARLIS ID))
        (RPLACD X (CONS (CONS V (CONS ID U)) (CDR X))))
       ((SETQ X (PREPSUBCHK2 U *VARLIS))
        (SETQ *VARLIS (CONS (CONS V (CONS ID X)) *VARLIS)))
       (T (SETQ *VARLIS (CONS (CONS V (CONS ID U)) *VARLIS))))
      (RETURN ID))) 
(PUT 'PREPSUBCHK1 'NUMBER-OF-ARGS 3) 
(PUT 'PREPSUBCHK1 'DEFINED-ON-LINE '210) 
(PUT 'PREPSUBCHK1 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSUBCHK1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREPSUBCHK1 (U V ID)
    (PROG (W)
      (PROG ()
       WHILELABEL
        (COND ((NOT V) (RETURN NIL)))
        (PROGN
         (AND (SMEMBER U (CDDAR V))
              (PROGN (SETQ W V) (RPLACD (CDAR V) (SUBST ID U (CDDAR V)))))
         (SETQ V (CDR V)))
        (GO WHILELABEL))
      (RETURN W))) 
(PUT 'PREPSUBCHK2 'NUMBER-OF-ARGS 2) 
(PUT 'PREPSUBCHK2 'DEFINED-ON-LINE '219) 
(PUT 'PREPSUBCHK2 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSUBCHK2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREPSUBCHK2 (U V)
    (PROG (BOOL)
      (PROG (X)
        (SETQ X V)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (AND (SMEMBER (CDDR X) U)
                (PROGN (SETQ BOOL T) (SETQ U (SUBST (CADR X) (CDDR X) U)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND (BOOL (RETURN U)) (T (RETURN NIL))))) 
(PUT 'PREPSTRUCTVAR 'NUMBER-OF-ARGS 0) 
(PUT 'PREPSTRUCTVAR 'DEFINED-ON-LINE '227) 
(PUT 'PREPSTRUCTVAR 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'PREPSTRUCTVAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PREPSTRUCTVAR NIL
    (PROG ()
      (SETQ COUNTR (PLUS COUNTR 1))
      (RETURN
       (COND ((ARRAYP SVAR) (LIST SVAR COUNTR))
             (T (COMPRESS (APPEND (EXPLODE SVAR) (EXPLODE COUNTR)))))))) 
(PUT 'REMREDUNDANCY 'NUMBER-OF-ARGS 1) 
(PUT 'REMREDUNDANCY 'DEFINED-ON-LINE '234) 
(PUT 'REMREDUNDANCY 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'REMREDUNDANCY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMREDUNDANCY (SETQLIST)
    (PROG (LSL LHS RHS RELEVANT J VAR FREQ K FIRSTOCC TEMPLIST)
      (SETQ LSL (LENGTH SETQLIST))
      (SETQ LHS (MKVECT LSL))
      (SETQ RHS (MKVECT LSL))
      (SETQ RELEVANT (MKVECT LSL))
      (SETQ J 0)
      (SETQ VAR (EXPLODE SVAR))
      (PROG (ITEM)
        (SETQ ITEM SETQLIST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (PUTV LHS (SETQ J (PLUS J 1)) (CADR ITEM))
            (PUTV RHS J (CADDR ITEM))
            (COND
             ((AND (ATOM (CADR ITEM)) (EQUAL (LETTERPARTS (CADR ITEM)) VAR))
              (PUTV RELEVANT J T)))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LSL J)) (RETURN NIL)))
        (COND
         ((GETV RELEVANT J)
          (PROGN
           (SETQ VAR (GETV LHS J))
           (SETQ FREQ 0)
           (SETQ K J)
           (SETQ FIRSTOCC 0)
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND (EQUAL FREQ 0) (LESSP K LSL))) (RETURN NIL)))
             (PROGN
              (COND
               ((AND
                 (EQUAL
                  (SETQ FREQ (NUMBEROFOCCS VAR (GETV RHS (SETQ K (PLUS K 1)))))
                  1)
                 (EQUAL FIRSTOCC 0))
                (PROGN (SETQ FIRSTOCC K) (SETQ FREQ 0))))
              (COND
               ((AND (GREATERP FIRSTOCC 0) (GREATERP FREQ 0))
                (SETQ FIRSTOCC 0))))
             (GO WHILELABEL))
           (COND
            ((EQUAL FIRSTOCC 0)
             (SETQ TEMPLIST
                     (CONS (LIST 'SETQ (GETV LHS J) (GETV RHS J)) TEMPLIST)))
            (T
             (PUTV RHS FIRSTOCC
                   (SUBST (GETV RHS J) VAR (GETV RHS FIRSTOCC)))))))
         (T
          (SETQ TEMPLIST
                  (CONS (LIST 'SETQ (GETV LHS J) (GETV RHS J)) TEMPLIST))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN (REVERSE TEMPLIST)))) 
(PUT 'LETTERPARTS 'NUMBER-OF-ARGS 1) 
(PUT 'LETTERPARTS 'DEFINED-ON-LINE '270) 
(PUT 'LETTERPARTS 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'LETTERPARTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LETTERPARTS (NAME)
    (PROG (LETTERS)
      (SETQ LETTERS (REVERSIP (EXPLODE NAME)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (DIGIT (CAR LETTERS))) (RETURN NIL)))
        (SETQ LETTERS (CDR LETTERS))
        (GO WHILELABEL))
      (RETURN (REVERSIP LETTERS)))) 
(PUT 'NUMBEROFOCCS 'NUMBER-OF-ARGS 2) 
(PUT 'NUMBEROFOCCS 'DEFINED-ON-LINE '281) 
(PUT 'NUMBEROFOCCS 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'NUMBEROFOCCS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUMBEROFOCCS (VAR EXPRESSION)
    (COND ((ATOM EXPRESSION) (COND ((EQUAL VAR EXPRESSION) 1) (T 0)))
          (T
           (PLUS
            (COND ((CDR EXPRESSION) (NUMBEROFOCCS VAR (CDR EXPRESSION))) (T 0))
            (COND ((EQUAL VAR (CAR EXPRESSION)) 1)
                  ((NOT (ATOM (CAR EXPRESSION)))
                   (NUMBEROFOCCS VAR (CAR EXPRESSION)))
                  (T 0)))))) 
(PUT 'ALGSTRUCTREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ALGSTRUCTREVAL 'DEFINED-ON-LINE '307) 
(PUT 'ALGSTRUCTREVAL 'DEFINED-IN-FILE 'SCOPE/CODSTR.RED) 
(PUT 'ALGSTRUCTREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGSTRUCTREVAL (U)
    (PROG (ALGPRI NAME PERIOD RES NARGS)
      (SETQ NARGS 0)
      (SETQ NARGS (LENGTH U))
      (SETQ NAME
              (COND ((AND (EQUAL NARGS 1) (GETD 'NEWSYM)) (FNEWSYM))
                    ((EQUAL NARGS 2) (CADR U)) (T '**ERROR**)))
      (COND
       ((EQ NAME '**ERROR**) (REDERR "WRONG NUMBER OF ARGUMENTS ALGSTRUCTR"))
       (T
        (PROGN
         (SETQ ALGPRI *ALGPRI)
         (SETQ PERIOD *PERIOD)
         (SETQ *ALGPRI (SETQ *PERIOD NIL))
         (SETQ RES (APPLY 'GSTRUCTR (LIST (CDAR U) NAME)))
         (SETQ *PERIOD PERIOD)
         (COND
          ((SETQ *ALGPRI ALGPRI)
           (RETURN
            (ALGRESULTS1
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL RES)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL)))
                                 (CAR EL))
                                NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL))) (CAR EL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))
          (T (RETURN RES)))))))) 
(PUT 'ALGSTRUCTR 'PSOPFN 'ALGSTRUCTREVAL) 
(ENDMODULE) 