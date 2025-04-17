(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DPMAT)) 
(PUT 'DPMAT_ROWS 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_ROWS 'DEFINED-ON-LINE '61) 
(PUT 'DPMAT_ROWS 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_ROWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_ROWS (M) (CADR M)) 
(PUT 'DPMAT_COLS 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_COLS 'DEFINED-ON-LINE '62) 
(PUT 'DPMAT_COLS 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_COLS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_COLS (M) (CADDR M)) 
(PUT 'DPMAT_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_LIST 'DEFINED-ON-LINE '63) 
(PUT 'DPMAT_LIST 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_LIST (M) (CADDDR M)) 
(PUT 'DPMAT_COLDEGS 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_COLDEGS 'DEFINED-ON-LINE '64) 
(PUT 'DPMAT_COLDEGS 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_COLDEGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_COLDEGS (M) (NTH M 5)) 
(PUT 'DPMAT_GBTAG 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_GBTAG 'DEFINED-ON-LINE '65) 
(PUT 'DPMAT_GBTAG 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_GBTAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_GBTAG (M) (NTH M 6)) 
(PUT 'DPMAT_ROWDEGREES 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_ROWDEGREES 'DEFINED-ON-LINE '69) 
(PUT 'DPMAT_ROWDEGREES 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_ROWDEGREES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_ROWDEGREES (M)
    ((LAMBDA (L)
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (DPMAT_LIST M))
        STARTOVER
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (X)
                    (COND
                     ((AND (GREATERP (BAS_NR X) 0) (BAS_DPOLY X))
                      (LIST
                       (CONS (BAS_NR X)
                             (MO_GETDEGREE (DP_LMON (BAS_DPOLY X)) L))))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ X (CDR X))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (X)
                    (COND
                     ((AND (GREATERP (BAS_NR X) 0) (BAS_DPOLY X))
                      (LIST
                       (CONS (BAS_NR X)
                             (MO_GETDEGREE (DP_LMON (BAS_DPOLY X)) L))))))
                  (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ X (CDR X))
         (GO LOOPLABEL)))
     (DPMAT_COLDEGS M))) 
(PUT 'DPMAT_MAKE 'NUMBER-OF-ARGS 5) 
(PUT 'DPMAT_MAKE 'DEFINED-ON-LINE '76) 
(PUT 'DPMAT_MAKE 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_MAKE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DPMAT_MAKE (R C BAS DEGS GBTAG) (LIST 'DPMAT R C BAS DEGS GBTAG)) 
(PUT 'DPMAT_ELEMENT 'NUMBER-OF-ARGS 3) 
(PUT 'DPMAT_ELEMENT 'DEFINED-ON-LINE '79) 
(PUT 'DPMAT_ELEMENT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_ELEMENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DPMAT_ELEMENT (R C MMAT)
    (DP_NEWORDER (DP_COMP C (BAS_DPOLY (BAS_GETELEMENT R (DPMAT_LIST MMAT)))))) 
(PUT 'DPMAT_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_PRINT 'DEFINED-ON-LINE '84) 
(PUT 'DPMAT_PRINT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_PRINT (M) (MATHPRINT (DPMAT_2A M))) 
(PUT 'GETLEADTERMS* 'NUMBER-OF-ARGS 1) 
(PUT 'GETLEADTERMS* 'DEFINED-ON-LINE '86) 
(PUT 'GETLEADTERMS* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'GETLEADTERMS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETLEADTERMS* (M)
    ((LAMBDA (CALI=DEGREES)
       (PROG (B)
         (SETQ B
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (DPMAT_LIST M))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (BAS_MAKE (BAS_NR X)
                                        (LIST (CAR (BAS_DPOLY X)))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (BAS_MAKE (BAS_NR X)
                                (LIST (CAR (BAS_DPOLY X)))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M) B CALI=DEGREES T))))
     (DPMAT_COLDEGS M))) 
(PUT 'SAVEMAT* 'NUMBER-OF-ARGS 2) 
(PUT 'SAVEMAT* 'DEFINED-ON-LINE '96) 
(PUT 'SAVEMAT* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'SAVEMAT* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAVEMAT* (M NAME)
    (PROG (NAT C)
      (COND ((NOT (OR (STRINGP NAME) (IDP NAME))) (TYPERR NAME "file name")))
      (COND ((NOT (EQCAR M 'DPMAT)) (TYPERR M "dpmat")))
      (SETQ NAT *NAT)
      (SETQ *NAT NIL)
      (PROGN (PRIN2 "Saving as ") (PRIN2 NAME) NIL)
      (OUT (LIST NAME))
      (PROGN (PRIN2 "algebraic(setring ") NIL)
      (MATHPRINT (RING_2A CALI=BASERING))
      (PROGN (PRIN2 ")$") NIL)
      (PROGN (PRIN2 "algebraic(<<basis :=") NIL)
      (DPMAT_PRINT M)
      (COND ((EQUAL (DPMAT_COLS M) 0) (PROGN (PRIN2 "$") NIL)))
      (PROGN (PRIN2 ">>)$") NIL)
      (COND
       ((SETQ C (DPMAT_COLDEGS M))
        (PROGN
         (PROGN (PRIN2 "algebraic(degrees:=") NIL)
         (MATHPRINT
          (MOID_2A
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X C)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))
         (PROGN (PRIN2 ")$") NIL)
         NIL)))
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (SHUT (LIST NAME))
      (TERPRI)
      (SETQ *NAT NAT))) 
(PUT 'INITMAT* 'NUMBER-OF-ARGS 1) 
(PUT 'INITMAT* 'DEFINED-ON-LINE '120) 
(PUT 'INITMAT* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'INITMAT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INITMAT* (NAME)
    (COND ((NOT (OR (STRINGP NAME) (IDP NAME))) (TYPERR NAME "file name"))
          (T
           (PROG (M C I)
             (SETQ I 0)
             (PROGN (PRIN2 "Initializing ") (PRIN2 NAME) NIL)
             (TERPRI)
             (IN (LIST NAME))
             (SETQ M (REVAL1 'BASIS T))
             (SETQ CALI=DEGREES NIL)
             (COND
              ((EQCAR M 'LIST)
               (PROGN
                (SETQ M (BAS_FROM_A M))
                (SETQ M (DPMAT_MAKE (LENGTH M) 0 M NIL NIL))))
              ((EQCAR M 'MAT)
               (PROGN
                (SETQ C (MOID_FROM_A (REVAL1 'DEGREES T)))
                (SETQ I 0)
                (SETQ CALI=DEGREES
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X C)
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X)
                                              (PROGN
                                               (SETQ I (PLUS I 1))
                                               (CONS I X)))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (PROGN (SETQ I (PLUS I 1)) (CONS I X)))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ M (DPMAT_FROM_A M))
                NIL))
              (T (TYPERR M "basis or matrix")))
             (DPMAT_PRINT M)
             (RETURN M))))) 
(FLAG '(SAVEMAT) 'OPFN) 
(PUT 'SAVEMAT 'NUMBER-OF-ARGS 2) 
(PUT 'SAVEMAT 'DEFINED-ON-LINE '141) 
(PUT 'SAVEMAT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'SAVEMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAVEMAT (M NAME)
    (COND ((EQUAL *MODE 'ALGEBRAIC) (SAVEMAT* (DPMAT_FROM_A M) NAME))
          (T (SAVEMAT* M NAME)))) 
(FLAG '(INITMAT) 'OPFN) 
(PUT 'INITMAT 'NUMBER-OF-ARGS 1) 
(PUT 'INITMAT 'DEFINED-ON-LINE '146) 
(PUT 'INITMAT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'INITMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INITMAT (NAME)
    (COND ((EQUAL *MODE 'ALGEBRAIC) (DPMAT_2A (INITMAT* NAME)))
          (T (INITMAT* NAME)))) 
(PUT 'DPMAT=DPSUBST 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT=DPSUBST 'DEFINED-ON-LINE '152) 
(PUT 'DPMAT=DPSUBST 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT=DPSUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT=DPSUBST (A B)
    (PROG (V)
      (PROG (X)
        (SETQ X B)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ V (DP_SUM V (DP_PROD (DP_COMP (BAS_NR X) A) (BAS_DPOLY X)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN V))) 
(PUT 'DPMAT_MULT 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_MULT 'DEFINED-ON-LINE '160) 
(PUT 'DPMAT_MULT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_MULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_MULT (A B)
    (COND
     ((NOT (EQN (DPMAT_COLS A) (DPMAT_ROWS B)))
      (RERROR 'DPMAT 1 " matrices don't match for MATMULT"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_COLS B)
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (DPMAT_LIST A))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (BAS_MAKE (BAS_NR X)
                                 (DPMAT=DPSUBST (BAS_DPOLY X) (DPMAT_LIST B))))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X)
                        (BAS_MAKE (BAS_NR X)
                         (DPMAT=DPSUBST (BAS_DPOLY X) (DPMAT_LIST B))))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          CALI=DEGREES NIL))
       (DPMAT_COLDEGS B))))) 
(PUT 'DPMAT_TIMES_DPOLY 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_TIMES_DPOLY 'DEFINED-ON-LINE '171) 
(PUT 'DPMAT_TIMES_DPOLY 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_TIMES_DPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_TIMES_DPOLY (F M)
    ((LAMBDA (CALI=DEGREES)
       (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M)
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X (DPMAT_LIST M))
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (BAS_MAKE1 (BAS_NR X) (DP_PROD F (BAS_DPOLY X))
                               (DP_PROD F (BAS_REP X))))
                            (CAR X))
                           NIL)))
         LOOPLABEL
          (SETQ X (CDR X))
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (X)
                      (BAS_MAKE1 (BAS_NR X) (DP_PROD F (BAS_DPOLY X))
                       (DP_PROD F (BAS_REP X))))
                    (CAR X))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        CALI=DEGREES NIL))
     (DPMAT_COLDEGS M))) 
(PUT 'DPMAT_NEG 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_NEG 'DEFINED-ON-LINE '179) 
(PUT 'DPMAT_NEG 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_NEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_NEG (A)
    ((LAMBDA (CALI=DEGREES)
       (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_COLS A)
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X (DPMAT_LIST A))
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (BAS_MAKE1 (BAS_NR X) (DP_NEG (BAS_DPOLY X))
                               (DP_NEG (BAS_REP X))))
                            (CAR X))
                           NIL)))
         LOOPLABEL
          (SETQ X (CDR X))
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (X)
                      (BAS_MAKE1 (BAS_NR X) (DP_NEG (BAS_DPOLY X))
                       (DP_NEG (BAS_REP X))))
                    (CAR X))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        CALI=DEGREES (DPMAT_GBTAG A)))
     (DPMAT_COLDEGS A))) 
(PUT 'DPMAT_DIFF 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_DIFF 'DEFINED-ON-LINE '189) 
(PUT 'DPMAT_DIFF 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_DIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_DIFF (A B) (DPMAT_SUM A (DPMAT_NEG B))) 
(PUT 'DPMAT_SUM 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_SUM 'DEFINED-ON-LINE '193) 
(PUT 'DPMAT_SUM 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_SUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_SUM (A B)
    (COND
     ((NOT
       (AND (EQN (DPMAT_ROWS A) (DPMAT_ROWS B))
            (EQN (DPMAT_COLS A) (DPMAT_COLS B))
            (EQUAL (DPMAT_COLDEGS A) (DPMAT_COLDEGS B))))
      (RERROR 'DPMAT 2 "matrices don't match for MATSUM"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (PROG (U V W)
           (SETQ U (DPMAT_LIST A))
           (SETQ V (DPMAT_LIST B))
           (SETQ W
                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                     (SETQ I 1)
                     (COND
                      ((MINUSP (DIFFERENCE (DPMAT_ROWS A) I)) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (Y X)
                                         (BAS_MAKE1 I
                                          (DP_SUM (BAS_DPOLY X) (BAS_DPOLY Y))
                                          (DP_SUM (BAS_REP X) (BAS_REP Y))))
                                       (BAS_GETELEMENT I V)
                                       (BAS_GETELEMENT I U))
                                      NIL)))
                    LOOPLABEL
                     (SETQ I (PLUS2 I 1))
                     (COND
                      ((MINUSP (DIFFERENCE (DPMAT_ROWS A) I))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (Y X)
                                 (BAS_MAKE1 I
                                  (DP_SUM (BAS_DPOLY X) (BAS_DPOLY Y))
                                  (DP_SUM (BAS_REP X) (BAS_REP Y))))
                               (BAS_GETELEMENT I V) (BAS_GETELEMENT I U))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN
            (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_COLS A) W CALI=DEGREES NIL))))
       (DPMAT_COLDEGS A))))) 
(PUT 'DPMAT_FROM_DPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_FROM_DPOLY 'DEFINED-ON-LINE '210) 
(PUT 'DPMAT_FROM_DPOLY 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_FROM_DPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_FROM_DPOLY (P)
    (COND ((NULL P) (DPMAT_MAKE 0 0 NIL NIL T))
          (T (DPMAT_MAKE 1 0 (LIST (BAS_MAKE 1 P)) NIL T)))) 
(PUT 'DPMAT_UNIT 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_UNIT 'DEFINED-ON-LINE '214) 
(PUT 'DPMAT_UNIT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_UNIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_UNIT (N DEGS)
    (DPMAT_MAKE N N
     (PROG (I FORALL-RESULT FORALL-ENDPTR)
       (SETQ I 1)
       (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR (CONS (BAS_MAKE I (DP_FROM_EI I)) NIL)))
      LOOPLABEL
       (SETQ I (PLUS2 I 1))
       (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS (BAS_MAKE I (DP_FROM_EI I)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     DEGS T)) 
(PUT 'DPMAT_UNITIDEAL? 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_UNITIDEAL? 'DEFINED-ON-LINE '218) 
(PUT 'DPMAT_UNITIDEAL? 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_UNITIDEAL? 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_UNITIDEAL? (M)
    (AND (EQUAL (DPMAT_COLS M) 0) (NULL (MATOP_PSEUDOMOD (DP_FI 1) M)))) 
(PUT 'DPMAT_TRANSPOSE 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_TRANSPOSE 'DEFINED-ON-LINE '221) 
(PUT 'DPMAT_TRANSPOSE 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_TRANSPOSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_TRANSPOSE (M)
    (COND ((EQUAL (DPMAT_COLS M) 0) (DPMAT=TRANSPOSE (IDEAL2MAT* M)))
          (T (DPMAT=TRANSPOSE M)))) 
(PUT 'DPMAT=TRANSPOSE 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT=TRANSPOSE 'DEFINED-ON-LINE '226) 
(PUT 'DPMAT=TRANSPOSE 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT=TRANSPOSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT=TRANSPOSE (M)
    ((LAMBDA (CALI=DEGREES)
       (PROG (B P Q)
         (SETQ CALI=DEGREES
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (DPMAT_ROWDEGREES M))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (CONS (CAR X) (MO_NEG (CDR X))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (CONS (CAR X) (MO_NEG (CDR X))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DPMAT_COLS M) I)) (RETURN NIL)))
           (PROGN
            (SETQ P NIL)
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DPMAT_ROWS M) J)) (RETURN NIL)))
              (PROGN
               (SETQ Q (DPMAT_ELEMENT J I M))
               (COND (Q (SETQ P (DP_SUM P (DP_TIMES_EI J Q))))))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (COND (P (SETQ B (CONS (BAS_MAKE I P) B))))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (RETURN
          (DPMAT_MAKE (DPMAT_COLS M) (DPMAT_ROWS M) (REVERSE B) CALI=DEGREES
           NIL))))
     CALI=DEGREES)) 
(PUT 'IDEAL2MAT* 'NUMBER-OF-ARGS 1) 
(PUT 'IDEAL2MAT* 'DEFINED-ON-LINE '243) 
(PUT 'IDEAL2MAT* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'IDEAL2MAT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDEAL2MAT* (U)
    (COND
     ((NEQ (DPMAT_COLS U) 0)
      (RERROR 'DPMAT 4 "IDEAL2MAT only for ideal bases"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (DPMAT_MAKE (DPMAT_ROWS U) 1
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (DPMAT_LIST U))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (BAS_MAKE (BAS_NR X)
                                 (DP_TIMES_EI 1 (BAS_DPOLY X))))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X)
                        (BAS_MAKE (BAS_NR X) (DP_TIMES_EI 1 (BAS_DPOLY X))))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          NIL (DPMAT_GBTAG U)))
       NIL)))) 
(PUT 'DPMAT_RENUMBER 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_RENUMBER 'DEFINED-ON-LINE '252) 
(PUT 'DPMAT_RENUMBER 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_RENUMBER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_RENUMBER (OLD)
    (COND
     ((NULL (DPMAT_LIST OLD)) (CONS OLD (DPMAT_UNIT (DPMAT_ROWS OLD) NIL)))
     (T
      ((LAMBDA (CALI=DEGREES)
         (PROG (I U V W)
           (SETQ CALI=DEGREES (DPMAT_ROWDEGREES OLD))
           (SETQ I 0)
           (SETQ U (DPMAT_LIST OLD))
           (PROG ()
            WHILELABEL
             (COND ((NOT U) (RETURN NIL)))
             (PROGN
              (SETQ I (PLUS I 1))
              (SETQ V (CONS (BAS_NEWNUMBER I (CAR U)) V))
              (SETQ W (CONS (BAS_MAKE I (DP_FROM_EI (BAS_NR (CAR U)))) W))
              (SETQ U (CDR U)))
             (GO WHILELABEL))
           (RETURN
            (CONS
             (DPMAT_MAKE I (DPMAT_COLS OLD) (REVERSE V) (DPMAT_COLDEGS OLD)
              (DPMAT_GBTAG OLD))
             (DPMAT_MAKE I (DPMAT_ROWS OLD) (REVERSE W) CALI=DEGREES T)))))
       CALI=DEGREES)))) 
(PUT 'MATHOMOGENIZE* 'NUMBER-OF-ARGS 2) 
(PUT 'MATHOMOGENIZE* 'DEFINED-ON-LINE '267) 
(PUT 'MATHOMOGENIZE* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'MATHOMOGENIZE* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATHOMOGENIZE* (M VAR)
    ((LAMBDA (CALI=DEGREES)
       (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M)
        (BAS_HOMOGENIZE (DPMAT_LIST M) VAR) CALI=DEGREES NIL))
     (DPMAT_COLDEGS M))) 
(FLAG '(MATHOMOGENIZE) 'OPFN) 
(PUT 'MATHOMOGENIZE 'NUMBER-OF-ARGS 2) 
(PUT 'MATHOMOGENIZE 'DEFINED-ON-LINE '274) 
(PUT 'MATHOMOGENIZE 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'MATHOMOGENIZE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATHOMOGENIZE (M V)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (MATHOMOGENIZE* (DPMAT_FROM_A (REVAL1 M T)) V)))
     (T (MATDEHOMOGENIZE* M V)))) 
(PUT 'MATDEHOMOGENIZE* 'NUMBER-OF-ARGS 2) 
(PUT 'MATDEHOMOGENIZE* 'DEFINED-ON-LINE '280) 
(PUT 'MATDEHOMOGENIZE* 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'MATDEHOMOGENIZE* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATDEHOMOGENIZE* (M VAR)
    ((LAMBDA (CALI=DEGREES)
       (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M)
        (BAS_DEHOMOGENIZE (DPMAT_LIST M) VAR) CALI=DEGREES NIL))
     (DPMAT_COLDEGS M))) 
(PUT 'DPMAT_SIEVE 'NUMBER-OF-ARGS 3) 
(PUT 'DPMAT_SIEVE 'DEFINED-ON-LINE '286) 
(PUT 'DPMAT_SIEVE 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_SIEVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DPMAT_SIEVE (M VARS GBTAG)
    ((LAMBDA (CALI=DEGREES)
       ((LAMBDA (X)
          (DPMAT_MAKE (LENGTH X) (DPMAT_COLS M) X CALI=DEGREES GBTAG))
        (BAS_SIEVE (DPMAT_LIST M) VARS)))
     (DPMAT_COLDEGS M))) 
(PUT 'DPMAT_NEWORDER 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_NEWORDER 'DEFINED-ON-LINE '293) 
(PUT 'DPMAT_NEWORDER 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_NEWORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_NEWORDER (M GBTAG)
    (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M) (BAS_NEWORDER (DPMAT_LIST M))
     CALI=DEGREES GBTAG)) 
(PUT 'DPMAT_ZERO? 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_ZERO? 'DEFINED-ON-LINE '299) 
(PUT 'DPMAT_ZERO? 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_ZERO? 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_ZERO? (M) (BAS_ZERO? (DPMAT_LIST M))) 
(PUT 'DPMAT_PROJECT 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_PROJECT 'DEFINED-ON-LINE '303) 
(PUT 'DPMAT_PROJECT 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_PROJECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_PROJECT (M K)
    (DPMAT_MAKE (DPMAT_ROWS M) K
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X (DPMAT_LIST M))
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (BAS_MAKE (BAS_NR X) (DP_PROJECT (BAS_DPOLY X) K)))
                         (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (X)
                   (BAS_MAKE (BAS_NR X) (DP_PROJECT (BAS_DPOLY X) K)))
                 (CAR X))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     (DPMAT_COLDEGS M) NIL)) 
(PUT 'DPMAT_2A 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_2A 'DEFINED-ON-LINE '312) 
(PUT 'DPMAT_2A 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_2A (M)
    (COND ((EQUAL (DPMAT_COLS M) 0) (BAS_2A (DPMAT_LIST M)))
          (T
           (CONS 'MAT
                 (COND
                  ((EQUAL (DPMAT_ROWS M) 0)
                   (LIST
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J 1)
                      (COND
                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J)) (RETURN NIL)))
                      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                     LOOPLABEL
                      (SETQ J (PLUS2 J 1))
                      (COND
                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
                  (T
                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                     (SETQ I 1)
                     (COND
                      ((MINUSP (DIFFERENCE (DPMAT_ROWS M) I)) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ J 1)
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DPMAT_COLS M) J))
                                          (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         (DP_2A
                                                          (DPMAT_ELEMENT I J
                                                           M))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ J (PLUS2 J 1))
                                        (COND
                                         ((MINUSP
                                           (DIFFERENCE (DPMAT_COLS M) J))
                                          (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 (DP_2A (DPMAT_ELEMENT I J M))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))
                                      NIL)))
                    LOOPLABEL
                     (SETQ I (PLUS2 I 1))
                     (COND
                      ((MINUSP (DIFFERENCE (DPMAT_ROWS M) I))
                       (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                (SETQ J 1)
                                (COND
                                 ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                  (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 (DP_2A (DPMAT_ELEMENT I J M))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ J (PLUS2 J 1))
                                (COND
                                 ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS (DP_2A (DPMAT_ELEMENT I J M))
                                              NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))))) 
(PUT 'DPMAT_FROM_A 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_FROM_A 'DEFINED-ON-LINE '322) 
(PUT 'DPMAT_FROM_A 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_FROM_A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_FROM_A (M)
    (COND
     ((EQCAR M 'MAT)
      (PROG (I U P)
        (SETQ I 0)
        (SETQ M (CDR M))
        (PROG (X)
          (SETQ X M)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROGN
              (SETQ I 1)
              (SETQ P NIL)
              (PROG (Y)
                (SETQ Y X)
               LAB
                (COND ((NULL Y) (RETURN NIL)))
                ((LAMBDA (Y)
                   (PROGN
                    (SETQ P
                            (DP_SUM P
                             (DP_TIMES_EI I (DP_FROM_A (REVAL1 Y T)))))
                    (SETQ I (PLUS I 1))))
                 (CAR Y))
                (SETQ Y (CDR Y))
                (GO LAB))
              (SETQ U (CONS (BAS_MAKE 0 P) U))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (RETURN
         (DPMAT_MAKE (LENGTH M) (LENGTH (CAR M)) (BAS_RENUMBER (REVERSIP U))
          CALI=DEGREES NIL))))
     ((EQCAR M 'LIST)
      ((LAMBDA (CALI=DEGREES)
         (PROG (X)
           (SETQ X (BAS_FROM_A (REVAL1 M T)))
           (RETURN (DPMAT_MAKE (LENGTH X) 0 X NIL NIL))))
       NIL))
     (T (TYPERR M "polynomial list or matrix")))) 
(PUT 'DPMAT_SUB 'NUMBER-OF-ARGS 2) 
(PUT 'DPMAT_SUB 'DEFINED-ON-LINE '344) 
(PUT 'DPMAT_SUB 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DPMAT_SUB (A M)
    ((LAMBDA (CALI=DEGREES) (DPMAT_FROM_A (SUBEVAL1 A (DPMAT_2A M))))
     (DPMAT_COLDEGS M))) 
(PUT 'DPMAT_DET 'NUMBER-OF-ARGS 1) 
(PUT 'DPMAT_DET 'DEFINED-ON-LINE '352) 
(PUT 'DPMAT_DET 'DEFINED-IN-FILE 'CALI/DPMAT.RED) 
(PUT 'DPMAT_DET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DPMAT_DET (M)
    (COND ((NEQ (DPMAT_ROWS M) (DPMAT_COLS M)) (REDERR "non-square matrix"))
          (T (DP_FROM_A (PREPF (CAR (DETQ (MATSM (DPMAT_2A M))))))))) 
(ENDMODULE) 