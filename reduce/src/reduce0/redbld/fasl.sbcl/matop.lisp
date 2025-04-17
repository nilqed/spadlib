(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATOP)) 
(PUT 'MATOP=TESTDPMATLIST 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=TESTDPMATLIST 'DEFINED-ON-LINE '42) 
(PUT 'MATOP=TESTDPMATLIST 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=TESTDPMATLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=TESTDPMATLIST (L)
    (COND ((NULL L) (REDERR "Empty DPMAT list"))
          (T
           (PROG (C D)
             (PROG (X)
               (SETQ X L)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X) (COND ((NOT (EQCAR X 'DPMAT)) (TYPERR X "DPMAT"))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (SETQ C (DPMAT_COLS (CAR L)))
             (SETQ D (DPMAT_COLDEGS (CAR L)))
             (PROG (X)
               (SETQ X (CDR L))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (COND
                   ((NOT
                     (AND (EQN C (DPMAT_COLS X)) (EQUAL D (DPMAT_COLDEGS X))))
                    (REDERR "Matrices don't match in the DPMAT list"))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB)))))) 
(PUT 'MATAPPEND* 'NUMBER-OF-ARGS 1) 
(PUT 'MATAPPEND* 'DEFINED-ON-LINE '54) 
(PUT 'MATAPPEND* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATAPPEND* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATAPPEND* (L)
    ((LAMBDA (CALI=DEGREES)
       (PROG (U R)
         (MATOP=TESTDPMATLIST L)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS (CAR L)))
         (SETQ U (DPMAT_LIST (CAR L)))
         (SETQ R (DPMAT_ROWS (CAR L)))
         (PROG (Y)
           (SETQ Y (CDR L))
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROGN
               (SETQ U
                       (APPEND U
                               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ X (DPMAT_LIST Y))
                                 (COND ((NULL X) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (X)
                                                     (BAS_NEWNUMBER
                                                      (PLUS (BAS_NR X) R) X))
                                                   (CAR X))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ X (CDR X))
                                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X)
                                             (BAS_NEWNUMBER (PLUS (BAS_NR X) R)
                                              X))
                                           (CAR X))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
               (SETQ R (PLUS R (DPMAT_ROWS Y)))
               NIL))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (RETURN (DPMAT_MAKE R (DPMAT_COLS (CAR L)) U CALI=DEGREES NIL))))
     CALI=DEGREES)) 
(PUT 'MATAPPEND 'PSOPFN 'MATOP=MATAPPEND) 
(PUT 'MATOP=MATAPPEND 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=MATAPPEND 'DEFINED-ON-LINE '69) 
(PUT 'MATOP=MATAPPEND 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=MATAPPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=MATAPPEND (L)
    (DPMAT_2A
     (MATAPPEND*
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X L)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'MAT2LIST* 'NUMBER-OF-ARGS 1) 
(PUT 'MAT2LIST* 'DEFINED-ON-LINE '73) 
(PUT 'MAT2LIST* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MAT2LIST* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAT2LIST* (M)
    (COND ((EQUAL (DPMAT_COLS M) 0) M)
          (T
           ((LAMBDA (CALI=DEGREES)
              (PROG (X)
                (SETQ X
                        (BAS_RENUMBER
                         (BAS_ZERODELETE
                          (PROG (I FORALL-RESULT FORALL-ENDPTR)
                            (SETQ I 1)
                           STARTOVER
                            (COND
                             ((MINUSP (DIFFERENCE (DPMAT_ROWS M) I))
                              (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ J 1)
                                      (COND
                                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                        (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       (BAS_MAKE 0
                                                        (DPMAT_ELEMENT I J M))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ J (PLUS2 J 1))
                                      (COND
                                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                        (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               (BAS_MAKE 0
                                                (DPMAT_ELEMENT I J M))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ I (PLUS2 I 1))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND
                             ((MINUSP (DIFFERENCE (DPMAT_ROWS M) I))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ J 1)
                                      (COND
                                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                        (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       (BAS_MAKE 0
                                                        (DPMAT_ELEMENT I J M))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ J (PLUS2 J 1))
                                      (COND
                                       ((MINUSP (DIFFERENCE (DPMAT_COLS M) J))
                                        (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               (BAS_MAKE 0
                                                (DPMAT_ELEMENT I J M))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ I (PLUS2 I 1))
                            (GO LOOPLABEL)))))
                (RETURN
                 (DPMAT_MAKE (LENGTH X) 0 X NIL
                  (COND ((EQUAL (DPMAT_COLS M) 1) (DPMAT_GBTAG M)) (T NIL))))))
            NIL)))) 
(PUT 'MATSUM* 'NUMBER-OF-ARGS 1) 
(PUT 'MATSUM* 'DEFINED-ON-LINE '85) 
(PUT 'MATSUM* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATSUM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATSUM* (L) (INTERREDUCE* (MATAPPEND* L))) 
(PUT 'MATSUM 'PSOPFN 'MATOP=MATSUM) 
(PUT 'IDEALSUM 'PSOPFN 'MATOP=MATSUM) 
(PUT 'MATOP=MATSUM 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=MATSUM 'DEFINED-ON-LINE '91) 
(PUT 'MATOP=MATSUM 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=MATSUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=MATSUM (L)
    (DPMAT_2A
     (MATSUM*
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X L)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'MATOP=IDEALPROD2 'NUMBER-OF-ARGS 2) 
(PUT 'MATOP=IDEALPROD2 'DEFINED-ON-LINE '95) 
(PUT 'MATOP=IDEALPROD2 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=IDEALPROD2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATOP=IDEALPROD2 (A B)
    (COND
     ((OR (GREATERP (DPMAT_COLS A) 0) (GREATERP (DPMAT_COLS B) 0))
      (REDERR "IDEALPROD only for ideals"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (PROG (X)
           (SETQ X
                   (BAS_RENUMBER
                    (PROG (A1 FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A1 (DPMAT_LIST A))
                     STARTOVER
                      (COND ((NULL A1) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (A1)
                                 (PROG (B1 FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ B1 (DPMAT_LIST B))
                                   (COND ((NULL B1) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (B1)
                                                       (BAS_MAKE 0
                                                        (DP_PROD (BAS_DPOLY A1)
                                                         (BAS_DPOLY B1))))
                                                     (CAR B1))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ B1 (CDR B1))
                                   (COND ((NULL B1) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B1)
                                               (BAS_MAKE 0
                                                (DP_PROD (BAS_DPOLY A1)
                                                 (BAS_DPOLY B1))))
                                             (CAR B1))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR A1)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ A1 (CDR A1))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL A1) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (A1)
                                 (PROG (B1 FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ B1 (DPMAT_LIST B))
                                   (COND ((NULL B1) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (B1)
                                                       (BAS_MAKE 0
                                                        (DP_PROD (BAS_DPOLY A1)
                                                         (BAS_DPOLY B1))))
                                                     (CAR B1))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ B1 (CDR B1))
                                   (COND ((NULL B1) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (B1)
                                               (BAS_MAKE 0
                                                (DP_PROD (BAS_DPOLY A1)
                                                 (BAS_DPOLY B1))))
                                             (CAR B1))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR A1)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ A1 (CDR A1))
                      (GO LOOPLABEL))))
           (RETURN (INTERREDUCE* (DPMAT_MAKE (LENGTH X) 0 X NIL NIL)))))
       NIL)))) 
(PUT 'IDEALPROD* 'NUMBER-OF-ARGS 1) 
(PUT 'IDEALPROD* 'DEFINED-ON-LINE '106) 
(PUT 'IDEALPROD* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'IDEALPROD* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDEALPROD* (L)
    (COND ((NULL L) (REDERR "empty list in IDEALPROD"))
          ((EQUAL (LENGTH L) 1) (CAR L))
          (T
           (PROG (U)
             (SETQ U (CAR L))
             (PROG (X)
               (SETQ X (CDR L))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X) (SETQ U (MATOP=IDEALPROD2 U X))) (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN U))))) 
(PUT 'IDEALPROD 'PSOPFN 'MATOP=IDEALPROD) 
(PUT 'MATOP=IDEALPROD 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=IDEALPROD 'DEFINED-ON-LINE '117) 
(PUT 'MATOP=IDEALPROD 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=IDEALPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=IDEALPROD (L)
    (DPMAT_2A
     (IDEALPROD*
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X L)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'IDEALPOWER* 'NUMBER-OF-ARGS 2) 
(PUT 'IDEALPOWER* 'DEFINED-ON-LINE '121) 
(PUT 'IDEALPOWER* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'IDEALPOWER* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDEALPOWER* (A N)
    (COND
     ((OR (GREATERP (DPMAT_COLS A) 0) (NOT (FIXP N)) (LESSP N 0))
      (REDERR " Syntax : idealpower(ideal,integer)"))
     ((EQUAL N 0) (DPMAT_FROM_DPOLY (DP_FI 1)))
     (T
      (PROG (W)
        (SETQ W A)
        (PROG (I)
          (SETQ I 2)
         LAB
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
          (SETQ W (MATOP=IDEALPROD2 W A))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (RETURN W))))) 
(FLAG '(IDEALPOWER) 'OPFN) 
(PUT 'IDEALPOWER 'NUMBER-OF-ARGS 2) 
(PUT 'IDEALPOWER 'DEFINED-ON-LINE '131) 
(PUT 'IDEALPOWER 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'IDEALPOWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDEALPOWER (M L)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (IDEALPOWER* (DPMAT_FROM_A (REVAL1 M T)) L)))
     (T (IDEALPOWER* M L)))) 
(PUT 'MATOP=SHIFTDEGS 'NUMBER-OF-ARGS 2) 
(PUT 'MATOP=SHIFTDEGS 'DEFINED-ON-LINE '136) 
(PUT 'MATOP=SHIFTDEGS 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=SHIFTDEGS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATOP=SHIFTDEGS (D N)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X D)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (CONS (PLUS (CAR X) N) (CDR X))) (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (X) (CONS (PLUS (CAR X) N) (CDR X))) (CAR X))
                    NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'DIRECTSUM* 'NUMBER-OF-ARGS 1) 
(PUT 'DIRECTSUM* 'DEFINED-ON-LINE '140) 
(PUT 'DIRECTSUM* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'DIRECTSUM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIRECTSUM* (L)
    (COND ((NULL L) (REDERR "Empty DPMAT list"))
          (T
           ((LAMBDA (CALI=DEGREES)
              (PROG (R C U)
                (PROG (X)
                  (SETQ X L)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (COND ((NOT (EQCAR X 'DPMAT)) (TYPERR X "DPMAT"))
                           ((EQUAL (DPMAT_COLS X) 0)
                            (REDERR "DIRECTSUM only for modules"))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (SETQ C (SETQ R 0))
                (SETQ CALI=DEGREES NIL)
                (PROG (X)
                  (SETQ X L)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (PROGN
                      (SETQ CALI=DEGREES
                              (APPEND CALI=DEGREES
                                      (MATOP=SHIFTDEGS (DPMAT_COLDEGS X) C)))
                      (SETQ U
                              (APPEND U
                                      (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ Y (DPMAT_LIST X))
                                        (COND ((NULL Y) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (Y)
                                                            (BAS_MAKE
                                                             (PLUS (BAS_NR Y)
                                                                   R)
                                                             (DP_TIMES_EI C
                                                              (BAS_DPOLY Y))))
                                                          (CAR Y))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ Y (CDR Y))
                                        (COND
                                         ((NULL Y) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Y)
                                                    (BAS_MAKE
                                                     (PLUS (BAS_NR Y) R)
                                                     (DP_TIMES_EI C
                                                      (BAS_DPOLY Y))))
                                                  (CAR Y))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))))
                      (SETQ R (PLUS R (DPMAT_ROWS X)))
                      (SETQ C (PLUS C (DPMAT_COLS X)))
                      NIL))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (RETURN (DPMAT_MAKE R C U CALI=DEGREES NIL))))
            CALI=DEGREES)))) 
(PUT 'DIRECTSUM 'PSOPFN 'MATOP=DIRECTSUM) 
(PUT 'MATOP=DIRECTSUM 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=DIRECTSUM 'DEFINED-ON-LINE '162) 
(PUT 'MATOP=DIRECTSUM 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=DIRECTSUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=DIRECTSUM (L)
    (DPMAT_2A
     (DIRECTSUM*
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X L)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(FLAG '(DELETEUNITS) 'OPFN) 
(PUT 'DELETEUNITS 'NUMBER-OF-ARGS 1) 
(PUT 'DELETEUNITS 'DEFINED-ON-LINE '167) 
(PUT 'DELETEUNITS 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'DELETEUNITS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETEUNITS (M)
    (COND (*NOETHERIAN M)
          ((EQUAL *MODE 'ALGEBRAIC) (DPMAT_2A (DELETEUNITS* (DPMAT_FROM_A M))))
          (T (DELETEUNITS* M)))) 
(PUT 'DELETEUNITS* 'NUMBER-OF-ARGS 1) 
(PUT 'DELETEUNITS* 'DEFINED-ON-LINE '172) 
(PUT 'DELETEUNITS* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'DELETEUNITS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETEUNITS* (M)
    (COND ((OR *NOETHERIAN (GREATERP (DPMAT_COLS M) 0)) M)
          (T
           (DPMAT_MAKE (DPMAT_ROWS M) 0
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (DPMAT_LIST M))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (BAS_FACTORUNITS X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (BAS_FACTORUNITS X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            NIL (DPMAT_GBTAG M))))) 
(PUT 'INTERREDUCE* 'NUMBER-OF-ARGS 1) 
(PUT 'INTERREDUCE* 'DEFINED-ON-LINE '179) 
(PUT 'INTERREDUCE* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'INTERREDUCE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTERREDUCE* (M)
    ((LAMBDA (CALI=DEGREES)
       (PROG (U)
         (SETQ U (RED_INTERREDUCE (DPMAT_LIST M)))
         (RETURN
          (DPMAT_MAKE (LENGTH U) (DPMAT_COLS M) (BAS_RENUMBER U) CALI=DEGREES
           (DPMAT_GBTAG M)))))
     (DPMAT_COLDEGS M))) 
(FLAG '(INTERREDUCE) 'OPFN) 
(PUT 'INTERREDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'INTERREDUCE 'DEFINED-ON-LINE '187) 
(PUT 'INTERREDUCE 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'INTERREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTERREDUCE (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (INTERREDUCE* (DPMAT_FROM_A (REVAL1 M T)))))
     (T (INTERREDUCE* M)))) 
(PUT 'GBASIS* 'NUMBER-OF-ARGS 1) 
(PUT 'GBASIS* 'DEFINED-ON-LINE '193) 
(PUT 'GBASIS* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'GBASIS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBASIS* (M)
    (COND ((DPMAT_GBTAG M) M) (T (CAR (GROEB_STBASIS M T NIL NIL))))) 
(PUT 'TANGENTCONE 'PSOPFN 'MATOP=TANGENTCONE) 
(PUT 'MATOP=TANGENTCONE 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=TANGENTCONE 'DEFINED-ON-LINE '198) 
(PUT 'MATOP=TANGENTCONE 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=TANGENTCONE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=TANGENTCONE (M)
    (PROG (C)
      (INTF_TEST M)
      (SETQ M (CAR M))
      (INTF_GET M)
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (SETQ C (TANGENTCONE* C))
      (RETURN (DPMAT_2A C)))) 
(PUT 'TANGENTCONE* 'NUMBER-OF-ARGS 1) 
(PUT 'TANGENTCONE* 'DEFINED-ON-LINE '207) 
(PUT 'TANGENTCONE* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'TANGENTCONE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TANGENTCONE* (M)
    (COND
     ((NULL (RING_DEGREES CALI=BASERING))
      (REDERR "tangent cone only for degree orders defined"))
     (T
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
                                          (DP_TCPART (BAS_DPOLY X))))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (BAS_MAKE (BAS_NR X)
                                  (DP_TCPART (BAS_DPOLY X))))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN
            (DPMAT_MAKE (DPMAT_ROWS M) (DPMAT_COLS M) B CALI=DEGREES
             (DPMAT_GBTAG M)))))
       (DPMAT_COLDEGS M))))) 
(PUT 'SYZYGIES1* 'NUMBER-OF-ARGS 1) 
(PUT 'SYZYGIES1* 'DEFINED-ON-LINE '220) 
(PUT 'SYZYGIES1* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'SYZYGIES1* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYZYGIES1* (BAS)
    (PROG ()
      (COND
       ((GREATERP (CALI_TRACE) 0)
        (PROGN (TERPRI) (PROGN (PRIN2 " Compute syzygies") NIL) (TERPRI))))
      (RETURN (THIRD (GROEB_STBASIS BAS NIL NIL T))))) 
(PUT 'SYZYGIES* 'NUMBER-OF-ARGS 1) 
(PUT 'SYZYGIES* 'DEFINED-ON-LINE '229) 
(PUT 'SYZYGIES* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'SYZYGIES* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYZYGIES* (BAS) (INTERREDUCE* (SYZYGIES1* BAS))) 
(PUT 'NORMALFORM* 'NUMBER-OF-ARGS 2) 
(PUT 'NORMALFORM* 'DEFINED-ON-LINE '233) 
(PUT 'NORMALFORM* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'NORMALFORM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NORMALFORM* (A B)
    (COND
     ((NOT
       (AND (EQN (DPMAT_COLS A) (DPMAT_COLS B))
            (EQUAL (DPMAT_COLDEGS A) (DPMAT_COLDEGS B))))
      (REDERR "dpmats don't match for NORMALFORM"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (PROG (A1 Z U R)
           (BAS_SETRELATIONS (DPMAT_LIST B))
           (SETQ A1
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (DPMAT_LIST A))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (PROGN
                                          (SETQ U
                                                  (RED_REDPOL (DPMAT_LIST B)
                                                   X))
                                          (SETQ Z
                                                  (CONS
                                                   (BAS_MAKE (BAS_NR X)
                                                    (DP_TIMES_EI (BAS_NR X)
                                                     (CDR U)))
                                                   Z))
                                          (CAR U)))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (PROGN
                                  (SETQ U (RED_REDPOL (DPMAT_LIST B) X))
                                  (SETQ Z
                                          (CONS
                                           (BAS_MAKE (BAS_NR X)
                                            (DP_TIMES_EI (BAS_NR X) (CDR U)))
                                           Z))
                                  (CAR U)))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ R (BAS_GETRELATIONS A1))
           (BAS_REMOVERELATIONS A1)
           (BAS_REMOVERELATIONS (DPMAT_LIST B))
           (SETQ Z (REVERSIP Z))
           (SETQ A1
                   (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_COLS A) A1 CALI=DEGREES
                    NIL))
           (SETQ CALI=DEGREES (DPMAT_ROWDEGREES B))
           (SETQ R
                   (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_ROWS B) (BAS_NEWORDER R)
                    CALI=DEGREES NIL))
           (SETQ CALI=DEGREES NIL)
           (SETQ Z
                   (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_ROWS A) (BAS_NEWORDER Z)
                    NIL NIL))
           (RETURN (LIST A1 R Z))))
       (DPMAT_COLDEGS B))))) 
(PUT 'MATOP_PSEUDOMOD 'NUMBER-OF-ARGS 2) 
(PUT 'MATOP_PSEUDOMOD 'DEFINED-ON-LINE '258) 
(PUT 'MATOP_PSEUDOMOD 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP_PSEUDOMOD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATOP_PSEUDOMOD (A B) (CAR (MOD* A B))) 
(PUT 'MOD* 'NUMBER-OF-ARGS 2) 
(PUT 'MOD* 'DEFINED-ON-LINE '260) 
(PUT 'MOD* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MOD* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD* (A B)
    ((LAMBDA (CALI=DEGREES)
       (PROG (U)
         (SETQ A (DP_NEWORDER A))
         (SETQ U (RED_REDPOL (DPMAT_LIST B) (BAS_MAKE 0 A)))
         (RETURN (CONS (BAS_DPOLY (CAR U)) (CDR U)))))
     (DPMAT_COLDEGS B))) 
(FLAG '(MOD) 'OPFN) 
(PUT 'MOD 'NUMBER-OF-ARGS 2) 
(PUT 'MOD 'DEFINED-ON-LINE '274) 
(PUT 'MOD 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MOD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD (A B)
    (COND ((EQUAL *MODE 'SYMBOLIC) (REDERR "only for algebraic mode"))
          (T
           (PROG (U)
             (SETQ B (DPMAT_FROM_A (REVAL1 B T)))
             (SETQ A (REVAL1 A T))
             (COND
              ((EQCAR A 'LIST)
               (COND
                ((GREATERP (DPMAT_COLS B) 0)
                 (REDERR "entries don't match for MOD"))
                (T
                 (SETQ A
                         (CONS 'LIST
                               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ X (CDR A))
                                 (COND ((NULL X) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (X)
                                                     (PROGN
                                                      (SETQ U
                                                              (MOD*
                                                               (DP_FROM_A X)
                                                               B))
                                                      (LIST 'QUOTIENT
                                                            (DP_2A (CAR U))
                                                            (DP_2A (CDR U)))))
                                                   (CAR X))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ X (CDR X))
                                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X)
                                             (PROGN
                                              (SETQ U (MOD* (DP_FROM_A X) B))
                                              (LIST 'QUOTIENT (DP_2A (CAR U))
                                                    (DP_2A (CDR U)))))
                                           (CAR X))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))))))
              ((EQCAR A 'MAT)
               (PROG ()
                 (SETQ A (DPMAT_FROM_A A))
                 (COND
                  ((NEQ (DPMAT_COLS A) (DPMAT_COLS B))
                   (REDERR "entries don't match for MOD")))
                 (SETQ A
                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                           (SETQ X (DPMAT_LIST A))
                           (COND ((NULL X) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (X)
                                               (MOD* (BAS_DPOLY X) B))
                                             (CAR X))
                                            NIL)))
                          LOOPLABEL
                           (SETQ X (CDR X))
                           (COND ((NULL X) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (MOD* (BAS_DPOLY X) B))
                                     (CAR X))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ A
                         (CONS 'MAT
                               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ X A)
                                 (COND ((NULL X) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (X)
                                                     (PROGN
                                                      (SETQ U (DP_2A (CDR X)))
                                                      (PROG (I FORALL-RESULT
                                                             FORALL-ENDPTR)
                                                        (SETQ I 1)
                                                        (COND
                                                         ((MINUSP
                                                           (DIFFERENCE
                                                            (DPMAT_COLS B) I))
                                                          (RETURN NIL)))
                                                        (SETQ FORALL-RESULT
                                                                (SETQ FORALL-ENDPTR
                                                                        (CONS
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          (DP_2A
                                                                           (DP_COMP
                                                                            I
                                                                            (CAR
                                                                             X)))
                                                                          U)
                                                                         NIL)))
                                                       LOOPLABEL
                                                        (SETQ I (PLUS2 I 1))
                                                        (COND
                                                         ((MINUSP
                                                           (DIFFERENCE
                                                            (DPMAT_COLS B) I))
                                                          (RETURN
                                                           FORALL-RESULT)))
                                                        (RPLACD FORALL-ENDPTR
                                                                (CONS
                                                                 (LIST
                                                                  'QUOTIENT
                                                                  (DP_2A
                                                                   (DP_COMP I
                                                                    (CAR X)))
                                                                  U)
                                                                 NIL))
                                                        (SETQ FORALL-ENDPTR
                                                                (CDR
                                                                 FORALL-ENDPTR))
                                                        (GO LOOPLABEL))))
                                                   (CAR X))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ X (CDR X))
                                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X)
                                             (PROGN
                                              (SETQ U (DP_2A (CDR X)))
                                              (PROG (I FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ I 1)
                                                (COND
                                                 ((MINUSP
                                                   (DIFFERENCE (DPMAT_COLS B)
                                                               I))
                                                  (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 (LIST
                                                                  'QUOTIENT
                                                                  (DP_2A
                                                                   (DP_COMP I
                                                                    (CAR X)))
                                                                  U)
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ I (PLUS2 I 1))
                                                (COND
                                                 ((MINUSP
                                                   (DIFFERENCE (DPMAT_COLS B)
                                                               I))
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         (LIST 'QUOTIENT
                                                               (DP_2A
                                                                (DP_COMP I
                                                                 (CAR X)))
                                                               U)
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                           (CAR X))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))))
              ((GREATERP (DPMAT_COLS B) 0)
               (REDERR "entries don't match for MOD"))
              (T
               (PROGN
                (SETQ U (MOD* (DP_FROM_A A) B))
                (SETQ A (LIST 'QUOTIENT (DP_2A (CAR U)) (DP_2A (CDR U)))))))
             (RETURN A))))) 
(INFIX (LIST 'MOD)) 
(FLAG '(NORMALFORM) 'OPFN) 
(PUT 'NORMALFORM 'NUMBER-OF-ARGS 2) 
(PUT 'NORMALFORM 'DEFINED-ON-LINE '307) 
(PUT 'NORMALFORM 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'NORMALFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NORMALFORM (A B)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (PROG (M)
        (SETQ M
                (NORMALFORM* (DPMAT_FROM_A (REVAL1 A T))
                 (DPMAT_FROM_A (REVAL1 B T))))
        (RETURN
         (LIST 'LIST (DPMAT_2A (CAR M)) (DPMAT_2A (CADR M))
               (DPMAT_2A (CADDR M))))))
     (T (NORMALFORM* A B)))) 
(PUT 'ELIMINATE* 'NUMBER-OF-ARGS 2) 
(PUT 'ELIMINATE* 'DEFINED-ON-LINE '317) 
(PUT 'ELIMINATE* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'ELIMINATE* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIMINATE* (M VARS)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (C E BAS V)
         (SETQ C CALI=BASERING)
         (SETQ E (RING_ECART C))
         (SETQ V (RING_NAMES CALI=BASERING))
         (SETRING* (RING_DEFINE V (ELIMINATIONORDER* V VARS) 'REVLEX E))
         (SETQ CALI=DEGREES NIL)
         (SETQ BAS
                 ((LAMBDA (*NOETHERIAN)
                    (BAS_SIEVE
                     (DPMAT_LIST
                      (CAR (GROEB_STBASIS (DPMAT_NEWORDER M NIL) T NIL NIL)))
                     VARS))
                  T))
         (SETRING* C)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (RETURN
          (DPMAT_MAKE (LENGTH BAS) (DPMAT_COLS M) (BAS_NEWORDER BAS)
           CALI=DEGREES NIL))))
     CALI=DEGREES CALI=BASERING)) 
(FLAG '(ELIMINATE) 'OPFN) 
(PUT 'ELIMINATE 'NUMBER-OF-ARGS 2) 
(PUT 'ELIMINATE 'DEFINED-ON-LINE '341) 
(PUT 'ELIMINATE 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'ELIMINATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIMINATE (M L)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (PROG ()
        (SETQ L (REVAL1 L T))
        (COND ((NOT (EQCAR L 'LIST)) (TYPERR L "variable list")))
        (SETQ M (DPMAT_FROM_A M))
        (SETQ L (CDR L))
        (RETURN (DPMAT_2A (ELIMINATE* M L)))))
     (T (ELIMINATE* M L)))) 
(PUT 'MATINTERSECT* 'NUMBER-OF-ARGS 1) 
(PUT 'MATINTERSECT* 'DEFINED-ON-LINE '352) 
(PUT 'MATINTERSECT* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATINTERSECT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATINTERSECT* (L)
    (COND ((NULL L) (REDERR "MATINTERSECT with empty list"))
          ((EQUAL (LENGTH L) 1) (CAR L))
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (C U V P SIZE)
                (MATOP=TESTDPMATLIST L)
                (SETQ SIZE (DPMAT_COLS (CAR L)))
                (SETQ V
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X L)
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X) (MAKE_CALI_VARNAME))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (MAKE_CALI_VARNAME)) (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ C CALI=BASERING)
                (SETRING*
                 (RING_SUM C
                  (RING_DEFINE V (DEGREEORDER* V) 'LEX
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X V)
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
                (SETQ CALI=DEGREES (MO_DEGNEWORDER (DPMAT_COLDEGS (CAR L))))
                (SETQ U
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X (PAIR V L))
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X)
                                              (DPMAT_TIMES_DPOLY
                                               (DP_FROM_A (CAR X))
                                               (DPMAT_NEWORDER (CDR X) NIL)))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (DPMAT_TIMES_DPOLY (DP_FROM_A (CAR X))
                                       (DPMAT_NEWORDER (CDR X) NIL)))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ P (DP_FI 1))
                (PROG (X)
                  (SETQ X V)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X) (SETQ P (DP_DIFF P (DP_FROM_A X)))) (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (COND ((EQUAL SIZE 0) (SETQ P (DPMAT_FROM_DPOLY P)))
                      (T
                       (SETQ P
                               (DPMAT_TIMES_DPOLY P
                                (DPMAT_UNIT SIZE CALI=DEGREES)))))
                (SETQ P (GBASIS* (MATSUM* (CONS P U))))
                (SETQ P (DPMAT_SIEVE P V T))
                (SETRING* C)
                (SETQ CALI=DEGREES (DPMAT_COLDEGS (CAR L)))
                (RETURN (DPMAT_NEWORDER P T))))
            CALI=DEGREES CALI=BASERING)))) 
(PUT 'MATINTERSECT 'PSOPFN 'MATOP=MATINTERSECT) 
(PUT 'IDEALINTERSECT 'PSOPFN 'MATOP=MATINTERSECT) 
(PUT 'MATOP=MATINTERSECT 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=MATINTERSECT 'DEFINED-ON-LINE '379) 
(PUT 'MATOP=MATINTERSECT 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=MATINTERSECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=MATINTERSECT (L)
    (DPMAT_2A
     (MATINTERSECT*
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X L)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (DPMAT_FROM_A (REVAL1 X T))) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'MODEQUALP 'PSOPFN 'MATOP=EQUALP) 
(PUT 'MATOP=EQUALP 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=EQUALP 'DEFINED-ON-LINE '389) 
(PUT 'MATOP=EQUALP 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=EQUALP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=EQUALP (U)
    (COND ((NEQ (LENGTH U) 2) (REDERR "Syntax : MODEQUALP(dpmat,dpmat) "))
          (T
           (PROG (A B)
             (INTF_GET (FIRST U))
             (INTF_GET (SECOND U))
             (COND
              ((NULL (SETQ A (GET (FIRST U) 'GBASIS)))
               (PUT (FIRST U) 'GBASIS
                    (SETQ A (GBASIS* (GET (FIRST U) 'BASIS))))))
             (COND
              ((NULL (SETQ B (GET (SECOND U) 'GBASIS)))
               (PUT (SECOND U) 'GBASIS
                    (SETQ B (GBASIS* (GET (SECOND U) 'BASIS))))))
             (COND ((MODEQUALP* A B) (RETURN 'YES)) (T (RETURN 'NO))))))) 
(PUT 'MODEQUALP* 'NUMBER-OF-ARGS 2) 
(PUT 'MODEQUALP* 'DEFINED-ON-LINE '400) 
(PUT 'MODEQUALP* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MODEQUALP* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODEQUALP* (A B) (AND (SUBMODULEP* A B) (SUBMODULEP* B A))) 
(PUT 'SUBMODULEP 'PSOPFN 'MATOP=SUBMODULEP) 
(PUT 'MATOP=SUBMODULEP 'NUMBER-OF-ARGS 1) 
(PUT 'MATOP=SUBMODULEP 'DEFINED-ON-LINE '405) 
(PUT 'MATOP=SUBMODULEP 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'MATOP=SUBMODULEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATOP=SUBMODULEP (U)
    (COND ((NEQ (LENGTH U) 2) (REDERR "Syntax : SUBMODULEP(dpmat,dpmat)"))
          (T
           (PROG (A B)
             (INTF_GET (SECOND U))
             (COND
              ((NULL (SETQ B (GET (SECOND U) 'GBASIS)))
               (PUT (SECOND U) 'GBASIS
                    (SETQ B (GBASIS* (GET (SECOND U) 'BASIS))))))
             (SETQ A (DPMAT_FROM_A (REVAL1 (FIRST U) T)))
             (COND ((SUBMODULEP* A B) (RETURN 'YES)) (T (RETURN 'NO))))))) 
(PUT 'SUBMODULEP* 'NUMBER-OF-ARGS 2) 
(PUT 'SUBMODULEP* 'DEFINED-ON-LINE '415) 
(PUT 'SUBMODULEP* 'DEFINED-IN-FILE 'CALI/MATOP.RED) 
(PUT 'SUBMODULEP* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBMODULEP* (A B)
    (COND
     ((NOT
       (AND (EQUAL (DPMAT_COLS A) (DPMAT_COLS B))
            (EQUAL (DPMAT_COLDEGS A) (DPMAT_COLDEGS B))))
      (REDERR "incompatible modules in SUBMODULEP"))
     (T
      ((LAMBDA (CALI=DEGREES)
         (PROG ()
           (SETQ A
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (DPMAT_LIST A))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (X) (BAS_DPOLY X)) (CAR X))
                                           NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (X) (BAS_DPOLY X)) (CAR X)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN (NOT (LISTTEST A B (FUNCTION MATOP_PSEUDOMOD))))))
       (DPMAT_COLDEGS A))))) 
(ENDMODULE) 