(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INVOL)) 
(FLUID '(*EDSVERBOSE *EDSDEBUG *EDSSLOPPY *GENPOS *RANPOS)) 
(PUT 'CHARACTERS 'PSOPFN 'CHAREVAL) 
(PUT 'CHAREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'CHAREVAL 'DEFINED-ON-LINE '37) 
(PUT 'CHAREVAL 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CHAREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHAREVAL (U)
    (COND
     ((OR (LESSP (LENGTH U) 1) (GREATERP (LENGTH U) 2))
      (RERROR 'EDS 0 "Wrong number of arguments to characters"))
     ((EDSP (CAR (SETQ U (REVLIS U))))
      (CONS 'LIST (CHARACTERS (CAR U) (COND ((CDR U) (*A2SYS (CADR U)))))))
     (T (CONS 'LIST (CHARACTERSTAB (*A2TAB U)))))) 
(PUT 'CHARACTERSTAB 'NUMBER-OF-ARGS 1) 
(PUT 'CHARACTERSTAB 'DEFINED-ON-LINE '46) 
(PUT 'CHARACTERSTAB 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CHARACTERSTAB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHARACTERSTAB (U)
    (COUNTCHARS
     (TP1
      (PROG (R FORALL-RESULT FORALL-ENDPTR)
        (SETQ R (CDR U))
        (COND ((NULL R) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (PROG (C FORALL-RESULT FORALL-ENDPTR)
                              (SETQ C R)
                              (COND ((NULL C) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS ((LAMBDA (C) C) (CAR C))
                                                    NIL)))
                             LOOPLABEL
                              (SETQ C (CDR C))
                              (COND ((NULL C) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (C) C) (CAR C)) NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR R))
                         NIL)))
       LOOPLABEL
        (SETQ R (CDR R))
        (COND ((NULL R) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (R)
                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                      (SETQ C R)
                      (COND ((NULL C) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (C) C) (CAR C)) NIL)))
                     LOOPLABEL
                      (SETQ C (CDR C))
                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (C) C) (CAR C)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR R))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'CHARACTERS 'NUMBER-OF-ARGS 2) 
(PUT 'CHARACTERS 'DEFINED-ON-LINE '52) 
(PUT 'CHARACTERS 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CHARACTERS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHARACTERS (S X) (EDSPROTECT (LIST 'CHARACTERS1 S X))) 
(PUT 'CHARACTERS1 'NUMBER-OF-ARGS 2) 
(PUT 'CHARACTERS1 'DEFINED-ON-LINE '58) 
(PUT 'CHARACTERS1 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CHARACTERS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHARACTERS1 (S X)
    (PROG (PRL IND Q SP)
      (COND ((NOT (NORMALEDSP S)) (RERROR 'EDS 0 "System not in normal form")))
      (COND
       ((SCALARPART (CADR S))
        (RERROR 'EDS 0 "Characters with 0-forms not yet implemented")))
      (SETQ S (CLOSURE S))
      (COND
       ((QUASILINEARP S)
        (SETQ S (CAR (TMPIND (CHANGEPOSITION (LINEARGENERATORS S))))))
       ((NULL X)
        (RERROR 'EDS 0
                "Integral element required for nonlinear EDS characters"))
       (T (SETQ S (CAR (TMPIND (CHANGEPOSITION (LINEARISE S X)))))))
      (COND
       ((OR (NOT (NORMALEDSP S)) (SCALARPART (CADR S)))
        (ERRDHH "Result from tmpind has 0-forms or is not in normal form")))
      (SETQ PRL (PRLKRNS S))
      (SETQ IND (INDKRNS S))
      (SETQ Q
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (NONPFAFFPART (CADR S)))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND ((SETQ F (LINEARPART F PRL)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND ((SETQ F (LINEARPART F PRL)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (SETQ X
              (REVERSIP
               (PROG (W FORALL-RESULT FORALL-ENDPTR)
                 (SETQ W (REVERSE IND))
                 (COND ((NULL W) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ F Q)
                                   STARTOVER
                                    (COND ((NULL F) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (F)
                                               (PROG (C FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ C
                                                         (ORDCOMB (CDR W)
                                                          (DIFFERENCE
                                                           (DEGREEPF F) 2)))
                                                STARTOVER
                                                 (COND ((NULL C) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST C))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-RESULT))
                                                 (SETQ C (CDR C))
                                                 (COND
                                                  ((ATOM FORALL-ENDPTR)
                                                   (GO STARTOVER)))
                                                LOOPLABEL
                                                 (COND
                                                  ((NULL C)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST C))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-ENDPTR))
                                                 (SETQ C (CDR C))
                                                 (GO LOOPLABEL)))
                                             (CAR F)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ F (CDR F))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            ((LAMBDA (F)
                                               (PROG (C FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ C
                                                         (ORDCOMB (CDR W)
                                                          (DIFFERENCE
                                                           (DEGREEPF F) 2)))
                                                STARTOVER
                                                 (COND ((NULL C) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST C))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-RESULT))
                                                 (SETQ C (CDR C))
                                                 (COND
                                                  ((ATOM FORALL-ENDPTR)
                                                   (GO STARTOVER)))
                                                LOOPLABEL
                                                 (COND
                                                  ((NULL C)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         ((LAMBDA (C)
                                                            (COND
                                                             ((SETQ C
                                                                      (XCOEFF F
                                                                       (CONS
                                                                        (CAR W)
                                                                        C)))
                                                              (LIST C))))
                                                          (CAR C)))
                                                 (SETQ FORALL-ENDPTR
                                                         (LASTPAIR
                                                          FORALL-ENDPTR))
                                                 (SETQ C (CDR C))
                                                 (GO LOOPLABEL)))
                                             (CAR F)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ F (CDR F))
                                    (GO LOOPLABEL))
                                  NIL)))
                LOOPLABEL
                 (SETQ W (CDR W))
                 (COND ((NULL W) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          (PROG (F FORALL-RESULT FORALL-ENDPTR)
                            (SETQ F Q)
                           STARTOVER
                            (COND ((NULL F) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (F)
                                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ C
                                                 (ORDCOMB (CDR W)
                                                  (DIFFERENCE (DEGREEPF F) 2)))
                                        STARTOVER
                                         (COND ((NULL C) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST C))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-RESULT))
                                         (SETQ C (CDR C))
                                         (COND
                                          ((ATOM FORALL-ENDPTR)
                                           (GO STARTOVER)))
                                        LOOPLABEL
                                         (COND
                                          ((NULL C) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST C))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-ENDPTR))
                                         (SETQ C (CDR C))
                                         (GO LOOPLABEL)))
                                     (CAR F)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ F (CDR F))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL F) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (F)
                                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ C
                                                 (ORDCOMB (CDR W)
                                                  (DIFFERENCE (DEGREEPF F) 2)))
                                        STARTOVER
                                         (COND ((NULL C) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST C))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-RESULT))
                                         (SETQ C (CDR C))
                                         (COND
                                          ((ATOM FORALL-ENDPTR)
                                           (GO STARTOVER)))
                                        LOOPLABEL
                                         (COND
                                          ((NULL C) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 ((LAMBDA (C)
                                                    (COND
                                                     ((SETQ C
                                                              (XCOEFF F
                                                               (CONS (CAR W)
                                                                     C)))
                                                      (LIST C))))
                                                  (CAR C)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-ENDPTR))
                                         (SETQ C (CDR C))
                                         (GO LOOPLABEL)))
                                     (CAR F)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ F (CDR F))
                            (GO LOOPLABEL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ X (REVERSE (COUNTCHARS X)))
      (SETQ SP
              (DIFFERENCE (LENGTH (EDSCOB S))
                          (PLUS (LENGTH IND) (LENGTH (PFAFFPART (CADR S)))
                                (PROG (SI FORALL-RESULT)
                                  (SETQ SI (CDR X))
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND ((NULL SI) (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (PLUS ((LAMBDA (SI) SI) (CAR SI))
                                                FORALL-RESULT))
                                  (SETQ SI (CDR SI))
                                  (GO LAB1)))))
      (COND
       ((NEQ SP (CAR X))
        (EDSVERBOSE "Cauchy characteristics detected from characters" NIL
         NIL)))
      (RETURN (REVERSE (CONS SP (CDR X)))))) 
(PUT 'CHANGEPOSITION 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGEPOSITION 'DEFINED-ON-LINE '95) 
(PUT 'CHANGEPOSITION 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CHANGEPOSITION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGEPOSITION (S)
    (COND
     (*GENPOS
      (PROG (X NEW)
        (SETQ NEW
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I 1)
                  (COND
                   ((MINUSP (DIFFERENCE (LENGTH (CADDR S)) I)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS (MKFORM* (INTERN (GENSYM)) 1) NIL)))
                 LOOPLABEL
                  (SETQ I (PLUS2 I 1))
                  (COND
                   ((MINUSP (DIFFERENCE (LENGTH (CADDR S)) I))
                    (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS (MKFORM* (INTERN (GENSYM)) 1) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ X
                (REVERSIP
                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                   (SETQ K NEW)
                   (COND ((NULL K) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (CONS (CONS K (CONS 1 1)) NIL))
                                     (CAR K))
                                    NIL)))
                  LOOPLABEL
                   (SETQ K (CDR K))
                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (K) (CONS (CONS K (CONS 1 1)) NIL))
                             (CAR K))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (SETQ X
                (PROG (L FORALL-RESULT FORALL-ENDPTR)
                  (SETQ L X)
                  (COND ((NULL L) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   (ZIPPF L
                                    (CONS (CONS 1 1)
                                          (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ I 2)
                                            (COND
                                             ((MINUSP
                                               (DIFFERENCE (LENGTH L) I))
                                              (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             (CONS
                                                              (LIST
                                                               (CONS
                                                                (CONS
                                                                 (INTERN
                                                                  (GENSYM))
                                                                 1)
                                                                1))
                                                              1)
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ I (PLUS2 I 1))
                                            (COND
                                             ((MINUSP
                                               (DIFFERENCE (LENGTH L) I))
                                              (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     (CONS
                                                      (LIST
                                                       (CONS
                                                        (CONS (INTERN (GENSYM))
                                                              1)
                                                        1))
                                                      1)
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                   NIL)))
                 LOOPLABEL
                  (SETQ L (CDR L))
                  (COND ((NULL L) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           (ZIPPF L
                            (CONS (CONS 1 1)
                                  (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ I 2)
                                    (COND
                                     ((MINUSP (DIFFERENCE (LENGTH L) I))
                                      (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     (CONS
                                                      (LIST
                                                       (CONS
                                                        (CONS (INTERN (GENSYM))
                                                              1)
                                                        1))
                                                      1)
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ I (PLUS2 I 1))
                                    (COND
                                     ((MINUSP (DIFFERENCE (LENGTH L) I))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             (CONS
                                              (LIST
                                               (CONS (CONS (INTERN (GENSYM)) 1)
                                                     1))
                                              1)
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ X (PAIR (INDKRNS S) (REVERSE X)))
        (EDSDEBUG "Transformation to general position" X 'XFORM)
        (RETURN (XFORMEDS0 S X NEW))))
     (*RANPOS
      (PROG (X Y K NEW)
        (SETQ NEW
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I 1)
                  (COND
                   ((MINUSP (DIFFERENCE (LENGTH (CADDR S)) I)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS (MKFORM* (INTERN (GENSYM)) 1) NIL)))
                 LOOPLABEL
                  (SETQ I (PLUS2 I 1))
                  (COND
                   ((MINUSP (DIFFERENCE (LENGTH (CADDR S)) I))
                    (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS (MKFORM* (INTERN (GENSYM)) 1) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ X
                (REVERSIP
                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                   (SETQ K NEW)
                   (COND ((NULL K) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (CONS (CONS K (CONS 1 1)) NIL))
                                     (CAR K))
                                    NIL)))
                  LOOPLABEL
                   (SETQ K (CDR K))
                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (K) (CONS (CONS K (CONS 1 1)) NIL))
                             (CAR K))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (SETQ K (UPDKORDL (LPOWS X)))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (LENGTH (CADDR S)) I)) (RETURN NIL)))
          (PROG (F)
            (PROG ()
             WHILELABEL
              (COND ((NOT (NULL (SETQ F (XREDUCE F Y)))) (RETURN NIL)))
              (SETQ F (ZIPPF X (RANLISTSQ (LENGTH (CADDR S)) 10 5)))
              (GO WHILELABEL))
            (SETQ Y (CONS F Y)))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETKORDER K)
        (SETQ X
                (PAIR (INDKRNS S)
                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                        (SETQ F Y)
                        (COND ((NULL F) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (F) (XREORDER F)) (CAR F))
                                         NIL)))
                       LOOPLABEL
                        (SETQ F (CDR F))
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (XREORDER F)) (CAR F)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (EDSDEBUG "Transformation to random position" X 'XFORM)
        (RETURN (XFORMEDS0 S X NEW))))
     (T S))) 
(PUT 'RANLISTSQ 'NUMBER-OF-ARGS 3) 
(PUT 'RANLISTSQ 'DEFINED-ON-LINE '134) 
(PUT 'RANLISTSQ 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'RANLISTSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RANLISTSQ (N P M)
    (PROG (U V)
      (SETQ P (MIN2 N P))
      (SETQ U
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE P I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (SIMPATOM
                                  (DIFFERENCE (RANDOM (PLUS (TIMES 2 M) 1)) M))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE P I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (SIMPATOM
                          (DIFFERENCE (RANDOM (PLUS (TIMES 2 M) 1)) M))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP (LENGTH V) P)) (RETURN NIL)))
        ((LAMBDA (X) (COND ((NOT (MEMQ X V)) (SETQ V (CONS X V)))))
         (PLUS 1 (RANDOM N)))
        (GO WHILELABEL))
      (SETQ U (PAIR V U))
      (RETURN
       (PROG (I FORALL-RESULT FORALL-ENDPTR)
         (SETQ I 1)
         (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (COND ((SETQ V (ATSOC I U)) (CDR V))
                                (T (CONS NIL 1)))
                          NIL)))
        LOOPLABEL
         (SETQ I (PLUS2 I 1))
         (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS (COND ((SETQ V (ATSOC I U)) (CDR V)) (T (CONS NIL 1)))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'COUNTCHARS 'NUMBER-OF-ARGS 1) 
(PUT 'COUNTCHARS 'DEFINED-ON-LINE '149) 
(PUT 'COUNTCHARS 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'COUNTCHARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COUNTCHARS (X)
    (PROG (P RI SI)
      (PROG (R)
        (SETQ R X)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (PROG ()
             (SETQ P (WEAK_XAUTOREDUCE (APPEND R P)))
             (SETQ RI (CONS (LENGTH P) RI))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR RI)) (RETURN NIL)))
        (PROGN
         (SETQ SI (CONS (DIFFERENCE (CAR RI) (CADR RI)) SI))
         (SETQ RI (CDR RI)))
        (GO WHILELABEL))
      (RETURN (CONS (CAR RI) SI)))) 
(PUT 'INVOLUTIONCHK 'NUMBER-OF-ARGS 2) 
(PUT 'INVOLUTIONCHK 'DEFINED-ON-LINE '165) 
(PUT 'INVOLUTIONCHK 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'INVOLUTIONCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVOLUTIONCHK (S0 S1)
    (CARTANTEST (CHARACTERS S0 (CADR S1))
     (DIFFERENCE (LENGTH (EDSCOB S1)) (LENGTH (EDSCOB S0))))) 
(PUT 'CARTANTEST 'NUMBER-OF-ARGS 2) 
(PUT 'CARTANTEST 'DEFINED-ON-LINE '172) 
(PUT 'CARTANTEST 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'CARTANTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CARTANTEST (C D)
    (PROG (M)
      (SETQ M 0)
      (SETQ C (REVERSE C))
      (PROG (K)
        (SETQ K C)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        (SETQ M (PLUS (TIMES (LENGTH K) (CAR K)) M))
        (SETQ K (CDR K))
        (GO LAB))
      (COND
       ((GREATERP D M)
        (ERRDHH (LIST "Inconsistency in Cartan's test:" (REVERSE C) D)))
       (T (RETURN (EQUAL D M)))))) 
(PUT 'INVOLUTIVE 'PSOPFN 'INVOLUTIVEEVAL) 
(PUT 'INVOLUTIVEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'INVOLUTIVEEVAL 'DEFINED-ON-LINE '186) 
(PUT 'INVOLUTIVEEVAL 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'INVOLUTIVEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVOLUTIVEEVAL (S)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (COND
       ((OR (KNOWNTRUEEDS S 'INVOLUTIVE)
            (AND (NOT (KNOWNFALSEEDS S 'INVOLUTIVE))
                 (EDSPROTECT (LIST 'INVOLUTIVE S))))
        1)
       (T 0)))
     (T (TYPERR S 'EDS)))) 
(PUT 'INVOLUTIVE 'NUMBER-OF-ARGS 1) 
(PUT 'INVOLUTIVE 'DEFINED-ON-LINE '195) 
(PUT 'INVOLUTIVE 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'INVOLUTIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVOLUTIVE (S)
    (OR (KNOWNTRUEEDS S 'INVOLUTIVE)
        (AND (NOT (KNOWNFALSEEDS S 'INVOLUTIVE))
             (PROG (S0 S1 FLG)
               (SETQ S0 (CLOSURE S))
               (COND
                ((SEMILINEARP S0)
                 (SETQ FLG
                         (AND
                          (CARTANTEST (CHARACTERS S0 NIL)
                           (DIMGRASSMANNVARIETY S0 NIL))
                          (NULL (GRASSMANNVARIETYTORSION S0)))))
                (T
                 (PROGN
                  (SETQ S1 (PROLONG S0))
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT
                       (AND S1 (EQUAL (CAAR S1) 'PROLONGED)
                            (INVOLUTIONCHK S0 (CDAR S1))))
                      (RETURN NIL)))
                    (SETQ S1 (CDR S1))
                    (GO WHILELABEL))
                  (SETQ FLG (NULL S1)))))
               (COND (FLG (PROGN (FLAGTRUEEDS S 'INVOLUTIVE) (RETURN T)))))))) 
(PUT 'INVOLUTION 'RTYPEFN 'QUOTEEDS) 
(PUT 'INVOLUTION 'EDSFN 'INVOLUTIONEDS) 
(PUT 'INVOLUTIONEDS 'NUMBER-OF-ARGS 1) 
(PUT 'INVOLUTIONEDS 'DEFINED-ON-LINE '222) 
(PUT 'INVOLUTIONEDS 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'INVOLUTIONEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVOLUTIONEDS (S)
    (COND ((NOT (EDSP S)) (TYPERR S 'EDS))
          (T
           (MKXEDS
            (CONS 'LIST
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X (EDSPROTECT (LIST 'INVOLUTION S)))
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X)
                                        (COND
                                         ((NULL (CAR X))
                                          (LIST 'INVOLUTION (CDR X)))
                                         (T (CDR X))))
                                      (CAR X))
                                     NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (COND
                                 ((NULL (CAR X)) (LIST 'INVOLUTION (CDR X)))
                                 (T (CDR X))))
                              (CAR X))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))) 
(PUT 'INVOLUTION 'NUMBER-OF-ARGS 1) 
(PUT 'INVOLUTION 'DEFINED-ON-LINE '231) 
(PUT 'INVOLUTION 'DEFINED-IN-FILE 'EDS/INVOL.RED) 
(PUT 'INVOLUTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVOLUTION (S)
    (PROG (R S0)
      (SETQ S0 (CLOSURE S))
      (COND
       ((AND (SEMILINEARP S0)
             (CARTANTEST (CHARACTERS S0 NIL) (DIMGRASSMANNVARIETY S0 NIL))
             (NULL (GRASSMANNVARIETYTORSION S0)))
        (RETURN (LIST (CONS T S)))))
      (PROG (S1)
        (SETQ S1 (EDSPROTECT (LIST 'PROLONG S0)))
       LAB
        (COND ((NULL S1) (RETURN NIL)))
        ((LAMBDA (S1)
           (COND ((EQUAL (CAR S1) 'INCONSISTENT) NIL)
                 ((EQUAL (CAR S1) 'FAILED)
                  (SETQ R (UNION (LIST (CONS NIL (CDR S1))) R)))
                 ((AND (EQUAL (CAR S1) 'PROLONGED) (INVOLUTIONCHK S0 (CDR S1)))
                  (SETQ R (UNION (LIST (CONS T S)) R)))
                 (T
                  (SETQ R
                          (UNION (EDSPROTECT (LIST 'INVOLUTION (CDR S1)))
                                 R)))))
         (CAR S1))
        (SETQ S1 (CDR S1))
        (GO LAB))
      (RETURN R))) 
(ENDMODULE) 