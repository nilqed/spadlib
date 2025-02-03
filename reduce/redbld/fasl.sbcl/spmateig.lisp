(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPMATEIG)) 
(PUT 'SPMATEIGEN 'NUMBER-OF-ARGS 2) 
(PUT 'SPMATEIGEN 'DEFINED-ON-LINE '44) 
(PUT 'SPMATEIGEN 'DEFINED-IN-FILE 'SPARSE/SPMATEIG.RED) 
(PUT 'SPMATEIGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPMATEIGEN (U EIVAL)
    (PROG (ARBVARS EXU SGN Q R S X Y Z EIVEC *FACTOR *SQFREE *EXP VAL RES RES1
           RL L CNT)
      (SETQ L 0)
      (SETQ CNT 0)
      (SETQ *EXP T)
      (COND ((NOT (EQ (GETRTYPE U) 'SPARSE)) (TYPERR U "sparse matrix")))
      (SETQ EIVAL (*A2K EIVAL))
      (SETQ KORD* (CONS EIVAL KORD*))
      (SETQ RL (SPROW_DIM U))
      (SETQ EXU (SPMATEIGEN1 (SPMATSM U) EIVAL))
      (SETQ Q (CAR EXU))
      (SETQ Y (CADR EXU))
      (SETQ Z (CADDR EXU))
      (SETQ EXU (CDDDR EXU))
      (SETQ *SQFREE T)
      (PROG (J)
        (SETQ J (CDR (FCTRF (CAR (SUBS2 (CONS (CDAR Z) 1))))))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((AND (NULL (OR (ATOM (CAR J)) (ATOM (CAR (CAR J)))))
                  (EQ (CAAAR (CAR J)) EIVAL))
             (SETQ S
                     (CONS
                      (COND
                       ((NULL (CDR (CAR J)))
                        (CONS (LIST (CONS (CONS (CAAAR (CAR J)) 1) 1))
                              (TIMES (CDAAR (CAR J)) (CDR J))))
                       (T J))
                      S)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (PROG (J)
        (SETQ J Q)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           ((LAMBDA (Y)
              ((LAMBDA (X)
                 (COND (X (RPLACD X (PLUS (CDR X) (CDR J))))
                       (T (SETQ S (CONS (CONS Y (CDR J)) S)))))
               (ASSOC Y S)))
            (ABSF (REORDER (CAR J)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ L (LENGTH S))
      (SETQ R
              (CONS 'LIST
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J S)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (PROGN
                                           (COND
                                            ((NULL
                                              (AND (EQUAL (CDR J) 1)
                                                   (EQUAL L 1)))
                                             (PROGN
                                              (SETQ Y 1)
                                              (PROG (K)
                                                (SETQ K EXU)
                                               LAB
                                                (COND ((NULL K) (RETURN NIL)))
                                                ((LAMBDA (K)
                                                   (COND
                                                    ((SETQ X
                                                             (REDUCE-MOD-EIG
                                                              (CAR J)
                                                              (|C:EXTMULT| K
                                                                           Y)))
                                                     (SETQ Y X))))
                                                 (CAR K))
                                                (SETQ K (CDR K))
                                                (GO LAB)))))
                                           (SETQ ARBVARS NIL)
                                           (PROG (K)
                                             (SETQ K (CAAR Z))
                                            LAB
                                             (COND ((NULL K) (RETURN NIL)))
                                             ((LAMBDA (K)
                                                (COND
                                                 ((OR (EQUAL Y 1)
                                                      (NULL
                                                       (MEMBER K (CAAR Y))))
                                                  (SETQ ARBVARS
                                                          (CONS
                                                           (CONS K
                                                                 (MAKEARBCOMPLEX))
                                                           ARBVARS)))))
                                              (CAR K))
                                             (SETQ K (CDR K))
                                             (GO LAB))
                                           (SETQ SGN
                                                   (OR (EQUAL Y 1)
                                                       (EVENP
                                                        (LENGTH (CAAR Y)))))
                                           (SETQ CNT 0)
                                           (PROG (K)
                                             (SETQ K (CAAR Z))
                                            LAB
                                             (COND ((NULL K) (RETURN NIL)))
                                             ((LAMBDA (K)
                                                (PROGN
                                                 (COND
                                                  ((SETQ X (ASSOC K ARBVARS))
                                                   (SETQ RES
                                                           (LIST
                                                            (SETQ CNT
                                                                    (PLUS CNT
                                                                          1))
                                                            (CONS 1
                                                                  (CAAAR
                                                                   (CDR X))))))
                                                  (T
                                                   (PROGN
                                                    (SETQ VAL
                                                            (MKGLEIG K Y
                                                                     (SETQ SGN
                                                                             (NOT
                                                                              SGN))
                                                                     ARBVARS))
                                                    (COND
                                                     ((EQUAL VAL (SIMP 0))
                                                      (SETQ CNT (PLUS CNT 1)))
                                                     (T
                                                      (SETQ RES
                                                              (LIST
                                                               (SETQ CNT
                                                                       (PLUS
                                                                        CNT 1))
                                                               (CONS 1
                                                                     (PREPSQ*
                                                                      VAL))))))
                                                    NIL)))
                                                 (COND ((EQUAL RES NIL) NIL)
                                                       (T
                                                        (PROGN
                                                         (SETQ RES1
                                                                 (APPEND RES1
                                                                         (LIST
                                                                          RES)))
                                                         (SETQ RES NIL)
                                                         (SETQ EIVEC
                                                                 (MKEMPSPMAT RL
                                                                  (LIST 'SPM RL
                                                                        1)))
                                                         (PROG (I)
                                                           (SETQ I 1)
                                                          LAB
                                                           (COND
                                                            ((MINUSP
                                                              (DIFFERENCE RL
                                                                          I))
                                                             (RETURN NIL)))
                                                           (PROGN
                                                            (LETMTR3
                                                             (LIST EIVEC I)
                                                             (ATSOC I RES1)
                                                             EIVEC NIL)
                                                            NIL)
                                                           (SETQ I (PLUS2 I 1))
                                                           (GO LAB))
                                                         NIL)))
                                                 NIL))
                                              (CAR K))
                                             (SETQ K (CDR K))
                                             (GO LAB))
                                           (SETQ RES1 NIL)
                                           (LIST 'LIST
                                                 (PREPSQ* (CONS (CAR J) 1))
                                                 (CDR J) EIVEC)))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (PROGN
                                   (COND
                                    ((NULL (AND (EQUAL (CDR J) 1) (EQUAL L 1)))
                                     (PROGN
                                      (SETQ Y 1)
                                      (PROG (K)
                                        (SETQ K EXU)
                                       LAB
                                        (COND ((NULL K) (RETURN NIL)))
                                        ((LAMBDA (K)
                                           (COND
                                            ((SETQ X
                                                     (REDUCE-MOD-EIG (CAR J)
                                                                     (|C:EXTMULT|
                                                                      K Y)))
                                             (SETQ Y X))))
                                         (CAR K))
                                        (SETQ K (CDR K))
                                        (GO LAB)))))
                                   (SETQ ARBVARS NIL)
                                   (PROG (K)
                                     (SETQ K (CAAR Z))
                                    LAB
                                     (COND ((NULL K) (RETURN NIL)))
                                     ((LAMBDA (K)
                                        (COND
                                         ((OR (EQUAL Y 1)
                                              (NULL (MEMBER K (CAAR Y))))
                                          (SETQ ARBVARS
                                                  (CONS
                                                   (CONS K (MAKEARBCOMPLEX))
                                                   ARBVARS)))))
                                      (CAR K))
                                     (SETQ K (CDR K))
                                     (GO LAB))
                                   (SETQ SGN
                                           (OR (EQUAL Y 1)
                                               (EVENP (LENGTH (CAAR Y)))))
                                   (SETQ CNT 0)
                                   (PROG (K)
                                     (SETQ K (CAAR Z))
                                    LAB
                                     (COND ((NULL K) (RETURN NIL)))
                                     ((LAMBDA (K)
                                        (PROGN
                                         (COND
                                          ((SETQ X (ASSOC K ARBVARS))
                                           (SETQ RES
                                                   (LIST
                                                    (SETQ CNT (PLUS CNT 1))
                                                    (CONS 1 (CAAAR (CDR X))))))
                                          (T
                                           (PROGN
                                            (SETQ VAL
                                                    (MKGLEIG K Y
                                                             (SETQ SGN
                                                                     (NOT SGN))
                                                             ARBVARS))
                                            (COND
                                             ((EQUAL VAL (SIMP 0))
                                              (SETQ CNT (PLUS CNT 1)))
                                             (T
                                              (SETQ RES
                                                      (LIST
                                                       (SETQ CNT (PLUS CNT 1))
                                                       (CONS 1
                                                             (PREPSQ* VAL))))))
                                            NIL)))
                                         (COND ((EQUAL RES NIL) NIL)
                                               (T
                                                (PROGN
                                                 (SETQ RES1
                                                         (APPEND RES1
                                                                 (LIST RES)))
                                                 (SETQ RES NIL)
                                                 (SETQ EIVEC
                                                         (MKEMPSPMAT RL
                                                          (LIST 'SPM RL 1)))
                                                 (PROG (I)
                                                   (SETQ I 1)
                                                  LAB
                                                   (COND
                                                    ((MINUSP (DIFFERENCE RL I))
                                                     (RETURN NIL)))
                                                   (PROGN
                                                    (LETMTR3 (LIST EIVEC I)
                                                     (ATSOC I RES1) EIVEC NIL)
                                                    NIL)
                                                   (SETQ I (PLUS2 I 1))
                                                   (GO LAB))
                                                 NIL)))
                                         NIL))
                                      (CAR K))
                                     (SETQ K (CDR K))
                                     (GO LAB))
                                   (SETQ RES1 NIL)
                                   (LIST 'LIST (PREPSQ* (CONS (CAR J) 1))
                                         (CDR J) EIVEC)))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ KORD* (CDR KORD*))
      (RETURN R))) 
(PUT 'SPMATEIGEN1 'NUMBER-OF-ARGS 2) 
(PUT 'SPMATEIGEN1 'DEFINED-ON-LINE '114) 
(PUT 'SPMATEIGEN1 'DEFINED-IN-FILE 'SPARSE/SPMATEIG.RED) 
(PUT 'SPMATEIGEN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPMATEIGEN1 (U EIVAL)
    (PROG (DIAG Q X Y Z W J RES L LM M CC)
      (SETQ L 0)
      (SETQ LM 0)
      (SETQ M 0)
      (SETQ CC 0)
      (SETQ LM (SPCOL_DIM U))
      (SETQ Z 1)
      (PROG (RR)
        (SETQ RR 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (SPROW_DIM U) RR)) (RETURN NIL)))
        (PROGN
         (SETQ Y 1)
         (SETQ DIAG NIL)
         (SETQ CC (FINDROW U RR))
         (COND
          ((NOT (EQUAL CC NIL))
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR CC))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN (SETQ W (SIMP (CDR XX))) (SETQ Y (LCM Y (CDR W))) NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         (SETQ M LM)
         (SETQ L (PLUS L 1))
         (SETQ X NIL)
         (COND ((EQUAL CC NIL) (SETQ CC (LIST (LIST NIL) (LIST NIL)))))
         (PROG (XX)
           (SETQ XX (REVERSE (CDR CC)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (COND
                ((EQUAL XX '(NIL))
                 (PROGN (SETQ M (PLUS RR 1)) (SETQ J (SIMP NIL))))
                (T (PROGN (SETQ J (SIMP (CDR XX))) (SETQ M (CAR XX)))))
               (COND
                ((NOT DIAG)
                 (PROGN
                  (COND
                   ((EQUAL M RR) (PROGN (SETQ DIAG J) (SETQ J (SIMP NIL))))
                   ((LESSP M RR) NIL)
                   (T
                    (PROGN
                     (SETQ DIAG (SIMP (FINDELEM2 U RR RR)))
                     (SETQ M RR))))
                  (SETQ X
                          (CONS
                           (CONS (LIST M)
                                 ((LAMBDA (G124 G125)
                                    (COND
                                     (*PHYSOP-LOADED (PHYSOP-MULTF G124 G125))
                                     (T (POLY-MULTF G124 G125))))
                                  (ADDF (CAR DIAG)
                                        (NEGF
                                         ((LAMBDA (G122)
                                            (COND
                                             (*PHYSOP-LOADED
                                              (PHYSOP-MULTF G122 (CDR DIAG)))
                                             (T (POLY-MULTF G122 (CDR DIAG)))))
                                          (LIST (CONS (CONS EIVAL 1) 1)))))
                                  ((LAMBDA (*EXP) (QUOTF1 Y (CDR DIAG))) T)))
                           X))
                  (COND ((NOT (EQUAL XX NIL)) (SETQ M (CAR XX))))
                  NIL)))
               (COND
                ((AND (CAR J) (NOT (EQUAL M RR)))
                 (SETQ X
                         (CONS
                          (CONS (LIST M)
                                ((LAMBDA (G127)
                                   (COND
                                    (*PHYSOP-LOADED
                                     (PHYSOP-MULTF (CAR J) G127))
                                    (T (POLY-MULTF (CAR J) G127))))
                                 ((LAMBDA (*EXP) (QUOTF1 Y (CDR J))) T)))
                          X))))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (SETQ Y Z)
         (SETQ Z
                 (|C:EXTMULT|
                  (COND
                   ((NULL (CDR X))
                    (PROGN
                     ((LAMBDA (P)
                        (SETQ Q
                                (COND
                                 (P
                                  (CONS (CONS (CAR P) (PLUS (CDR P) 1))
                                        (DELETE P Q)))
                                 (T (CONS (CONS (CDAR X) 1) Q)))))
                      (ASSOC (CDAR X) Q))
                     (LIST (CONS (CAAR X) 1))))
                   (T X))
                  Z))
         (SETQ RES (APPEND RES (LIST X)))
         NIL)
        (SETQ RR (PLUS2 RR 1))
        (GO LAB))
      (SETQ U RES)
      (RETURN (CONS Q (CONS Y (CONS Z U)))))) 
(FLAG '(SPMATEIGEN) 'OPFN) 
(FLAG '(SPMATEIGEN) 'NOVAL) 
(PUT 'RANK-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RANK-EVAL 'DEFINED-ON-LINE '166) 
(PUT 'RANK-EVAL 'DEFINED-IN-FILE 'SPARSE/SPMATEIG.RED) 
(PUT 'RANK-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANK-EVAL (U)
    (PROG (N)
      (COND ((CDR U) (RERROR 'MATRIX 17 "Wrong number of arguments"))
            ((EQ (GETRTYPE (SETQ U (CAR U))) 'MATRIX)
             (RETURN (RANK-MATRIX (SPMATSM U))))
            ((EQ (GETRTYPE U) 'SPARSE) (RETURN (SPRANK-MATRIX (SPMATSM U))))
            ((NULL (EQCAR (SETQ U (REVAL1 U NIL)) 'LIST)) (TYPERR U "matrix"))
            (T
             (RETURN
              (RANK-MATRIX
               (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ROW (CDR U))
                 (COND ((NULL ROW) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ROW)
                                     (COND
                                      ((NOT (EQCAR ROW 'LIST))
                                       (RERROR 'MATRIX 15
                                               "list not in matrix shape"))
                                      (T
                                       (PROGN
                                        (SETQ ROW (CDR ROW))
                                        (COND ((NULL N) (SETQ N (LENGTH ROW)))
                                              ((NEQ N (LENGTH ROW))
                                               (RERROR 'MATRIX 151
                                                       "list not in matrix shape")))
                                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ J ROW)
                                          (COND ((NULL J) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (J)
                                                              (SIMP J))
                                                            (CAR J))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ J (CDR J))
                                          (COND
                                           ((NULL J) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J) (SIMP J))
                                                    (CAR J))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))))
                                   (CAR ROW))
                                  NIL)))
                LOOPLABEL
                 (SETQ ROW (CDR ROW))
                 (COND ((NULL ROW) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ROW)
                             (COND
                              ((NOT (EQCAR ROW 'LIST))
                               (RERROR 'MATRIX 15 "list not in matrix shape"))
                              (T
                               (PROGN
                                (SETQ ROW (CDR ROW))
                                (COND ((NULL N) (SETQ N (LENGTH ROW)))
                                      ((NEQ N (LENGTH ROW))
                                       (RERROR 'MATRIX 151
                                               "list not in matrix shape")))
                                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ J ROW)
                                  (COND ((NULL J) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J) (SIMP J))
                                                    (CAR J))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ J (CDR J))
                                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J))
                                                NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))))
                           (CAR ROW))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'RANK 'PSOPFN 'RANK-EVAL) 
(PUT 'SPRANK-MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'SPRANK-MATRIX 'DEFINED-ON-LINE '191) 
(PUT 'SPRANK-MATRIX 'DEFINED-IN-FILE 'SPARSE/SPMATEIG.RED) 
(PUT 'SPRANK-MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPRANK-MATRIX (U)
    (PROG (X Y Z W J CC M N)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ Z 1)
      (PROG (RR)
        (SETQ RR 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (SPROW_DIM U) RR)) (RETURN NIL)))
        (PROGN
         (SETQ Y 1)
         (SETQ CC (FINDROW U RR))
         (COND
          ((NOT (EQUAL CC NIL))
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR CC))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN (SETQ W (SIMP (CDR XX))) (SETQ Y (LCM Y (CDR W))) NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         (SETQ M 1)
         (SETQ X NIL)
         (COND
          ((NOT (EQUAL CC NIL))
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR CC))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ J (SIMP (CDR XX)))
                  (SETQ M (CAR XX))
                  (COND
                   ((CAR J)
                    (SETQ X
                            (CONS
                             (CONS (LIST M)
                                   ((LAMBDA (G129)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAR J) G129))
                                       (T (POLY-MULTF (CAR J) G129))))
                                    ((LAMBDA (*EXP) (QUOTF1 Y (CDR J))) T)))
                             X))))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         (COND
          ((SETQ Y (|C:EXTMULT| X Z)) (PROGN (SETQ Z Y) (SETQ N (PLUS N 1)))))
         NIL)
        (SETQ RR (PLUS2 RR 1))
        (GO LAB))
      (RETURN N))) 
(ENDMODULE) 