(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BAREISS)) 
(FLUID
 '(*EXP ASYMPLIS* SUBFG* WTL* *TRSPARSE POWLIS* POWLIS1* BAREISS-STEP-SIZE*)) 
(GLOBAL '(ASSUMPTIONS REQUIREMENTS)) 
(SETQ BAREISS-STEP-SIZE* 2) 
(PUT 'MATINVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'MATINVERSE 'DEFINED-ON-LINE '41) 
(PUT 'MATINVERSE 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'MATINVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATINVERSE (U) (LNRSOLVE U (GENERATEIDENT (LENGTH U)))) 
(PUT 'LNRSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'LNRSOLVE 'DEFINED-ON-LINE '44) 
(PUT 'LNRSOLVE 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'LNRSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LNRSOLVE (U V)
    (PROG (TEMP VLHS VRHS OK *EXP *SOLVESINGULAR)
      (COND (*NCMP (RETURN (CLNRSOLVE U V))))
      (SETQ *EXP T)
      (COND
       ((OR ASYMPLIS* WTL*)
        (PROGN
         (SETQ TEMP (CONS ASYMPLIS* WTL*))
         (SETQ ASYMPLIS* (SETQ WTL* NIL)))))
      (SETQ VLHS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE (LENGTH (CAR U)) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (INTERN (GENSYM)) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CAR U)) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (INTERN (GENSYM)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VRHS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE (LENGTH (CAR V)) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (INTERN (GENSYM)) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CAR V)) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (INTERN (GENSYM)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (CAR (NORMMAT (AUGMENT U V))))
      (SETQ V (APPEND VLHS VRHS))
      (SETQ OK (SETKORDER V))
      (SETQ U
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R U)
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (R) (PRSUM V R)) (CAR R)) NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (R) (PRSUM V R)) (CAR R)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V
              (ERRORSET*
               (LIST (FUNCTION SOLVEBAREISS) (MKQUOTE U) (MKQUOTE VLHS)) T))
      (COND
       ((MEMQ (CAAR V) (LIST 'SINGULAR 'INCONSISTENT))
        (PROGN (SETKORDER OK) (RERROR 'MATRIX 13 "Singular matrix"))))
      ((LAMBDA (S) (SETQ V (PAIR (CADR S) (CAR S)))) (CADAR V))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J VLHS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    ((LAMBDA (Q)
                                       (COEFFROW (NEGF (CAR Q)) VRHS (CDR Q)))
                                     (CDR (ATSOC J V))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            ((LAMBDA (Q)
                               (COEFFROW (NEGF (CAR Q)) VRHS (CDR Q)))
                             (CDR (ATSOC J V))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETKORDER OK)
      (COND (TEMP (PROGN (SETQ ASYMPLIS* (CAR TEMP)) (SETQ WTL* (CDR TEMP)))))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J U)
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J)
                             (PROG (K FORALL-RESULT FORALL-ENDPTR)
                               (SETQ K J)
                               (COND ((NULL K) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (K)
                                                   (COND (TEMP (RESIMP K))
                                                         (T (CANCEL K))))
                                                 (CAR K))
                                                NIL)))
                              LOOPLABEL
                               (SETQ K (CDR K))
                               (COND ((NULL K) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (K)
                                           (COND (TEMP (RESIMP K))
                                                 (T (CANCEL K))))
                                         (CAR K))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                           (CAR J))
                          NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (J)
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K J)
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (K)
                                           (COND (TEMP (RESIMP K))
                                                 (T (CANCEL K))))
                                         (CAR K))
                                        NIL)))
                      LOOPLABEL
                       (SETQ K (CDR K))
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (K)
                                   (COND (TEMP (RESIMP K)) (T (CANCEL K))))
                                 (CAR K))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
                   (CAR J))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'PRSUM 'NUMBER-OF-ARGS 2) 
(PUT 'PRSUM 'DEFINED-ON-LINE '73) 
(PUT 'PRSUM 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'PRSUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRSUM (KL CL)
    (COND ((NULL KL) NIL) ((NULL (CAR CL)) (PRSUM (CDR KL) (CDR CL)))
          (T
           (CONS (CONS (CONS (CAR KL) 1) (CAR CL)) (PRSUM (CDR KL) (CDR CL)))))) 
(PUT 'SOLVEBAREISS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEBAREISS 'DEFINED-ON-LINE '80) 
(PUT 'SOLVEBAREISS 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'SOLVEBAREISS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEBAREISS (EXLIS VARLIS)
    (PROG ()
      (SETQ EXLIS (SPARSE_BAREISS EXLIS VARLIS BAREISS-STEP-SIZE*))
      (COND
       ((EQUAL (CAR EXLIS) 'INCONSISTENT) (RETURN (CONS 'INCONSISTENT NIL))))
      (SETQ EXLIS (CDR EXLIS))
      (COND
       ((AND (NOT *SOLVESINGULAR) (LESSP (LENGTH EXLIS) (LENGTH VARLIS)))
        (RETURN (CONS 'SINGULAR NIL))))
      (COND
       (*TRSPARSE (SOLVESPARSEPRINT "Reduced system" (REVERSE EXLIS) VARLIS)))
      (SETQ EXLIS (SPARSE_BACKSUB EXLIS VARLIS))
      (SETQ VARLIS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P EXLIS)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EXLIS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P EXLIS)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EXLIS
              (PROG (EX FORALL-RESULT FORALL-ENDPTR)
                (SETQ EX EXLIS)
                (COND ((NULL EX) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EX) (RESIMP (SUBS2* EX))) (CAR EX))
                                 NIL)))
               LOOPLABEL
                (SETQ EX (CDR EX))
                (COND ((NULL EX) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EX) (RESIMP (SUBS2* EX))) (CAR EX))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS T (LIST (LIST EXLIS VARLIS 1)))))) 
(PUT 'COEFFROW 'NUMBER-OF-ARGS 3) 
(PUT 'COEFFROW 'DEFINED-ON-LINE '105) 
(PUT 'COEFFROW 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'COEFFROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFFROW (U V D)
    (COND ((NULL V) NIL)
          ((OR (NULL U) (NEQ (CAAAR U) (CAR V)))
           (CONS (CONS NIL 1) (COEFFROW U (CDR V) D)))
          (T (CONS (CONS (CDAR U) D) (COEFFROW (CDR U) (CDR V) D))))) 
(PUT 'AUGMENT 'NUMBER-OF-ARGS 2) 
(PUT 'AUGMENT 'DEFINED-ON-LINE '114) 
(PUT 'AUGMENT 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'AUGMENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AUGMENT (U V)
    (COND ((NULL U) NIL)
          (T (CONS (APPEND (CAR U) (CAR V)) (AUGMENT (CDR U) (CDR V)))))) 
(PUT 'GENERATEIDENT 'NUMBER-OF-ARGS 1) 
(PUT 'GENERATEIDENT 'DEFINED-ON-LINE '117) 
(PUT 'GENERATEIDENT 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'GENERATEIDENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERATEIDENT (N)
    (PROG (U V)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ U NIL)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (SETQ U (CONS (CONS (COND ((EQUAL I J) 1) (T NIL)) 1) U))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ V (CONS U V)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN V))) 
(PUT 'NORMMAT 'NUMBER-OF-ARGS 1) 
(PUT 'NORMMAT 'DEFINED-ON-LINE '127) 
(PUT 'NORMMAT 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'NORMMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMMAT (U)
    (PROG (X Y Z)
      (SETQ X 1)
      (PROG (V)
        (SETQ V U)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ Y 1)
            (PROG (W)
              (SETQ W V)
             LAB
              (COND ((NULL W) (RETURN NIL)))
              ((LAMBDA (W) (SETQ Y (LCM Y (CDR W)))) (CAR W))
              (SETQ W (CDR W))
              (GO LAB))
            (SETQ Z
                    (CONS
                     (PROG (W FORALL-RESULT FORALL-ENDPTR)
                       (SETQ W V)
                       (COND ((NULL W) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (W)
                                           ((LAMBDA (G544)
                                              (COND
                                               (*PHYSOP-LOADED
                                                (PHYSOP-MULTF (CAR W) G544))
                                               (T (POLY-MULTF (CAR W) G544))))
                                            ((LAMBDA (*EXP) (QUOTF1 Y (CDR W)))
                                             T)))
                                         (CAR W))
                                        NIL)))
                      LOOPLABEL
                       (SETQ W (CDR W))
                       (COND ((NULL W) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (W)
                                   ((LAMBDA (G544)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAR W) G544))
                                       (T (POLY-MULTF (CAR W) G544))))
                                    ((LAMBDA (*EXP) (QUOTF1 Y (CDR W))) T)))
                                 (CAR W))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     Z))
            (SETQ X
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y X))
                          (T (POLY-MULTF Y X))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS (REVERSE Z) X)))) 
(PUT 'SPARSE_BAREISS 'NUMBER-OF-ARGS 3) 
(PUT 'SPARSE_BAREISS 'DEFINED-ON-LINE '142) 
(PUT 'SPARSE_BAREISS 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'SPARSE_BAREISS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPARSE_BAREISS (U V K)
    (PROG (P D W PIVS S ASYMPLIS* POWLIS* POWLIS1* WTL*)
      (SETQ D 1)
      (SETQ U
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F U)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (COND (F (LIST (*SF2EX F V))))) (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (COND (F (LIST (*SF2EX F V))))) (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ P (CHOOSE_PIVOT_ROWS U V K D))) (RETURN NIL)))
        (PROG ()
          (SETQ U (CAR P))
          (SETQ V (CADR P))
          (SETQ P (CDDR P))
          (SETQ PIVS (CAAR (CAR P)))
          (SETQ U
                  (PROG (R FORALL-RESULT FORALL-ENDPTR)
                    (SETQ R U)
                   STARTOVER
                    (COND ((NULL R) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (R)
                               (PROG ()
                                 (SETQ R (SPLITUP R V))
                                 (SETQ R
                                         (EXTADD (EXTMULT (CADR R) (CAR P))
                                          (EXTMULT (CAR R) (CADR P))))
                                 (COND
                                  ((NULL (SETQ R (SUBS2CHKEX R)))
                                   (RETURN NIL)))
                                 (SETQ R (INNPRODPEX PIVS (QUOTEXF* R D)))
                                 (COND
                                  ((NOT (EVENP (LENGTH PIVS)))
                                   (SETQ R (NEGEX R))))
                                 (RETURN (LIST R))))
                             (CAR R)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ R (CDR R))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL R) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (R)
                               (PROG ()
                                 (SETQ R (SPLITUP R V))
                                 (SETQ R
                                         (EXTADD (EXTMULT (CADR R) (CAR P))
                                          (EXTMULT (CAR R) (CADR P))))
                                 (COND
                                  ((NULL (SETQ R (SUBS2CHKEX R)))
                                   (RETURN NIL)))
                                 (SETQ R (INNPRODPEX PIVS (QUOTEXF* R D)))
                                 (COND
                                  ((NOT (EVENP (LENGTH PIVS)))
                                   (SETQ R (NEGEX R))))
                                 (RETURN (LIST R))))
                             (CAR R)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ R (CDR R))
                    (GO LOOPLABEL)))
          (SETQ D (CDAR (CAR P)))
          (SETQ ASSUMPTIONS
                  (CONS 'LIST
                        (CONS (MK*SQ (CONS D 1))
                              (AND (PAIRP ASSUMPTIONS) (CDR ASSUMPTIONS)))))
          (SETQ P (EXTADD (CAR P) (CADR P)))
          (SETQ S (EVENP (LENGTH PIVS)))
          (PROG (X)
            (SETQ X PIVS)
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (SETQ W
                       (COND
                        ((SETQ S (NOT S))
                         (CONS (INNPRODPEX (DELETE X PIVS) P) W))
                        (T (CONS (NEGEX (INNPRODPEX (DELETE X PIVS) P)) W)))))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB)))
        (GO WHILELABEL))
      (PROG (F)
        (SETQ F U)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (SETQ REQUIREMENTS
                   (CONS 'LIST
                         (CONS (MK*SQ (CONS (*EX2SF F) 1))
                               (AND (PAIRP REQUIREMENTS)
                                    (CDR REQUIREMENTS))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN
       (COND (U (CONS 'INCONSISTENT NIL))
             (T
              (CONS T
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F W)
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (F) (*EX2SF F)) (CAR F))
                                            NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (F) (*EX2SF F)) (CAR F)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))))) 
(PUT 'CHOOSE_PIVOT_ROWS 'NUMBER-OF-ARGS 4) 
(PUT 'CHOOSE_PIVOT_ROWS 'DEFINED-ON-LINE '186) 
(PUT 'CHOOSE_PIVOT_ROWS 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'CHOOSE_PIVOT_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHOOSE_PIVOT_ROWS (U V K D)
    (COND ((OR (NULL U) (NULL V)) NIL)
          (T
           (PROG (W S SS P X Y ROWS PIVOTS)
             (SETQ W U)
             (PROG (I)
               (SETQ I 1)
              LAB
               (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
               (COND (V (SETQ V (CDR V))))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (PROG ()
              WHILELABEL
               (COND ((NOT (NEQ K 0)) (RETURN NIL)))
               (COND
                ((NULL U)
                 (COND ((OR (NULL V) (NULL W) PIVOTS) (SETQ K 0))
                       (T
                        (PROGN
                         (PROG (I)
                           (SETQ I 1)
                          LAB
                           (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
                           (COND (V (SETQ V (CDR V))))
                           (SETQ I (PLUS2 I 1))
                           (GO LAB))
                         (SETQ S NIL)
                         (SETQ U W)))))
                ((AND (CAR (SETQ X (SPLITUP (CAR U) V)))
                      (SETQ Y
                              (COND ((NULL PIVOTS) (CAR X))
                                    (T
                                     (SUBS2CHKEX
                                      (EXTMULT (CAR X) (CAR PIVOTS)))))))
                 (PROG ()
                   (SETQ ROWS (CONS X ROWS))
                   (SETQ PIVOTS
                           (CONS (COND ((NULL PIVOTS) Y) (T (QUOTEXF* Y D)))
                                 PIVOTS))
                   (COND (S (SETQ SS (NOT SS))))
                   (SETQ W (DELETE (CAR U) W))
                   (SETQ U (CDR U))
                   (SETQ K (DIFFERENCE K 1))))
                (T (PROGN (SETQ U (CDR U)) (SETQ S (NOT S)))))
               (GO WHILELABEL))
             (COND ((NULL PIVOTS) (RETURN NIL)))
             (COND
              ((MEMBER (REMAINDER (LENGTH (CAAR (CAR PIVOTS))) 4) (LIST 2 3))
               (SETQ SS (NOT SS))))
             (SETQ ROWS (REVERSE ROWS))
             (SETQ PIVOTS (REVERSE PIVOTS))
             (SETQ P (CAR ROWS))
             (PROG (R)
               (SETQ R (CDR ROWS))
              LAB
               (COND ((NULL R) (RETURN NIL)))
               ((LAMBDA (R)
                  (SETQ P
                          (LIST (CAR (SETQ PIVOTS (CDR PIVOTS)))
                                (QUOTEXF*
                                 (EXTADD (EXTMULT (CADR R) (CAR P))
                                  (EXTMULT (CAR R) (CADR P)))
                                 D))))
                (CAR R))
               (SETQ R (CDR R))
               (GO LAB))
             (RETURN
              (CONS W
                    (CONS V
                          (COND (SS (LIST (NEGEX (CAR P)) (NEGEX (CADR P))))
                                (T P))))))))) 
(PUT 'SPARSE_BACKSUB 'NUMBER-OF-ARGS 2) 
(PUT 'SPARSE_BACKSUB 'DEFINED-ON-LINE '229) 
(PUT 'SPARSE_BACKSUB 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'SPARSE_BACKSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPARSE_BACKSUB (EXLIS VARLIS)
    (PROG (D Z C)
      (COND ((NULL EXLIS) (RETURN NIL)))
      (SETQ D (CDAR (CAR EXLIS)))
      (PROG (X)
        (SETQ X EXLIS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (S P V R)
             (SETQ P (CDAR X))
             (SETQ V (CAAAR X))
             (SETQ X (CDR X))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (NOT (OR (ATOM X) (ATOM (CAR X))))
                       (MEMBER (CAAAR X) VARLIS)))
                 (RETURN NIL)))
               (PROGN
                (COND
                 ((SETQ C (ATSOC (CAAAR X) Z))
                  (SETQ S
                          (ADDF
                           (COND
                            (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR X) (CDR C)))
                            (T (POLY-MULTF (CDAR X) (CDR C))))
                           S)))
                 (T (SETQ R (ADDF (LIST (CAR X)) R))))
                (SETQ X (CDR X)))
               (GO WHILELABEL))
             (SETQ S
                     (NEGF
                      (QUOTFF
                       (ADDF
                        ((LAMBDA (G545)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G545 D))
                                 (T (POLY-MULTF G545 D))))
                         (ADDF R X))
                        S)
                       P)))
             (SETQ Z (CONS (CONS V S) Z))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (P)
        (SETQ P Z)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETCDR P (CANCEL (CONS (CDR P) D)))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN Z))) 
(PUT 'QUOTFF 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTFF 'DEFINED-ON-LINE '258) 
(PUT 'QUOTFF 'DEFINED-IN-FILE 'MATRIX/BAREISS.RED) 
(PUT 'QUOTFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTFF (U V)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (X)
              (COND (X X)
                    (T
                     ((LAMBDA (Y)
                        (COND ((EQUAL (CDR Y) 1) (CAR Y))
                              (T (REDERR "Invalid division in backsub"))))
                      (RATIONALIZESQ (CONS U V))))))
            ((LAMBDA (*EXP) (QUOTF1 U V)) T))))) 
(ENDMODULE) 