(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NULLSP)) 
(PUT 'NULLSPACE 'PSOPFN 'NULLSPACE-EVAL) 
(PUT 'NULLSPACE-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'NULLSPACE-EVAL 'DEFINED-ON-LINE '34) 
(PUT 'NULLSPACE-EVAL 'DEFINED-IN-FILE 'MATRIX/NULLSP.RED) 
(PUT 'NULLSPACE-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULLSPACE-EVAL (U)
    (PROG (V N MATINPUT)
      (SETQ V (REVAL1 (CAR U) T))
      (COND ((EQCAR V 'MAT) (PROGN (SETQ MATINPUT T) (SETQ V (CDR V))))
            ((EQCAR V 'LIST)
             (SETQ V
                     (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                       (SETQ ROW (CDR V))
                       (COND ((NULL ROW) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ROW)
                                           (COND
                                            ((NOT (EQCAR ROW 'LIST))
                                             (TYPERR "matrix" U))
                                            (T
                                             (PROGN
                                              (SETQ ROW (CDR ROW))
                                              (COND
                                               ((NULL N) (SETQ N (LENGTH ROW)))
                                               ((NEQ N (LENGTH ROW))
                                                (RERROR 'MATRIX 15
                                                        "lists not in matrix shape")))
                                              ROW))))
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
                                     (TYPERR "matrix" U))
                                    (T
                                     (PROGN
                                      (SETQ ROW (CDR ROW))
                                      (COND ((NULL N) (SETQ N (LENGTH ROW)))
                                            ((NEQ N (LENGTH ROW))
                                             (RERROR 'MATRIX 15
                                                     "lists not in matrix shape")))
                                      ROW))))
                                 (CAR ROW))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T (RERROR 'MATRIX 16 "Not a matrix")))
      (SETQ V (NULLSPACE-ALG V))
      (RETURN
       (CONS 'LIST
             (PROG (VECT FORALL-RESULT FORALL-ENDPTR)
               (SETQ VECT V)
               (COND ((NULL VECT) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (VECT)
                                   (COND
                                    (MATINPUT
                                     (CONS 'MAT
                                           (PROG (X FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ X VECT)
                                             (COND ((NULL X) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (X)
                                                                 (LIST X))
                                                               (CAR X))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ X (CDR X))
                                             (COND
                                              ((NULL X)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (X) (LIST X))
                                                       (CAR X))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL))))
                                    (T (CONS 'LIST VECT))))
                                 (CAR VECT))
                                NIL)))
              LOOPLABEL
               (SETQ VECT (CDR VECT))
               (COND ((NULL VECT) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (VECT)
                           (COND
                            (MATINPUT
                             (CONS 'MAT
                                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ X VECT)
                                     (COND ((NULL X) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (X) (LIST X))
                                                       (CAR X))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ X (CDR X))
                                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (X) (LIST X)) (CAR X))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))))
                            (T (CONS 'LIST VECT))))
                         (CAR VECT))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'NULLSPACE-ALG 'NUMBER-OF-ARGS 1) 
(PUT 'NULLSPACE-ALG 'DEFINED-ON-LINE '54) 
(PUT 'NULLSPACE-ALG 'DEFINED-IN-FILE 'MATRIX/NULLSP.RED) 
(PUT 'NULLSPACE-ALG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULLSPACE-ALG (M)
    (PROG (MP VARS RVARS R RES OLDORDER N)
      (SETQ N 0)
      (SETQ N (LENGTH (CAR M)))
      (SETQ VARS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (GENSYM) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (GENSYM) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RVARS (REVERSE VARS))
      (SETQ OLDORDER (SETKORDER RVARS))
      (SETQ MP
              (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                (SETQ ROW M)
                (COND ((NULL ROW) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ROW)
                                    (PROGN
                                     (SETQ R (CONS NIL 1))
                                     (PROG (COL)
                                       (SETQ COL (PAIR VARS ROW))
                                      LAB
                                       (COND ((NULL COL) (RETURN NIL)))
                                       ((LAMBDA (COL)
                                          (SETQ R
                                                  (ADDSQ R
                                                         (SIMP
                                                          (LIST 'TIMES
                                                                (CAR COL)
                                                                (CDR COL))))))
                                        (CAR COL))
                                       (SETQ COL (CDR COL))
                                       (GO LAB))
                                     R))
                                  (CAR ROW))
                                 NIL)))
               LOOPLABEL
                (SETQ ROW (CDR ROW))
                (COND ((NULL ROW) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ROW)
                            (PROGN
                             (SETQ R (CONS NIL 1))
                             (PROG (COL)
                               (SETQ COL (PAIR VARS ROW))
                              LAB
                               (COND ((NULL COL) (RETURN NIL)))
                               ((LAMBDA (COL)
                                  (SETQ R
                                          (ADDSQ R
                                                 (SIMP
                                                  (LIST 'TIMES (CAR COL)
                                                        (CDR COL))))))
                                (CAR COL))
                               (SETQ COL (CDR COL))
                               (GO LAB))
                             R))
                          (CAR ROW))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES (NULLSPACE-ELIM MP RVARS))
      (SETKORDER OLDORDER)
      (RETURN
       (REVERSE
        (PROG (Q FORALL-RESULT FORALL-ENDPTR)
          (SETQ Q RES)
          (COND ((NULL Q) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (Q)
                              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                (SETQ X VARS)
                                (COND ((NULL X) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (X)
                                                    (CDR (ATSOC X Q)))
                                                  (CAR X))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ X (CDR X))
                                (COND ((NULL X) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X) (CDR (ATSOC X Q)))
                                          (CAR X))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR Q))
                           NIL)))
         LOOPLABEL
          (SETQ Q (CDR Q))
          (COND ((NULL Q) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (Q)
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X VARS)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X) (CDR (ATSOC X Q)))
                                          (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CDR (ATSOC X Q))) (CAR X))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                    (CAR Q))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'NULLSPACE-ELIM 'NUMBER-OF-ARGS 2) 
(PUT 'NULLSPACE-ELIM 'DEFINED-ON-LINE '75) 
(PUT 'NULLSPACE-ELIM 'DEFINED-IN-FILE 'MATRIX/NULLSP.RED) 
(PUT 'NULLSPACE-ELIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NULLSPACE-ELIM (M VARS)
    (PROG (C S X W ARBVARS DEPVARS ROW RES BREAK)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VARS (NOT BREAK))) (RETURN NIL)))
        (PROGN
         (PROG (P)
           (SETQ P M)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((OR (ATOM (CAR P)) (ATOM (CAR (CAR P))))
                (COND ((CAR P) (SETQ BREAK T)) (T (SETQ M (DELETE P M)))))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (COND
          ((NOT BREAK)
           (PROGN
            (SETQ X (CAR VARS))
            (SETQ VARS (CDR VARS))
            (SETQ ROW NIL)
            (PROG (P)
              (SETQ P M)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (COND
                  ((AND (NULL ROW) (EQUAL (CAAAR (CAR P)) X)) (SETQ ROW P))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (COND ((NULL ROW) (SETQ ARBVARS (CONS X ARBVARS)))
                  (T
                   (PROGN
                    (SETQ M (DELETE ROW M))
                    (SETQ C
                            (MULTSQ (CONS (NEGF (CDR ROW)) 1)
                                    (CONS 1 (CDAR (CAR ROW)))))
                    (SETQ ROW (MULTSQ ROW C))
                    (SETQ DEPVARS
                            (CONS (CONS X (CONS (CDR (CAR ROW)) (CDR ROW)))
                                  DEPVARS))
                    (SETQ M
                            (PROG (P FORALL-RESULT FORALL-ENDPTR)
                              (SETQ P M)
                              (COND ((NULL P) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (P)
                                                  (PROGN
                                                   (COND
                                                    ((EQUAL (CAAAR (CAR P)) X)
                                                     (PROGN
                                                      (SETQ P
                                                              (ADDSQ P
                                                                     (MULTSQ
                                                                      ROW
                                                                      (CONS
                                                                       (CDAR
                                                                        (CAR
                                                                         P))
                                                                       (CDR
                                                                        P)))))
                                                      (COND
                                                       ((AND
                                                         (NOT
                                                          ((LAMBDA (U)
                                                             (OR (ATOM U)
                                                                 (ATOM
                                                                  (CAR U))))
                                                           (SETQ W (CAR P))))
                                                         (NOT
                                                          (OR (ATOM (CDAR W))
                                                              (ATOM
                                                               (CAR
                                                                (CDAR W))))))
                                                        (SETQ P
                                                                (SUBS2*
                                                                 P)))))))
                                                   P))
                                                (CAR P))
                                               NIL)))
                             LOOPLABEL
                              (SETQ P (CDR P))
                              (COND ((NULL P) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (P)
                                          (PROGN
                                           (COND
                                            ((EQUAL (CAAAR (CAR P)) X)
                                             (PROGN
                                              (SETQ P
                                                      (ADDSQ P
                                                             (MULTSQ ROW
                                                                     (CONS
                                                                      (CDAR
                                                                       (CAR P))
                                                                      (CDR
                                                                       P)))))
                                              (COND
                                               ((AND
                                                 (NOT
                                                  ((LAMBDA (U)
                                                     (OR (ATOM U)
                                                         (ATOM (CAR U))))
                                                   (SETQ W (CAR P))))
                                                 (NOT
                                                  (OR (ATOM (CDAR W))
                                                      (ATOM (CAR (CDAR W))))))
                                                (SETQ P (SUBS2* P)))))))
                                           P))
                                        (CAR P))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    NIL)))
            NIL)))
         NIL)
        (GO WHILELABEL))
      (COND (BREAK (RETURN NIL)))
      (PROG (X)
        (SETQ X ARBVARS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ S
                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Y ARBVARS)
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (CONS Y
                                                (COND ((EQUAL Y X) 1) (T 0))))
                                        (CAR Y))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Y)
                                  (CONS Y (COND ((EQUAL Y X) 1) (T 0))))
                                (CAR Y))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ C 1)
            (PROG (Y)
              (SETQ Y DEPVARS)
             LAB
              (COND ((NULL Y) (RETURN NIL)))
              ((LAMBDA (Y)
                 (PROGN
                  (SETQ S
                          (CONS
                           (CONS (CAR Y) (PREPSQ (SETQ W (SUBSQ (CDR Y) S))))
                           S))
                  (SETQ C (LCM* C (CDR W)))))
               (CAR Y))
              (SETQ Y (CDR Y))
              (GO LAB))
            (COND
             ((NOT (EQUAL C 1))
              (PROGN
               (SETQ C (PREPF C))
               (SETQ S
                       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Q S)
                         (COND ((NULL Q) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Q)
                                             (CONS (CAR Q)
                                                   (REVAL1
                                                    (LIST 'TIMES (CDR Q) C)
                                                    T)))
                                           (CAR Q))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Q (CDR Q))
                         (COND ((NULL Q) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (Q)
                                     (CONS (CAR Q)
                                           (REVAL1 (LIST 'TIMES (CDR Q) C) T)))
                                   (CAR Q))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
            (SETQ RES (CONS S RES))
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN RES))) 
(ENDMODULE) 